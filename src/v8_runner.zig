// v8_runner.zig — V8 embed runner: executes JavaScript files with Node.js compatibility
//
// This is the main entry point for running JS files (like _tsc.js) through V8
// with full filesystem access via __edgebox_io_sync.
//
// Features:
// - CJS module wrapping with __filename/__dirname
// - V8 code cache for instant script reloads (~0ms parse on warm runs)
// - Embedded bootstrap (no external files needed)
//
// Usage: edgebox-v8 <script.js> [args...]
//
// Build: zig build v8-run
// Run:   ./zig-out/bin/edgebox-v8 node_modules/typescript/lib/_tsc.js --version

const std = @import("std");
const v8 = @import("v8.zig");
const v8_io = @import("v8_io.zig");

pub fn main() !void {
    const alloc = std.heap.page_allocator;

    // Parse args: edgebox-v8 <script.js> [script args...]
    var args = try std.process.argsWithAllocator(alloc);
    defer args.deinit();

    _ = args.next(); // skip binary name
    const script_path = args.next() orelse {
        const stderr = std.fs.File.stderr();
        stderr.writeAll("Usage: edgebox-v8 <script.js> [args...]\n") catch {};
        std.process.exit(1);
    };

    // Resolve to absolute path for __filename/__dirname
    var abs_buf: [std.fs.max_path_bytes]u8 = undefined;
    const abs_path = std.fs.cwd().realpath(script_path, &abs_buf) catch script_path;

    // Step 1: Initialize V8
    _ = try v8.initPlatform();
    defer v8.disposePlatform();

    // Step 2: Create Isolate
    const isolate = v8.IsolateApi.create();
    defer v8.IsolateApi.dispose(isolate);
    v8.IsolateApi.enter(isolate);
    defer v8.IsolateApi.exit(isolate);

    // Step 3: HandleScope + Context
    var handle_scope = v8.HandleScope.init(isolate);
    defer handle_scope.deinit();
    const context = v8.ContextApi.create(isolate);
    v8.ContextApi.enter(context);
    defer v8.ContextApi.exit(context);

    // Step 4: Register __edgebox_io_sync
    v8_io.registerGlobals(isolate, context);

    // Step 5: Load bootstrap (embedded at compile time for self-contained binary)
    const bootstrap_code = @embedFile("v8_bootstrap.js");
    _ = v8.eval(isolate, context, bootstrap_code, "v8_bootstrap.js") catch |err| {
        std.debug.print("[v8] ERROR: bootstrap failed: {}\n", .{err});
        return err;
    };

    // Step 6: Read user script
    const script_code = std.fs.cwd().readFileAlloc(alloc, script_path, 64 * 1024 * 1024) catch |err| {
        std.debug.print("[v8] ERROR: could not read {s}: {}\n", .{ script_path, err });
        return err;
    };
    defer alloc.free(script_code);

    // Step 7: Wrap script as CJS module with __filename/__dirname
    const dirname = std.fs.path.dirname(abs_path) orelse ".";
    const prefix = "(function(exports, require, module, __filename, __dirname) {\n";
    const suffix = try std.fmt.allocPrint(alloc,
        "\n}})(globalThis.module.exports, require, globalThis.module, \"{s}\", \"{s}\");",
        .{ abs_path, dirname },
    );
    defer alloc.free(suffix);

    const total_len = prefix.len + script_code.len + suffix.len;
    const wrapped = try alloc.alloc(u8, total_len);
    defer alloc.free(wrapped);

    @memcpy(wrapped[0..prefix.len], prefix);
    @memcpy(wrapped[prefix.len..][0..script_code.len], script_code);
    @memcpy(wrapped[prefix.len + script_code.len ..][0..suffix.len], suffix);

    // Step 8: Compile with code cache support
    var try_catch = v8.TryCatch.init(isolate);
    defer try_catch.deinit();

    const source_str = v8.StringApi.fromUtf8(isolate, wrapped) orelse {
        std.debug.print("[v8] ERROR: failed to create source string\n", .{});
        std.process.exit(1);
    };
    const name_str = v8.StringApi.fromUtf8(isolate, abs_path) orelse {
        std.debug.print("[v8] ERROR: failed to create name string\n", .{});
        std.process.exit(1);
    };
    var origin = v8.ScriptOrigin.init(@ptrCast(name_str));

    // Try to load code cache from disk
    const cache_path = getCachePath(alloc, script_code) catch null;
    var cached_data: ?*v8.CachedData = null;
    var cache_bytes: ?[]u8 = null;
    if (cache_path) |cp| {
        cache_bytes = std.fs.cwd().readFileAlloc(alloc, cp, 128 * 1024 * 1024) catch null;
        if (cache_bytes) |cb| {
            cached_data = v8.ScriptCompilerApi.createCachedData(cb.ptr, @intCast(cb.len));
        }
    }

    // Compile with or without code cache
    var compiler_source = v8.CompilerSource.init(source_str, &origin, cached_data);
    defer compiler_source.deinit();

    const compile_options: c_int = if (cached_data != null)
        v8.ScriptCompilerApi.kConsumeCodeCache
    else
        v8.ScriptCompilerApi.kNoCompileOptions;

    const script = v8.ScriptCompilerApi.compile(context, &compiler_source, compile_options) orelse {
        reportException(&try_catch, isolate, context);
        std.process.exit(1);
    };

    // Check if cache was rejected
    var cache_was_used = false;
    if (cached_data != null) {
        if (compiler_source.getCachedData()) |cd| {
            const info = v8.ScriptCompilerApi.getCachedDataBytes(cd);
            cache_was_used = !info.rejected;
        }
    }

    // If no cache was used, create and save code cache for next run
    if (!cache_was_used) {
        if (cache_path) |cp| {
            const unbound = v8.ScriptExtApi.getUnboundScript(script);
            const new_cache = v8.UnboundScriptApi.createCodeCache(unbound);
            const cache_info = v8.ScriptCompilerApi.getCachedDataBytes(new_cache);
            if (cache_info.length > 0) {
                const cache_data_slice = cache_info.data[0..@intCast(cache_info.length)];
                // Ensure cache directory exists
                if (std.fs.path.dirname(cp)) |dir| {
                    std.fs.cwd().makePath(dir) catch {};
                }
                const file = std.fs.cwd().createFile(cp, .{}) catch null;
                if (file) |f| {
                    defer f.close();
                    f.writeAll(cache_data_slice) catch {};
                }
            }
        }
    }

    // Free cache bytes after compilation (CachedData references them)
    if (cache_bytes) |cb| alloc.free(cb);
    if (cached_data) |cd| v8.ScriptCompilerApi.deleteCachedData(cd);

    // Step 9: Execute
    _ = v8.ScriptApi.run(script, context) orelse {
        reportException(&try_catch, isolate, context);
        std.process.exit(1);
    };
}

/// Report a JavaScript exception to stderr.
fn reportException(try_catch: *const v8.TryCatch, isolate: *v8.Isolate, context: *const v8.Context) void {
    const stderr = std.fs.File.stderr();
    if (try_catch.stackTrace(context)) |stack_val| {
        if (v8.ValueApi.toString(stack_val, context)) |stack_str| {
            var stack_buf: [8192]u8 = undefined;
            const stack_len = v8.StringApi.writeUtf8(stack_str, isolate, &stack_buf);
            stderr.writeAll(stack_buf[0..stack_len]) catch {};
            stderr.writeAll("\n") catch {};
        }
    } else if (try_catch.exception()) |exc_val| {
        if (v8.ValueApi.toString(exc_val, context)) |exc_str| {
            var exc_buf: [4096]u8 = undefined;
            const exc_len = v8.StringApi.writeUtf8(exc_str, isolate, &exc_buf);
            stderr.writeAll(exc_buf[0..exc_len]) catch {};
            stderr.writeAll("\n") catch {};
        }
    }
}

/// Compute cache path based on script content hash.
/// Cache stored in ~/.cache/edgebox/v8-cache/<hash>.codecache
fn getCachePath(allocator: std.mem.Allocator, content: []const u8) ![]const u8 {
    // Simple FNV-1a hash of content
    var hash: u64 = 14695981039346656037;
    for (content) |byte| {
        hash ^= byte;
        hash *%= 1099511628211;
    }

    // Get cache directory
    const home = std.process.getEnvVarOwned(allocator, "HOME") catch return error.NoHome;
    defer allocator.free(home);

    return try std.fmt.allocPrint(allocator, "{s}/.cache/edgebox/v8-cache/{x}.codecache", .{ home, hash });
}
