// v8_runner.zig — V8 embed runner: executes JavaScript files with Node.js compatibility
//
// This is the main entry point for running JS files (like _tsc.js) through V8
// with full filesystem access via __edgebox_io_sync.
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

    // Step 7: Wrap script as CJS module with __filename/__dirname, then eval
    // _tsc.js uses __filename at module scope, so we must provide it.
    const dirname = std.fs.path.dirname(abs_path) orelse ".";
    const prefix = "(function(exports, require, module, __filename, __dirname) {\n";
    const suffix = try std.fmt.allocPrint(alloc,
        "\n}})(globalThis.module.exports, require, globalThis.module, \"{s}\", \"{s}\");",
        .{ abs_path, dirname },
    );
    defer alloc.free(suffix);

    // Build full wrapped script
    const total_len = prefix.len + script_code.len + suffix.len;
    const wrapped = try alloc.alloc(u8, total_len);
    defer alloc.free(wrapped);

    @memcpy(wrapped[0..prefix.len], prefix);
    @memcpy(wrapped[prefix.len..][0..script_code.len], script_code);
    @memcpy(wrapped[prefix.len + script_code.len ..][0..suffix.len], suffix);

    // Step 8: Execute with TryCatch for error reporting
    var try_catch = v8.TryCatch.init(isolate);
    defer try_catch.deinit();

    _ = v8.eval(isolate, context, wrapped, abs_path) catch {
        // Report the JS exception
        if (try_catch.stackTrace(context)) |stack_val| {
            if (v8.ValueApi.toString(stack_val, context)) |stack_str| {
                var stack_buf: [8192]u8 = undefined;
                const stack_len = v8.StringApi.writeUtf8(stack_str, isolate, &stack_buf);
                const stderr = std.fs.File.stderr();
                stderr.writeAll(stack_buf[0..stack_len]) catch {};
                stderr.writeAll("\n") catch {};
            }
        } else if (try_catch.exception()) |exc_val| {
            if (v8.ValueApi.toString(exc_val, context)) |exc_str| {
                var exc_buf: [4096]u8 = undefined;
                const exc_len = v8.StringApi.writeUtf8(exc_str, isolate, &exc_buf);
                const stderr = std.fs.File.stderr();
                stderr.writeAll(exc_buf[0..exc_len]) catch {};
                stderr.writeAll("\n") catch {};
            }
        }
        std.process.exit(1);
    };
}
