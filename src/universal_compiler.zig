/// Universal Compiler Tool
/// Compiles JavaScript to standalone native binaries or WASM modules
/// WITH freeze optimization for maximum performance
///
/// Usage:
///   edgebox-compile input.js -o output [--native|--wasm]
///
/// This compiler:
///   1. Compiles JS to bytecode using QuickJS
///   2. Runs freeze to generate optimized native C code
///   3. Compiles QuickJS + frozen code + polyfills into final binary
///
const std = @import("std");
const quickjs_core = @import("quickjs_core.zig");
const qjs = quickjs_core.c;

// Freeze module for generating optimized C code
const freeze = @import("freeze/frozen_registry.zig");

const OutputTarget = enum {
    native,
    wasm,
};

pub fn main() !u8 {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    if (args.len < 2) {
        printUsage();
        return 1;
    }

    // Parse arguments
    var input_file: ?[]const u8 = null;
    var output_file: []const u8 = "output";
    var target: OutputTarget = .native;
    var debug_mode = false;

    var i: usize = 1;
    while (i < args.len) : (i += 1) {
        const arg = args[i];
        if (std.mem.eql(u8, arg, "-o") and i + 1 < args.len) {
            i += 1;
            output_file = args[i];
        } else if (std.mem.eql(u8, arg, "--native")) {
            target = .native;
        } else if (std.mem.eql(u8, arg, "--wasm")) {
            target = .wasm;
        } else if (std.mem.eql(u8, arg, "--debug")) {
            debug_mode = true;
        } else if (std.mem.eql(u8, arg, "--help") or std.mem.eql(u8, arg, "-h")) {
            printUsage();
            return 0;
        } else if (!std.mem.startsWith(u8, arg, "-")) {
            input_file = arg;
        }
    }

    if (input_file == null) {
        std.debug.print("Error: No input file specified\n", .{});
        printUsage();
        return 1;
    }

    // Read input JavaScript
    std.debug.print("Reading {s}...\n", .{input_file.?});
    const js_code = std.fs.cwd().readFileAlloc(allocator, input_file.?, 50 * 1024 * 1024) catch |err| {
        std.debug.print("Error reading file: {}\n", .{err});
        return 1;
    };
    defer allocator.free(js_code);

    // Create temp directory for build artifacts
    const tmp_dir = "/tmp/edgebox-compile";
    std.fs.cwd().makePath(tmp_dir) catch {};

    // Step 1: Compile ORIGINAL JS to bytecode for freeze analysis
    std.debug.print("Compiling original JS for analysis...\n", .{});
    const original_bytecode_path = try std.fmt.allocPrint(allocator, "{s}/original.c", .{tmp_dir});
    defer allocator.free(original_bytecode_path);

    // Write original JS to temp file
    const original_js_path = try std.fmt.allocPrint(allocator, "{s}/original.js", .{tmp_dir});
    defer allocator.free(original_js_path);
    try std.fs.cwd().writeFile(.{ .sub_path = original_js_path, .data = js_code });

    if (try compileWithQjsc(allocator, original_js_path, original_bytecode_path) != 0) {
        std.debug.print("Failed to compile original JavaScript\n", .{});
        return 1;
    }

    // Step 2: Generate frozen C code from ORIGINAL bytecode (no hooks)
    std.debug.print("Generating frozen optimizations...\n", .{});
    const frozen_path = try std.fmt.allocPrint(allocator, "{s}/frozen_functions.c", .{tmp_dir});
    defer allocator.free(frozen_path);

    const original_bytecode_content = std.fs.cwd().readFileAlloc(allocator, original_bytecode_path, 50 * 1024 * 1024) catch |err| {
        std.debug.print("Error reading bytecode file: {}\n", .{err});
        return 1;
    };
    defer allocator.free(original_bytecode_content);

    const frozen_code = freeze.freezeModuleWithManifest(allocator, original_bytecode_content, "frozen", null, debug_mode) catch |err| {
        std.debug.print("Warning: Freeze failed ({s}), using empty frozen_init\n", .{@errorName(err)});
        try std.fs.cwd().writeFile(.{
            .sub_path = frozen_path,
            .data =
            \\// No frozen functions generated
            \\#include "quickjs.h"
            \\int frozen_init_c(JSContext *ctx) { (void)ctx; return 0; }
            \\
            ,
        });
        return try buildBinary(allocator, tmp_dir, output_file, target);
    };
    defer allocator.free(frozen_code);

    try std.fs.cwd().writeFile(.{ .sub_path = frozen_path, .data = frozen_code });
    std.debug.print("Generated frozen C code: {} bytes\n", .{frozen_code.len});

    // Step 3: Inject hooks into JS source for freeze dispatch
    std.debug.print("Injecting hooks...\n", .{});
    const hooked_js_path = try std.fmt.allocPrint(allocator, "{s}/hooked.js", .{tmp_dir});
    defer allocator.free(hooked_js_path);
    const hooked_js = try injectHooks(allocator, js_code);
    defer allocator.free(hooked_js);
    try std.fs.cwd().writeFile(.{ .sub_path = hooked_js_path, .data = hooked_js });

    // Step 4: Compile HOOKED JS to bytecode
    std.debug.print("Compiling hooked JS...\n", .{});

    if (target == .native) {
        // For native: compile to C format with main()
        const bytecode_c_path = try std.fmt.allocPrint(allocator, "{s}/bundle_compiled.c", .{tmp_dir});
        defer allocator.free(bytecode_c_path);

        if (try compileWithQjsc(allocator, hooked_js_path, bytecode_c_path) != 0) {
            std.debug.print("Failed to compile hooked JavaScript\n", .{});
            return 1;
        }

        // Patch bundle_compiled.c for polyfills and frozen init
        std.debug.print("Patching bundle_compiled.c...\n", .{});
        try patchBundleCompiled(allocator, bytecode_c_path);
    } else {
        // For WASM: compile to raw bytecode for embedding
        const bytecode_bin_path = try std.fmt.allocPrint(allocator, "{s}/bytecode.bin", .{tmp_dir});
        defer allocator.free(bytecode_bin_path);

        if (try compileWithQjscRaw(allocator, hooked_js_path, bytecode_bin_path) != 0) {
            std.debug.print("Failed to compile hooked JavaScript to raw bytecode\n", .{});
            return 1;
        }
    }

    // Step 5: Build the final binary
    std.debug.print("Building {s} binary...\n", .{@tagName(target)});
    return try buildBinary(allocator, tmp_dir, output_file, target);
}

/// Compile JS to raw bytecode format using qjsc -b
fn compileWithQjscRaw(allocator: std.mem.Allocator, input_file: []const u8, output_path: []const u8) !u8 {
    const self_exe = try std.fs.selfExePathAlloc(allocator);
    defer allocator.free(self_exe);
    const bin_dir = std.fs.path.dirname(self_exe) orelse ".";

    const qjsc_path = try std.fmt.allocPrint(allocator, "{s}/qjsc", .{bin_dir});
    defer allocator.free(qjsc_path);

    std.fs.cwd().access(qjsc_path, .{}) catch {
        std.debug.print("Error: qjsc not found at {s}\n", .{qjsc_path});
        return 1;
    };

    const argv = [_][]const u8{
        qjsc_path,
        "-b", // Output raw bytecode
        "-o",
        output_path,
        input_file,
    };

    var child = std.process.Child.init(&argv, allocator);
    const term = try child.spawnAndWait();

    return switch (term) {
        .Exited => |code| code,
        else => 1,
    };
}

/// Compile JS to C bytecode format using qjsc
fn compileWithQjsc(allocator: std.mem.Allocator, input_file: []const u8, output_path: []const u8) !u8 {
    // Get the EdgeBox directory to find qjsc
    const self_exe = try std.fs.selfExePathAlloc(allocator);
    defer allocator.free(self_exe);
    const bin_dir = std.fs.path.dirname(self_exe) orelse ".";

    const qjsc_path = try std.fmt.allocPrint(allocator, "{s}/qjsc", .{bin_dir});
    defer allocator.free(qjsc_path);

    // Check if qjsc exists
    std.fs.cwd().access(qjsc_path, .{}) catch {
        std.debug.print("Error: qjsc not found at {s}\n", .{qjsc_path});
        std.debug.print("Build it first: zig build qjsc\n", .{});
        return 1;
    };

    const argv = [_][]const u8{
        qjsc_path,
        "-e", // Output main() and bytecode in C file
        "-N",
        "bundle", // Variable name for bytecode array
        "-o",
        output_path,
        input_file,
    };

    var child = std.process.Child.init(&argv, allocator);
    const term = try child.spawnAndWait();

    return switch (term) {
        .Exited => |code| code,
        else => 1,
    };
}

/// Patch bundle_compiled.c to:
/// 1. Add extern declaration for frozen_init_c
/// 2. Rename main() to qjsc_entry()
/// 3. Inject polyfill init before js_std_eval_binary
fn patchBundleCompiled(allocator: std.mem.Allocator, path: []const u8) !void {
    const content = try std.fs.cwd().readFileAlloc(allocator, path, 50 * 1024 * 1024);
    defer allocator.free(content);

    var modified = std.ArrayListUnmanaged(u8){};
    defer modified.deinit(allocator);

    // Performance polyfill - Date.now() fallback
    const polyfill_code =
        \\
        \\extern int frozen_init_c(JSContext *ctx);
        \\
        \\// Polyfill for performance.now() - uses Date.now() as fallback
        \\static const char *__polyfill_js =
        \\    "if (typeof performance === 'undefined') {"
        \\    "  globalThis.performance = { now: function() { return Date.now(); } };"
        \\    "}"
        \\    "if (typeof process === 'undefined') {"
        \\    "  globalThis.process = { exit: function(code) { std.exit(code); } };"
        \\    "}";
        \\
        \\static void __init_polyfills(JSContext *ctx) {
        \\    JS_Eval(ctx, __polyfill_js, strlen(__polyfill_js), "<polyfill>", JS_EVAL_TYPE_GLOBAL);
        \\}
        \\
    ;

    // Find the include line and inject extern declaration
    const include_marker = "#include \"quickjs-libc.h\"";
    if (std.mem.indexOf(u8, content, include_marker)) |pos| {
        var end_of_line = pos + include_marker.len;
        while (end_of_line < content.len and content[end_of_line] != '\n') : (end_of_line += 1) {}
        if (end_of_line < content.len) end_of_line += 1;

        try modified.appendSlice(allocator, content[0..end_of_line]);
        try modified.appendSlice(allocator, "\n#include <string.h>\n");
        try modified.appendSlice(allocator, polyfill_code);

        // Process the rest of the file
        var i: usize = end_of_line;
        while (i < content.len) {
            if (i + 8 <= content.len and std.mem.eql(u8, content[i .. i + 8], "int main")) {
                try modified.appendSlice(allocator, "int qjsc_entry");
                i += 8;
            } else if (i + 18 <= content.len and std.mem.eql(u8, content[i .. i + 18], "js_std_eval_binary")) {
                // Init polyfills and frozen functions before running bytecode
                // The hooks in the JS code check for globalThis.__frozen_NAME
                try modified.appendSlice(allocator, "__init_polyfills(ctx); frozen_init_c(ctx); js_std_eval_binary");
                i += 18;
            } else {
                try modified.append(allocator, content[i]);
                i += 1;
            }
        }
    } else {
        // No include found, just do simple replacements
        var i: usize = 0;
        while (i < content.len) {
            if (i + 8 <= content.len and std.mem.eql(u8, content[i .. i + 8], "int main")) {
                try modified.appendSlice(allocator, "int qjsc_entry");
                i += 8;
            } else if (i + 18 <= content.len and std.mem.eql(u8, content[i .. i + 18], "js_std_eval_binary")) {
                try modified.appendSlice(allocator, "js_std_eval_binary");
                i += 18;
            } else {
                try modified.append(allocator, content[i]);
                i += 1;
            }
        }
    }

    try std.fs.cwd().writeFile(.{ .sub_path = path, .data = modified.items });
}

fn buildBinary(allocator: std.mem.Allocator, tmp_dir: []const u8, output_file: []const u8, target: OutputTarget) !u8 {
    // Get the EdgeBox source directory
    const self_exe = try std.fs.selfExePathAlloc(allocator);
    defer allocator.free(self_exe);

    const bin_dir = std.fs.path.dirname(self_exe) orelse ".";
    const zig_out_dir = std.fs.path.dirname(bin_dir) orelse ".";
    const edgebox_dir = std.fs.path.dirname(zig_out_dir) orelse ".";

    const output_name = std.fs.path.basename(output_file);
    const output_dir = std.fs.path.dirname(output_file);
    const prefix_dir = output_dir orelse ".";

    // Build system requires files in zig-out/cache/
    // Copy generated files there
    const cache_dir = try std.fmt.allocPrint(allocator, "{s}/zig-out/cache", .{edgebox_dir});
    defer allocator.free(cache_dir);

    std.fs.cwd().makePath(cache_dir) catch {};

    // Copy bundle_compiled.c
    const src_bundle = try std.fmt.allocPrint(allocator, "{s}/bundle_compiled.c", .{tmp_dir});
    defer allocator.free(src_bundle);
    const dst_bundle = try std.fmt.allocPrint(allocator, "{s}/bundle_compiled.c", .{cache_dir});
    defer allocator.free(dst_bundle);
    std.fs.cwd().copyFile(src_bundle, std.fs.cwd(), dst_bundle, .{}) catch |err| {
        std.debug.print("Error copying bundle_compiled.c: {}\n", .{err});
        return 1;
    };

    // Copy frozen_functions.c
    const src_frozen = try std.fmt.allocPrint(allocator, "{s}/frozen_functions.c", .{tmp_dir});
    defer allocator.free(src_frozen);
    const dst_frozen = try std.fmt.allocPrint(allocator, "{s}/frozen_functions.c", .{cache_dir});
    defer allocator.free(dst_frozen);
    std.fs.cwd().copyFile(src_frozen, std.fs.cwd(), dst_frozen, .{}) catch |err| {
        std.debug.print("Error copying frozen_functions.c: {}\n", .{err});
        return 1;
    };

    // Use wasm-standalone (native flow removed - WAMR AOT is faster)
    const build_target = "wasm-standalone";
    _ = target; // Keep for future browser target

    // For WASM, we need to pass bytecode path
    const bytecode_opt = try std.fmt.allocPrint(allocator, "-Dbytecode={s}/bytecode.bin", .{tmp_dir});
    defer allocator.free(bytecode_opt);

    // Always use ReleaseFast for maximum performance
    var argv_list = std.ArrayListUnmanaged([]const u8){};
    defer argv_list.deinit(allocator);
    try argv_list.appendSlice(allocator, &[_][]const u8{
        "zig",
        "build",
        build_target,
        "-Doptimize=ReleaseFast",
    });

    // Always pass bytecode path for wasm-standalone
    try argv_list.append(allocator, bytecode_opt);

    try argv_list.appendSlice(allocator, &[_][]const u8{
        "-p",
        prefix_dir,
    });

    var child = std.process.Child.init(argv_list.items, allocator);
    child.cwd = edgebox_dir;

    const term = try child.spawnAndWait();

    const result_code: u8 = switch (term) {
        .Exited => |code| code,
        else => 1,
    };

    if (result_code == 0) {
        // The build system produces bin/edgebox-standalone.wasm
        // Rename to the requested output name
        const built_name = "bin/edgebox-standalone.wasm";
        const built_path = try std.fmt.allocPrint(allocator, "{s}/{s}", .{ prefix_dir, built_name });
        defer allocator.free(built_path);

        const final_path = try std.fmt.allocPrint(allocator, "{s}/{s}", .{ prefix_dir, output_name });
        defer allocator.free(final_path);

        std.fs.cwd().rename(built_path, final_path) catch |err| {
            std.debug.print("Warning: Could not rename output: {}\n", .{err});
        };
    }

    return result_code;
}

/// Inject hooks into JavaScript source for freeze support
/// This adds checks at function entry points to call frozen versions if available
fn injectHooks(allocator: std.mem.Allocator, code: []const u8) ![]u8 {
    var result = std.ArrayListUnmanaged(u8){};
    defer result.deinit(allocator);

    var i: usize = 0;
    while (i < code.len) {
        // Look for "function NAME("
        if (i + 9 <= code.len and std.mem.eql(u8, code[i .. i + 8], "function")) {
            // Skip whitespace
            var j = i + 8;
            while (j < code.len and (code[j] == ' ' or code[j] == '\t' or code[j] == '\n')) : (j += 1) {}

            // Check if it's a named function (not anonymous)
            if (j < code.len and isIdentChar(code[j])) {
                // Extract function name
                const name_start = j;
                while (j < code.len and isIdentChar(code[j])) : (j += 1) {}
                const name = code[name_start..j];

                // Skip to opening paren
                while (j < code.len and code[j] != '(') : (j += 1) {}
                if (j >= code.len) {
                    try result.append(allocator, code[i]);
                    i += 1;
                    continue;
                }

                // Find matching closing paren
                var paren_count: u32 = 1;
                j += 1; // skip (
                const args_start = j;
                while (j < code.len and paren_count > 0) {
                    if (code[j] == '(') paren_count += 1 else if (code[j] == ')') paren_count -= 1;
                    j += 1;
                }
                const args_end = j - 1;
                const args = code[args_start..args_end];

                // Skip to opening brace
                while (j < code.len and code[j] != '{') : (j += 1) {}
                if (j >= code.len) {
                    try result.append(allocator, code[i]);
                    i += 1;
                    continue;
                }

                // Extract argument names (simple parsing)
                var arg_names = std.ArrayListUnmanaged([]const u8){};
                defer arg_names.deinit(allocator);
                var k: usize = 0;
                while (k < args.len) {
                    // Skip whitespace
                    while (k < args.len and (args[k] == ' ' or args[k] == ',' or args[k] == '\t')) : (k += 1) {}
                    if (k >= args.len) break;

                    const arg_start = k;
                    while (k < args.len and isIdentChar(args[k])) : (k += 1) {}
                    if (k > arg_start) {
                        try arg_names.append(allocator, args[arg_start..k]);
                    }

                    // Skip to next comma or end
                    while (k < args.len and args[k] != ',') : (k += 1) {}
                }

                // Copy everything up to and including the opening brace
                try result.appendSlice(allocator, code[i .. j + 1]);

                // Generate and insert hook
                try result.appendSlice(allocator, "if(globalThis.__frozen_");
                try result.appendSlice(allocator, name);
                try result.appendSlice(allocator, "){if(!globalThis.__frozen_fallback_active){return globalThis.__frozen_");
                try result.appendSlice(allocator, name);
                try result.appendSlice(allocator, "(");
                for (arg_names.items, 0..) |arg, idx| {
                    if (idx > 0) try result.appendSlice(allocator, ",");
                    try result.appendSlice(allocator, arg);
                }
                try result.appendSlice(allocator, ");}}");

                i = j + 1;
                continue;
            }
        }

        try result.append(allocator, code[i]);
        i += 1;
    }

    return try allocator.dupe(u8, result.items);
}

fn isIdentChar(c: u8) bool {
    return (c >= 'a' and c <= 'z') or (c >= 'A' and c <= 'Z') or (c >= '0' and c <= '9') or c == '_' or c == '$';
}

fn printUsage() void {
    std.debug.print(
        \\EdgeBox Universal Compiler
        \\
        \\Compiles JavaScript to standalone WASM modules with freeze optimization.
        \\For best performance, use `edgebox --binary` with WAMR AOT instead.
        \\
        \\Usage:
        \\  edgebox-compile <input.js> -o <output> [options]
        \\
        \\Options:
        \\  -o <file>     Output file name (default: output.wasm)
        \\  --debug       Enable debug output
        \\  -h, --help    Show this help
        \\
        \\Examples:
        \\  edgebox-compile app.js -o myapp.wasm
        \\
        \\Output runs on any WASI runtime (wasmtime, wasmer, Node.js WASI).
        \\For production, use WAMR AOT via `edgebox --binary` for best speed.
        \\
    , .{});
}
