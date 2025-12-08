/// EdgeBox Native Runtime
/// Embeds WasmEdge C library directly for minimal cold start overhead
///
/// Usage:
///   edgebox build [app-directory]  - Build app (bundle + WASM + AOT)
///   edgebox run <script.js>        - Run JavaScript file
///   edgebox <script.js>            - Run JavaScript file (shorthand)
const std = @import("std");
const builtin = @import("builtin");
const c = @cImport({
    @cInclude("wasmedge/wasmedge.h");
});

const VERSION = "0.1.0";

pub fn main() !void {
    const allocator = std.heap.page_allocator;

    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    if (args.len < 2) {
        printUsage();
        std.process.exit(1);
    }

    const cmd = args[1];

    if (std.mem.eql(u8, cmd, "build")) {
        // Static mode is the default (faster), --dynamic for development
        var dynamic_mode = false;
        var app_dir: []const u8 = "examples/hello";
        for (args[2..]) |arg| {
            if (std.mem.eql(u8, arg, "--dynamic")) {
                dynamic_mode = true;
            } else if (!std.mem.startsWith(u8, arg, "-")) {
                app_dir = arg;
            }
        }
        if (dynamic_mode) {
            try runBuild(allocator, app_dir);
        } else {
            try runStaticBuild(allocator, app_dir);
        }
    } else if (std.mem.eql(u8, cmd, "run")) {
        if (args.len < 3) {
            std.debug.print("Usage: edgebox run <file.wasm> [args...]\n", .{});
            std.process.exit(1);
        }
        const file = args[2];
        if (std.mem.endsWith(u8, file, ".wasm") or std.mem.endsWith(u8, file, ".dylib") or std.mem.endsWith(u8, file, ".so")) {
            try runWasm(allocator, args[2..]);
        } else {
            try runScript(allocator, args[2..]);
        }
    } else if (std.mem.eql(u8, cmd, "--help") or std.mem.eql(u8, cmd, "-h")) {
        printUsage();
    } else if (std.mem.eql(u8, cmd, "--version") or std.mem.eql(u8, cmd, "-v")) {
        std.debug.print("edgebox {s}\n", .{VERSION});
    } else if (std.mem.endsWith(u8, cmd, ".wasm") or std.mem.endsWith(u8, cmd, ".dylib") or std.mem.endsWith(u8, cmd, ".so")) {
        // Direct WASM execution: edgebox app.wasm
        try runWasm(allocator, args[1..]);
    } else if (std.mem.endsWith(u8, cmd, ".js")) {
        // Direct script execution (dynamic mode): edgebox script.js
        try runScript(allocator, args[1..]);
    } else {
        std.debug.print("Unknown command: {s}\n", .{cmd});
        printUsage();
        std.process.exit(1);
    }
}

fn printUsage() void {
    std.debug.print(
        \\EdgeBox - QuickJS JavaScript Runtime with WASI + WasmEdge AOT
        \\
        \\Usage:
        \\  edgebox build [app-directory]   Compile JS to WASM with embedded bytecode
        \\  edgebox run <file.wasm>         Run compiled WASM module
        \\  edgebox <file.wasm>             Run WASM (shorthand)
        \\
        \\Options:
        \\  --help, -h     Show this help
        \\  --version, -v  Show version
        \\
        \\Build Options:
        \\  --dynamic      Use dynamic JS loading (for development)
        \\
        \\Examples:
        \\  edgebox build my-app             Compile app to WASM
        \\  edgebox run edgebox-static.wasm  Run the compiled WASM
        \\  edgebox edgebox-static.wasm      Run WASM (shorthand)
        \\
    , .{});
}

fn runBuild(allocator: std.mem.Allocator, app_dir: []const u8) !void {
    std.debug.print("[build] App directory: {s}\n", .{app_dir});

    // Step 1: Check if app directory exists
    var dir = std.fs.cwd().openDir(app_dir, .{}) catch {
        std.debug.print("[error] App directory not found: {s}\n", .{app_dir});
        std.process.exit(1);
    };
    dir.close();

    // Step 2: Find entry point
    var entry_path_buf: [4096]u8 = undefined;
    const entry_path = findEntryPoint(app_dir, &entry_path_buf) catch {
        std.debug.print("[error] No entry point found in {s} (index.js, main.js, or app.js)\n", .{app_dir});
        std.process.exit(1);
    };
    std.debug.print("[build] Entry point: {s}\n", .{entry_path});

    // Step 3: Bundle with Bun
    std.debug.print("[build] Bundling with Bun...\n", .{});
    const bun_result = try runCommand(allocator, &.{
        "bun", "build", entry_path, "--outfile=bundle.js", "--target=node", "--format=cjs", "--minify",
    });
    defer {
        if (bun_result.stdout) |s| allocator.free(s);
        if (bun_result.stderr) |s| allocator.free(s);
    }

    if (bun_result.term.Exited != 0) {
        // Try without minify
        std.debug.print("[warn] Bun minify failed, trying without...\n", .{});
        const retry = try runCommand(allocator, &.{
            "bun", "build", entry_path, "--outfile=bundle.js", "--target=node", "--format=cjs",
        });
        defer {
            if (retry.stdout) |s| allocator.free(s);
            if (retry.stderr) |s| allocator.free(s);
        }
        if (retry.term.Exited != 0) {
            std.debug.print("[error] Bun bundling failed\n", .{});
            std.process.exit(1);
        }
    }

    // Step 4: Prepend polyfills
    const polyfills_path = "src/polyfills/runtime.js";
    if (std.fs.cwd().access(polyfills_path, .{})) |_| {
        std.debug.print("[build] Prepending runtime polyfills...\n", .{});
        try prependPolyfills(allocator, polyfills_path, "bundle.js");
    } else |_| {}

    // Print bundle size
    if (std.fs.cwd().statFile("bundle.js")) |stat| {
        const size_kb = @as(f64, @floatFromInt(stat.size)) / 1024.0;
        std.debug.print("[build] Bundle: bundle.js ({d:.1}KB)\n", .{size_kb});
    } else |_| {}

    // Step 5: Build WASM if not exists
    const wasm_exists = std.fs.cwd().access("edgebox-base.wasm", .{}) catch null;
    if (wasm_exists == null) {
        std.debug.print("[build] Building QuickJS WASM with Zig...\n", .{});
        const zig_result = try runCommand(allocator, &.{
            "zig", "build", "wasm", "-Doptimize=ReleaseFast",
        });
        defer {
            if (zig_result.stdout) |s| allocator.free(s);
            if (zig_result.stderr) |s| allocator.free(s);
        }

        if (zig_result.term.Exited == 0) {
            // Copy from zig-out
            std.fs.cwd().copyFile("zig-out/bin/edgebox-base.wasm", std.fs.cwd(), "edgebox-base.wasm", .{}) catch {};

            // Run Wizer pre-initialization
            try runWizer(allocator);

            // Run wasm-opt
            try runWasmOpt(allocator);
        } else {
            std.debug.print("[warn] Zig WASM build failed\n", .{});
        }
    } else {
        std.debug.print("[build] Using existing edgebox-base.wasm\n", .{});
    }

    // Step 6: Pre-compile bytecode cache
    const wasm_ok = std.fs.cwd().access("edgebox-base.wasm", .{}) catch null;
    const bundle_ok = std.fs.cwd().access("bundle.js", .{}) catch null;
    if (wasm_ok != null and bundle_ok != null) {
        std.debug.print("[build] Pre-compiling bytecode cache...\n", .{});

        var cwd_buf: [4096]u8 = undefined;
        const cwd = std.process.getCwd(&cwd_buf) catch ".";

        var bundle_abs_buf: [4096]u8 = undefined;
        const bundle_abs = std.fmt.bufPrint(&bundle_abs_buf, "{s}/bundle.js", .{cwd}) catch "bundle.js";

        var dir_arg_buf: [4096]u8 = undefined;
        const dir_arg = std.fmt.bufPrint(&dir_arg_buf, "--dir={s}", .{cwd}) catch "--dir=.";

        const compile_result = try runCommand(allocator, &.{
            "wasmedge", dir_arg, "edgebox-base.wasm", "--compile-only", bundle_abs,
        });
        defer {
            if (compile_result.stdout) |s| allocator.free(s);
            if (compile_result.stderr) |s| allocator.free(s);
        }

        if (std.fs.cwd().statFile("bundle.js.cache")) |stat| {
            const size_kb = @as(f64, @floatFromInt(stat.size)) / 1024.0;
            std.debug.print("[build] Bytecode cache: bundle.js.cache ({d:.1}KB)\n", .{size_kb});
        } else |_| {
            std.debug.print("[warn] Bytecode pre-compilation failed (will compile on first run)\n", .{});
        }
    }

    // Step 7: AOT compile
    const aot_ext = if (builtin.os.tag == .macos) "dylib" else "so";
    var aot_path_buf: [256]u8 = undefined;
    const aot_path = std.fmt.bufPrint(&aot_path_buf, "edgebox-aot.{s}", .{aot_ext}) catch "edgebox-aot.so";

    const aot_exists = std.fs.cwd().access(aot_path, .{}) catch null;
    if (aot_exists == null and wasm_ok != null) {
        std.debug.print("[build] AOT compiling with WasmEdge...\n", .{});
        const aot_result = try runCommand(allocator, &.{
            "wasmedge", "compile", "edgebox-base.wasm", aot_path,
        });
        defer {
            if (aot_result.stdout) |s| allocator.free(s);
            if (aot_result.stderr) |s| allocator.free(s);
        }

        if (aot_result.term.Exited == 0) {
            if (std.fs.cwd().statFile(aot_path)) |stat| {
                const size_mb = @as(f64, @floatFromInt(stat.size)) / 1024.0 / 1024.0;
                std.debug.print("[build] AOT compiled: {s} ({d:.1}MB)\n", .{ aot_path, size_mb });
            } else |_| {}
        } else {
            std.debug.print("[warn] AOT compilation failed, WASM module still usable\n", .{});
        }
    } else if (aot_exists != null) {
        std.debug.print("[build] Using existing {s}\n", .{aot_path});
    }

    // Copy .edgebox.json if exists
    var config_path_buf: [4096]u8 = undefined;
    const config_path = std.fmt.bufPrint(&config_path_buf, "{s}/.edgebox.json", .{app_dir}) catch null;
    if (config_path) |cp| {
        if (std.fs.cwd().access(cp, .{})) |_| {
            std.fs.cwd().copyFile(cp, std.fs.cwd(), ".edgebox.json", .{}) catch {};
            std.debug.print("[build] Config copied to .edgebox.json\n", .{});
        } else |_| {}
    }

    // Summary
    std.debug.print("\n[build] === Build Complete ===\n\n", .{});
    std.debug.print("To run:\n", .{});
    std.debug.print("  edgebox run bundle.js\n", .{});
    std.debug.print("  edgebox bundle.js\n\n", .{});
}

/// Static build: compile JS to C bytecode with qjsc, embed in WASM
fn runStaticBuild(allocator: std.mem.Allocator, app_dir: []const u8) !void {
    std.debug.print("[build] Static mode: compiling JS to bytecode\n", .{});
    std.debug.print("[build] App directory: {s}\n", .{app_dir});

    // Step 1: Check app directory
    var dir = std.fs.cwd().openDir(app_dir, .{}) catch {
        std.debug.print("[error] App directory not found: {s}\n", .{app_dir});
        std.process.exit(1);
    };
    dir.close();

    // Step 2: Find entry point
    var entry_path_buf: [4096]u8 = undefined;
    const entry_path = findEntryPoint(app_dir, &entry_path_buf) catch {
        std.debug.print("[error] No entry point found in {s}\n", .{app_dir});
        std.process.exit(1);
    };
    std.debug.print("[build] Entry point: {s}\n", .{entry_path});

    // Step 3: Bundle with Bun
    std.debug.print("[build] Bundling with Bun...\n", .{});
    const bun_result = try runCommand(allocator, &.{
        "bun", "build", entry_path, "--outfile=bundle.js", "--target=node", "--format=cjs", "--minify",
    });
    defer {
        if (bun_result.stdout) |s| allocator.free(s);
        if (bun_result.stderr) |s| allocator.free(s);
    }

    if (bun_result.term.Exited != 0) {
        const retry = try runCommand(allocator, &.{
            "bun", "build", entry_path, "--outfile=bundle.js", "--target=node", "--format=cjs",
        });
        defer {
            if (retry.stdout) |s| allocator.free(s);
            if (retry.stderr) |s| allocator.free(s);
        }
        if (retry.term.Exited != 0) {
            std.debug.print("[error] Bun bundling failed\n", .{});
            std.process.exit(1);
        }
    }

    // Step 4: Prepend polyfills
    const polyfills_path = "src/polyfills/runtime.js";
    if (std.fs.cwd().access(polyfills_path, .{})) |_| {
        std.debug.print("[build] Prepending runtime polyfills...\n", .{});
        try prependPolyfills(allocator, polyfills_path, "bundle.js");
    } else |_| {}

    if (std.fs.cwd().statFile("bundle.js")) |stat| {
        const size_kb = @as(f64, @floatFromInt(stat.size)) / 1024.0;
        std.debug.print("[build] Bundle: bundle.js ({d:.1}KB)\n", .{size_kb});
    } else |_| {}

    // Step 5: Build qjsc if not exists
    const qjsc_exists = std.fs.cwd().access("zig-out/bin/qjsc", .{}) catch null;
    if (qjsc_exists == null) {
        std.debug.print("[build] Building qjsc compiler...\n", .{});
        const qjsc_result = try runCommand(allocator, &.{
            "zig", "build", "qjsc", "-Doptimize=ReleaseFast",
        });
        defer {
            if (qjsc_result.stdout) |s| allocator.free(s);
            if (qjsc_result.stderr) |s| allocator.free(s);
        }
        if (qjsc_result.term.Exited != 0) {
            std.debug.print("[error] Failed to build qjsc\n", .{});
            std.process.exit(1);
        }
    }

    // Step 6: Compile JS to C with qjsc
    std.debug.print("[build] Compiling JS to bytecode with qjsc...\n", .{});
    const qjsc_result = try runCommand(allocator, &.{
        "zig-out/bin/qjsc",
        "-s",              // Strip source
        "-N", "bundle",    // Variable name prefix
        "-o", "bundle_compiled.c",
        "bundle.js",
    });
    defer {
        if (qjsc_result.stdout) |s| allocator.free(s);
        if (qjsc_result.stderr) |s| allocator.free(s);
    }

    if (qjsc_result.term.Exited != 0) {
        std.debug.print("[error] qjsc compilation failed\n", .{});
        if (qjsc_result.stderr) |err| {
            std.debug.print("{s}\n", .{err});
        }
        std.process.exit(1);
    }

    // Append C bridge functions for Zig extern access
    const bridge_code =
        \\
        \\// Bridge functions for Zig extern access
        \\const uint8_t* get_bundle_ptr(void) { return bundle; }
        \\uint32_t get_bundle_size(void) { return bundle_size; }
        \\
    ;
    const bridge_file = std.fs.cwd().openFile("bundle_compiled.c", .{ .mode = .read_write }) catch {
        std.debug.print("[error] Failed to open bundle_compiled.c\n", .{});
        std.process.exit(1);
    };
    defer bridge_file.close();
    bridge_file.seekFromEnd(0) catch {};
    bridge_file.writeAll(bridge_code) catch {};

    if (std.fs.cwd().statFile("bundle_compiled.c")) |stat| {
        const size_kb = @as(f64, @floatFromInt(stat.size)) / 1024.0;
        std.debug.print("[build] Bytecode: bundle_compiled.c ({d:.1}KB)\n", .{size_kb});
    } else |_| {}

    // Step 7: Build WASM with embedded bytecode
    std.debug.print("[build] Building static WASM with embedded bytecode...\n", .{});
    const wasm_result = try runCommand(allocator, &.{
        "zig", "build", "wasm-static", "-Doptimize=ReleaseFast",
    });
    defer {
        if (wasm_result.stdout) |s| allocator.free(s);
        if (wasm_result.stderr) |s| allocator.free(s);
    }

    if (wasm_result.term.Exited != 0) {
        std.debug.print("[error] WASM build failed\n", .{});
        if (wasm_result.stderr) |err| {
            std.debug.print("{s}\n", .{err});
        }
        std.process.exit(1);
    }

    // Copy from zig-out
    std.fs.cwd().copyFile("zig-out/bin/edgebox-static.wasm", std.fs.cwd(), "edgebox-static.wasm", .{}) catch {
        std.debug.print("[error] Failed to copy WASM\n", .{});
        std.process.exit(1);
    };

    if (std.fs.cwd().statFile("edgebox-static.wasm")) |stat| {
        const size_kb = @as(f64, @floatFromInt(stat.size)) / 1024.0;
        std.debug.print("[build] Static WASM: edgebox-static.wasm ({d:.1}KB)\n", .{size_kb});
    } else |_| {}

    // Step 8: Wizer pre-initialization
    try runWizerStatic(allocator);

    // Step 9: wasm-opt
    try runWasmOptStatic(allocator);

    // Step 10: AOT compile
    const aot_ext = if (builtin.os.tag == .macos) "dylib" else "so";
    var aot_path_buf: [256]u8 = undefined;
    const aot_path = std.fmt.bufPrint(&aot_path_buf, "edgebox-static-aot.{s}", .{aot_ext}) catch "edgebox-static-aot.so";

    std.debug.print("[build] AOT compiling with WasmEdge...\n", .{});
    const aot_result = try runCommand(allocator, &.{
        "wasmedge", "compile", "edgebox-static.wasm", aot_path,
    });
    defer {
        if (aot_result.stdout) |s| allocator.free(s);
        if (aot_result.stderr) |s| allocator.free(s);
    }

    if (aot_result.term.Exited == 0) {
        if (std.fs.cwd().statFile(aot_path)) |stat| {
            const size_mb = @as(f64, @floatFromInt(stat.size)) / 1024.0 / 1024.0;
            std.debug.print("[build] AOT: {s} ({d:.1}MB)\n", .{ aot_path, size_mb });
        } else |_| {}
    } else {
        std.debug.print("[warn] AOT compilation failed\n", .{});
    }

    // Summary
    std.debug.print("\n[build] === Static Build Complete ===\n\n", .{});
    std.debug.print("Files created:\n", .{});
    std.debug.print("  bundle_compiled.c     - Compiled bytecode (C source)\n", .{});
    std.debug.print("  edgebox-static.wasm   - WASM with embedded bytecode\n", .{});
    std.debug.print("  {s}  - AOT native module\n\n", .{aot_path});
    std.debug.print("To run:\n", .{});
    std.debug.print("  wasmedge edgebox-static.wasm\n", .{});
    std.debug.print("  wasmedge {s}\n\n", .{aot_path});
}

fn runWizerStatic(allocator: std.mem.Allocator) !void {
    const which_result = try runCommand(allocator, &.{ "which", "wizer" });
    defer {
        if (which_result.stdout) |s| allocator.free(s);
        if (which_result.stderr) |s| allocator.free(s);
    }

    if (which_result.term.Exited != 0) {
        std.debug.print("[build] Wizer not found - skipping pre-initialization\n", .{});
        return;
    }

    std.debug.print("[build] Running Wizer pre-initialization...\n", .{});
    const wizer_result = try runCommand(allocator, &.{
        "wizer",
        "edgebox-static.wasm",
        "-o",
        "edgebox-static-wizer.wasm",
        "--allow-wasi",
        "--wasm-bulk-memory",
        "true",
        "--init-func",
        "wizer_init",
    });
    defer {
        if (wizer_result.stdout) |s| allocator.free(s);
        if (wizer_result.stderr) |s| allocator.free(s);
    }

    if (wizer_result.term.Exited == 0) {
        std.fs.cwd().deleteFile("edgebox-static.wasm") catch {};
        std.fs.cwd().rename("edgebox-static-wizer.wasm", "edgebox-static.wasm") catch {};
        if (std.fs.cwd().statFile("edgebox-static.wasm")) |stat| {
            const size_kb = @as(f64, @floatFromInt(stat.size)) / 1024.0;
            std.debug.print("[build] Wizer snapshot: {d:.1}KB\n", .{size_kb});
        } else |_| {}
    } else {
        std.debug.print("[warn] Wizer failed (will use slower init)\n", .{});
    }
}

fn runWasmOptStatic(allocator: std.mem.Allocator) !void {
    const which_result = try runCommand(allocator, &.{ "which", "wasm-opt" });
    defer {
        if (which_result.stdout) |s| allocator.free(s);
        if (which_result.stderr) |s| allocator.free(s);
    }

    if (which_result.term.Exited != 0) return;

    std.debug.print("[build] Optimizing with wasm-opt...\n", .{});
    const opt_result = try runCommand(allocator, &.{
        "wasm-opt", "-Oz", "--enable-simd", "--strip-debug", "edgebox-static.wasm", "-o", "edgebox-static-opt.wasm",
    });
    defer {
        if (opt_result.stdout) |s| allocator.free(s);
        if (opt_result.stderr) |s| allocator.free(s);
    }

    if (opt_result.term.Exited == 0) {
        std.fs.cwd().deleteFile("edgebox-static.wasm") catch {};
        std.fs.cwd().rename("edgebox-static-opt.wasm", "edgebox-static.wasm") catch {};
        if (std.fs.cwd().statFile("edgebox-static.wasm")) |stat| {
            const size_kb = @as(f64, @floatFromInt(stat.size)) / 1024.0;
            std.debug.print("[build] Optimized: {d:.1}KB\n", .{size_kb});
        } else |_| {}
    }
}

fn runWizer(allocator: std.mem.Allocator) !void {
    // Check if wizer is available
    const which_result = try runCommand(allocator, &.{ "which", "wizer" });
    defer {
        if (which_result.stdout) |s| allocator.free(s);
        if (which_result.stderr) |s| allocator.free(s);
    }

    if (which_result.term.Exited != 0) {
        std.debug.print("[build] Wizer not found - install with: cargo install wizer --features=\"env_logger structopt\"\n", .{});
        return;
    }

    std.debug.print("[build] Running Wizer pre-initialization...\n", .{});
    const wizer_result = try runCommand(allocator, &.{
        "wizer",
        "edgebox-base.wasm",
        "-o",
        "edgebox-wizer.wasm",
        "--allow-wasi",
        "--wasm-bulk-memory",
        "true",
        "--init-func",
        "wizer_init",
    });
    defer {
        if (wizer_result.stdout) |s| allocator.free(s);
        if (wizer_result.stderr) |s| allocator.free(s);
    }

    if (wizer_result.term.Exited == 0) {
        std.fs.cwd().deleteFile("edgebox-base.wasm") catch {};
        std.fs.cwd().rename("edgebox-wizer.wasm", "edgebox-base.wasm") catch {};
        if (std.fs.cwd().statFile("edgebox-base.wasm")) |stat| {
            const size_kb = @as(f64, @floatFromInt(stat.size)) / 1024.0;
            std.debug.print("[build] Wizer snapshot: edgebox-base.wasm ({d:.1}KB)\n", .{size_kb});
        } else |_| {}
    } else {
        std.debug.print("[warn] Wizer pre-initialization failed (will use slower init path)\n", .{});
    }
}

fn runWasmOpt(allocator: std.mem.Allocator) !void {
    // Check if wasm-opt is available
    const which_result = try runCommand(allocator, &.{ "which", "wasm-opt" });
    defer {
        if (which_result.stdout) |s| allocator.free(s);
        if (which_result.stderr) |s| allocator.free(s);
    }

    if (which_result.term.Exited != 0) {
        return;
    }

    std.debug.print("[build] Optimizing WASM with wasm-opt...\n", .{});
    const opt_result = try runCommand(allocator, &.{
        "wasm-opt", "-Oz", "--enable-simd", "--strip-debug", "edgebox-base.wasm", "-o", "edgebox-base-opt.wasm",
    });
    defer {
        if (opt_result.stdout) |s| allocator.free(s);
        if (opt_result.stderr) |s| allocator.free(s);
    }

    if (opt_result.term.Exited == 0) {
        std.fs.cwd().deleteFile("edgebox-base.wasm") catch {};
        std.fs.cwd().rename("edgebox-base-opt.wasm", "edgebox-base.wasm") catch {};
        if (std.fs.cwd().statFile("edgebox-base.wasm")) |stat| {
            const size_kb = @as(f64, @floatFromInt(stat.size)) / 1024.0;
            std.debug.print("[build] Optimized WASM: {d:.1}KB\n", .{size_kb});
        } else |_| {}
    }
}

fn findEntryPoint(app_dir: []const u8, buf: *[4096]u8) ![]const u8 {
    const entries = [_][]const u8{ "index.js", "main.js", "app.js" };
    for (entries) |entry| {
        const path = std.fmt.bufPrint(buf, "{s}/{s}", .{ app_dir, entry }) catch continue;
        if (std.fs.cwd().access(path, .{})) |_| {
            return path;
        } else |_| {}
    }
    return error.NotFound;
}

fn prependPolyfills(allocator: std.mem.Allocator, polyfills_path: []const u8, bundle_path: []const u8) !void {
    // Read polyfills
    const polyfills = try std.fs.cwd().readFileAlloc(allocator, polyfills_path, 1024 * 1024);
    defer allocator.free(polyfills);

    // Read bundle
    const bundle = try std.fs.cwd().readFileAlloc(allocator, bundle_path, 10 * 1024 * 1024);
    defer allocator.free(bundle);

    // Write combined
    const file = try std.fs.cwd().createFile(bundle_path, .{});
    defer file.close();

    try file.writeAll(polyfills);
    try file.writeAll(";\n");
    try file.writeAll(bundle);
}

const CommandResult = struct {
    term: std.process.Child.Term,
    stdout: ?[]u8,
    stderr: ?[]u8,
};

fn runCommand(allocator: std.mem.Allocator, argv: []const []const u8) !CommandResult {
    var child = std.process.Child.init(argv, allocator);
    child.stdout_behavior = .Pipe;
    child.stderr_behavior = .Pipe;

    try child.spawn();

    // Read stdout and stderr in Zig 0.15 style
    const stdout = if (child.stdout) |f| f.readToEndAlloc(allocator, 10 * 1024 * 1024) catch null else null;
    const stderr = if (child.stderr) |f| f.readToEndAlloc(allocator, 10 * 1024 * 1024) catch null else null;

    const term = try child.wait();

    return .{
        .term = term,
        .stdout = stdout,
        .stderr = stderr,
    };
}

fn runScript(allocator: std.mem.Allocator, args: []const [:0]const u8) !void {
    _ = allocator;

    const script_path = args[0];

    // Initialize WasmEdge
    c.WasmEdge_LogSetErrorLevel();

    // Create configure
    const conf = c.WasmEdge_ConfigureCreate();
    defer c.WasmEdge_ConfigureDelete(conf);

    // Enable WASI
    c.WasmEdge_ConfigureAddHostRegistration(conf, c.WasmEdge_HostRegistration_Wasi);

    // Create VM
    const vm = c.WasmEdge_VMCreate(conf, null);
    defer c.WasmEdge_VMDelete(vm);

    // Get WASI module and initialize
    const wasi_module = c.WasmEdge_VMGetImportModuleContext(vm, c.WasmEdge_HostRegistration_Wasi);

    // Setup WASI args - just pass script path
    var wasi_args: [64][*c]const u8 = undefined;
    var wasi_argc: usize = 0;
    wasi_args[wasi_argc] = script_path.ptr;
    wasi_argc += 1;
    for (args[1..]) |arg| {
        if (wasi_argc < wasi_args.len) {
            wasi_args[wasi_argc] = arg.ptr;
            wasi_argc += 1;
        }
    }

    // Simple preopens - current dir and common paths
    var preopens: [4][*c]const u8 = undefined;
    preopens[0] = ".:.";
    preopens[1] = "/tmp:/private/tmp";

    var cwd_buf: [1024]u8 = undefined;
    const cwd = std.process.getCwd(&cwd_buf) catch ".";

    var cwd_preopen_buf: [2048]u8 = undefined;
    const cwd_preopen = std.fmt.bufPrintZ(&cwd_preopen_buf, "{s}:{s}", .{ cwd, cwd }) catch ".:.";
    preopens[2] = cwd_preopen.ptr;

    // Get script directory
    const script_dir = std.fs.path.dirname(script_path) orelse ".";
    var script_preopen_buf: [2048]u8 = undefined;
    const script_preopen = std.fmt.bufPrintZ(&script_preopen_buf, "{s}:{s}", .{ script_dir, script_dir }) catch ".:.";
    preopens[3] = script_preopen.ptr;

    c.WasmEdge_ModuleInstanceInitWASI(
        wasi_module,
        &wasi_args,
        @intCast(wasi_argc),
        null, // No env
        0,
        &preopens,
        4,
    );

    // Try AOT first, then WASM
    const aot_ext = if (builtin.os.tag == .macos) "dylib" else "so";
    var aot_path_buf: [256]u8 = undefined;
    const aot_path = std.fmt.bufPrintZ(&aot_path_buf, "edgebox-aot.{s}", .{aot_ext}) catch "edgebox-aot.so";
    const wasm_path = "edgebox-base.wasm";

    var module_path: [*c]const u8 = wasm_path;
    if (std.fs.cwd().access(aot_path, .{})) |_| {
        module_path = aot_path.ptr;
    } else |_| {}

    // Load module
    var result = c.WasmEdge_VMLoadWasmFromFile(vm, module_path);
    if (!c.WasmEdge_ResultOK(result)) {
        std.debug.print("Failed to load WASM: {s}\n", .{c.WasmEdge_ResultGetMessage(result)});
        std.process.exit(1);
    }

    // Validate
    result = c.WasmEdge_VMValidate(vm);
    if (!c.WasmEdge_ResultOK(result)) {
        std.debug.print("Failed to validate: {s}\n", .{c.WasmEdge_ResultGetMessage(result)});
        std.process.exit(1);
    }

    // Instantiate
    result = c.WasmEdge_VMInstantiate(vm);
    if (!c.WasmEdge_ResultOK(result)) {
        std.debug.print("Failed to instantiate: {s}\n", .{c.WasmEdge_ResultGetMessage(result)});
        std.process.exit(1);
    }

    // Run _start
    const func_name = c.WasmEdge_StringCreateByCString("_start");
    defer c.WasmEdge_StringDelete(func_name);

    result = c.WasmEdge_VMExecute(vm, func_name, null, 0, null, 0);
    if (!c.WasmEdge_ResultOK(result)) {
        const msg = c.WasmEdge_ResultGetMessage(result);
        if (msg != null) {
            const msg_str = std.mem.span(msg);
            // Ignore normal exit
            if (std.mem.indexOf(u8, msg_str, "terminated") == null) {
                std.debug.print("Execution failed: {s}\n", .{msg_str});
                std.process.exit(1);
            }
        }
    }
}

/// Run a compiled WASM/AOT module directly (replacement for wasmedge CLI)
fn runWasm(allocator: std.mem.Allocator, args: []const [:0]const u8) !void {
    _ = allocator;

    const wasm_path = args[0];

    // Initialize WasmEdge
    c.WasmEdge_LogSetErrorLevel();

    // Create configure
    const conf = c.WasmEdge_ConfigureCreate();
    defer c.WasmEdge_ConfigureDelete(conf);

    // Enable WASI
    c.WasmEdge_ConfigureAddHostRegistration(conf, c.WasmEdge_HostRegistration_Wasi);

    // Create VM
    const vm = c.WasmEdge_VMCreate(conf, null);
    defer c.WasmEdge_VMDelete(vm);

    // Get WASI module and initialize
    const wasi_module = c.WasmEdge_VMGetImportModuleContext(vm, c.WasmEdge_HostRegistration_Wasi);

    // Setup WASI args - pass remaining args
    var wasi_args: [64][*c]const u8 = undefined;
    var wasi_argc: usize = 0;
    for (args) |arg| {
        if (wasi_argc < wasi_args.len) {
            wasi_args[wasi_argc] = arg.ptr;
            wasi_argc += 1;
        }
    }

    // Preopens - current dir and common paths
    var preopens: [4][*c]const u8 = undefined;
    preopens[0] = ".:.";
    preopens[1] = "/tmp:/private/tmp";

    var cwd_buf: [1024]u8 = undefined;
    const cwd = std.process.getCwd(&cwd_buf) catch ".";

    var cwd_preopen_buf: [2048]u8 = undefined;
    const cwd_preopen = std.fmt.bufPrintZ(&cwd_preopen_buf, "{s}:{s}", .{ cwd, cwd }) catch ".:.";
    preopens[2] = cwd_preopen.ptr;

    // Get WASM directory
    const wasm_dir = std.fs.path.dirname(wasm_path) orelse ".";
    var wasm_preopen_buf: [2048]u8 = undefined;
    const wasm_preopen = std.fmt.bufPrintZ(&wasm_preopen_buf, "{s}:{s}", .{ wasm_dir, wasm_dir }) catch ".:.";
    preopens[3] = wasm_preopen.ptr;

    c.WasmEdge_ModuleInstanceInitWASI(
        wasi_module,
        &wasi_args,
        @intCast(wasi_argc),
        null, // No env
        0,
        &preopens,
        4,
    );

    // Load module directly from provided path
    var result = c.WasmEdge_VMLoadWasmFromFile(vm, wasm_path.ptr);
    if (!c.WasmEdge_ResultOK(result)) {
        std.debug.print("Failed to load WASM: {s}\n", .{c.WasmEdge_ResultGetMessage(result)});
        std.process.exit(1);
    }

    // Validate
    result = c.WasmEdge_VMValidate(vm);
    if (!c.WasmEdge_ResultOK(result)) {
        std.debug.print("Failed to validate: {s}\n", .{c.WasmEdge_ResultGetMessage(result)});
        std.process.exit(1);
    }

    // Instantiate
    result = c.WasmEdge_VMInstantiate(vm);
    if (!c.WasmEdge_ResultOK(result)) {
        std.debug.print("Failed to instantiate: {s}\n", .{c.WasmEdge_ResultGetMessage(result)});
        std.process.exit(1);
    }

    // Run _start
    const func_name = c.WasmEdge_StringCreateByCString("_start");
    defer c.WasmEdge_StringDelete(func_name);

    result = c.WasmEdge_VMExecute(vm, func_name, null, 0, null, 0);
    if (!c.WasmEdge_ResultOK(result)) {
        const msg = c.WasmEdge_ResultGetMessage(result);
        if (msg != null) {
            const msg_str = std.mem.span(msg);
            // Ignore normal exit
            if (std.mem.indexOf(u8, msg_str, "terminated") == null) {
                std.debug.print("Execution failed: {s}\n", .{msg_str});
                std.process.exit(1);
            }
        }
    }
}
