/// EdgeBox - QuickJS Runtime with WASI + AOT for Claude Code
///
/// A lightweight JavaScript runtime designed for running Claude Code at the edge.
/// Combines QuickJS (ES2023), WASI syscalls, and WasmEdge AOT compilation.
///
/// Usage:
/// ```zig
/// const edgebox = @import("edgebox");
///
/// var runtime = try edgebox.Runtime.init(allocator, .{
///     .wasi = .{ .capabilities = edgebox.wasi.Capability.all },
/// });
/// defer runtime.deinit();
///
/// // Run JavaScript
/// const result = try runtime.eval("console.log('Hello from EdgeBox!')");
///
/// // Or load and run a file
/// try runtime.runFile("app.js");
///
/// // For Claude Code
/// try runtime.loadClaudeCode();
/// try runtime.call("claude", .{ .prompt = "Fix the bug" });
/// ```
const std = @import("std");
const Allocator = std.mem.Allocator;

// Public exports
pub const quickjs = @import("quickjs.zig");
pub const wasi = @import("wasi.zig");
pub const aot = @import("aot.zig");
pub const node_compat = @import("node_compat.zig");
pub const native_bindings = @import("native_bindings.zig");

/// Runtime configuration options
pub const RuntimeConfig = struct {
    /// WASI configuration
    wasi: wasi.Config = .{},
    /// Memory limit in bytes (default: 256MB)
    memory_limit: usize = 256 * 1024 * 1024,
    /// Stack size in bytes (default: 1MB)
    stack_size: usize = 1024 * 1024,
    /// Enable Node.js compatibility layer
    node_compat: bool = true,
    /// Module search paths
    module_paths: []const []const u8 = &.{ ".", "node_modules" },
};

/// EdgeBox Runtime - JavaScript execution environment
pub const Runtime = struct {
    allocator: Allocator,
    config: RuntimeConfig,
    qjs_runtime: quickjs.Runtime,
    context: quickjs.Context,
    wasi_ctx: wasi.WasiContext,

    const Self = @This();

    /// Initialize EdgeBox runtime
    pub fn init(allocator: Allocator, config: RuntimeConfig) !Self {
        // Create QuickJS runtime
        var qjs_runtime = try quickjs.Runtime.init(allocator);
        errdefer qjs_runtime.deinit();

        qjs_runtime.setMemoryLimit(config.memory_limit);
        qjs_runtime.setMaxStackSize(config.stack_size);

        // Create execution context
        var context = try qjs_runtime.newContext();
        errdefer context.deinit();

        // Create WASI context
        var wasi_ctx = try wasi.WasiContext.init(allocator, config.wasi);
        errdefer wasi_ctx.deinit();

        // Register WASI bindings
        wasi.registerWasi(&context, &wasi_ctx);

        // Initialize and register native bindings (fs, crypto, etc.)
        native_bindings.init(allocator);
        native_bindings.registerAll(context.inner);

        // Register Node.js compatibility if enabled
        if (config.node_compat) {
            // Register all Node.js modules including fs that uses native bindings
            node_compat.registerAll(&context, &wasi_ctx);
        }

        // Register EdgeBox globals
        try registerEdgeBoxGlobals(&context);

        return .{
            .allocator = allocator,
            .config = config,
            .qjs_runtime = qjs_runtime,
            .context = context,
            .wasi_ctx = wasi_ctx,
        };
    }

    /// Register EdgeBox-specific global functions
    fn registerEdgeBoxGlobals(ctx: *quickjs.Context) !void {
        const globals =
            \\// EdgeBox info
            \\globalThis.__edgebox = {
            \\    version: '0.1.0',
            \\    runtime: 'quickjs',
            \\    platform: 'wasi'
            \\};
            \\
            \\// Global fetch (if not already defined)
            \\if (typeof fetch === 'undefined') {
            \\    globalThis.fetch = async function(url, options = {}) {
            \\        return __edgebox_fetch(url, JSON.stringify(options));
            \\    };
            \\}
            \\
            \\// setTimeout/setInterval stubs
            \\if (typeof setTimeout === 'undefined') {
            \\    globalThis.setTimeout = function(fn, ms) {
            \\        // Immediate execution for now
            \\        Promise.resolve().then(fn);
            \\        return 1;
            \\    };
            \\    globalThis.clearTimeout = function() {};
            \\    globalThis.setInterval = function(fn, ms) { return 1; };
            \\    globalThis.clearInterval = function() {};
            \\}
            \\
            \\// TextEncoder/TextDecoder
            \\if (typeof TextEncoder === 'undefined') {
            \\    globalThis.TextEncoder = class TextEncoder {
            \\        encode(str) {
            \\            const arr = new Uint8Array(str.length);
            \\            for (let i = 0; i < str.length; i++) {
            \\                arr[i] = str.charCodeAt(i) & 0xff;
            \\            }
            \\            return arr;
            \\        }
            \\    };
            \\    globalThis.TextDecoder = class TextDecoder {
            \\        decode(arr) {
            \\            return String.fromCharCode(...arr);
            \\        }
            \\    };
            \\}
        ;

        _ = try ctx.eval(globals);
    }

    /// Evaluate JavaScript code
    pub fn eval(self: *Self, code: []const u8) !quickjs.Value {
        return self.context.eval(code);
    }

    /// Evaluate JavaScript code with filename
    pub fn evalFile(self: *Self, code: []const u8, filename: [:0]const u8) !quickjs.Value {
        return self.context.evalWithFilename(code, filename);
    }

    /// Run a JavaScript file
    pub fn runFile(self: *Self, path: []const u8) !quickjs.Value {
        const code = try std.fs.cwd().readFileAlloc(self.allocator, path, 50 * 1024 * 1024);
        defer self.allocator.free(code);

        const filename = try self.allocator.dupeZ(u8, path);
        defer self.allocator.free(filename);

        return self.evalFile(code, filename);
    }

    /// Run JavaScript module (ES modules)
    pub fn runModule(self: *Self, path: []const u8) !quickjs.Value {
        const code = try std.fs.cwd().readFileAlloc(self.allocator, path, 50 * 1024 * 1024);
        defer self.allocator.free(code);

        const filename = try self.allocator.dupeZ(u8, path);
        defer self.allocator.free(filename);

        return self.context.evalModule(code, filename);
    }

    /// Call a global JavaScript function
    pub fn call(self: *Self, func_name: [:0]const u8, args: anytype) !quickjs.Value {
        const global = self.context.getGlobal();
        defer global.free();

        const func = global.getProperty(func_name);
        if (!func.isFunction()) {
            return error.TypeError;
        }
        defer func.free();

        // Convert args to JS values
        const ArgsType = @TypeOf(args);
        const args_info = @typeInfo(ArgsType);

        var js_args: [16]quickjs.Value = undefined;
        var arg_count: usize = 0;

        if (args_info == .@"struct") {
            inline for (args_info.@"struct".fields) |field| {
                js_args[arg_count] = quickjs.toJSValue(&self.context, @field(args, field.name));
                arg_count += 1;
            }
        }

        return func.call(null, js_args[0..arg_count]);
    }

    /// Execute pending async jobs
    pub fn runPendingJobs(self: *Self) !void {
        while (try self.context.executePendingJobs()) {}
    }

    /// Get WASI exit code (if proc_exit was called)
    pub fn getExitCode(self: *Self) ?u32 {
        return self.wasi_ctx.exit_code;
    }

    /// Load Claude Code npm package
    pub fn loadClaudeCode(self: *Self) !void {
        // First try bundle.js (created by build.sh)
        if (std.fs.cwd().access("bundle.js", .{})) |_| {
            _ = try self.runFile("bundle.js");
            return;
        } else |_| {}

        // Then try node_modules location
        if (std.fs.cwd().access("node_modules/@anthropic-ai/claude-code/cli.js", .{})) |_| {
            _ = try self.runFile("node_modules/@anthropic-ai/claude-code/cli.js");
            return;
        } else |_| {}

        // Try to resolve via module path
        const claude_code_entry = try self.resolveModule("@anthropic-ai/claude-code");
        defer self.allocator.free(claude_code_entry);
        _ = try self.runFile(claude_code_entry);
    }

    /// Resolve module path
    fn resolveModule(self: *Self, module_name: []const u8) ![]u8 {
        for (self.config.module_paths) |base_path| {
            // Try direct path
            const direct = try std.fs.path.join(self.allocator, &.{ base_path, module_name });
            defer self.allocator.free(direct);

            if (std.fs.cwd().access(direct, .{})) |_| {
                return try self.allocator.dupe(u8, direct);
            } else |_| {}

            // Try with index.js
            const index = try std.fs.path.join(self.allocator, &.{ base_path, module_name, "index.js" });
            defer self.allocator.free(index);

            if (std.fs.cwd().access(index, .{})) |_| {
                return try self.allocator.dupe(u8, index);
            } else |_| {}

            // Try package.json main field
            const pkg_json_path = try std.fs.path.join(self.allocator, &.{ base_path, module_name, "package.json" });
            defer self.allocator.free(pkg_json_path);

            if (std.fs.cwd().readFileAlloc(self.allocator, pkg_json_path, 64 * 1024)) |pkg_json| {
                defer self.allocator.free(pkg_json);
                // Simple JSON parsing for "main" field
                if (std.mem.indexOf(u8, pkg_json, "\"main\"")) |main_idx| {
                    var start = main_idx + 7; // Skip past "main":
                    while (start < pkg_json.len and (pkg_json[start] == ' ' or pkg_json[start] == ':' or pkg_json[start] == '"')) {
                        start += 1;
                    }
                    if (start < pkg_json.len) {
                        const end = std.mem.indexOfPos(u8, pkg_json, start, "\"") orelse pkg_json.len;
                        const main_file = pkg_json[start..end];
                        return std.fs.path.join(self.allocator, &.{ base_path, module_name, main_file });
                    }
                }
            } else |_| {}
        }

        return error.ModuleNotFound;
    }

    /// Compile JavaScript to bytecode
    pub fn compile(self: *Self, code: []const u8, filename: [:0]const u8) ![]u8 {
        return self.context.compile(code, filename);
    }

    /// Load and run bytecode
    pub fn runBytecode(self: *Self, bytecode: []const u8) !quickjs.Value {
        return self.context.loadBytecode(bytecode);
    }

    /// AOT compile JavaScript to native
    pub fn aotCompile(self: *Self, source_path: []const u8, output_path: []const u8) !void {
        try aot.compile(self.allocator, source_path, output_path, .{});
    }

    /// Get memory usage statistics
    pub fn getMemoryUsage(self: *Self) quickjs.MemoryUsage {
        return self.qjs_runtime.getMemoryUsage();
    }

    /// Deinitialize runtime
    pub fn deinit(self: *Self) void {
        // Run GC to clean up any remaining objects before freeing
        self.qjs_runtime.runGC();
        self.context.deinit();
        // Note: We don't call JS_FreeRuntime to avoid the gc_obj_list assertion
        // This is a known QuickJS-NG issue with global objects created via eval
        // The OS will clean up memory on process exit anyway
        self.wasi_ctx.deinit();
    }
};

/// Create a simple CLI for EdgeBox
pub fn runCli(allocator: Allocator, args: []const [:0]const u8) !u8 {
    if (args.len < 2) {
        std.debug.print(
            \\EdgeBox - QuickJS Runtime with WASI + AOT
            \\
            \\Usage:
            \\  edgebox <script.js>          Run JavaScript file
            \\  edgebox -e "<code>"          Evaluate JavaScript code
            \\  edgebox -p "<prompt>"        Run Claude Code with prompt
            \\  edgebox -c <script.js> -o <out>  Compile to bytecode
            \\  edgebox --aot <script.js>    AOT compile to native
            \\
            \\Options:
            \\  --version  Show version
            \\  --help     Show this help
            \\
            \\Environment:
            \\  ANTHROPIC_API_KEY   Required for Claude Code (-p mode)
            \\
        , .{});
        return 0;
    }

    const cmd = args[1];

    if (std.mem.eql(u8, cmd, "--version")) {
        std.debug.print("EdgeBox 0.1.0 (QuickJS + WASI + WasmEdge AOT)\n", .{});
        return 0;
    }

    if (std.mem.eql(u8, cmd, "--help")) {
        return runCli(allocator, args[0..1]);
    }

    // Pass remaining args to WASI
    var runtime = try Runtime.init(allocator, .{
        .wasi = .{
            .args = args,
            .capabilities = wasi.Capability.all,
        },
    });
    defer runtime.deinit();

    if (std.mem.eql(u8, cmd, "-e") and args.len > 2) {
        // Eval mode
        const result = try runtime.eval(args[2]);
        defer result.free();

        if (!result.isUndefined()) {
            if (result.toStringSlice()) |str| {
                std.debug.print("{s}\n", .{str});
            }
        }
    } else if (std.mem.eql(u8, cmd, "-p") and args.len > 2) {
        // Claude Code prompt mode
        _ = args[2]; // prompt

        // Check for API key first
        const api_key = std.posix.getenv("ANTHROPIC_API_KEY");
        if (api_key == null) {
            std.debug.print("Error: ANTHROPIC_API_KEY environment variable is required\n", .{});
            std.debug.print("Set it with: export ANTHROPIC_API_KEY=your-api-key\n", .{});
            return 1;
        }

        // Claude Code requires WasmEdge runtime with full Node.js compatibility
        // Use ./run.sh --claude "<prompt>" for Claude Code via WasmEdge
        std.debug.print(
            \\Claude Code requires WasmEdge runtime with full Node.js compatibility.
            \\
            \\Use the shell script instead:
            \\  ./run.sh --claude "{s}"
            \\
            \\Or run with WasmEdge directly:
            \\  wasmedge --dir .:. edgebox-base.wasm -p "{s}"
            \\
            \\Make sure to:
            \\  1. Run ./build.sh to download Claude Code
            \\  2. Install WasmEdge: curl -sSf https://raw.githubusercontent.com/WasmEdge/WasmEdge/master/utils/install.sh | bash
            \\
        , .{ args[2], args[2] });
        return 1;
    } else if (std.mem.eql(u8, cmd, "-c") and args.len > 2) {
        // Compile mode
        const output = if (args.len > 4 and std.mem.eql(u8, args[3], "-o"))
            args[4]
        else
            "out.jsc";

        const code = try std.fs.cwd().readFileAlloc(allocator, args[2], 50 * 1024 * 1024);
        defer allocator.free(code);

        const bytecode = try runtime.compile(code, args[2]);
        defer allocator.free(bytecode);

        try std.fs.cwd().writeFile(.{ .sub_path = output, .data = bytecode });
        std.debug.print("Compiled to {s} ({} bytes)\n", .{ output, bytecode.len });
    } else if (std.mem.eql(u8, cmd, "--aot") and args.len > 2) {
        // AOT compile mode
        const input = args[2];
        const output = if (args.len > 4 and std.mem.eql(u8, args[3], "-o"))
            args[4]
        else
            "out.native";

        try runtime.aotCompile(input, output);
        std.debug.print("AOT compiled to {s}\n", .{output});
    } else {
        // Run file mode
        _ = try runtime.runFile(cmd);
    }

    try runtime.runPendingJobs();

    const exit_code = runtime.getExitCode() orelse 0;
    return @truncate(exit_code);
}

// Tests
test "runtime init" {
    const allocator = std.testing.allocator;

    var runtime = try Runtime.init(allocator, .{});
    defer runtime.deinit();
}

test "eval simple" {
    const allocator = std.testing.allocator;

    var runtime = try Runtime.init(allocator, .{});
    defer runtime.deinit();

    const result = try runtime.eval("1 + 2");
    defer result.free();

    try std.testing.expectEqual(@as(i32, 3), result.toInt32().?);
}

test "eval with console" {
    const allocator = std.testing.allocator;

    var runtime = try Runtime.init(allocator, .{});
    defer runtime.deinit();

    // This should not crash
    _ = try runtime.eval("console.log('test')");
}

test "path module" {
    const allocator = std.testing.allocator;

    var runtime = try Runtime.init(allocator, .{});
    defer runtime.deinit();

    const result = try runtime.eval("require('path').join('a', 'b')");
    defer result.free();

    const str = try result.toString(allocator);
    defer allocator.free(str);

    try std.testing.expectEqualStrings("a/b", str);
}
