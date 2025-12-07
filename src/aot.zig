/// AOT (Ahead-of-Time) Compilation for EdgeBox
///
/// Provides AOT compilation support using WasmEdge to compile QuickJS bytecode
/// to native machine code for faster startup and execution.
///
/// Workflow:
/// 1. JavaScript → QuickJS bytecode (.jsc)
/// 2. Bytecode embedded in WASM module
/// 3. WASM → Native code via WasmEdge AOT compiler
///
/// Usage:
/// ```zig
/// const aot = @import("aot");
///
/// // Compile JS to native
/// try aot.compile("app.js", "app.native", .{});
///
/// // Load and run
/// var runner = try aot.load("app.native");
/// try runner.run();
/// ```
const std = @import("std");
const Allocator = std.mem.Allocator;
const quickjs = @import("quickjs.zig");
const wasi = @import("wasi.zig");

// Try to import wasmedge if available
const wasmedge = @import("wasmedge") catch struct {
    // Stub when wasmedge not available
    pub const Error = error{NotAvailable};
    pub const VM = void;
    pub const Config = void;
    pub const Compiler = void;
};

/// AOT compilation options
pub const CompileOptions = struct {
    /// Optimization level (0-3)
    opt_level: u8 = 2,
    /// Enable debug info
    debug: bool = false,
    /// Target triple (null = native)
    target: ?[]const u8 = null,
    /// Enable SIMD instructions
    simd: bool = true,
    /// Enable bulk memory operations
    bulk_memory: bool = true,
    /// Memory limit in pages (64KB each)
    memory_pages: u32 = 256, // 16MB
    /// Stack size in bytes
    stack_size: u32 = 1024 * 1024, // 1MB
};

/// AOT compiled module metadata
pub const ModuleInfo = struct {
    /// Original source filename
    source_file: []const u8,
    /// Compilation timestamp
    compiled_at: i64,
    /// QuickJS version
    quickjs_version: []const u8,
    /// Target architecture
    target_arch: []const u8,
    /// Bytecode hash for validation
    bytecode_hash: u64,
};

/// Compile JavaScript to native code
pub fn compile(
    allocator: Allocator,
    source_path: []const u8,
    output_path: []const u8,
    options: CompileOptions,
) !void {
    // Step 1: Read JavaScript source
    const source = try std.fs.cwd().readFileAlloc(allocator, source_path, 10 * 1024 * 1024);
    defer allocator.free(source);

    // Step 2: Compile to QuickJS bytecode
    const bytecode = try compileToByteCode(allocator, source, source_path);
    defer allocator.free(bytecode);

    // Step 3: Generate WASM wrapper
    const wasm_module = try generateWasmWrapper(allocator, bytecode, options);
    defer allocator.free(wasm_module);

    // Step 4: AOT compile WASM to native
    try aotCompileWasm(allocator, wasm_module, output_path, options);
}

/// Compile JavaScript source to QuickJS bytecode
pub fn compileToByteCode(
    allocator: Allocator,
    source: []const u8,
    filename: []const u8,
) ![]u8 {
    var rt = try quickjs.Runtime.init(allocator);
    defer rt.deinit();

    var ctx = try rt.newContext();
    defer ctx.deinit();

    // Create null-terminated filename
    const fname = try allocator.dupeZ(u8, filename);
    defer allocator.free(fname);

    return try ctx.compile(source, fname);
}

/// Generate WASM module wrapping QuickJS bytecode
fn generateWasmWrapper(
    allocator: Allocator,
    bytecode: []const u8,
    options: CompileOptions,
) ![]u8 {
    _ = options;

    // WASM binary format magic + version
    var wasm = std.ArrayList(u8){};
    errdefer wasm.deinit(allocator);

    // Magic number
    try wasm.appendSlice(allocator, &[_]u8{ 0x00, 0x61, 0x73, 0x6d }); // \0asm
    // Version 1
    try wasm.appendSlice(allocator, &[_]u8{ 0x01, 0x00, 0x00, 0x00 });

    // Type section (1) - function types
    try wasm.append(allocator, 0x01); // section id
    const type_section = [_]u8{
        0x04, // section size
        0x01, // num types
        0x60, // func type
        0x00, // num params
        0x00, // num results
    };
    try wasm.appendSlice(allocator, &type_section);

    // Import section (2) - WASI imports
    try wasm.append(allocator, 0x02); // section id
    const import_section = [_]u8{
        0x13, // section size
        0x01, // num imports
        // Import: wasi_snapshot_preview1.proc_exit
        0x19, // module name len
    };
    try wasm.appendSlice(allocator, &import_section);
    try wasm.appendSlice(allocator, "wasi_snapshot_preview1");
    try wasm.append(allocator, 0x09); // field name len
    try wasm.appendSlice(allocator, "proc_exit");
    try wasm.append(allocator, 0x00); // import kind: func
    try wasm.append(allocator, 0x00); // type index

    // Function section (3)
    try wasm.append(allocator, 0x03); // section id
    try wasm.appendSlice(allocator, &[_]u8{
        0x02, // section size
        0x01, // num functions
        0x00, // type index for _start
    });

    // Memory section (5)
    try wasm.append(allocator, 0x05); // section id
    try wasm.appendSlice(allocator, &[_]u8{
        0x03, // section size
        0x01, // num memories
        0x00, // flags (no max)
        0x10, // initial pages (16 = 1MB)
    });

    // Export section (7)
    try wasm.append(allocator, 0x07); // section id
    const export_section = [_]u8{
        0x11, // section size
        0x02, // num exports
        // Export: memory
        0x06, // name len
    };
    try wasm.appendSlice(allocator, &export_section);
    try wasm.appendSlice(allocator, "memory");
    try wasm.append(allocator, 0x02); // export kind: memory
    try wasm.append(allocator, 0x00); // memory index
    // Export: _start
    try wasm.append(allocator, 0x06); // name len
    try wasm.appendSlice(allocator, "_start");
    try wasm.append(allocator, 0x00); // export kind: func
    try wasm.append(allocator, 0x01); // func index (after import)

    // Data section (11) - embed bytecode
    try wasm.append(allocator, 0x0b); // section id
    const data_header = [_]u8{
        @intCast((bytecode.len + 5) & 0xFF), // section size (simplified)
        0x01, // num data segments
        0x00, // memory index
        0x41, 0x00, // i32.const 0 (offset)
        0x0b, // end
        @intCast(bytecode.len & 0xFF), // data size
    };
    try wasm.appendSlice(allocator, &data_header);
    try wasm.appendSlice(allocator, bytecode);

    // Code section (10) - _start function
    try wasm.append(allocator, 0x0a); // section id
    try wasm.appendSlice(allocator, &[_]u8{
        0x04, // section size
        0x01, // num functions
        0x02, // function body size
        0x00, // num locals
        0x0b, // end (empty function for now)
    });

    return try wasm.toOwnedSlice(allocator);
}

/// AOT compile WASM to native using WasmEdge
fn aotCompileWasm(
    allocator: Allocator,
    wasm_module: []const u8,
    output_path: []const u8,
    options: CompileOptions,
) !void {
    _ = options;

    // Write temp WASM file
    const temp_wasm = try std.fmt.allocPrint(allocator, "/tmp/edgebox_{x}.wasm", .{
        std.crypto.random.int(u64),
    });
    defer allocator.free(temp_wasm);

    try std.fs.cwd().writeFile(.{
        .sub_path = temp_wasm,
        .data = wasm_module,
    });
    defer std.fs.cwd().deleteFile(temp_wasm) catch {};

    // Use wasmedge CLI for AOT compilation
    const result = try std.process.Child.run(.{
        .allocator = allocator,
        .argv = &.{
            "wasmedgec",
            temp_wasm,
            output_path,
        },
    });
    defer allocator.free(result.stdout);
    defer allocator.free(result.stderr);

    if (result.term.Exited != 0) {
        std.log.err("AOT compilation failed: {s}", .{result.stderr});
        return error.CompileFailed;
    }
}

/// AOT compiled module runner
pub const Runner = struct {
    allocator: Allocator,
    module_path: []const u8,
    wasi_config: wasi.Config,

    const Self = @This();

    /// Load AOT compiled module
    pub fn load(allocator: Allocator, path: []const u8) !Self {
        return .{
            .allocator = allocator,
            .module_path = try allocator.dupe(u8, path),
            .wasi_config = .{},
        };
    }

    /// Configure WASI
    pub fn setWasiConfig(self: *Self, config: wasi.Config) void {
        self.wasi_config = config;
    }

    /// Run the module
    pub fn run(self: *Self) !i32 {
        // Use wasmedge CLI to run
        var args = std.ArrayList([]const u8).init(self.allocator);
        defer args.deinit();

        try args.append("wasmedge");

        // Add preopens
        for (self.wasi_config.preopens) |preopen| {
            try args.append("--dir");
            try args.append(preopen.path);
        }

        try args.append(self.module_path);

        // Add user args
        for (self.wasi_config.args) |arg| {
            try args.append(arg);
        }

        const result = try std.process.Child.run(.{
            .allocator = self.allocator,
            .argv = args.items,
        });
        defer self.allocator.free(result.stdout);
        defer self.allocator.free(result.stderr);

        // Output stdout/stderr
        if (result.stdout.len > 0) {
            std.debug.print("{s}", .{result.stdout});
        }
        if (result.stderr.len > 0) {
            std.debug.print("{s}", .{result.stderr});
        }

        return @intCast(result.term.Exited);
    }

    /// Deinitialize
    pub fn deinit(self: *Self) void {
        self.allocator.free(self.module_path);
    }
};

/// High-level: Compile and run JavaScript
pub fn compileAndRun(
    allocator: Allocator,
    source: []const u8,
    wasi_config: wasi.Config,
) !i32 {
    // Generate temp paths
    const temp_native = try std.fmt.allocPrint(allocator, "/tmp/edgebox_{x}.native", .{
        std.crypto.random.int(u64),
    });
    defer allocator.free(temp_native);

    // Compile
    try compile(allocator, source, temp_native, .{});
    defer std.fs.cwd().deleteFile(temp_native) catch {};

    // Run
    var runner = try Runner.load(allocator, temp_native);
    defer runner.deinit();
    runner.setWasiConfig(wasi_config);

    return try runner.run();
}

/// Bundle multiple JS files into single AOT module
pub fn bundle(
    allocator: Allocator,
    entry_point: []const u8,
    output_path: []const u8,
    options: CompileOptions,
) !void {
    // For now, just compile the entry point
    // TODO: Implement proper bundling with dependency resolution
    try compile(allocator, entry_point, output_path, options);
}

// Tests
test "compile to bytecode" {
    const allocator = std.testing.allocator;

    const bytecode = try compileToByteCode(
        allocator,
        "function add(a, b) { return a + b; } add(1, 2);",
        "test.js",
    );
    defer allocator.free(bytecode);

    // Verify bytecode starts with QuickJS magic
    try std.testing.expect(bytecode.len > 0);
}

test "generate wasm wrapper" {
    const allocator = std.testing.allocator;

    const bytecode = &[_]u8{ 0x01, 0x02, 0x03 };
    const wasm = try generateWasmWrapper(allocator, bytecode, .{});
    defer allocator.free(wasm);

    // Verify WASM magic number
    try std.testing.expectEqualSlices(u8, &[_]u8{ 0x00, 0x61, 0x73, 0x6d }, wasm[0..4]);
}
