//! LLVM LTO C API Wrapper
//! Provides direct access to LLVM's Link-Time Optimization for frozen modules
//!
//! This avoids spawning external opt/llc processes and allows fine-grained control
//! over optimization passes.

const std = @import("std");

// LLVM LTO C API bindings
const c = @cImport({
    @cInclude("llvm-c/lto.h");
});

pub const LtoModule = c.lto_module_t;
pub const LtoCodeGen = c.lto_code_gen_t;

pub const Error = error{
    ModuleLoadFailed,
    CodeGenFailed,
    OptimizeFailed,
    CompileFailed,
};

/// Get LLVM version string
pub fn getVersion() [*:0]const u8 {
    return c.lto_get_version();
}

/// Get last error message
pub fn getError() ?[*:0]const u8 {
    const msg = c.lto_get_error_message();
    if (msg) |m| {
        return m;
    }
    return null;
}

/// Load a module from bitcode file
pub fn loadModule(path: [*:0]const u8) Error!LtoModule {
    const mod = c.lto_module_create(path);
    if (mod == null) {
        return Error.ModuleLoadFailed;
    }
    return mod;
}

/// Load a module from memory
pub fn loadModuleFromMemory(data: []const u8, path: [*:0]const u8) Error!LtoModule {
    const mod = c.lto_module_create_from_memory_with_path(data.ptr, data.len, path);
    if (mod == null) {
        return Error.ModuleLoadFailed;
    }
    return mod;
}

/// Free a module
pub fn freeModule(mod: LtoModule) void {
    c.lto_module_dispose(mod);
}

/// Create an LTO code generator
pub fn createCodeGen() Error!LtoCodeGen {
    const cg = c.lto_codegen_create();
    if (cg == null) {
        return Error.CodeGenFailed;
    }
    return cg;
}

/// Free a code generator
pub fn freeCodeGen(cg: LtoCodeGen) void {
    c.lto_codegen_dispose(cg);
}

/// Add a module to the code generator
/// Returns false on success, true on error (following LLVM convention)
pub fn addModule(cg: LtoCodeGen, mod: LtoModule) bool {
    return c.lto_codegen_add_module(cg, mod);
}

/// Run LTO optimization passes
/// Returns false on success, true on error (following LLVM convention)
pub fn optimize(cg: LtoCodeGen) Error!void {
    if (c.lto_codegen_optimize(cg)) {
        return Error.OptimizeFailed;
    }
}

/// Compile to native object code
pub fn compile(cg: LtoCodeGen) Error![]const u8 {
    var len: usize = 0;
    const ptr = c.lto_codegen_compile_optimized(cg, &len);
    if (ptr == null) {
        return Error.CompileFailed;
    }
    const byte_ptr: [*]const u8 = @ptrCast(ptr);
    return byte_ptr[0..len];
}

/// Compile and write to file
pub fn compileToFile(cg: LtoCodeGen, path: [*:0]const u8) Error!void {
    if (c.lto_codegen_compile_to_file(cg, @ptrCast(&path))) {
        return Error.CompileFailed;
    }
}

/// Apply LTO optimization to a bitcode file and produce an object file
pub fn optimizeBitcode(bc_path: []const u8, obj_path: []const u8) Error!void {
    var path_buf: [4096]u8 = undefined;
    const bc_path_z = std.fmt.bufPrintZ(&path_buf, "{s}", .{bc_path}) catch return Error.ModuleLoadFailed;

    // Load the module
    const mod = try loadModule(bc_path_z);
    defer freeModule(mod);

    // Create code generator
    const cg = try createCodeGen();
    defer freeCodeGen(cg);

    // Add module - returns true on error
    if (addModule(cg, mod)) {
        return Error.CodeGenFailed;
    }

    // Run optimization
    try optimize(cg);

    // Compile to object
    const obj_data = try compile(cg);

    // Write to file
    var obj_path_buf: [4096]u8 = undefined;
    const obj_path_slice = std.fmt.bufPrint(&obj_path_buf, "{s}", .{obj_path}) catch return Error.CompileFailed;
    const file = std.fs.cwd().createFile(obj_path_slice, .{}) catch return Error.CompileFailed;
    defer file.close();
    file.writeAll(obj_data) catch return Error.CompileFailed;
}

/// Test if LLVM LTO is available
pub fn isAvailable() bool {
    const version = getVersion();
    return version[0] != 0;
}
