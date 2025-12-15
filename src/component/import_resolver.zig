/// Component Model Import Resolver for WAMR
/// Bridges WAMR's flat calling convention to Component Model's rich types
///
/// Phase 8a MVP: Timer interface only
/// This module registers Component Model imports as WAMR native functions,
/// allowing WASM modules to call Component Model interfaces.

const std = @import("std");
const NativeRegistry = @import("native_registry.zig").NativeRegistry;
const Value = @import("native_registry.zig").Value;
const ProcessOutput = @import("native_registry.zig").ProcessOutput;
const SpawnOptions = @import("native_registry.zig").SpawnOptions;
const HttpHeader = @import("native_registry.zig").HttpHeader;
const HttpRequest = @import("native_registry.zig").HttpRequest;
const HttpResponse = @import("native_registry.zig").HttpResponse;

// WAMR C API
const c = @cImport({
    @cInclude("wasm_export.h");
});

/// Global timer registry - set by registerTimerImports()
var timer_registry: *NativeRegistry = undefined;

// ============================================================================
// ABI Lowering: Core Helper Functions
// ============================================================================
// These functions bridge between WAMR's flat calling convention and
// Component Model's rich types by managing WASM linear memory.

/// Read string from WASM memory (lowering: WASM → Host)
///
/// Validates WASM memory bounds and returns a borrowed reference to the string.
/// The returned slice is valid only until the next WASM operation.
///
/// Arguments:
///   exec_env: WAMR execution environment
///   ptr: WASM memory offset (u32)
///   len: String length in bytes
///
/// Returns:
///   Borrowed string slice if valid, null if invalid pointer or out of bounds
fn readString(exec_env: c.wasm_exec_env_t, ptr: u32, len: u32) ?[]const u8 {
    const module_inst = c.wasm_runtime_get_module_inst(exec_env);
    if (module_inst == null) return null;

    // Validate WASM memory bounds
    if (!c.wasm_runtime_validate_app_addr(module_inst, ptr, len)) return null;

    // Convert WASM offset to native pointer
    const native_ptr = c.wasm_runtime_addr_app_to_native(module_inst, ptr);
    if (native_ptr == null) return null;

    const bytes: [*]const u8 = @ptrCast(native_ptr);
    return bytes[0..len];
}

/// Allocate memory in WASM heap (lifting: Host → WASM)
///
/// Allocates memory in the WASM module's linear memory. The allocated memory
/// is owned by the WASM module and must be freed by WASM code or by calling
/// freeWasmMemory().
///
/// Arguments:
///   exec_env: WAMR execution environment
///   size: Number of bytes to allocate
///
/// Returns:
///   WASM memory offset (u32) if successful, 0 on allocation failure
fn allocWasmMemory(exec_env: c.wasm_exec_env_t, size: u32) u32 {
    const module_inst = c.wasm_runtime_get_module_inst(exec_env);
    if (module_inst == null) return 0;

    var native_ptr: ?*anyopaque = null;
    const app_offset = c.wasm_runtime_module_malloc(
        module_inst,
        size,
        @ptrCast(&native_ptr),
    );

    if (app_offset == 0 or native_ptr == null) return 0;
    return @intCast(app_offset & 0xFFFFFFFF);
}

/// Write data to WASM memory
///
/// Writes a byte slice to WASM linear memory at the specified offset.
/// Validates bounds before writing.
///
/// Arguments:
///   exec_env: WAMR execution environment
///   ptr: WASM memory offset to write to
///   data: Byte slice to write
///
/// Returns:
///   true if write succeeded, false if invalid pointer or out of bounds
fn writeString(exec_env: c.wasm_exec_env_t, ptr: u32, data: []const u8) bool {
    const module_inst = c.wasm_runtime_get_module_inst(exec_env);
    if (module_inst == null) return false;

    // Check size fits in u32
    if (data.len > 0xFFFFFFFF) return false;

    // Validate WASM memory bounds
    if (!c.wasm_runtime_validate_app_addr(module_inst, ptr, @intCast(data.len))) {
        return false;
    }

    // Convert WASM offset to native pointer
    const native_ptr = c.wasm_runtime_addr_app_to_native(module_inst, ptr);
    if (native_ptr == null) return false;

    // Copy data to WASM memory
    const dest: [*]u8 = @ptrCast(native_ptr);
    @memcpy(dest[0..data.len], data);
    return true;
}

/// Free WASM memory
///
/// Frees memory previously allocated with allocWasmMemory().
///
/// Arguments:
///   exec_env: WAMR execution environment
///   ptr: WASM memory offset to free
fn freeWasmMemory(exec_env: c.wasm_exec_env_t, ptr: u32) void {
    const module_inst = c.wasm_runtime_get_module_inst(exec_env);
    if (module_inst == null) return;
    c.wasm_runtime_module_free(module_inst, ptr);
}

/// Allocate and write string to WASM memory (convenience)
///
/// Combines allocWasmMemory() and writeString() into a single operation.
/// Useful for returning strings from Component Model to WASM.
///
/// Arguments:
///   exec_env: WAMR execution environment
///   data: String to allocate and write
///
/// Returns:
///   Struct with ptr (WASM offset) and len, or {0, 0} on failure
fn allocAndWriteString(exec_env: c.wasm_exec_env_t, data: []const u8) struct { ptr: u32, len: u32 } {
    if (data.len == 0) return .{ .ptr = 0, .len = 0 };

    const ptr = allocWasmMemory(exec_env, @intCast(data.len));
    if (ptr == 0) return .{ .ptr = 0, .len = 0 };

    if (!writeString(exec_env, ptr, data)) {
        freeWasmMemory(exec_env, ptr);
        return .{ .ptr = 0, .len = 0 };
    }

    return .{ .ptr = ptr, .len = @intCast(data.len) };
}

/// Read u32 value from WASM memory
///
/// Reads a single u32 value from WASM linear memory at the specified offset.
/// Used for reading struct fields, list counts, and other scalar values.
///
/// Arguments:
///   exec_env: WAMR execution environment
///   ptr: WASM memory offset (u32)
///
/// Returns:
///   u32 value if valid, 0 if invalid pointer or out of bounds
fn readU32(exec_env: c.wasm_exec_env_t, ptr: u32) u32 {
    const module_inst = c.wasm_runtime_get_module_inst(exec_env);
    if (module_inst == null) return 0;

    // Validate WASM memory bounds (4 bytes for u32)
    if (!c.wasm_runtime_validate_app_addr(module_inst, ptr, 4)) return 0;

    // Convert WASM offset to native pointer
    const native_ptr = c.wasm_runtime_addr_app_to_native(module_inst, ptr);
    if (native_ptr == null) return 0;

    // Read u32 value
    const value_ptr: *u32 = @ptrCast(@alignCast(native_ptr));
    return value_ptr.*;
}

/// Write u32 value to WASM memory
///
/// Writes a single u32 value to WASM linear memory at the specified offset.
/// Used for writing struct fields and status values.
///
/// Arguments:
///   exec_env: WAMR execution environment
///   ptr: WASM memory offset to write to
///   value: u32 value to write
///
/// Returns:
///   true if write succeeded, false if invalid pointer or out of bounds
fn writeU32(exec_env: c.wasm_exec_env_t, ptr: u32, value: u32) bool {
    const module_inst = c.wasm_runtime_get_module_inst(exec_env);
    if (module_inst == null) return false;

    // Validate WASM memory bounds (4 bytes for u32)
    if (!c.wasm_runtime_validate_app_addr(module_inst, ptr, 4)) return false;

    // Convert WASM offset to native pointer
    const native_ptr = c.wasm_runtime_addr_app_to_native(module_inst, ptr);
    if (native_ptr == null) return false;

    // Write u32 value
    const value_ptr: *u32 = @ptrCast(@alignCast(native_ptr));
    value_ptr.* = value;
    return true;
}

/// Write u8 value to WASM memory
///
/// Arguments:
///   exec_env: WAMR execution environment
///   ptr: WASM memory offset to write to
///   value: u8 value to write
///
/// Returns:
///   true if write succeeded, false if invalid pointer or out of bounds
fn writeU8(exec_env: c.wasm_exec_env_t, ptr: u32, value: u8) bool {
    const module_inst = c.wasm_runtime_get_module_inst(exec_env);
    if (module_inst == null) return false;

    // Validate WASM memory bounds (1 byte for u8)
    if (!c.wasm_runtime_validate_app_addr(module_inst, ptr, 1)) return false;

    // Convert WASM offset to native pointer
    const native_ptr = c.wasm_runtime_addr_app_to_native(module_inst, ptr);
    if (native_ptr == null) return false;

    // Write u8 value
    const value_ptr: *u8 = @ptrCast(native_ptr);
    value_ptr.* = value;
    return true;
}

// ============================================================================
// Timer Interface (Phase 8a MVP)
// ============================================================================

/// WAMR bridge function for timer::set-timeout
/// Maps WAMR flat calling convention to Component Model
///
/// WAMR signature: (i)I  (u32 → u64)
/// Component Model: set-timeout: func(delay: u32) -> u64
fn wamr_timer_set_timeout(
    exec_env: c.wasm_exec_env_t,
    delay: u32,
) callconv(.c) u64 {
    _ = exec_env; // Unused for simple primitive types

    // Call Component Model through NativeRegistry
    const args = [_]Value{Value{ .u32 = delay }};
    const result = timer_registry.call("timer", "set-timeout", &args) catch {
        // Error calling Component Model - return invalid timer ID
        return 0;
    };

    // Extract u64 timer ID from result
    return result.asU64() catch 0;
}

/// WAMR bridge function for timer::clear-timeout
/// WAMR signature: (I)  (u64 → void)
/// Component Model: clear-timeout: func(id: u64)
fn wamr_timer_clear_timeout(
    exec_env: c.wasm_exec_env_t,
    timer_id: u64,
) callconv(.c) void {
    _ = exec_env;

    const args = [_]Value{Value{ .u64 = timer_id }};
    _ = timer_registry.call("timer", "clear-timeout", &args) catch {
        // Ignore errors for clear-timeout (best effort)
        return;
    };
}

/// WAMR bridge function for timer::set-interval
/// WAMR signature: (i)I  (u32 → u64)
/// Component Model: set-interval: func(delay: u32) -> u64
fn wamr_timer_set_interval(
    exec_env: c.wasm_exec_env_t,
    delay: u32,
) callconv(.c) u64 {
    _ = exec_env;

    const args = [_]Value{Value{ .u32 = delay }};
    const result = timer_registry.call("timer", "set-interval", &args) catch {
        return 0;
    };

    return result.asU64() catch 0;
}

/// Native symbols for WAMR registration (timer interface)
var timer_symbols = [_]c.NativeSymbol{
    .{
        .symbol = "set-timeout",
        .func_ptr = @ptrCast(@constCast(&wamr_timer_set_timeout)),
        .signature = "(i)I", // u32 → u64
        .attachment = null,
    },
    .{
        .symbol = "clear-timeout",
        .func_ptr = @ptrCast(@constCast(&wamr_timer_clear_timeout)),
        .signature = "(I)", // u64 → void
        .attachment = null,
    },
    .{
        .symbol = "set-interval",
        .func_ptr = @ptrCast(@constCast(&wamr_timer_set_interval)),
        .signature = "(i)I", // u32 → u64
        .attachment = null,
    },
};

/// Register timer imports with WAMR
/// Must be called before loading WASM modules that import timer functions
///
/// Arguments:
///   registry: NativeRegistry containing timer implementation
pub fn registerTimerImports(registry: *NativeRegistry) void {
    timer_registry = registry;

    // Register native symbols with WAMR
    // Module name "timer" matches WIT interface name
    const result = c.wasm_runtime_register_natives(
        "timer",
        &timer_symbols,
        timer_symbols.len,
    );

    if (!result) {
        std.debug.print("[import_resolver] Warning: Failed to register timer imports\n", .{});
    }
}

// ============================================================================
// Filesystem Interface (Phase 8b)
// ============================================================================

/// Global filesystem registry - set by registerFilesystemImports()
var fs_registry: *NativeRegistry = undefined;

/// Filesystem error enum (matches filesystem_impl.zig)
const FsError = enum(u32) {
    not_found = 0,
    permission_denied = 1,
    already_exists = 2,
    invalid_path = 3,
    not_a_directory = 4,
    not_a_file = 5,
    directory_not_empty = 6,
    io_error = 7,
    invalid_encoding = 8,
};

/// filesystem::exists: func(path: string) -> bool
/// WAMR signature: (ii)i
fn wamr_filesystem_exists(
    exec_env: c.wasm_exec_env_t,
    path_ptr: u32,
    path_len: u32,
) callconv(.c) i32 {
    const path = readString(exec_env, path_ptr, path_len) orelse {
        return 0; // Invalid path = doesn't exist
    };

    const args = [_]Value{Value{ .string = path }};
    const result = fs_registry.call("filesystem", "exists", &args) catch {
        return 0; // Error = doesn't exist
    };

    const exists_bool = result.asBool() catch false;
    return if (exists_bool) 1 else 0;
}

/// filesystem::write-file: func(path: string, data: string) -> result<_, fs-error>
/// WAMR signature: (iiii)i
fn wamr_filesystem_write_file(
    exec_env: c.wasm_exec_env_t,
    path_ptr: u32,
    path_len: u32,
    data_ptr: u32,
    data_len: u32,
) callconv(.c) i32 {
    // Read inputs
    const path = readString(exec_env, path_ptr, path_len) orelse {
        return @intCast(@intFromEnum(FsError.invalid_path));
    };
    const data = readString(exec_env, data_ptr, data_len) orelse {
        return @intCast(@intFromEnum(FsError.io_error));
    };

    // Call Component Model
    const args = [_]Value{
        Value{ .string = path },
        Value{ .string = data },
    };
    const result = fs_registry.call("filesystem", "write-file", &args) catch {
        return @intCast(@intFromEnum(FsError.io_error));
    };

    // Handle result<_, fs-error>
    if (result.isErr()) {
        return @intCast(result.asErr() catch @intFromEnum(FsError.io_error));
    }

    return 0; // Success
}

/// filesystem::read-file: func(path: string, encoding: u32) -> result<string, fs-error>
/// WAMR signature: (iiii*i)i
fn wamr_filesystem_read_file(
    exec_env: c.wasm_exec_env_t,
    path_ptr: u32,
    path_len: u32,
    encoding: u32,
    out_ptr_ptr: u32, // Where to write result ptr
    out_len_ptr: u32, // Where to write result len
) callconv(.c) i32 {
    // 1. Read input
    const path = readString(exec_env, path_ptr, path_len) orelse {
        return @intCast(@intFromEnum(FsError.invalid_path));
    };

    // 2. Call Component Model
    const args = [_]Value{
        Value{ .string = path },
        Value{ .u32 = encoding },
    };
    const result = fs_registry.call("filesystem", "read-file", &args) catch {
        return @intCast(@intFromEnum(FsError.io_error));
    };

    // 3. Handle result<string, fs-error>
    if (result.isErr()) {
        return @intCast(result.asErr() catch @intFromEnum(FsError.io_error));
    }

    // Success - allocate in WASM memory
    const content = result.asOkString() catch {
        return @intCast(@intFromEnum(FsError.io_error));
    };

    const wasm_alloc = allocAndWriteString(exec_env, content);
    if (wasm_alloc.ptr == 0 and content.len > 0) {
        return @intCast(@intFromEnum(FsError.io_error));
    }

    // 4. Write result ptr/len to WASM memory
    const module_inst = c.wasm_runtime_get_module_inst(exec_env);
    if (module_inst == null) {
        freeWasmMemory(exec_env, wasm_alloc.ptr);
        return @intCast(@intFromEnum(FsError.io_error));
    }

    // Write ptr
    const out_ptr_native = c.wasm_runtime_addr_app_to_native(module_inst, out_ptr_ptr);
    if (out_ptr_native == null) {
        freeWasmMemory(exec_env, wasm_alloc.ptr);
        return @intCast(@intFromEnum(FsError.io_error));
    }
    const out_ptr_dest: *u32 = @ptrCast(@alignCast(out_ptr_native));
    out_ptr_dest.* = wasm_alloc.ptr;

    // Write len
    const out_len_native = c.wasm_runtime_addr_app_to_native(module_inst, out_len_ptr);
    if (out_len_native == null) {
        freeWasmMemory(exec_env, wasm_alloc.ptr);
        return @intCast(@intFromEnum(FsError.io_error));
    }
    const out_len_dest: *u32 = @ptrCast(@alignCast(out_len_native));
    out_len_dest.* = wasm_alloc.len;

    return 0; // Success
}

/// filesystem::stat: func(path: string) -> result<file-stat, fs-error>
/// WAMR signature: (ii*i)i
/// FileStat is written to WASM memory as 7 fields (40 bytes total):
///   size: u64 (8 bytes)
///   mode: u32 (4 bytes)
///   is_file: bool (1 byte, padded to 4)
///   is_directory: bool (1 byte, padded to 4)
///   modified_time: i64 (8 bytes)
///   created_time: i64 (8 bytes)
///   accessed_time: i64 (8 bytes)
fn wamr_filesystem_stat(
    exec_env: c.wasm_exec_env_t,
    path_ptr: u32,
    path_len: u32,
    out_stat_ptr: u32, // Pointer to FileStat struct in WASM memory
) callconv(.c) i32 {
    // 1. Read input
    const path = readString(exec_env, path_ptr, path_len) orelse {
        return @intCast(@intFromEnum(FsError.invalid_path));
    };

    // 2. Call Component Model
    const args = [_]Value{Value{ .string = path }};
    const result = fs_registry.call("filesystem", "stat", &args) catch {
        return @intCast(@intFromEnum(FsError.io_error));
    };

    // 3. Handle result<file-stat, fs-error>
    if (result.isErr()) {
        return @intCast(result.asErr() catch @intFromEnum(FsError.io_error));
    }

    // Success - extract FileStat
    const file_stat = result.asOkFileStat() catch {
        return @intCast(@intFromEnum(FsError.io_error));
    };

    // 4. Write FileStat to WASM memory
    const module_inst = c.wasm_runtime_get_module_inst(exec_env);
    if (module_inst == null) {
        return @intCast(@intFromEnum(FsError.io_error));
    }

    // Validate WASM memory bounds (7 fields = 40 bytes with padding)
    const stat_size: u32 = 40;
    if (!c.wasm_runtime_validate_app_addr(module_inst, out_stat_ptr, stat_size)) {
        return @intCast(@intFromEnum(FsError.io_error));
    }

    // Convert to native pointer
    const stat_native = c.wasm_runtime_addr_app_to_native(module_inst, out_stat_ptr);
    if (stat_native == null) {
        return @intCast(@intFromEnum(FsError.io_error));
    }

    // Write fields (aligned)
    const bytes: [*]u8 = @ptrCast(stat_native);
    var offset: usize = 0;

    // size: u64
    const size_ptr: *u64 = @ptrCast(@alignCast(bytes + offset));
    size_ptr.* = file_stat.size;
    offset += 8;

    // mode: u32
    const mode_ptr: *u32 = @ptrCast(@alignCast(bytes + offset));
    mode_ptr.* = file_stat.mode;
    offset += 4;

    // is_file: bool (as u32 for alignment)
    const is_file_ptr: *u32 = @ptrCast(@alignCast(bytes + offset));
    is_file_ptr.* = if (file_stat.is_file) 1 else 0;
    offset += 4;

    // is_directory: bool (as u32 for alignment)
    const is_dir_ptr: *u32 = @ptrCast(@alignCast(bytes + offset));
    is_dir_ptr.* = if (file_stat.is_directory) 1 else 0;
    offset += 4;

    // modified_time: i64
    const mtime_ptr: *i64 = @ptrCast(@alignCast(bytes + offset));
    mtime_ptr.* = file_stat.modified_time;
    offset += 8;

    // created_time: i64
    const ctime_ptr: *i64 = @ptrCast(@alignCast(bytes + offset));
    ctime_ptr.* = file_stat.created_time;
    offset += 8;

    // accessed_time: i64
    const atime_ptr: *i64 = @ptrCast(@alignCast(bytes + offset));
    atime_ptr.* = file_stat.accessed_time;

    return 0; // Success
}

/// filesystem::remove-file: func(path: string) -> result<_, fs-error>
/// WAMR signature: (ii)i
fn wamr_filesystem_remove_file(
    exec_env: c.wasm_exec_env_t,
    path_ptr: u32,
    path_len: u32,
) callconv(.c) i32 {
    const path = readString(exec_env, path_ptr, path_len) orelse {
        return @intCast(@intFromEnum(FsError.invalid_path));
    };

    const args = [_]Value{Value{ .string = path }};
    const result = fs_registry.call("filesystem", "remove-file", &args) catch {
        return @intCast(@intFromEnum(FsError.io_error));
    };

    if (result.isErr()) {
        return @intCast(result.asErr() catch @intFromEnum(FsError.io_error));
    }

    return 0; // Success
}

/// filesystem::rename: func(old-path: string, new-path: string) -> result<_, fs-error>
/// WAMR signature: (iiii)i
fn wamr_filesystem_rename(
    exec_env: c.wasm_exec_env_t,
    old_path_ptr: u32,
    old_path_len: u32,
    new_path_ptr: u32,
    new_path_len: u32,
) callconv(.c) i32 {
    const old_path = readString(exec_env, old_path_ptr, old_path_len) orelse {
        return @intCast(@intFromEnum(FsError.invalid_path));
    };
    const new_path = readString(exec_env, new_path_ptr, new_path_len) orelse {
        return @intCast(@intFromEnum(FsError.invalid_path));
    };

    const args = [_]Value{
        Value{ .string = old_path },
        Value{ .string = new_path },
    };
    const result = fs_registry.call("filesystem", "rename", &args) catch {
        return @intCast(@intFromEnum(FsError.io_error));
    };

    if (result.isErr()) {
        return @intCast(result.asErr() catch @intFromEnum(FsError.io_error));
    }

    return 0; // Success
}

/// filesystem::copy-file: func(src: string, dest: string) -> result<_, fs-error>
/// WAMR signature: (iiii)i
fn wamr_filesystem_copy_file(
    exec_env: c.wasm_exec_env_t,
    src_ptr: u32,
    src_len: u32,
    dest_ptr: u32,
    dest_len: u32,
) callconv(.c) i32 {
    const src = readString(exec_env, src_ptr, src_len) orelse {
        return @intCast(@intFromEnum(FsError.invalid_path));
    };
    const dest = readString(exec_env, dest_ptr, dest_len) orelse {
        return @intCast(@intFromEnum(FsError.invalid_path));
    };

    const args = [_]Value{
        Value{ .string = src },
        Value{ .string = dest },
    };
    const result = fs_registry.call("filesystem", "copy-file", &args) catch {
        return @intCast(@intFromEnum(FsError.io_error));
    };

    if (result.isErr()) {
        return @intCast(result.asErr() catch @intFromEnum(FsError.io_error));
    }

    return 0; // Success
}

/// filesystem::append-file: func(path: string, data: string) -> result<_, fs-error>
/// WAMR signature: (iiii)i
fn wamr_filesystem_append_file(
    exec_env: c.wasm_exec_env_t,
    path_ptr: u32,
    path_len: u32,
    data_ptr: u32,
    data_len: u32,
) callconv(.c) i32 {
    const path = readString(exec_env, path_ptr, path_len) orelse {
        return @intCast(@intFromEnum(FsError.invalid_path));
    };
    const data = readString(exec_env, data_ptr, data_len) orelse {
        return @intCast(@intFromEnum(FsError.invalid_path));
    };

    const args = [_]Value{
        Value{ .string = path },
        Value{ .string = data },
    };
    const result = fs_registry.call("filesystem", "append-file", &args) catch {
        return @intCast(@intFromEnum(FsError.io_error));
    };

    if (result.isErr()) {
        return @intCast(result.asErr() catch @intFromEnum(FsError.io_error));
    }

    return 0; // Success
}

/// filesystem::read-dir: func(path: string) -> result<list<dir-entry>, fs-error>
/// WAMR signature: (iiii)i
/// Returns list<dir-entry> where each entry is { name: string, is_file: bool, is_directory: bool }
fn wamr_filesystem_read_dir(
    exec_env: c.wasm_exec_env_t,
    path_ptr: u32,
    path_len: u32,
    out_ptr_ptr: u32,
    out_len_ptr: u32,
) callconv(.c) i32 {
    const path = readString(exec_env, path_ptr, path_len) orelse {
        return @intCast(@intFromEnum(FsError.invalid_path));
    };

    const args = [_]Value{
        Value{ .string = path },
    };
    const result = fs_registry.call("filesystem", "read-dir", &args) catch {
        return @intCast(@intFromEnum(FsError.io_error));
    };

    if (result.isErr()) {
        return @intCast(result.asErr() catch @intFromEnum(FsError.io_error));
    }

    // Get the DirEntry list
    const entries = result.asOkDirEntries() catch {
        return @intCast(@intFromEnum(FsError.io_error));
    };

    // Allocate WASM memory for list<dir-entry>
    // Each entry is 12 bytes: name_ptr (4) + name_len (4) + is_file (1) + is_directory (1) + padding (2)
    const entry_size = 12;
    const list_size = entries.len * entry_size;
    const list_ptr = allocWasmMemory(exec_env, @intCast(list_size));
    if (list_ptr == 0 and list_size > 0) {
        return @intCast(@intFromEnum(FsError.io_error));
    }

    // Write each entry
    for (entries, 0..) |entry, i| {
        // Allocate memory for name string
        const name_ptr = allocWasmMemory(exec_env, @intCast(entry.name.len));
        if (name_ptr == 0 and entry.name.len > 0) {
            return @intCast(@intFromEnum(FsError.io_error));
        }
        _ = writeString(exec_env, name_ptr, entry.name);

        // Write entry struct
        const entry_offset = list_ptr + @as(u32, @intCast(i)) * entry_size;
        _ = writeU32(exec_env, entry_offset, name_ptr);
        _ = writeU32(exec_env, entry_offset + 4, @intCast(entry.name.len));
        _ = writeU8(exec_env, entry_offset + 8, if (entry.is_file) 1 else 0);
        _ = writeU8(exec_env, entry_offset + 9, if (entry.is_directory) 1 else 0);
    }

    _ = writeU32(exec_env, out_ptr_ptr, list_ptr);
    _ = writeU32(exec_env, out_len_ptr, @intCast(entries.len));
    return 0; // Success
}

/// filesystem::mkdir: func(path: string, recursive: bool) -> result<_, fs-error>
/// WAMR signature: (iii)i
fn wamr_filesystem_mkdir(
    exec_env: c.wasm_exec_env_t,
    path_ptr: u32,
    path_len: u32,
    recursive: i32,
) callconv(.c) i32 {
    const path = readString(exec_env, path_ptr, path_len) orelse {
        return @intCast(@intFromEnum(FsError.invalid_path));
    };

    const args = [_]Value{
        Value{ .string = path },
        Value{ .bool = recursive != 0 },
    };
    const result = fs_registry.call("filesystem", "mkdir", &args) catch {
        return @intCast(@intFromEnum(FsError.io_error));
    };

    if (result.isErr()) {
        return @intCast(result.asErr() catch @intFromEnum(FsError.io_error));
    }

    return 0; // Success
}

/// filesystem::remove-dir: func(path: string, recursive: bool) -> result<_, fs-error>
/// WAMR signature: (iii)i
fn wamr_filesystem_remove_dir(
    exec_env: c.wasm_exec_env_t,
    path_ptr: u32,
    path_len: u32,
    recursive: i32,
) callconv(.c) i32 {
    const path = readString(exec_env, path_ptr, path_len) orelse {
        return @intCast(@intFromEnum(FsError.invalid_path));
    };

    const args = [_]Value{
        Value{ .string = path },
        Value{ .bool = recursive != 0 },
    };
    const result = fs_registry.call("filesystem", "remove-dir", &args) catch {
        return @intCast(@intFromEnum(FsError.io_error));
    };

    if (result.isErr()) {
        return @intCast(result.asErr() catch @intFromEnum(FsError.io_error));
    }

    return 0; // Success
}

/// Native symbols for WAMR registration (filesystem interface)
var fs_symbols = [_]c.NativeSymbol{
    .{
        .symbol = "exists",
        .func_ptr = @ptrCast(@constCast(&wamr_filesystem_exists)),
        .signature = "(ii)i", // path_ptr, path_len → bool
        .attachment = null,
    },
    .{
        .symbol = "write-file",
        .func_ptr = @ptrCast(@constCast(&wamr_filesystem_write_file)),
        .signature = "(iiii)i", // path_ptr, path_len, data_ptr, data_len → error_code
        .attachment = null,
    },
    .{
        .symbol = "read-file",
        .func_ptr = @ptrCast(@constCast(&wamr_filesystem_read_file)),
        .signature = "(iiiii)i", // path_ptr, path_len, encoding, out_ptr_ptr, out_len_ptr → error_code
        .attachment = null,
    },
    .{
        .symbol = "stat",
        .func_ptr = @ptrCast(@constCast(&wamr_filesystem_stat)),
        .signature = "(iii)i", // path_ptr, path_len, out_stat_ptr → error_code
        .attachment = null,
    },
    .{
        .symbol = "remove-file",
        .func_ptr = @ptrCast(@constCast(&wamr_filesystem_remove_file)),
        .signature = "(ii)i", // path_ptr, path_len → error_code
        .attachment = null,
    },
    .{
        .symbol = "rename",
        .func_ptr = @ptrCast(@constCast(&wamr_filesystem_rename)),
        .signature = "(iiii)i", // old_path_ptr, old_path_len, new_path_ptr, new_path_len → error_code
        .attachment = null,
    },
    .{
        .symbol = "copy-file",
        .func_ptr = @ptrCast(@constCast(&wamr_filesystem_copy_file)),
        .signature = "(iiii)i", // src_ptr, src_len, dest_ptr, dest_len → error_code
        .attachment = null,
    },
    .{
        .symbol = "append-file",
        .func_ptr = @ptrCast(@constCast(&wamr_filesystem_append_file)),
        .signature = "(iiii)i", // path_ptr, path_len, data_ptr, data_len → error_code
        .attachment = null,
    },
    .{
        .symbol = "read-dir",
        .func_ptr = @ptrCast(@constCast(&wamr_filesystem_read_dir)),
        .signature = "(iiii)i", // path_ptr, path_len, out_ptr_ptr, out_len_ptr → error_code
        .attachment = null,
    },
    .{
        .symbol = "mkdir",
        .func_ptr = @ptrCast(@constCast(&wamr_filesystem_mkdir)),
        .signature = "(iii)i", // path_ptr, path_len, recursive → error_code
        .attachment = null,
    },
    .{
        .symbol = "remove-dir",
        .func_ptr = @ptrCast(@constCast(&wamr_filesystem_remove_dir)),
        .signature = "(iii)i", // path_ptr, path_len, recursive → error_code
        .attachment = null,
    },
};

/// Register filesystem imports with WAMR
/// Must be called before loading WASM modules that import filesystem functions
///
/// Arguments:
///   registry: NativeRegistry containing filesystem implementation
pub fn registerFilesystemImports(registry: *NativeRegistry) void {
    fs_registry = registry;

    // Register native symbols with WAMR
    // Module name "filesystem" matches WIT interface name
    const result = c.wasm_runtime_register_natives(
        "filesystem",
        &fs_symbols,
        fs_symbols.len,
    );

    if (!result) {
        std.debug.print("[import_resolver] Warning: Failed to register filesystem imports\n", .{});
    }
}

// ============================================================================
// Crypto Interface (Phase 8b)
// ============================================================================

/// Global crypto registry - set by registerCryptoImports()
var crypto_registry: *NativeRegistry = undefined;

/// Crypto error enum (matches crypto_impl.zig)
const CryptoError = enum(u32) {
    invalid_algorithm = 0,
    invalid_input = 1,
    invalid_key = 2,
    operation_failed = 3,
};

/// crypto::hash: func(algorithm: u32, data: string) -> result<string, crypto-error>
/// WAMR signature: (iiii)i
fn wamr_crypto_hash(
    exec_env: c.wasm_exec_env_t,
    algorithm: u32,
    data_ptr: u32,
    data_len: u32,
    out_ptr_ptr: u32, // Where to write result ptr
    out_len_ptr: u32, // Where to write result len
) callconv(.c) i32 {
    // 1. Read input
    const data = readString(exec_env, data_ptr, data_len) orelse {
        return @intCast(@intFromEnum(CryptoError.invalid_input));
    };

    // 2. Call Component Model
    const args = [_]Value{
        Value{ .u32 = algorithm },
        Value{ .string = data },
    };
    const result = crypto_registry.call("crypto", "hash", &args) catch {
        return @intCast(@intFromEnum(CryptoError.operation_failed));
    };

    // 3. Handle result<string, crypto-error>
    if (result.isErr()) {
        return @intCast(result.asErr() catch @intFromEnum(CryptoError.operation_failed));
    }

    // Success - allocate hex string in WASM memory
    const hex_string = result.asOkString() catch {
        return @intCast(@intFromEnum(CryptoError.operation_failed));
    };

    const wasm_alloc = allocAndWriteString(exec_env, hex_string);
    if (wasm_alloc.ptr == 0 and hex_string.len > 0) {
        return @intCast(@intFromEnum(CryptoError.operation_failed));
    }

    // 4. Write result ptr/len to WASM memory
    const module_inst = c.wasm_runtime_get_module_inst(exec_env);
    if (module_inst == null) {
        freeWasmMemory(exec_env, wasm_alloc.ptr);
        return @intCast(@intFromEnum(CryptoError.operation_failed));
    }

    // Write ptr
    const out_ptr_native = c.wasm_runtime_addr_app_to_native(module_inst, out_ptr_ptr);
    if (out_ptr_native == null) {
        freeWasmMemory(exec_env, wasm_alloc.ptr);
        return @intCast(@intFromEnum(CryptoError.operation_failed));
    }
    const out_ptr_dest: *u32 = @ptrCast(@alignCast(out_ptr_native));
    out_ptr_dest.* = wasm_alloc.ptr;

    // Write len
    const out_len_native = c.wasm_runtime_addr_app_to_native(module_inst, out_len_ptr);
    if (out_len_native == null) {
        freeWasmMemory(exec_env, wasm_alloc.ptr);
        return @intCast(@intFromEnum(CryptoError.operation_failed));
    }
    const out_len_dest: *u32 = @ptrCast(@alignCast(out_len_native));
    out_len_dest.* = wasm_alloc.len;

    return 0; // Success
}

/// crypto::hmac: func(algorithm: u32, key: string, data: string) -> result<string, crypto-error>
/// WAMR signature: (iiiiiii)i
fn wamr_crypto_hmac(
    exec_env: c.wasm_exec_env_t,
    algorithm: u32,
    key_ptr: u32,
    key_len: u32,
    data_ptr: u32,
    data_len: u32,
    out_ptr_ptr: u32,
    out_len_ptr: u32,
) callconv(.c) i32 {
    // 1. Read inputs
    const key = readString(exec_env, key_ptr, key_len) orelse {
        return @intCast(@intFromEnum(CryptoError.invalid_key));
    };
    const data = readString(exec_env, data_ptr, data_len) orelse {
        return @intCast(@intFromEnum(CryptoError.invalid_input));
    };

    // 2. Call Component Model
    const args = [_]Value{
        Value{ .u32 = algorithm },
        Value{ .string = key },
        Value{ .string = data },
    };
    const result = crypto_registry.call("crypto", "hmac", &args) catch {
        return @intCast(@intFromEnum(CryptoError.operation_failed));
    };

    // 3. Handle result<string, crypto-error>
    if (result.isErr()) {
        return @intCast(result.asErr() catch @intFromEnum(CryptoError.operation_failed));
    }

    // Success - allocate hex string in WASM memory
    const hex_string = result.asOkString() catch {
        return @intCast(@intFromEnum(CryptoError.operation_failed));
    };

    const wasm_alloc = allocAndWriteString(exec_env, hex_string);
    if (wasm_alloc.ptr == 0 and hex_string.len > 0) {
        return @intCast(@intFromEnum(CryptoError.operation_failed));
    }

    // 4. Write result ptr/len to WASM memory
    const module_inst = c.wasm_runtime_get_module_inst(exec_env);
    if (module_inst == null) {
        freeWasmMemory(exec_env, wasm_alloc.ptr);
        return @intCast(@intFromEnum(CryptoError.operation_failed));
    }

    // Write ptr
    const out_ptr_native = c.wasm_runtime_addr_app_to_native(module_inst, out_ptr_ptr);
    if (out_ptr_native == null) {
        freeWasmMemory(exec_env, wasm_alloc.ptr);
        return @intCast(@intFromEnum(CryptoError.operation_failed));
    }
    const out_ptr_dest: *u32 = @ptrCast(@alignCast(out_ptr_native));
    out_ptr_dest.* = wasm_alloc.ptr;

    // Write len
    const out_len_native = c.wasm_runtime_addr_app_to_native(module_inst, out_len_ptr);
    if (out_len_native == null) {
        freeWasmMemory(exec_env, wasm_alloc.ptr);
        return @intCast(@intFromEnum(CryptoError.operation_failed));
    }
    const out_len_dest: *u32 = @ptrCast(@alignCast(out_len_native));
    out_len_dest.* = wasm_alloc.len;

    return 0; // Success
}

/// crypto::random-bytes: func(size: u32) -> result<list<u8>, crypto-error>
/// WAMR signature: (iii)i
fn wamr_crypto_random_bytes(
    exec_env: c.wasm_exec_env_t,
    size: u32,
    out_ptr_ptr: u32,
    out_len_ptr: u32,
) callconv(.c) i32 {
    // 1. Call Component Model
    const args = [_]Value{Value{ .u32 = size }};
    const result = crypto_registry.call("crypto", "random-bytes", &args) catch {
        return @intCast(@intFromEnum(CryptoError.operation_failed));
    };

    // 2. Handle result<list<u8>, crypto-error>
    if (result.isErr()) {
        return @intCast(result.asErr() catch @intFromEnum(CryptoError.operation_failed));
    }

    // Success - allocate bytes in WASM memory
    const bytes = result.asOkListU8() catch {
        return @intCast(@intFromEnum(CryptoError.operation_failed));
    };

    const wasm_alloc = allocAndWriteString(exec_env, bytes);
    if (wasm_alloc.ptr == 0 and bytes.len > 0) {
        return @intCast(@intFromEnum(CryptoError.operation_failed));
    }

    // 3. Write result ptr/len to WASM memory
    const module_inst = c.wasm_runtime_get_module_inst(exec_env);
    if (module_inst == null) {
        freeWasmMemory(exec_env, wasm_alloc.ptr);
        return @intCast(@intFromEnum(CryptoError.operation_failed));
    }

    // Write ptr
    const out_ptr_native = c.wasm_runtime_addr_app_to_native(module_inst, out_ptr_ptr);
    if (out_ptr_native == null) {
        freeWasmMemory(exec_env, wasm_alloc.ptr);
        return @intCast(@intFromEnum(CryptoError.operation_failed));
    }
    const out_ptr_dest: *u32 = @ptrCast(@alignCast(out_ptr_native));
    out_ptr_dest.* = wasm_alloc.ptr;

    // Write len
    const out_len_native = c.wasm_runtime_addr_app_to_native(module_inst, out_len_ptr);
    if (out_len_native == null) {
        freeWasmMemory(exec_env, wasm_alloc.ptr);
        return @intCast(@intFromEnum(CryptoError.operation_failed));
    }
    const out_len_dest: *u32 = @ptrCast(@alignCast(out_len_native));
    out_len_dest.* = wasm_alloc.len;

    return 0; // Success
}

/// crypto::get-hash-algorithms: func() -> list<string>
/// WAMR signature: (ii)i
fn wamr_crypto_get_hash_algorithms(
    exec_env: c.wasm_exec_env_t,
    out_ptr_ptr: u32, // Where to write list pointer
    out_len_ptr: u32, // Where to write list count
) callconv(.c) i32 {
    // Call crypto impl
    const args = [_]Value{};
    const result = crypto_registry.call("crypto", "get-hash-algorithms", &args) catch |err| {
        std.debug.print("[crypto::get-hash-algorithms] Registry call failed: {}\n", .{err});
        return @intFromEnum(CryptoError.operation_failed);
    };

    // Extract list<string> from result
    const algorithms = result.asListString() catch |err| {
        std.debug.print("[crypto::get-hash-algorithms] Failed to extract list: {}\n", .{err});
        return @intFromEnum(CryptoError.operation_failed);
    };

    // Allocate WASM memory for list<string> data
    // Each string is stored as {ptr: u32, len: u32} pairs (8 bytes each)
    const list_size = algorithms.len * 8;
    const list_ptr = allocWasmMemory(exec_env, @intCast(list_size));
    if (list_ptr == 0 and list_size > 0) {
        std.debug.print("[crypto::get-hash-algorithms] Failed to allocate WASM memory\n", .{});
        return @intFromEnum(CryptoError.operation_failed);
    }

    // Write each string to WASM memory
    for (algorithms, 0..) |algo, i| {
        // Allocate memory for string content
        const str_ptr = allocWasmMemory(exec_env, @intCast(algo.len));
        if (str_ptr == 0 and algo.len > 0) {
            return @intFromEnum(CryptoError.operation_failed);
        }

        // Write string content
        _ = writeString(exec_env, str_ptr, algo);

        // Write {ptr, len} pair to list
        const pair_offset = list_ptr + @as(u32, @intCast(i)) * 8;
        _ = writeU32(exec_env, pair_offset, str_ptr);
        _ = writeU32(exec_env, pair_offset + 4, @intCast(algo.len));
    }

    // Write output: list pointer and count
    _ = writeU32(exec_env, out_ptr_ptr, list_ptr);
    _ = writeU32(exec_env, out_len_ptr, @intCast(algorithms.len));

    return 0; // Success
}

/// Native symbols for WAMR registration (crypto interface)
var crypto_symbols = [_]c.NativeSymbol{
    .{
        .symbol = "hash",
        .func_ptr = @ptrCast(@constCast(&wamr_crypto_hash)),
        .signature = "(iiiii)i", // algorithm, data_ptr, data_len, out_ptr_ptr, out_len_ptr → error_code
        .attachment = null,
    },
    .{
        .symbol = "hmac",
        .func_ptr = @ptrCast(@constCast(&wamr_crypto_hmac)),
        .signature = "(iiiiiii)i", // algorithm, key_ptr, key_len, data_ptr, data_len, out_ptr_ptr, out_len_ptr → error_code
        .attachment = null,
    },
    .{
        .symbol = "random-bytes",
        .func_ptr = @ptrCast(@constCast(&wamr_crypto_random_bytes)),
        .signature = "(iii)i", // size, out_ptr_ptr, out_len_ptr → error_code
        .attachment = null,
    },
    .{
        .symbol = "get-hash-algorithms",
        .func_ptr = @ptrCast(@constCast(&wamr_crypto_get_hash_algorithms)),
        .signature = "(ii)i", // out_ptr_ptr, out_len_ptr → error_code
        .attachment = null,
    },
};

/// Register crypto imports with WAMR
/// Must be called before loading WASM modules that import crypto functions
///
/// Arguments:
///   registry: NativeRegistry containing crypto implementation
pub fn registerCryptoImports(registry: *NativeRegistry) void {
    crypto_registry = registry;

    // Register native symbols with WAMR
    // Module name "crypto" matches WIT interface name
    const result = c.wasm_runtime_register_natives(
        "crypto",
        &crypto_symbols,
        crypto_symbols.len,
    );

    if (!result) {
        std.debug.print("[import_resolver] Warning: Failed to register crypto imports\n", .{});
    }
}

// ============================================================================
// Process Interface (Phase 8b)
// ============================================================================

/// Global process registry - set by registerProcessImports()
var process_registry: *NativeRegistry = undefined;

/// Process error enum (matches process_impl.zig)
const ProcessError = enum(u32) {
    permission_denied = 0,
    command_not_found = 1,
    timeout = 2,
    invalid_input = 3,
    spawn_failed = 4,
    operation_failed = 5,
};

/// Helper: Read list<string> from WASM memory
/// Args array is [{ptr, len}, {ptr, len}, ...] in WASM memory
fn readListString(exec_env: c.wasm_exec_env_t, array_ptr: u32, count: u32, allocator: std.mem.Allocator) ![][]const u8 {
    if (count == 0) return &[_][]const u8{};

    // Allocate result slice
    const result = try allocator.alloc([]const u8, count);

    // Read each {ptr, len} pair (8 bytes each)
    for (0..count) |i| {
        const pair_offset = array_ptr + @as(u32, @intCast(i)) * 8;
        const str_ptr = readU32(exec_env, pair_offset);
        const str_len = readU32(exec_env, pair_offset + 4);

        const str = readString(exec_env, str_ptr, str_len) orelse {
            // Cleanup on error
            allocator.free(result);
            return error.InvalidString;
        };

        result[i] = str;
    }

    return result;
}

/// Helper: Write ProcessOutput struct to WASM memory
/// Struct layout: exit_code(i32), stdout_ptr(u32), stdout_len(u32), stderr_ptr(u32), stderr_len(u32)
fn writeProcessOutput(exec_env: c.wasm_exec_env_t, out_ptr: u32, output: ProcessOutput) !void {
    const module_inst = c.wasm_runtime_get_module_inst(exec_env);
    if (module_inst == null) return error.NoModuleInst;

    // Allocate stdout in WASM memory
    const stdout_alloc = allocAndWriteString(exec_env, output.stdout);
    if (stdout_alloc.ptr == 0 and output.stdout.len > 0) return error.AllocationFailed;

    // Allocate stderr in WASM memory
    const stderr_alloc = allocAndWriteString(exec_env, output.stderr);
    if (stderr_alloc.ptr == 0 and output.stderr.len > 0) {
        freeWasmMemory(exec_env, stdout_alloc.ptr);
        return error.AllocationFailed;
    }

    // Validate WASM memory bounds (20 bytes for ProcessOutput)
    if (!c.wasm_runtime_validate_app_addr(module_inst, out_ptr, 20)) {
        freeWasmMemory(exec_env, stdout_alloc.ptr);
        freeWasmMemory(exec_env, stderr_alloc.ptr);
        return error.InvalidAddress;
    }

    // Convert to native pointer
    const native_ptr = c.wasm_runtime_addr_app_to_native(module_inst, out_ptr);
    if (native_ptr == null) {
        freeWasmMemory(exec_env, stdout_alloc.ptr);
        freeWasmMemory(exec_env, stderr_alloc.ptr);
        return error.InvalidAddress;
    }

    // Write fields
    const bytes: [*]u8 = @ptrCast(native_ptr);
    var offset: usize = 0;

    // exit_code: i32
    const exit_code_ptr: *i32 = @ptrCast(@alignCast(bytes + offset));
    exit_code_ptr.* = output.exit_code;
    offset += 4;

    // stdout_ptr: u32
    const stdout_ptr_ptr: *u32 = @ptrCast(@alignCast(bytes + offset));
    stdout_ptr_ptr.* = stdout_alloc.ptr;
    offset += 4;

    // stdout_len: u32
    const stdout_len_ptr: *u32 = @ptrCast(@alignCast(bytes + offset));
    stdout_len_ptr.* = stdout_alloc.len;
    offset += 4;

    // stderr_ptr: u32
    const stderr_ptr_ptr: *u32 = @ptrCast(@alignCast(bytes + offset));
    stderr_ptr_ptr.* = stderr_alloc.ptr;
    offset += 4;

    // stderr_len: u32
    const stderr_len_ptr: *u32 = @ptrCast(@alignCast(bytes + offset));
    stderr_len_ptr.* = stderr_alloc.len;
}

/// process::exec-sync: func(command: string, options: spawn-options) -> result<process-output, process-error>
/// WAMR signature: (iiiiiiii)i
fn wamr_process_exec_sync(
    exec_env: c.wasm_exec_env_t,
    cmd_ptr: u32,
    cmd_len: u32,
    timeout_seconds: u32,
    stdin_data_ptr: u32,
    stdin_data_len: u32,
    capture_output: u32,
    out_output_ptr: u32,
) callconv(.c) i32 {
    // 1. Read inputs
    const command = readString(exec_env, cmd_ptr, cmd_len) orelse {
        return @intCast(@intFromEnum(ProcessError.invalid_input));
    };
    const stdin_data = readString(exec_env, stdin_data_ptr, stdin_data_len) orelse {
        return @intCast(@intFromEnum(ProcessError.invalid_input));
    };

    // 2. Build SpawnOptions
    const options = SpawnOptions{
        .timeout_seconds = timeout_seconds,
        .stdin_data = stdin_data,
        .capture_output = capture_output != 0,
    };

    // 3. Call Component Model
    const args = [_]Value{
        Value{ .string = command },
        Value{ .spawn_options = options },
    };
    const result = process_registry.call("process", "exec-sync", &args) catch {
        return @intCast(@intFromEnum(ProcessError.operation_failed));
    };

    // 4. Handle result<process-output, process-error>
    if (result.isErr()) {
        return @intCast(result.asErr() catch @intFromEnum(ProcessError.operation_failed));
    }

    const output = result.asOkProcessOutput() catch {
        return @intCast(@intFromEnum(ProcessError.operation_failed));
    };

    // 5. Write ProcessOutput to WASM memory
    writeProcessOutput(exec_env, out_output_ptr, output) catch {
        return @intCast(@intFromEnum(ProcessError.operation_failed));
    };

    return 0; // Success
}

/// process::spawn-sync: func(command: string, args: list<string>, options: spawn-options) -> result<process-output, process-error>
/// WAMR signature: (iiiiiiiiiii)i
fn wamr_process_spawn_sync(
    exec_env: c.wasm_exec_env_t,
    cmd_ptr: u32,
    cmd_len: u32,
    args_ptr: u32,
    args_count: u32,
    timeout_seconds: u32,
    stdin_data_ptr: u32,
    stdin_data_len: u32,
    capture_output: u32,
    out_output_ptr: u32,
) callconv(.c) i32 {
    // Get allocator (need for list<string>)
    const allocator = std.heap.c_allocator;

    // 1. Read inputs
    const command = readString(exec_env, cmd_ptr, cmd_len) orelse {
        return @intCast(@intFromEnum(ProcessError.invalid_input));
    };

    const arg_list = readListString(exec_env, args_ptr, args_count, allocator) catch {
        return @intCast(@intFromEnum(ProcessError.invalid_input));
    };
    defer allocator.free(arg_list);

    const stdin_data = readString(exec_env, stdin_data_ptr, stdin_data_len) orelse {
        return @intCast(@intFromEnum(ProcessError.invalid_input));
    };

    // 2. Build SpawnOptions
    const options = SpawnOptions{
        .timeout_seconds = timeout_seconds,
        .stdin_data = stdin_data,
        .capture_output = capture_output != 0,
    };

    // 3. Call Component Model
    const args = [_]Value{
        Value{ .string = command },
        Value{ .list_string = arg_list },
        Value{ .spawn_options = options },
    };
    const result = process_registry.call("process", "spawn-sync", &args) catch {
        return @intCast(@intFromEnum(ProcessError.operation_failed));
    };

    // 4. Handle result<process-output, process-error>
    if (result.isErr()) {
        return @intCast(result.asErr() catch @intFromEnum(ProcessError.operation_failed));
    }

    const output = result.asOkProcessOutput() catch {
        return @intCast(@intFromEnum(ProcessError.operation_failed));
    };

    // 5. Write ProcessOutput to WASM memory
    writeProcessOutput(exec_env, out_output_ptr, output) catch {
        return @intCast(@intFromEnum(ProcessError.operation_failed));
    };

    return 0; // Success
}

/// process::spawn-start: func(command: string, args: list<string>, options: spawn-options) -> result<u32, process-error>
/// WAMR signature: (iiiiiiiii)i
fn wamr_process_spawn_start(
    exec_env: c.wasm_exec_env_t,
    cmd_ptr: u32,
    cmd_len: u32,
    args_ptr: u32,
    args_count: u32,
    timeout_seconds: u32,
    stdin_data_ptr: u32,
    stdin_data_len: u32,
    capture_output: u32,
    out_spawn_id_ptr: u32,
) callconv(.c) i32 {
    const allocator = std.heap.c_allocator;

    // 1. Read inputs
    const command = readString(exec_env, cmd_ptr, cmd_len) orelse {
        return @intCast(@intFromEnum(ProcessError.invalid_input));
    };

    const arg_list = readListString(exec_env, args_ptr, args_count, allocator) catch {
        return @intCast(@intFromEnum(ProcessError.invalid_input));
    };
    defer allocator.free(arg_list);

    const stdin_data = readString(exec_env, stdin_data_ptr, stdin_data_len) orelse {
        return @intCast(@intFromEnum(ProcessError.invalid_input));
    };

    // 2. Build SpawnOptions
    const options = SpawnOptions{
        .timeout_seconds = timeout_seconds,
        .stdin_data = stdin_data,
        .capture_output = capture_output != 0,
    };

    // 3. Call Component Model
    const args = [_]Value{
        Value{ .string = command },
        Value{ .list_string = arg_list },
        Value{ .spawn_options = options },
    };
    const result = process_registry.call("process", "spawn-start", &args) catch {
        return @intCast(@intFromEnum(ProcessError.operation_failed));
    };

    // 4. Handle result<u32, process-error>
    if (result.isErr()) {
        return @intCast(result.asErr() catch @intFromEnum(ProcessError.operation_failed));
    }

    const spawn_id = result.asOkSpawnId() catch {
        return @intCast(@intFromEnum(ProcessError.operation_failed));
    };

    // 5. Write spawn ID to WASM memory
    if (!writeU32(exec_env, out_spawn_id_ptr, spawn_id)) {
        return @intCast(@intFromEnum(ProcessError.operation_failed));
    }

    return 0; // Success
}

/// process::spawn-poll: func(spawn-id: u32) -> result<u32, process-error>
/// WAMR signature: (ii)i
fn wamr_process_spawn_poll(
    exec_env: c.wasm_exec_env_t,
    spawn_id: u32,
    out_status_ptr: u32,
) callconv(.c) i32 {
    // 1. Call Component Model
    const args = [_]Value{Value{ .u32 = spawn_id }};
    const result = process_registry.call("process", "spawn-poll", &args) catch {
        return @intCast(@intFromEnum(ProcessError.operation_failed));
    };

    // 2. Handle result<u32, process-error>
    if (result.isErr()) {
        return @intCast(result.asErr() catch @intFromEnum(ProcessError.operation_failed));
    }

    const status = result.asU32() catch {
        return @intCast(@intFromEnum(ProcessError.operation_failed));
    };

    // 3. Write status to WASM memory
    if (!writeU32(exec_env, out_status_ptr, status)) {
        return @intCast(@intFromEnum(ProcessError.operation_failed));
    }

    return 0; // Success
}

/// process::spawn-output: func(spawn-id: u32) -> result<process-output, process-error>
/// WAMR signature: (ii)i
fn wamr_process_spawn_output(
    exec_env: c.wasm_exec_env_t,
    spawn_id: u32,
    out_output_ptr: u32,
) callconv(.c) i32 {
    // 1. Call Component Model
    const args = [_]Value{Value{ .u32 = spawn_id }};
    const result = process_registry.call("process", "spawn-output", &args) catch {
        return @intCast(@intFromEnum(ProcessError.operation_failed));
    };

    // 2. Handle result<process-output, process-error>
    if (result.isErr()) {
        return @intCast(result.asErr() catch @intFromEnum(ProcessError.operation_failed));
    }

    const output = result.asOkProcessOutput() catch {
        return @intCast(@intFromEnum(ProcessError.operation_failed));
    };

    // 3. Write ProcessOutput to WASM memory
    writeProcessOutput(exec_env, out_output_ptr, output) catch {
        return @intCast(@intFromEnum(ProcessError.operation_failed));
    };

    return 0; // Success
}

/// process::spawn-free: func(spawn-id: u32) -> result<_, process-error>
/// WAMR signature: (i)i
fn wamr_process_spawn_free(
    exec_env: c.wasm_exec_env_t,
    spawn_id: u32,
) callconv(.c) i32 {
    _ = exec_env;

    // 1. Call Component Model
    const args = [_]Value{Value{ .u32 = spawn_id }};
    const result = process_registry.call("process", "spawn-free", &args) catch {
        return @intCast(@intFromEnum(ProcessError.operation_failed));
    };

    // 2. Handle result<_, process-error>
    if (result.isErr()) {
        return @intCast(result.asErr() catch @intFromEnum(ProcessError.operation_failed));
    }

    return 0; // Success
}

/// Native symbols for WAMR registration (process interface)
var process_symbols = [_]c.NativeSymbol{
    .{
        .symbol = "exec-sync",
        .func_ptr = @ptrCast(@constCast(&wamr_process_exec_sync)),
        .signature = "(iiiiiii)i", // cmd_ptr, cmd_len, timeout, stdin_ptr, stdin_len, capture, out_ptr → error_code
        .attachment = null,
    },
    .{
        .symbol = "spawn-sync",
        .func_ptr = @ptrCast(@constCast(&wamr_process_spawn_sync)),
        .signature = "(iiiiiiiii)i", // cmd_ptr, cmd_len, args_ptr, args_count, timeout, stdin_ptr, stdin_len, capture, out_ptr → error_code
        .attachment = null,
    },
    .{
        .symbol = "spawn-start",
        .func_ptr = @ptrCast(@constCast(&wamr_process_spawn_start)),
        .signature = "(iiiiiiiii)i", // cmd_ptr, cmd_len, args_ptr, args_count, timeout, stdin_ptr, stdin_len, capture, out_id_ptr → error_code
        .attachment = null,
    },
    .{
        .symbol = "spawn-poll",
        .func_ptr = @ptrCast(@constCast(&wamr_process_spawn_poll)),
        .signature = "(ii)i", // spawn_id, out_status_ptr → error_code
        .attachment = null,
    },
    .{
        .symbol = "spawn-output",
        .func_ptr = @ptrCast(@constCast(&wamr_process_spawn_output)),
        .signature = "(ii)i", // spawn_id, out_output_ptr → error_code
        .attachment = null,
    },
    .{
        .symbol = "spawn-free",
        .func_ptr = @ptrCast(@constCast(&wamr_process_spawn_free)),
        .signature = "(i)i", // spawn_id → error_code
        .attachment = null,
    },
};

/// Register process imports with WAMR
/// Must be called before loading WASM modules that import process functions
///
/// Arguments:
///   registry: NativeRegistry containing process implementation
pub fn registerProcessImports(registry: *NativeRegistry) void {
    process_registry = registry;

    // Register native symbols with WAMR
    // Module name "process" matches WIT interface name
    const result = c.wasm_runtime_register_natives(
        "process",
        &process_symbols,
        process_symbols.len,
    );

    if (!result) {
        std.debug.print("[import_resolver] Warning: Failed to register process imports\n", .{});
    }
}

// ============================================================================
// HTTP Interface Bridge (Phase 8b)
// ============================================================================
// Bridges Component Model HTTP interface to WAMR flat calling convention.
// Handles nested list<http-header> and HttpResponse struct.

/// HTTP error enum (matches WIT http-error)
const HttpError = enum(u32) {
    invalid_url = 0,
    connection_failed = 1,
    timeout = 2,
    permission_denied = 3,
    invalid_method = 4,
    invalid_response = 5,
    rate_limit_exceeded = 6,
};

/// Global HTTP registry - set by registerHttpImports()
var http_registry: *NativeRegistry = undefined;

/// Helper: Read list<http-header> from WASM memory
/// WASM passes array of HttpHeader structs (16 bytes each): name_ptr, name_len, value_ptr, value_len
fn readHttpHeaders(exec_env: c.wasm_exec_env_t, array_ptr: u32, count: u32, allocator: std.mem.Allocator) ![]HttpHeader {
    if (count == 0) return &[_]HttpHeader{};

    // Allocate headers array
    const result = try allocator.alloc(HttpHeader, count);

    // Read each header struct (16 bytes)
    for (0..count) |i| {
        const header_offset = array_ptr + @as(u32, @intCast(i)) * 16;

        // Read 4 u32 fields from WASM memory
        const name_ptr = readU32(exec_env, header_offset + 0);
        const name_len = readU32(exec_env, header_offset + 4);
        const value_ptr = readU32(exec_env, header_offset + 8);
        const value_len = readU32(exec_env, header_offset + 12);

        // Read strings
        const name = readString(exec_env, name_ptr, name_len) orelse {
            allocator.free(result);
            return error.InvalidString;
        };
        const value = readString(exec_env, value_ptr, value_len) orelse {
            allocator.free(result);
            return error.InvalidString;
        };

        result[i] = HttpHeader{ .name = name, .value = value };
    }

    return result;
}

/// Helper: Write HttpResponse struct to WASM memory
/// Struct layout: status(u16, padded to 4), ok(u32), body_ptr(u32), body_len(u32), headers_ptr(u32), headers_count(u32)
/// Total: 24 bytes
fn writeHttpResponse(exec_env: c.wasm_exec_env_t, out_ptr: u32, response: HttpResponse) !void {
    const module_inst = c.wasm_runtime_get_module_inst(exec_env);
    if (module_inst == null) return error.NoModuleInst;

    // Allocate body in WASM memory
    const body_alloc = allocAndWriteString(exec_env, response.body);

    // Write HttpResponse struct (24 bytes)
    // Field offsets: status(0), ok(4), body_ptr(8), body_len(12), headers_ptr(16), headers_count(20)

    // status (u16, padded to 4 bytes)
    const status_u32: u32 = @intCast(response.status);
    _ = writeU32(exec_env, out_ptr + 0, status_u32);

    // ok (bool as u32)
    const ok_u32: u32 = if (response.ok) 1 else 0;
    _ = writeU32(exec_env, out_ptr + 4, ok_u32);

    // body_ptr and body_len
    _ = writeU32(exec_env, out_ptr + 8, body_alloc.ptr);
    _ = writeU32(exec_env, out_ptr + 12, body_alloc.len);

    // headers_ptr and headers_count (currently unused, set to 0)
    _ = writeU32(exec_env, out_ptr + 16, 0);
    _ = writeU32(exec_env, out_ptr + 20, 0);
}

// Bridge function: http::fetch (synchronous)
// Component Model: func(request: http-request) -> result<http-response, http-error>
// WAMR Signature: (iiiiiiiii)i
// Parameters: url_ptr, url_len, method, headers_ptr, headers_count, body_ptr, body_len, timeout_ms, out_response_ptr
// Returns: error_code (0 = success, >0 = HttpError discriminant)
export fn wamr_http_fetch(
    exec_env: c.wasm_exec_env_t,
    url_ptr: u32,
    url_len: u32,
    method: u32,
    headers_ptr: u32,
    headers_count: u32,
    body_ptr: u32,
    body_len: u32,
    timeout_ms: u32,
    out_response_ptr: u32,
) u32 {
    // 1. Read HTTP request parameters from WASM memory
    const url = readString(exec_env, url_ptr, url_len) orelse return @intFromEnum(HttpError.invalid_url);

    // Read headers (nested list<http-header>)
    const allocator = http_registry.allocator;
    const headers = readHttpHeaders(exec_env, headers_ptr, headers_count, allocator) catch return @intFromEnum(HttpError.invalid_response);
    defer if (headers.len > 0) allocator.free(headers);

    // Read optional body (both ptr and len must be non-zero)
    const body: ?[]const u8 = if (body_ptr != 0 and body_len != 0) readString(exec_env, body_ptr, body_len) else null;

    // 2. Build HttpRequest
    const request = HttpRequest{
        .url = url,
        .method = method,
        .headers = headers,
        .body = body,
        .timeout_ms = timeout_ms,
    };

    // 3. Call Component Model
    const args = [_]Value{
        Value{ .http_request = request },
    };

    const result = http_registry.call("http", "fetch", &args) catch |err| {
        std.debug.print("[http fetch] Call failed: {}\n", .{err});
        return @intFromEnum(HttpError.connection_failed);
    };

    // 4. Handle result<http-response, http-error>
    if (result.isErr()) {
        return @intCast(result.asErr() catch @intFromEnum(HttpError.invalid_response));
    }

    // Extract HttpResponse from ok result
    const http_response = result.asOkHttpResponse() catch return @intFromEnum(HttpError.invalid_response);

    // 5. Write HttpResponse to WASM memory
    writeHttpResponse(exec_env, out_response_ptr, http_response) catch return @intFromEnum(HttpError.invalid_response);

    return 0; // Success
}

// Bridge function: http::fetch-start (async start)
// Component Model: func(request: http-request) -> result<u32, http-error>
// WAMR Signature: (iiiiiiii)i
// Parameters: url_ptr, url_len, method, headers_ptr, headers_count, body_ptr, body_len, timeout_ms, out_request_id_ptr
// Returns: error_code (0 = success, >0 = HttpError discriminant)
export fn wamr_http_fetch_start(
    exec_env: c.wasm_exec_env_t,
    url_ptr: u32,
    url_len: u32,
    method: u32,
    headers_ptr: u32,
    headers_count: u32,
    body_ptr: u32,
    body_len: u32,
    timeout_ms: u32,
    out_request_id_ptr: u32,
) u32 {
    // 1. Read HTTP request parameters
    const url = readString(exec_env, url_ptr, url_len) orelse return @intFromEnum(HttpError.invalid_url);

    const allocator = http_registry.allocator;
    const headers = readHttpHeaders(exec_env, headers_ptr, headers_count, allocator) catch return @intFromEnum(HttpError.invalid_response);
    defer if (headers.len > 0) allocator.free(headers);

    const body: ?[]const u8 = if (body_ptr != 0 and body_len != 0) readString(exec_env, body_ptr, body_len) else null;

    // 2. Build HttpRequest
    const request = HttpRequest{
        .url = url,
        .method = method,
        .headers = headers,
        .body = body,
        .timeout_ms = timeout_ms,
    };

    // 3. Call Component Model
    const args = [_]Value{
        Value{ .http_request = request },
    };

    const result = http_registry.call("http", "fetch-start", &args) catch |err| {
        std.debug.print("[http fetch-start] Call failed: {}\n", .{err});
        return @intFromEnum(HttpError.connection_failed);
    };

    // 4. Handle result<u32, http-error>
    if (result.isErr()) {
        return @intCast(result.asErr() catch @intFromEnum(HttpError.invalid_response));
    }

    // Extract request_id from ok result
    const request_id = result.asOkRequestId() catch return @intFromEnum(HttpError.invalid_response);

    // 5. Write request_id to WASM memory
    if (!writeU32(exec_env, out_request_id_ptr, request_id)) {
        return @intFromEnum(HttpError.invalid_response);
    }

    return 0; // Success
}

// Bridge function: http::fetch-poll (poll async status)
// Component Model: func(request-id: u32) -> result<u32, http-error>
// WAMR Signature: (ii)i
// Parameters: request_id, out_status_ptr
// Returns: error_code (0 = success, >0 = HttpError discriminant)
export fn wamr_http_fetch_poll(exec_env: c.wasm_exec_env_t, request_id: u32, out_status_ptr: u32) u32 {
    // Call Component Model
    const args = [_]Value{
        Value{ .u32 = request_id },
    };

    const result = http_registry.call("http", "fetch-poll", &args) catch |err| {
        std.debug.print("[http fetch-poll] Call failed: {}\n", .{err});
        return @intFromEnum(HttpError.connection_failed);
    };

    // Handle result<u32, http-error>
    if (result.isErr()) {
        return @intCast(result.asErr() catch @intFromEnum(HttpError.invalid_response));
    }

    // Extract status from ok result
    const status = result.asU32() catch return @intFromEnum(HttpError.invalid_response);

    // Write status to WASM memory
    if (!writeU32(exec_env, out_status_ptr, status)) {
        return @intFromEnum(HttpError.invalid_response);
    }

    return 0; // Success
}

// Bridge function: http::fetch-response (get async response)
// Component Model: func(request-id: u32) -> result<http-response, http-error>
// WAMR Signature: (ii)i
// Parameters: request_id, out_response_ptr
// Returns: error_code (0 = success, >0 = HttpError discriminant)
export fn wamr_http_fetch_response(exec_env: c.wasm_exec_env_t, request_id: u32, out_response_ptr: u32) u32 {
    // Call Component Model
    const args = [_]Value{
        Value{ .u32 = request_id },
    };

    const result = http_registry.call("http", "fetch-response", &args) catch |err| {
        std.debug.print("[http fetch-response] Call failed: {}\n", .{err});
        return @intFromEnum(HttpError.connection_failed);
    };

    // Handle result<http-response, http-error>
    if (result.isErr()) {
        return @intCast(result.asErr() catch @intFromEnum(HttpError.invalid_response));
    }

    // Extract HttpResponse from ok result
    const http_response = result.asOkHttpResponse() catch return @intFromEnum(HttpError.invalid_response);

    // Write HttpResponse to WASM memory
    writeHttpResponse(exec_env, out_response_ptr, http_response) catch return @intFromEnum(HttpError.invalid_response);

    return 0; // Success
}

// Bridge function: http::fetch-free (free async request)
// Component Model: func(request-id: u32) -> result<_, http-error>
// WAMR Signature: (i)i
// Parameters: request_id
// Returns: error_code (0 = success, >0 = HttpError discriminant)
export fn wamr_http_fetch_free(exec_env: c.wasm_exec_env_t, request_id: u32) u32 {
    _ = exec_env; // Not needed for this function

    // Call Component Model
    const args = [_]Value{
        Value{ .u32 = request_id },
    };

    const result = http_registry.call("http", "fetch-free", &args) catch |err| {
        std.debug.print("[http fetch-free] Call failed: {}\n", .{err});
        return @intFromEnum(HttpError.connection_failed);
    };

    // Handle result<_, http-error>
    if (result.isErr()) {
        return @intCast(result.asErr() catch @intFromEnum(HttpError.connection_failed));
    }

    return 0; // Success
}

// HTTP native symbols for WAMR registration
var http_symbols = [_]c.NativeSymbol{
    c.NativeSymbol{ .symbol = "fetch", .func_ptr = @ptrCast(@constCast(&wamr_http_fetch)), .signature = "(iiiiiiiii)i" },
    c.NativeSymbol{ .symbol = "fetch-start", .func_ptr = @ptrCast(@constCast(&wamr_http_fetch_start)), .signature = "(iiiiiiiii)i" },
    c.NativeSymbol{ .symbol = "fetch-poll", .func_ptr = @ptrCast(@constCast(&wamr_http_fetch_poll)), .signature = "(ii)i" },
    c.NativeSymbol{ .symbol = "fetch-response", .func_ptr = @ptrCast(@constCast(&wamr_http_fetch_response)), .signature = "(ii)i" },
    c.NativeSymbol{ .symbol = "fetch-free", .func_ptr = @ptrCast(@constCast(&wamr_http_fetch_free)), .signature = "(i)i" },
};

/// Register HTTP imports with WAMR
/// Called once during initialization to register HTTP functions
pub fn registerHttpImports(registry: *NativeRegistry) void {
    http_registry = registry;

    // Register native symbols with WAMR
    // Module name "http" matches WIT interface name
    const result = c.wasm_runtime_register_natives(
        "http",
        &http_symbols,
        http_symbols.len,
    );

    if (!result) {
        std.debug.print("[import_resolver] Warning: Failed to register http imports\n", .{});
    }
}
