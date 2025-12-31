//! File Dispatch - Async and Component Model Integration
//!
//! Provides two types of file operations:
//! 1. Legacy async file operations (opcodes 0-5) - async read/write with polling
//! 2. Component Model sync file operations (opcodes 100+) - full filesystem API
//!
//! Legacy async operations use a simple async state machine for reads/writes.
//! Component Model operations delegate to the native registry for comprehensive
//! filesystem access (read, write, stat, mkdir, etc.).

const std = @import("std");
const wasm_helpers = @import("../wasm_helpers.zig");
const c = wasm_helpers.c;
const errors = @import("../errors.zig");
const Config = @import("../config/types.zig").Config;
const NativeRegistry = @import("../component/native_registry.zig").NativeRegistry;
const Value = @import("../component/native_registry.zig").Value;

// =============================================================================
// Opcodes
// =============================================================================

// Legacy async file operations (0-5)
pub const OP_READ_START: i32 = 0;
pub const OP_WRITE_START: i32 = 1;
pub const OP_POLL: i32 = 2;
pub const OP_RESULT_LEN: i32 = 3;
pub const OP_RESULT: i32 = 4;
pub const OP_FREE: i32 = 5;

// Component Model sync file operations (100+)
pub const OP_CM_READ: i32 = 100;
pub const OP_CM_WRITE: i32 = 101;
pub const OP_CM_EXISTS: i32 = 102;
pub const OP_CM_STAT: i32 = 103;
pub const OP_CM_READDIR: i32 = 104;
pub const OP_CM_MKDIR: i32 = 105;
pub const OP_CM_UNLINK: i32 = 106;
pub const OP_CM_RMDIR: i32 = 107;
pub const OP_CM_RENAME: i32 = 108;
pub const OP_CM_COPY: i32 = 109;
pub const OP_CM_GET_RESULT_LEN: i32 = 110;
pub const OP_CM_GET_RESULT: i32 = 111;

// =============================================================================
// State Management
// =============================================================================

// Max concurrent async operations
const MAX_ASYNC_OPS: usize = 64;

// Async file operation state
const FileOpStatus = enum { pending, complete, error_state };
const FileOp = enum { read, write };

const AsyncFileRequest = struct {
    id: u32,
    op: FileOp,
    status: FileOpStatus,
    data: ?[]u8,
    error_msg: ?[]const u8,
    bytes_written: usize,
};

var g_file_ops: [MAX_ASYNC_OPS]?AsyncFileRequest = [_]?AsyncFileRequest{null} ** MAX_ASYNC_OPS;
var g_next_file_id: u32 = 1;
var g_allocator: std.mem.Allocator = std.heap.page_allocator;

// Component Model file result buffer (single-threaded, reused between calls)
var g_cm_result: ?[]const u8 = null;
var g_cm_result_allocator: std.mem.Allocator = std.heap.page_allocator;

// Optional config for path remapping (mount support)
var g_config: ?*const Config = null;

// =============================================================================
// Public API
// =============================================================================

/// Set the allocator for file operations
pub fn setAllocator(allocator: std.mem.Allocator) void {
    g_allocator = allocator;
    g_cm_result_allocator = allocator;
}

/// Set the config for path remapping
pub fn setConfig(config: *const Config) void {
    g_config = config;
}

/// Clear the config
pub fn clearConfig() void {
    g_config = null;
}

/// Free all resources
pub fn deinit() void {
    // Free async operations
    for (&g_file_ops) |*slot| {
        if (slot.*) |*req| {
            if (req.data) |data| {
                g_allocator.free(data);
            }
            slot.* = null;
        }
    }

    // Free component model result
    if (g_cm_result) |old| {
        g_cm_result_allocator.free(old);
        g_cm_result = null;
    }
}

/// Main dispatch function for file operations
pub fn dispatch(
    exec_env: c.wasm_exec_env_t,
    registry_opt: ?*NativeRegistry,
    opcode: i32,
    a1: i32,
    a2: i32,
    a3: i32,
    a4: i32,
) i32 {
    return switch (opcode) {
        // Legacy async file operations (0-5)
        OP_READ_START => readStart(exec_env, @bitCast(a1), @bitCast(a2)),
        OP_WRITE_START => writeStart(exec_env, @bitCast(a1), @bitCast(a2), @bitCast(a3), @bitCast(a4)),
        OP_POLL => poll(@bitCast(a1)),
        OP_RESULT_LEN => resultLen(@bitCast(a1)),
        OP_RESULT => result(exec_env, @bitCast(a1), @bitCast(a2)),
        OP_FREE => free(@bitCast(a1)),

        // Component Model sync file operations (100+)
        OP_CM_READ => cmRead(exec_env, registry_opt, @bitCast(a1), @bitCast(a2)),
        OP_CM_WRITE => cmWrite(exec_env, registry_opt, @bitCast(a1), @bitCast(a2), @bitCast(a3), @bitCast(a4)),
        OP_CM_EXISTS => cmExists(exec_env, registry_opt, @bitCast(a1), @bitCast(a2)),
        OP_CM_STAT => cmStat(exec_env, registry_opt, @bitCast(a1), @bitCast(a2)),
        OP_CM_READDIR => cmReaddir(exec_env, registry_opt, @bitCast(a1), @bitCast(a2)),
        OP_CM_MKDIR => cmMkdir(exec_env, registry_opt, @bitCast(a1), @bitCast(a2), @bitCast(a3)),
        OP_CM_UNLINK => cmUnlink(exec_env, registry_opt, @bitCast(a1), @bitCast(a2)),
        OP_CM_RMDIR => cmRmdir(exec_env, registry_opt, @bitCast(a1), @bitCast(a2), @bitCast(a3)),
        OP_CM_RENAME => cmRename(exec_env, registry_opt, @bitCast(a1), @bitCast(a2), @bitCast(a3), @bitCast(a4)),
        OP_CM_COPY => cmCopy(exec_env, registry_opt, @bitCast(a1), @bitCast(a2), @bitCast(a3), @bitCast(a4)),
        OP_CM_GET_RESULT_LEN => cmGetResultLen(),
        OP_CM_GET_RESULT => cmGetResult(exec_env, @bitCast(a1), @bitCast(a2)),

        else => -1,
    };
}

// =============================================================================
// Legacy Async File Operations (Opcodes 0-5)
// =============================================================================

fn readStart(exec_env: c.wasm_exec_env_t, path_ptr: u32, path_len: u32) i32 {
    const raw_path = readWasmMemory(exec_env, path_ptr, path_len) orelse return -1;

    // Apply mount remapping (guest path -> host path)
    var owned_path: ?[]const u8 = null;
    defer if (owned_path) |p| g_allocator.free(p);
    const path = if (g_config) |config| blk: {
        if (config.remapPath(g_allocator, raw_path)) |remapped| {
            owned_path = remapped;
            break :blk remapped;
        }
        break :blk raw_path;
    } else raw_path;

    // Find free slot
    var slot_idx: ?usize = null;
    for (&g_file_ops, 0..) |*slot, i| {
        if (slot.* == null) {
            slot_idx = i;
            break;
        }
    }
    if (slot_idx == null) return @intFromEnum(errors.ErrorCode.slot_exhausted);

    const request_id = g_next_file_id;
    g_next_file_id +%= 1;

    // Read file synchronously (async can be added later with threads)
    const file = std.fs.cwd().openFile(path, .{}) catch |err| {
        g_file_ops[slot_idx.?] = AsyncFileRequest{
            .id = request_id,
            .op = .read,
            .status = .error_state,
            .data = null,
            .error_msg = switch (err) {
                error.FileNotFound => "ENOENT",
                error.AccessDenied => "EACCES",
                else => "EIO",
            },
            .bytes_written = 0,
        };
        return @intCast(request_id);
    };
    defer file.close();

    const content = file.readToEndAlloc(g_allocator, 100 * 1024 * 1024) catch {
        g_file_ops[slot_idx.?] = AsyncFileRequest{
            .id = request_id,
            .op = .read,
            .status = .error_state,
            .data = null,
            .error_msg = "EIO",
            .bytes_written = 0,
        };
        return @intCast(request_id);
    };

    g_file_ops[slot_idx.?] = AsyncFileRequest{
        .id = request_id,
        .op = .read,
        .status = .complete,
        .data = content,
        .error_msg = null,
        .bytes_written = content.len,
    };

    return @intCast(request_id);
}

fn writeStart(exec_env: c.wasm_exec_env_t, path_ptr: u32, path_len: u32, data_ptr: u32, data_len: u32) i32 {
    const raw_path = readWasmMemory(exec_env, path_ptr, path_len) orelse return -1;
    const data = readWasmMemory(exec_env, data_ptr, data_len) orelse return -2;

    // Apply mount remapping (guest path -> host path)
    var owned_path: ?[]const u8 = null;
    defer if (owned_path) |p| g_allocator.free(p);
    const path = if (g_config) |config| blk: {
        if (config.remapPath(g_allocator, raw_path)) |remapped| {
            owned_path = remapped;
            break :blk remapped;
        }
        break :blk raw_path;
    } else raw_path;

    // Find free slot
    var slot_idx: ?usize = null;
    for (&g_file_ops, 0..) |*slot, i| {
        if (slot.* == null) {
            slot_idx = i;
            break;
        }
    }
    if (slot_idx == null) return @intFromEnum(errors.ErrorCode.slot_exhausted);

    const request_id = g_next_file_id;
    g_next_file_id +%= 1;

    // Create parent directories if needed
    if (std.mem.lastIndexOf(u8, path, "/")) |last_slash| {
        if (last_slash > 0) {
            const parent = path[0..last_slash];
            if (path[0] == '/') {
                std.fs.makeDirAbsolute(parent) catch {};
            } else {
                std.fs.cwd().makePath(parent) catch {};
            }
        }
    }

    // Write file
    const abs_file = if (path[0] == '/')
        std.fs.openFileAbsolute(path, .{ .mode = .write_only })
    else
        std.fs.cwd().openFile(path, .{ .mode = .write_only });

    const file = abs_file catch |err| {
        // Try to create the file
        const create_result = if (path[0] == '/')
            std.fs.createFileAbsolute(path, .{})
        else
            std.fs.cwd().createFile(path, .{});

        const created_file = create_result catch {
            g_file_ops[slot_idx.?] = AsyncFileRequest{
                .id = request_id,
                .op = .write,
                .status = .error_state,
                .data = null,
                .error_msg = switch (err) {
                    error.FileNotFound => "ENOENT",
                    error.AccessDenied => "EACCES",
                    else => "EIO",
                },
                .bytes_written = 0,
            };
            return @intCast(request_id);
        };

        created_file.writeAll(data) catch {
            created_file.close();
            g_file_ops[slot_idx.?] = AsyncFileRequest{
                .id = request_id,
                .op = .write,
                .status = .error_state,
                .data = null,
                .error_msg = "EIO",
                .bytes_written = 0,
            };
            return @intCast(request_id);
        };
        created_file.close();

        g_file_ops[slot_idx.?] = AsyncFileRequest{
            .id = request_id,
            .op = .write,
            .status = .complete,
            .data = null,
            .error_msg = null,
            .bytes_written = data.len,
        };
        return @intCast(request_id);
    };

    file.writeAll(data) catch {
        file.close();
        g_file_ops[slot_idx.?] = AsyncFileRequest{
            .id = request_id,
            .op = .write,
            .status = .error_state,
            .data = null,
            .error_msg = "EIO",
            .bytes_written = 0,
        };
        return @intCast(request_id);
    };
    file.close();

    g_file_ops[slot_idx.?] = AsyncFileRequest{
        .id = request_id,
        .op = .write,
        .status = .complete,
        .data = null,
        .error_msg = null,
        .bytes_written = data.len,
    };

    return @intCast(request_id);
}

fn poll(request_id: u32) i32 {
    for (&g_file_ops) |*slot| {
        if (slot.*) |*req| {
            if (req.id == request_id) {
                return switch (req.status) {
                    .pending => 0,
                    .complete => 1,
                    .error_state => -1,
                };
            }
        }
    }
    return -2; // Not found
}

fn resultLen(request_id: u32) i32 {
    for (&g_file_ops) |*slot| {
        if (slot.*) |*req| {
            if (req.id == request_id) {
                if (req.data) |data| {
                    return @intCast(data.len);
                }
                return 0;
            }
        }
    }
    return -1;
}

fn result(exec_env: c.wasm_exec_env_t, request_id: u32, dest_ptr: u32) i32 {
    for (&g_file_ops) |*slot| {
        if (slot.*) |*req| {
            if (req.id == request_id) {
                if (req.data) |data| {
                    if (!writeWasmMemory(exec_env, dest_ptr, data)) {
                        return -1;
                    }
                    return @intCast(data.len);
                }
                return 0;
            }
        }
    }
    return -1;
}

fn free(request_id: u32) i32 {
    for (&g_file_ops) |*slot| {
        if (slot.*) |*req| {
            if (req.id == request_id) {
                if (req.data) |data| {
                    g_allocator.free(data);
                }
                slot.* = null;
                return 0;
            }
        }
    }
    return -1;
}

// =============================================================================
// Component Model File Operations (Opcodes 100+)
// =============================================================================

// FS error codes (match Node.js conventions)
const FS_ERR_ENOENT: i32 = -2; // No such file or directory
const FS_ERR_EACCES: i32 = -3; // Permission denied
const FS_ERR_EEXIST: i32 = -4; // File already exists
const FS_ERR_ENOTDIR: i32 = -5; // Not a directory
const FS_ERR_EISDIR: i32 = -6; // Is a directory
const FS_ERR_ENOTEMPTY: i32 = -7; // Directory not empty
const FS_ERR_EIO: i32 = -8; // I/O error
const FS_ERR_EINVAL: i32 = -9; // Invalid argument

/// Map Component Model fs-error enum to error codes
fn mapFsErrorToCode(err_val: u32) i32 {
    return switch (err_val) {
        0 => FS_ERR_ENOENT, // not_found
        1 => FS_ERR_EACCES, // permission_denied
        2 => FS_ERR_EEXIST, // already_exists
        3 => FS_ERR_EINVAL, // invalid_path
        4 => FS_ERR_ENOTDIR, // not_a_directory
        5 => FS_ERR_EISDIR, // not_a_file
        6 => FS_ERR_ENOTEMPTY, // directory_not_empty
        7 => FS_ERR_EIO, // io_error
        8 => FS_ERR_EINVAL, // invalid_encoding
        else => -1,
    };
}

fn cmRead(exec_env: c.wasm_exec_env_t, registry_opt: ?*NativeRegistry, path_ptr: u32, path_len: u32) i32 {
    const registry = registry_opt orelse return -1;
    const path = readStringFromWasm(exec_env, path_ptr, path_len) orelse return -1;

    const args = [_]Value{
        Value{ .string = path },
        Value{ .u32 = 0 }, // encoding = utf8
    };

    const cm_result = registry.call("filesystem", "read-file", &args) catch return -1;

    if (cm_result.isOk()) {
        const content = cm_result.asOkString() catch return -1;
        if (g_cm_result) |old| {
            g_cm_result_allocator.free(old);
        }
        g_cm_result = g_cm_result_allocator.dupe(u8, content) catch return -1;
        return 0;
    } else {
        const err_val = cm_result.asErr() catch return -1;
        return mapFsErrorToCode(err_val);
    }
}

fn cmWrite(exec_env: c.wasm_exec_env_t, registry_opt: ?*NativeRegistry, path_ptr: u32, path_len: u32, data_ptr: u32, data_len: u32) i32 {
    const registry = registry_opt orelse return -1;
    const path = readStringFromWasm(exec_env, path_ptr, path_len) orelse return -1;
    const data = readStringFromWasm(exec_env, data_ptr, data_len) orelse return -1;

    const args = [_]Value{
        Value{ .string = path },
        Value{ .string = data },
    };

    const cm_result = registry.call("filesystem", "write-file", &args) catch return -1;

    if (cm_result.isOk()) {
        return 0;
    } else {
        const err_val = cm_result.asErr() catch return -1;
        return mapFsErrorToCode(err_val);
    }
}

fn cmExists(exec_env: c.wasm_exec_env_t, registry_opt: ?*NativeRegistry, path_ptr: u32, path_len: u32) i32 {
    const registry = registry_opt orelse return -1;
    const path = readStringFromWasm(exec_env, path_ptr, path_len) orelse return -1;

    const args = [_]Value{
        Value{ .string = path },
    };

    const cm_result = registry.call("filesystem", "exists", &args) catch return 0;

    // exists returns a bool directly (not a result type)
    if (cm_result == .bool) {
        return if (cm_result.bool) 1 else 0;
    }
    return 0;
}

fn cmStat(exec_env: c.wasm_exec_env_t, registry_opt: ?*NativeRegistry, path_ptr: u32, path_len: u32) i32 {
    const registry = registry_opt orelse return -1;
    const path = readStringFromWasm(exec_env, path_ptr, path_len) orelse return -1;

    const args = [_]Value{
        Value{ .string = path },
    };

    const cm_result = registry.call("filesystem", "stat", &args) catch return -1;

    if (cm_result.isOk()) {
        // Serialize file-stat as binary: size(8) | mode(4) | is_file(1) | is_dir(1) | mtime(8) | ctime(8) | atime(8)
        const stat = cm_result.asOkFileStat() catch return -1;
        var buf: [38]u8 = undefined;

        // size (8 bytes, little endian)
        std.mem.writeInt(i64, buf[0..8], @intCast(stat.size), .little);

        // mode (4 bytes, little endian) - compute from is_file/is_directory
        const mode: i32 = if (stat.is_directory) 0o40755 else 0o100644;
        std.mem.writeInt(i32, buf[8..12], mode, .little);

        // is_file (1 byte)
        buf[12] = if (stat.is_file) 1 else 0;

        // is_directory (1 byte)
        buf[13] = if (stat.is_directory) 1 else 0;

        // mtime (8 bytes, little endian)
        std.mem.writeInt(i64, buf[14..22], stat.modified_time, .little);

        // ctime (8 bytes, little endian)
        std.mem.writeInt(i64, buf[22..30], stat.created_time, .little);

        // atime (8 bytes, little endian)
        std.mem.writeInt(i64, buf[30..38], stat.accessed_time, .little);

        if (g_cm_result) |old| {
            g_cm_result_allocator.free(old);
        }
        g_cm_result = g_cm_result_allocator.dupe(u8, &buf) catch return -1;
        return 0;
    } else {
        const err_val = cm_result.asErr() catch return -1;
        return mapFsErrorToCode(err_val);
    }
}

fn cmReaddir(exec_env: c.wasm_exec_env_t, registry_opt: ?*NativeRegistry, path_ptr: u32, path_len: u32) i32 {
    const registry = registry_opt orelse return -1;
    const path = readStringFromWasm(exec_env, path_ptr, path_len) orelse return -1;

    const args = [_]Value{
        Value{ .string = path },
    };

    const cm_result = registry.call("filesystem", "read-dir", &args) catch return -1;

    if (cm_result.isOk()) {
        // Serialize directory entries as binary: count(4) | [len(4) | name(len)]...
        const entries = cm_result.asOkDirEntries() catch return -1;
        defer g_cm_result_allocator.free(entries);

        var bin_list = std.ArrayListUnmanaged(u8){};
        defer bin_list.deinit(g_cm_result_allocator);

        // Write count (4 bytes)
        var count_buf: [4]u8 = undefined;
        std.mem.writeInt(u32, &count_buf, @intCast(entries.len), .little);
        bin_list.appendSlice(g_cm_result_allocator, &count_buf) catch return -1;

        for (entries) |entry| {
            // Write name length (4 bytes)
            var len_buf: [4]u8 = undefined;
            std.mem.writeInt(u32, &len_buf, @intCast(entry.name.len), .little);
            bin_list.appendSlice(g_cm_result_allocator, &len_buf) catch return -1;

            // Write name
            bin_list.appendSlice(g_cm_result_allocator, entry.name) catch return -1;
            g_cm_result_allocator.free(entry.name);
        }

        if (g_cm_result) |old| {
            g_cm_result_allocator.free(old);
        }
        g_cm_result = bin_list.toOwnedSlice(g_cm_result_allocator) catch return -1;
        return 0;
    } else {
        const err_val = cm_result.asErr() catch return -1;
        return mapFsErrorToCode(err_val);
    }
}

fn cmMkdir(exec_env: c.wasm_exec_env_t, registry_opt: ?*NativeRegistry, path_ptr: u32, path_len: u32, recursive: u32) i32 {
    const registry = registry_opt orelse return -1;
    const path = readStringFromWasm(exec_env, path_ptr, path_len) orelse return -1;

    const args = [_]Value{
        Value{ .string = path },
        Value{ .bool = recursive != 0 },
    };

    const cm_result = registry.call("filesystem", "mkdir", &args) catch return -1;

    if (cm_result.isOk()) {
        return 0;
    } else {
        const err_val = cm_result.asErr() catch return -1;
        return mapFsErrorToCode(err_val);
    }
}

fn cmUnlink(exec_env: c.wasm_exec_env_t, registry_opt: ?*NativeRegistry, path_ptr: u32, path_len: u32) i32 {
    const registry = registry_opt orelse return -1;
    const path = readStringFromWasm(exec_env, path_ptr, path_len) orelse return -1;

    const args = [_]Value{
        Value{ .string = path },
    };

    const cm_result = registry.call("filesystem", "remove-file", &args) catch return -1;

    if (cm_result.isOk()) {
        return 0;
    } else {
        const err_val = cm_result.asErr() catch return -1;
        return mapFsErrorToCode(err_val);
    }
}

fn cmRmdir(exec_env: c.wasm_exec_env_t, registry_opt: ?*NativeRegistry, path_ptr: u32, path_len: u32, recursive: u32) i32 {
    const registry = registry_opt orelse return -1;
    const path = readStringFromWasm(exec_env, path_ptr, path_len) orelse return -1;

    const args = [_]Value{
        Value{ .string = path },
        Value{ .bool = recursive != 0 },
    };

    const cm_result = registry.call("filesystem", "remove-dir", &args) catch return -1;

    if (cm_result.isOk()) {
        return 0;
    } else {
        const err_val = cm_result.asErr() catch return -1;
        return mapFsErrorToCode(err_val);
    }
}

fn cmRename(exec_env: c.wasm_exec_env_t, registry_opt: ?*NativeRegistry, old_ptr: u32, old_len: u32, new_ptr: u32, new_len: u32) i32 {
    const registry = registry_opt orelse return -1;
    const old_path = readStringFromWasm(exec_env, old_ptr, old_len) orelse return -1;
    const new_path = readStringFromWasm(exec_env, new_ptr, new_len) orelse return -1;

    const args = [_]Value{
        Value{ .string = old_path },
        Value{ .string = new_path },
    };

    const cm_result = registry.call("filesystem", "rename", &args) catch return -1;

    if (cm_result.isOk()) {
        return 0;
    } else {
        const err_val = cm_result.asErr() catch return -1;
        return mapFsErrorToCode(err_val);
    }
}

fn cmCopy(exec_env: c.wasm_exec_env_t, registry_opt: ?*NativeRegistry, src_ptr: u32, src_len: u32, dest_ptr: u32, dest_len: u32) i32 {
    const registry = registry_opt orelse return -1;
    const src_path = readStringFromWasm(exec_env, src_ptr, src_len) orelse return -1;
    const dest_path = readStringFromWasm(exec_env, dest_ptr, dest_len) orelse return -1;

    const args = [_]Value{
        Value{ .string = src_path },
        Value{ .string = dest_path },
    };

    const cm_result = registry.call("filesystem", "copy-file", &args) catch return -1;

    if (cm_result.isOk()) {
        return 0;
    } else {
        const err_val = cm_result.asErr() catch return -1;
        return mapFsErrorToCode(err_val);
    }
}

fn cmGetResultLen() i32 {
    if (g_cm_result) |file_result| {
        return @intCast(file_result.len);
    }
    return -1;
}

fn cmGetResult(exec_env: c.wasm_exec_env_t, out_ptr: u32, max_len: u32) i32 {
    _ = max_len;
    if (g_cm_result) |file_result| {
        if (writeStringToWasm(exec_env, out_ptr, file_result)) {
            return @intCast(file_result.len);
        }
    }
    return -1;
}

// =============================================================================
// Internal Helpers
// =============================================================================

fn readWasmMemory(exec_env: c.wasm_exec_env_t, ptr: u32, len: u32) ?[]const u8 {
    return wasm_helpers.readWasmMemory(exec_env, ptr, len);
}

fn writeWasmMemory(exec_env: c.wasm_exec_env_t, ptr: u32, data: []const u8) bool {
    return wasm_helpers.writeWasmMemory(exec_env, ptr, data);
}

fn readStringFromWasm(exec_env: c.wasm_exec_env_t, ptr: u32, len: u32) ?[]const u8 {
    const module_inst = c.wasm_runtime_get_module_inst(exec_env);
    if (module_inst == null) return null;
    if (!c.wasm_runtime_validate_app_addr(module_inst, ptr, len)) return null;
    const native_ptr = c.wasm_runtime_addr_app_to_native(module_inst, ptr);
    if (native_ptr == null) return null;
    const bytes: [*]const u8 = @ptrCast(native_ptr);
    return bytes[0..len];
}

fn writeStringToWasm(exec_env: c.wasm_exec_env_t, ptr: u32, data: []const u8) bool {
    const module_inst = c.wasm_runtime_get_module_inst(exec_env);
    if (module_inst == null) return false;
    if (!c.wasm_runtime_validate_app_addr(module_inst, ptr, @intCast(data.len))) return false;
    const native_ptr = c.wasm_runtime_addr_app_to_native(module_inst, ptr);
    if (native_ptr == null) return false;
    const dest: [*]u8 = @ptrCast(native_ptr);
    @memcpy(dest[0..data.len], data);
    return true;
}
