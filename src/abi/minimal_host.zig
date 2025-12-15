//! Minimal Host Implementation for EdgeBox ABI
//!
//! This implements the 13 host functions that the WASM sandbox needs.
//! All complex logic (HTTP parsing, crypto, etc.) runs in WASM.
//!
//! Host provides ONLY:
//! - Random bytes (for crypto)
//! - Raw socket I/O (for HTTP)
//! - Raw filesystem I/O (for fs)
//! - Process spawning (for child_process)
//! - Current time
//!
//! Design: Maximum security through minimal attack surface.
//! A bug in HTTP parsing crashes WASM, not the host.

const std = @import("std");
const builtin = @import("builtin");

// WAMR C API
const c = @cImport({
    @cInclude("wasm_export.h");
});

const allocator = std.heap.page_allocator;

// ============================================================================
// Symbol Table for WAMR Registration
// ============================================================================

/// Register minimal host functions with WAMR runtime.
/// Call this before running WASM modules that use the new ABI.
pub fn registerNatives() bool {
    return c.wasm_runtime_register_natives(
        "edgebox_host",
        @constCast(&g_host_symbols),
        g_host_symbols.len,
    );
}

const g_host_symbols = [_]c.NativeSymbol{
    // Random (only crypto from host)
    .{ .symbol = "host_random_get", .func_ptr = @constCast(@ptrCast(&hostRandomGet)), .signature = "(*i)" },

    // Network (raw sockets)
    .{ .symbol = "host_net_connect", .func_ptr = @constCast(@ptrCast(&hostNetConnect)), .signature = "(*ii)i" },
    .{ .symbol = "host_net_send", .func_ptr = @constCast(@ptrCast(&hostNetSend)), .signature = "(i*i)i" },
    .{ .symbol = "host_net_recv", .func_ptr = @constCast(@ptrCast(&hostNetRecv)), .signature = "(i*i)i" },
    .{ .symbol = "host_net_close", .func_ptr = @constCast(@ptrCast(&hostNetClose)), .signature = "(i)" },

    // Process (raw fd operations)
    .{ .symbol = "host_proc_spawn", .func_ptr = @constCast(@ptrCast(&hostProcSpawn)), .signature = "(*i***)i" },
    .{ .symbol = "host_proc_wait", .func_ptr = @constCast(@ptrCast(&hostProcWait)), .signature = "(i)i" },
    .{ .symbol = "host_proc_kill", .func_ptr = @constCast(@ptrCast(&hostProcKill)), .signature = "(ii)i" },

    // Filesystem (raw fd operations)
    .{ .symbol = "host_fs_open", .func_ptr = @constCast(@ptrCast(&hostFsOpen)), .signature = "(*ii)i" },
    .{ .symbol = "host_fs_read", .func_ptr = @constCast(@ptrCast(&hostFsRead)), .signature = "(i*i)i" },
    .{ .symbol = "host_fs_write", .func_ptr = @constCast(@ptrCast(&hostFsWrite)), .signature = "(i*i)i" },
    .{ .symbol = "host_fs_close", .func_ptr = @constCast(@ptrCast(&hostFsClose)), .signature = "(i)" },
    .{ .symbol = "host_fs_stat", .func_ptr = @constCast(@ptrCast(&hostFsStat)), .signature = "(*i*)i" },
    .{ .symbol = "host_fs_readdir", .func_ptr = @constCast(@ptrCast(&hostFsReaddir)), .signature = "(*i*i)i" },

    // Time
    .{ .symbol = "host_time_now", .func_ptr = @constCast(@ptrCast(&hostTimeNow)), .signature = "()I" },
};

// ============================================================================
// Random - The ONLY crypto operation from host
// ============================================================================

fn hostRandomGet(exec_env: c.wasm_exec_env_t, ptr: u32, len: u32) void {
    const module_inst = c.wasm_runtime_get_module_inst(exec_env);
    const buf = c.wasm_runtime_addr_app_to_native(module_inst, ptr);
    if (buf == null) return;

    const slice: [*]u8 = @ptrCast(buf);
    std.crypto.random.bytes(slice[0..len]);
}

// ============================================================================
// Network - Raw socket operations
// ============================================================================

fn hostNetConnect(exec_env: c.wasm_exec_env_t, host_ptr: u32, host_len: u32, port: u16) i32 {
    const module_inst = c.wasm_runtime_get_module_inst(exec_env);
    const host_buf = c.wasm_runtime_addr_app_to_native(module_inst, host_ptr);
    if (host_buf == null) return -1;

    const host_slice: [*]const u8 = @ptrCast(host_buf);
    const hostname = host_slice[0..host_len];

    // Resolve and connect
    const stream = std.net.tcpConnectToHost(allocator, hostname, port) catch return -1;
    return stream.handle;
}

fn hostNetSend(exec_env: c.wasm_exec_env_t, fd: i32, ptr: u32, len: u32) i32 {
    const module_inst = c.wasm_runtime_get_module_inst(exec_env);
    const buf = c.wasm_runtime_addr_app_to_native(module_inst, ptr);
    if (buf == null) return -1;

    const slice: [*]const u8 = @ptrCast(buf);
    const stream = std.net.Stream{ .handle = @intCast(fd) };
    const written = stream.write(slice[0..len]) catch return -1;
    return @intCast(written);
}

fn hostNetRecv(exec_env: c.wasm_exec_env_t, fd: i32, ptr: u32, len: u32) i32 {
    const module_inst = c.wasm_runtime_get_module_inst(exec_env);
    const buf = c.wasm_runtime_addr_app_to_native(module_inst, ptr);
    if (buf == null) return -1;

    const slice: [*]u8 = @ptrCast(buf);
    const stream = std.net.Stream{ .handle = @intCast(fd) };
    const read_len = stream.read(slice[0..len]) catch return -1;
    return @intCast(read_len);
}

fn hostNetClose(_: c.wasm_exec_env_t, fd: i32) void {
    const stream = std.net.Stream{ .handle = @intCast(fd) };
    stream.close();
}

// ============================================================================
// Process - Raw fd operations for spawning
// ============================================================================

/// Process handle storage (maps to fd triplets)
const ProcessHandle = struct {
    pid: std.process.Child.Id,
    stdin_fd: i32,
    stdout_fd: i32,
    stderr_fd: i32,
};

var process_handles: [64]?ProcessHandle = [_]?ProcessHandle{null} ** 64;
var next_process_handle: usize = 0;

fn hostProcSpawn(
    exec_env: c.wasm_exec_env_t,
    cmd_ptr: u32,
    cmd_len: u32,
    stdin_fd_ptr: u32,
    stdout_fd_ptr: u32,
    stderr_fd_ptr: u32,
) i32 {
    const module_inst = c.wasm_runtime_get_module_inst(exec_env);

    const cmd_buf = c.wasm_runtime_addr_app_to_native(module_inst, cmd_ptr);
    if (cmd_buf == null) return -1;

    const cmd_slice: [*]const u8 = @ptrCast(cmd_buf);
    const cmd = cmd_slice[0..cmd_len];

    // Parse command (simple shell-style split)
    var argv = std.ArrayListUnmanaged([]const u8){};
    defer argv.deinit(allocator);

    var iter = std.mem.splitScalar(u8, cmd, ' ');
    while (iter.next()) |arg| {
        if (arg.len > 0) {
            argv.append(allocator, arg) catch return -1;
        }
    }

    if (argv.items.len == 0) return -1;

    // Spawn child process
    var child = std.process.Child.init(argv.items, allocator);
    child.stdin_behavior = .Pipe;
    child.stdout_behavior = .Pipe;
    child.stderr_behavior = .Pipe;

    child.spawn() catch return -1;

    // Get file descriptors
    const stdin_fd: i32 = if (child.stdin) |s| @intCast(s.handle) else -1;
    const stdout_fd: i32 = if (child.stdout) |s| @intCast(s.handle) else -1;
    const stderr_fd: i32 = if (child.stderr) |s| @intCast(s.handle) else -1;

    // Write fds back to WASM memory
    const stdin_out = c.wasm_runtime_addr_app_to_native(module_inst, stdin_fd_ptr);
    const stdout_out = c.wasm_runtime_addr_app_to_native(module_inst, stdout_fd_ptr);
    const stderr_out = c.wasm_runtime_addr_app_to_native(module_inst, stderr_fd_ptr);

    if (stdin_out != null) {
        const p: *i32 = @ptrCast(@alignCast(stdin_out));
        p.* = stdin_fd;
    }
    if (stdout_out != null) {
        const p: *i32 = @ptrCast(@alignCast(stdout_out));
        p.* = stdout_fd;
    }
    if (stderr_out != null) {
        const p: *i32 = @ptrCast(@alignCast(stderr_out));
        p.* = stderr_fd;
    }

    // Store process handle for wait/kill
    const handle_idx = next_process_handle;
    next_process_handle = (next_process_handle + 1) % 64;

    process_handles[handle_idx] = ProcessHandle{
        .pid = child.id,
        .stdin_fd = stdin_fd,
        .stdout_fd = stdout_fd,
        .stderr_fd = stderr_fd,
    };

    // Return pid as handle
    return @intCast(child.id);
}

fn hostProcWait(_: c.wasm_exec_env_t, pid: i32) i32 {
    // Find process handle
    for (&process_handles) |*maybe_handle| {
        if (maybe_handle.*) |handle| {
            if (@as(i32, @intCast(handle.pid)) == pid) {
                // Wait using posix directly
                const status = std.posix.waitpid(@intCast(pid), 0);
                const exit_code: i32 = if (std.posix.W.IFEXITED(status.status))
                    @intCast(std.posix.W.EXITSTATUS(status.status))
                else
                    -1;

                maybe_handle.* = null;
                return exit_code;
            }
        }
    }
    return -1;
}

fn hostProcKill(_: c.wasm_exec_env_t, pid: i32, signal: i32) i32 {
    if (builtin.os.tag == .windows) return -1;

    // Use C kill directly for cross-platform signal handling
    const result = std.c.kill(@intCast(pid), @intCast(signal));
    if (result != 0) return -1;
    return 0;
}

// ============================================================================
// Filesystem - Raw fd operations
// ============================================================================

fn hostFsOpen(exec_env: c.wasm_exec_env_t, path_ptr: u32, path_len: u32, flags: u32) i32 {
    const module_inst = c.wasm_runtime_get_module_inst(exec_env);
    const path_buf = c.wasm_runtime_addr_app_to_native(module_inst, path_ptr);
    if (path_buf == null) return -1;

    const path_slice: [*]const u8 = @ptrCast(path_buf);
    const path = path_slice[0..path_len];

    // Convert flags (matching host.zig constants)
    const O_WRONLY: u32 = 1;
    const O_RDWR: u32 = 2;
    const O_CREAT: u32 = 0x40;
    const O_TRUNC: u32 = 0x200;
    const O_APPEND: u32 = 0x400;

    var std_flags: std.fs.File.OpenFlags = .{};
    if (flags & O_WRONLY != 0 or flags & O_RDWR != 0) {
        std_flags.mode = .write_only;
    }
    if (flags & O_RDWR != 0) {
        std_flags.mode = .read_write;
    }

    // For create/truncate, use createFile
    if (flags & O_CREAT != 0) {
        const file = std.fs.cwd().createFile(path, .{
            .truncate = (flags & O_TRUNC) != 0,
        }) catch return -1;
        return @intCast(file.handle);
    }

    // Regular open
    const file = std.fs.cwd().openFile(path, std_flags) catch return -1;

    // Handle append
    if (flags & O_APPEND != 0) {
        file.seekFromEnd(0) catch {};
    }

    return @intCast(file.handle);
}

fn hostFsRead(exec_env: c.wasm_exec_env_t, fd: i32, buf_ptr: u32, len: u32) i32 {
    const module_inst = c.wasm_runtime_get_module_inst(exec_env);
    const buf = c.wasm_runtime_addr_app_to_native(module_inst, buf_ptr);
    if (buf == null) return -1;

    const slice: [*]u8 = @ptrCast(buf);
    const file = std.fs.File{ .handle = @intCast(fd) };
    const read_len = file.read(slice[0..len]) catch return -1;
    return @intCast(read_len);
}

fn hostFsWrite(exec_env: c.wasm_exec_env_t, fd: i32, buf_ptr: u32, len: u32) i32 {
    const module_inst = c.wasm_runtime_get_module_inst(exec_env);
    const buf = c.wasm_runtime_addr_app_to_native(module_inst, buf_ptr);
    if (buf == null) return -1;

    const slice: [*]const u8 = @ptrCast(buf);
    const file = std.fs.File{ .handle = @intCast(fd) };
    const written = file.write(slice[0..len]) catch return -1;
    return @intCast(written);
}

fn hostFsClose(_: c.wasm_exec_env_t, fd: i32) void {
    const file = std.fs.File{ .handle = @intCast(fd) };
    file.close();
}

fn hostFsStat(exec_env: c.wasm_exec_env_t, path_ptr: u32, path_len: u32, stat_buf_ptr: u32) i32 {
    const module_inst = c.wasm_runtime_get_module_inst(exec_env);

    const path_buf = c.wasm_runtime_addr_app_to_native(module_inst, path_ptr);
    if (path_buf == null) return -1;

    const stat_buf = c.wasm_runtime_addr_app_to_native(module_inst, stat_buf_ptr);
    if (stat_buf == null) return -1;

    const path_slice: [*]const u8 = @ptrCast(path_buf);
    const path = path_slice[0..path_len];

    const stat = std.fs.cwd().statFile(path) catch return -1;

    // Write stat info to buffer: [size:u64][mtime:i64][mode:u32][is_dir:u8]
    const out: [*]u8 = @ptrCast(stat_buf);

    std.mem.writeInt(u64, out[0..8], stat.size, .little);
    std.mem.writeInt(i64, out[8..16], @intCast(@divFloor(stat.mtime, std.time.ns_per_s)), .little);
    std.mem.writeInt(u32, out[16..20], @intFromEnum(stat.kind), .little);
    out[20] = if (stat.kind == .directory) 1 else 0;

    return 0;
}

fn hostFsReaddir(exec_env: c.wasm_exec_env_t, path_ptr: u32, path_len: u32, buf_ptr: u32, buf_len: u32) i32 {
    const module_inst = c.wasm_runtime_get_module_inst(exec_env);

    const path_buf = c.wasm_runtime_addr_app_to_native(module_inst, path_ptr);
    if (path_buf == null) return -1;

    const out_buf = c.wasm_runtime_addr_app_to_native(module_inst, buf_ptr);
    if (out_buf == null) return -1;

    const path_slice: [*]const u8 = @ptrCast(path_buf);
    const path = path_slice[0..path_len];

    var dir = std.fs.cwd().openDir(path, .{ .iterate = true }) catch return -1;
    defer dir.close();

    const out: [*]u8 = @ptrCast(out_buf);
    var pos: usize = 0;
    var count: i32 = 0;

    var iter = dir.iterate();
    while (iter.next() catch null) |entry| {
        if (pos + entry.name.len + 1 > buf_len) break;
        @memcpy(out[pos .. pos + entry.name.len], entry.name);
        out[pos + entry.name.len] = 0; // null separator
        pos += entry.name.len + 1;
        count += 1;
    }

    // Double null to mark end
    if (pos < buf_len) out[pos] = 0;

    return count;
}

// ============================================================================
// Time
// ============================================================================

fn hostTimeNow(_: c.wasm_exec_env_t) i64 {
    const ns = std.time.nanoTimestamp();
    return @intCast(@divFloor(ns, std.time.ns_per_ms));
}
