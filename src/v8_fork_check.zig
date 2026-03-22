//! Fork-based parallel type checking.
//!
//! After main's createProgram completes, fork() N children.
//! Children inherit V8 heap via COW (zero copy of parsed SourceFiles).
//! Each child checks its shard via getSemanticDiagnostics(file).
//! Diagnostics written to pipes, parent collects + deduplicates.
//!
//! This is the zero-copy parallel approach: no re-parse, no re-create program.
//! Children share the EXACT same V8 heap state as parent.

const std = @import("std");
const v8 = @import("v8.zig");
const v8_io = @import("v8_io.zig");

const alloc = std.heap.page_allocator;
const max_children = 8;

/// Fork-based parallel check state
var g_pipes: [max_children][2]std.posix.fd_t = undefined;
var g_child_pids: [max_children]std.posix.pid_t = .{0} ** max_children;
var g_num_children: usize = 0;

/// Called from JS via __edgebox_fork_check(workerCount).
/// Forks N children. Returns child_id (0..N-1) in children, -1 in parent.
pub fn forkCheckCallback(info: *const v8.FunctionCallbackInfo) callconv(.c) void {
    const iso = v8.CallbackInfoApi.getIsolate(info);
    const context = v8.IsolateApi.getCurrentContext(iso);

    // Get worker count from argument
    const num_workers: usize = blk: {
        if (v8.CallbackInfoApi.length(info) >= 1) {
            const arg = v8.CallbackInfoApi.get(info, 0) orelse break :blk 4;
            if (v8.ValueApi.isNumber(arg)) {
                const n = v8.NumberApi.value(@ptrCast(arg));
                break :blk @intCast(@as(i64, @intFromFloat(n)));
            }
        }
        break :blk 4;
    };

    const actual_workers = @min(num_workers, max_children);
    g_num_children = actual_workers;

    // Create pipes for each child
    for (0..actual_workers) |i| {
        g_pipes[i] = std.posix.pipe() catch {
            const result = v8.NumberApi.create(iso, -2);
            v8.CallbackInfoApi.setReturnValue(info, @ptrCast(result));
            return;
        };
    }

    // Flush all V8/Zig state before fork
    v8_io.flushAll();

    // Fork children
    for (0..actual_workers) |i| {
        const pid = std.posix.fork() catch {
            const result = v8.NumberApi.create(iso, -3);
            v8.CallbackInfoApi.setReturnValue(info, @ptrCast(result));
            return;
        };

        if (pid == 0) {
            // CHILD: close read end, set worker state
            std.posix.close(g_pipes[i][0]);
            v8_io.is_worker_thread = true;
            v8_io.worker_id = @intCast(i);

            // Set JS globals for this child
            var buf: [128]u8 = undefined;
            const js = std.fmt.bufPrint(&buf,
                "globalThis.__edgebox_fork_child_id={d};globalThis.__edgebox_fork_count={d};",
                .{ i, actual_workers },
            ) catch {
                std.posix.exit(1);
            };
            _ = v8.eval(iso, context, js, "fork_child_init.js") catch {};

            // Return child_id to JS
            const result = v8.NumberApi.create(iso, @floatFromInt(i));
            v8.CallbackInfoApi.setReturnValue(info, @ptrCast(result));
            return;
        } else {
            // PARENT: close write end, save child PID
            std.posix.close(g_pipes[i][1]);
            g_child_pids[i] = pid;
        }
    }

    // PARENT: return -1
    const result = v8.NumberApi.create(iso, -1);
    v8.CallbackInfoApi.setReturnValue(info, @ptrCast(result));
}

/// Called from child JS: write diagnostic line to parent via pipe
pub fn forkWriteDiagCallback(info: *const v8.FunctionCallbackInfo) callconv(.c) void {
    const iso = v8.CallbackInfoApi.getIsolate(info);
    if (v8.CallbackInfoApi.length(info) < 1) return;
    const arg = v8.CallbackInfoApi.get(info, 0) orelse return;
    if (!v8.ValueApi.isString(arg)) return;

    const str: *const v8.String = @ptrCast(arg);
    const len: usize = @intCast(v8.StringApi.utf8Length(str, iso));
    if (len == 0) return;

    var stack_buf: [8192]u8 = undefined;
    const buf = if (len <= stack_buf.len) &stack_buf else (alloc.alloc(u8, len) catch return);
    defer if (len > stack_buf.len) alloc.free(buf);
    const written = v8.StringApi.writeUtf8(str, iso, buf);

    // Write to pipe (child's write end)
    const child_id = v8_io.worker_id;
    if (child_id < max_children) {
        const fd = g_pipes[child_id][1];
        var w: usize = 0;
        while (w < written) {
            const n = std.posix.write(fd, buf[w..written]) catch break;
            if (n == 0) break;
            w += n;
        }
    }
}

/// Called from parent: wait for all children, collect diagnostics from pipes
pub fn collectChildDiagnostics() void {
    var seen = std.StringHashMapUnmanaged(void){};
    defer seen.deinit(alloc);

    // Read from each child's pipe
    for (0..g_num_children) |i| {
        const fd = g_pipes[i][0];
        var buf = std.ArrayListUnmanaged(u8){};
        defer buf.deinit(alloc);

        // Read all data from pipe
        var read_buf: [4096]u8 = undefined;
        while (true) {
            const n = std.posix.read(fd, &read_buf) catch break;
            if (n == 0) break;
            buf.appendSlice(alloc, read_buf[0..n]) catch break;
        }
        std.posix.close(fd);

        // Dedup and output
        var start: usize = 0;
        for (0..buf.items.len) |j| {
            if (buf.items[j] == '\n') {
                const line = buf.items[start..j];
                if (line.len > 0) {
                    const gop = seen.getOrPut(alloc, line) catch {
                        start = j + 1;
                        continue;
                    };
                    if (!gop.found_existing) {
                        _ = std.posix.write(1, line) catch {};
                        _ = std.posix.write(1, "\n") catch {};
                    }
                }
                start = j + 1;
            }
        }
    }

    // Wait for all children
    for (0..g_num_children) |i| {
        if (g_child_pids[i] != 0) {
            _ = std.posix.waitpid(g_child_pids[i], 0);
            g_child_pids[i] = 0;
        }
    }
}

/// Register fork check globals on an isolate
pub fn registerForkGlobals(isolate: *v8.Isolate, context: *const v8.Context) void {
    const global = v8.ContextApi.global(context);

    const fork_tmpl = v8.FunctionTemplateApi.create(isolate, &forkCheckCallback) orelse return;
    const fork_func = v8.FunctionTemplateApi.getFunction(fork_tmpl, context) orelse return;
    const fork_key = v8.StringApi.fromUtf8(isolate, "__edgebox_fork_check") orelse return;
    _ = v8.ObjectApi.set(global, context, @ptrCast(fork_key), @ptrCast(fork_func));

    const diag_tmpl = v8.FunctionTemplateApi.create(isolate, &forkWriteDiagCallback) orelse return;
    const diag_func = v8.FunctionTemplateApi.getFunction(diag_tmpl, context) orelse return;
    const diag_key = v8.StringApi.fromUtf8(isolate, "__edgebox_fork_write_diag") orelse return;
    _ = v8.ObjectApi.set(global, context, @ptrCast(diag_key), @ptrCast(diag_func));
}
