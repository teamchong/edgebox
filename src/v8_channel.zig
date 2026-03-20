// v8_channel.zig — Go-like channels for cross-isolate communication
//
// Channels are Zig-managed ring buffers with mutex + condition variable.
// Both sender and receiver access the same Zig memory — no V8 heap sharing needed.
// Data is passed as JSON strings (for structured data) or raw bytes (for typed arrays).
//
// JS API:
//   const ch = edgebox.channel(10);  // buffered channel
//   ch.send(value);                  // blocks if full
//   const val = ch.recv();           // blocks if empty, null when closed
//   ch.close();                      // signal done

const std = @import("std");
const v8 = @import("v8.zig");

const alloc = std.heap.page_allocator;

const max_channels = 64;
const max_buffer_size = 4096;

// === Zero-copy shared buffer channel ===
// Uses a flat mmap'd buffer visible to both isolates as SharedArrayBuffer.
// Layout: [head:i32][tail:i32][closed:i32][padding:i32][data:f64 × capacity]
// JS code uses Atomics for lock-free send/recv on the Int32Array header.
const HEADER_BYTES = 16; // 4 × i32: head, tail, closed, padding

/// Allocate a shared buffer for a zero-copy channel
fn allocSharedBuffer(capacity: usize) ?[*]u8 {
    const total = HEADER_BYTES + capacity * 8; // 8 bytes per f64 slot
    const buf = alloc.alloc(u8, total) catch return null;
    @memset(buf, 0);
    return buf.ptr;
}

/// Global shared buffers (indexed by channel ID)
var shared_buffers: [max_channels]?[*]u8 = .{null} ** max_channels;
var shared_buffer_sizes: [max_channels]usize = .{0} ** max_channels;

/// Ring buffer channel with mutex synchronization
const Channel = struct {
    buffer: []?[]const u8,
    capacity: usize,
    head: usize = 0,
    tail: usize = 0,
    count: usize = 0,
    closed: bool = false,
    mutex: std.Thread.Mutex = .{},
    not_empty: std.Thread.Condition = .{},
    not_full: std.Thread.Condition = .{},
    active: bool = false,

    fn init(cap: usize) !Channel {
        const c = @min(if (cap == 0) 1 else cap, max_buffer_size);
        const buf = try alloc.alloc(?[]const u8, c);
        for (buf) |*slot| slot.* = null;
        return .{ .buffer = buf, .capacity = c, .active = true };
    }

    fn send(self: *Channel, data: []const u8) bool {
        self.mutex.lock();
        defer self.mutex.unlock();
        while (self.count >= self.capacity and !self.closed) {
            self.not_full.wait(&self.mutex);
        }
        if (self.closed) return false;
        self.buffer[self.tail] = alloc.dupe(u8, data) catch return false;
        self.tail = (self.tail + 1) % self.capacity;
        self.count += 1;
        self.not_empty.signal();
        return true;
    }

    fn recv(self: *Channel) ?[]const u8 {
        self.mutex.lock();
        defer self.mutex.unlock();
        while (self.count == 0 and !self.closed) {
            self.not_empty.wait(&self.mutex);
        }
        if (self.count == 0) return null;
        const data = self.buffer[self.head] orelse return null;
        self.buffer[self.head] = null;
        self.head = (self.head + 1) % self.capacity;
        self.count -= 1;
        self.not_full.signal();
        return data; // caller frees
    }

    fn close(self: *Channel) void {
        self.mutex.lock();
        defer self.mutex.unlock();
        self.closed = true;
        self.not_empty.broadcast();
        self.not_full.broadcast();
    }
};

var channels: [max_channels]Channel = undefined;
var channel_count: usize = 0;
var registry_mutex: std.Thread.Mutex = .{};

/// V8 callback: __edgebox_chan_create(capacity) → "channelId" string
pub fn chanCreateCallback(info: *const v8.FunctionCallbackInfo) callconv(.c) void {
    const isolate: *v8.Isolate = v8.CallbackInfoApi.getIsolate(info);
    var rv = v8.CallbackInfoApi.getReturnValue(info);

    var capacity: usize = 1;
    if (v8.CallbackInfoApi.length(info) >= 1) {
        if (v8.CallbackInfoApi.get(info, 0)) |arg| {
            if (v8.ValueApi.isString(arg)) {
                const str: *const v8.String = @ptrCast(arg);
                const len: usize = @intCast(v8.StringApi.utf8Length(str, isolate));
                if (len > 0 and len < 32) {
                    var buf: [32]u8 = undefined;
                    _ = v8.StringApi.writeUtf8(str, isolate, &buf);
                    capacity = std.fmt.parseInt(usize, buf[0..len], 10) catch 1;
                }
            }
        }
    }

    registry_mutex.lock();
    defer registry_mutex.unlock();
    if (channel_count >= max_channels) {
        rv.set(@ptrCast(v8.StringApi.fromUtf8(isolate, "-1") orelse return));
        return;
    }
    const id = channel_count;
    channels[id] = Channel.init(capacity) catch {
        rv.set(@ptrCast(v8.StringApi.fromUtf8(isolate, "-1") orelse return));
        return;
    };
    channel_count += 1;

    var id_buf: [16]u8 = undefined;
    const id_str = std.fmt.bufPrint(&id_buf, "{d}", .{id}) catch "-1";
    rv.set(@ptrCast(v8.StringApi.fromUtf8(isolate, id_str) orelse return));
}

/// V8 callback: __edgebox_chan_send(channelIdStr, jsonValueStr) → "true"/"false"
pub fn chanSendCallback(info: *const v8.FunctionCallbackInfo) callconv(.c) void {
    const isolate: *v8.Isolate = v8.CallbackInfoApi.getIsolate(info);
    var rv = v8.CallbackInfoApi.getReturnValue(info);

    if (v8.CallbackInfoApi.length(info) < 2) {
        rv.set(@ptrCast(v8.StringApi.fromUtf8(isolate, "false") orelse return));
        return;
    }

    // Parse channel ID from string
    const arg0 = v8.CallbackInfoApi.get(info, 0) orelse {
        rv.set(@ptrCast(v8.StringApi.fromUtf8(isolate, "false") orelse return));
        return;
    };
    if (!v8.ValueApi.isString(arg0)) {
        rv.set(@ptrCast(v8.StringApi.fromUtf8(isolate, "false") orelse return));
        return;
    }
    const id_s: *const v8.String = @ptrCast(arg0);
    const id_len: usize = @intCast(v8.StringApi.utf8Length(id_s, isolate));
    var id_buf: [16]u8 = undefined;
    _ = v8.StringApi.writeUtf8(id_s, isolate, &id_buf);
    const id = std.fmt.parseInt(usize, id_buf[0..id_len], 10) catch {
        rv.set(@ptrCast(v8.StringApi.fromUtf8(isolate, "false") orelse return));
        return;
    };
    if (id >= channel_count or !channels[id].active) {
        rv.set(@ptrCast(v8.StringApi.fromUtf8(isolate, "false") orelse return));
        return;
    }

    // Get value string
    const arg1 = v8.CallbackInfoApi.get(info, 1) orelse {
        rv.set(@ptrCast(v8.StringApi.fromUtf8(isolate, "false") orelse return));
        return;
    };
    if (!v8.ValueApi.isString(arg1)) {
        rv.set(@ptrCast(v8.StringApi.fromUtf8(isolate, "false") orelse return));
        return;
    }
    const val_s: *const v8.String = @ptrCast(arg1);
    const val_len: usize = @intCast(v8.StringApi.utf8Length(val_s, isolate));
    const val_buf = alloc.alloc(u8, val_len + 1) catch {
        rv.set(@ptrCast(v8.StringApi.fromUtf8(isolate, "false") orelse return));
        return;
    };
    defer alloc.free(val_buf);
    _ = v8.StringApi.writeUtf8(val_s, isolate, val_buf);

    const ok = channels[id].send(val_buf[0..val_len]);
    rv.set(@ptrCast(v8.StringApi.fromUtf8(isolate, if (ok) "true" else "false") orelse return));
}

/// V8 callback: __edgebox_chan_recv(channelIdStr) → jsonStr or "null"
pub fn chanRecvCallback(info: *const v8.FunctionCallbackInfo) callconv(.c) void {
    const isolate: *v8.Isolate = v8.CallbackInfoApi.getIsolate(info);
    var rv = v8.CallbackInfoApi.getReturnValue(info);

    if (v8.CallbackInfoApi.length(info) < 1) {
        rv.setNull();
        return;
    }
    const arg0 = v8.CallbackInfoApi.get(info, 0) orelse { rv.setNull(); return; };
    if (!v8.ValueApi.isString(arg0)) { rv.setNull(); return; }
    const id_s: *const v8.String = @ptrCast(arg0);
    const id_len: usize = @intCast(v8.StringApi.utf8Length(id_s, isolate));
    var id_buf: [16]u8 = undefined;
    _ = v8.StringApi.writeUtf8(id_s, isolate, &id_buf);
    const id = std.fmt.parseInt(usize, id_buf[0..id_len], 10) catch { rv.setNull(); return; };
    if (id >= channel_count or !channels[id].active) { rv.setNull(); return; }

    if (channels[id].recv()) |data| {
        defer alloc.free(data);
        rv.set(@ptrCast(v8.StringApi.fromUtf8(isolate, data) orelse return));
    } else {
        rv.setNull();
    }
}

/// V8 callback: __edgebox_chan_close(channelIdStr)
pub fn chanCloseCallback(info: *const v8.FunctionCallbackInfo) callconv(.c) void {
    const isolate: *v8.Isolate = v8.CallbackInfoApi.getIsolate(info);
    if (v8.CallbackInfoApi.length(info) < 1) return;
    const arg0 = v8.CallbackInfoApi.get(info, 0) orelse return;
    if (!v8.ValueApi.isString(arg0)) return;
    const id_s: *const v8.String = @ptrCast(arg0);
    const id_len: usize = @intCast(v8.StringApi.utf8Length(id_s, isolate));
    var id_buf: [16]u8 = undefined;
    _ = v8.StringApi.writeUtf8(id_s, isolate, &id_buf);
    const id = std.fmt.parseInt(usize, id_buf[0..id_len], 10) catch return;
    if (id >= channel_count or !channels[id].active) return;
    channels[id].close();
}

/// V8 callback: __edgebox_chan_shared(capacityStr) → SharedArrayBuffer
/// Creates a zero-copy channel backed by Zig-managed shared memory.
/// Both the caller and any worker isolate can create views into the same buffer.
pub fn chanSharedCallback(info: *const v8.FunctionCallbackInfo) callconv(.c) void {
    const isolate: *v8.Isolate = v8.CallbackInfoApi.getIsolate(info);
    var rv = v8.CallbackInfoApi.getReturnValue(info);

    var capacity: usize = 1024;
    if (v8.CallbackInfoApi.length(info) >= 1) {
        if (v8.CallbackInfoApi.get(info, 0)) |arg| {
            if (v8.ValueApi.isString(arg)) {
                const str: *const v8.String = @ptrCast(arg);
                const len: usize = @intCast(v8.StringApi.utf8Length(str, isolate));
                if (len > 0 and len < 32) {
                    var buf: [32]u8 = undefined;
                    _ = v8.StringApi.writeUtf8(str, isolate, &buf);
                    capacity = std.fmt.parseInt(usize, buf[0..len], 10) catch 1024;
                }
            }
        }
    }

    // Allocate shared buffer
    registry_mutex.lock();
    const id = channel_count;
    if (id >= max_channels) {
        registry_mutex.unlock();
        rv.setNull();
        return;
    }
    const buf_ptr = allocSharedBuffer(capacity) orelse {
        registry_mutex.unlock();
        rv.setNull();
        return;
    };
    const total_size = HEADER_BYTES + capacity * 8;
    shared_buffers[id] = buf_ptr;
    shared_buffer_sizes[id] = total_size;

    // Also create a regular channel for the mutex-based fallback
    channels[id] = Channel.init(1) catch {
        registry_mutex.unlock();
        rv.setNull();
        return;
    };
    channel_count += 1;
    registry_mutex.unlock();

    // Create SharedArrayBuffer backed by this memory
    const sab = v8.SharedArrayBufferApi.fromExternalMemory(isolate, buf_ptr, total_size) orelse {
        rv.setNull();
        return;
    };
    rv.set(sab);
}

/// V8 callback: __edgebox_chan_get_shared(channelIdStr) → SharedArrayBuffer
/// Returns the SharedArrayBuffer for an existing channel (for worker isolates).
pub fn chanGetSharedCallback(info: *const v8.FunctionCallbackInfo) callconv(.c) void {
    const isolate: *v8.Isolate = v8.CallbackInfoApi.getIsolate(info);
    var rv = v8.CallbackInfoApi.getReturnValue(info);

    if (v8.CallbackInfoApi.length(info) < 1) { rv.setNull(); return; }
    const arg0 = v8.CallbackInfoApi.get(info, 0) orelse { rv.setNull(); return; };
    if (!v8.ValueApi.isString(arg0)) { rv.setNull(); return; }
    const id_s: *const v8.String = @ptrCast(arg0);
    const id_len: usize = @intCast(v8.StringApi.utf8Length(id_s, isolate));
    var id_buf: [16]u8 = undefined;
    _ = v8.StringApi.writeUtf8(id_s, isolate, &id_buf);
    const id = std.fmt.parseInt(usize, id_buf[0..id_len], 10) catch { rv.setNull(); return; };

    if (id >= channel_count) { rv.setNull(); return; }
    const buf_ptr = shared_buffers[id] orelse { rv.setNull(); return; };
    const total_size = shared_buffer_sizes[id];

    const sab = v8.SharedArrayBufferApi.fromExternalMemory(isolate, buf_ptr, total_size) orelse {
        rv.setNull();
        return;
    };
    rv.set(sab);
}

/// Register channel callbacks as V8 globals
pub fn registerGlobals(isolate: *v8.Isolate, context: *const v8.Context) void {
    const global = v8.ContextApi.global(context);
    registerOne(isolate, context, global, "__edgebox_chan_create", &chanCreateCallback);
    registerOne(isolate, context, global, "__edgebox_chan_send", &chanSendCallback);
    registerOne(isolate, context, global, "__edgebox_chan_recv", &chanRecvCallback);
    registerOne(isolate, context, global, "__edgebox_chan_close", &chanCloseCallback);
    registerOne(isolate, context, global, "__edgebox_chan_shared", &chanSharedCallback);
    registerOne(isolate, context, global, "__edgebox_chan_get_shared", &chanGetSharedCallback);
}

fn registerOne(isolate: *v8.Isolate, context: *const v8.Context, global: *const v8.Object, name: []const u8, cb: *const fn (*const v8.FunctionCallbackInfo) callconv(.c) void) void {
    const tmpl = v8.FunctionTemplateApi.create(isolate, cb) orelse return;
    const func = v8.FunctionTemplateApi.getFunction(tmpl, context) orelse return;
    const key = v8.StringApi.fromUtf8(isolate, name) orelse return;
    _ = v8.ObjectApi.set(global, context, @ptrCast(key), @ptrCast(func));
}
