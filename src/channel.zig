// Thread-safe channel for OS threads (no goroutine runtime dependency).
// Used for V8 worker coordination: worker 0 signals → workers 1-N receive.
//
// Usage:
//   var ch = Channel(u32).init();
//   ch.send(42);           // from any thread
//   const val = ch.recv(); // blocks until value available, null if closed

const std = @import("std");

pub fn Channel(comptime T: type) type {
    return struct {
        const Self = @This();
        const CAPACITY = 256;

        buffer: [CAPACITY]T = undefined,
        head: usize = 0,
        tail: usize = 0,
        count: usize = 0,
        closed: bool = false,
        mutex: std.Thread.Mutex = .{},
        not_empty: std.Thread.Condition = .{},
        not_full: std.Thread.Condition = .{},

        pub fn init() Self {
            return .{};
        }

        pub fn send(self: *Self, value: T) bool {
            self.mutex.lock();
            defer self.mutex.unlock();
            while (self.count >= CAPACITY and !self.closed) {
                self.not_full.wait(&self.mutex);
            }
            if (self.closed) return false;
            self.buffer[self.tail] = value;
            self.tail = (self.tail + 1) % CAPACITY;
            self.count += 1;
            self.not_empty.signal();
            return true;
        }

        pub fn recv(self: *Self) ?T {
            self.mutex.lock();
            defer self.mutex.unlock();
            while (self.count == 0 and !self.closed) {
                self.not_empty.wait(&self.mutex);
            }
            if (self.count == 0) return null; // closed + empty
            const value = self.buffer[self.head];
            self.head = (self.head + 1) % CAPACITY;
            self.count -= 1;
            self.not_full.signal();
            return value;
        }

        pub fn close(self: *Self) void {
            self.mutex.lock();
            defer self.mutex.unlock();
            self.closed = true;
            self.not_empty.broadcast();
            self.not_full.broadcast();
        }

        pub fn reset(self: *Self) void {
            self.mutex.lock();
            defer self.mutex.unlock();
            self.head = 0;
            self.tail = 0;
            self.count = 0;
            self.closed = false;
        }
    };
}
