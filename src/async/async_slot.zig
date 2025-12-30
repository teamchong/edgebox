//! Async Slot Table - Reusable async operation tracking
//!
//! Provides a generic slot table pattern for managing async operations with:
//! - Atomic state tracking (pending/running/complete/failed)
//! - Thread-safe slot allocation
//! - ID-based access
//!
//! Consolidated from duplicate implementations in:
//! - component/impls/process_impl.zig
//! - component/impls/http_impl.zig

const std = @import("std");

/// Async operation state (u8 backing for atomic compatibility)
pub const AsyncState = enum(u8) {
    pending = 0, // Created, not started
    running = 1, // Executing in background
    complete = 2, // Finished successfully
    failed = 3, // Finished with error

    /// Check if the operation is done (complete or failed)
    pub fn isDone(self: AsyncState) bool {
        return self == .complete or self == .failed;
    }
};

/// Generic async slot table for managing async operations
/// T is the operation struct type that must have:
/// - state: std.atomic.Value(AsyncState)
/// - deinit(self: *T) method
pub fn AsyncSlotTable(comptime T: type, comptime max_slots: usize) type {
    return struct {
        const Self = @This();

        slots: [max_slots]?*T = [_]?*T{null} ** max_slots,
        next_id: u32 = 0,
        mutex: std.Thread.Mutex = .{},

        /// Allocate a new slot for an operation
        /// Returns the slot ID and a pointer to the operation, or null if full
        pub fn allocate(self: *Self, allocator: std.mem.Allocator) ?struct { id: u32, op: *T } {
            self.mutex.lock();
            defer self.mutex.unlock();

            // Find empty slot
            const start = self.next_id % max_slots;
            var i: usize = 0;
            while (i < max_slots) : (i += 1) {
                const idx = (start + i) % max_slots;
                if (self.slots[idx] == null) {
                    const op = allocator.create(T) catch return null;
                    op.* = T{}; // Zero-initialize
                    self.slots[idx] = op;
                    const id = self.next_id;
                    self.next_id +%= 1;
                    return .{ .id = id, .op = op };
                }
            }
            return null; // All slots full
        }

        /// Get an operation by ID
        pub fn get(self: *Self, id: u32) ?*T {
            self.mutex.lock();
            defer self.mutex.unlock();
            const idx = id % max_slots;
            return self.slots[idx];
        }

        /// Free a slot by ID
        pub fn free(self: *Self, id: u32, allocator: std.mem.Allocator) void {
            self.mutex.lock();
            defer self.mutex.unlock();
            const idx = id % max_slots;
            if (self.slots[idx]) |op| {
                if (@hasDecl(T, "deinit")) {
                    op.deinit();
                }
                allocator.destroy(op);
                self.slots[idx] = null;
            }
        }

        /// Check if an operation is complete
        pub fn isComplete(self: *Self, id: u32) bool {
            self.mutex.lock();
            defer self.mutex.unlock();
            const idx = id % max_slots;
            if (self.slots[idx]) |op| {
                if (@hasField(T, "state")) {
                    const state = op.state.load(.acquire);
                    return state.isDone();
                }
            }
            return false;
        }

        /// Clean up all slots
        pub fn deinitAll(self: *Self, allocator: std.mem.Allocator) void {
            self.mutex.lock();
            defer self.mutex.unlock();
            for (&self.slots) |*slot| {
                if (slot.*) |op| {
                    if (@hasDecl(T, "deinit")) {
                        op.deinit();
                    }
                    allocator.destroy(op);
                    slot.* = null;
                }
            }
        }
    };
}

/// Mixin for async operations that adds common state management
/// Use this as a base for your async operation struct
pub fn AsyncOpMixin(comptime Self: type) type {
    return struct {
        /// Check if the operation is complete (either successfully or with error)
        pub fn isComplete(self: *const Self) bool {
            const state = self.state.load(.acquire);
            return state.isDone();
        }

        /// Check if the operation completed successfully
        pub fn isSuccess(self: *const Self) bool {
            return self.state.load(.acquire) == .complete;
        }

        /// Check if the operation failed
        pub fn isFailed(self: *const Self) bool {
            return self.state.load(.acquire) == .failed;
        }

        /// Set state to running
        pub fn setRunning(self: *Self) void {
            self.state.store(.running, .release);
        }

        /// Set state to complete
        pub fn setComplete(self: *Self) void {
            self.state.store(.complete, .release);
        }

        /// Set state to failed
        pub fn setFailed(self: *Self) void {
            self.state.store(.failed, .release);
        }
    };
}

test "AsyncSlotTable basic" {
    const TestOp = struct {
        state: std.atomic.Value(AsyncState) = std.atomic.Value(AsyncState).init(.pending),
        value: i32 = 0,

        pub fn deinit(_: *@This()) void {}
    };

    var table = AsyncSlotTable(TestOp, 4){};
    const allocator = std.testing.allocator;

    // Allocate
    const result = table.allocate(allocator);
    try std.testing.expect(result != null);
    const r = result.?;
    r.op.value = 42;
    r.op.state.store(.complete, .release);

    // Get
    const got = table.get(r.id);
    try std.testing.expect(got != null);
    try std.testing.expectEqual(@as(i32, 42), got.?.value);

    // isComplete
    try std.testing.expect(table.isComplete(r.id));

    // Free
    table.free(r.id, allocator);
    try std.testing.expect(table.get(r.id) == null);
}
