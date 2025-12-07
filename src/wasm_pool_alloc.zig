/// WASM Pool Allocator with Size-Class Freelists
/// O(1) malloc, O(1) free, O(1) bulk reset
///
/// Designed for QuickJS in WASM where:
/// - Memory only grows (no OS-level free)
/// - Request-scoped execution (reset at request end)
/// - Size headers to recover size in free() calls
const std = @import("std");

pub const WasmPoolAllocator = struct {
    const Self = @This();

    /// Number of size classes: 32, 64, 128, 256, 512, 1024, 2048, 4096
    const NUM_CLASSES = 8;
    const MIN_SIZE = 32;
    const MAX_POOLED = 4096;
    const HEADER_SIZE = 8; // Store allocation size before each block

    /// Freelist node - reuses the freed memory itself
    const FreeNode = struct {
        next: ?*FreeNode,
    };

    /// Freelist head for each size class
    freelists: [NUM_CLASSES]?*FreeNode,

    /// Bump allocator for large/fresh allocations
    heap_start: [*]u8,
    bump_ptr: [*]u8,
    heap_end: [*]u8,

    /// Metrics
    alloc_count: usize,
    reuse_count: usize,
    total_allocated: usize,

    /// Initialize the allocator using WASM __heap_base
    pub fn init() Self {
        // Get heap base from WASM linker symbol
        const heap_start = getHeapBase();
        const memory_size = getMemorySize();

        return .{
            .freelists = [_]?*FreeNode{null} ** NUM_CLASSES,
            .heap_start = heap_start,
            .bump_ptr = heap_start,
            .heap_end = @ptrFromInt(memory_size),
            .alloc_count = 0,
            .reuse_count = 0,
            .total_allocated = 0,
        };
    }

    /// Get WASM heap base (platform-specific)
    fn getHeapBase() [*]u8 {
        if (@import("builtin").target.cpu.arch == .wasm32) {
            // In WASM, __heap_base is a linker-provided symbol
            return @extern([*]u8, .{ .name = "__heap_base" });
        } else {
            // For native testing, allocate a fake heap
            const static = struct {
                var fake_heap: [64 * 1024 * 1024]u8 = undefined; // 64MB
            };
            return &static.fake_heap;
        }
    }

    /// Get current WASM memory size in bytes
    fn getMemorySize() usize {
        if (@import("builtin").target.cpu.arch == .wasm32) {
            // memory.size returns pages (64KB each)
            return @wasmMemorySize(0) * 65536;
        } else {
            // For native testing
            return 64 * 1024 * 1024; // 64MB
        }
    }

    /// Grow WASM memory by specified pages
    fn growMemory(pages: usize) bool {
        if (@import("builtin").target.cpu.arch == .wasm32) {
            return @wasmMemoryGrow(0, pages) != -1;
        } else {
            // Native: can't grow fake heap
            return false;
        }
    }

    /// Map allocation size to size class index
    /// Returns null for sizes > MAX_POOLED
    fn sizeToClass(size: usize) ?usize {
        if (size > MAX_POOLED) return null;

        var class: usize = 0;
        var threshold: usize = MIN_SIZE;
        while (threshold < size and class < NUM_CLASSES - 1) {
            threshold *= 2;
            class += 1;
        }
        return class;
    }

    /// Map size class index to actual allocation size
    fn classToSize(class: usize) usize {
        const shift: u5 = @intCast(class);
        const min: usize = MIN_SIZE;
        return min << shift;
    }

    /// O(1) allocation - try freelist first, then bump
    /// Stores size in header for free() to use
    pub fn alloc(self: *Self, requested_size: usize) ?[*]u8 {
        if (requested_size == 0) return null;

        self.alloc_count += 1;

        // Total size including header
        const total_size = requested_size + HEADER_SIZE;

        // Try freelist for pooled sizes
        if (sizeToClass(total_size)) |class| {
            if (self.freelists[class]) |node| {
                // Pop from freelist
                self.freelists[class] = node.next;
                self.reuse_count += 1;

                // Header already contains size from previous alloc
                const ptr: [*]u8 = @ptrCast(node);
                return ptr + HEADER_SIZE;
            }

            // Freelist empty - bump allocate with class size
            const class_size = classToSize(class);
            return self.bumpAlloc(class_size, requested_size);
        }

        // Large allocation - bump only (no pooling)
        return self.bumpAlloc(total_size, requested_size);
    }

    /// Bump allocate with size stored in header
    fn bumpAlloc(self: *Self, alloc_size: usize, user_size: usize) ?[*]u8 {
        const aligned = std.mem.alignForward(usize, alloc_size, 8);
        const new_bump = @intFromPtr(self.bump_ptr) + aligned;

        // Check if we need more memory
        if (new_bump > @intFromPtr(self.heap_end)) {
            // Grow by at least 16 pages (1MB) or enough for this allocation
            const pages_needed = @max(16, (aligned / 65536) + 1);
            if (!growMemory(pages_needed)) {
                return null; // OOM
            }
            self.heap_end = @ptrFromInt(getMemorySize());

            // Check again after grow
            if (new_bump > @intFromPtr(self.heap_end)) {
                return null; // Still OOM
            }
        }

        const result = self.bump_ptr;
        self.bump_ptr = @ptrFromInt(new_bump);
        self.total_allocated += aligned;

        // Store size in header (first 8 bytes)
        const size_ptr: *usize = @ptrCast(@alignCast(result));
        size_ptr.* = user_size;

        // Return pointer after header
        return result + HEADER_SIZE;
    }

    /// O(1) free - read size from header, push to freelist
    pub fn free(self: *Self, ptr: ?*anyopaque) void {
        const p: [*]u8 = @ptrCast(ptr orelse return);

        // Read size from header (8 bytes before ptr)
        const header = p - HEADER_SIZE;
        const size_ptr: *const usize = @ptrCast(@alignCast(header));
        const user_size = size_ptr.*;
        const total_size = user_size + HEADER_SIZE;

        // Add to freelist if poolable size
        if (sizeToClass(total_size)) |class| {
            const node: *FreeNode = @ptrCast(@alignCast(header));
            node.next = self.freelists[class];
            self.freelists[class] = node;
        }
        // Large allocations: leak until reset (can't efficiently reuse)
    }

    /// Realloc - allocate new, copy, free old
    pub fn realloc(self: *Self, ptr: ?*anyopaque, new_size: usize) ?[*]u8 {
        if (ptr == null) {
            return self.alloc(new_size);
        }

        if (new_size == 0) {
            self.free(ptr);
            return null;
        }

        const old_ptr: [*]u8 = @ptrCast(ptr.?);

        // Read old size from header
        const header = old_ptr - HEADER_SIZE;
        const size_ptr: *const usize = @ptrCast(@alignCast(header));
        const old_size = size_ptr.*;

        // If new size fits in same block, just update header
        const old_total = old_size + HEADER_SIZE;
        const new_total = new_size + HEADER_SIZE;
        if (sizeToClass(old_total)) |old_class| {
            if (sizeToClass(new_total)) |new_class| {
                if (old_class == new_class) {
                    // Same size class - update header and return same ptr
                    @as(*usize, @ptrCast(@alignCast(header))).* = new_size;
                    return old_ptr;
                }
            }
        }

        // Different size - allocate new, copy, free old
        const new_ptr = self.alloc(new_size) orelse return null;
        const copy_size = @min(old_size, new_size);
        @memcpy(new_ptr[0..copy_size], old_ptr[0..copy_size]);
        self.free(ptr);

        return new_ptr;
    }

    /// O(1) bulk reset - clear freelists, reset bump pointer
    /// Call at request end to reclaim all memory
    pub fn reset(self: *Self) void {
        for (&self.freelists) |*fl| {
            fl.* = null;
        }
        self.bump_ptr = self.heap_start;
        self.alloc_count = 0;
        self.reuse_count = 0;
        self.total_allocated = 0;
    }

    /// Get current memory usage
    pub fn usedBytes(self: *const Self) usize {
        return @intFromPtr(self.bump_ptr) - @intFromPtr(self.heap_start);
    }

    /// Get allocation statistics
    pub fn getStats(self: *const Self) Stats {
        return .{
            .alloc_count = self.alloc_count,
            .reuse_count = self.reuse_count,
            .total_allocated = self.total_allocated,
            .used_bytes = self.usedBytes(),
            .reuse_rate = if (self.alloc_count > 0)
                @as(f32, @floatFromInt(self.reuse_count)) / @as(f32, @floatFromInt(self.alloc_count))
            else
                0.0,
        };
    }

    pub const Stats = struct {
        alloc_count: usize,
        reuse_count: usize,
        total_allocated: usize,
        used_bytes: usize,
        reuse_rate: f32,
    };
};

// ============================================================================
// QuickJS Integration
// ============================================================================

/// Global pool allocator instance (WASM is single-threaded)
var global_pool: WasmPoolAllocator = undefined;
var pool_initialized: bool = false;

/// Initialize the global pool allocator
pub fn initGlobalPool() void {
    if (!pool_initialized) {
        global_pool = WasmPoolAllocator.init();
        pool_initialized = true;
    }
}

/// Reset the global pool (call at request end)
pub fn resetGlobalPool() void {
    if (pool_initialized) {
        global_pool.reset();
    }
}

/// Get pool statistics
pub fn getGlobalPoolStats() ?WasmPoolAllocator.Stats {
    if (pool_initialized) {
        return global_pool.getStats();
    }
    return null;
}

/// QuickJS calloc callback (allocate and zero)
pub fn qjs_calloc(ctx: ?*anyopaque, count: usize, size: usize) callconv(.c) ?*anyopaque {
    _ = ctx;
    initGlobalPool();
    const total = count * size;
    const ptr = global_pool.alloc(total) orelse return null;
    @memset(ptr[0..total], 0);
    return ptr;
}

/// QuickJS malloc callback
pub fn qjs_malloc(ctx: ?*anyopaque, size: usize) callconv(.c) ?*anyopaque {
    _ = ctx;
    initGlobalPool();
    return global_pool.alloc(size);
}

/// QuickJS free callback
pub fn qjs_free(ctx: ?*anyopaque, ptr: ?*anyopaque) callconv(.c) void {
    _ = ctx;
    if (pool_initialized) {
        global_pool.free(ptr);
    }
}

/// QuickJS realloc callback
pub fn qjs_realloc(ctx: ?*anyopaque, ptr: ?*anyopaque, size: usize) callconv(.c) ?*anyopaque {
    _ = ctx;
    initGlobalPool();
    return global_pool.realloc(ptr, size);
}

/// QuickJS malloc_usable_size callback
/// Returns the usable size of an allocation (needed for realloc optimization)
pub fn qjs_malloc_usable_size(ptr: ?*const anyopaque) callconv(.c) usize {
    if (ptr == null) return 0;
    if (!pool_initialized) return 0;

    const p: [*]const u8 = @ptrCast(ptr.?);

    // Safety check: verify pointer is within our heap
    const p_addr = @intFromPtr(p);
    const heap_start = @intFromPtr(global_pool.heap_start);
    const heap_end = @intFromPtr(global_pool.bump_ptr);
    if (p_addr < heap_start + WasmPoolAllocator.HEADER_SIZE or p_addr >= heap_end) {
        return 0; // Not our allocation
    }

    const header = p - WasmPoolAllocator.HEADER_SIZE;
    const size_ptr: *const usize = @ptrCast(@alignCast(header));
    return size_ptr.*;
}

// ============================================================================
// Tests
// ============================================================================

test "basic allocation" {
    var alloc = WasmPoolAllocator.init();

    // Allocate various sizes
    const p1 = alloc.alloc(16) orelse return error.AllocFailed;
    const p2 = alloc.alloc(100) orelse return error.AllocFailed;
    const p3 = alloc.alloc(1000) orelse return error.AllocFailed;

    // Write to allocations
    @memset(p1[0..16], 0xAA);
    @memset(p2[0..100], 0xBB);
    @memset(p3[0..1000], 0xCC);

    // Verify no overlap
    try std.testing.expect(p1[0] == 0xAA);
    try std.testing.expect(p2[0] == 0xBB);
    try std.testing.expect(p3[0] == 0xCC);

    // Free and reallocate - should reuse
    alloc.free(p1);
    const p4 = alloc.alloc(16) orelse return error.AllocFailed;

    // p4 should reuse p1's memory (same size class)
    try std.testing.expect(@intFromPtr(p4) == @intFromPtr(p1));

    const stats = alloc.getStats();
    try std.testing.expect(stats.reuse_count >= 1);
}

test "realloc" {
    var alloc = WasmPoolAllocator.init();

    // Allocate and write data
    const p1 = alloc.alloc(32) orelse return error.AllocFailed;
    @memcpy(p1[0..5], "hello");

    // Realloc to larger size
    const p2 = alloc.realloc(p1, 64) orelse return error.AllocFailed;

    // Data should be preserved
    try std.testing.expectEqualStrings("hello", p2[0..5]);
}

test "reset" {
    var alloc = WasmPoolAllocator.init();

    // Allocate a bunch
    _ = alloc.alloc(100);
    _ = alloc.alloc(200);
    _ = alloc.alloc(300);

    const before = alloc.usedBytes();
    try std.testing.expect(before > 0);

    // Reset
    alloc.reset();

    const after = alloc.usedBytes();
    try std.testing.expect(after == 0);
}

test "size classes" {
    // Test size class mapping
    try std.testing.expect(WasmPoolAllocator.sizeToClass(1) == 0); // ≤32
    try std.testing.expect(WasmPoolAllocator.sizeToClass(32) == 0);
    try std.testing.expect(WasmPoolAllocator.sizeToClass(33) == 1); // ≤64
    try std.testing.expect(WasmPoolAllocator.sizeToClass(64) == 1);
    try std.testing.expect(WasmPoolAllocator.sizeToClass(65) == 2); // ≤128
    try std.testing.expect(WasmPoolAllocator.sizeToClass(4096) == 7);
    try std.testing.expect(WasmPoolAllocator.sizeToClass(4097) == null); // Too large
}
