/// WASM Arena Allocator - Pure Bump Pointer
/// O(1) malloc, O(1) free (no-op), O(1) bulk reset
///
/// Perfect for EdgeBox where:
/// - Request-scoped execution (50ms - 30s)
/// - Memory freed at request end (arena reset)
/// - No fragmentation, no freelist overhead
const std = @import("std");

pub const WasmArenaAllocator = struct {
    const Self = @This();

    /// Bump allocator state
    heap_start: [*]u8,
    bump_ptr: [*]u8,
    heap_end: [*]u8,

    /// Metrics
    alloc_count: usize,
    total_allocated: usize,

    /// Initialize the allocator using WASM __heap_base
    pub fn init() Self {
        const heap_start = getHeapBase();
        const memory_size = getMemorySize();

        return .{
            .heap_start = heap_start,
            .bump_ptr = heap_start,
            .heap_end = @ptrFromInt(memory_size),
            .alloc_count = 0,
            .total_allocated = 0,
        };
    }

    /// Get WASM heap base (platform-specific)
    fn getHeapBase() [*]u8 {
        if (@import("builtin").target.cpu.arch == .wasm32) {
            return @extern([*]u8, .{ .name = "__heap_base" });
        } else {
            const static = struct {
                var fake_heap: [64 * 1024 * 1024]u8 = undefined;
            };
            return &static.fake_heap;
        }
    }

    /// Get current WASM memory size in bytes
    fn getMemorySize() usize {
        if (@import("builtin").target.cpu.arch == .wasm32) {
            return @wasmMemorySize(0) * 65536;
        } else {
            return 64 * 1024 * 1024;
        }
    }

    /// Grow WASM memory by specified pages
    fn growMemory(pages: usize) bool {
        if (@import("builtin").target.cpu.arch == .wasm32) {
            return @wasmMemoryGrow(0, pages) != -1;
        } else {
            return false;
        }
    }

    /// O(1) allocation - just bump the pointer
    pub fn alloc(self: *Self, size: usize) ?[*]u8 {
        if (size == 0) return null;

        self.alloc_count += 1;

        // 8-byte alignment
        const aligned = std.mem.alignForward(usize, size, 8);
        const new_bump = @intFromPtr(self.bump_ptr) + aligned;

        // Check if we need more memory
        if (new_bump > @intFromPtr(self.heap_end)) {
            const pages_needed = @max(16, (aligned / 65536) + 1);
            if (!growMemory(pages_needed)) {
                return null;
            }
            self.heap_end = @ptrFromInt(getMemorySize());

            if (new_bump > @intFromPtr(self.heap_end)) {
                return null;
            }
        }

        const result = self.bump_ptr;
        self.bump_ptr = @ptrFromInt(new_bump);
        self.total_allocated += aligned;

        return result;
    }

    /// O(1) free - NO-OP! Memory freed at arena reset
    pub fn free(self: *Self, ptr: ?*anyopaque) void {
        _ = self;
        _ = ptr;
        // Intentionally empty - arena doesn't free individual allocations
    }

    /// Realloc - allocate new, copy old data
    pub fn realloc(self: *Self, ptr: ?*anyopaque, old_size: usize, new_size: usize) ?[*]u8 {
        if (ptr == null) {
            return self.alloc(new_size);
        }

        if (new_size == 0) {
            return null;
        }

        // Always allocate new (arena can't shrink)
        const new_ptr = self.alloc(new_size) orelse return null;
        const old_ptr: [*]u8 = @ptrCast(ptr.?);
        const copy_size = @min(old_size, new_size);
        @memcpy(new_ptr[0..copy_size], old_ptr[0..copy_size]);

        return new_ptr;
    }

    /// O(1) bulk reset - just reset the bump pointer
    pub fn reset(self: *Self) void {
        self.bump_ptr = self.heap_start;
        self.alloc_count = 0;
        self.total_allocated = 0;
    }

    /// Get current memory usage
    pub fn usedBytes(self: *const Self) usize {
        return @intFromPtr(self.bump_ptr) - @intFromPtr(self.heap_start);
    }

    pub const Stats = struct {
        alloc_count: usize,
        total_allocated: usize,
        used_bytes: usize,
    };

    pub fn getStats(self: *const Self) Stats {
        return .{
            .alloc_count = self.alloc_count,
            .total_allocated = self.total_allocated,
            .used_bytes = self.usedBytes(),
        };
    }
};

// ============================================================================
// QuickJS Integration
// ============================================================================

var global_arena: WasmArenaAllocator = undefined;
var arena_initialized: bool = false;

/// Debug: track if we're in the middle of initialization
var initializing: bool = false;

/// Track allocation sizes for realloc (QuickJS needs this)
/// Simple approach: store size in first 8 bytes of each allocation
const HEADER_SIZE: usize = 8;

pub fn initGlobalPool() void {
    if (!arena_initialized) {
        global_arena = WasmArenaAllocator.init();
        arena_initialized = true;
    }
}

pub fn resetGlobalPool() void {
    if (arena_initialized) {
        global_arena.reset();
    }
}

pub fn getGlobalPoolStats() ?WasmArenaAllocator.Stats {
    if (arena_initialized) {
        return global_arena.getStats();
    }
    return null;
}

/// QuickJS calloc callback
pub fn qjs_calloc(ctx: ?*anyopaque, count: usize, size: usize) callconv(.c) ?*anyopaque {
    _ = ctx;
    initGlobalPool();
    const total = count * size;
    const ptr = global_arena.alloc(total + HEADER_SIZE) orelse return null;

    // Store size in header
    const size_ptr: *usize = @ptrCast(@alignCast(ptr));
    size_ptr.* = total;

    // Zero the user memory
    const user_ptr = ptr + HEADER_SIZE;
    @memset(user_ptr[0..total], 0);

    return user_ptr;
}

/// QuickJS malloc callback
pub fn qjs_malloc(ctx: ?*anyopaque, size: usize) callconv(.c) ?*anyopaque {
    _ = ctx;
    initGlobalPool();
    const ptr = global_arena.alloc(size + HEADER_SIZE) orelse return null;

    // Store size in header
    const size_ptr: *usize = @ptrCast(@alignCast(ptr));
    size_ptr.* = size;

    return ptr + HEADER_SIZE;
}

/// QuickJS free callback - NO-OP for arena
pub fn qjs_free(ctx: ?*anyopaque, ptr: ?*anyopaque) callconv(.c) void {
    _ = ctx;
    _ = ptr;
    // Arena doesn't free - memory reclaimed at reset
}

/// QuickJS realloc callback
pub fn qjs_realloc(ctx: ?*anyopaque, ptr: ?*anyopaque, new_size: usize) callconv(.c) ?*anyopaque {
    _ = ctx;
    initGlobalPool();

    if (ptr == null) {
        return qjs_malloc(null, new_size);
    }

    if (new_size == 0) {
        return null;
    }

    const old_ptr: [*]u8 = @ptrCast(ptr.?);
    const header = old_ptr - HEADER_SIZE;
    const old_size_ptr: *const usize = @ptrCast(@alignCast(header));
    const old_size = old_size_ptr.*;

    // Allocate new with header
    const new_ptr = global_arena.alloc(new_size + HEADER_SIZE) orelse return null;

    // Store new size
    const new_size_ptr: *usize = @ptrCast(@alignCast(new_ptr));
    new_size_ptr.* = new_size;

    // Copy old data
    const user_ptr = new_ptr + HEADER_SIZE;
    const copy_size = @min(old_size, new_size);
    @memcpy(user_ptr[0..copy_size], old_ptr[0..copy_size]);

    return user_ptr;
}

/// QuickJS malloc_usable_size callback
pub fn qjs_malloc_usable_size(ptr: ?*const anyopaque) callconv(.c) usize {
    if (ptr == null) return 0;
    if (!arena_initialized) return 0;

    const p: [*]const u8 = @ptrCast(ptr.?);
    const p_addr = @intFromPtr(p);

    // Safety: verify pointer is within our heap (with header)
    const heap_start = @intFromPtr(global_arena.heap_start);
    const heap_end = @intFromPtr(global_arena.bump_ptr);

    // Pointer must be after heap_start + HEADER_SIZE and before heap_end
    if (p_addr < heap_start + HEADER_SIZE or p_addr > heap_end) {
        return 0; // Not our allocation - return 0 (QuickJS will handle it)
    }

    const header = p - HEADER_SIZE;

    // Make sure header is within valid range too
    if (@intFromPtr(header) < heap_start) {
        return 0;
    }

    const size_ptr: *const usize = @ptrCast(@alignCast(header));
    return size_ptr.*;
}

// Re-export for compatibility
pub const WasmPoolAllocator = WasmArenaAllocator;
