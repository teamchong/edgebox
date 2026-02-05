/// Native Arena Allocator for QuickJS
///
/// A bump allocator optimized for TSC-like workloads:
/// - O(1) malloc: bump pointer with size header
/// - O(1) realloc: in-place growth if at top of arena
/// - O(1) free: reclaims space if at top (LIFO pattern)
/// - Reset: instant cleanup after compilation
///
/// For TSC compilation, this avoids millions of malloc/free calls
/// by using fast pointer bumping and bulk reset at the end.
const std = @import("std");
const builtin = @import("builtin");

const ALIGNMENT: usize = 16; // 16-byte alignment for SIMD compatibility
const HEADER_SIZE: usize = @sizeOf(Header);

// Header for size tracking (needed for realloc and LIFO free)
const Header = extern struct {
    size: usize,
    _pad: usize = 0, // Ensure 16-byte alignment
};

// Arena block for chained allocation
const Block = struct {
    data: []align(16) u8,
    next: ?*Block,
};

// Global state
var heap_base: [*]align(16) u8 = undefined;
var heap_ptr: usize = 0;
var heap_end: usize = 0;
var heap_mark: usize = 0;
var initialized: bool = false;

// Block chain for large arenas
var first_block: ?*Block = null;
var current_block: ?*Block = null;

// Initial arena size (64MB - good for TSC)
const INITIAL_SIZE: usize = 64 * 1024 * 1024;
// Growth size when arena is full
const GROW_SIZE: usize = 32 * 1024 * 1024;

// Stats for debugging
var stats_malloc_count: usize = 0;
var stats_realloc_inplace: usize = 0;
var stats_realloc_copy: usize = 0;
var stats_free_reclaimed: usize = 0;
var stats_bytes_allocated: usize = 0;
var stats_grow_count: usize = 0;

pub fn init() void {
    if (initialized) return;

    // Allocate initial arena using page allocator
    const data = std.heap.page_allocator.alignedAlloc(u8, .@"16", INITIAL_SIZE) catch {
        @panic("Failed to allocate native arena");
    };

    heap_base = data.ptr;
    heap_ptr = 0;
    heap_end = INITIAL_SIZE;
    heap_mark = 0;
    initialized = true;

    // Track block for cleanup
    const block = std.heap.page_allocator.create(Block) catch {
        @panic("Failed to allocate arena block");
    };
    block.* = .{
        .data = data,
        .next = null,
    };
    first_block = block;
    current_block = block;
}

/// Save current position as mark
pub fn mark() void {
    heap_mark = heap_ptr;
}

/// Reset arena to mark position (instant cleanup)
pub fn reset() void {
    heap_ptr = heap_mark;
    stats_malloc_count = 0;
    stats_realloc_inplace = 0;
    stats_realloc_copy = 0;
    stats_free_reclaimed = 0;
}

/// Reset arena to base (full cleanup)
pub fn resetAll() void {
    heap_ptr = 0;
    heap_mark = 0;
    stats_malloc_count = 0;
    stats_realloc_inplace = 0;
    stats_realloc_copy = 0;
    stats_free_reclaimed = 0;
    stats_bytes_allocated = 0;
}

/// Deinitialize and free all memory
pub fn deinit() void {
    if (!initialized) return;

    var block = first_block;
    while (block) |b| {
        const next = b.next;
        std.heap.page_allocator.free(b.data);
        std.heap.page_allocator.destroy(b);
        block = next;
    }
    first_block = null;
    current_block = null;
    initialized = false;
}

/// Get arena stats for debugging
pub fn getStats() struct {
    malloc_count: usize,
    realloc_inplace: usize,
    realloc_copy: usize,
    free_reclaimed: usize,
    used_bytes: usize,
    grow_count: usize,
} {
    return .{
        .malloc_count = stats_malloc_count,
        .realloc_inplace = stats_realloc_inplace,
        .realloc_copy = stats_realloc_copy,
        .free_reclaimed = stats_free_reclaimed,
        .used_bytes = heap_ptr,
        .grow_count = stats_grow_count,
    };
}

fn grow(required: usize) bool {
    const size = @max(GROW_SIZE, required + HEADER_SIZE);

    const data = std.heap.page_allocator.alignedAlloc(u8, .@"16", size) catch {
        return false;
    };

    const block = std.heap.page_allocator.create(Block) catch {
        std.heap.page_allocator.free(data);
        return false;
    };

    block.* = .{
        .data = data,
        .next = null,
    };

    // Link to chain
    if (current_block) |cb| {
        cb.next = block;
    }
    current_block = block;

    // Switch to new block
    heap_base = data.ptr;
    heap_ptr = 0;
    heap_end = size;

    stats_grow_count += 1;
    return true;
}

fn alignUp(size: usize) usize {
    return (size + ALIGNMENT - 1) & ~(ALIGNMENT - 1);
}

fn bump_alloc(size: usize) ?[*]align(16) u8 {
    const aligned = alignUp(size);
    const new_ptr = heap_ptr + aligned;

    if (new_ptr > heap_end) {
        if (!grow(aligned)) return null;
        // After grow, heap_ptr is reset to 0
        const after_grow = heap_ptr + aligned;
        if (after_grow > heap_end) return null;
        const result: [*]align(16) u8 = @alignCast(heap_base + heap_ptr);
        heap_ptr = after_grow;
        return result;
    }

    const result: [*]align(16) u8 = @alignCast(heap_base + heap_ptr);
    heap_ptr = new_ptr;
    return result;
}

// ============================================================================
// QuickJS Allocator Interface
// ============================================================================

pub fn js_malloc(ctx: ?*anyopaque, size: usize) callconv(.c) ?*anyopaque {
    _ = ctx;
    init();
    if (size == 0) return null;

    const total = size + HEADER_SIZE;
    const ptr = bump_alloc(total) orelse return null;

    // Store size in header
    const header: *Header = @ptrCast(@alignCast(ptr));
    header.size = size;

    stats_malloc_count += 1;
    stats_bytes_allocated += size;
    return ptr + HEADER_SIZE;
}

pub fn js_calloc(ctx: ?*anyopaque, count: usize, size: usize) callconv(.c) ?*anyopaque {
    const total = count *| size;
    const ptr = js_malloc(ctx, total) orelse return null;
    const slice: [*]u8 = @ptrCast(ptr);
    @memset(slice[0..total], 0);
    return ptr;
}

/// Check if a pointer belongs to any of our arena blocks
fn isArenaPointer(ptr: *anyopaque) bool {
    const addr = @intFromPtr(ptr);
    var block = first_block;
    while (block) |b| {
        const block_start = @intFromPtr(b.data.ptr);
        const block_end = block_start + b.data.len;
        if (addr >= block_start and addr < block_end) {
            return true;
        }
        block = b.next;
    }
    return false;
}

pub fn js_free(ctx: ?*anyopaque, ptr: ?*anyopaque) callconv(.c) void {
    _ = ctx;
    if (ptr == null) return;
    if (!initialized) return;

    // Guard: ignore pointers not from our arena (e.g., frozen/static data)
    if (!isArenaPointer(ptr.?)) return;

    // LIFO optimization only works if pointer is in the CURRENT block
    // (not an older block from before a grow)
    const addr = @intFromPtr(ptr.?);
    const current_block_start = @intFromPtr(heap_base);
    const current_block_end = current_block_start + heap_end;

    // Only attempt LIFO reclaim if pointer is in current block
    if (addr >= current_block_start + HEADER_SIZE and addr < current_block_end) {
        const p: [*]u8 = @ptrCast(ptr.?);
        const header_ptr = p - HEADER_SIZE;
        const header: *const Header = @ptrCast(@alignCast(header_ptr));

        const alloc_end = @intFromPtr(header_ptr) - @intFromPtr(heap_base) + HEADER_SIZE + alignUp(header.size);
        if (alloc_end == heap_ptr) {
            // This is the last allocation - reclaim it!
            heap_ptr = @intFromPtr(header_ptr) - @intFromPtr(heap_base);
            stats_free_reclaimed += 1;
        }
    }
    // Else: "leak" it - we clean up at compilation end via reset()
}

pub fn js_realloc(ctx: ?*anyopaque, ptr: ?*anyopaque, new_size: usize) callconv(.c) ?*anyopaque {
    _ = ctx;
    init();

    // Case A: null ptr -> malloc
    if (ptr == null) return js_malloc(null, new_size);
    if (new_size == 0) return null;

    // Guard: if pointer is not from arena (e.g., frozen/static data),
    // we can't realloc it - just allocate fresh
    if (!isArenaPointer(ptr.?)) {
        return js_malloc(null, new_size);
    }

    // Check if pointer is in CURRENT block (for LIFO optimizations)
    const addr = @intFromPtr(ptr.?);
    const current_block_start = @intFromPtr(heap_base);
    const current_block_end = current_block_start + heap_end;
    const in_current_block = (addr >= current_block_start + HEADER_SIZE and addr < current_block_end);

    const p: [*]u8 = @ptrCast(ptr.?);
    const header_ptr = p - HEADER_SIZE;
    const header: *Header = @ptrCast(@alignCast(header_ptr));
    const old_size = header.size;

    // Case B: Shrinking -> just update size
    if (new_size <= old_size) {
        // If in current block and at top of arena, reclaim the extra space
        if (in_current_block) {
            const old_end = @intFromPtr(header_ptr) - @intFromPtr(heap_base) + HEADER_SIZE + alignUp(old_size);
            if (old_end == heap_ptr) {
                heap_ptr = @intFromPtr(header_ptr) - @intFromPtr(heap_base) + HEADER_SIZE + alignUp(new_size);
            }
        }
        header.size = new_size;
        return ptr;
    }

    // Case C: Growing - check if at top of arena (LIFO optimization) - only for current block
    if (in_current_block) {
        const old_end = @intFromPtr(header_ptr) - @intFromPtr(heap_base) + HEADER_SIZE + alignUp(old_size);
        if (old_end == heap_ptr) {
            // This is the last allocation! We can grow in-place (zero copy)
            const extra = alignUp(new_size) - alignUp(old_size);
            const new_end = heap_ptr + extra;

            // Ensure capacity
            if (new_end <= heap_end) {
                heap_ptr = new_end;
                header.size = new_size;
                stats_realloc_inplace += 1;
                return ptr; // Same pointer, no copy!
            }
            // Fall through to copy path if we can't grow current block
        }
    }

    // Case D: Not at top or block full or in old block, must copy to new location
    const new_ptr = js_malloc(null, new_size) orelse return null;
    const new_slice: [*]u8 = @ptrCast(new_ptr);
    const copy_size = @min(old_size, new_size);
    @memcpy(new_slice[0..copy_size], p[0..copy_size]);

    stats_realloc_copy += 1;
    return new_ptr;
}

pub fn js_malloc_usable_size(ptr: ?*const anyopaque) callconv(.c) usize {
    if (ptr == null) return 0;
    if (!initialized) return 0;

    // Guard: return 0 for non-arena pointers (e.g., frozen/static data)
    if (!isArenaPointer(@constCast(ptr.?))) return 0;

    const p: [*]const u8 = @ptrCast(ptr.?);
    const header: *const Header = @ptrCast(@alignCast(p - HEADER_SIZE));
    return header.size;
}

// ============================================================================
// Debug / Stats exports
// ============================================================================

/// Get heap base address (for V8-style pointer compression)
pub fn getHeapBase() usize {
    if (!initialized) return 0;
    return @intFromPtr(heap_base);
}

/// Print arena stats (for debugging)
pub fn printStats() void {
    const s = getStats();
    std.debug.print("[arena] malloc={d} realloc_inplace={d} realloc_copy={d} free_reclaimed={d} used={d}MB grows={d}\n", .{
        s.malloc_count,
        s.realloc_inplace,
        s.realloc_copy,
        s.free_reclaimed,
        s.used_bytes / (1024 * 1024),
        s.grow_count,
    });
}
