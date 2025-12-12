/// WASM Smart Arena Allocator
///
/// A bump allocator with LIFO optimizations for QuickJS patterns:
/// - O(1) malloc: bump pointer with size header
/// - O(1) realloc: in-place growth if at top of arena (common for arrays/strings)
/// - O(1) free: reclaims space if at top of arena (LIFO pattern)
/// - Reset: instant cleanup at request end
///
/// This handles QuickJS's allocation patterns efficiently:
/// - Array.push() -> realloc at top -> zero copy growth
/// - String building -> realloc at top -> zero copy growth
/// - Stack-like temp allocations -> free at top -> space reclaimed
const std = @import("std");

const ALIGNMENT: usize = 8;

// Header for size tracking (needed for realloc and LIFO free)
const Header = extern struct {
    size: usize,
};
const HEADER_SIZE: usize = @sizeOf(Header);

// Global state
var heap_base: usize = 0; // Start of arena (for reset)
var heap_ptr: usize = 0; // Current allocation pointer
var heap_end: usize = 0; // End of available memory
var heap_mark: usize = 0; // Snapshot mark (for Wizer)
var initialized: bool = false;

// Stats for debugging
var stats_malloc_count: usize = 0;
var stats_realloc_inplace: usize = 0;
var stats_realloc_copy: usize = 0;
var stats_free_reclaimed: usize = 0;

pub fn init() void {
    if (initialized) return;

    if (@import("builtin").target.cpu.arch == .wasm32) {
        // Get current memory end and grow from there
        const pages = @wasmMemorySize(0);
        const addr = pages * 65536;

        heap_base = addr;
        heap_ptr = addr;
        heap_end = addr;
        heap_mark = addr;

        // Pre-grow 4MB for our allocator
        _ = grow(4 * 1024 * 1024);
    }

    initialized = true;
}

/// Save current position as mark (for Wizer snapshot)
pub fn mark() void {
    heap_mark = heap_ptr;
}

/// Reset arena to mark position (instant cleanup)
pub fn reset() void {
    heap_ptr = heap_mark;
}

/// Reset arena to base (full cleanup)
pub fn resetAll() void {
    heap_ptr = heap_base;
}

/// Export reset for WASM (can be called from host after request)
export fn arena_reset() void {
    reset();
}

/// Export mark for WASM (can be called from host before request)
export fn arena_mark() void {
    mark();
}

/// Get arena stats for debugging
pub fn getStats() struct { malloc_count: usize, realloc_inplace: usize, realloc_copy: usize, free_reclaimed: usize, used_bytes: usize } {
    return .{
        .malloc_count = stats_malloc_count,
        .realloc_inplace = stats_realloc_inplace,
        .realloc_copy = stats_realloc_copy,
        .free_reclaimed = stats_free_reclaimed,
        .used_bytes = heap_ptr - heap_base,
    };
}

fn grow(required: usize) bool {
    if (@import("builtin").target.cpu.arch != .wasm32) {
        return false;
    }

    const pages_needed = (required + 65535) / 65536;
    const result = @wasmMemoryGrow(0, pages_needed);
    if (result == -1) return false;

    heap_end = @wasmMemorySize(0) * 65536;
    return true;
}

fn bump_alloc(size: usize) ?[*]u8 {
    const aligned = (size + ALIGNMENT - 1) & ~(ALIGNMENT - 1);
    const new_ptr = heap_ptr + aligned;

    if (new_ptr > heap_end) {
        if (!grow(aligned)) return null;
        if (new_ptr > heap_end) return null;
    }

    const result: [*]u8 = @ptrFromInt(heap_ptr);
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
    return ptr + HEADER_SIZE;
}

pub fn js_calloc(ctx: ?*anyopaque, count: usize, size: usize) callconv(.c) ?*anyopaque {
    const total = count *| size;
    const ptr = js_malloc(ctx, total) orelse return null;
    const slice: [*]u8 = @ptrCast(ptr);
    @memset(slice[0..total], 0);
    return ptr;
}

pub fn js_free(ctx: ?*anyopaque, ptr: ?*anyopaque) callconv(.c) void {
    _ = ctx;
    if (ptr == null) return;

    // LIFO optimization: if freeing the last allocation, reclaim space
    const p: [*]u8 = @ptrCast(ptr.?);
    const header_ptr = p - HEADER_SIZE;
    const header: *const Header = @ptrCast(@alignCast(header_ptr));

    const alloc_end = @intFromPtr(p) + alignUp(header.size);
    if (alloc_end == heap_ptr) {
        // This is the last allocation - reclaim it!
        heap_ptr = @intFromPtr(header_ptr);
        stats_free_reclaimed += 1;
    }
    // Else: "leak" it - we clean up at request end via reset()
}

pub fn js_realloc(ctx: ?*anyopaque, ptr: ?*anyopaque, new_size: usize) callconv(.c) ?*anyopaque {
    _ = ctx;
    init();

    // Case A: null ptr -> malloc
    if (ptr == null) return js_malloc(null, new_size);
    if (new_size == 0) return null;

    const p: [*]u8 = @ptrCast(ptr.?);
    const header_ptr = p - HEADER_SIZE;
    const header: *Header = @ptrCast(@alignCast(header_ptr));
    const old_size = header.size;

    // Case B: Shrinking -> just update size
    if (new_size <= old_size) {
        // If at top of arena, reclaim the extra space
        const old_end = @intFromPtr(p) + alignUp(old_size);
        if (old_end == heap_ptr) {
            heap_ptr = @intFromPtr(p) + alignUp(new_size);
        }
        header.size = new_size;
        return ptr;
    }

    // Case C: Growing - check if at top of arena (LIFO optimization)
    const old_end = @intFromPtr(p) + alignUp(old_size);
    if (old_end == heap_ptr) {
        // This is the last allocation! We can grow in-place (zero copy)
        const extra = alignUp(new_size) - alignUp(old_size);
        const new_end = heap_ptr + extra;

        // Ensure capacity
        if (new_end > heap_end) {
            if (!grow(extra)) {
                // Fall through to copy path
            } else {
                heap_ptr = new_end;
                header.size = new_size;
                stats_realloc_inplace += 1;
                return ptr; // Same pointer, no copy!
            }
        } else {
            heap_ptr = new_end;
            header.size = new_size;
            stats_realloc_inplace += 1;
            return ptr; // Same pointer, no copy!
        }
    }

    // Case D: Not at top, must copy to new location
    const new_ptr = js_malloc(null, new_size) orelse return null;
    const new_slice: [*]u8 = @ptrCast(new_ptr);
    const copy_size = @min(old_size, new_size);
    @memcpy(new_slice[0..copy_size], p[0..copy_size]);

    stats_realloc_copy += 1;
    return new_ptr;
}

fn alignUp(size: usize) usize {
    return (size + ALIGNMENT - 1) & ~(ALIGNMENT - 1);
}

pub fn js_malloc_usable_size(ptr: ?*const anyopaque) callconv(.c) usize {
    if (ptr == null) return 0;
    if (!initialized) return 0;

    const p: [*]const u8 = @ptrCast(ptr.?);
    const header: *const Header = @ptrCast(@alignCast(p - HEADER_SIZE));
    return header.size;
}
