/// WASM Bump Allocator - Maximum Speed
///
/// - O(1) malloc: bump pointer
/// - O(1) free: NO-OP
/// - realloc: needs size headers for safety
const std = @import("std");

const ALIGNMENT: usize = 8;

// Header for size tracking (needed for realloc)
const Header = extern struct {
    size: usize,
};
const HEADER_SIZE: usize = @sizeOf(Header);

// Global state
var heap_ptr: usize = 0;
var heap_end: usize = 0;
var initialized: bool = false;

pub fn init() void {
    if (initialized) return;

    if (@import("builtin").target.cpu.arch == .wasm32) {
        // Get current memory end and grow from there
        const pages = @wasmMemorySize(0);
        const addr = pages * 65536;

        heap_ptr = addr;
        heap_end = addr;

        // Pre-grow 4MB for our allocator
        _ = grow(4 * 1024 * 1024);
    }

    initialized = true;
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
    _ = ptr;
    // NO-OP - bump allocator doesn't free
}

pub fn js_realloc(ctx: ?*anyopaque, ptr: ?*anyopaque, new_size: usize) callconv(.c) ?*anyopaque {
    _ = ctx;
    init();

    if (ptr == null) return js_malloc(null, new_size);
    if (new_size == 0) return null;

    const p: [*]u8 = @ptrCast(ptr.?);
    const header: *const Header = @ptrCast(@alignCast(p - HEADER_SIZE));
    const old_size = header.size;

    const new_ptr = js_malloc(null, new_size) orelse return null;
    const new_slice: [*]u8 = @ptrCast(new_ptr);
    const copy_size = @min(old_size, new_size);
    @memcpy(new_slice[0..copy_size], p[0..copy_size]);

    return new_ptr;
}

pub fn js_malloc_usable_size(ptr: ?*const anyopaque) callconv(.c) usize {
    if (ptr == null) return 0;
    if (!initialized) return 0;

    const p: [*]const u8 = @ptrCast(ptr.?);
    const header: *const Header = @ptrCast(@alignCast(p - HEADER_SIZE));
    return header.size;
}
