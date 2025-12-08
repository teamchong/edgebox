/// WASM Bump Allocator - Maximum Speed
///
/// - O(1) malloc: bump pointer + return
/// - O(1) free: no-op (memory reclaimed at process exit)
/// - O(1) realloc: alloc new + copy (needs size header)
///
/// Perfect for EdgeBox serverless functions (50ms - 30s lifetime)
const std = @import("std");

const ALIGNMENT: usize = 8;

// Header stores size for realloc
const Header = extern struct {
    size: usize,
};
const HEADER_SIZE: usize = @sizeOf(Header);

// Global state
var heap_ptr: usize = 0;
var heap_end: usize = 0;
var initialized: bool = false;

fn init() void {
    if (initialized) return;

    if (@import("builtin").target.cpu.arch == .wasm32) {
        // Grow memory by 16 pages (1MB) and use that new region
        // This avoids conflicting with libc's malloc which uses __heap_base
        const current_pages = @wasmMemorySize(0);
        const current_end = current_pages * 65536;

        // Grow by 64 pages (4MB) for our allocator
        const grow_result = @wasmMemoryGrow(0, 64);
        if (grow_result == -1) {
            // Fallback: use current end (risky but might work)
            heap_ptr = std.mem.alignForward(usize, current_end, ALIGNMENT);
        } else {
            // Use the new memory region
            heap_ptr = std.mem.alignForward(usize, current_end, ALIGNMENT);
        }
        heap_end = @wasmMemorySize(0) * 65536;
    } else {
        // Native testing
        const static = struct {
            var heap: [64 * 1024 * 1024]u8 align(8) = undefined;
        };
        heap_ptr = @intFromPtr(&static.heap);
        heap_end = heap_ptr + static.heap.len;
    }

    initialized = true;
}

fn grow(required: usize) bool {
    if (@import("builtin").target.cpu.arch != .wasm32) {
        return false;
    }

    const pages_needed = @max(16, (required + 65535) / 65536);
    const result = @wasmMemoryGrow(0, pages_needed);
    if (result == -1) return false;

    heap_end = @wasmMemorySize(0) * 65536;
    return true;
}

fn bump_alloc(size: usize) ?[*]u8 {
    const aligned = std.mem.alignForward(usize, size, ALIGNMENT);
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
// QuickJS Integration
// ============================================================================

pub fn qjs_malloc(ctx: ?*anyopaque, size: usize) callconv(.c) ?*anyopaque {
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

pub fn qjs_calloc(ctx: ?*anyopaque, count: usize, size: usize) callconv(.c) ?*anyopaque {
    const total = count *| size;
    const ptr = qjs_malloc(ctx, total) orelse return null;
    const slice: [*]u8 = @ptrCast(ptr);
    @memset(slice[0..total], 0);
    return ptr;
}

pub fn qjs_free(ctx: ?*anyopaque, ptr: ?*anyopaque) callconv(.c) void {
    _ = ctx;
    _ = ptr;
    // NO-OP - bump allocator doesn't free
    // Memory reclaimed when WASM instance exits
}

pub fn qjs_realloc(ctx: ?*anyopaque, ptr: ?*anyopaque, new_size: usize) callconv(.c) ?*anyopaque {
    _ = ctx;
    init();

    if (ptr == null) return qjs_malloc(null, new_size);
    if (new_size == 0) return null;

    const p: [*]u8 = @ptrCast(ptr.?);
    const header: *const Header = @ptrCast(@alignCast(p - HEADER_SIZE));
    const old_size = header.size;

    // Allocate new
    const new_ptr = qjs_malloc(null, new_size) orelse return null;
    const new_slice: [*]u8 = @ptrCast(new_ptr);

    // Copy data
    const copy_size = @min(old_size, new_size);
    @memcpy(new_slice[0..copy_size], p[0..copy_size]);

    return new_ptr;
}

pub fn qjs_malloc_usable_size(ptr: ?*const anyopaque) callconv(.c) usize {
    if (ptr == null) return 0;
    if (!initialized) return 0;

    const p: [*]const u8 = @ptrCast(ptr.?);
    const header: *const Header = @ptrCast(@alignCast(p - HEADER_SIZE));
    return header.size;
}

// ============================================================================
// Pool API (for compatibility)
// ============================================================================

pub fn initGlobalPool() void {
    init();
}

pub fn resetGlobalPool() void {
    // Could reset heap_ptr to heap_base for request-scoped cleanup
    // but QuickJS might still have references, so we don't
}

pub const WasmPoolAllocator = struct {
    pub const Stats = struct {
        alloc_count: usize,
        total_allocated: usize,
        used_bytes: usize,
    };
};

pub fn getGlobalPoolStats() ?WasmPoolAllocator.Stats {
    return null;
}
