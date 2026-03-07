/// Native Arena Allocator for QuickJS
///
/// A bump allocator optimized for TSC-like workloads:
/// - O(1) malloc: bump pointer with size header
/// - O(1) realloc: in-place growth if at top of arena
/// - O(1) free: reclaims space if at top (LIFO pattern)
/// - Reset: instant cleanup after compilation
///
/// On Linux x86_64, uses a single large virtual address reservation (mmap PROT_NONE)
/// to keep all allocations contiguous. This is critical for CompressedValue pointer
/// encoding which requires all pointers to be within 44 bits of the heap base.
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

// Arena block for chained allocation (used in fallback mode only)
const Block = struct {
    data: []align(16) u8,
    next: ?*Block,
};

// Global state
var heap_base: [*]align(16) u8 = undefined;
var heap_ptr: usize = 0;
var heap_end: usize = 0; // committed bytes
var heap_mark: usize = 0;
var initialized: bool = false;

// Virtual reservation (Linux contiguous mode)
var reserved_size: usize = 0; // total reserved virtual address space
var using_reservation: bool = false; // true if using contiguous reservation

// Block chain for fallback mode (non-Linux or reservation failure)
var first_block: ?*Block = null;
var current_block: ?*Block = null;

// Virtual reservation size: 32GB — covers large TSC workloads (date-fns, typeorm, playwright).
// Only virtual address space is reserved (no physical memory until committed).
const RESERVED_SIZE: usize = 32 * 1024 * 1024 * 1024;
// Initial commit size
const INITIAL_COMMIT: usize = 64 * 1024 * 1024;
// Commit growth granularity
const COMMIT_GRANULARITY: usize = 64 * 1024 * 1024;
// Growth size for fallback block chain mode
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

    // On Linux, try to reserve a large contiguous virtual address range.
    // This ensures all arena pointers are within a single base+offset range,
    // which is required for CompressedValue's 44-bit pointer encoding.
    if (comptime builtin.os.tag == .linux) {
        if (initContiguous()) {
            initialized = true;
            return;
        }
    }

    // Fallback: use page_allocator (block chain mode — may break CompressedValue on Linux x86_64)
    initFallback();
    initialized = true;
}

/// Reserve a large contiguous virtual address space and commit initial pages.
/// Tries to allocate in the low 44-bit address range (< 0x100_0000_0000 = 1TB)
/// so that pointer compression can use the fast path (addr & MASK) without
/// needing base-offset encoding. Falls back to any address if low range is full.
fn initContiguous() bool {
    const linux = std.os.linux;

    // Try to reserve in the low address range (below 44-bit limit).
    // Hint at 0x10_0000_0000 (64GB) — high enough to avoid conflicts with
    // the program text/data/stack, low enough to fit in 44 bits.
    const LOW_HINT: usize = 0x10_0000_0000; // 64GB
    var result = linux.mmap(@ptrFromInt(LOW_HINT), RESERVED_SIZE, linux.PROT.NONE, .{ .TYPE = .PRIVATE, .ANONYMOUS = true }, -1, 0);
    var addr = @as(usize, @bitCast(result));

    // Check if we got a low address (within 44-bit range)
    if (addr == @as(usize, @bitCast(@as(isize, -1))) or addr + RESERVED_SIZE > 0xFFF_FFFF_FFFF) {
        // Low hint failed or returned high address — try without hint
        if (addr != @as(usize, @bitCast(@as(isize, -1)))) {
            _ = linux.munmap(@ptrFromInt(addr), RESERVED_SIZE);
        }
        result = linux.mmap(null, RESERVED_SIZE, linux.PROT.NONE, .{ .TYPE = .PRIVATE, .ANONYMOUS = true }, -1, 0);
        addr = @as(usize, @bitCast(result));
        if (addr == @as(usize, @bitCast(@as(isize, -1)))) {
            return false;
        }
    }

    // Commit initial pages with PROT_READ | PROT_WRITE
    const commit_result = linux.mprotect(@ptrFromInt(addr), INITIAL_COMMIT, linux.PROT.READ | linux.PROT.WRITE);
    if (commit_result != 0) {
        _ = linux.munmap(@ptrFromInt(addr), RESERVED_SIZE);
        return false;
    }

    heap_base = @ptrFromInt(addr);
    heap_ptr = 0;
    heap_end = INITIAL_COMMIT;
    heap_mark = 0;
    reserved_size = RESERVED_SIZE;
    using_reservation = true;
    return true;
}

/// Fallback initialization using page_allocator
fn initFallback() void {
    const data = std.heap.page_allocator.alignedAlloc(u8, .@"16", INITIAL_COMMIT) catch {
        @panic("Failed to allocate native arena");
    };

    heap_base = data.ptr;
    heap_ptr = 0;
    heap_end = INITIAL_COMMIT;
    heap_mark = 0;

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

    if (using_reservation) {
        // Single munmap for the entire reservation
        const linux = std.os.linux;
        _ = linux.munmap(@ptrCast(heap_base), reserved_size);
        using_reservation = false;
        reserved_size = 0;
    } else {
        var block = first_block;
        while (block) |b| {
            const next = b.next;
            std.heap.page_allocator.free(b.data);
            std.heap.page_allocator.destroy(b);
            block = next;
        }
        first_block = null;
        current_block = null;
    }
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

/// Grow the arena. In contiguous mode, commit more pages from the reservation.
/// In fallback mode, allocate a new block.
fn grow(required: usize) bool {
    if (using_reservation) {
        return growContiguous(required);
    } else {
        return growFallback(required);
    }
}

/// Commit more pages from the contiguous reservation
fn growContiguous(required: usize) bool {
    const linux = std.os.linux;
    const needed = heap_ptr + required;
    if (needed <= heap_end) return true; // Already have enough

    // Round up to COMMIT_GRANULARITY
    const new_commit = ((needed + COMMIT_GRANULARITY - 1) / COMMIT_GRANULARITY) * COMMIT_GRANULARITY;
    if (new_commit > reserved_size) {
        // Exceeded virtual reservation — this would require > 8GB heap
        @panic("Arena exceeded 8GB virtual reservation");
    }

    // Commit new pages
    const commit_start = @intFromPtr(heap_base) + heap_end;
    const commit_size = new_commit - heap_end;
    const result = linux.mprotect(@ptrFromInt(commit_start), commit_size, linux.PROT.READ | linux.PROT.WRITE);
    if (result != 0) return false;

    heap_end = new_commit;
    stats_grow_count += 1;
    return true;
}

/// Fallback: allocate a new block (breaks CompressedValue on Linux x86_64)
fn growFallback(required: usize) bool {
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
        if (using_reservation) {
            // Contiguous mode: heap_ptr hasn't changed, just more pages committed
            const result: [*]align(16) u8 = @alignCast(heap_base + heap_ptr);
            heap_ptr = heap_ptr + aligned;
            return result;
        } else {
            // Fallback mode: heap_ptr reset to 0 after grow
            const after_grow = heap_ptr + aligned;
            if (after_grow > heap_end) return null;
            const result: [*]align(16) u8 = @alignCast(heap_base + heap_ptr);
            heap_ptr = after_grow;
            return result;
        }
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

/// Check if a pointer belongs to our arena
fn isArenaPointer(ptr: *anyopaque) bool {
    const addr = @intFromPtr(ptr);
    if (using_reservation) {
        // Contiguous mode: single range check
        const base = @intFromPtr(heap_base);
        return addr >= base and addr < base + reserved_size;
    }
    // Fallback mode: check all blocks
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

    // LIFO optimization: reclaim if this is the most recent allocation
    const addr = @intFromPtr(ptr.?);
    const current_block_start = @intFromPtr(heap_base);
    const current_block_end = current_block_start + heap_end;

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

    // Check if pointer is in the active region (for LIFO optimizations)
    const addr = @intFromPtr(ptr.?);
    const current_block_start = @intFromPtr(heap_base);

    // Read old size from header
    const p: [*]u8 = @ptrCast(ptr.?);
    const header_ptr = p - HEADER_SIZE;
    const header: *Header = @ptrCast(@alignCast(header_ptr));
    const old_size = header.size;

    // Case B: shrink - always in-place
    if (new_size <= old_size) {
        header.size = new_size;
        return ptr;
    }

    // Case C: grow — try in-place if this is the last allocation in the current region
    if (addr >= current_block_start + HEADER_SIZE) {
        const alloc_end = @intFromPtr(header_ptr) - @intFromPtr(heap_base) + HEADER_SIZE + alignUp(old_size);
        if (alloc_end == heap_ptr) {
            // Last allocation - try to extend
            const needed = HEADER_SIZE + alignUp(new_size);
            const base_offset = @intFromPtr(header_ptr) - @intFromPtr(heap_base);
            if (base_offset + needed <= heap_end or grow(base_offset + needed)) {
                heap_ptr = base_offset + needed;
                header.size = new_size;
                stats_realloc_inplace += 1;
                return ptr;
            }
        }
    }

    // Case D: copy to new location
    const new_ptr = js_malloc(null, new_size) orelse return null;
    const dest: [*]u8 = @ptrCast(new_ptr);
    const src: [*]const u8 = @ptrCast(ptr.?);
    @memcpy(dest[0..old_size], src[0..old_size]);
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

pub fn getHeapBase() usize {
    if (!initialized) return 0;
    const base = @intFromPtr(heap_base);
    // If arena is in the low 44-bit range, return 0 so CompressedValue uses
    // the fast path (addr & PTR_ADDR_MASK) without base-offset encoding.
    // This matches macOS ARM64 behavior where addresses are naturally low.
    if (base + RESERVED_SIZE <= 0xFFF_FFFF_FFFF) return 0;
    return base;
}
