//! Parallel Prefetching for SSD-bound Module Loading
//!
//! Single-threaded page faulting is latency-bound (~200MB/s).
//! By using multiple threads to touch pages in parallel, we can
//! saturate the SSD bandwidth (~2000MB/s on M2 Pro).
//!
//! Based on zell's prefetch.zig implementation.

const std = @import("std");
const builtin = @import("builtin");

const log = std.log.scoped(.prefetch);

/// Number of prefetch threads to spawn
/// 4 threads is optimal for most NVMe SSDs
const PREFETCH_THREADS = 4;

/// Page size (4KB on both macOS and Linux)
const PAGE_SIZE = 4096;

/// Prefetch a memory region in parallel using multiple threads.
/// This saturates SSD bandwidth by triggering page faults concurrently.
///
/// Usage:
///   const mapped = mmap(file, ...);
///   prefetchParallel(mapped, file_size);  // Now all pages in RAM
///
pub fn prefetchParallel(ptr: [*]const u8, size: usize) void {
    if (size == 0) return;

    // For small files, single-threaded is faster (no thread spawn overhead)
    if (size < PAGE_SIZE * PREFETCH_THREADS * 4) {
        prefetchWorker(ptr, size);
        return;
    }

    var threads: [PREFETCH_THREADS]?std.Thread = .{null} ** PREFETCH_THREADS;
    const chunk_size = size / PREFETCH_THREADS;

    // Spawn worker threads
    for (0..PREFETCH_THREADS) |i| {
        const start = i * chunk_size;
        const len = if (i == PREFETCH_THREADS - 1)
            size - start // Last thread gets remainder
        else
            chunk_size;

        threads[i] = std.Thread.spawn(.{}, prefetchWorker, .{
            ptr + start,
            len,
        }) catch null;
    }

    // Wait for all threads to complete
    for (&threads) |*maybe_thread| {
        if (maybe_thread.*) |thread| {
            thread.join();
        }
    }
}

/// Worker function that touches pages to trigger prefetch.
/// Uses volatile reads to prevent compiler optimization.
fn prefetchWorker(start_ptr: [*]const u8, len: usize) void {
    var i: usize = 0;

    // Volatile read to force page fault - compiler can't optimize away
    const vol_ptr: [*]volatile const u8 = @ptrCast(start_ptr);

    while (i < len) : (i += PAGE_SIZE) {
        _ = vol_ptr[i];
    }

    // Touch last byte if not page-aligned
    if (len > 0 and i != len) {
        _ = vol_ptr[len - 1];
    }
}

/// Prefetch a slice (convenience wrapper)
pub fn prefetchSlice(slice: []const u8) void {
    if (slice.len == 0) return;
    prefetchParallel(slice.ptr, slice.len);
}

/// Platform-optimized madvise wrapper
/// Returns true if madvise succeeded (hint accepted)
pub fn adviseWillNeed(ptr: [*]const u8, size: usize) bool {
    if (size == 0) return true;

    switch (builtin.os.tag) {
        .macos, .linux, .freebsd, .netbsd, .openbsd => {
            const c = @cImport(@cInclude("sys/mman.h"));
            const result = c.madvise(
                @constCast(@ptrCast(ptr)),
                size,
                c.MADV_WILLNEED,
            );
            return result == 0;
        },
        else => return false,
    }
}

/// Combined prefetch: madvise + parallel page touching
/// This is the recommended approach for maximum throughput.
pub fn prefetchOptimized(ptr: [*]const u8, size: usize) void {
    if (size == 0) return;

    // First, tell kernel to start background prefetch
    _ = adviseWillNeed(ptr, size);

    // Then touch pages in parallel to ensure they're in RAM
    prefetchParallel(ptr, size);
}

// Tests
test "prefetch small buffer" {
    const allocator = std.testing.allocator;

    // Create test buffer
    const size = PAGE_SIZE * 2;
    const buffer = try allocator.alloc(u8, size);
    defer allocator.free(buffer);

    // Fill with pattern
    @memset(buffer, 0xAB);

    // Prefetch (should work without crash)
    prefetchParallel(buffer.ptr, buffer.len);

    // Verify data intact
    try std.testing.expectEqual(@as(u8, 0xAB), buffer[0]);
    try std.testing.expectEqual(@as(u8, 0xAB), buffer[size - 1]);
}

test "prefetch large buffer parallel" {
    const allocator = std.testing.allocator;

    // Create larger buffer (1MB - will use parallel threads)
    const size = 1024 * 1024;
    const buffer = try allocator.alloc(u8, size);
    defer allocator.free(buffer);

    @memset(buffer, 0xCD);

    // Prefetch with parallel threads
    prefetchParallel(buffer.ptr, buffer.len);

    // Verify data intact
    try std.testing.expectEqual(@as(u8, 0xCD), buffer[0]);
    try std.testing.expectEqual(@as(u8, 0xCD), buffer[size / 2]);
    try std.testing.expectEqual(@as(u8, 0xCD), buffer[size - 1]);
}

test "prefetch empty" {
    // Should not crash on empty input
    prefetchParallel(@as([*]const u8, undefined), 0);
    prefetchSlice(&[_]u8{});
}
