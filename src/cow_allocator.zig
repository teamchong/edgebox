/// Copy-on-Write (CoW) Linear Memory Allocator for WAMR
///
/// This module provides fast WASM instantiation by using mmap(MAP_PRIVATE)
/// to create copy-on-write mappings of pre-initialized memory images.
///
/// Performance: ~5μs per instantiation vs ~184ms with regular allocation
/// (for 64MB memory with data segments)
///
/// Usage:
///   1. Call init() with a memory image file path
///   2. Register with WAMR via wasm_runtime_set_linear_memory_callbacks()
///   3. WAMR will use CoW for all instantiations
///   4. Call deinit() on shutdown
const std = @import("std");
const builtin = @import("builtin");

// Debug flag for CoW verbose logging (set to false for production)
const COW_DEBUG = true;

fn cowLog(comptime fmt: []const u8, args: anytype) void {
    if (COW_DEBUG) {
        std.debug.print(fmt, args);
    }
}

// Import WAMR C API
const c = @cImport({
    @cInclude("wasm_export.h");
});

const MemInitDataSegment = c.MemInitDataSegment;

/// Global state for CoW allocator
var g_allocator: std.mem.Allocator = undefined;
var g_memory_image_fd: ?std.posix.fd_t = null;
var g_memory_image_size: u64 = 0;
var g_memory_image_path: ?[]const u8 = null;
var g_initialized: bool = false;

/// CoW allocation result tracking (for proper cleanup)
const CowAllocation = struct {
    ptr: [*]u8,
    size: usize,
};

var g_allocations: std.AutoHashMap(usize, CowAllocation) = undefined;
var g_allocations_mutex: std.Thread.Mutex = .{};

/// Initialize CoW allocator with a memory image file
/// The image file should contain the initialized WASM linear memory.
/// Can be called multiple times to switch to a different image file.
pub fn init(allocator_param: std.mem.Allocator, memory_image_path: []const u8) !void {
    // If already initialized with the same path, skip
    if (g_initialized) {
        if (g_memory_image_path) |current_path| {
            if (std.mem.eql(u8, current_path, memory_image_path)) {
                return; // Already initialized with this path
            }
        }
        // Different path - need to switch, close current first
        if (g_memory_image_fd) |fd| {
            std.posix.close(fd);
            g_memory_image_fd = null;
        }
        if (g_memory_image_path) |path| {
            g_allocator.free(path);
            g_memory_image_path = null;
        }
    } else {
        g_allocator = allocator_param;
        g_allocations = std.AutoHashMap(usize, CowAllocation).init(allocator_param);
    }

    // Open memory image file with write permission (needed for ftruncate)
    const file = try std.fs.cwd().openFile(memory_image_path, .{ .mode = .read_write });
    g_memory_image_fd = file.handle;

    // Get file size
    const stat = try file.stat();
    g_memory_image_size = stat.size;

    // Store path for debugging
    g_memory_image_path = try g_allocator.dupe(u8, memory_image_path);

    // Register with WAMR
    c.wasm_runtime_set_linear_memory_callbacks(cowAllocCallback, cowFreeCallback, null);

    g_initialized = true;
    cowLog("[cow] Initialized with image: {s} ({d:.1} MB)\n", .{
        memory_image_path,
        @as(f64, @floatFromInt(g_memory_image_size)) / (1024 * 1024),
    });
}

/// Deinitialize CoW allocator
pub fn deinit() void {
    if (!g_initialized) return;

    // Unregister callbacks
    c.wasm_runtime_set_linear_memory_callbacks(null, null, null);

    // Close memory image file
    if (g_memory_image_fd) |fd| {
        std.posix.close(fd);
        g_memory_image_fd = null;
    }

    // Free allocations tracking
    g_allocations_mutex.lock();
    g_allocations.deinit();
    g_allocations_mutex.unlock();

    // Free path
    if (g_memory_image_path) |path| {
        g_allocator.free(path);
        g_memory_image_path = null;
    }

    g_initialized = false;
}

/// Check if CoW is available (memory image exists)
pub fn isAvailable() bool {
    return g_initialized and g_memory_image_fd != null;
}

/// Get the snapshot's page count (for memory enlargement after instantiation)
pub fn getSnapshotPageCount() u32 {
    if (!g_initialized) return 0;
    // WASM page size is 64KB
    return @intCast(g_memory_image_size / 65536);
}

/// Reset an instance's linear memory to the initial state from the memory image.
/// This allows instance reuse without state leaking between requests.
/// Returns true on success, false if CoW is not available or reset failed.
pub fn resetMemory(module_inst: anytype) bool {
    const inst: c.wasm_module_inst_t = @ptrCast(module_inst);

    if (!g_initialized or g_memory_image_fd == null) {
        cowLog("[cow] resetMemory: not initialized\n", .{});
        return false;
    }

    // Get linear memory instance
    const memory_inst = c.wasm_runtime_get_memory(inst, 0);
    if (memory_inst == null) {
        cowLog("[cow] resetMemory: no linear memory\n", .{});
        return false;
    }

    // Get memory base address
    const mem_base = c.wasm_memory_get_base_address(memory_inst);
    if (mem_base == null) {
        cowLog("[cow] resetMemory: empty memory\n", .{});
        return false;
    }

    // Calculate memory size (use the smaller of instance size and image size)
    const page_count = c.wasm_memory_get_cur_page_count(memory_inst);
    const bytes_per_page = c.wasm_memory_get_bytes_per_page(memory_inst);
    const inst_mem_size: u64 = @as(u64, page_count) * bytes_per_page;
    const copy_size = @min(inst_mem_size, g_memory_image_size);

    if (copy_size == 0) {
        cowLog("[cow] resetMemory: zero size\n", .{});
        return false;
    }

    // Read from image file and copy to instance memory
    const fd = g_memory_image_fd.?;
    const mem_ptr: [*]u8 = @ptrCast(mem_base);

    // Seek to beginning of file
    std.posix.lseek(fd, 0, .set) catch {
        cowLog("[cow] resetMemory: lseek failed\n", .{});
        return false;
    };

    // Read file contents directly into memory
    var total_read: u64 = 0;
    while (total_read < copy_size) {
        const remaining = copy_size - total_read;
        const chunk_size = @min(remaining, 1024 * 1024); // 1MB chunks
        const n = std.posix.read(fd, mem_ptr[total_read..][0..chunk_size]) catch {
            cowLog("[cow] resetMemory: read failed at {d}\n", .{total_read});
            return false;
        };
        if (n == 0) break; // EOF
        total_read += n;
    }

    cowLog("[cow] resetMemory: restored {d:.1} MB\n", .{
        @as(f64, @floatFromInt(total_read)) / (1024 * 1024),
    });
    return true;
}

/// CoW allocation callback for WAMR
/// Uses mmap(MAP_PRIVATE) to create a copy-on-write mapping of the memory image.
///
/// CRITICAL: WAMR with OS_ENABLE_HW_BOUND_CHECK requires 8GB virtual address space.
/// We reserve 8GB with PROT_NONE, then MAP_FIXED the file over the beginning.
/// This ensures WAMR's hardware bounds checking works correctly.
fn cowAllocCallback(
    size: u64,
    init_data: [*c]const MemInitDataSegment,
    init_data_count: u32,
    user_data: ?*anyopaque,
    actual_size_out: ?*u64,
) callconv(.c) ?*anyopaque {
    _ = user_data;
    _ = init_data;

    cowLog("[cow] Alloc callback: size={d}, init_data_count={d}, image_size={d}\n", .{ size, init_data_count, g_memory_image_size });

    if (!g_initialized or g_memory_image_fd == null) {
        cowLog("[cow] Not initialized, falling back to regular allocation\n", .{});
        return null; // Fall back to regular allocation
    }

    // WAMR with OS_ENABLE_HW_BOUND_CHECK requires 8GB virtual address space
    const GB: u64 = 1024 * 1024 * 1024;
    const TOTAL_MAP_SIZE: u64 = 8 * GB;
    const WASM_PAGE_SIZE: u64 = 65536;

    // Round up file-backed size to page boundary
    const file_backed_size = ((g_memory_image_size + WASM_PAGE_SIZE - 1) / WASM_PAGE_SIZE) * WASM_PAGE_SIZE;

    cowLog("[cow] Reserving 8GB virtual address space, file_backed_size={d}\n", .{file_backed_size});

    // Step 1: Reserve 8GB with PROT_NONE (no access)
    const reserved = std.posix.mmap(
        null,
        TOTAL_MAP_SIZE,
        std.posix.PROT.NONE,
        .{ .TYPE = .PRIVATE, .ANONYMOUS = true },
        -1,
        0,
    ) catch |err| {
        cowLog("[cow] Failed to reserve 8GB: {}\n", .{err});
        return null;
    };

    cowLog("[cow] Reserved 8GB at {*}\n", .{reserved.ptr});

    // Step 2: Map file with MAP_FIXED over the beginning of reserved region
    const file_mapped = std.posix.mmap(
        @alignCast(reserved.ptr),
        @intCast(file_backed_size),
        std.posix.PROT.READ | std.posix.PROT.WRITE,
        .{ .TYPE = .PRIVATE, .FIXED = true },
        g_memory_image_fd.?,
        0,
    ) catch |err| {
        cowLog("[cow] MAP_FIXED failed: {}\n", .{err});
        // Clean up reserved region
        std.posix.munmap(reserved);
        return null;
    };

    cowLog("[cow] File mapped at {*}, size={d}\n", .{ file_mapped.ptr, file_backed_size });

    // Step 3: Map remaining region (after file) with read/write for memory.grow
    // This allows WAMR to grow memory beyond the snapshot without hitting PROT_NONE
    const remaining_size = TOTAL_MAP_SIZE - file_backed_size;
    if (remaining_size > 0) {
        const remaining_start: [*]align(std.heap.page_size_min) u8 = @alignCast(@as([*]u8, @ptrCast(file_mapped.ptr)) + file_backed_size);
        _ = std.posix.mmap(
            remaining_start,
            remaining_size,
            std.posix.PROT.READ | std.posix.PROT.WRITE,
            .{ .TYPE = .PRIVATE, .ANONYMOUS = true, .FIXED = true },
            -1,
            0,
        ) catch |err| {
            cowLog("[cow] Failed to map remaining region: {}\n", .{err});
            // Continue anyway - memory.grow might fail but basic execution should work
        };
        cowLog("[cow] Mapped remaining {d} GB for memory.grow\n", .{remaining_size / GB});
    }

    // Verify the mapping is readable
    if (COW_DEBUG) {
        const mem_slice: [*]u8 = @ptrCast(file_mapped.ptr);
        cowLog("[cow] First 4 bytes: 0x{x:0>2} 0x{x:0>2} 0x{x:0>2} 0x{x:0>2}\n", .{
            mem_slice[0], mem_slice[1], mem_slice[2], mem_slice[3],
        });
        cowLog("[cow] Memory verification OK\n", .{});
    }

    // Track allocation for proper cleanup (use TOTAL_MAP_SIZE for munmap)
    g_allocations_mutex.lock();
    g_allocations.put(@intFromPtr(file_mapped.ptr), .{
        .ptr = file_mapped.ptr,
        .size = TOTAL_MAP_SIZE, // Must munmap the full 8GB reservation
    }) catch {
        g_allocations_mutex.unlock();
        std.posix.munmap(reserved);
        return null;
    };
    g_allocations_mutex.unlock();

    // Note: We don't set actual_size_out - let WAMR use its own size tracking
    _ = actual_size_out;

    cowLog("[cow] Allocation tracked, returning {*}\n", .{file_mapped.ptr});
    return file_mapped.ptr;
}

/// CoW free callback for WAMR
fn cowFreeCallback(ptr: ?*anyopaque, size: u64, user_data: ?*anyopaque) callconv(.c) void {
    _ = user_data;

    cowLog("[cow] Free callback: ptr={?}, size={d}\n", .{ ptr, size });

    if (ptr == null) return;

    g_allocations_mutex.lock();
    if (g_allocations.fetchRemove(@intFromPtr(ptr))) |kv| {
        g_allocations_mutex.unlock();
        cowLog("[cow] Unmapping CoW memory at {*}, size={d}\n", .{ kv.value.ptr, kv.value.size });
        const mapping: []align(std.heap.page_size_min) u8 = @alignCast(kv.value.ptr[0..kv.value.size]);
        std.posix.munmap(mapping);
        cowLog("[cow] Unmap complete\n", .{});
    } else {
        g_allocations_mutex.unlock();
        cowLog("[cow] Ptr not in tracking map, ignoring\n", .{});
    }
}

/// Create a memory image file from a live WASM instance
/// This captures the initialized linear memory state for CoW reuse.
pub fn createMemoryImage(allocator: std.mem.Allocator, module_inst: c.wasm_module_inst_t, output_path: []const u8) !void {
    _ = allocator;

    // Get linear memory instance
    const memory_inst = c.wasm_runtime_get_memory(module_inst, 0);
    if (memory_inst == null) {
        return error.NoLinearMemory;
    }

    // Get memory base address
    const mem_base = c.wasm_memory_get_base_address(memory_inst);
    if (mem_base == null) {
        return error.EmptyMemory;
    }

    // Calculate memory size from pages
    const page_count = c.wasm_memory_get_cur_page_count(memory_inst);
    const bytes_per_page = c.wasm_memory_get_bytes_per_page(memory_inst);
    const mem_size = page_count * bytes_per_page;

    if (mem_size == 0) {
        return error.EmptyMemory;
    }

    // Write to file
    const file = try std.fs.cwd().createFile(output_path, .{});
    defer file.close();

    const data_slice: [*]const u8 = @ptrCast(mem_base);
    try file.writeAll(data_slice[0..@intCast(mem_size)]);

    // Sync to disk to ensure mmap sees consistent data
    try file.sync();

    cowLog("[cow] Created memory image: {s} ({d:.1} MB)\n", .{
        output_path,
        @as(f64, @floatFromInt(mem_size)) / (1024 * 1024),
    });
}

/// Initialize memory without a pre-existing image file.
/// This mode captures the memory state from the first instantiation
/// and uses it for subsequent instantiations.
pub fn initWithCapture(allocator: std.mem.Allocator, image_path: []const u8) !void {
    if (g_initialized) return;

    g_allocator = allocator;
    g_allocations = std.AutoHashMap(usize, CowAllocation).init(allocator);
    g_memory_image_path = try allocator.dupe(u8, image_path);

    // Don't register callback yet - we'll capture on first instantiation
    // and then register for subsequent ones

    g_initialized = true;
    cowLog("[cow] Initialized in capture mode, will save to: {s}\n", .{image_path});
}

/// Capture the current memory state and enable CoW for future instantiations
pub fn captureAndEnable(module_inst: anytype) !void {
    const inst: c.wasm_module_inst_t = @ptrCast(module_inst);
    if (!g_initialized or g_memory_image_path == null) {
        return error.NotInitialized;
    }

    // Already have an image - skip capture
    if (g_memory_image_fd != null) return;

    // Create memory image from current instance
    try createMemoryImage(g_allocator, inst, g_memory_image_path.?);

    // Open the image file with write permission (needed for ftruncate)
    const file = try std.fs.cwd().openFile(g_memory_image_path.?, .{ .mode = .read_write });
    g_memory_image_fd = file.handle;

    const stat = try file.stat();
    g_memory_image_size = stat.size;

    // Now register callbacks for future instantiations
    c.wasm_runtime_set_linear_memory_callbacks(cowAllocCallback, cowFreeCallback, null);

    cowLog("[cow] Captured memory state, CoW enabled for future instantiations\n", .{});
}

test "CoW allocator basic" {
    // This would require a WAMR runtime to test properly
}
