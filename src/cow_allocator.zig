/// Copy-on-Write (CoW) Linear Memory Allocator for WAMR
///
/// STUB VERSION: The prebuilt WAMR library doesn't include the custom
/// linear memory callbacks. This stub provides no-op implementations.
///
/// To enable CoW:
/// 1. Build WAMR from source with COW_LINEAR_MEMORY=1
/// 2. Replace this stub with the full implementation
const std = @import("std");

/// Initialize CoW allocator - stub does nothing
pub fn init(_: std.mem.Allocator, _: []const u8) !void {}

/// Deinitialize CoW allocator - stub does nothing
pub fn deinit() void {}

/// Check if CoW is available - stub always returns false
pub fn isAvailable() bool {
    return false;
}

/// Create a memory image - stub does nothing
pub fn createMemoryImage(_: std.mem.Allocator, _: *anyopaque, _: []const u8) !void {}

/// Initialize with capture mode - stub does nothing
pub fn initWithCapture(_: std.mem.Allocator, _: []const u8) !void {}

/// Capture and enable - stub does nothing
pub fn captureAndEnable(_: *anyopaque) !void {}

/// Get snapshot page count - stub returns 0
pub fn getSnapshotPageCount() u32 {
    return 0;
}
