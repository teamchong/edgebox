// v8_tsc_transforms.zig — TSC source transforms
//
// DEPRECATED: All transforms are now applied via patch file:
//   patches/typescript/001-edgebox-parallel.patch
//
// Applied to node_modules/typescript/lib/{typescript.js,_tsc.js}
// at install time via: ./scripts/apply-patches.sh
//
// This file is kept for compatibility but does NO transforms.
// The snapshot uses the pre-patched typescript.js.
// The runtime uses the pre-patched _tsc.js.

const std = @import("std");

/// No-op — all transforms are in the patch file now.
/// Returns a copy of the source unchanged.
pub fn apply(allocator: std.mem.Allocator, source: []const u8) ![]u8 {
    const buf = try allocator.alloc(u8, source.len);
    @memcpy(buf, source);
    return buf;
}
