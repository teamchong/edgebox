// V8 linking test — Phase 1 of V8 embedding
// Verifies that librusty_v8.a links correctly with Zig.
// Calls v8__V8__GetVersion() and initializes the V8 platform.
//
// Build: zig build v8-test
// Run: ./zig-out/bin/edgebox-v8-test

const std = @import("std");

// Extern declarations for functions in librusty_v8.a (binding.cc)
// These are all extern "C" — no name mangling, ABI-compatible with Zig.
// V8 types are opaque pointers from Zig's perspective.
const V8Platform = opaque {};

extern fn v8__V8__GetVersion() [*:0]const u8;
extern fn v8__Platform__NewDefaultPlatform(thread_pool_size: c_int, idle_task_support: bool) ?*V8Platform;
extern fn v8__V8__InitializePlatform(platform: *V8Platform) void;
extern fn v8__V8__Initialize() void;
extern fn v8__V8__Dispose() bool;
extern fn v8__V8__DisposePlatform() void;

pub fn main() !void {
    // Step 1: Get V8 version string (doesn't require initialization)
    const version = v8__V8__GetVersion();
    const version_str = std.mem.span(version);
    std.debug.print("[v8-test] V8 version: {s}\n", .{version_str});

    // Step 2: Create platform and initialize V8
    const platform = v8__Platform__NewDefaultPlatform(0, false) orelse {
        std.debug.print("[v8-test] ERROR: Failed to create V8 platform\n", .{});
        return error.V8PlatformFailed;
    };
    std.debug.print("[v8-test] Platform created\n", .{});

    v8__V8__InitializePlatform(platform);
    std.debug.print("[v8-test] Platform initialized\n", .{});

    v8__V8__Initialize();
    std.debug.print("[v8-test] V8 initialized\n", .{});

    // Step 3: Cleanup
    _ = v8__V8__Dispose();
    v8__V8__DisposePlatform();
    std.debug.print("[v8-test] V8 disposed\n", .{});

    std.debug.print("[v8-test] SUCCESS: V8 linking verified!\n", .{});
}
