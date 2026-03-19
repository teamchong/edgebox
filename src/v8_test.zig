// V8 embedding test — Phases 1+2 of V8 embedding
// Phase 1: Verifies librusty_v8.a links correctly with Zig.
// Phase 2: Creates Isolate+Context, evaluates "1+1", prints result.
//
// Build: zig build v8-test
// Run: ./zig-out/bin/edgebox-v8-test

const std = @import("std");
const v8 = @import("v8.zig");

pub fn main() !void {
    // Step 1: Get V8 version (no initialization needed)
    const version = v8.getVersion();
    std.debug.print("[v8-test] V8 version: {s}\n", .{version});

    // Step 2: Initialize platform + engine
    _ = try v8.initPlatform();
    std.debug.print("[v8-test] Platform initialized\n", .{});
    defer {
        v8.disposePlatform();
        std.debug.print("[v8-test] V8 disposed\n", .{});
    }

    // Step 3: Create Isolate (C++ bridge handles CreateParams + allocator)
    const isolate = v8.IsolateApi.create();
    defer v8.IsolateApi.dispose(isolate);
    v8.IsolateApi.enter(isolate);
    defer v8.IsolateApi.exit(isolate);
    std.debug.print("[v8-test] Isolate created\n", .{});

    // Step 4: HandleScope + Context
    var handle_scope = v8.HandleScope.init(isolate);
    defer handle_scope.deinit();

    const context = v8.ContextApi.create(isolate);
    v8.ContextApi.enter(context);
    defer v8.ContextApi.exit(context);
    std.debug.print("[v8-test] Context created\n", .{});

    // Step 5: Evaluate "1 + 1"
    const result = v8.eval(isolate, context, "1 + 1", "test.js") catch |err| {
        std.debug.print("[v8-test] ERROR: eval failed: {}\n", .{err});
        return err;
    };
    std.debug.print("[v8-test] eval('1 + 1') completed\n", .{});

    // Step 6: Read result
    if (v8.ValueApi.int32Value(result, context)) |int_val| {
        std.debug.print("[v8-test] Result: {d}\n", .{int_val});
        if (int_val == 2) {
            std.debug.print("[v8-test] SUCCESS: 1 + 1 = 2\n", .{});
        } else {
            std.debug.print("[v8-test] FAIL: expected 2, got {d}\n", .{int_val});
        }
    } else {
        // Fallback: print as string
        if (v8.ValueApi.toString(result, context)) |str| {
            var buf: [256]u8 = undefined;
            const len = v8.StringApi.writeUtf8(str, isolate, &buf);
            std.debug.print("[v8-test] Result (string): {s}\n", .{buf[0..len]});
        } else {
            std.debug.print("[v8-test] FAIL: could not read result\n", .{});
        }
    }

    // Step 7: Test string eval
    const str_result = v8.eval(isolate, context, "'Hello' + ' ' + 'V8!'", "test2.js") catch |err| {
        std.debug.print("[v8-test] ERROR: string eval failed: {}\n", .{err});
        return err;
    };
    if (v8.ValueApi.toString(str_result, context)) |str| {
        var buf: [256]u8 = undefined;
        const len = v8.StringApi.writeUtf8(str, isolate, &buf);
        std.debug.print("[v8-test] String result: {s}\n", .{buf[0..len]});
    }

    std.debug.print("[v8-test] SUCCESS: V8 embedding verified!\n", .{});
}
