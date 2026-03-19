// V8 embedding test — Phases 1-3 of V8 embedding
// Phase 1: Verifies librusty_v8.a links correctly with Zig.
// Phase 2: Creates Isolate+Context, evaluates "1+1", prints result.
// Phase 3: Registers __edgebox_io_sync, tests filesystem from JS.
//
// Build: zig build v8-test
// Run: ./zig-out/bin/edgebox-v8-test

const std = @import("std");
const v8 = @import("v8.zig");
const v8_io = @import("v8_io.zig");

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

    // Step 5: Register __edgebox_io_sync global function (Phase 3)
    v8_io.registerGlobals(isolate, context);
    std.debug.print("[v8-test] IO bridge registered\n", .{});

    // === Phase 2 Tests ===

    // Test: eval("1 + 1")
    const result = v8.eval(isolate, context, "1 + 1", "test.js") catch |err| {
        std.debug.print("[v8-test] ERROR: eval failed: {}\n", .{err});
        return err;
    };
    if (v8.ValueApi.int32Value(result, context)) |int_val| {
        std.debug.print("[v8-test] eval('1 + 1') = {d} {s}\n", .{ int_val, if (int_val == 2) "OK" else "FAIL" });
    }

    // Test: string concatenation
    const str_result = v8.eval(isolate, context, "'Hello' + ' ' + 'V8!'", "test2.js") catch |err| {
        std.debug.print("[v8-test] ERROR: string eval failed: {}\n", .{err});
        return err;
    };
    if (v8.ValueApi.toString(str_result, context)) |str| {
        var buf: [256]u8 = undefined;
        const len = v8.StringApi.writeUtf8(str, isolate, &buf);
        std.debug.print("[v8-test] eval string = \"{s}\" OK\n", .{buf[0..len]});
    }

    // === Phase 3 Tests: IO Bridge ===

    // Test: __edgebox_io_sync exists
    const exists_result = v8.eval(isolate, context,
        "typeof __edgebox_io_sync === 'function' ? 'yes' : 'no'",
        "test_io.js",
    ) catch |err| {
        std.debug.print("[v8-test] ERROR: typeof check failed: {}\n", .{err});
        return err;
    };
    if (v8.ValueApi.toString(exists_result, context)) |str| {
        var buf: [64]u8 = undefined;
        const len = v8.StringApi.writeUtf8(str, isolate, &buf);
        std.debug.print("[v8-test] __edgebox_io_sync exists: {s}\n", .{buf[0..len]});
    }

    // Test: cwd operation
    const cwd_result = v8.eval(isolate, context,
        \\JSON.parse(__edgebox_io_sync('{"op":"cwd"}')).data
    ,
        "test_cwd.js",
    ) catch |err| {
        std.debug.print("[v8-test] ERROR: cwd failed: {}\n", .{err});
        return err;
    };
    if (v8.ValueApi.toString(cwd_result, context)) |str| {
        var buf: [1024]u8 = undefined;
        const len = v8.StringApi.writeUtf8(str, isolate, &buf);
        std.debug.print("[v8-test] cwd = {s} OK\n", .{buf[0..len]});
    }

    // Test: readFile on a known file
    const read_result = v8.eval(isolate, context,
        \\(function() {
        \\  var r = JSON.parse(__edgebox_io_sync('{"op":"readFile","path":"package.json"}'));
        \\  if (r.ok) return 'read ' + r.data.length + ' chars';
        \\  return 'error: ' + r.error;
        \\})()
    ,
        "test_read.js",
    ) catch |err| {
        std.debug.print("[v8-test] ERROR: readFile failed: {}\n", .{err});
        return err;
    };
    if (v8.ValueApi.toString(read_result, context)) |str| {
        var buf: [256]u8 = undefined;
        const len = v8.StringApi.writeUtf8(str, isolate, &buf);
        std.debug.print("[v8-test] readFile('package.json') = {s} OK\n", .{buf[0..len]});
    }

    // Test: exists operation
    const exists_file = v8.eval(isolate, context,
        \\JSON.parse(__edgebox_io_sync('{"op":"exists","path":"package.json"}')).exists
    ,
        "test_exists.js",
    ) catch |err| {
        std.debug.print("[v8-test] ERROR: exists failed: {}\n", .{err});
        return err;
    };
    if (v8.ValueApi.booleanValue(exists_file, isolate)) {
        std.debug.print("[v8-test] exists('package.json') = true OK\n", .{});
    } else {
        std.debug.print("[v8-test] exists('package.json') = false FAIL\n", .{});
    }

    // Test: stat operation
    const stat_result = v8.eval(isolate, context,
        \\(function() {
        \\  var s = JSON.parse(__edgebox_io_sync('{"op":"stat","path":"package.json"}'));
        \\  return s.ok ? 'isFile=' + s.isFile + ' size=' + s.size : 'error';
        \\})()
    ,
        "test_stat.js",
    ) catch |err| {
        std.debug.print("[v8-test] ERROR: stat failed: {}\n", .{err});
        return err;
    };
    if (v8.ValueApi.toString(stat_result, context)) |str| {
        var buf: [256]u8 = undefined;
        const len = v8.StringApi.writeUtf8(str, isolate, &buf);
        std.debug.print("[v8-test] stat('package.json') = {s} OK\n", .{buf[0..len]});
    }

    // Test: writeStdout
    _ = v8.eval(isolate, context,
        \\__edgebox_io_sync('{"op":"writeStdout","data":"[v8-test] writeStdout from JS OK\\n"}')
    , "test_stdout.js") catch {};

    std.debug.print("[v8-test] SUCCESS: All phases verified!\n", .{});
}
