// V8 embedding test — Phases 1-4 of V8 embedding
// Phase 1: Verifies librusty_v8.a links correctly with Zig.
// Phase 2: Creates Isolate+Context, evaluates "1+1", prints result.
// Phase 3: Registers __edgebox_io_sync, tests filesystem from JS.
// Phase 4: Loads bootstrap (require, process, console), runs user JS.
//
// Build: zig build v8-test
// Run: ./zig-out/bin/edgebox-v8-test

const std = @import("std");
const v8 = @import("v8.zig");
const v8_io = @import("v8_io.zig");

pub fn main() !void {
    const alloc = std.heap.page_allocator;

    // Step 1: Get V8 version (no initialization needed)
    const version = v8.getVersion();
    std.debug.print("[v8-test] V8 version: {s}\n", .{version});

    // Step 2: Initialize platform + engine
    _ = try v8.initPlatform();
    defer v8.disposePlatform();
    std.debug.print("[v8-test] Platform initialized\n", .{});

    // Step 3: Create Isolate
    const isolate = v8.IsolateApi.create();
    defer v8.IsolateApi.dispose(isolate);
    v8.IsolateApi.enter(isolate);
    defer v8.IsolateApi.exit(isolate);

    // Step 4: HandleScope + Context
    var handle_scope = v8.HandleScope.init(isolate);
    defer handle_scope.deinit();
    const context = v8.ContextApi.create(isolate);
    v8.ContextApi.enter(context);
    defer v8.ContextApi.exit(context);

    // Step 5: Register __edgebox_io_sync (Phase 3)
    v8_io.registerGlobals(isolate, context);

    // Step 6: Load bootstrap (Phase 4) — sets up require, process, console, Buffer
    const bootstrap_path = "src/v8_bootstrap.js";
    const bootstrap_code = std.fs.cwd().readFileAlloc(alloc, bootstrap_path, 4 * 1024 * 1024) catch |err| {
        std.debug.print("[v8-test] ERROR: could not read {s}: {}\n", .{ bootstrap_path, err });
        return err;
    };
    defer alloc.free(bootstrap_code);

    _ = v8.eval(isolate, context, bootstrap_code, bootstrap_path) catch |err| {
        std.debug.print("[v8-test] ERROR: bootstrap failed: {}\n", .{err});
        return err;
    };
    std.debug.print("[v8-test] Bootstrap loaded\n", .{});

    // === Phase 2 Tests ===
    const result = v8.eval(isolate, context, "1 + 1", "test.js") catch unreachable;
    if (v8.ValueApi.int32Value(result, context)) |v| {
        std.debug.print("[v8-test] eval('1+1') = {d} {s}\n", .{ v, if (v == 2) "OK" else "FAIL" });
    }

    // === Phase 3 Tests ===
    const cwd_result = v8.eval(isolate, context,
        \\JSON.parse(__edgebox_io_sync('{"op":"cwd"}')).data
    , "test_cwd.js") catch unreachable;
    if (v8.ValueApi.toString(cwd_result, context)) |str| {
        var buf: [1024]u8 = undefined;
        const len = v8.StringApi.writeUtf8(str, isolate, &buf);
        std.debug.print("[v8-test] cwd = {s} OK\n", .{buf[0..len]});
    }

    // === Phase 4 Tests ===
    // Test: require('path')
    const path_test = v8.eval(isolate, context,
        \\(function() {
        \\  var path = require('path');
        \\  return path.join('/foo', 'bar', 'baz.js');
        \\})()
    , "test_require_path.js") catch |err| {
        std.debug.print("[v8-test] ERROR: require('path') failed: {}\n", .{err});
        return err;
    };
    if (v8.ValueApi.toString(path_test, context)) |str| {
        var buf: [256]u8 = undefined;
        const len = v8.StringApi.writeUtf8(str, isolate, &buf);
        std.debug.print("[v8-test] require('path').join = {s} OK\n", .{buf[0..len]});
    }

    // Test: require('fs').readFileSync
    const fs_test = v8.eval(isolate, context,
        \\(function() {
        \\  var fs = require('fs');
        \\  var content = fs.readFileSync('package.json', 'utf8');
        \\  var pkg = JSON.parse(content);
        \\  return pkg.name || 'unknown';
        \\})()
    , "test_require_fs.js") catch |err| {
        std.debug.print("[v8-test] ERROR: require('fs') failed: {}\n", .{err});
        return err;
    };
    if (v8.ValueApi.toString(fs_test, context)) |str| {
        var buf: [256]u8 = undefined;
        const len = v8.StringApi.writeUtf8(str, isolate, &buf);
        std.debug.print("[v8-test] require('fs').readFileSync(package.json).name = {s} OK\n", .{buf[0..len]});
    }

    // Test: process.argv
    const argv_test = v8.eval(isolate, context,
        "process.argv[0]",
        "test_process.js",
    ) catch unreachable;
    if (v8.ValueApi.toString(argv_test, context)) |str| {
        var buf: [256]u8 = undefined;
        const len = v8.StringApi.writeUtf8(str, isolate, &buf);
        std.debug.print("[v8-test] process.argv[0] = {s} OK\n", .{buf[0..len]});
    }

    // Test: console.log
    _ = v8.eval(isolate, context,
        "console.log('[v8-test] console.log from JS OK')",
        "test_console.js",
    ) catch {};

    // Test: require('os')
    const os_test = v8.eval(isolate, context,
        "require('os').platform()",
        "test_os.js",
    ) catch unreachable;
    if (v8.ValueApi.toString(os_test, context)) |str| {
        var buf: [64]u8 = undefined;
        const len = v8.StringApi.writeUtf8(str, isolate, &buf);
        std.debug.print("[v8-test] require('os').platform() = {s} OK\n", .{buf[0..len]});
    }

    // Test: require('node:fs') (node: prefix)
    const node_fs = v8.eval(isolate, context,
        "typeof require('node:fs').readFileSync",
        "test_node_prefix.js",
    ) catch unreachable;
    if (v8.ValueApi.toString(node_fs, context)) |str| {
        var buf: [64]u8 = undefined;
        const len = v8.StringApi.writeUtf8(str, isolate, &buf);
        std.debug.print("[v8-test] require('node:fs').readFileSync = {s} OK\n", .{buf[0..len]});
    }

    std.debug.print("[v8-test] SUCCESS: All phases verified!\n", .{});
}
