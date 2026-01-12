const std = @import("std");
const Engine = @import("main.zig").Engine;
const harness = @import("harness.zig");
const test_parser = @import("test_parser.zig");

pub const TestResult = struct {
    status: Status,
    duration_ms: i64,
    output: ?[]const u8 = null,
    error_msg: ?[]const u8 = null,

    pub const Status = enum {
        pass,
        fail,
        skip,
        timeout,
        error_,
    };
};

pub const Runner = struct {
    allocator: std.mem.Allocator,
    engine: Engine,
    harness_loader: *harness.HarnessLoader,
    timeout_ms: u32,
    temp_dir: []const u8,
    temp_counter: u64,

    pub fn init(
        allocator: std.mem.Allocator,
        engine: Engine,
        harness_loader: *harness.HarnessLoader,
        timeout_ms: u32,
    ) Runner {
        return .{
            .allocator = allocator,
            .engine = engine,
            .harness_loader = harness_loader,
            .timeout_ms = timeout_ms,
            .temp_dir = "/tmp/test262",
            .temp_counter = 0,
        };
    }

    pub fn deinit(self: *Runner) void {
        _ = self;
    }

    pub fn runTest(self: *Runner, test_path: []const u8) !TestResult {
        const start_time = std.time.milliTimestamp();

        // Read test file
        const test_content = blk: {
            const file = std.fs.cwd().openFile(test_path, .{}) catch |err| {
                return TestResult{
                    .status = .error_,
                    .duration_ms = std.time.milliTimestamp() - start_time,
                    .error_msg = try std.fmt.allocPrint(self.allocator, "Failed to open test file: {}", .{err}),
                };
            };
            defer file.close();
            break :blk try file.readToEndAlloc(self.allocator, 10 * 1024 * 1024);
        };
        defer self.allocator.free(test_content);

        // Parse test metadata
        var metadata = try test_parser.TestMetadata.parse(self.allocator, test_content);
        defer metadata.deinit();

        // Check if test should be skipped
        if (metadata.flags.module) {
            // Module tests require special handling - skip for now
            return TestResult{
                .status = .skip,
                .duration_ms = std.time.milliTimestamp() - start_time,
            };
        }

        // Determine includes (always include sta.js and assert.js)
        var includes: std.ArrayListUnmanaged([]const u8) = .{};
        defer includes.deinit(self.allocator);

        try includes.append(self.allocator, "sta.js");
        try includes.append(self.allocator, "assert.js");

        for (metadata.includes.items) |inc| {
            try includes.append(self.allocator, inc);
        }

        // Handle async tests
        if (metadata.flags.async_) {
            try includes.append(self.allocator, "doneprintHandle.js");
        }

        // Prepare test code
        const use_strict = metadata.flags.only_strict and !metadata.flags.no_strict;
        const prepared_code = try harness.prepareTest(
            self.allocator,
            self.harness_loader,
            test_content,
            includes.items,
            use_strict,
        );
        defer self.allocator.free(prepared_code);

        // Write to temp file
        const temp_path = try self.writeTempFile(prepared_code);
        defer {
            std.fs.cwd().deleteFile(temp_path) catch {};
            self.allocator.free(temp_path);
        }

        // Run the engine
        const run_result = try self.executeEngine(temp_path, metadata.flags.async_);
        const end_time = std.time.milliTimestamp();

        // Determine result
        const status = self.determineStatus(run_result, &metadata);

        return TestResult{
            .status = status,
            .duration_ms = end_time - start_time,
            .output = run_result.stdout,
            .error_msg = run_result.stderr,
        };
    }

    fn writeTempFile(self: *Runner, content: []const u8) ![]const u8 {
        // Ensure temp directory exists
        std.fs.cwd().makePath(self.temp_dir) catch {};

        // Generate unique filename
        self.temp_counter += 1;
        const filename = try std.fmt.allocPrint(
            self.allocator,
            "{s}/test_{d}_{d}.js",
            .{ self.temp_dir, std.time.milliTimestamp(), self.temp_counter },
        );

        // Write file
        const file = try std.fs.cwd().createFile(filename, .{});
        defer file.close();
        try file.writeAll(content);

        return filename;
    }

    const ExecuteResult = struct {
        exit_code: u8,
        stdout: ?[]const u8,
        stderr: ?[]const u8,
        timed_out: bool,
    };

    fn executeEngine(self: *Runner, test_path: []const u8, is_async: bool) !ExecuteResult {
        // EdgeBox: compile to native binary, then run it
        if (self.engine == .edgebox) {
            return self.executeEdgebox(test_path, is_async);
        }

        // Other engines: run directly
        const cmd = self.engine.getCommand();
        var args = [_][]const u8{ cmd, test_path };

        var child = std.process.Child.init(&args, self.allocator);
        child.stdout_behavior = .Pipe;
        child.stderr_behavior = .Pipe;

        try child.spawn();

        // Set up timeout
        const timeout_ns: u64 = @as(u64, self.timeout_ms) * std.time.ns_per_ms;
        const deadline = std.time.nanoTimestamp() + @as(i128, timeout_ns);

        // Wait for completion with timeout
        var stdout_buf: std.ArrayListUnmanaged(u8) = .{};
        defer stdout_buf.deinit(self.allocator);
        var stderr_buf: std.ArrayListUnmanaged(u8) = .{};
        defer stderr_buf.deinit(self.allocator);

        // Read output
        if (child.stdout) |stdout| {
            while (true) {
                if (std.time.nanoTimestamp() > deadline) {
                    _ = child.kill() catch {};
                    return ExecuteResult{
                        .exit_code = 1,
                        .stdout = null,
                        .stderr = null,
                        .timed_out = true,
                    };
                }

                var buf: [4096]u8 = undefined;
                const n = stdout.read(&buf) catch break;
                if (n == 0) break;
                try stdout_buf.appendSlice(self.allocator, buf[0..n]);
            }
        }

        if (child.stderr) |stderr| {
            var buf: [4096]u8 = undefined;
            const n = stderr.read(&buf) catch 0;
            if (n > 0) {
                try stderr_buf.appendSlice(self.allocator, buf[0..n]);
            }
        }

        const term = try child.wait();
        const exit_code: u8 = switch (term) {
            .Exited => |code| code,
            .Signal => 1,
            .Stopped => 1,
            .Unknown => 1,
        };

        // For async tests, check for completion marker
        if (is_async) {
            const stdout_str = stdout_buf.items;
            if (std.mem.indexOf(u8, stdout_str, "Test262:AsyncTestComplete") == null and
                std.mem.indexOf(u8, stdout_str, "Test262:AsyncTestFailure") == null)
            {
                // Async test didn't complete properly
                return ExecuteResult{
                    .exit_code = 1,
                    .stdout = try self.allocator.dupe(u8, stdout_buf.items),
                    .stderr = try self.allocator.dupe(u8, stderr_buf.items),
                    .timed_out = false,
                };
            }
        }

        return ExecuteResult{
            .exit_code = exit_code,
            .stdout = if (stdout_buf.items.len > 0) try self.allocator.dupe(u8, stdout_buf.items) else null,
            .stderr = if (stderr_buf.items.len > 0) try self.allocator.dupe(u8, stderr_buf.items) else null,
            .timed_out = false,
        };
    }

    fn executeEdgebox(self: *Runner, test_path: []const u8, is_async: bool) !ExecuteResult {
        _ = is_async;

        // Step 1: Compile with edgeboxc build
        // Use relative path to ensure we find the binary in zig-out/bin/
        var compile_args = [_][]const u8{ "./zig-out/bin/edgeboxc", "build", test_path };
        var compile_child = std.process.Child.init(&compile_args, self.allocator);
        compile_child.stdout_behavior = .Pipe;
        compile_child.stderr_behavior = .Pipe;

        try compile_child.spawn();
        const compile_term = try compile_child.wait();

        // Check compilation result
        const compile_exit: u8 = switch (compile_term) {
            .Exited => |code| code,
            else => 1,
        };

        if (compile_exit != 0) {
            // Compilation failed - read stderr for error message
            var stderr_buf: [4096]u8 = undefined;
            var stderr_len: usize = 0;
            if (compile_child.stderr) |stderr| {
                stderr_len = stderr.read(&stderr_buf) catch 0;
            }
            return ExecuteResult{
                .exit_code = compile_exit,
                .stdout = null,
                .stderr = if (stderr_len > 0) try self.allocator.dupe(u8, stderr_buf[0..stderr_len]) else null,
                .timed_out = false,
            };
        }

        // Step 2: Find the native binary
        // edgeboxc outputs to zig-out/bin/tmp/<path>/<name> (no extension)
        // test_path is like /tmp/test262/test_123_1.js
        // So binary would be at zig-out/bin/tmp/test262/test_123_1.js/test_123_1
        const basename = std.fs.path.stem(test_path);
        const native_path = try std.fmt.allocPrint(
            self.allocator,
            "zig-out/bin{s}/{s}",
            .{ test_path, basename },
        );
        defer self.allocator.free(native_path);

        // Check if binary exists
        std.fs.cwd().access(native_path, .{}) catch {
            return ExecuteResult{
                .exit_code = 1,
                .stdout = null,
                .stderr = try self.allocator.dupe(u8, "Native binary not found after compilation"),
                .timed_out = false,
            };
        };

        // Step 3: Run the native binary
        var run_args = [_][]const u8{native_path};
        var run_child = std.process.Child.init(&run_args, self.allocator);
        run_child.stdout_behavior = .Pipe;
        run_child.stderr_behavior = .Pipe;

        try run_child.spawn();

        // Set up timeout
        const timeout_ns: u64 = @as(u64, self.timeout_ms) * std.time.ns_per_ms;
        const deadline = std.time.nanoTimestamp() + @as(i128, timeout_ns);

        // Read output with timeout
        var stdout_buf: std.ArrayListUnmanaged(u8) = .{};
        defer stdout_buf.deinit(self.allocator);
        var stderr_buf: std.ArrayListUnmanaged(u8) = .{};
        defer stderr_buf.deinit(self.allocator);

        if (run_child.stdout) |stdout| {
            while (true) {
                if (std.time.nanoTimestamp() > deadline) {
                    _ = run_child.kill() catch {};
                    // Cleanup compiled files
                    self.cleanupEdgeboxBuild(test_path);
                    return ExecuteResult{
                        .exit_code = 1,
                        .stdout = null,
                        .stderr = null,
                        .timed_out = true,
                    };
                }

                var buf: [4096]u8 = undefined;
                const n = stdout.read(&buf) catch break;
                if (n == 0) break;
                try stdout_buf.appendSlice(self.allocator, buf[0..n]);
            }
        }

        if (run_child.stderr) |stderr| {
            var buf: [4096]u8 = undefined;
            const n = stderr.read(&buf) catch 0;
            if (n > 0) {
                try stderr_buf.appendSlice(self.allocator, buf[0..n]);
            }
        }

        const term = try run_child.wait();
        const exit_code: u8 = switch (term) {
            .Exited => |code| code,
            else => 1,
        };

        // Step 4: Cleanup compiled files
        self.cleanupEdgeboxBuild(test_path);

        return ExecuteResult{
            .exit_code = exit_code,
            .stdout = if (stdout_buf.items.len > 0) try self.allocator.dupe(u8, stdout_buf.items) else null,
            .stderr = if (stderr_buf.items.len > 0) try self.allocator.dupe(u8, stderr_buf.items) else null,
            .timed_out = false,
        };
    }

    fn cleanupEdgeboxBuild(self: *Runner, test_path: []const u8) void {
        // Clean up zig-out/bin/<test_path>/ directory
        const build_dir = std.fmt.allocPrint(
            self.allocator,
            "zig-out/bin{s}",
            .{test_path},
        ) catch return;
        defer self.allocator.free(build_dir);

        // Delete the build directory recursively
        std.fs.cwd().deleteTree(build_dir) catch {};
    }

    fn determineStatus(self: *Runner, result: ExecuteResult, metadata: *const test_parser.TestMetadata) TestResult.Status {
        _ = self;

        if (result.timed_out) {
            return .timeout;
        }

        // Check for negative tests (expecting an error)
        if (metadata.negative) |negative| {
            // Test expects an error
            if (result.exit_code != 0) {
                // Check if the error type matches
                if (result.stderr) |stderr| {
                    if (std.mem.indexOf(u8, stderr, negative.type_) != null) {
                        return .pass; // Got expected error
                    }
                }
                if (result.stdout) |stdout| {
                    if (std.mem.indexOf(u8, stdout, negative.type_) != null) {
                        return .pass; // Got expected error
                    }
                }
                // Got an error but not the expected type
                return .fail;
            } else {
                // Expected error but test passed
                return .fail;
            }
        }

        // Normal test - success if exit code is 0
        if (result.exit_code == 0) {
            // Check for async test failure marker
            if (result.stdout) |stdout| {
                if (std.mem.indexOf(u8, stdout, "Test262:AsyncTestFailure") != null) {
                    return .fail;
                }
            }
            return .pass;
        }

        return .fail;
    }
};
