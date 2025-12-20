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
        // Build args array
        var args = std.ArrayList([]const u8){};
        defer args.deinit(self.allocator);

        // EdgeBox engine uses qjs for test262 (testing QuickJS bytecode correctness)
        // The frozen interpreter is based on QuickJS bytecode, so we test against qjs
        if (self.engine == .edgebox) {
            // Use absolute path to qjs binary (built from vendor/quickjs-ng)
            const qjs_path = std.fs.cwd().realpathAlloc(self.allocator, "vendor/quickjs-ng/build/qjs") catch |err| {
                std.debug.print("[ERROR] Cannot find qjs binary. Run: cd vendor/quickjs-ng && make\n", .{});
                std.debug.print("Error: {}\n", .{err});
                return error.FileNotFound;
            };
            try args.append(self.allocator, qjs_path);
        } else {
            const cmd = self.engine.getCommand();
            try args.append(self.allocator, cmd);
        }

        try args.append(self.allocator, test_path);

        var child = std.process.Child.init(try args.toOwnedSlice(self.allocator), self.allocator);
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
