const std = @import("std");
const Engine = @import("main.zig").Engine;
const runner = @import("runner.zig");

pub const ResultCollector = struct {
    allocator: std.mem.Allocator,
    passed: u64,
    failed: u64,
    skipped: u64,
    timeouts: u64,
    errors: u64,
    total_duration_ms: i64,
    failures: std.ArrayListUnmanaged(Failure),

    pub const Failure = struct {
        path: []const u8,
        status: runner.TestResult.Status,
        error_msg: ?[]const u8,
    };

    pub fn init(allocator: std.mem.Allocator) ResultCollector {
        return .{
            .allocator = allocator,
            .passed = 0,
            .failed = 0,
            .skipped = 0,
            .timeouts = 0,
            .errors = 0,
            .total_duration_ms = 0,
            .failures = .{},
        };
    }

    pub fn deinit(self: *ResultCollector) void {
        for (self.failures.items) |f| {
            self.allocator.free(f.path);
            if (f.error_msg) |msg| self.allocator.free(msg);
        }
        self.failures.deinit(self.allocator);
    }

    pub fn addResult(self: *ResultCollector, path: []const u8, result: runner.TestResult) void {
        self.total_duration_ms += result.duration_ms;

        switch (result.status) {
            .pass => self.passed += 1,
            .fail => {
                self.failed += 1;
                self.recordFailure(path, result) catch {};
            },
            .skip => self.skipped += 1,
            .timeout => {
                self.timeouts += 1;
                self.recordFailure(path, result) catch {};
            },
            .error_ => {
                self.errors += 1;
                self.recordFailure(path, result) catch {};
            },
        }
    }

    pub fn addError(self: *ResultCollector, path: []const u8, err: anyerror) void {
        self.errors += 1;
        const failure = Failure{
            .path = self.allocator.dupe(u8, path) catch return,
            .status = .error_,
            .error_msg = std.fmt.allocPrint(self.allocator, "{}", .{err}) catch null,
        };
        self.failures.append(self.allocator, failure) catch {};
    }

    fn recordFailure(self: *ResultCollector, path: []const u8, result: runner.TestResult) !void {
        // Only record first 100 failures to avoid memory issues
        if (self.failures.items.len >= 100) return;

        const failure = Failure{
            .path = try self.allocator.dupe(u8, path),
            .status = result.status,
            .error_msg = if (result.error_msg) |msg| try self.allocator.dupe(u8, msg) else null,
        };
        try self.failures.append(self.allocator, failure);
    }

    pub fn printSummary(self: *ResultCollector, engine: Engine, total_time_ms: i64) void {
        const total = self.passed + self.failed + self.skipped + self.timeouts + self.errors;
        const pass_rate: f64 = if (total > 0)
            @as(f64, @floatFromInt(self.passed)) / @as(f64, @floatFromInt(total)) * 100.0
        else
            0.0;

        std.debug.print("\n", .{});
        std.debug.print("# Test262 Results - {s}\n", .{engine.getCommand()});
        std.debug.print("\n", .{});
        std.debug.print("passed: {d}\n", .{self.passed});
        std.debug.print("failed: {d}\n", .{self.failed});
        std.debug.print("skipped: {d}\n", .{self.skipped});
        std.debug.print("timeout: {d}\n", .{self.timeouts});
        std.debug.print("errors: {d}\n", .{self.errors});
        std.debug.print("total: {d}\n", .{total});
        std.debug.print("pass_rate: {d:.1}%\n", .{pass_rate});
        std.debug.print("time: {d}ms\n", .{total_time_ms});

        // Print first few failures
        if (self.failures.items.len > 0) {
            std.debug.print("\n", .{});
            std.debug.print("# First {d} failures:\n", .{@min(self.failures.items.len, 10)});
            for (self.failures.items[0..@min(self.failures.items.len, 10)]) |f| {
                const status_str = switch (f.status) {
                    .fail => "FAIL",
                    .timeout => "TIMEOUT",
                    .error_ => "ERROR",
                    else => "?",
                };
                std.debug.print("  [{s}] {s}\n", .{ status_str, f.path });
                if (f.error_msg) |msg| {
                    // Print first line of error
                    var lines = std.mem.splitScalar(u8, msg, '\n');
                    if (lines.next()) |first_line| {
                        const truncated = if (first_line.len > 80) first_line[0..80] else first_line;
                        std.debug.print("         {s}\n", .{truncated});
                    }
                }
            }
        }
    }

    pub fn printJson(self: *ResultCollector) void {
        const total = self.passed + self.failed + self.skipped + self.timeouts + self.errors;

        std.debug.print("{{", .{});
        std.debug.print("\"passed\":{d},", .{self.passed});
        std.debug.print("\"failed\":{d},", .{self.failed});
        std.debug.print("\"skipped\":{d},", .{self.skipped});
        std.debug.print("\"timeout\":{d},", .{self.timeouts});
        std.debug.print("\"errors\":{d},", .{self.errors});
        std.debug.print("\"total\":{d},", .{total});
        std.debug.print("\"duration_ms\":{d},", .{self.total_duration_ms});

        std.debug.print("\"failures\":[", .{});
        for (self.failures.items, 0..) |f, i| {
            if (i > 0) std.debug.print(",", .{});
            std.debug.print("{{\"path\":\"{s}\",\"status\":\"{s}\"", .{
                f.path,
                switch (f.status) {
                    .fail => "fail",
                    .timeout => "timeout",
                    .error_ => "error",
                    else => "unknown",
                },
            });
            if (f.error_msg) |msg| {
                // Escape JSON string
                std.debug.print(",\"error\":\"", .{});
                for (msg) |c| {
                    switch (c) {
                        '"' => std.debug.print("\\\"", .{}),
                        '\\' => std.debug.print("\\\\", .{}),
                        '\n' => std.debug.print("\\n", .{}),
                        '\r' => std.debug.print("\\r", .{}),
                        '\t' => std.debug.print("\\t", .{}),
                        else => std.debug.print("{c}", .{c}),
                    }
                }
                std.debug.print("\"", .{});
            }
            std.debug.print("}}", .{});
        }
        std.debug.print("]}}\n", .{});
    }
};
