const std = @import("std");

/// Parser for test262.conf style config files
pub const Config = struct {
    allocator: std.mem.Allocator,
    test_dir: ?[]const u8 = null,
    harness_dir: ?[]const u8 = null,
    error_file: ?[]const u8 = null,
    features: std.StringHashMap(FeatureState),
    excludes: std.ArrayList([]const u8),

    pub const FeatureState = enum {
        enabled,
        skip,
    };

    pub fn init(allocator: std.mem.Allocator) Config {
        return .{
            .allocator = allocator,
            .features = std.StringHashMap(FeatureState).init(allocator),
            .excludes = .{},
        };
    }

    pub fn deinit(self: *Config) void {
        if (self.test_dir) |d| self.allocator.free(d);
        if (self.harness_dir) |d| self.allocator.free(d);
        if (self.error_file) |d| self.allocator.free(d);

        var feat_iter = self.features.keyIterator();
        while (feat_iter.next()) |key| {
            self.allocator.free(key.*);
        }
        self.features.deinit();

        for (self.excludes.items) |exc| {
            self.allocator.free(exc);
        }
        self.excludes.deinit(self.allocator);
    }

    pub fn load(allocator: std.mem.Allocator, path: []const u8) !Config {
        var cfg = Config.init(allocator);
        errdefer cfg.deinit();

        const file = try std.fs.cwd().openFile(path, .{});
        defer file.close();

        const content = try file.readToEndAlloc(allocator, 10 * 1024 * 1024);
        defer allocator.free(content);

        var section: Section = .none;
        var lines = std.mem.splitScalar(u8, content, '\n');

        while (lines.next()) |line| {
            const trimmed = std.mem.trim(u8, line, " \t\r");

            // Skip empty lines and comments
            if (trimmed.len == 0 or trimmed[0] == '#') continue;

            // Section headers
            if (trimmed[0] == '[') {
                if (std.mem.eql(u8, trimmed, "[config]")) {
                    section = .config;
                } else if (std.mem.eql(u8, trimmed, "[features]")) {
                    section = .features;
                } else if (std.mem.eql(u8, trimmed, "[exclude]")) {
                    section = .exclude;
                } else if (std.mem.eql(u8, trimmed, "[tests]")) {
                    section = .tests;
                } else {
                    section = .none;
                }
                continue;
            }

            switch (section) {
                .config => try parseConfigLine(&cfg, trimmed),
                .features => try parseFeatureLine(&cfg, trimmed),
                .exclude => try parseExcludeLine(&cfg, trimmed),
                .tests, .none => {},
            }
        }

        return cfg;
    }

    const Section = enum {
        none,
        config,
        features,
        exclude,
        tests,
    };

    fn parseConfigLine(cfg: *Config, line: []const u8) !void {
        if (std.mem.indexOf(u8, line, "=")) |eq_pos| {
            const key = std.mem.trim(u8, line[0..eq_pos], " \t");
            const value = std.mem.trim(u8, line[eq_pos + 1 ..], " \t");

            if (std.mem.eql(u8, key, "testdir")) {
                cfg.test_dir = try cfg.allocator.dupe(u8, value);
            } else if (std.mem.eql(u8, key, "harnessdir")) {
                cfg.harness_dir = try cfg.allocator.dupe(u8, value);
            } else if (std.mem.eql(u8, key, "errorfile")) {
                cfg.error_file = try cfg.allocator.dupe(u8, value);
            }
        }
    }

    fn parseFeatureLine(cfg: *Config, line: []const u8) !void {
        // Format: "FeatureName" or "FeatureName=skip" or "FeatureName=!tcc"
        if (std.mem.indexOf(u8, line, "=")) |eq_pos| {
            const feature = std.mem.trim(u8, line[0..eq_pos], " \t");
            const state_str = std.mem.trim(u8, line[eq_pos + 1 ..], " \t");

            const state: FeatureState = if (std.mem.eql(u8, state_str, "skip"))
                .skip
            else
                .enabled;

            const feature_copy = try cfg.allocator.dupe(u8, feature);
            try cfg.features.put(feature_copy, state);
        } else {
            // Just feature name = enabled
            const feature_copy = try cfg.allocator.dupe(u8, line);
            try cfg.features.put(feature_copy, .enabled);
        }
    }

    fn parseExcludeLine(cfg: *Config, line: []const u8) !void {
        const exclude_copy = try cfg.allocator.dupe(u8, line);
        try cfg.excludes.append(cfg.allocator, exclude_copy);
    }

    pub fn isExcluded(self: *const Config, path: []const u8) bool {
        for (self.excludes.items) |exc| {
            if (std.mem.indexOf(u8, path, exc) != null) {
                return true;
            }
        }
        return false;
    }

    pub fn isFeatureEnabled(self: *const Config, feature: []const u8) bool {
        if (self.features.get(feature)) |state| {
            return state == .enabled;
        }
        // Unknown features are enabled by default
        return true;
    }

    pub fn isFeatureSkipped(self: *const Config, feature: []const u8) bool {
        if (self.features.get(feature)) |state| {
            return state == .skip;
        }
        return false;
    }
};

test "config parser" {
    const allocator = std.testing.allocator;

    const test_config =
        \\[config]
        \\testdir=test262/test
        \\harnessdir=test262/harness
        \\
        \\[features]
        \\BigInt
        \\async-functions
        \\Temporal=skip
        \\
        \\[exclude]
        \\test262/test/intl402/
        \\test262/test/staging/
    ;

    // Write temp config file
    const tmp_path = "/tmp/test262_config_test.conf";
    {
        const file = try std.fs.cwd().createFile(tmp_path, .{});
        defer file.close();
        try file.writeAll(test_config);
    }
    defer std.fs.cwd().deleteFile(tmp_path) catch {};

    var cfg = try Config.load(allocator, tmp_path);
    defer cfg.deinit();

    try std.testing.expectEqualStrings("test262/test", cfg.test_dir.?);
    try std.testing.expectEqualStrings("test262/harness", cfg.harness_dir.?);
    try std.testing.expect(cfg.isFeatureEnabled("BigInt"));
    try std.testing.expect(cfg.isFeatureSkipped("Temporal"));
    try std.testing.expect(cfg.isExcluded("test262/test/intl402/foo.js"));
    try std.testing.expect(!cfg.isExcluded("test262/test/language/foo.js"));
}
