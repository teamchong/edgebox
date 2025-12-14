const std = @import("std");

/// Parser for test262 YAML frontmatter
/// Format:
/// /*---
/// description: Test description
/// flags: [onlyStrict, async]
/// includes: [sta.js, assert.js]
/// features: [BigInt, Promise]
/// negative:
///   phase: parse
///   type: SyntaxError
/// ---*/
pub const TestMetadata = struct {
    allocator: std.mem.Allocator,
    description: ?[]const u8 = null,
    flags: Flags = .{},
    includes: std.ArrayListUnmanaged([]const u8),
    features: std.ArrayListUnmanaged([]const u8),
    negative: ?Negative = null,
    raw_content: []const u8 = "",

    pub const Flags = struct {
        only_strict: bool = false,
        no_strict: bool = false,
        module: bool = false,
        async_: bool = false,
        raw: bool = false,
        can_block_is_false: bool = false,
    };

    pub const Negative = struct {
        phase: Phase = .runtime,
        type_: []const u8 = "",

        pub const Phase = enum {
            parse,
            resolution,
            runtime,
        };
    };

    pub fn init(allocator: std.mem.Allocator) TestMetadata {
        return .{
            .allocator = allocator,
            .includes = .{},
            .features = .{},
        };
    }

    pub fn deinit(self: *TestMetadata) void {
        if (self.description) |d| self.allocator.free(d);
        for (self.includes.items) |inc| self.allocator.free(inc);
        self.includes.deinit(self.allocator);
        for (self.features.items) |feat| self.allocator.free(feat);
        self.features.deinit(self.allocator);
        if (self.negative) |n| {
            if (n.type_.len > 0) self.allocator.free(n.type_);
        }
    }

    pub fn parse(allocator: std.mem.Allocator, content: []const u8) !TestMetadata {
        var meta = TestMetadata.init(allocator);
        errdefer meta.deinit();

        // Find frontmatter boundaries
        const start_marker = "/*---";
        const end_marker = "---*/";

        const start_pos = std.mem.indexOf(u8, content, start_marker) orelse {
            // No frontmatter, return empty metadata
            meta.raw_content = content;
            return meta;
        };

        const yaml_start = start_pos + start_marker.len;
        const end_pos = std.mem.indexOf(u8, content[yaml_start..], end_marker) orelse {
            meta.raw_content = content;
            return meta;
        };

        const yaml_content = content[yaml_start .. yaml_start + end_pos];
        meta.raw_content = content[yaml_start + end_pos + end_marker.len ..];

        // Parse YAML-like content line by line
        var lines = std.mem.splitScalar(u8, yaml_content, '\n');
        var current_key: ?[]const u8 = null;

        while (lines.next()) |line| {
            const trimmed = std.mem.trim(u8, line, " \t\r");
            if (trimmed.len == 0) continue;

            // Check for key: value
            if (std.mem.indexOf(u8, trimmed, ":")) |colon_pos| {
                const key = std.mem.trim(u8, trimmed[0..colon_pos], " \t");
                const value = std.mem.trim(u8, trimmed[colon_pos + 1 ..], " \t");

                if (std.mem.eql(u8, key, "description")) {
                    meta.description = try allocator.dupe(u8, value);
                } else if (std.mem.eql(u8, key, "flags")) {
                    try parseFlags(&meta, value);
                } else if (std.mem.eql(u8, key, "includes")) {
                    try parseList(allocator, &meta.includes, value);
                } else if (std.mem.eql(u8, key, "features")) {
                    try parseList(allocator, &meta.features, value);
                } else if (std.mem.eql(u8, key, "negative")) {
                    meta.negative = .{};
                    current_key = "negative";
                } else if (std.mem.eql(u8, key, "type") and current_key != null and std.mem.eql(u8, current_key.?, "negative")) {
                    if (meta.negative) |*n| {
                        n.type_ = try allocator.dupe(u8, value);
                    }
                } else if (std.mem.eql(u8, key, "phase") and current_key != null and std.mem.eql(u8, current_key.?, "negative")) {
                    if (meta.negative) |*n| {
                        if (std.mem.eql(u8, value, "parse")) {
                            n.phase = .parse;
                        } else if (std.mem.eql(u8, value, "resolution")) {
                            n.phase = .resolution;
                        } else {
                            n.phase = .runtime;
                        }
                    }
                }
            }
        }

        return meta;
    }

    fn parseFlags(meta: *TestMetadata, value: []const u8) !void {
        // Format: [flag1, flag2, flag3] or just flag1
        var content = value;
        if (std.mem.startsWith(u8, content, "[")) {
            content = content[1..];
        }
        if (std.mem.endsWith(u8, content, "]")) {
            content = content[0 .. content.len - 1];
        }

        var flags = std.mem.splitScalar(u8, content, ',');
        while (flags.next()) |flag_raw| {
            const flag = std.mem.trim(u8, flag_raw, " \t");
            if (std.mem.eql(u8, flag, "onlyStrict")) {
                meta.flags.only_strict = true;
            } else if (std.mem.eql(u8, flag, "noStrict")) {
                meta.flags.no_strict = true;
            } else if (std.mem.eql(u8, flag, "module")) {
                meta.flags.module = true;
            } else if (std.mem.eql(u8, flag, "async")) {
                meta.flags.async_ = true;
            } else if (std.mem.eql(u8, flag, "raw")) {
                meta.flags.raw = true;
            } else if (std.mem.eql(u8, flag, "CanBlockIsFalse")) {
                meta.flags.can_block_is_false = true;
            }
        }
    }

    fn parseList(allocator: std.mem.Allocator, list: *std.ArrayListUnmanaged([]const u8), value: []const u8) !void {
        // Format: [item1, item2, item3] or just item1
        var content = value;
        if (std.mem.startsWith(u8, content, "[")) {
            content = content[1..];
        }
        if (std.mem.endsWith(u8, content, "]")) {
            content = content[0 .. content.len - 1];
        }

        var items = std.mem.splitScalar(u8, content, ',');
        while (items.next()) |item_raw| {
            const item = std.mem.trim(u8, item_raw, " \t");
            if (item.len > 0) {
                const item_copy = try allocator.dupe(u8, item);
                try list.append(allocator, item_copy);
            }
        }
    }

    pub fn shouldSkipForFeatures(self: *const TestMetadata, config: anytype) bool {
        for (self.features.items) |feature| {
            if (config.isFeatureSkipped(feature)) {
                return true;
            }
        }
        return false;
    }
};

test "parse test metadata" {
    const allocator = std.testing.allocator;

    const test_content =
        \\/*---
        \\description: Test for Array.prototype.map
        \\flags: [onlyStrict, async]
        \\includes: [sta.js, assert.js]
        \\features: [BigInt, Promise]
        \\negative:
        \\  phase: parse
        \\  type: SyntaxError
        \\---*/
        \\var x = 1;
    ;

    var meta = try TestMetadata.parse(allocator, test_content);
    defer meta.deinit();

    try std.testing.expectEqualStrings("Test for Array.prototype.map", meta.description.?);
    try std.testing.expect(meta.flags.only_strict);
    try std.testing.expect(meta.flags.async_);
    try std.testing.expect(!meta.flags.module);
    try std.testing.expectEqual(@as(usize, 2), meta.includes.items.len);
    try std.testing.expectEqualStrings("sta.js", meta.includes.items[0]);
    try std.testing.expectEqual(@as(usize, 2), meta.features.items.len);
    try std.testing.expect(meta.negative != null);
    try std.testing.expectEqual(TestMetadata.Negative.Phase.parse, meta.negative.?.phase);
    try std.testing.expectEqualStrings("SyntaxError", meta.negative.?.type_);
}

test "parse test without frontmatter" {
    const allocator = std.testing.allocator;

    const test_content = "var x = 1;";

    var meta = try TestMetadata.parse(allocator, test_content);
    defer meta.deinit();

    try std.testing.expect(meta.description == null);
    try std.testing.expectEqual(@as(usize, 0), meta.includes.items.len);
}
