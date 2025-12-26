/// Path utilities for EdgeBox configuration
///
/// Provides path expansion (~, $HOME, $PWD) and path matching utilities
/// used by the configuration system for permission checking.
const std = @import("std");

/// Expand path variables: ~, $HOME, $PWD
/// Caller owns returned memory.
pub fn expandPath(alloc: std.mem.Allocator, path: []const u8) ![]const u8 {
    // Handle ~ expansion
    if (path.len > 0 and path[0] == '~') {
        const home = std.process.getEnvVarOwned(alloc, "HOME") catch return try alloc.dupe(u8, path);
        defer alloc.free(home);
        const rest = if (path.len > 1) path[1..] else "";
        return try std.fmt.allocPrint(alloc, "{s}{s}", .{ home, rest });
    }

    // Handle $HOME expansion
    if (std.mem.eql(u8, path, "$HOME")) {
        return std.process.getEnvVarOwned(alloc, "HOME") catch try alloc.dupe(u8, path);
    }
    if (std.mem.startsWith(u8, path, "$HOME/")) {
        const home = std.process.getEnvVarOwned(alloc, "HOME") catch return try alloc.dupe(u8, path);
        defer alloc.free(home);
        const rest = path[5..]; // "$HOME" is 5 chars, keep the "/"
        return try std.fmt.allocPrint(alloc, "{s}{s}", .{ home, rest });
    }

    // Handle $PWD expansion
    if (std.mem.eql(u8, path, "$PWD")) {
        var buf: [4096]u8 = undefined;
        const cwd = std.process.getCwd(&buf) catch return try alloc.dupe(u8, path);
        return try alloc.dupe(u8, cwd);
    }
    if (std.mem.startsWith(u8, path, "$PWD/")) {
        var buf: [4096]u8 = undefined;
        const cwd = std.process.getCwd(&buf) catch return try alloc.dupe(u8, path);
        const rest = path[4..]; // "$PWD" is 4 chars, keep the "/"
        return try std.fmt.allocPrint(alloc, "{s}{s}", .{ cwd, rest });
    }

    return try alloc.dupe(u8, path);
}

/// Check if path starts with prefix (handles trailing slashes correctly)
/// Used for permission checking - e.g., does "/tmp/foo" start with "/tmp"?
pub fn pathStartsWith(path: []const u8, prefix: []const u8) bool {
    if (prefix.len == 0) return false;

    // Normalize: remove trailing slash from prefix for comparison
    const clean_prefix = if (prefix.len > 1 and prefix[prefix.len - 1] == '/')
        prefix[0 .. prefix.len - 1]
    else
        prefix;

    // Exact match
    if (std.mem.eql(u8, path, clean_prefix)) return true;

    // Path starts with prefix/
    if (path.len > clean_prefix.len and
        std.mem.startsWith(u8, path, clean_prefix) and
        path[clean_prefix.len] == '/')
    {
        return true;
    }

    // Handle "." prefix for current directory
    if (std.mem.eql(u8, clean_prefix, ".")) return true;

    return false;
}

/// Normalize path by removing trailing slashes and resolving . and ..
/// Does not resolve symlinks or check existence.
pub fn normalizePath(alloc: std.mem.Allocator, path: []const u8) ![]const u8 {
    if (path.len == 0) return try alloc.dupe(u8, ".");

    var components = std.ArrayListUnmanaged([]const u8){};
    defer components.deinit(alloc);

    const is_absolute = path[0] == '/';
    var iter = std.mem.splitScalar(u8, path, '/');

    while (iter.next()) |component| {
        if (component.len == 0 or std.mem.eql(u8, component, ".")) {
            continue;
        }
        if (std.mem.eql(u8, component, "..")) {
            if (components.items.len > 0 and !std.mem.eql(u8, components.items[components.items.len - 1], "..")) {
                _ = components.pop();
            } else if (!is_absolute) {
                try components.append(alloc, component);
            }
        } else {
            try components.append(alloc, component);
        }
    }

    if (components.items.len == 0) {
        return try alloc.dupe(u8, if (is_absolute) "/" else ".");
    }

    // Calculate result length
    var len: usize = if (is_absolute) 1 else 0;
    for (components.items, 0..) |comp, i| {
        if (i > 0) len += 1; // separator
        len += comp.len;
    }

    const result = try alloc.alloc(u8, len);
    var pos: usize = 0;

    if (is_absolute) {
        result[0] = '/';
        pos = 1;
    }

    for (components.items, 0..) |comp, i| {
        if (i > 0) {
            result[pos] = '/';
            pos += 1;
        }
        @memcpy(result[pos..][0..comp.len], comp);
        pos += comp.len;
    }

    return result;
}

// ============================================================================
// Tests
// ============================================================================

test "pathStartsWith basic" {
    try std.testing.expect(pathStartsWith("/tmp/foo", "/tmp"));
    try std.testing.expect(pathStartsWith("/tmp/foo/bar", "/tmp"));
    try std.testing.expect(pathStartsWith("/tmp", "/tmp"));
    try std.testing.expect(!pathStartsWith("/tmpfoo", "/tmp"));
    try std.testing.expect(!pathStartsWith("/var/tmp", "/tmp"));
}

test "pathStartsWith trailing slash" {
    try std.testing.expect(pathStartsWith("/tmp/foo", "/tmp/"));
    try std.testing.expect(pathStartsWith("/tmp", "/tmp/"));
}

test "pathStartsWith dot prefix" {
    try std.testing.expect(pathStartsWith("./foo", "."));
    try std.testing.expect(pathStartsWith("foo", "."));
}

test "pathStartsWith empty prefix" {
    try std.testing.expect(!pathStartsWith("/tmp", ""));
}

test "expandPath no expansion" {
    const alloc = std.testing.allocator;
    const result = try expandPath(alloc, "/tmp/foo");
    defer alloc.free(result);
    try std.testing.expectEqualStrings("/tmp/foo", result);
}

test "normalizePath basic" {
    const alloc = std.testing.allocator;

    const r1 = try normalizePath(alloc, "/foo/bar/../baz");
    defer alloc.free(r1);
    try std.testing.expectEqualStrings("/foo/baz", r1);

    const r2 = try normalizePath(alloc, "/foo/./bar");
    defer alloc.free(r2);
    try std.testing.expectEqualStrings("/foo/bar", r2);

    const r3 = try normalizePath(alloc, "foo//bar");
    defer alloc.free(r3);
    try std.testing.expectEqualStrings("foo/bar", r3);
}
