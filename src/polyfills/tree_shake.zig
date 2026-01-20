/// Tree shaking - analyze JavaScript source to find required modules
/// Used at build time to include only needed polyfills
const std = @import("std");

/// All known module names that can be required
pub const known_modules = [_][]const u8{
    "fs",
    "path",
    "crypto",
    "http",
    "https",
    "http2",
    "net",
    "tls",
    "dgram",
    "url",
    "stream",
    "events",
    "buffer",
    "util",
    "os",
    "child_process",
    "cluster",
    "zlib",
    "dns",
    "assert",
    "querystring",
    "string_decoder",
    "punycode",
    "perf_hooks",
    "timers",
    "timers/promises",
    "readline",
    "repl",
    "vm",
    "v8",
    "worker_threads",
    "async_hooks",
    "inspector",
    "trace_events",
    "constants",
    "module",
    "process",
};

/// Analyze JavaScript source code to find all require() calls
/// Returns a list of module names that are required
pub fn analyzeRequires(allocator: std.mem.Allocator, source: []const u8) ![][]const u8 {
    var result = try std.ArrayList([]const u8).initCapacity(allocator, 16);
    errdefer result.deinit(allocator);

    var i: usize = 0;
    while (i < source.len) {
        // Look for require('...) or require("...)
        if (i + 8 < source.len and std.mem.eql(u8, source[i .. i + 8], "require(")) {
            i += 8;
            // Skip whitespace
            while (i < source.len and (source[i] == ' ' or source[i] == '\t')) {
                i += 1;
            }
            // Check for string quote
            if (i < source.len and (source[i] == '"' or source[i] == '\'')) {
                const quote = source[i];
                i += 1;
                const start = i;
                // Find end of string
                while (i < source.len and source[i] != quote) {
                    if (source[i] == '\\' and i + 1 < source.len) {
                        i += 2; // Skip escaped char
                    } else {
                        i += 1;
                    }
                }
                if (i > start) {
                    const module_name = source[start..i];
                    // Check if it's a known module (not a relative path)
                    if (!std.mem.startsWith(u8, module_name, ".") and
                        !std.mem.startsWith(u8, module_name, "/"))
                    {
                        // Strip node: prefix if present
                        const clean_name = if (std.mem.startsWith(u8, module_name, "node:"))
                            module_name[5..]
                        else
                            module_name;

                        // Check if already in list
                        var found = false;
                        for (result.items) |existing| {
                            if (std.mem.eql(u8, existing, clean_name)) {
                                found = true;
                                break;
                            }
                        }
                        if (!found) {
                            // Duplicate the string since source may be freed
                            const dup = try allocator.dupe(u8, clean_name);
                            try result.append(allocator, dup);
                        }
                    }
                }
            }
        }

        // Also check for import ... from '...'
        if (i + 6 < source.len and std.mem.eql(u8, source[i .. i + 6], "import")) {
            // Find 'from' keyword
            var j = i + 6;
            while (j + 5 < source.len) {
                if (std.mem.eql(u8, source[j .. j + 4], "from")) {
                    j += 4;
                    // Skip whitespace
                    while (j < source.len and (source[j] == ' ' or source[j] == '\t')) {
                        j += 1;
                    }
                    // Check for string quote
                    if (j < source.len and (source[j] == '"' or source[j] == '\'')) {
                        const quote = source[j];
                        j += 1;
                        const start = j;
                        while (j < source.len and source[j] != quote) {
                            j += 1;
                        }
                        if (j > start) {
                            const module_name = source[start..j];
                            if (!std.mem.startsWith(u8, module_name, ".") and
                                !std.mem.startsWith(u8, module_name, "/"))
                            {
                                const clean_name = if (std.mem.startsWith(u8, module_name, "node:"))
                                    module_name[5..]
                                else
                                    module_name;

                                var found = false;
                                for (result.items) |existing| {
                                    if (std.mem.eql(u8, existing, clean_name)) {
                                        found = true;
                                        break;
                                    }
                                }
                                if (!found) {
                                    const dup = try allocator.dupe(u8, clean_name);
                                    try result.append(allocator, dup);
                                }
                            }
                        }
                        i = j;
                    }
                    break;
                }
                j += 1;
            }
        }

        i += 1;
    }

    return result.toOwnedSlice(allocator);
}

/// Get the transitive dependencies for a set of modules
pub fn getDependencies(allocator: std.mem.Allocator, required: []const []const u8) ![][]const u8 {
    var result = try std.ArrayList([]const u8).initCapacity(allocator, 16);
    errdefer result.deinit(allocator);

    // Module dependency graph
    const deps = .{
        .{ "stream", &[_][]const u8{"events"} },
        .{ "fs", &[_][]const u8{ "buffer", "path", "stream", "events" } },
        .{ "crypto", &[_][]const u8{"buffer"} },
        .{ "http", &[_][]const u8{ "stream", "url", "events", "net" } },
        .{ "https", &[_][]const u8{ "http", "tls" } },
        .{ "http2", &[_][]const u8{"http"} },
        .{ "net", &[_][]const u8{ "stream", "events" } },
        .{ "tls", &[_][]const u8{"net"} },
        .{ "dgram", &[_][]const u8{"events"} },
        .{ "child_process", &[_][]const u8{ "events", "stream" } },
        .{ "cluster", &[_][]const u8{ "events", "net" } },
        .{ "zlib", &[_][]const u8{ "buffer", "stream" } },
        .{ "readline", &[_][]const u8{ "events", "stream" } },
        .{ "timers/promises", &[_][]const u8{"timers"} },
        .{ "stream/promises", &[_][]const u8{"stream"} },
    };

    // Always include core modules
    const core = [_][]const u8{ "events", "buffer", "path" };
    for (core) |mod| {
        try result.append(allocator, mod);
    }

    // Add required modules and their deps
    for (required) |mod| {
        try addWithDeps(allocator, mod, &result, &deps);
    }

    return result.toOwnedSlice(allocator);
}

fn addWithDeps(
    allocator: std.mem.Allocator,
    mod: []const u8,
    result: *std.ArrayList([]const u8),
    deps: anytype,
) !void {
    // Check if already added
    for (result.items) |existing| {
        if (std.mem.eql(u8, existing, mod)) return;
    }

    // Find dependencies
    inline for (deps) |entry| {
        if (std.mem.eql(u8, entry[0], mod)) {
            // Add dependencies first
            for (entry[1]) |dep| {
                try addWithDeps(allocator, dep, result, deps);
            }
            break;
        }
    }

    // Add this module
    try result.append(allocator, mod);
}

/// Filter polyfill modules based on required modules
pub fn filterPolyfills(
    all_modules: []const []const u8,
    required: []const []const u8,
) []const []const u8 {
    var result: [32][]const u8 = undefined;
    var count: usize = 0;

    for (all_modules) |mod| {
        for (required) |req| {
            if (std.mem.eql(u8, mod, req)) {
                result[count] = mod;
                count += 1;
                break;
            }
        }
    }

    return result[0..count];
}

test "analyzeRequires finds require calls" {
    const allocator = std.testing.allocator;
    const source = "const fs = require('fs');\nconst path = require(\"path\");";
    const requires = try analyzeRequires(allocator, source);
    defer {
        for (requires) |r| allocator.free(r);
        allocator.free(requires);
    }

    try std.testing.expectEqual(@as(usize, 2), requires.len);
    try std.testing.expectEqualStrings("fs", requires[0]);
    try std.testing.expectEqualStrings("path", requires[1]);
}

test "analyzeRequires handles node: prefix" {
    const allocator = std.testing.allocator;
    const source = "const fs = require('node:fs');";
    const requires = try analyzeRequires(allocator, source);
    defer {
        for (requires) |r| allocator.free(r);
        allocator.free(requires);
    }

    try std.testing.expectEqual(@as(usize, 1), requires.len);
    try std.testing.expectEqualStrings("fs", requires[0]);
}
