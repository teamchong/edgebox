/// Tree shaking - analyze JavaScript source to find required modules
/// Used at build time to include only needed polyfills
const std = @import("std");

/// Check if source uses require() at all (any require call, including relative paths)
/// Used to determine if polyfill modules are needed
pub fn usesRequire(source: []const u8) bool {
    // Look for require( pattern with word boundary check
    var i: usize = 0;
    while (i + 8 <= source.len) {
        if (std.mem.eql(u8, source[i .. i + 8], "require(")) {
            // Check word boundary before 'require'
            const before_ok = i == 0 or !isIdentifierChar(source[i - 1]);
            if (before_ok) {
                return true;
            }
        }
        i += 1;
    }
    return false;
}

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

// ============================================================================
// Runtime Module Tree Shaking
// ============================================================================

/// Global API to runtime module mapping
/// Each entry maps a global identifier to its runtime module file
pub const global_to_runtime = [_]struct { global: []const u8, module: []const u8 }{
    // base64.js
    .{ .global = "atob", .module = "src/polyfills/runtime/base64.js" },
    .{ .global = "btoa", .module = "src/polyfills/runtime/base64.js" },
    .{ .global = "DOMException", .module = "src/polyfills/runtime/base64.js" },
    // storage.js
    .{ .global = "localStorage", .module = "src/polyfills/runtime/storage.js" },
    // text.js
    .{ .global = "TextEncoder", .module = "src/polyfills/runtime/text.js" },
    .{ .global = "TextDecoder", .module = "src/polyfills/runtime/text.js" },
    // events.js
    .{ .global = "Event", .module = "src/polyfills/runtime/events.js" },
    .{ .global = "CustomEvent", .module = "src/polyfills/runtime/events.js" },
    .{ .global = "EventTarget", .module = "src/polyfills/runtime/events.js" },
    // crypto.js
    .{ .global = "crypto", .module = "src/polyfills/runtime/crypto.js" },
    .{ .global = "randomUUID", .module = "src/polyfills/runtime/crypto.js" },
    // streams.js
    .{ .global = "ReadableStream", .module = "src/polyfills/runtime/streams.js" },
    .{ .global = "WritableStream", .module = "src/polyfills/runtime/streams.js" },
    .{ .global = "TransformStream", .module = "src/polyfills/runtime/streams.js" },
    .{ .global = "ByteLengthQueuingStrategy", .module = "src/polyfills/runtime/streams.js" },
    .{ .global = "CountQueuingStrategy", .module = "src/polyfills/runtime/streams.js" },
    // url.js
    .{ .global = "URL", .module = "src/polyfills/runtime/url.js" },
    .{ .global = "URLSearchParams", .module = "src/polyfills/runtime/url.js" },
    .{ .global = "Intl", .module = "src/polyfills/runtime/url.js" },
    // abort.js
    .{ .global = "AbortController", .module = "src/polyfills/runtime/abort.js" },
    .{ .global = "AbortSignal", .module = "src/polyfills/runtime/abort.js" },
    // fetch.js
    .{ .global = "fetch", .module = "src/polyfills/runtime/fetch.js" },
    .{ .global = "Headers", .module = "src/polyfills/runtime/fetch.js" },
    .{ .global = "Request", .module = "src/polyfills/runtime/fetch.js" },
    .{ .global = "Response", .module = "src/polyfills/runtime/fetch.js" },
    .{ .global = "FormData", .module = "src/polyfills/runtime/fetch.js" },
    // buffer.js
    .{ .global = "Buffer", .module = "src/polyfills/runtime/buffer.js" },
    // wasm.js
    .{ .global = "WebAssembly", .module = "src/polyfills/runtime/wasm.js" },
    // host.js
    .{ .global = "HostArray", .module = "src/polyfills/runtime/host.js" },
    .{ .global = "HostMap", .module = "src/polyfills/runtime/host.js" },
    // tsc.js (TypeScript-specific) - triggered by createSourceFile which is TypeScript's main API
    .{ .global = "createSourceFile", .module = "src/polyfills/runtime/tsc.js" },
};

/// Runtime module dependencies
/// Each module may require other modules to be loaded first
pub const runtime_deps = [_]struct { module: []const u8, deps: []const []const u8 }{
    .{ .module = "src/polyfills/runtime/fetch.js", .deps = &[_][]const u8{
        "src/polyfills/runtime/text.js",
        "src/polyfills/runtime/base64.js",
    } },
    .{ .module = "src/polyfills/runtime/buffer.js", .deps = &[_][]const u8{
        "src/polyfills/runtime/text.js",
    } },
    .{ .module = "src/polyfills/runtime/abort.js", .deps = &[_][]const u8{
        "src/polyfills/runtime/events.js",
        "src/polyfills/runtime/base64.js", // DOMException
    } },
};

/// Analyze JavaScript source code to find all global API usages
/// Returns a list of runtime module paths that are needed
pub fn analyzeGlobals(allocator: std.mem.Allocator, source: []const u8) ![][]const u8 {
    var result = try std.ArrayList([]const u8).initCapacity(allocator, 16);
    errdefer result.deinit(allocator);

    // Check for each known global in the source
    for (global_to_runtime) |entry| {
        if (containsGlobal(source, entry.global)) {
            // Add module if not already in list
            var found = false;
            for (result.items) |existing| {
                if (std.mem.eql(u8, existing, entry.module)) {
                    found = true;
                    break;
                }
            }
            if (!found) {
                try result.append(allocator, entry.module);
            }
        }
    }

    // Add dependencies
    var to_add = try std.ArrayList([]const u8).initCapacity(allocator, 8);
    defer to_add.deinit(allocator);

    for (result.items) |mod| {
        for (runtime_deps) |dep_entry| {
            if (std.mem.eql(u8, dep_entry.module, mod)) {
                for (dep_entry.deps) |dep| {
                    var found = false;
                    for (result.items) |existing| {
                        if (std.mem.eql(u8, existing, dep)) {
                            found = true;
                            break;
                        }
                    }
                    for (to_add.items) |existing| {
                        if (std.mem.eql(u8, existing, dep)) {
                            found = true;
                            break;
                        }
                    }
                    if (!found) {
                        try to_add.append(allocator, dep);
                    }
                }
            }
        }
    }

    // Append dependencies to result
    for (to_add.items) |dep| {
        try result.append(allocator, dep);
    }

    return result.toOwnedSlice(allocator);
}

/// Check if source contains a global identifier (word-boundary aware)
fn containsGlobal(source: []const u8, global: []const u8) bool {
    var i: usize = 0;
    while (i + global.len <= source.len) {
        if (std.mem.eql(u8, source[i .. i + global.len], global)) {
            // Check word boundaries
            const before_ok = i == 0 or !isIdentifierChar(source[i - 1]);
            const after_ok = i + global.len >= source.len or !isIdentifierChar(source[i + global.len]);
            if (before_ok and after_ok) {
                return true;
            }
        }
        i += 1;
    }
    return false;
}

/// Check if character is valid in JavaScript identifier
fn isIdentifierChar(c: u8) bool {
    return (c >= 'a' and c <= 'z') or
        (c >= 'A' and c <= 'Z') or
        (c >= '0' and c <= '9') or
        c == '_' or c == '$';
}

/// Sort runtime modules in dependency order (dependencies first)
pub fn sortRuntimeModules(allocator: std.mem.Allocator, modules: []const []const u8) ![][]const u8 {
    var result = try std.ArrayList([]const u8).initCapacity(allocator, modules.len);
    errdefer result.deinit(allocator);

    var remaining = try std.ArrayList([]const u8).initCapacity(allocator, modules.len);
    defer remaining.deinit(allocator);

    for (modules) |mod| {
        try remaining.append(allocator, mod);
    }

    // Repeatedly add modules whose dependencies are satisfied
    var max_iterations: usize = modules.len * 2;
    while (remaining.items.len > 0 and max_iterations > 0) {
        max_iterations -= 1;

        var added_any = false;
        var i: usize = 0;
        while (i < remaining.items.len) {
            const mod = remaining.items[i];
            var deps_satisfied = true;

            // Check if all dependencies are in result
            for (runtime_deps) |dep_entry| {
                if (std.mem.eql(u8, dep_entry.module, mod)) {
                    for (dep_entry.deps) |dep| {
                        // Only check if dep is in our modules list
                        var dep_in_modules = false;
                        for (modules) |m| {
                            if (std.mem.eql(u8, m, dep)) {
                                dep_in_modules = true;
                                break;
                            }
                        }
                        if (dep_in_modules) {
                            var dep_in_result = false;
                            for (result.items) |r| {
                                if (std.mem.eql(u8, r, dep)) {
                                    dep_in_result = true;
                                    break;
                                }
                            }
                            if (!dep_in_result) {
                                deps_satisfied = false;
                                break;
                            }
                        }
                    }
                    break;
                }
            }

            if (deps_satisfied) {
                try result.append(allocator, mod);
                _ = remaining.orderedRemove(i);
                added_any = true;
            } else {
                i += 1;
            }
        }

        // If we couldn't add anything, there's a circular dependency - just add remaining
        if (!added_any) {
            for (remaining.items) |mod| {
                try result.append(allocator, mod);
            }
            break;
        }
    }

    return result.toOwnedSlice(allocator);
}

test "analyzeGlobals finds fetch" {
    const allocator = std.testing.allocator;
    const source = "fetch('https://example.com').then(r => r.json())";
    const globals = try analyzeGlobals(allocator, source);
    defer allocator.free(globals);

    // Should include fetch.js and its deps (text.js, base64.js)
    try std.testing.expect(globals.len >= 1);
    var found_fetch = false;
    for (globals) |g| {
        if (std.mem.eql(u8, g, "src/polyfills/runtime/fetch.js")) {
            found_fetch = true;
        }
    }
    try std.testing.expect(found_fetch);
}

test "analyzeGlobals finds Buffer" {
    const allocator = std.testing.allocator;
    const source = "const b = Buffer.from('hello');";
    const globals = try analyzeGlobals(allocator, source);
    defer allocator.free(globals);

    var found_buffer = false;
    for (globals) |g| {
        if (std.mem.eql(u8, g, "src/polyfills/runtime/buffer.js")) {
            found_buffer = true;
        }
    }
    try std.testing.expect(found_buffer);
}

test "containsGlobal word boundary" {
    // Should match standalone "fetch"
    try std.testing.expect(containsGlobal("fetch(url)", "fetch"));
    // Should not match "prefetch" or "fetcher"
    try std.testing.expect(!containsGlobal("prefetch(url)", "fetch"));
    try std.testing.expect(!containsGlobal("const fetcher = 1;", "fetch"));
    // Should match fetch in various contexts
    try std.testing.expect(containsGlobal("globalThis.fetch", "fetch"));
    try std.testing.expect(containsGlobal("const f = fetch;", "fetch"));
}
