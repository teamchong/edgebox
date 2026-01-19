/// Optimized polyfill loading with minification, compression, and lazy loading
/// This replaces polyfills.zig for production builds
const std = @import("std");
const minify = @import("minify.zig");

// ============ Module Metadata ============
pub const ModuleInfo = struct {
    name: []const u8,
    source: []const u8,
    dependencies: []const []const u8,
    is_core: bool, // Core modules are always loaded
};

// ============ Individual Modules (minified at comptime) ============
const mod_core = comptime minify.minify(@embedFile("modules/core.js"));
const mod_path = comptime minify.minify(@embedFile("modules/path.js"));
const mod_buffer = comptime minify.minify(@embedFile("modules/buffer.js"));
const mod_encoding = comptime minify.minify(@embedFile("modules/encoding.js"));
const mod_events = comptime minify.minify(@embedFile("modules/events.js"));
const mod_stream = comptime minify.minify(@embedFile("modules/stream.js"));
const mod_fs = comptime minify.minify(@embedFile("modules/fs.js"));
const mod_crypto = comptime minify.minify(@embedFile("modules/crypto.js"));
const mod_http = comptime minify.minify(@embedFile("modules/http.js"));
const mod_https = comptime minify.minify(@embedFile("modules/https.js"));
const mod_http2 = comptime minify.minify(@embedFile("modules/http2.js"));
const mod_net = comptime minify.minify(@embedFile("modules/net.js"));
const mod_tls = comptime minify.minify(@embedFile("modules/tls.js"));
const mod_dgram = comptime minify.minify(@embedFile("modules/dgram.js"));
const mod_url = comptime minify.minify(@embedFile("modules/url.js"));
const mod_os = comptime minify.minify(@embedFile("modules/os.js"));
const mod_process = comptime minify.minify(@embedFile("modules/process.js"));
const mod_zlib = comptime minify.minify(@embedFile("modules/zlib.js"));
const mod_cluster = comptime minify.minify(@embedFile("modules/cluster.js"));
const mod_perf_hooks = comptime minify.minify(@embedFile("modules/perf_hooks.js"));
const mod_timers = comptime minify.minify(@embedFile("modules/timers.js"));

/// Runtime polyfills (minified)
pub const runtime_js = comptime minify.minify(@embedFile("runtime.js"));

// ============ Module Registry ============
pub const modules = [_]ModuleInfo{
    // Core modules - always loaded (required for require() to work)
    .{ .name = "core", .source = mod_core, .dependencies = &.{}, .is_core = true },
    .{ .name = "path", .source = mod_path, .dependencies = &.{}, .is_core = true },
    .{ .name = "buffer", .source = mod_buffer, .dependencies = &.{}, .is_core = true },
    .{ .name = "events", .source = mod_events, .dependencies = &.{}, .is_core = true },

    // Standard modules - loaded on demand
    .{ .name = "encoding", .source = mod_encoding, .dependencies = &.{}, .is_core = false },
    .{ .name = "stream", .source = mod_stream, .dependencies = &.{"events"}, .is_core = false },
    .{ .name = "fs", .source = mod_fs, .dependencies = &.{ "buffer", "path", "stream" }, .is_core = false },
    .{ .name = "crypto", .source = mod_crypto, .dependencies = &.{"buffer"}, .is_core = false },
    .{ .name = "url", .source = mod_url, .dependencies = &.{}, .is_core = false },
    .{ .name = "http", .source = mod_http, .dependencies = &.{ "stream", "url", "events" }, .is_core = false },
    .{ .name = "https", .source = mod_https, .dependencies = &.{"http"}, .is_core = false },
    .{ .name = "http2", .source = mod_http2, .dependencies = &.{"http"}, .is_core = false },
    .{ .name = "net", .source = mod_net, .dependencies = &.{ "stream", "events" }, .is_core = false },
    .{ .name = "tls", .source = mod_tls, .dependencies = &.{"net"}, .is_core = false },
    .{ .name = "dgram", .source = mod_dgram, .dependencies = &.{"events"}, .is_core = false },
    .{ .name = "zlib", .source = mod_zlib, .dependencies = &.{"buffer"}, .is_core = false },
    .{ .name = "cluster", .source = mod_cluster, .dependencies = &.{ "events", "net" }, .is_core = false },
    .{ .name = "perf_hooks", .source = mod_perf_hooks, .dependencies = &.{}, .is_core = false },
    .{ .name = "timers", .source = mod_timers, .dependencies = &.{}, .is_core = false },
};

// ============ Concatenated Core (always needed) ============
pub const core_polyfill_js = mod_core ++ mod_path ++ mod_buffer ++ mod_events;

// ============ Full polyfills (for compatibility) ============
pub const node_polyfill_js = mod_core ++ mod_path ++ mod_buffer ++ mod_encoding ++
    mod_events ++ mod_stream ++ mod_fs ++ mod_crypto ++ mod_http ++ mod_https ++ mod_http2 ++
    mod_net ++ mod_tls ++ mod_dgram ++ mod_url ++
    mod_zlib ++ mod_cluster ++ mod_perf_hooks ++ mod_timers;

// ============ Tree Shaking Support ============

/// Get modules required for a set of require() calls
pub fn getRequiredModules(required_names: []const []const u8) []const ModuleInfo {
    var result: [modules.len]ModuleInfo = undefined;
    var count: usize = 0;
    var visited = [_]bool{false} ** modules.len;

    // Always include core modules
    for (modules, 0..) |mod, i| {
        if (mod.is_core and !visited[i]) {
            result[count] = mod;
            count += 1;
            visited[i] = true;
        }
    }

    // Add required modules and their dependencies
    for (required_names) |name| {
        addModuleWithDeps(name, &result, &count, &visited);
    }

    return result[0..count];
}

fn addModuleWithDeps(name: []const u8, result: *[modules.len]ModuleInfo, count: *usize, visited: *[modules.len]bool) void {
    // Find module
    for (modules, 0..) |mod, i| {
        if (std.mem.eql(u8, mod.name, name)) {
            if (visited[i]) return;
            visited[i] = true;

            // Add dependencies first
            for (mod.dependencies) |dep| {
                addModuleWithDeps(dep, result, count, visited);
            }

            // Add this module
            result[count.*] = mod;
            count.* += 1;
            return;
        }
    }
}

/// Concatenate module sources in dependency order
pub fn concatenateModules(mods: []const ModuleInfo, allocator: std.mem.Allocator) ![]u8 {
    var total_len: usize = 0;
    for (mods) |mod| {
        total_len += mod.source.len;
    }

    var result = try allocator.alloc(u8, total_len);
    var offset: usize = 0;
    for (mods) |mod| {
        @memcpy(result[offset..][0..mod.source.len], mod.source);
        offset += mod.source.len;
    }

    return result;
}

// ============ Size Stats ============
pub fn printSizeStats() void {
    var total: usize = 0;
    std.debug.print("\n=== Polyfill Module Sizes (minified) ===\n", .{});
    for (modules) |mod| {
        std.debug.print("  {s}: {} bytes\n", .{ mod.name, mod.source.len });
        total += mod.source.len;
    }
    std.debug.print("  TOTAL: {} bytes ({} KB)\n", .{ total, total / 1024 });
}

// For compatibility with existing code
pub const all_sources = [_][]const u8{
    "EdgeBox-Polyfills-v6-optimized",
    runtime_js,
    mod_core,
    mod_path,
    mod_buffer,
    mod_encoding,
    mod_events,
    mod_stream,
    mod_fs,
    mod_crypto,
    mod_http,
    mod_https,
    mod_http2,
    mod_net,
    mod_tls,
    mod_dgram,
    mod_url,
    mod_zlib,
    mod_cluster,
    mod_perf_hooks,
    mod_timers,
};
