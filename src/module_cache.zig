//! Thread-Safe Module Cache with Reference Counting
//!
//! Caches loaded WASM/AOT modules to avoid redundant file I/O and parsing.
//! Uses reference counting for safe concurrent access and LRU eviction.
//!
//! Features:
//! - Content hash (SHA256 of first 1MB) for change detection
//! - Path hash (Wyhash) for fast lookup
//! - Reference counting prevents premature eviction
//! - LRU eviction when cache is full
//!
//! Based on zell's llama.zig model cache implementation.

const std = @import("std");
const builtin = @import("builtin");

const log = std.log.scoped(.module_cache);

/// Maximum number of modules to cache
const MAX_CACHED_MODULES = 64;

/// Size of content to hash for change detection (1MB)
const CONTENT_HASH_SIZE = 1024 * 1024;

pub const ModuleCache = struct {
    entries: [MAX_CACHED_MODULES]CacheEntry = [_]CacheEntry{CacheEntry{}} ** MAX_CACHED_MODULES,
    mutex: std.Thread.Mutex = .{},
    stats: Stats = .{},

    pub const Stats = struct {
        hits: u64 = 0,
        misses: u64 = 0,
        evictions: u64 = 0,
    };

    pub const CacheEntry = struct {
        /// Hash of file path for fast lookup
        path_hash: u64 = 0,
        /// SHA256 of first 1MB for change detection
        content_hash: [32]u8 = [_]u8{0} ** 32,
        /// Cached module data (mmap'd or heap allocated)
        data: ?[]const u8 = null,
        /// File size
        size: usize = 0,
        /// Reference count (0 = unused, can be evicted)
        ref_count: std.atomic.Value(u32) = std.atomic.Value(u32).init(0),
        /// Last access timestamp (for LRU)
        last_used: i64 = 0,
        /// Is this entry valid?
        valid: bool = false,
    };

    /// Acquire a cached module, incrementing ref count
    /// Returns null if not cached or content changed
    pub fn acquire(self: *ModuleCache, path: []const u8) ?[]const u8 {
        self.mutex.lock();
        defer self.mutex.unlock();

        const path_hash = std.hash.Wyhash.hash(0, path);

        for (&self.entries) |*entry| {
            if (entry.valid and entry.path_hash == path_hash) {
                // Verify content hash hasn't changed
                const current_hash = computeContentHash(path) catch {
                    // File access error - invalidate entry
                    entry.valid = false;
                    self.stats.misses += 1;
                    return null;
                };

                if (!std.mem.eql(u8, &entry.content_hash, &current_hash)) {
                    // Content changed - invalidate
                    log.info("Cache invalidated (content changed): {s}", .{path});
                    entry.valid = false;
                    self.stats.misses += 1;
                    return null;
                }

                // Cache hit!
                _ = entry.ref_count.fetchAdd(1, .monotonic);
                entry.last_used = std.time.timestamp();
                self.stats.hits += 1;

                log.debug("Cache hit: {s} (refs: {})", .{
                    path,
                    entry.ref_count.load(.monotonic),
                });

                return entry.data;
            }
        }

        self.stats.misses += 1;
        return null;
    }

    /// Release a cached module, decrementing ref count
    pub fn release(self: *ModuleCache, data: []const u8) void {
        self.mutex.lock();
        defer self.mutex.unlock();

        for (&self.entries) |*entry| {
            if (!entry.valid) continue;
            if (entry.data) |cached_data| {
                if (cached_data.ptr == data.ptr) {
                    const prev = entry.ref_count.fetchSub(1, .monotonic);
                    log.debug("Released module (refs: {} -> {})", .{ prev, prev - 1 });
                    return;
                }
            }
        }

        log.warn("Released unknown module data", .{});
    }

    /// Insert a module into cache
    /// Caller must ensure data remains valid while cached
    pub fn insert(self: *ModuleCache, path: []const u8, data: []const u8) !void {
        self.mutex.lock();
        defer self.mutex.unlock();

        const path_hash = std.hash.Wyhash.hash(0, path);
        const content_hash = try computeContentHash(path);

        // Check if already cached
        for (&self.entries) |*entry| {
            if (entry.valid and entry.path_hash == path_hash) {
                // Update existing entry
                entry.content_hash = content_hash;
                entry.data = data;
                entry.size = data.len;
                entry.last_used = std.time.timestamp();
                log.debug("Updated cached module: {s}", .{path});
                return;
            }
        }

        // Find empty slot or evict LRU
        var target: ?*CacheEntry = null;
        var oldest_time: i64 = std.math.maxInt(i64);
        var lowest_ref: u32 = std.math.maxInt(u32);

        for (&self.entries) |*entry| {
            if (!entry.valid) {
                target = entry;
                break;
            }

            // For eviction, prefer lowest ref count, then oldest
            const ref = entry.ref_count.load(.monotonic);
            if (ref < lowest_ref or (ref == lowest_ref and entry.last_used < oldest_time)) {
                // Only evict if ref count is 0
                if (ref == 0) {
                    lowest_ref = ref;
                    oldest_time = entry.last_used;
                    target = entry;
                }
            }
        }

        if (target) |entry| {
            if (entry.valid) {
                log.info("Evicting cached module (LRU)", .{});
                self.stats.evictions += 1;
            }

            entry.* = CacheEntry{
                .path_hash = path_hash,
                .content_hash = content_hash,
                .data = data,
                .size = data.len,
                .ref_count = std.atomic.Value(u32).init(1), // Start with ref=1
                .last_used = std.time.timestamp(),
                .valid = true,
            };

            log.info("Cached module: {s} ({d:.1} MB)", .{
                path,
                @as(f64, @floatFromInt(data.len)) / (1024 * 1024),
            });
        } else {
            log.warn("Cache full, all entries in use - skipping cache", .{});
        }
    }

    /// Invalidate all entries for a path
    pub fn invalidate(self: *ModuleCache, path: []const u8) void {
        self.mutex.lock();
        defer self.mutex.unlock();

        const path_hash = std.hash.Wyhash.hash(0, path);

        for (&self.entries) |*entry| {
            if (entry.valid and entry.path_hash == path_hash) {
                entry.valid = false;
                log.debug("Invalidated cache entry: {s}", .{path});
            }
        }
    }

    /// Clear all cache entries
    pub fn clear(self: *ModuleCache) void {
        self.mutex.lock();
        defer self.mutex.unlock();

        for (&self.entries) |*entry| {
            entry.valid = false;
        }

        log.info("Cache cleared", .{});
    }

    /// Get cache statistics
    pub fn getStats(self: *ModuleCache) Stats {
        self.mutex.lock();
        defer self.mutex.unlock();
        return self.stats;
    }

    /// Get cache hit rate
    pub fn getHitRate(self: *ModuleCache) f64 {
        self.mutex.lock();
        defer self.mutex.unlock();

        const total = self.stats.hits + self.stats.misses;
        if (total == 0) return 0;
        return @as(f64, @floatFromInt(self.stats.hits)) / @as(f64, @floatFromInt(total));
    }
};

/// Compute SHA256 hash of first 1MB of file (for change detection)
fn computeContentHash(path: []const u8) ![32]u8 {
    const file = try std.fs.cwd().openFile(path, .{});
    defer file.close();

    var hasher = std.crypto.hash.sha2.Sha256.init(.{});

    var buf: [CONTENT_HASH_SIZE]u8 = undefined;
    const bytes_read = try file.read(&buf);

    hasher.update(buf[0..bytes_read]);

    var hash: [32]u8 = undefined;
    hasher.final(&hash);
    return hash;
}

/// Global module cache instance
pub var global_cache: ModuleCache = .{};

// Tests
test "module cache basic" {
    var cache = ModuleCache{};

    const test_path = "/tmp/module_cache_test.wasm";
    const test_data = "test wasm data";

    // Create test file
    {
        const file = try std.fs.cwd().createFile(test_path, .{});
        defer file.close();
        try file.writeAll(test_data);
    }
    defer std.fs.cwd().deleteFile(test_path) catch {};

    // Should miss initially
    try std.testing.expectEqual(@as(?[]const u8, null), cache.acquire(test_path));

    // Insert
    try cache.insert(test_path, test_data);

    // Should hit now
    const cached = cache.acquire(test_path);
    try std.testing.expect(cached != null);
    try std.testing.expectEqualStrings(test_data, cached.?);

    // Release
    cache.release(cached.?);

    // Check stats
    const stats = cache.getStats();
    try std.testing.expectEqual(@as(u64, 1), stats.hits);
    try std.testing.expectEqual(@as(u64, 1), stats.misses);
}

test "module cache invalidation" {
    var cache = ModuleCache{};

    const test_path = "/tmp/module_cache_invalid_test.wasm";
    const test_data = "original data";
    const new_data = "modified data";

    // Create test file
    {
        const file = try std.fs.cwd().createFile(test_path, .{});
        defer file.close();
        try file.writeAll(test_data);
    }
    defer std.fs.cwd().deleteFile(test_path) catch {};

    // Insert
    try cache.insert(test_path, test_data);

    // Modify file (changes content hash)
    {
        const file = try std.fs.cwd().createFile(test_path, .{});
        defer file.close();
        try file.writeAll(new_data);
    }

    // Should miss (content changed)
    try std.testing.expectEqual(@as(?[]const u8, null), cache.acquire(test_path));
}

test "module cache hit rate" {
    var cache = ModuleCache{};

    const test_path = "/tmp/module_cache_hitrate_test.wasm";
    const test_data = "hitrate test";

    {
        const file = try std.fs.cwd().createFile(test_path, .{});
        defer file.close();
        try file.writeAll(test_data);
    }
    defer std.fs.cwd().deleteFile(test_path) catch {};

    // Miss
    _ = cache.acquire(test_path);
    try cache.insert(test_path, test_data);

    // 3 hits
    for (0..3) |_| {
        const data = cache.acquire(test_path);
        if (data) |d| cache.release(d);
    }

    // Hit rate should be 3/4 = 75%
    const hit_rate = cache.getHitRate();
    try std.testing.expectApproxEqAbs(@as(f64, 0.75), hit_rate, 0.01);
}
