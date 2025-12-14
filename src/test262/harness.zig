const std = @import("std");
const Engine = @import("main.zig").Engine;

/// Loads and manages test262 harness files
pub const HarnessLoader = struct {
    allocator: std.mem.Allocator,
    harness_dir: []const u8,
    engine: Engine,
    cache: std.StringHashMap([]const u8),
    polyfill: []const u8,

    pub fn init(allocator: std.mem.Allocator, harness_dir: []const u8, engine: Engine) HarnessLoader {
        return .{
            .allocator = allocator,
            .harness_dir = harness_dir,
            .engine = engine,
            .cache = std.StringHashMap([]const u8).init(allocator),
            .polyfill = getPolyfill(engine),
        };
    }

    pub fn deinit(self: *HarnessLoader) void {
        var iter = self.cache.valueIterator();
        while (iter.next()) |value| {
            self.allocator.free(value.*);
        }
        self.cache.deinit();
    }

    /// Load a harness file by name (e.g., "sta.js", "assert.js")
    pub fn load(self: *HarnessLoader, name: []const u8) ![]const u8 {
        // Check cache first
        if (self.cache.get(name)) |content| {
            return content;
        }

        // Load from file
        const path = try std.fs.path.join(self.allocator, &.{ self.harness_dir, name });
        defer self.allocator.free(path);

        const file = try std.fs.cwd().openFile(path, .{});
        defer file.close();

        const content = try file.readToEndAlloc(self.allocator, 10 * 1024 * 1024);

        // Cache it
        const name_copy = try self.allocator.dupe(u8, name);
        try self.cache.put(name_copy, content);

        return content;
    }

    /// Load multiple harness files and concatenate them
    pub fn loadAll(self: *HarnessLoader, names: []const []const u8) ![]const u8 {
        var total_size: usize = self.polyfill.len + 1; // +1 for newline

        // First pass: calculate total size
        for (names) |name| {
            const content = try self.load(name);
            total_size += content.len + 1; // +1 for newline
        }

        // Allocate buffer
        var result = try self.allocator.alloc(u8, total_size);
        var pos: usize = 0;

        // Add polyfill first
        @memcpy(result[pos .. pos + self.polyfill.len], self.polyfill);
        pos += self.polyfill.len;
        result[pos] = '\n';
        pos += 1;

        // Add each harness file
        for (names) |name| {
            const content = try self.load(name);
            @memcpy(result[pos .. pos + content.len], content);
            pos += content.len;
            result[pos] = '\n';
            pos += 1;
        }

        return result;
    }

    /// Get the polyfill code for the engine
    fn getPolyfill(engine: Engine) []const u8 {
        return switch (engine) {
            .node, .bun => node_bun_polyfill,
            .qjs, .edgebox => qjs_polyfill,
        };
    }
};

/// Polyfill for Node.js and Bun
/// Provides: print, $262
const node_bun_polyfill =
    \\// test262 polyfill for Node.js/Bun
    \\var print = typeof print !== 'undefined' ? print : console.log.bind(console);
    \\
    \\var $262 = {
    \\  createRealm: function() {
    \\    // In Node/Bun, we can't easily create realms
    \\    // Return a mock that works for basic tests
    \\    return {
    \\      global: globalThis,
    \\      evalScript: function(s) { return eval(s); }
    \\    };
    \\  },
    \\  evalScript: function(s) {
    \\    return eval(s);
    \\  },
    \\  gc: function() {
    \\    if (typeof gc === 'function') gc();
    \\  },
    \\  global: globalThis,
    \\  agent: {
    \\    start: function() {},
    \\    broadcast: function() {},
    \\    getReport: function() { return null; },
    \\    sleep: function(ms) {
    \\      var end = Date.now() + ms;
    \\      while (Date.now() < end) {}
    \\    },
    \\    monotonicNow: function() { return Date.now(); }
    \\  },
    \\  detachArrayBuffer: function(buf) {
    \\    // Can't actually detach in standard JS
    \\    // Some tests may fail because of this
    \\  }
    \\};
    \\
    \\// Async test support
    \\var $DONE = function(err) {
    \\  if (err) {
    \\    print('Test262:AsyncTestFailure:' + err);
    \\    process.exit(1);
    \\  } else {
    \\    print('Test262:AsyncTestComplete');
    \\  }
    \\};
    \\
;

/// Polyfill for QuickJS and EdgeBox
/// QuickJS already has print and most $262 features
const qjs_polyfill =
    \\// test262 polyfill for QuickJS/EdgeBox
    \\var $DONE = function(err) {
    \\  if (err) {
    \\    print('Test262:AsyncTestFailure:' + err);
    \\  } else {
    \\    print('Test262:AsyncTestComplete');
    \\  }
    \\};
    \\
;

/// Strict mode wrapper
pub const strict_prefix = "'use strict';\n";

/// Get the complete prepared test code
pub fn prepareTest(
    allocator: std.mem.Allocator,
    harness_loader: *HarnessLoader,
    test_content: []const u8,
    includes: []const []const u8,
    use_strict: bool,
) ![]const u8 {
    // Load harness files
    const harness_code = try harness_loader.loadAll(includes);
    defer allocator.free(harness_code);

    // Calculate total size
    const strict_len: usize = if (use_strict) strict_prefix.len else 0;
    const total_size = strict_len + harness_code.len + test_content.len + 2; // +2 for newlines

    // Allocate and combine
    var result = try allocator.alloc(u8, total_size);
    var pos: usize = 0;

    // Add strict mode if needed
    if (use_strict) {
        @memcpy(result[pos .. pos + strict_prefix.len], strict_prefix);
        pos += strict_prefix.len;
    }

    // Add harness code
    @memcpy(result[pos .. pos + harness_code.len], harness_code);
    pos += harness_code.len;
    result[pos] = '\n';
    pos += 1;

    // Add test content
    @memcpy(result[pos .. pos + test_content.len], test_content);
    pos += test_content.len;
    result[pos] = '\n';
    pos += 1;

    return result[0..pos];
}
