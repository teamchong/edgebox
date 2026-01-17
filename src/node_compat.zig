/// Node.js Compatibility Layer for EdgeBox
///
/// Provides Node.js-compatible APIs for running Claude Code and npm packages.
/// Implements: fs, path, http/https, child_process, crypto, buffer, stream.
///
/// Usage:
/// ```zig
/// const node = @import("node_compat");
///
/// // Register Node.js APIs in QuickJS context
/// node.registerAll(&ctx, &wasi);
/// ```
const std = @import("std");
const Allocator = std.mem.Allocator;
const quickjs = @import("quickjs.zig");
const wasi_mod = @import("wasi.zig");

/// The Node.js polyfill JavaScript code (from single source of truth)
const polyfills = @import("polyfills/polyfills.zig");
const polyfill_js = polyfills.node_polyfill_js;

/// Register all Node.js compatibility modules
pub fn registerAll(ctx: *quickjs.Context, wasi: *wasi_mod.WasiContext) void {
    _ = wasi; // Will use later for env/argv

    _ = ctx.eval(polyfill_js) catch |err| {
        std.log.err("Failed to register Node.js polyfills: {}", .{err});
    };
}

/// Register basic Node.js compatibility (path, buffer, util only)
pub fn registerBasics(ctx: *quickjs.Context) void {
    const basic_polyfill =
        \\(function() {
        \\    'use strict';
        \\    const _modules = {};
        \\
        \\    // Path
        \\    _modules.path = {
        \\        sep: '/',
        \\        delimiter: ':',
        \\        join: function(...parts) { return parts.filter(p => p).join('/').replace(/\/+/g, '/').replace(/\/$/, '') || '.'; },
        \\        dirname: function(p) { const idx = p.lastIndexOf('/'); return idx === -1 ? '.' : idx === 0 ? '/' : p.slice(0, idx); },
        \\        basename: function(p, ext) { let base = p.slice(p.lastIndexOf('/') + 1); if (ext && base.endsWith(ext)) base = base.slice(0, -ext.length); return base; },
        \\        extname: function(p) { const base = this.basename(p); const idx = base.lastIndexOf('.'); return idx > 0 ? base.slice(idx) : ''; },
        \\        isAbsolute: function(p) { return p.startsWith('/'); }
        \\    };
        \\
        \\    // Buffer
        \\    class Buffer extends Uint8Array {
        \\        static from(data) { return typeof data === 'string' ? new Buffer(new TextEncoder().encode(data)) : new Buffer(data); }
        \\        static alloc(size) { return new Buffer(size); }
        \\        toString() { return new TextDecoder().decode(this); }
        \\    }
        \\    _modules.buffer = { Buffer };
        \\    globalThis.Buffer = Buffer;
        \\
        \\    // Util
        \\    _modules.util = {
        \\        promisify: fn => (...args) => new Promise((resolve, reject) => fn(...args, (err, result) => err ? reject(err) : resolve(result))),
        \\        format: (fmt, ...args) => { let i = 0; return fmt.replace(/%[sd]/g, () => args[i++]); }
        \\    };
        \\
        \\    globalThis.require = function(name) {
        \\        if (_modules[name]) return _modules[name];
        \\        throw new Error('Module not found: ' + name);
        \\    };
        \\})();
    ;

    _ = ctx.eval(basic_polyfill) catch |err| {
        std.log.err("Failed to register basic polyfills: {}", .{err});
    };
}

// Tests
test "path module" {
    const allocator = std.testing.allocator;

    var rt = try quickjs.Runtime.init(allocator);
    defer rt.deinit();

    var ctx = try rt.newContext();
    defer ctx.deinit();

    registerBasics(&ctx);

    const result = try ctx.eval("require('path').join('a', 'b', 'c')");
    defer result.free();

    const str = try result.toString(allocator);
    defer allocator.free(str);

    try std.testing.expectEqualStrings("a/b/c", str);
}
