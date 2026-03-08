//! Per-function call profiling for Profile-Guided Optimization (PGO).
//!
//! Two-phase workflow:
//!   Phase 1: Build with --no-freeze, run with --profile-out=profile.json
//!            → counts every JS function call via JS_CallInternal hook
//!   Phase 2: Build with --freeze-profile=profile.json --freeze-max-functions=N
//!            → freezes the top-N hottest functions by actual call frequency
//!
//! Exported as C symbols so QuickJS patch can call them from JS_CallInternal.

const std = @import("std");

// --- Global state (C-callable) ---

var enabled: bool = false;
/// Key = (atom << 32) | line_num, Value = call count
var counts: std.AutoHashMapUnmanaged(u64, u64) = .{};
const alloc = std.heap.c_allocator;

/// Stored context and path for dump-on-exit
var stored_ctx: ?*anyopaque = null;
var stored_path: ?[*:0]const u8 = null;

/// Enable call profiling. Called from native_main_embed when --profile-out is set.
pub fn enable() void {
    enabled = true;
    edgebox_call_profile_is_enabled = 1;
}

/// Store context/path for later dump (called from native_main_embed after ctx is ready)
pub fn setDumpTarget(ctx: ?*anyopaque, path: [*:0]const u8) void {
    stored_ctx = ctx;
    stored_path = path;
}

/// C-callable: flush profile to disk. Called from process.exit polyfill before _exit().
pub export fn edgebox_call_profile_flush() void {
    if (stored_path) |path| {
        dump(stored_ctx, path);
        stored_path = null; // prevent double-dump
    }
}

/// Global variable checked by QuickJS JS_CallInternal (avoids function call overhead).
/// A load from a global is ~1 cycle vs ~10 cycles for an extern function call.
pub export var edgebox_call_profile_is_enabled: c_int = 0;

/// C-callable: increment call count for function identified by (atom, line_num).
pub export fn edgebox_call_profile_increment(atom: u32, line_num: u32) void {
    const key = (@as(u64, atom) << 32) | @as(u64, line_num);
    const gop = counts.getOrPut(alloc, key) catch return;
    if (!gop.found_existing) gop.value_ptr.* = 0;
    gop.value_ptr.* += 1;
}

/// Dump collected profile to a JSON file.
/// Uses QuickJS JS_AtomGetStr to resolve atom IDs to function names.
pub fn dump(ctx: ?*anyopaque, path: [*:0]const u8) void {
    const file = std.fs.cwd().createFileZ(path, .{}) catch |err| {
        std.debug.print("[profile] Failed to create {s}: {}\n", .{ path, err });
        return;
    };
    defer file.close();

    // Build output into a buffer, then write all at once
    var buf: std.ArrayListUnmanaged(u8) = .{};
    defer buf.deinit(alloc);
    const w = buf.writer(alloc);

    // Collect entries and sort by count descending
    const Entry = struct { key: u64, count: u64 };
    var entries = std.ArrayListUnmanaged(Entry){};
    defer entries.deinit(alloc);

    var iter = counts.iterator();
    while (iter.next()) |kv| {
        entries.append(alloc, .{ .key = kv.key_ptr.*, .count = kv.value_ptr.* }) catch continue;
    }

    std.mem.sort(Entry, entries.items, {}, struct {
        fn lessThan(_: void, a: Entry, b: Entry) bool {
            return a.count > b.count; // descending
        }
    }.lessThan);

    w.writeAll("{\n") catch return;

    // Resolve atom names via QuickJS public API (JS_AtomToCStringLen is JS_EXTERN)
    const JS_AtomToCStringLen = @extern(*const fn (?*anyopaque, ?*usize, u32) callconv(.c) ?[*:0]const u8, .{
        .name = "JS_AtomToCStringLen",
    });
    const JS_FreeCString = @extern(*const fn (?*anyopaque, ?[*:0]const u8) callconv(.c) void, .{
        .name = "JS_FreeCString",
    });

    for (entries.items, 0..) |entry, i| {
        const atom: u32 = @intCast(entry.key >> 32);
        const line_num: u32 = @intCast(entry.key & 0xFFFFFFFF);

        const name_ptr = JS_AtomToCStringLen(ctx, null, atom);
        defer JS_FreeCString(ctx, name_ptr);
        const name = if (name_ptr) |p| std.mem.sliceTo(p, 0) else "<unknown>";

        if (i > 0) w.writeAll(",\n") catch return;
        w.print("  \"{s}@{d}\": {d}", .{ name, line_num, entry.count }) catch return;
    }

    w.writeAll("\n}\n") catch return;
    file.writeAll(buf.items) catch return;

    std.debug.print("[profile] Wrote {d} function entries to {s}\n", .{ entries.items.len, path });
}

/// Parse a profile JSON file into a lookup map.
/// Returns a map of "name@line" → call_count.
pub fn parseProfileJson(allocator: std.mem.Allocator, path: []const u8) ?std.StringHashMapUnmanaged(u64) {
    const file = std.fs.cwd().openFile(path, .{}) catch |err| {
        std.debug.print("[profile] Failed to open {s}: {}\n", .{ path, err });
        return null;
    };
    defer file.close();

    const content = file.readToEndAlloc(allocator, 64 * 1024 * 1024) catch |err| {
        std.debug.print("[profile] Failed to read {s}: {}\n", .{ path, err });
        return null;
    };
    defer allocator.free(content);

    var map = std.StringHashMapUnmanaged(u64){};

    // Simple JSON parser: extract "key": value pairs
    // Format: { "name@line": count, ... }
    var pos: usize = 0;
    while (pos < content.len) {
        // Find next quoted key
        const quote_start = std.mem.indexOfScalarPos(u8, content, pos, '"') orelse break;
        const key_start = quote_start + 1;
        const quote_end = std.mem.indexOfScalarPos(u8, content, key_start, '"') orelse break;
        const key = content[key_start..quote_end];

        // Find colon then number
        pos = quote_end + 1;
        const colon = std.mem.indexOfScalarPos(u8, content, pos, ':') orelse break;
        pos = colon + 1;

        // Skip whitespace
        while (pos < content.len and (content[pos] == ' ' or content[pos] == '\t')) pos += 1;

        // Parse number
        var num_end = pos;
        while (num_end < content.len and content[num_end] >= '0' and content[num_end] <= '9') num_end += 1;
        if (num_end == pos) {
            pos = num_end + 1;
            continue;
        }

        const count = std.fmt.parseInt(u64, content[pos..num_end], 10) catch {
            pos = num_end;
            continue;
        };

        // Dupe the key so it survives after content is freed
        const key_dupe = allocator.dupe(u8, key) catch continue;
        map.put(allocator, key_dupe, count) catch continue;

        pos = num_end;
    }

    std.debug.print("[profile] Loaded {d} entries from {s}\n", .{ map.count(), path });
    return map;
}
