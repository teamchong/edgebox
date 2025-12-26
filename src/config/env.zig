/// Environment variable handling for EdgeBox configuration
///
/// Handles:
/// - Loading .env files
/// - EDGEBOX_* prefix mapping (EDGEBOX_API_KEY -> API_KEY)
/// - $HOME expansion in values
const std = @import("std");

/// Parse a .env file and return key-value pairs
/// Handles:
/// - Comments (#)
/// - Quoted values ("value" or 'value')
/// - Inline comments (key=value # comment)
/// - EDGEBOX_ prefix stripping
/// - $HOME expansion in values
pub fn loadEnvFile(alloc: std.mem.Allocator, env_vars: *std.ArrayListUnmanaged([]const u8)) void {
    const env_file = std.fs.cwd().openFile(".env", .{}) catch return;
    defer env_file.close();

    const env_size = env_file.getEndPos() catch return;
    if (env_size > 64 * 1024) return; // Max 64KB .env file

    const env_buf = alloc.alloc(u8, env_size) catch return;
    defer alloc.free(env_buf);

    const bytes_read = env_file.readAll(env_buf) catch return;

    // Parse line by line
    var lines = std.mem.splitScalar(u8, env_buf[0..bytes_read], '\n');
    while (lines.next()) |line| {
        parseEnvLine(alloc, line, env_vars);
    }
}

/// Parse a single .env line and add to env_vars if valid
fn parseEnvLine(alloc: std.mem.Allocator, line: []const u8, env_vars: *std.ArrayListUnmanaged([]const u8)) void {
    // Skip comments and empty lines
    const trimmed = std.mem.trim(u8, line, " \t\r");
    if (trimmed.len == 0 or trimmed[0] == '#') return;

    // Find = separator
    const eq_pos = std.mem.indexOfScalar(u8, trimmed, '=') orelse return;
    const key = trimmed[0..eq_pos];
    var value = trimmed[eq_pos + 1 ..];

    // Strip inline comments (# not inside quotes)
    if (value.len > 0 and value[0] != '"' and value[0] != '\'') {
        if (std.mem.indexOfScalar(u8, value, '#')) |hash_pos| {
            value = std.mem.trim(u8, value[0..hash_pos], " \t");
        }
    }

    // Remove surrounding quotes if present
    if (value.len >= 2) {
        if ((value[0] == '"' and value[value.len - 1] == '"') or
            (value[0] == '\'' and value[value.len - 1] == '\''))
        {
            value = value[1 .. value.len - 1];
        }
    }

    // Expand $HOME in value
    const expanded_value = expandHomeInValue(alloc, value) catch return;

    // Map EDGEBOX_ prefixed vars to unprefixed
    const mapped_key = if (std.mem.startsWith(u8, key, "EDGEBOX_"))
        key[8..] // Strip EDGEBOX_ prefix
    else
        key;

    // Format as KEY=value
    const env_str = std.fmt.allocPrint(alloc, "{s}={s}", .{ mapped_key, expanded_value }) catch {
        alloc.free(expanded_value);
        return;
    };
    alloc.free(expanded_value);

    env_vars.append(alloc, env_str) catch {
        alloc.free(env_str);
    };
}

/// Expand $HOME in a value string
fn expandHomeInValue(alloc: std.mem.Allocator, value: []const u8) ![]const u8 {
    if (std.mem.indexOf(u8, value, "$HOME") == null) {
        return try alloc.dupe(u8, value);
    }

    const home = std.process.getEnvVarOwned(alloc, "HOME") catch {
        return try alloc.dupe(u8, value);
    };
    defer alloc.free(home);

    // Replace all occurrences of $HOME
    var result = std.ArrayListUnmanaged(u8){};
    var i: usize = 0;
    while (i < value.len) {
        if (i + 5 <= value.len and std.mem.eql(u8, value[i .. i + 5], "$HOME")) {
            result.appendSlice(alloc, home) catch return try alloc.dupe(u8, value);
            i += 5;
        } else {
            result.append(alloc, value[i]) catch return try alloc.dupe(u8, value);
            i += 1;
        }
    }
    return result.toOwnedSlice(alloc) catch try alloc.dupe(u8, value);
}

/// Get environment variable with EDGEBOX_ prefix fallback
/// First checks for VAR, then EDGEBOX_VAR
pub fn getEnvVar(name: []const u8) ?[]const u8 {
    // First try the exact name
    if (std.posix.getenv(name)) |val| {
        return val;
    }

    // Try with EDGEBOX_ prefix
    var buf: [256]u8 = undefined;
    const prefixed = std.fmt.bufPrint(&buf, "EDGEBOX_{s}", .{name}) catch return null;

    // Need null-terminated for getenv
    if (prefixed.len >= buf.len) return null;
    buf[prefixed.len] = 0;

    return std.posix.getenv(buf[0..prefixed.len :0]);
}

/// Check if an environment variable is set (with EDGEBOX_ fallback)
pub fn isEnvSet(name: []const u8) bool {
    return getEnvVar(name) != null;
}

// ============================================================================
// Tests
// ============================================================================

test "expandHomeInValue no expansion" {
    const alloc = std.testing.allocator;
    const result = try expandHomeInValue(alloc, "/tmp/foo");
    defer alloc.free(result);
    try std.testing.expectEqualStrings("/tmp/foo", result);
}

test "getEnvVar returns null for unset" {
    const result = getEnvVar("DEFINITELY_NOT_SET_12345");
    try std.testing.expect(result == null);
}
