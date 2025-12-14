// Command Emulators for EdgeBox
//
// Provides mock implementations of common commands (git, gh, npm) that can be
// used instead of executing real commands. This enables:
// - Sandboxed command execution without real system access
// - Deterministic test environments
// - Custom output formatting (server/browser/viewkit modes)
//
// Emulators are configured via .edgebox.json:
// {
//   "commands": {
//     "git": { "emulator": "git", "outputMode": "server" }
//   }
// }

const std = @import("std");
const runtime = @import("../../runtime.zig");

pub const git_emulator = @import("git_emulator.zig");
pub const gh_emulator = @import("gh_emulator.zig");
pub const npm_emulator = @import("npm_emulator.zig");

// ============================================================================
// Types
// ============================================================================

pub const EmulatorResult = struct {
    stdout: []const u8,
    stderr: []const u8,
    exit_code: i32,

    pub fn success(stdout: []const u8) EmulatorResult {
        return .{
            .stdout = stdout,
            .stderr = "",
            .exit_code = 0,
        };
    }

    pub fn failure(stderr: []const u8, code: i32) EmulatorResult {
        return .{
            .stdout = "",
            .stderr = stderr,
            .exit_code = code,
        };
    }
};

pub const OutputMode = runtime.OutputMode;

pub const EmulatorFn = *const fn (
    args: []const []const u8,
    mode: OutputMode,
    allocator: std.mem.Allocator,
) ?EmulatorResult;

// ============================================================================
// Emulator Registry
// ============================================================================

var gpa: std.mem.Allocator = undefined;
var initialized: bool = false;

/// Built-in emulators mapped by name
const builtin_emulators = [_]struct { name: []const u8, func: EmulatorFn }{
    .{ .name = "git", .func = git_emulator.emulate },
    .{ .name = "gh", .func = gh_emulator.emulate },
    .{ .name = "npm", .func = npm_emulator.emulate },
};

/// Initialize the emulator system
pub fn init(allocator: std.mem.Allocator) void {
    gpa = allocator;
    initialized = true;
}

/// Deinitialize the emulator system
pub fn deinit() void {
    initialized = false;
}

/// Try to emulate a command
/// Returns null if emulator doesn't handle this command (fall through to real exec)
pub fn tryEmulate(
    emulator_name: []const u8,
    args: []const []const u8,
    mode: OutputMode,
) ?EmulatorResult {
    if (!initialized) return null;

    // Find the emulator by name
    for (builtin_emulators) |entry| {
        if (std.mem.eql(u8, entry.name, emulator_name)) {
            return entry.func(args, mode, gpa);
        }
    }

    return null; // No emulator found
}

/// Check if an emulator exists for the given name
pub fn hasEmulator(emulator_name: []const u8) bool {
    for (builtin_emulators) |entry| {
        if (std.mem.eql(u8, entry.name, emulator_name)) {
            return true;
        }
    }
    return false;
}

/// Get list of available emulator names
pub fn getAvailableEmulators() []const []const u8 {
    comptime {
        var names: [builtin_emulators.len][]const u8 = undefined;
        for (builtin_emulators, 0..) |entry, i| {
            names[i] = entry.name;
        }
        return &names;
    }
}

// ============================================================================
// Output Formatting Helpers
// ============================================================================

/// Format data based on output mode
pub fn formatOutput(comptime T: type, data: T, mode: OutputMode, allocator: std.mem.Allocator) ![]const u8 {
    return switch (mode) {
        .server => try formatAsText(T, data, allocator),
        .browser => try formatAsHtml(T, data, allocator),
        .viewkit => try formatAsViewKit(T, data, allocator),
    };
}

fn formatAsText(comptime T: type, data: T, allocator: std.mem.Allocator) ![]const u8 {
    _ = allocator;
    // For simple types, just return the string representation
    if (T == []const u8) {
        return data;
    }
    // For other types, use default formatting
    return @typeName(T);
}

fn formatAsHtml(comptime T: type, data: T, allocator: std.mem.Allocator) ![]const u8 {
    _ = data;
    _ = allocator;
    // Wrap in HTML
    return "<pre>" ++ @typeName(T) ++ "</pre>";
}

fn formatAsViewKit(comptime T: type, data: T, allocator: std.mem.Allocator) ![]const u8 {
    _ = data;
    _ = allocator;
    // Return ViewKit JSON format
    return "{\"type\":\"text\",\"content\":\"" ++ @typeName(T) ++ "\"}";
}

// ============================================================================
// Tests
// ============================================================================

test "emulator registry - init and deinit" {
    init(std.testing.allocator);
    defer deinit();

    try std.testing.expect(initialized);
}

test "emulator registry - hasEmulator" {
    init(std.testing.allocator);
    defer deinit();

    try std.testing.expect(hasEmulator("git"));
    try std.testing.expect(hasEmulator("gh"));
    try std.testing.expect(hasEmulator("npm"));
    try std.testing.expect(!hasEmulator("unknown"));
}

test "emulator registry - tryEmulate returns null for unknown" {
    init(std.testing.allocator);
    defer deinit();

    const result = tryEmulate("unknown", &.{}, .server);
    try std.testing.expect(result == null);
}

test "emulator registry - tryEmulate git status" {
    init(std.testing.allocator);
    defer deinit();

    const args = [_][]const u8{"status"};
    const result = tryEmulate("git", &args, .server);
    try std.testing.expect(result != null);
    try std.testing.expectEqual(@as(i32, 0), result.?.exit_code);
}

test "emulator registry - tryEmulate gh pr list" {
    init(std.testing.allocator);
    defer deinit();

    const args = [_][]const u8{ "pr", "list" };
    const result = tryEmulate("gh", &args, .server);
    try std.testing.expect(result != null);
    try std.testing.expectEqual(@as(i32, 0), result.?.exit_code);
}

test "emulator registry - tryEmulate npm list" {
    init(std.testing.allocator);
    defer deinit();

    const args = [_][]const u8{"list"};
    const result = tryEmulate("npm", &args, .server);
    try std.testing.expect(result != null);
    try std.testing.expectEqual(@as(i32, 0), result.?.exit_code);
}
