/// EdgeBox Unified Error Codes
///
/// Provides consistent error codes across all EdgeBox components with:
/// - User-friendly error messages
/// - WASI errno mapping for ABI compatibility
/// - Structured error context for CLI formatting
///
/// Error code ranges:
///   0:          Success
///   -1 to -9:   General/internal errors
///   -10 to -29: Security/permission errors
///   -30 to -49: Process/spawn errors
///   -50 to -69: Filesystem errors
///   -70 to -89: Network/HTTP errors
///   -90 to -99: GPU errors
const std = @import("std");
const wasi = @import("wasi.zig");

/// Unified EdgeBox error codes
/// Negative values for ABI compatibility (WASI uses positive errno, we use negative)
pub const ErrorCode = enum(i32) {
    // === Success ===
    success = 0,

    // === General errors (-1 to -9) ===
    unknown = -1,
    out_of_memory = -2,
    invalid_argument = -3,
    slot_exhausted = -4,
    allocation_failed = -5,
    build_args_failed = -6,
    not_initialized = -7,
    wasm_memory_error = -8,
    not_found = -9,

    // === Security/permission errors (-10 to -29) ===
    permission_denied = -10,
    command_in_deny_list = -11,
    command_not_in_allow_list = -12,
    destructive_command_blocked = -20,
    sensitive_file_blocked = -21,

    // === Process/spawn errors (-30 to -49) ===
    command_not_found = -30,
    spawn_failed = -31,
    process_timeout = -32,
    invalid_command = -33,
    process_io_error = -34,
    process_killed = -35,

    // === Filesystem errors (-50 to -69) ===
    file_not_found = -50,
    fs_permission_denied = -51,
    file_exists = -52,
    not_a_directory = -53,
    is_a_directory = -54,
    directory_not_empty = -55,
    fs_io_error = -56,
    invalid_path = -57,
    path_too_long = -58,
    read_only_fs = -59,

    // === Network/HTTP errors (-70 to -89) ===
    connection_refused = -70,
    connection_timeout = -71,
    dns_error = -72,
    ssl_error = -73,
    http_forbidden = -74,
    http_not_found = -75,
    http_error = -76,

    // === GPU errors (-90 to -99) ===
    gpu_not_initialized = -90,
    gpu_invalid_handle = -91,
    gpu_dispatch_limit = -92,
    gpu_workgroup_limit = -93,
    gpu_memory_limit = -94,
    gpu_timeout = -95,
    gpu_shader_error = -96,
    gpu_feature_disabled = -97,

    /// Get human-readable error message
    pub fn message(self: ErrorCode) [:0]const u8 {
        return switch (self) {
            // Success
            .success => "Success",

            // General errors
            .unknown => "Unknown error",
            .out_of_memory => "Out of memory",
            .invalid_argument => "Invalid argument",
            .slot_exhausted => "Maximum concurrent operations exceeded",
            .allocation_failed => "Memory allocation failed",
            .build_args_failed => "Failed to build command arguments",
            .not_initialized => "Not initialized",
            .wasm_memory_error => "Failed to read WASM memory",
            .not_found => "Not found",

            // Security errors
            .permission_denied => "Permission denied: shell access requires execute permission in .edgebox.json",
            .command_in_deny_list => "Permission denied: command is in deny list",
            .command_not_in_allow_list => "Permission denied: command not in allow list (allowCommands is configured)",
            .destructive_command_blocked => "Permission denied: command matches blocked pattern (e.g., rm -rf, git reset --hard)",
            .sensitive_file_blocked => "Permission denied: command accesses sensitive file (e.g., .env, .ssh)",

            // Process errors
            .command_not_found => "Command not found",
            .spawn_failed => "Failed to spawn process",
            .process_timeout => "Process timed out",
            .invalid_command => "Invalid command",
            .process_io_error => "Process I/O error",
            .process_killed => "Process was killed",

            // Filesystem errors
            .file_not_found => "ENOENT: no such file or directory",
            .fs_permission_denied => "EACCES: permission denied",
            .file_exists => "EEXIST: file already exists",
            .not_a_directory => "ENOTDIR: not a directory",
            .is_a_directory => "EISDIR: is a directory",
            .directory_not_empty => "ENOTEMPTY: directory not empty",
            .fs_io_error => "EIO: I/O error",
            .invalid_path => "EINVAL: invalid path",
            .path_too_long => "ENAMETOOLONG: path too long",
            .read_only_fs => "EROFS: read-only file system",

            // Network errors
            .connection_refused => "Connection refused",
            .connection_timeout => "Connection timed out",
            .dns_error => "DNS resolution failed",
            .ssl_error => "SSL/TLS error",
            .http_forbidden => "HTTP 403: Forbidden - URL not in allowed list",
            .http_not_found => "HTTP 404: Not found",
            .http_error => "HTTP request failed",

            // GPU errors
            .gpu_not_initialized => "GPU not initialized",
            .gpu_invalid_handle => "Invalid GPU handle",
            .gpu_dispatch_limit => "GPU dispatch limit exceeded",
            .gpu_workgroup_limit => "GPU workgroup limit exceeded",
            .gpu_memory_limit => "GPU memory limit exceeded",
            .gpu_timeout => "GPU operation timed out",
            .gpu_shader_error => "Shader validation failed",
            .gpu_feature_disabled => "GPU feature disabled",
        };
    }

    /// Convert to WASI errno for ABI compatibility
    pub fn toWasiErrno(self: ErrorCode) wasi.Errno {
        return switch (self) {
            .success => .success,
            .file_not_found => .noent,
            .fs_permission_denied, .permission_denied => .acces,
            .file_exists => .exist,
            .not_a_directory => .notdir,
            .is_a_directory => .isdir,
            .directory_not_empty => .notempty,
            .fs_io_error, .process_io_error => .io,
            .invalid_argument, .invalid_path, .invalid_command => .inval,
            .out_of_memory, .allocation_failed => .nomem,
            .path_too_long => .nametoolong,
            .read_only_fs => .rofs,
            .connection_refused => .connrefused,
            .connection_timeout, .process_timeout, .gpu_timeout => .timedout,
            .command_not_found => .noent,
            else => .inval, // Default to EINVAL for unmapped errors
        };
    }

    /// Create ErrorCode from legacy magic number (for migration)
    pub fn fromLegacyCode(code: i32) ErrorCode {
        return switch (code) {
            0 => .success,
            -1 => .unknown,
            -2 => .not_found,
            -3 => .process_timeout,
            -4 => .slot_exhausted,
            -5 => .allocation_failed,
            -6 => .build_args_failed,
            -7 => .spawn_failed,
            -8 => .wasm_memory_error,
            -10 => .permission_denied,
            -11 => .command_in_deny_list,
            -12 => .command_not_in_allow_list,
            -20 => .destructive_command_blocked,
            -21 => .sensitive_file_blocked,
            -403 => .http_forbidden,
            else => .unknown,
        };
    }

    /// Create ErrorCode from legacy FS error codes (different scheme)
    pub fn fromFsLegacyCode(code: i32) ErrorCode {
        return switch (code) {
            0 => .success,
            -1 => .unknown,
            -2 => .file_not_found,
            -3 => .fs_permission_denied,
            -4 => .file_exists,
            -5 => .not_a_directory,
            -6 => .is_a_directory,
            -7 => .directory_not_empty,
            -8 => .fs_io_error,
            -9 => .invalid_path,
            else => .unknown,
        };
    }
};

/// Rich error context for CLI formatting
pub const ErrorContext = struct {
    code: ErrorCode,
    operation: []const u8 = "",
    path: ?[]const u8 = null,
    command: ?[]const u8 = null,
    hint: ?[]const u8 = null,

    /// Format error for CLI output
    pub fn format(self: ErrorContext, writer: anytype) !void {
        try writer.print("error: {s}\n", .{self.code.message()});

        if (self.operation.len > 0) {
            try writer.print("  operation: {s}\n", .{self.operation});
        }
        if (self.path) |p| {
            try writer.print("  path: {s}\n", .{p});
        }
        if (self.command) |c| {
            try writer.print("  command: {s}\n", .{c});
        }
        if (self.hint) |h| {
            try writer.print("\nhint: {s}\n", .{h});
        }
    }

    /// Get exit code (positive version of error code)
    pub fn exitCode(self: ErrorContext) u8 {
        const code = @intFromEnum(self.code);
        if (code >= 0) return 0;
        // Clamp to 255 max
        const abs_code: u32 = @intCast(-code);
        return if (abs_code > 255) 255 else @intCast(abs_code);
    }
};

/// Log warning for non-fatal errors (replaces silent catch {})
/// Only logs in debug mode or when EDGEBOX_DEBUG is set
pub fn logWarning(comptime fmt: []const u8, args: anytype) void {
    if (@import("builtin").mode == .Debug or
        std.posix.getenv("EDGEBOX_DEBUG") != null)
    {
        std.debug.print("[WARN] " ++ fmt ++ "\n", args);
    }
}

/// Log error unconditionally
pub fn logError(comptime fmt: []const u8, args: anytype) void {
    std.debug.print("[ERROR] " ++ fmt ++ "\n", args);
}

// ============================================================================
// Tests
// ============================================================================

test "ErrorCode.message returns valid strings" {
    const codes = [_]ErrorCode{
        .success,
        .permission_denied,
        .file_not_found,
        .command_not_found,
    };

    for (codes) |code| {
        const msg = code.message();
        try std.testing.expect(msg.len > 0);
    }
}

test "ErrorCode.toWasiErrno maps correctly" {
    try std.testing.expectEqual(wasi.Errno.success, ErrorCode.success.toWasiErrno());
    try std.testing.expectEqual(wasi.Errno.noent, ErrorCode.file_not_found.toWasiErrno());
    try std.testing.expectEqual(wasi.Errno.acces, ErrorCode.permission_denied.toWasiErrno());
    try std.testing.expectEqual(wasi.Errno.timedout, ErrorCode.process_timeout.toWasiErrno());
}

test "ErrorCode.fromLegacyCode maps magic numbers" {
    try std.testing.expectEqual(ErrorCode.permission_denied, ErrorCode.fromLegacyCode(-10));
    try std.testing.expectEqual(ErrorCode.command_in_deny_list, ErrorCode.fromLegacyCode(-11));
    try std.testing.expectEqual(ErrorCode.destructive_command_blocked, ErrorCode.fromLegacyCode(-20));
    try std.testing.expectEqual(ErrorCode.http_forbidden, ErrorCode.fromLegacyCode(-403));
}

test "ErrorContext.exitCode clamps to u8" {
    const ctx = ErrorContext{ .code = .permission_denied };
    try std.testing.expectEqual(@as(u8, 10), ctx.exitCode());

    const success_ctx = ErrorContext{ .code = .success };
    try std.testing.expectEqual(@as(u8, 0), success_ctx.exitCode());
}
