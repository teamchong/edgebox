/// WASI TTY Support
/// Provides terminal/TTY detection and raw mode for interactive CLI applications
const std = @import("std");

// WASI file descriptor types
pub const FILETYPE_UNKNOWN: u8 = 0;
pub const FILETYPE_BLOCK_DEVICE: u8 = 1;
pub const FILETYPE_CHARACTER_DEVICE: u8 = 2;
pub const FILETYPE_DIRECTORY: u8 = 3;
pub const FILETYPE_REGULAR_FILE: u8 = 4;
pub const FILETYPE_SOCKET_DGRAM: u8 = 5;
pub const FILETYPE_SOCKET_STREAM: u8 = 6;
pub const FILETYPE_SYMBOLIC_LINK: u8 = 7;

// Standard file descriptors
pub const STDIN_FD: i32 = 0;
pub const STDOUT_FD: i32 = 1;
pub const STDERR_FD: i32 = 2;

/// File descriptor stat structure (WASI)
pub const FdStat = extern struct {
    fs_filetype: u8,
    fs_flags: u16,
    fs_rights_base: u64,
    fs_rights_inheriting: u64,
};

// WASI imports for file descriptor operations
extern "wasi_snapshot_preview1" fn fd_fdstat_get(fd: i32, stat: *FdStat) u32;
extern "wasi_snapshot_preview1" fn fd_read(fd: i32, iovs: [*]const IoVec, iovs_len: usize, nread: *usize) u32;
extern "wasi_snapshot_preview1" fn fd_write(fd: i32, iovs: [*]const CIoVec, iovs_len: usize, nwritten: *usize) u32;

/// IoVec for reading
pub const IoVec = extern struct {
    buf: [*]u8,
    buf_len: usize,
};

/// IoVec for writing (const)
pub const CIoVec = extern struct {
    buf: [*]const u8,
    buf_len: usize,
};

/// Check if a file descriptor is a TTY (terminal)
/// In WASI, CHARACTER_DEVICE typically indicates a TTY
pub fn isatty(fd: i32) bool {
    var stat: FdStat = undefined;
    const ret = fd_fdstat_get(fd, &stat);
    if (ret != 0) return false;

    // Character devices are typically TTYs in WASI
    return stat.fs_filetype == FILETYPE_CHARACTER_DEVICE;
}

/// Check if stdin is a TTY
pub fn stdinIsatty() bool {
    return isatty(STDIN_FD);
}

/// Check if stdout is a TTY
pub fn stdoutIsatty() bool {
    return isatty(STDOUT_FD);
}

/// Check if stderr is a TTY
pub fn stderrIsatty() bool {
    return isatty(STDERR_FD);
}

/// Get terminal size (not available in standard WASI, returns default)
pub const TerminalSize = struct {
    rows: u16,
    cols: u16,
};

pub fn getTerminalSize() ?TerminalSize {
    // WASI doesn't have ioctl for TIOCGWINSZ
    // Return a reasonable default or try environment variables
    // In WasmEdge, terminal size might be passed via env vars
    return TerminalSize{
        .rows = 24,
        .cols = 80,
    };
}

/// Read a single character from stdin (blocking)
pub fn readChar() ?u8 {
    var buf: [1]u8 = undefined;
    var iov = [_]IoVec{.{ .buf = &buf, .buf_len = 1 }};
    var nread: usize = 0;

    const ret = fd_read(STDIN_FD, &iov, 1, &nread);
    if (ret != 0 or nread == 0) return null;
    return buf[0];
}

/// Read a line from stdin
pub fn readLine(allocator: std.mem.Allocator, max_len: usize) !?[]u8 {
    var result = std.ArrayList(u8){};
    errdefer result.deinit(allocator);

    while (result.items.len < max_len) {
        const ch = readChar() orelse {
            if (result.items.len == 0) return null;
            break;
        };

        if (ch == '\n') break;
        if (ch == '\r') continue; // Skip CR in CRLF

        try result.append(allocator, ch);
    }

    return try result.toOwnedSlice(allocator);
}

/// Write string to stdout
pub fn writeStdout(data: []const u8) !void {
    var remaining = data;
    while (remaining.len > 0) {
        var iov = [_]CIoVec{.{ .buf = remaining.ptr, .buf_len = remaining.len }};
        var nwritten: usize = 0;

        const ret = fd_write(STDOUT_FD, &iov, 1, &nwritten);
        if (ret != 0) return error.WriteError;
        if (nwritten == 0) return error.WriteError;

        remaining = remaining[nwritten..];
    }
}

/// Write string to stderr
pub fn writeStderr(data: []const u8) !void {
    var remaining = data;
    while (remaining.len > 0) {
        var iov = [_]CIoVec{.{ .buf = remaining.ptr, .buf_len = remaining.len }};
        var nwritten: usize = 0;

        const ret = fd_write(STDERR_FD, &iov, 1, &nwritten);
        if (ret != 0) return error.WriteError;
        if (nwritten == 0) return error.WriteError;

        remaining = remaining[nwritten..];
    }
}

// ANSI escape codes for terminal control
pub const ansi = struct {
    pub const RESET = "\x1b[0m";
    pub const BOLD = "\x1b[1m";
    pub const DIM = "\x1b[2m";
    pub const ITALIC = "\x1b[3m";
    pub const UNDERLINE = "\x1b[4m";

    // Colors
    pub const BLACK = "\x1b[30m";
    pub const RED = "\x1b[31m";
    pub const GREEN = "\x1b[32m";
    pub const YELLOW = "\x1b[33m";
    pub const BLUE = "\x1b[34m";
    pub const MAGENTA = "\x1b[35m";
    pub const CYAN = "\x1b[36m";
    pub const WHITE = "\x1b[37m";

    // Background colors
    pub const BG_BLACK = "\x1b[40m";
    pub const BG_RED = "\x1b[41m";
    pub const BG_GREEN = "\x1b[42m";
    pub const BG_YELLOW = "\x1b[43m";
    pub const BG_BLUE = "\x1b[44m";
    pub const BG_MAGENTA = "\x1b[45m";
    pub const BG_CYAN = "\x1b[46m";
    pub const BG_WHITE = "\x1b[47m";

    // Cursor control
    pub const CLEAR_SCREEN = "\x1b[2J";
    pub const CLEAR_LINE = "\x1b[2K";
    pub const CURSOR_HOME = "\x1b[H";
    pub const CURSOR_HIDE = "\x1b[?25l";
    pub const CURSOR_SHOW = "\x1b[?25h";

    /// Move cursor to position
    pub fn moveTo(row: u16, col: u16) [16]u8 {
        var buf: [16]u8 = undefined;
        _ = std.fmt.bufPrint(&buf, "\x1b[{d};{d}H", .{ row, col }) catch {};
        return buf;
    }

    /// Move cursor up N lines
    pub fn moveUp(n: u16) [8]u8 {
        var buf: [8]u8 = undefined;
        _ = std.fmt.bufPrint(&buf, "\x1b[{d}A", .{n}) catch {};
        return buf;
    }

    /// Move cursor down N lines
    pub fn moveDown(n: u16) [8]u8 {
        var buf: [8]u8 = undefined;
        _ = std.fmt.bufPrint(&buf, "\x1b[{d}B", .{n}) catch {};
        return buf;
    }
};
