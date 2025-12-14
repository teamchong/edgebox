/// Filesystem Implementation for Component Model
/// Wraps std.fs operations as Component Model functions

const std = @import("std");
const native_registry = @import("../native_registry.zig");
const Value = native_registry.Value;
const NativeRegistry = native_registry.NativeRegistry;
const FileStat = native_registry.FileStat;
const DirEntry = native_registry.DirEntry;

/// Filesystem error enum (matches WIT fs-error)
pub const FsError = enum(u32) {
    not_found = 0,
    permission_denied = 1,
    already_exists = 2,
    invalid_path = 3,
    not_a_directory = 4,
    not_a_file = 5,
    directory_not_empty = 6,
    io_error = 7,
    invalid_encoding = 8,
};

/// Encoding enum (matches WIT encoding)
pub const Encoding = enum(u32) {
    utf8 = 0,
    utf16 = 1,
    latin1 = 2,
    base64 = 3,
    binary = 4,
};

/// Global allocator for filesystem operations
var fs_allocator: ?std.mem.Allocator = null;

/// Initialize filesystem implementation
pub fn init(allocator: std.mem.Allocator) void {
    fs_allocator = allocator;
}

/// Clean up filesystem implementation
pub fn deinit() void {
    fs_allocator = null;
}

/// Register filesystem implementations with native registry
pub fn registerFilesystemImpl(registry: *NativeRegistry) !void {
    try registry.register("filesystem", "read-file", readFile);
    try registry.register("filesystem", "write-file", writeFile);
    try registry.register("filesystem", "append-file", appendFile);
    try registry.register("filesystem", "exists", exists);
    try registry.register("filesystem", "stat", stat);
    try registry.register("filesystem", "remove-file", removeFile);
    try registry.register("filesystem", "rename", rename);
    try registry.register("filesystem", "copy-file", copyFile);
    try registry.register("filesystem", "read-dir", readDir);
    try registry.register("filesystem", "mkdir", mkdir);
    try registry.register("filesystem", "remove-dir", removeDir);
    try registry.register("filesystem", "access", access);
}

/// Map Zig errors to WIT fs-error enum
fn mapFsError(err: anyerror) u32 {
    return switch (err) {
        error.FileNotFound => @intFromEnum(FsError.not_found),
        error.AccessDenied => @intFromEnum(FsError.permission_denied),
        error.PathAlreadyExists => @intFromEnum(FsError.already_exists),
        error.InvalidPath => @intFromEnum(FsError.invalid_path),
        error.NotDir => @intFromEnum(FsError.not_a_directory),
        error.IsDir => @intFromEnum(FsError.not_a_file),
        error.DirNotEmpty => @intFromEnum(FsError.directory_not_empty),
        else => @intFromEnum(FsError.io_error),
    };
}

/// read-file: func(path: path, encoding: encoding) -> result<string, fs-error>
fn readFile(args: []const Value) !Value {
    const allocator = fs_allocator orelse return error.NotInitialized;
    const path = try args[0].asString();
    _ = try args[1].asU32(); // encoding - For PoC, we assume UTF-8

    // Read file
    const content = std.fs.cwd().readFileAlloc(
        allocator,
        path,
        100 * 1024 * 1024, // 100MB max
    ) catch |err| {
        return Value{ .err = mapFsError(err) };
    };

    // For PoC: content is owned by caller, they must free it
    // In production: would use arena allocator per-call
    return Value{ .ok_string = content };
}

/// write-file: func(path: path, data: string) -> result<_, fs-error>
fn writeFile(args: []const Value) !Value {
    const path = try args[0].asString();
    const data = try args[1].asString();

    // Write file (create or truncate)
    std.fs.cwd().writeFile(.{
        .sub_path = path,
        .data = data,
    }) catch |err| {
        return Value{ .err = mapFsError(err) };
    };

    return Value{ .ok_void = {} };
}

/// append-file: func(path: path, data: string) -> result<_, fs-error>
fn appendFile(args: []const Value) !Value {
    const path = try args[0].asString();
    const data = try args[1].asString();

    // Open file in append mode
    const file = std.fs.cwd().openFile(path, .{ .mode = .write_only }) catch |err| {
        // If file doesn't exist, create it
        if (err == error.FileNotFound) {
            return writeFile(args);
        }
        return Value{ .err = mapFsError(err) };
    };
    defer file.close();

    // Seek to end and write
    file.seekFromEnd(0) catch |err| {
        return Value{ .err = mapFsError(err) };
    };

    file.writeAll(data) catch |err| {
        return Value{ .err = mapFsError(err) };
    };

    return Value{ .ok_void = {} };
}

/// exists: func(path: path) -> bool
fn exists(args: []const Value) !Value {
    const path = try args[0].asString();

    // Try to access file
    std.fs.cwd().access(path, .{}) catch {
        return Value{ .bool = false };
    };

    return Value{ .bool = true };
}

/// stat: func(path: path) -> result<file-stat, fs-error>
fn stat(args: []const Value) !Value {
    const path = try args[0].asString();

    // Get file metadata
    const file_stat = std.fs.cwd().statFile(path) catch |err| {
        return Value{ .err = mapFsError(err) };
    };

    // Convert to WIT file-stat record
    const is_file = file_stat.kind == .file;
    const is_directory = file_stat.kind == .directory;

    const result = FileStat{
        .size = file_stat.size,
        .mode = if (is_directory) 0o40755 else 0o100644,
        .is_file = is_file,
        .is_directory = is_directory,
        .modified_time = @as(i64, @intCast(@divFloor(file_stat.mtime, std.time.ns_per_ms))),
        .created_time = @as(i64, @intCast(@divFloor(file_stat.ctime, std.time.ns_per_ms))),
        .accessed_time = @as(i64, @intCast(@divFloor(file_stat.atime, std.time.ns_per_ms))),
    };

    return Value{ .ok_file_stat = result };
}

/// remove-file: func(path: path) -> result<_, fs-error>
fn removeFile(args: []const Value) !Value {
    const path = try args[0].asString();

    std.fs.cwd().deleteFile(path) catch |err| {
        return Value{ .err = mapFsError(err) };
    };

    return Value{ .ok_void = {} };
}

/// rename: func(old-path: path, new-path: path) -> result<_, fs-error>
fn rename(args: []const Value) !Value {
    const old_path = try args[0].asString();
    const new_path = try args[1].asString();

    std.fs.cwd().rename(old_path, new_path) catch |err| {
        return Value{ .err = mapFsError(err) };
    };

    return Value{ .ok_void = {} };
}

/// copy-file: func(src-path: path, dest-path: path) -> result<_, fs-error>
fn copyFile(args: []const Value) !Value {
    const src_path = try args[0].asString();
    const dest_path = try args[1].asString();

    std.fs.cwd().copyFile(src_path, std.fs.cwd(), dest_path, .{}) catch |err| {
        return Value{ .err = mapFsError(err) };
    };

    return Value{ .ok_void = {} };
}

/// read-dir: func(path: path) -> result<list<dir-entry>, fs-error>
fn readDir(args: []const Value) !Value {
    const allocator = fs_allocator orelse return error.NotInitialized;
    const path = try args[0].asString();

    // Open directory
    var dir = std.fs.cwd().openDir(path, .{ .iterate = true }) catch |err| {
        return Value{ .err = mapFsError(err) };
    };
    defer dir.close();

    // Collect entries - count first
    var count: usize = 0;
    {
        var count_iter = dir.iterate();
        while (try count_iter.next()) |_| {
            count += 1;
        }
    }

    // Allocate and fill entries
    const entries = try allocator.alloc(DirEntry, count);
    errdefer allocator.free(entries);

    var i: usize = 0;
    var iterator = dir.iterate();
    while (try iterator.next()) |entry| {
        // Duplicate name string (owned by caller)
        const name_copy = try allocator.dupe(u8, entry.name);
        errdefer allocator.free(name_copy);

        entries[i] = DirEntry{
            .name = name_copy,
            .is_file = entry.kind == .file,
            .is_directory = entry.kind == .directory,
        };
        i += 1;
    }

    return Value{ .ok_dir_entries = entries };
}

/// mkdir: func(path: path, recursive: bool) -> result<_, fs-error>
fn mkdir(args: []const Value) !Value {
    const path = try args[0].asString();
    const recursive = try args[1].asBool();

    if (recursive) {
        std.fs.cwd().makePath(path) catch |err| {
            return Value{ .err = mapFsError(err) };
        };
    } else {
        std.fs.cwd().makeDir(path) catch |err| {
            return Value{ .err = mapFsError(err) };
        };
    }

    return Value{ .ok_void = {} };
}

/// remove-dir: func(path: path, recursive: bool) -> result<_, fs-error>
fn removeDir(args: []const Value) !Value {
    const path = try args[0].asString();
    const recursive = try args[1].asBool();

    if (recursive) {
        std.fs.cwd().deleteTree(path) catch |err| {
            return Value{ .err = mapFsError(err) };
        };
    } else {
        std.fs.cwd().deleteDir(path) catch |err| {
            return Value{ .err = mapFsError(err) };
        };
    }

    return Value{ .ok_void = {} };
}

/// access: func(path: path, mode: u32) -> bool
fn access(args: []const Value) !Value {
    const path = try args[0].asString();
    const mode = try args[1].asU32();

    // For PoC: simplified access check (mode ignored)
    _ = mode; // TODO: implement proper read/write/execute checks

    std.fs.cwd().access(path, .{}) catch {
        return Value{ .bool = false };
    };

    return Value{ .bool = true };
}
