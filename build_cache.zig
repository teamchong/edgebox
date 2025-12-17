const std = @import("std");

/// Computes SHA256 hash of all source files in a directory tree
pub fn hashDirectory(allocator: std.mem.Allocator, dir_path: []const u8, extensions: []const []const u8) ![]const u8 {
    var hasher = std.crypto.hash.sha2.Sha256.init(.{});

    var dir = try std.fs.cwd().openDir(dir_path, .{ .iterate = true });
    defer dir.close();

    var walker = try dir.walk(allocator);
    defer walker.deinit();

    // Collect and sort paths for deterministic hashing
    var paths = std.ArrayListUnmanaged([]const u8){};
    defer {
        for (paths.items) |path| allocator.free(path);
        paths.deinit(allocator);
    }

    while (try walker.next()) |entry| {
        if (entry.kind != .file) continue;

        // Check if file has valid extension
        var has_ext = false;
        for (extensions) |ext| {
            if (std.mem.endsWith(u8, entry.basename, ext)) {
                has_ext = true;
                break;
            }
        }
        if (!has_ext) continue;

        const full_path = try std.fmt.allocPrint(allocator, "{s}/{s}", .{ dir_path, entry.path });
        try paths.append(allocator, full_path);
    }

    // Sort paths for deterministic hash
    std.mem.sort([]const u8, paths.items, {}, struct {
        fn lessThan(_: void, a: []const u8, b: []const u8) bool {
            return std.mem.lessThan(u8, a, b);
        }
    }.lessThan);

    // Hash each file
    for (paths.items) |path| {
        const file = std.fs.cwd().openFile(path, .{}) catch continue;
        defer file.close();

        // Hash file path first for uniqueness
        hasher.update(path);

        // Hash file contents
        var buf: [8192]u8 = undefined;
        while (true) {
            const bytes_read = try file.read(&buf);
            if (bytes_read == 0) break;
            hasher.update(buf[0..bytes_read]);
        }
    }

    var hash_bytes: [32]u8 = undefined;
    hasher.final(&hash_bytes);

    // Convert to hex string
    const hex = try allocator.alloc(u8, 64);
    const hex_str = std.fmt.bytesToHex(&hash_bytes, .lower);
    @memcpy(hex, &hex_str);
    return hex;
}

/// Check if prebuilt libraries exist and match source hash
pub fn shouldUsePrebuilt(allocator: std.mem.Allocator, prebuilt_dir: []const u8, source_dirs: []const []const u8) !bool {
    // Check if all required .a files exist
    const required_files = [_][]const u8{
        "wamr/libiwasm.a",
        "wamr/libaotclib.a",
        "wamr/libvmlib.a",
        "binaryen/libbinaryen.dylib",
    };

    for (required_files) |file| {
        const path = try std.fmt.allocPrint(allocator, "{s}/{s}", .{ prebuilt_dir, file });
        defer allocator.free(path);

        std.fs.cwd().access(path, .{}) catch return false;
    }

    // Compute current source hash
    const extensions = [_][]const u8{ ".c", ".h", ".cpp", ".hpp", "CMakeLists.txt" };
    var current_hash = std.crypto.hash.sha2.Sha256.init(.{});

    for (source_dirs) |dir| {
        const dir_hash = try hashDirectory(allocator, dir, &extensions);
        defer allocator.free(dir_hash);
        current_hash.update(dir_hash);
    }

    var hash_bytes: [32]u8 = undefined;
    current_hash.final(&hash_bytes);
    const hex_str = std.fmt.bytesToHex(&hash_bytes, .lower);
    const current_hash_hex = try allocator.alloc(u8, 64);
    @memcpy(current_hash_hex, &hex_str);
    defer allocator.free(current_hash_hex);

    // Read stored hash
    const hash_file_path = try std.fmt.allocPrint(allocator, "{s}/.build-hash", .{prebuilt_dir});
    defer allocator.free(hash_file_path);

    const stored_hash = std.fs.cwd().readFileAlloc(allocator, hash_file_path, 1024) catch return false;
    defer allocator.free(stored_hash);

    const stored_hash_trimmed = std.mem.trim(u8, stored_hash, &std.ascii.whitespace);

    return std.mem.eql(u8, current_hash_hex, stored_hash_trimmed);
}

/// Save current source hash to prebuilt directory
pub fn saveSourceHash(allocator: std.mem.Allocator, prebuilt_dir: []const u8, source_dirs: []const []const u8) !void {
    const extensions = [_][]const u8{ ".c", ".h", ".cpp", ".hpp", "CMakeLists.txt" };
    var hasher = std.crypto.hash.sha2.Sha256.init(.{});

    for (source_dirs) |dir| {
        const dir_hash = try hashDirectory(allocator, dir, &extensions);
        defer allocator.free(dir_hash);
        hasher.update(dir_hash);
    }

    var hash_bytes: [32]u8 = undefined;
    hasher.final(&hash_bytes);
    const hex_str = std.fmt.bytesToHex(&hash_bytes, .lower);
    const hash_hex = try std.fmt.allocPrint(allocator, "{s}\n", .{hex_str});
    defer allocator.free(hash_hex);

    const hash_file_path = try std.fmt.allocPrint(allocator, "{s}/.build-hash", .{prebuilt_dir});
    defer allocator.free(hash_file_path);

    const file = try std.fs.cwd().createFile(hash_file_path, .{});
    defer file.close();

    try file.writeAll(hash_hex);
}
