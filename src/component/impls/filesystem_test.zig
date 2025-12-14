/// Tests for Filesystem Implementation

const std = @import("std");
const filesystem = @import("filesystem_impl.zig");
const native_registry = @import("../native_registry.zig");
const Value = native_registry.Value;

test "Filesystem - read and write file" {
    var registry = native_registry.NativeRegistry.init(std.testing.allocator);
    defer registry.deinit();

    filesystem.init(std.testing.allocator);
    defer filesystem.deinit();

    try filesystem.registerFilesystemImpl(&registry);

    // Write file
    const write_result = try registry.call("filesystem", "write-file", &[_]Value{
        Value{ .string = "/tmp/edgebox_test_write.txt" },
        Value{ .string = "Hello, Component Model!" },
    });
    try std.testing.expect(write_result.isOk());

    // Read file back
    const read_result = try registry.call("filesystem", "read-file", &[_]Value{
        Value{ .string = "/tmp/edgebox_test_write.txt" },
        Value{ .u32 = 0 }, // utf8 encoding
    });
    try std.testing.expect(read_result.isOk());
    const content = try read_result.asOkString();
    try std.testing.expectEqualStrings("Hello, Component Model!", content);

    // Cleanup
    std.testing.allocator.free(content);
    _ = try registry.call("filesystem", "remove-file", &[_]Value{
        Value{ .string = "/tmp/edgebox_test_write.txt" },
    });
}

test "Filesystem - append file" {
    var registry = native_registry.NativeRegistry.init(std.testing.allocator);
    defer registry.deinit();

    filesystem.init(std.testing.allocator);
    defer filesystem.deinit();

    try filesystem.registerFilesystemImpl(&registry);

    const test_path = "/tmp/edgebox_test_append.txt";

    // Write initial content
    _ = try registry.call("filesystem", "write-file", &[_]Value{
        Value{ .string = test_path },
        Value{ .string = "Line 1\n" },
    });

    // Append more content
    const append_result = try registry.call("filesystem", "append-file", &[_]Value{
        Value{ .string = test_path },
        Value{ .string = "Line 2\n" },
    });
    try std.testing.expect(append_result.isOk());

    // Read and verify
    const read_result = try registry.call("filesystem", "read-file", &[_]Value{
        Value{ .string = test_path },
        Value{ .u32 = 0 },
    });
    const content = try read_result.asOkString();
    try std.testing.expectEqualStrings("Line 1\nLine 2\n", content);

    // Cleanup
    std.testing.allocator.free(content);
    _ = try registry.call("filesystem", "remove-file", &[_]Value{
        Value{ .string = test_path },
    });
}

test "Filesystem - exists" {
    var registry = native_registry.NativeRegistry.init(std.testing.allocator);
    defer registry.deinit();

    filesystem.init(std.testing.allocator);
    defer filesystem.deinit();

    try filesystem.registerFilesystemImpl(&registry);

    const test_path = "/tmp/edgebox_test_exists.txt";

    // Should not exist initially
    var exists_result = try registry.call("filesystem", "exists", &[_]Value{
        Value{ .string = test_path },
    });
    try std.testing.expect(exists_result == .bool);
    try std.testing.expect(exists_result.bool == false);

    // Create file
    _ = try registry.call("filesystem", "write-file", &[_]Value{
        Value{ .string = test_path },
        Value{ .string = "test" },
    });

    // Should exist now
    exists_result = try registry.call("filesystem", "exists", &[_]Value{
        Value{ .string = test_path },
    });
    try std.testing.expect(exists_result.bool == true);

    // Cleanup
    _ = try registry.call("filesystem", "remove-file", &[_]Value{
        Value{ .string = test_path },
    });
}

test "Filesystem - stat" {
    var registry = native_registry.NativeRegistry.init(std.testing.allocator);
    defer registry.deinit();

    filesystem.init(std.testing.allocator);
    defer filesystem.deinit();

    try filesystem.registerFilesystemImpl(&registry);

    const test_path = "/tmp/edgebox_test_stat.txt";
    const test_content = "Hello, stat!";

    // Create file
    _ = try registry.call("filesystem", "write-file", &[_]Value{
        Value{ .string = test_path },
        Value{ .string = test_content },
    });

    // Get stat
    const stat_result = try registry.call("filesystem", "stat", &[_]Value{
        Value{ .string = test_path },
    });
    try std.testing.expect(stat_result.isOk());

    const file_stat = try stat_result.asOkFileStat();
    try std.testing.expectEqual(@as(u64, test_content.len), file_stat.size);
    try std.testing.expect(file_stat.is_file);
    try std.testing.expect(!file_stat.is_directory);

    // Cleanup
    _ = try registry.call("filesystem", "remove-file", &[_]Value{
        Value{ .string = test_path },
    });
}

test "Filesystem - directory operations" {
    var registry = native_registry.NativeRegistry.init(std.testing.allocator);
    defer registry.deinit();

    filesystem.init(std.testing.allocator);
    defer filesystem.deinit();

    try filesystem.registerFilesystemImpl(&registry);

    const test_dir = "/tmp/edgebox_test_dir";

    // Create directory
    const mkdir_result = try registry.call("filesystem", "mkdir", &[_]Value{
        Value{ .string = test_dir },
        Value{ .bool = false }, // non-recursive
    });
    try std.testing.expect(mkdir_result.isOk());

    // Check exists
    const exists_result = try registry.call("filesystem", "exists", &[_]Value{
        Value{ .string = test_dir },
    });
    try std.testing.expect(exists_result.bool == true);

    // Create a file in directory
    const file_path = "/tmp/edgebox_test_dir/file.txt";
    _ = try registry.call("filesystem", "write-file", &[_]Value{
        Value{ .string = file_path },
        Value{ .string = "content" },
    });

    // Read directory
    const readdir_result = try registry.call("filesystem", "read-dir", &[_]Value{
        Value{ .string = test_dir },
    });
    try std.testing.expect(readdir_result.isOk());

    const entries = try readdir_result.asOkDirEntries();
    try std.testing.expect(entries.len >= 1);

    // Find our file
    var found = false;
    for (entries) |entry| {
        if (std.mem.eql(u8, entry.name, "file.txt")) {
            found = true;
            try std.testing.expect(entry.is_file);
            try std.testing.expect(!entry.is_directory);
        }
    }
    try std.testing.expect(found);

    // Cleanup entries (strings are allocated)
    for (entries) |entry| {
        std.testing.allocator.free(entry.name);
    }
    std.testing.allocator.free(entries);

    // Remove directory (should fail - not empty)
    const rmdir_result = try registry.call("filesystem", "remove-dir", &[_]Value{
        Value{ .string = test_dir },
        Value{ .bool = false }, // non-recursive
    });
    try std.testing.expect(rmdir_result.isErr());

    // Remove directory recursively
    const rmdir_recursive_result = try registry.call("filesystem", "remove-dir", &[_]Value{
        Value{ .string = test_dir },
        Value{ .bool = true }, // recursive
    });
    try std.testing.expect(rmdir_recursive_result.isOk());
}

test "Filesystem - rename and copy" {
    var registry = native_registry.NativeRegistry.init(std.testing.allocator);
    defer registry.deinit();

    filesystem.init(std.testing.allocator);
    defer filesystem.deinit();

    try filesystem.registerFilesystemImpl(&registry);

    const original_path = "/tmp/edgebox_test_original.txt";
    const renamed_path = "/tmp/edgebox_test_renamed.txt";
    const copied_path = "/tmp/edgebox_test_copied.txt";
    const test_content = "test content";

    // Create original file
    _ = try registry.call("filesystem", "write-file", &[_]Value{
        Value{ .string = original_path },
        Value{ .string = test_content },
    });

    // Copy file
    const copy_result = try registry.call("filesystem", "copy-file", &[_]Value{
        Value{ .string = original_path },
        Value{ .string = copied_path },
    });
    try std.testing.expect(copy_result.isOk());

    // Verify copied file exists and has same content
    const read_copied = try registry.call("filesystem", "read-file", &[_]Value{
        Value{ .string = copied_path },
        Value{ .u32 = 0 },
    });
    const copied_content = try read_copied.asOkString();
    try std.testing.expectEqualStrings(test_content, copied_content);
    std.testing.allocator.free(copied_content);

    // Rename original file
    const rename_result = try registry.call("filesystem", "rename", &[_]Value{
        Value{ .string = original_path },
        Value{ .string = renamed_path },
    });
    try std.testing.expect(rename_result.isOk());

    // Verify original doesn't exist
    const exists_original = try registry.call("filesystem", "exists", &[_]Value{
        Value{ .string = original_path },
    });
    try std.testing.expect(exists_original.bool == false);

    // Verify renamed exists
    const exists_renamed = try registry.call("filesystem", "exists", &[_]Value{
        Value{ .string = renamed_path },
    });
    try std.testing.expect(exists_renamed.bool == true);

    // Cleanup
    _ = try registry.call("filesystem", "remove-file", &[_]Value{
        Value{ .string = renamed_path },
    });
    _ = try registry.call("filesystem", "remove-file", &[_]Value{
        Value{ .string = copied_path },
    });
}

test "Filesystem - error handling" {
    var registry = native_registry.NativeRegistry.init(std.testing.allocator);
    defer registry.deinit();

    filesystem.init(std.testing.allocator);
    defer filesystem.deinit();

    try filesystem.registerFilesystemImpl(&registry);

    // Read non-existent file
    const read_result = try registry.call("filesystem", "read-file", &[_]Value{
        Value{ .string = "/nonexistent/file.txt" },
        Value{ .u32 = 0 },
    });
    try std.testing.expect(read_result.isErr());
    const err_code = try read_result.asErr();
    try std.testing.expectEqual(@as(u32, 0), err_code); // not-found

    // Stat non-existent file
    const stat_result = try registry.call("filesystem", "stat", &[_]Value{
        Value{ .string = "/nonexistent/file.txt" },
    });
    try std.testing.expect(stat_result.isErr());

    // Remove non-existent file
    const remove_result = try registry.call("filesystem", "remove-file", &[_]Value{
        Value{ .string = "/nonexistent/file.txt" },
    });
    try std.testing.expect(remove_result.isErr());
}

test "Filesystem - recursive mkdir" {
    var registry = native_registry.NativeRegistry.init(std.testing.allocator);
    defer registry.deinit();

    filesystem.init(std.testing.allocator);
    defer filesystem.deinit();

    try filesystem.registerFilesystemImpl(&registry);

    const nested_dir = "/tmp/edgebox_test_a/b/c";

    // Create nested directories recursively
    const mkdir_result = try registry.call("filesystem", "mkdir", &[_]Value{
        Value{ .string = nested_dir },
        Value{ .bool = true }, // recursive
    });
    try std.testing.expect(mkdir_result.isOk());

    // Verify exists
    const exists_result = try registry.call("filesystem", "exists", &[_]Value{
        Value{ .string = nested_dir },
    });
    try std.testing.expect(exists_result.bool == true);

    // Cleanup
    _ = try registry.call("filesystem", "remove-dir", &[_]Value{
        Value{ .string = "/tmp/edgebox_test_a" },
        Value{ .bool = true },
    });
}

test "Filesystem - access check" {
    var registry = native_registry.NativeRegistry.init(std.testing.allocator);
    defer registry.deinit();

    filesystem.init(std.testing.allocator);
    defer filesystem.deinit();

    try filesystem.registerFilesystemImpl(&registry);

    const test_path = "/tmp/edgebox_test_access.txt";

    // Create file
    _ = try registry.call("filesystem", "write-file", &[_]Value{
        Value{ .string = test_path },
        Value{ .string = "test" },
    });

    // Check access (read mode)
    const access_result = try registry.call("filesystem", "access", &[_]Value{
        Value{ .string = test_path },
        Value{ .u32 = 4 }, // read mode
    });
    try std.testing.expect(access_result == .bool);
    try std.testing.expect(access_result.bool == true);

    // Check access on non-existent file
    const access_none = try registry.call("filesystem", "access", &[_]Value{
        Value{ .string = "/nonexistent.txt" },
        Value{ .u32 = 4 },
    });
    try std.testing.expect(access_none.bool == false);

    // Cleanup
    _ = try registry.call("filesystem", "remove-file", &[_]Value{
        Value{ .string = test_path },
    });
}
