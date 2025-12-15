/// Filesystem Component Integration Test
/// Tests filesystem through the native registry
///
/// Architecture Note (Phase 4):
/// The WIT parser now fully supports result/list/record/enum types (proven by wit_parser tests).
/// However, filesystem.wit defines types inside the interface block (interface-scoped types),
/// while ComponentAdapter's resolveType() expects types at package level.
///
/// This test uses NativeRegistry directly, which is appropriate because:
/// 1. It tests the filesystem implementation correctness
/// 2. It validates the Value enum's complex type support
/// 3. ComponentAdapter integration would require either:
///    - Moving types outside interface in filesystem.wit (structural change), OR
///    - Adding interface.types storage to Package (architecture change)
///
/// For proof that ComponentAdapter works with complex types at package level,
/// see adapter.zig tests which exercise the full stack with timer.wit (package-level types).

const std = @import("std");
const NativeRegistry = @import("native_registry.zig").NativeRegistry;
const filesystem = @import("impls/filesystem_impl.zig");
const Value = @import("native_registry.zig").Value;

test "Filesystem Component - integration test" {
    var registry = NativeRegistry.init(std.testing.allocator);
    defer registry.deinit();

    // Initialize filesystem implementation
    filesystem.init(std.testing.allocator);
    defer filesystem.deinit();

    // Register filesystem implementation
    try filesystem.registerFilesystemImpl(&registry);

    const test_path = "/tmp/component_fs_test.txt";
    const test_content = "Component Model Filesystem Works!";

    // Test write-file
    const write_result = try registry.call("filesystem", "write-file", &[_]Value{
        Value{ .string = test_path },
        Value{ .string = test_content },
    });
    try std.testing.expect(write_result.isOk());

    // Test exists
    const exists_result = try registry.call("filesystem", "exists", &[_]Value{
        Value{ .string = test_path },
    });
    try std.testing.expect(exists_result == .bool);
    try std.testing.expect(exists_result.bool == true);

    // Test read-file
    const read_result = try registry.call("filesystem", "read-file", &[_]Value{
        Value{ .string = test_path },
        Value{ .u32 = 0 }, // utf8 encoding
    });
    try std.testing.expect(read_result.isOk());
    const content = try read_result.asOkString();
    try std.testing.expectEqualStrings(test_content, content);
    std.testing.allocator.free(content);

    // Test stat
    const stat_result = try registry.call("filesystem", "stat", &[_]Value{
        Value{ .string = test_path },
    });
    try std.testing.expect(stat_result.isOk());
    const file_stat = try stat_result.asOkFileStat();
    try std.testing.expectEqual(@as(u64, test_content.len), file_stat.size);
    try std.testing.expect(file_stat.is_file);

    // Test append-file
    const append_result = try registry.call("filesystem", "append-file", &[_]Value{
        Value{ .string = test_path },
        Value{ .string = "\nAppended!" },
    });
    try std.testing.expect(append_result.isOk());

    // Test remove-file
    const remove_result = try registry.call("filesystem", "remove-file", &[_]Value{
        Value{ .string = test_path },
    });
    try std.testing.expect(remove_result.isOk());

    // Verify file is gone
    const exists_after = try registry.call("filesystem", "exists", &[_]Value{
        Value{ .string = test_path },
    });
    try std.testing.expect(exists_after.bool == false);
}

test "Filesystem Component - directory operations" {
    var registry = NativeRegistry.init(std.testing.allocator);
    defer registry.deinit();

    filesystem.init(std.testing.allocator);
    defer filesystem.deinit();

    try filesystem.registerFilesystemImpl(&registry);

    const test_dir = "/tmp/component_fs_test_dir";

    // Create directory
    const mkdir_result = try registry.call("filesystem", "mkdir", &[_]Value{
        Value{ .string = test_dir },
        Value{ .bool = false },
    });
    try std.testing.expect(mkdir_result.isOk());

    // Create a file in directory
    _ = try registry.call("filesystem", "write-file", &[_]Value{
        Value{ .string = "/tmp/component_fs_test_dir/test.txt" },
        Value{ .string = "content" },
    });

    // Read directory
    const readdir_result = try registry.call("filesystem", "read-dir", &[_]Value{
        Value{ .string = test_dir },
    });
    try std.testing.expect(readdir_result.isOk());

    const entries = try readdir_result.asOkDirEntries();
    try std.testing.expect(entries.len >= 1);

    // Cleanup
    for (entries) |entry| {
        std.testing.allocator.free(entry.name);
    }
    std.testing.allocator.free(entries);

    // Remove directory recursively
    _ = try registry.call("filesystem", "remove-dir", &[_]Value{
        Value{ .string = test_dir },
        Value{ .bool = true },
    });
}

test "Filesystem Component - error handling" {
    var registry = NativeRegistry.init(std.testing.allocator);
    defer registry.deinit();

    filesystem.init(std.testing.allocator);
    defer filesystem.deinit();

    try filesystem.registerFilesystemImpl(&registry);

    // Try to read non-existent file
    const read_result = try registry.call("filesystem", "read-file", &[_]Value{
        Value{ .string = "/nonexistent/file.txt" },
        Value{ .u32 = 0 },
    });
    try std.testing.expect(read_result.isErr());
    const err_code = try read_result.asErr();
    try std.testing.expectEqual(@as(u32, 0), err_code); // not-found
}
