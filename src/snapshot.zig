/// EdgeBox Snapshot System
///
/// Implements zero-latency cold starts by serializing QuickJS state to disk
/// after initialization, then restoring it in <1ms instead of re-parsing.
///
/// Snapshot Format:
/// ┌─────────────────┐
/// │ SnapshotHeader  │ 64 bytes
/// ├─────────────────┤
/// │ Bytecode Data   │ Variable (compiled JS)
/// ├─────────────────┤
/// │ Global State    │ Variable (object graph)
/// └─────────────────┘
///
/// Usage:
/// ```zig
/// // Create snapshot after initialization
/// try snapshot.createSnapshot(ctx, "app.snapshot");
///
/// // Restore from snapshot (fast path)
/// var ctx = try snapshot.loadSnapshot(allocator, "app.snapshot");
/// ```
const std = @import("std");
const quickjs = @import("quickjs_core.zig");
const Allocator = std.mem.Allocator;

/// Magic bytes identifying EdgeBox snapshot files
const SNAPSHOT_MAGIC: [4]u8 = .{ 'E', 'D', 'G', 'E' };

/// Current snapshot format version
const SNAPSHOT_VERSION: u32 = 1;

/// Snapshot file header (64 bytes, aligned)
pub const SnapshotHeader = extern struct {
    /// Magic bytes: "EDGE"
    magic: [4]u8 = SNAPSHOT_MAGIC,

    /// Snapshot format version
    version: u32 = SNAPSHOT_VERSION,

    /// QuickJS engine version hash (for compatibility check)
    quickjs_version: u32 = 0,

    /// FNV-1a hash of polyfill sources (cache invalidation)
    polyfills_hash: u64 = 0,

    /// Offset to bytecode section from start of file
    bytecode_offset: u32 = @sizeOf(SnapshotHeader),

    /// Length of bytecode section in bytes
    bytecode_len: u32 = 0,

    /// Offset to global state section from start of file
    global_state_offset: u32 = 0,

    /// Length of global state section in bytes
    global_state_len: u32 = 0,

    /// Timestamp when snapshot was created (unix ms)
    created_at: u64 = 0,

    /// Reserved for future use
    _reserved: [24]u8 = [_]u8{0} ** 24,

    /// Validate header magic and version
    pub fn validate(self: *const SnapshotHeader) bool {
        if (!std.mem.eql(u8, &self.magic, &SNAPSHOT_MAGIC)) {
            return false;
        }
        if (self.version != SNAPSHOT_VERSION) {
            return false;
        }
        return true;
    }
};

/// Errors specific to snapshot operations
pub const SnapshotError = error{
    InvalidMagic,
    VersionMismatch,
    CorruptedData,
    PolyfillMismatch,
    FileNotFound,
    ReadError,
    WriteError,
    OutOfMemory,
};

/// Create a bytecode-only snapshot (faster, more reliable than full state)
/// This is the recommended approach - avoids QuickJS global state serialization limitations
pub fn createSnapshotBytecodeOnly(
    bytecode: []const u8,
    output_path: []const u8,
    polyfills_hash: u64,
) !void {
    // Calculate offsets
    const header_size = @sizeOf(SnapshotHeader);
    const bytecode_len: u32 = @intCast(bytecode.len);

    // Build header (no global state)
    var header = SnapshotHeader{
        .bytecode_len = bytecode_len,
        .global_state_offset = @intCast(header_size + bytecode_len),
        .global_state_len = 0, // No global state
        .polyfills_hash = polyfills_hash,
        .created_at = @intCast(std.time.milliTimestamp()),
    };

    // Extract directory and filename from path
    const dir_path = std.fs.path.dirname(output_path) orelse ".";
    const filename = std.fs.path.basename(output_path);

    // Open directory (works better with WASI path preopens)
    var dir = std.fs.cwd().openDir(dir_path, .{}) catch |err| {
        std.debug.print("Failed to open directory '{s}': {}\n", .{ dir_path, err });
        return SnapshotError.WriteError;
    };
    defer dir.close();

    // Create file in that directory
    const file = dir.createFile(filename, .{}) catch |err| {
        std.debug.print("Failed to create file '{s}': {}\n", .{ filename, err });
        return SnapshotError.WriteError;
    };
    defer file.close();

    // Write header
    file.writeAll(std.mem.asBytes(&header)) catch {
        return SnapshotError.WriteError;
    };

    // Write bytecode
    file.writeAll(bytecode) catch {
        return SnapshotError.WriteError;
    };
}

/// Create a snapshot of the current QuickJS context state
/// Saves both compiled bytecode and global object state to a file
/// NOTE: Global state serialization has limitations - use createSnapshotBytecodeOnly for reliability
pub fn createSnapshot(
    ctx: *quickjs.Context,
    bytecode: ?[]const u8,
    output_path: []const u8,
    polyfills_hash: u64,
    allocator: Allocator,
) !void {
    // Serialize global state
    const global_state = ctx.serializeGlobalState(allocator) catch {
        return SnapshotError.OutOfMemory;
    };
    defer allocator.free(global_state);

    // Calculate offsets
    const header_size = @sizeOf(SnapshotHeader);
    const bytecode_len: u32 = if (bytecode) |bc| @intCast(bc.len) else 0;
    const global_offset: u32 = @intCast(header_size + bytecode_len);

    // Build header
    var header = SnapshotHeader{
        .bytecode_len = bytecode_len,
        .global_state_offset = global_offset,
        .global_state_len = @intCast(global_state.len),
        .polyfills_hash = polyfills_hash,
        .created_at = @intCast(std.time.milliTimestamp()),
    };

    // Write to file
    const file = std.fs.cwd().createFile(output_path, .{}) catch {
        return SnapshotError.WriteError;
    };
    defer file.close();

    // Write header
    file.writeAll(std.mem.asBytes(&header)) catch {
        return SnapshotError.WriteError;
    };

    // Write bytecode (if any)
    if (bytecode) |bc| {
        file.writeAll(bc) catch {
            return SnapshotError.WriteError;
        };
    }

    // Write global state
    file.writeAll(global_state) catch {
        return SnapshotError.WriteError;
    };
}

/// Result from loading a snapshot
pub const LoadResult = struct {
    /// The restored QuickJS runtime (must be kept alive)
    runtime: quickjs.Runtime,

    /// The restored QuickJS context
    context: quickjs.Context,

    /// Bytecode that was stored (if any) - caller must free
    bytecode: ?[]u8,

    /// Allocator used
    allocator: Allocator,

    pub fn deinit(self: *LoadResult) void {
        if (self.bytecode) |bc| {
            self.allocator.free(bc);
        }
        self.context.deinit();
        // Note: Runtime will be deinitialized when LoadResult goes out of scope
        // but we're copying the struct so we need to be careful
    }
};

/// Load a snapshot from disk and restore QuickJS state
/// Returns a context with global state restored
/// NOTE: Native bindings must be re-registered after loading!
pub fn loadSnapshot(
    allocator: Allocator,
    path: []const u8,
    expected_polyfills_hash: ?u64,
) !LoadResult {
    // Open and read file
    const file = std.fs.cwd().openFile(path, .{}) catch {
        return SnapshotError.FileNotFound;
    };
    defer file.close();

    // Read header using pread
    var header: SnapshotHeader = undefined;
    const header_bytes_read = file.pread(std.mem.asBytes(&header), 0) catch {
        return SnapshotError.ReadError;
    };
    if (header_bytes_read != @sizeOf(SnapshotHeader)) {
        return SnapshotError.ReadError;
    }

    // Validate header
    if (!header.validate()) {
        return SnapshotError.InvalidMagic;
    }

    // Check polyfills hash if provided (cache invalidation)
    if (expected_polyfills_hash) |expected| {
        if (header.polyfills_hash != expected) {
            return SnapshotError.PolyfillMismatch;
        }
    }

    // Read bytecode
    var bytecode: ?[]u8 = null;
    if (header.bytecode_len > 0) {
        bytecode = try allocator.alloc(u8, header.bytecode_len);
        errdefer allocator.free(bytecode.?);

        const bytecode_read = file.pread(bytecode.?, header.bytecode_offset) catch {
            return SnapshotError.ReadError;
        };
        if (bytecode_read != header.bytecode_len) {
            return SnapshotError.ReadError;
        }
    }
    errdefer if (bytecode) |bc| allocator.free(bc);

    // Create fresh runtime and context with std module
    var runtime = quickjs.Runtime.init(allocator) catch {
        return SnapshotError.OutOfMemory;
    };
    errdefer runtime.deinit();

    var context = runtime.newStdContext() catch {
        return SnapshotError.OutOfMemory;
    };
    errdefer context.deinit();

    // Read and restore global state (only if present)
    if (header.global_state_len > 0) {
        const global_state = try allocator.alloc(u8, header.global_state_len);
        defer allocator.free(global_state);

        const global_read = file.pread(global_state, header.global_state_offset) catch {
            return SnapshotError.ReadError;
        };
        if (global_read != header.global_state_len) {
            return SnapshotError.ReadError;
        }

        context.restoreGlobalState(global_state) catch {
            return SnapshotError.CorruptedData;
        };
    }

    return LoadResult{
        .runtime = runtime,
        .context = context,
        .bytecode = bytecode,
        .allocator = allocator,
    };
}

/// Compute FNV-1a hash for polyfill source (cache key)
pub fn hashPolyfills(sources: []const []const u8) u64 {
    var hash: u64 = 0xcbf29ce484222325; // FNV offset basis
    for (sources) |source| {
        for (source) |byte| {
            hash ^= byte;
            hash *%= 0x100000001b3; // FNV prime
        }
    }
    return hash;
}

/// Check if a snapshot file exists and is valid
pub fn validateSnapshot(path: []const u8, expected_polyfills_hash: ?u64) bool {
    const file = std.fs.cwd().openFile(path, .{}) catch {
        return false;
    };
    defer file.close();

    var header: SnapshotHeader = undefined;
    const bytes_read = file.pread(std.mem.asBytes(&header), 0) catch {
        return false;
    };
    if (bytes_read != @sizeOf(SnapshotHeader)) {
        return false;
    }

    if (!header.validate()) {
        return false;
    }

    if (expected_polyfills_hash) |expected| {
        if (header.polyfills_hash != expected) {
            return false;
        }
    }

    return true;
}

// ============================================================================
// Tests
// ============================================================================

test "snapshot header size" {
    // Ensure header is exactly 64 bytes for alignment
    try std.testing.expectEqual(@as(usize, 64), @sizeOf(SnapshotHeader));
}

test "snapshot header validation" {
    var header = SnapshotHeader{};
    try std.testing.expect(header.validate());

    header.magic = .{ 'B', 'A', 'D', '!' };
    try std.testing.expect(!header.validate());
}

test "polyfills hash" {
    const sources = [_][]const u8{
        "console.log('hello');",
        "globalThis.x = 42;",
    };
    const hash1 = hashPolyfills(&sources);
    const hash2 = hashPolyfills(&sources);
    try std.testing.expectEqual(hash1, hash2);

    const different = [_][]const u8{
        "console.log('world');",
    };
    const hash3 = hashPolyfills(&different);
    try std.testing.expect(hash1 != hash3);
}
