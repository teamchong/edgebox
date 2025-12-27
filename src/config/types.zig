/// EdgeBox Configuration Types
///
/// Unified configuration structures used by all EdgeBox tools:
/// - edgebox (runtime)
/// - edgeboxd (daemon)
/// - edgeboxc (compiler)
const std = @import("std");
const path_utils = @import("path.zig");

/// Directory permission flags
pub const DirPerms = struct {
    path: []const u8,
    read: bool = false,
    write: bool = false,
    execute: bool = false, // Shell/spawn access to files in this dir

    /// Parse "rwx" string into permissions
    pub fn fromString(dir_path: []const u8, perms: []const u8) DirPerms {
        return DirPerms{
            .path = dir_path,
            .read = std.mem.indexOfScalar(u8, perms, 'r') != null,
            .write = std.mem.indexOfScalar(u8, perms, 'w') != null,
            .execute = std.mem.indexOfScalar(u8, perms, 'x') != null,
        };
    }

    /// Convert to "rwx" string representation
    pub fn toString(self: DirPerms, buf: *[3]u8) []const u8 {
        var len: usize = 0;
        if (self.read) {
            buf[len] = 'r';
            len += 1;
        }
        if (self.write) {
            buf[len] = 'w';
            len += 1;
        }
        if (self.execute) {
            buf[len] = 'x';
            len += 1;
        }
        return buf[0..len];
    }
};

/// Mount point for path remapping (Docker-style volumes)
pub const Mount = struct {
    host: []const u8, // Actual path on host filesystem
    guest: []const u8, // Path the app sees (e.g., $HOME)
};

/// Runtime limits configuration
pub const RuntimeConfig = struct {
    stack_size: u32 = 2 * 1024 * 1024, // 2MB stack
    heap_size: u32 = 16 * 1024 * 1024, // 16MB host heap
    max_memory_pages: u32 = 32768, // 2GB max linear memory (32768 * 64KB pages)
    max_instructions: i32 = -1, // CPU limit for interpreter mode (-1 = unlimited)
    exec_timeout_ms: u64 = 0, // Wall-clock timeout in ms (0 = unlimited)
    cpu_limit_seconds: u32 = 0, // CPU time limit via setrlimit (0 = unlimited)
    use_bump_allocator: bool = false, // Use bump allocator for serverless
};

/// HTTP security configuration
pub const HttpSecurityConfig = struct {
    allowed_urls: std.ArrayListUnmanaged([]const u8) = .{}, // Empty = allow all
    blocked_urls: std.ArrayListUnmanaged([]const u8) = .{},
    rate_limit_rps: u32 = 0, // 0 = unlimited
    max_connections: u32 = 100,

    pub fn deinit(self: *HttpSecurityConfig, alloc: std.mem.Allocator) void {
        for (self.allowed_urls.items) |url| {
            alloc.free(url);
        }
        self.allowed_urls.deinit(alloc);
        for (self.blocked_urls.items) |url| {
            alloc.free(url);
        }
        self.blocked_urls.deinit(alloc);
    }
};

/// Command permission entry
pub const CommandPermission = struct {
    name: []const u8,
    allow: []const []const u8 = &.{},
    deny: []const []const u8 = &.{},
    credentials: ?std.json.ObjectMap = null,
};

/// Command security configuration
pub const CommandConfig = struct {
    allow_commands: std.ArrayListUnmanaged([]const u8) = .{}, // Empty = allow all
    deny_commands: std.ArrayListUnmanaged([]const u8) = .{}, // Takes precedence
    permissions: []const CommandPermission = &.{},
    sensitive_files: []const []const u8 = &.{}, // Glob patterns
    blocked_patterns: []const []const u8 = &.{}, // Destructive command patterns
    use_keychain: bool = false,

    pub fn deinit(self: *CommandConfig, alloc: std.mem.Allocator) void {
        for (self.allow_commands.items) |cmd| {
            alloc.free(cmd);
        }
        self.allow_commands.deinit(alloc);
        for (self.deny_commands.items) |cmd| {
            alloc.free(cmd);
        }
        self.deny_commands.deinit(alloc);
    }
};

/// Daemon-specific configuration
pub const DaemonConfig = struct {
    pool_size: usize = 32,
    port: u16 = 8080,
    socket_path: []const u8 = "/tmp/edgebox.sock",
    socket_mode: SocketMode = .unix,
    reuse_instances: bool = false,
    enable_cow: bool = true,
    heap_size_mb: u32 = 64,

    pub const SocketMode = enum { unix, tcp };
};

/// Unified EdgeBox Configuration
/// All tools (edgebox, edgeboxd, edgeboxc) use subsets of this
pub const Config = struct {
    // === Identity ===
    name: ?[]const u8 = null,

    // === Directory Permissions ===
    dirs: std.ArrayListUnmanaged(DirPerms) = .{},
    mounts: std.ArrayListUnmanaged(Mount) = .{},

    // === Environment ===
    env_vars: std.ArrayListUnmanaged([]const u8) = .{}, // Format: "KEY=value"

    // === Runtime Limits ===
    runtime: RuntimeConfig = .{},

    // === HTTP Security ===
    http: HttpSecurityConfig = .{},

    // === Command Security ===
    commands: CommandConfig = .{},

    // === Daemon-specific ===
    daemon: ?DaemonConfig = null,

    /// Free all owned memory
    pub fn deinit(self: *Config, alloc: std.mem.Allocator) void {
        if (self.name) |n| alloc.free(n);

        for (self.dirs.items) |dir| {
            alloc.free(dir.path);
        }
        self.dirs.deinit(alloc);

        for (self.mounts.items) |mount| {
            alloc.free(mount.host);
            alloc.free(mount.guest);
        }
        self.mounts.deinit(alloc);

        for (self.env_vars.items) |env| {
            alloc.free(env);
        }
        self.env_vars.deinit(alloc);

        self.http.deinit(alloc);
        self.commands.deinit(alloc);
    }

    /// Remap a guest path to host path using mounts (longest match first)
    pub fn remapPath(self: *const Config, alloc: std.mem.Allocator, guest_path: []const u8) ?[]const u8 {
        var best_match: ?Mount = null;
        var best_len: usize = 0;

        for (self.mounts.items) |mount| {
            // Check exact match or path starts with mount.guest/
            if (std.mem.eql(u8, guest_path, mount.guest)) {
                if (mount.guest.len > best_len) {
                    best_match = mount;
                    best_len = mount.guest.len;
                }
            } else if (guest_path.len > mount.guest.len and
                std.mem.startsWith(u8, guest_path, mount.guest) and
                guest_path[mount.guest.len] == '/')
            {
                if (mount.guest.len > best_len) {
                    best_match = mount;
                    best_len = mount.guest.len;
                }
            }
        }

        if (best_match) |mount| {
            const rest = guest_path[mount.guest.len..];
            return std.fmt.allocPrint(alloc, "{s}{s}", .{ mount.host, rest }) catch null;
        }

        return null;
    }

    /// Check if path has read permission
    pub fn canRead(self: *const Config, check_path: []const u8) bool {
        for (self.dirs.items) |dir| {
            if (dir.read and path_utils.pathStartsWith(check_path, dir.path)) return true;
        }
        return false;
    }

    /// Check if path has write permission
    pub fn canWrite(self: *const Config, check_path: []const u8) bool {
        for (self.dirs.items) |dir| {
            if (dir.write and path_utils.pathStartsWith(check_path, dir.path)) return true;
        }
        return false;
    }

    /// Check if path has execute permission (for spawn/shell)
    pub fn canExecute(self: *const Config, check_path: []const u8) bool {
        for (self.dirs.items) |dir| {
            if (dir.execute and path_utils.pathStartsWith(check_path, dir.path)) return true;
        }
        return false;
    }

    /// Check if any execute permission exists (for general spawn access)
    pub fn hasAnyExecute(self: *const Config) bool {
        for (self.dirs.items) |dir| {
            if (dir.execute) return true;
        }
        return false;
    }

    /// Check if a command is allowed
    pub fn isCommandAllowed(self: *const Config, cmd_name: []const u8) bool {
        // Deny list takes precedence
        for (self.commands.deny_commands.items) |denied| {
            if (std.mem.eql(u8, cmd_name, denied)) return false;
        }

        // If allow list is empty, allow all (minus deny list)
        if (self.commands.allow_commands.items.len == 0) return true;

        // Check allow list
        for (self.commands.allow_commands.items) |allowed| {
            if (std.mem.eql(u8, cmd_name, allowed)) return true;
        }

        return false;
    }
};

// ============================================================================
// Tests
// ============================================================================

test "DirPerms.fromString" {
    const perms = DirPerms.fromString("/tmp", "rwx");
    try std.testing.expect(perms.read);
    try std.testing.expect(perms.write);
    try std.testing.expect(perms.execute);
    try std.testing.expectEqualStrings("/tmp", perms.path);
}

test "DirPerms.fromString read-only" {
    const perms = DirPerms.fromString("/var", "r");
    try std.testing.expect(perms.read);
    try std.testing.expect(!perms.write);
    try std.testing.expect(!perms.execute);
}

test "Config.canRead" {
    var config = Config{};
    defer config.deinit(std.testing.allocator);

    const path = std.testing.allocator.dupe(u8, "/tmp") catch unreachable;
    config.dirs.append(std.testing.allocator, DirPerms{
        .path = path,
        .read = true,
        .write = false,
        .execute = false,
    }) catch unreachable;

    try std.testing.expect(config.canRead("/tmp/foo"));
    try std.testing.expect(!config.canRead("/var/foo"));
}

test "Config.isCommandAllowed" {
    var config = Config{};
    defer config.deinit(std.testing.allocator);

    // Add deny command
    const denied = std.testing.allocator.dupe(u8, "rm") catch unreachable;
    config.commands.deny_commands.append(std.testing.allocator, denied) catch unreachable;

    try std.testing.expect(!config.isCommandAllowed("rm"));
    try std.testing.expect(config.isCommandAllowed("ls")); // Empty allow = allow all
}
