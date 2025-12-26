/// EdgeBox Configuration Module
///
/// Unified configuration system used by all EdgeBox tools.
///
/// Usage:
/// ```zig
/// const config = @import("config");
///
/// var cfg = try config.load(allocator, .{});
/// defer cfg.deinit(allocator);
///
/// if (cfg.canRead("/tmp/foo")) {
///     // ...
/// }
/// ```
pub const types = @import("types.zig");
pub const loader = @import("loader.zig");
pub const path = @import("path.zig");
pub const env = @import("env.zig");

// Re-export main types for convenience
pub const Config = types.Config;
pub const DirPerms = types.DirPerms;
pub const Mount = types.Mount;
pub const RuntimeConfig = types.RuntimeConfig;
pub const HttpSecurityConfig = types.HttpSecurityConfig;
pub const CommandConfig = types.CommandConfig;
pub const DaemonConfig = types.DaemonConfig;

// Re-export loader functions
pub const load = loader.load;
pub const LoadOptions = loader.LoadOptions;
pub const ConfigSections = loader.ConfigSections;

// Re-export path utilities
pub const expandPath = path.expandPath;
pub const pathStartsWith = path.pathStartsWith;
pub const normalizePath = path.normalizePath;

// Re-export env utilities
pub const loadEnvFile = env.loadEnvFile;
pub const getEnvVar = env.getEnvVar;
pub const isEnvSet = env.isEnvSet;

test {
    // Run all module tests
    @import("std").testing.refAllDecls(@This());
}
