/// Spawn Dispatch Implementation
///
/// Handles process spawning operations with security controls:
/// - Command allow/deny lists
/// - Shell metacharacter blocking
/// - Sensitive file access checks
/// - Destructive command pattern detection
/// - Credential injection for authorized commands
/// - Emulator support (git, npm, etc.)
/// - Memory limits (per-spawn and global)

const std = @import("std");
const runtime = @import("../runtime.zig");
const shell_parser = @import("../shell_parser.zig");
const emulators = @import("../component/emulators/mod.zig");
const errors = @import("../errors.zig");
const wasm_helpers = @import("../wasm_helpers.zig");

const c = wasm_helpers.c;
const readWasmMemory = wasm_helpers.readWasmMemory;
const writeWasmMemory = wasm_helpers.writeWasmMemory;

// =============================================================================
// Public Types and Constants
// =============================================================================

/// Spawn operation opcodes
pub const SPAWN_OP_START: i32 = 0;
pub const SPAWN_OP_POLL: i32 = 1;
pub const SPAWN_OP_OUTPUT_LEN: i32 = 2;
pub const SPAWN_OP_OUTPUT: i32 = 3;
pub const SPAWN_OP_FREE: i32 = 4;

/// Max concurrent async spawn operations
pub const MAX_ASYNC_OPS: usize = 64;

/// Memory limits for spawn operations
pub const MAX_TOTAL_SPAWN_MEMORY: u64 = 100 * 1024 * 1024; // 100MB total for all spawns
pub const MAX_PER_SPAWN_MEMORY: u64 = 2 * 1024 * 1024; // 2MB per spawn

/// Spawn operation status
pub const SpawnOpStatus = enum { pending, complete, error_state };

/// Async spawn request state
pub const AsyncSpawnRequest = struct {
    id: u32,
    status: SpawnOpStatus,
    exit_code: i32,
    stdout_data: ?[]u8,
    stderr_data: ?[]u8,
};

// =============================================================================
// Security Constants
// =============================================================================

/// Dangerous commands that should be blocked when receiving piped input
const dangerous_pipe_targets = [_][]const u8{
    "tee",    // Can write to files
    "xargs",  // Can execute arbitrary commands
    "sh",     // Shell execution
    "bash",   // Shell execution
    "eval",   // Arbitrary evaluation
    "exec",   // Replace process
    "source", // Execute file
    ".",      // Execute file (POSIX)
};

/// Sensitive environment variables that should not be exposed
const sensitive_env_vars = [_][]const u8{
    "API_KEY",
    "ANTHROPIC_API_KEY",
    "AWS_SECRET_ACCESS_KEY",
    "AWS_SECRET",
    "GH_TOKEN",
    "GITHUB_TOKEN",
    "NPM_TOKEN",
    "DATABASE_URL",
    "DB_PASSWORD",
    "SECRET_KEY",
    "PRIVATE_KEY",
};

// =============================================================================
// Module State
// =============================================================================

/// Global spawn operation slots
pub var g_spawn_ops: [MAX_ASYNC_OPS]?AsyncSpawnRequest = [_]?AsyncSpawnRequest{null} ** MAX_ASYNC_OPS;

/// Next spawn request ID
pub var g_next_spawn_id: u32 = 1;

/// Global JSON buffer for spawn output (needed because we return length first then data)
pub var g_spawn_json_buf: ?[]u8 = null;
pub var g_spawn_json_id: u32 = 0;

/// Global spawn memory tracking to prevent resource exhaustion
pub var g_spawn_memory_used: std.atomic.Value(u64) = std.atomic.Value(u64).init(0);

// =============================================================================
// Configuration Interface
// =============================================================================

/// Configuration struct for spawn security
pub const Config = struct {
    dirs: std.ArrayListUnmanaged(DirPerms),
    allow_commands: std.ArrayListUnmanaged([]const u8),
    deny_commands: std.ArrayListUnmanaged([]const u8),
    commands: []const runtime.CommandPermission,
    sensitive_files: []const []const u8,
    blocked_patterns: []const []const u8,

    pub fn hasAnyExecute(self: *const Config) bool {
        for (self.dirs.items) |dir| {
            if (dir.execute) return true;
        }
        return false;
    }
};

pub const DirPerms = struct {
    path: []const u8,
    read: bool = false,
    write: bool = false,
    execute: bool = false,
};

// =============================================================================
// Main Dispatch Function
// =============================================================================

/// Main spawn dispatch entry point (exported to WASM)
pub export fn __edgebox_spawn_dispatch(exec_env: c.wasm_exec_env_t, opcode: i32, a1: i32, a2: i32, a3: i32, a4: i32) i32 {
    const result = switch (opcode) {
        SPAWN_OP_START => spawnStart(exec_env, @bitCast(a1), @bitCast(a2), @bitCast(a3), @bitCast(a4)),
        SPAWN_OP_POLL => spawnPoll(@bitCast(a1)),
        SPAWN_OP_OUTPUT_LEN => spawnOutputLen(@bitCast(a1)),
        SPAWN_OP_OUTPUT => spawnOutput(exec_env, @bitCast(a1), @bitCast(a2)),
        SPAWN_OP_FREE => spawnFree(@bitCast(a1)),
        else => -1,
    };

    return result;
}

// =============================================================================
// Spawn Operations
// =============================================================================

fn spawnStart(exec_env: c.wasm_exec_env_t, cmd_ptr: u32, cmd_len: u32, args_ptr: u32, args_len: u32) i32 {
    // Get allocator and config from module-level globals
    const allocator = std.heap.page_allocator;

    // Cleanup any stale JSON buffer from previous spawn (in case JS crashed without calling spawnFree)
    if (g_spawn_json_buf) |buf| {
        allocator.free(buf);
        g_spawn_json_buf = null;
    }

    // Security check: require execute permission on at least one directory
    // Note: This assumes g_config is set by the main module
    const cfg = getConfig() orelse {
        std.debug.print("[SPAWN DENIED] No config loaded - shell access denied by default\n", .{});
        return @intFromEnum(errors.ErrorCode.permission_denied);
    };
    if (!cfg.hasAnyExecute()) {
        std.debug.print("[SPAWN DENIED] No execute permission granted in .edgebox.json dirs\n", .{});
        return @intFromEnum(errors.ErrorCode.permission_denied);
    }

    const cmd = readWasmMemory(exec_env, cmd_ptr, cmd_len) orelse return @intFromEnum(errors.ErrorCode.wasm_memory_error);
    const args_json = if (args_len > 0) readWasmMemory(exec_env, args_ptr, args_len) else null;

    // Command allow/deny filtering
    const cmd_name = extractCmdName(cmd);

    // Deny list takes precedence
    if (cfg.deny_commands.items.len > 0 and cmdInList(cmd_name, cfg.deny_commands.items)) {
        std.debug.print("[SPAWN DENIED] Command '{s}' is in deny list\n", .{cmd_name});
        return @intFromEnum(errors.ErrorCode.command_in_deny_list);
    }

    // If allow list is set, command must be in it
    if (cfg.allow_commands.items.len > 0 and !cmdInList(cmd_name, cfg.allow_commands.items)) {
        std.debug.print("[SPAWN DENIED] Command '{s}' is not in allow list\n", .{cmd_name});
        return @intFromEnum(errors.ErrorCode.command_not_in_allow_list);
    }

    // SECURITY LAYER 2: Command AST Parsing - Check blocked patterns
    if (cfg.blocked_patterns.len > 0 and isCommandDestructive(allocator, cmd, cfg.blocked_patterns)) {
        std.debug.print("[SPAWN DENIED] Command matches blocked pattern: {s}\n", .{cmd});
        return @intFromEnum(errors.ErrorCode.destructive_command_blocked);
    }

    // SECURITY LAYER 3: Sensitive File Access Check
    if (cfg.sensitive_files.len > 0 and accessesSensitiveFile(allocator, cmd, cfg.sensitive_files)) {
        std.debug.print("[SPAWN DENIED] Command accesses sensitive file: {s}\n", .{cmd});
        return @intFromEnum(errors.ErrorCode.sensitive_file_blocked);
    }

    // EMULATOR LAYER: Check if command should be emulated
    const binary_name = extractBinaryName(cmd);
    const cmd_perm = findCommandPermission(cfg.commands, binary_name);
    if (cmd_perm) |perm| {
        if (perm.emulator_component) |emulator_name| {
            // Parse command arguments for emulator
            var emulator_args = std.ArrayListUnmanaged([]const u8){};
            defer emulator_args.deinit(allocator);

            // Parse command to extract args (skip the binary name)
            var tokenizer = std.mem.tokenizeScalar(u8, cmd, ' ');
            _ = tokenizer.next(); // Skip the command itself
            while (tokenizer.next()) |arg| {
                emulator_args.append(allocator, arg) catch continue;
            }

            // Try the emulator
            if (emulators.tryEmulate(emulator_name, emulator_args.items, perm.output_mode)) |result| {
                // Find free slot for emulated result
                var emulator_slot_idx: ?usize = null;
                for (&g_spawn_ops, 0..) |*slot, i| {
                    if (slot.* == null) {
                        emulator_slot_idx = i;
                        break;
                    }
                }
                if (emulator_slot_idx == null) return @intFromEnum(errors.ErrorCode.slot_exhausted);

                const emulator_request_id = g_next_spawn_id;
                g_next_spawn_id +%= 1;

                // Store emulated result (need to dupe strings since they're comptime)
                const stdout_copy = allocator.dupe(u8, result.stdout) catch null;
                const stderr_copy = if (result.stderr.len > 0) allocator.dupe(u8, result.stderr) catch null else null;

                g_spawn_ops[emulator_slot_idx.?] = AsyncSpawnRequest{
                    .id = emulator_request_id,
                    .status = .complete,
                    .exit_code = result.exit_code,
                    .stdout_data = stdout_copy,
                    .stderr_data = stderr_copy,
                };

                std.debug.print("[SPAWN EMULATED] {s} -> {s} (exit={d})\n", .{ binary_name, emulator_name, result.exit_code });
                return @intCast(emulator_request_id);
            }
            // Emulator returned null - fall through to real execution
        }
    }

    // Find free slot
    var slot_idx: ?usize = null;
    for (&g_spawn_ops, 0..) |*slot, i| {
        if (slot.* == null) {
            slot_idx = i;
            break;
        }
    }
    if (slot_idx == null) return @intFromEnum(errors.ErrorCode.slot_exhausted);

    const request_id = g_next_spawn_id;
    g_next_spawn_id +%= 1;

    // SECURITY: Block shell metacharacters to prevent command injection
    // Commands like "git; rm -rf /" or "echo $(cat /etc/passwd)" would bypass security
    const shell_chars = ";|&$`(){}[]<>\"'\\!#*?";
    for (cmd) |ch| {
        if (std.mem.indexOfScalar(u8, shell_chars, ch) != null) {
            std.debug.print("[SPAWN DENIED] Shell metacharacters not allowed: {s}\n", .{cmd});
            return @intFromEnum(errors.ErrorCode.shell_metachar_blocked);
        }
    }

    // Build argument list - use direct exec instead of shell for security
    var argv = std.ArrayListUnmanaged([]const u8){};
    defer argv.deinit(allocator);

    // Parse command by whitespace for direct exec (safe, no shell interpretation)
    var tokenizer = std.mem.tokenizeAny(u8, cmd, " \t");
    while (tokenizer.next()) |token| {
        argv.append(allocator, token) catch return -1;
    }

    if (argv.items.len == 0) return -1;

    // Note: args_json is ignored for now since we're using direct execution
    _ = args_json;

    // Execute the process
    var child = std.process.Child.init(argv.items, allocator);
    child.stdout_behavior = .Pipe;
    child.stderr_behavior = .Pipe;
    child.stdin_behavior = .Close; // Close stdin immediately - hooks use `cat > /dev/null` which blocks on stdin

    // Set up environment with sandbox config vars
    var env_map = std.process.getEnvMap(allocator) catch null;
    if (env_map) |*em| {
        // Wire config.dirs → __EDGEBOX_DIRS for OS-level sandbox
        if (cfg.dirs.items.len > 0) {
            var dirs_json = std.ArrayListUnmanaged(u8){};
            dirs_json.append(allocator, '[') catch {};
            for (cfg.dirs.items, 0..) |dir, i| {
                if (i > 0) dirs_json.append(allocator, ',') catch {};
                dirs_json.append(allocator, '"') catch {};
                dirs_json.appendSlice(allocator, dir.path) catch {};
                dirs_json.append(allocator, '"') catch {};
            }
            dirs_json.append(allocator, ']') catch {};
            em.put("__EDGEBOX_DIRS", dirs_json.items) catch {};
        }

        // Wire allow/deny commands
        if (cfg.allow_commands.items.len > 0) {
            var cmds_json = std.ArrayListUnmanaged(u8){};
            cmds_json.append(allocator, '[') catch {};
            for (cfg.allow_commands.items, 0..) |cmd_item, i| {
                if (i > 0) cmds_json.append(allocator, ',') catch {};
                cmds_json.append(allocator, '"') catch {};
                cmds_json.appendSlice(allocator, cmd_item) catch {};
                cmds_json.append(allocator, '"') catch {};
            }
            cmds_json.append(allocator, ']') catch {};
            em.put("__EDGEBOX_ALLOW_CMDS", cmds_json.items) catch {};
        }

        if (cfg.deny_commands.items.len > 0) {
            var deny_json = std.ArrayListUnmanaged(u8){};
            deny_json.append(allocator, '[') catch {};
            for (cfg.deny_commands.items, 0..) |cmd_item, i| {
                if (i > 0) deny_json.append(allocator, ',') catch {};
                deny_json.append(allocator, '"') catch {};
                deny_json.appendSlice(allocator, cmd_item) catch {};
                deny_json.append(allocator, '"') catch {};
            }
            deny_json.append(allocator, ']') catch {};
            em.put("__EDGEBOX_DENY_CMDS", deny_json.items) catch {};
        }

        // SECURITY LAYER 4: Credential Proxy - Inject credentials from config
        // (binary_name and cmd_perm already computed above for emulator check)
        if (cmd_perm) |perm| {
            if (perm.credentials) |creds| {
                var cred_iter = creds.iterator();
                while (cred_iter.next()) |entry| {
                    em.put(entry.key_ptr.*, entry.value_ptr.*) catch {};
                }
                std.debug.print("[SPAWN] Injecting {d} credentials for '{s}'\n", .{ creds.count(), binary_name });
            }
        }

        child.env_map = em;
    }

    child.spawn() catch {
        g_spawn_ops[slot_idx.?] = AsyncSpawnRequest{
            .id = request_id,
            .status = .error_state,
            .exit_code = -1,
            .stdout_data = null,
            .stderr_data = null,
        };
        return @intCast(request_id);
    };

    // SECURITY: Check global spawn memory limit before reading
    const reserved_memory = MAX_PER_SPAWN_MEMORY * 2; // Reserve for both stdout and stderr
    const current_usage = g_spawn_memory_used.load(.acquire);
    if (current_usage + reserved_memory > MAX_TOTAL_SPAWN_MEMORY) {
        std.debug.print("[SPAWN] Global memory limit reached ({d}MB used)\n", .{current_usage / (1024 * 1024)});
        _ = child.kill() catch {};
        _ = child.wait() catch {};
        g_spawn_ops[slot_idx.?] = AsyncSpawnRequest{
            .id = request_id,
            .status = .error_state,
            .exit_code = -1,
            .stdout_data = null,
            .stderr_data = null,
        };
        return @intCast(request_id);
    }
    _ = g_spawn_memory_used.fetchAdd(reserved_memory, .release);

    // Read stdout/stderr with reduced per-spawn limit
    const stdout = if (child.stdout) |stdout_file|
        stdout_file.readToEndAlloc(allocator, MAX_PER_SPAWN_MEMORY) catch null
    else
        null;
    const stderr = if (child.stderr) |stderr_file|
        stderr_file.readToEndAlloc(allocator, MAX_PER_SPAWN_MEMORY) catch null
    else
        null;

    // Calculate actual memory used and release excess reservation
    const actual_stdout_len: u64 = if (stdout) |s| s.len else 0;
    const actual_stderr_len: u64 = if (stderr) |s| s.len else 0;
    const actual_used = actual_stdout_len + actual_stderr_len;
    if (reserved_memory > actual_used) {
        _ = g_spawn_memory_used.fetchSub(reserved_memory - actual_used, .release);
    }

    const result = child.wait() catch {
        // Release memory on error
        _ = g_spawn_memory_used.fetchSub(actual_used, .release);
        if (stdout) |s| allocator.free(s);
        if (stderr) |s| allocator.free(s);
        g_spawn_ops[slot_idx.?] = AsyncSpawnRequest{
            .id = request_id,
            .status = .error_state,
            .exit_code = -1,
            .stdout_data = null,
            .stderr_data = null,
        };
        return @intCast(request_id);
    };

    // Note: Memory will be released when spawnGetStdout/spawnGetStderr copies data to WASM
    g_spawn_ops[slot_idx.?] = AsyncSpawnRequest{
        .id = request_id,
        .status = .complete,
        .exit_code = @intCast(result.Exited),
        .stdout_data = stdout,
        .stderr_data = stderr,
    };

    return @intCast(request_id);
}

fn spawnPoll(request_id: u32) i32 {
    for (&g_spawn_ops) |*slot| {
        if (slot.*) |*req| {
            if (req.id == request_id) {
                const result = switch (req.status) {
                    .pending => @as(i32, 0),
                    .complete => @as(i32, 1),
                    .error_state => @as(i32, -1),
                };

                return result;
            }
        }
    }

    return -2;
}

fn spawnOutputLen(request_id: u32) i32 {
    const allocator = std.heap.page_allocator;

    // If we already have JSON for this request, return its length
    if (g_spawn_json_buf) |buf| {
        if (g_spawn_json_id == request_id) {
            return @intCast(buf.len);
        }
        // Free previous buffer if different request
        allocator.free(buf);
        g_spawn_json_buf = null;
    }

    for (&g_spawn_ops) |*slot| {
        if (slot.*) |*req| {
            if (req.id == request_id) {
                // Build JSON response
                var json_buf = std.ArrayListUnmanaged(u8){};
                const writer = json_buf.writer(allocator);

                writer.print("{{\"exitCode\":{d},\"stdout\":\"", .{req.exit_code}) catch return -1;

                // Escape stdout
                if (req.stdout_data) |data| {
                    for (data) |ch| {
                        switch (ch) {
                            '"' => writer.writeAll("\\\"") catch {},
                            '\\' => writer.writeAll("\\\\") catch {},
                            '\n' => writer.writeAll("\\n") catch {},
                            '\r' => writer.writeAll("\\r") catch {},
                            '\t' => writer.writeAll("\\t") catch {},
                            else => {
                                if (ch < 0x20) {
                                    writer.print("\\u{x:0>4}", .{ch}) catch {};
                                } else {
                                    writer.writeByte(ch) catch {};
                                }
                            },
                        }
                    }
                }

                writer.writeAll("\",\"stderr\":\"") catch return -1;

                // Escape stderr
                if (req.stderr_data) |data| {
                    for (data) |ch| {
                        switch (ch) {
                            '"' => writer.writeAll("\\\"") catch {},
                            '\\' => writer.writeAll("\\\\") catch {},
                            '\n' => writer.writeAll("\\n") catch {},
                            '\r' => writer.writeAll("\\r") catch {},
                            '\t' => writer.writeAll("\\t") catch {},
                            else => {
                                if (ch < 0x20) {
                                    writer.print("\\u{x:0>4}", .{ch}) catch {};
                                } else {
                                    writer.writeByte(ch) catch {};
                                }
                            },
                        }
                    }
                }

                writer.writeAll("\"}") catch return -1;

                g_spawn_json_buf = json_buf.toOwnedSlice(allocator) catch return -1;
                g_spawn_json_id = request_id;

                return @intCast(g_spawn_json_buf.?.len);
            }
        }
    }
    return -1;
}

fn spawnOutput(exec_env: c.wasm_exec_env_t, request_id: u32, dest_ptr: u32) i32 {
    if (g_spawn_json_buf) |buf| {
        if (g_spawn_json_id == request_id) {
            if (!writeWasmMemory(exec_env, dest_ptr, buf)) {
                return -1;
            }
            return @intCast(buf.len);
        }
    }
    return -1;
}

fn spawnFree(request_id: u32) i32 {
    const allocator = std.heap.page_allocator;

    // Free JSON buffer if it was for this request
    if (g_spawn_json_buf) |buf| {
        if (g_spawn_json_id == request_id) {
            allocator.free(buf);
            g_spawn_json_buf = null;
        }
    }

    for (&g_spawn_ops) |*slot| {
        if (slot.*) |*req| {
            if (req.id == request_id) {
                // Release global spawn memory tracking
                var freed_bytes: u64 = 0;
                if (req.stdout_data) |data| {
                    freed_bytes += data.len;
                    allocator.free(data);
                }
                if (req.stderr_data) |data| {
                    freed_bytes += data.len;
                    allocator.free(data);
                }
                if (freed_bytes > 0) {
                    _ = g_spawn_memory_used.fetchSub(freed_bytes, .release);
                }
                slot.* = null;
                return 0;
            }
        }
    }
    return -1;
}

// =============================================================================
// Command Security Functions
// =============================================================================

/// Extract command name from command string (e.g., "git status" -> "git")
fn extractCmdName(cmd: []const u8) []const u8 {
    // Skip leading whitespace
    var start: usize = 0;
    while (start < cmd.len and (cmd[start] == ' ' or cmd[start] == '\t')) : (start += 1) {}

    // Find end of command (space or end of string)
    var end = start;
    while (end < cmd.len and cmd[end] != ' ' and cmd[end] != '\t') : (end += 1) {}

    const full_cmd = cmd[start..end];

    // Extract basename (handle paths like /usr/bin/git)
    if (std.mem.lastIndexOfScalar(u8, full_cmd, '/')) |slash| {
        return full_cmd[slash + 1 ..];
    }
    return full_cmd;
}

/// Check if command is in list
fn cmdInList(cmd_name: []const u8, list: []const []const u8) bool {
    for (list) |allowed| {
        if (std.mem.eql(u8, cmd_name, allowed)) return true;
    }
    return false;
}

/// Check if command matches destructive patterns using AST-based parsing
/// Returns true if command should be blocked
fn isCommandDestructive(allocator: std.mem.Allocator, cmd: []const u8, blocked_patterns: []const []const u8) bool {
    // LAYER 1: Simple pattern matching (fast path for obvious cases)
    for (blocked_patterns) |pattern| {
        if (std.mem.indexOf(u8, cmd, pattern)) |_| {
            return true;
        }
    }

    // LAYER 2: AST-based analysis for complex cases
    var analysis = shell_parser.analyzeForSecurity(allocator, cmd) catch {
        // If parsing fails, block for safety
        return true;
    };
    defer analysis.deinit();

    // Block subshells by default (security risk - arbitrary code execution)
    if (analysis.has_subshell) {
        std.debug.print("[SECURITY] Blocking subshell execution in: {s}\n", .{cmd});
        return true;
    }

    // Check for dangerous pipe targets
    if (shell_parser.hasDangerousPipeTarget(&analysis, &dangerous_pipe_targets)) {
        std.debug.print("[SECURITY] Blocking dangerous pipe target in: {s}\n", .{cmd});
        return true;
    }

    // Check for sensitive variable usage (potential credential leak)
    if (shell_parser.usesSensitiveVariable(&analysis, &sensitive_env_vars)) {
        std.debug.print("[SECURITY] Blocking sensitive variable usage in: {s}\n", .{cmd});
        return true;
    }

    return false;
}

/// Check if command tries to access/write sensitive files using AST-based parsing
/// Returns true if command should be blocked
fn accessesSensitiveFile(allocator: std.mem.Allocator, cmd: []const u8, sensitive_files: []const []const u8) bool {
    // LAYER 1: Simple pattern matching for read access
    for (sensitive_files) |pattern| {
        // Remove glob wildcards for simple matching
        var clean_pattern = pattern;
        if (std.mem.startsWith(u8, pattern, "**/")) {
            clean_pattern = pattern[3..];
        }
        if (std.mem.endsWith(u8, clean_pattern, "/*")) {
            clean_pattern = clean_pattern[0 .. clean_pattern.len - 2];
        }

        if (std.mem.indexOf(u8, cmd, clean_pattern)) |_| {
            return true;
        }
    }

    // LAYER 2: AST-based analysis for redirects (write access)
    var analysis = shell_parser.analyzeForSecurity(allocator, cmd) catch {
        // If parsing fails, allow (already checked patterns above)
        return false;
    };
    defer analysis.deinit();

    // Check if command redirects output to sensitive files
    if (shell_parser.hasRedirectToSensitiveFile(&analysis, sensitive_files)) {
        std.debug.print("[SECURITY] Blocking redirect to sensitive file in: {s}\n", .{cmd});
        return true;
    }

    return false;
}

/// Find command permission by binary name
/// Returns null if not found
fn findCommandPermission(commands: []const runtime.CommandPermission, binary: []const u8) ?*const runtime.CommandPermission {
    for (commands) |*cmd| {
        if (std.mem.eql(u8, cmd.binary, binary)) {
            return cmd;
        }
    }
    return null;
}

/// Extract binary name from command string
/// Example: "gh pr list" -> "gh"
fn extractBinaryName(cmd: []const u8) []const u8 {
    // Find first space or end of string
    if (std.mem.indexOf(u8, cmd, " ")) |space_idx| {
        return cmd[0..space_idx];
    }
    return cmd;
}

// =============================================================================
// Config Access (to be set by main module)
// =============================================================================

/// Global config pointer (must be set by main module before spawning)
var g_config: ?*const Config = null;

/// Set the global config (called by main module)
pub fn setConfig(config: *const Config) void {
    g_config = config;
}

/// Get the global config
fn getConfig() ?*const Config {
    return g_config;
}
