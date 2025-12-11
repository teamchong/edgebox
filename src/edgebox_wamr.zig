/// EdgeBox WAMR Runtime - Fast cold start using WAMR (WebAssembly Micro Runtime)
///
/// WAMR is a lightweight WASM runtime from Bytecode Alliance.
/// - 4ms cold start vs 11ms with WasmEdge
/// - 400KB binary vs 2.6MB
/// - Pure C, compiled with Zig
///
/// Implements wasmedge_process.* compatible API for process spawning
/// with edgebox-sandbox integration for OS-level isolation.
const std = @import("std");

// Import WAMR C API
const c = @cImport({
    @cInclude("wasm_export.h");
    @cInclude("lib_export.h");
});

// Host function signatures for EdgeBox extensions
const NativeSymbol = c.NativeSymbol;

// =============================================================================
// Process State - wasmedge_process compatible implementation
// =============================================================================

/// Global process builder state (wasmedge_process uses global state)
var process_state: ProcessState = .{};
const allocator = std.heap.page_allocator;

const ProcessState = struct {
    program: ?[]const u8 = null,
    args: std.ArrayListUnmanaged([]const u8) = .{},
    env_vars: std.ArrayListUnmanaged(EnvVar) = .{},
    stdin_data: ?[]const u8 = null,
    timeout_ms: u32 = 30000,

    // Result state
    exit_code: i32 = 0,
    stdout_buf: ?[]u8 = null,
    stderr_buf: ?[]u8 = null,

    const EnvVar = struct { key: []const u8, value: []const u8 };

    fn reset(self: *ProcessState) void {
        self.program = null;
        self.args.clearRetainingCapacity(allocator);
        self.env_vars.clearRetainingCapacity(allocator);
        self.stdin_data = null;
        self.timeout_ms = 30000;
        self.exit_code = 0;
        if (self.stdout_buf) |buf| allocator.free(buf);
        if (self.stderr_buf) |buf| allocator.free(buf);
        self.stdout_buf = null;
        self.stderr_buf = null;
    }
};

// =============================================================================
// Configuration from .edgebox.json
// =============================================================================

const Config = struct {
    dirs: std.ArrayListUnmanaged([]const u8) = .{},
    env_vars: std.ArrayListUnmanaged([]const u8) = .{}, // Format: "KEY=value"
    stack_size: u32 = 8 * 1024 * 1024, // 8MB default (for large bytecode like Claude Code)
    heap_size: u32 = 64 * 1024 * 1024, // 64MB default

    fn deinit(self: *Config) void {
        for (self.dirs.items) |dir| {
            allocator.free(dir);
        }
        self.dirs.deinit(allocator);
        for (self.env_vars.items) |env| {
            allocator.free(env);
        }
        self.env_vars.deinit(allocator);
    }
};

fn expandPath(path: []const u8) ![]const u8 {
    if (path.len > 0 and path[0] == '~') {
        const home = std.process.getEnvVarOwned(allocator, "HOME") catch return try allocator.dupe(u8, path);
        defer allocator.free(home);
        const rest = if (path.len > 1) path[1..] else "";
        return try std.fmt.allocPrint(allocator, "{s}{s}", .{ home, rest });
    }
    return try allocator.dupe(u8, path);
}

fn loadConfig() Config {
    var config = Config{};

    // Always include current dir and /tmp
    config.dirs.append(allocator, allocator.dupe(u8, ".") catch ".") catch {};
    config.dirs.append(allocator, allocator.dupe(u8, "/tmp") catch "/tmp") catch {};

    // Try to load .edgebox.json
    const config_file = std.fs.cwd().openFile(".edgebox.json", .{}) catch return config;
    defer config_file.close();

    const config_size = config_file.getEndPos() catch return config;
    if (config_size > 1024 * 1024) return config; // Max 1MB config

    const config_buf = allocator.alloc(u8, config_size) catch return config;
    defer allocator.free(config_buf);

    _ = config_file.readAll(config_buf) catch return config;

    // Parse JSON
    const parsed = std.json.parseFromSlice(std.json.Value, allocator, config_buf, .{}) catch return config;
    defer parsed.deinit();

    const root = parsed.value;
    if (root != .object) return config;

    // Parse dirs array
    if (root.object.get("dirs")) |dirs_val| {
        if (dirs_val == .array) {
            for (dirs_val.array.items) |item| {
                if (item == .string) {
                    const expanded = expandPath(item.string) catch continue;
                    config.dirs.append(allocator, expanded) catch {
                        allocator.free(expanded);
                    };
                }
            }
        }
    }

    // Parse runtime config
    if (root.object.get("runtime")) |runtime_val| {
        if (runtime_val == .object) {
            if (runtime_val.object.get("stack_size")) |stack_val| {
                if (stack_val == .integer) {
                    config.stack_size = @intCast(@max(0, stack_val.integer));
                }
            }
            if (runtime_val.object.get("heap_size")) |heap_val| {
                if (heap_val == .integer) {
                    config.heap_size = @intCast(@max(0, heap_val.integer));
                }
            }
        }
    }

    // Parse env array - pass through specified env vars from host
    if (root.object.get("env")) |env_val| {
        if (env_val == .array) {
            for (env_val.array.items) |item| {
                if (item == .string) {
                    // Get env var value from host
                    const val = std.process.getEnvVarOwned(allocator, item.string) catch continue;
                    defer allocator.free(val);
                    // Format as KEY=value
                    const env_str = std.fmt.allocPrint(allocator, "{s}={s}", .{ item.string, val }) catch continue;
                    config.env_vars.append(allocator, env_str) catch {
                        allocator.free(env_str);
                    };
                }
            }
        }
    }

    return config;
}

// =============================================================================
// Main Entry Point
// =============================================================================

pub fn main() !void {
    // Load config first
    var config = loadConfig();
    defer config.deinit();

    // Collect all args
    var args_list = std.ArrayListUnmanaged([*:0]const u8){};
    defer args_list.deinit(allocator);

    var args_iter = std.process.args();
    _ = args_iter.next(); // skip program name

    const wasm_path = args_iter.next() orelse {
        std.debug.print("Usage: edgebox <file.wasm> [args...]\n", .{});
        return;
    };

    // Collect remaining args for WASI
    // First arg to WASM should be the wasm filename itself
    const wasm_basename = std.fs.path.basename(wasm_path);
    const basename_z = try allocator.dupeZ(u8, wasm_basename);
    try args_list.append(allocator, basename_z);

    while (args_iter.next()) |arg| {
        try args_list.append(allocator, arg);
    }

    const debug = std.process.getEnvVarOwned(allocator, "EDGEBOX_DEBUG") catch null;
    const show_debug = debug != null;
    if (debug) |d| allocator.free(d);

    const start = std.time.nanoTimestamp();

    // Initialize WAMR runtime - use system allocator for simplicity
    var init_args = std.mem.zeroes(c.RuntimeInitArgs);
    init_args.mem_alloc_type = c.Alloc_With_System_Allocator;

    if (!c.wasm_runtime_full_init(&init_args)) {
        std.debug.print("Failed to initialize WAMR runtime\n", .{});
        return;
    }
    defer c.wasm_runtime_destroy();

    // Register host functions (WASI extensions)
    registerWasmedgeProcess();
    registerHostFunctions();

    // Load WASM file
    var error_buf: [256]u8 = undefined;
    const wasm_file = std.fs.cwd().openFile(wasm_path, .{}) catch |err| {
        std.debug.print("Failed to open {s}: {}\n", .{ wasm_path, err });
        return;
    };
    defer wasm_file.close();

    const wasm_size = wasm_file.getEndPos() catch 0;
    const wasm_buf = std.heap.page_allocator.alloc(u8, wasm_size) catch {
        std.debug.print("Failed to allocate memory for WASM\n", .{});
        return;
    };
    defer std.heap.page_allocator.free(wasm_buf);

    _ = wasm_file.readAll(wasm_buf) catch {
        std.debug.print("Failed to read WASM file\n", .{});
        return;
    };

    const load_time = std.time.nanoTimestamp();

    // Load module
    const module = c.wasm_runtime_load(wasm_buf.ptr, @intCast(wasm_size), &error_buf, error_buf.len);
    if (module == null) {
        std.debug.print("Failed to load module: {s}\n", .{&error_buf});
        return;
    }
    defer c.wasm_runtime_unload(module);

    const parse_time = std.time.nanoTimestamp();

    // Set WASI args before instantiation
    // dir_list: preopened directories from .edgebox.json
    var dir_list_z = std.ArrayListUnmanaged([*:0]const u8){};
    defer dir_list_z.deinit(allocator);

    for (config.dirs.items) |dir| {
        const dir_z = allocator.dupeZ(u8, dir) catch continue;
        dir_list_z.append(allocator, dir_z) catch {
            allocator.free(dir_z);
        };
    }

    if (show_debug) {
        std.debug.print("[DEBUG] Preopened dirs ({}):\n", .{dir_list_z.items.len});
        for (dir_list_z.items) |dir| {
            std.debug.print("  - {s}\n", .{dir});
        }
    }

    // Build env var list for WASI
    var env_list_z = std.ArrayListUnmanaged([*:0]const u8){};
    defer env_list_z.deinit(allocator);

    for (config.env_vars.items) |env| {
        const env_z = allocator.dupeZ(u8, env) catch continue;
        env_list_z.append(allocator, env_z) catch {
            allocator.free(env_z);
        };
    }

    if (show_debug) {
        std.debug.print("[DEBUG] Env vars ({}):\n", .{env_list_z.items.len});
        for (env_list_z.items) |env| {
            // Only show key, not value for security
            const env_str = std.mem.span(env);
            if (std.mem.indexOf(u8, env_str, "=")) |eq| {
                std.debug.print("  - {s}=...\n", .{env_str[0..eq]});
            }
        }
    }

    c.wasm_runtime_set_wasi_args(
        module,
        @ptrCast(dir_list_z.items.ptr), // dir_list
        @intCast(dir_list_z.items.len), // dir_count
        null, // map_dir_list
        0, // map_dir_count
        @ptrCast(env_list_z.items.ptr), // env
        @intCast(env_list_z.items.len), // env_count
        @ptrCast(args_list.items.ptr), // argv
        @intCast(args_list.items.len), // argc
    );

    // Instantiate module with configurable stack/heap from .edgebox.json
    const stack_size = config.stack_size;
    const heap_size = config.heap_size;

    if (show_debug) std.debug.print("[DEBUG] Instantiating module (stack={d}MB, heap={d}MB)...\n", .{ stack_size / 1024 / 1024, heap_size / 1024 / 1024 });
    const module_inst = c.wasm_runtime_instantiate(module, stack_size, heap_size, &error_buf, error_buf.len);
    if (module_inst == null) {
        std.debug.print("Failed to instantiate: {s}\n", .{&error_buf});
        return;
    }
    defer c.wasm_runtime_deinstantiate(module_inst);
    if (show_debug) std.debug.print("[DEBUG] Module instantiated OK\n", .{});

    const inst_time = std.time.nanoTimestamp();

    // Create execution environment
    const exec_env = c.wasm_runtime_create_exec_env(module_inst, stack_size);
    if (exec_env == null) {
        std.debug.print("Failed to create exec env\n", .{});
        return;
    }
    defer c.wasm_runtime_destroy_exec_env(exec_env);

    // Find and call _start
    const start_func = c.wasm_runtime_lookup_function(module_inst, "_start");
    if (start_func == null) {
        std.debug.print("_start function not found\n", .{});
        return;
    }
    if (show_debug) std.debug.print("[DEBUG] Found _start, calling...\n", .{});

    const exec_start = std.time.nanoTimestamp();

    if (!c.wasm_runtime_call_wasm(exec_env, start_func, 0, null)) {
        const exception = c.wasm_runtime_get_exception(module_inst);
        if (exception != null) {
            std.debug.print("Exception: {s}\n", .{exception});
        }
        return;
    }

    const exec_time = std.time.nanoTimestamp();

    if (show_debug) {
        const load_ms = @as(f64, @floatFromInt(load_time - start)) / 1_000_000.0;
        const parse_ms = @as(f64, @floatFromInt(parse_time - load_time)) / 1_000_000.0;
        const inst_ms = @as(f64, @floatFromInt(inst_time - parse_time)) / 1_000_000.0;
        const exec_ms = @as(f64, @floatFromInt(exec_time - exec_start)) / 1_000_000.0;
        const total_ms = @as(f64, @floatFromInt(exec_time - start)) / 1_000_000.0;
        std.debug.print("\n[WAMR Debug] load: {d:.2}ms, parse: {d:.2}ms, instantiate: {d:.2}ms, exec: {d:.2}ms, total: {d:.2}ms\n", .{ load_ms, parse_ms, inst_ms, exec_ms, total_ms });
    }
}

// =============================================================================
// wasmedge_process.* Host Functions
// Compatible with WasmEdge process plugin API
// =============================================================================

fn getWasmMemory(exec_env: c.wasm_exec_env_t) ?[*]u8 {
    const module_inst = c.wasm_runtime_get_module_inst(exec_env);
    if (module_inst == null) return null;
    // Get linear memory base address
    const mem = c.wasm_runtime_addr_app_to_native(module_inst, 0);
    return @ptrCast(mem);
}

fn readWasmString(exec_env: c.wasm_exec_env_t, ptr: u32, len: u32) ?[]const u8 {
    const module_inst = c.wasm_runtime_get_module_inst(exec_env);
    if (module_inst == null) return null;
    const native_ptr = c.wasm_runtime_addr_app_to_native(module_inst, ptr);
    if (native_ptr == null) return null;
    const slice: [*]const u8 = @ptrCast(native_ptr);
    return slice[0..len];
}

fn writeWasmBuffer(exec_env: c.wasm_exec_env_t, ptr: u32, data: []const u8) void {
    const module_inst = c.wasm_runtime_get_module_inst(exec_env);
    if (module_inst == null) return;
    const native_ptr = c.wasm_runtime_addr_app_to_native(module_inst, ptr);
    if (native_ptr == null) return;
    const slice: [*]u8 = @ptrCast(native_ptr);
    @memcpy(slice[0..data.len], data);
}

/// wasmedge_process_set_prog_name(name_ptr, name_len)
fn processSetProgName(exec_env: c.wasm_exec_env_t, name_ptr: u32, name_len: u32) void {
    const name = readWasmString(exec_env, name_ptr, name_len) orelse return;
    // Duplicate to ensure we own the memory
    process_state.program = allocator.dupe(u8, name) catch return;
}

/// wasmedge_process_add_arg(arg_ptr, arg_len)
fn processAddArg(exec_env: c.wasm_exec_env_t, arg_ptr: u32, arg_len: u32) void {
    const arg = readWasmString(exec_env, arg_ptr, arg_len) orelse return;
    const duped = allocator.dupe(u8, arg) catch return;
    process_state.args.append(allocator, duped) catch return;
}

/// wasmedge_process_add_env(key_ptr, key_len, val_ptr, val_len)
fn processAddEnv(exec_env: c.wasm_exec_env_t, key_ptr: u32, key_len: u32, val_ptr: u32, val_len: u32) void {
    const key = readWasmString(exec_env, key_ptr, key_len) orelse return;
    const val = readWasmString(exec_env, val_ptr, val_len) orelse return;
    const key_duped = allocator.dupe(u8, key) catch return;
    const val_duped = allocator.dupe(u8, val) catch return;
    process_state.env_vars.append(allocator, .{ .key = key_duped, .value = val_duped }) catch return;
}

/// wasmedge_process_add_stdin(buf_ptr, buf_len)
fn processAddStdin(exec_env: c.wasm_exec_env_t, buf_ptr: u32, buf_len: u32) void {
    const data = readWasmString(exec_env, buf_ptr, buf_len) orelse return;
    process_state.stdin_data = allocator.dupe(u8, data) catch return;
}

/// wasmedge_process_set_timeout(time_ms)
fn processSetTimeout(_: c.wasm_exec_env_t, time_ms: u32) void {
    process_state.timeout_ms = time_ms;
}

/// wasmedge_process_run() -> i32 (0 = success, -1 = error)
fn processRun(_: c.wasm_exec_env_t) i32 {
    const program = process_state.program orelse return -1;

    // Check if OS-level sandbox is enabled
    const use_sandbox = std.posix.getenv("__EDGEBOX_DIRS") != null;

    // Build argv
    var argv = std.ArrayListUnmanaged([]const u8){};
    defer argv.deinit(allocator);

    if (use_sandbox) {
        // Wrap with edgebox-sandbox
        argv.append(allocator, "edgebox-sandbox") catch return -1;
    }

    // Add program
    argv.append(allocator, program) catch return -1;

    // Add args
    for (process_state.args.items) |arg| {
        argv.append(allocator, arg) catch return -1;
    }

    // Spawn child process using std.process.Child
    var child = std.process.Child.init(argv.items, allocator);
    child.stdout_behavior = .Pipe;
    child.stderr_behavior = .Pipe;

    // Note: Environment inheritance happens automatically
    // __EDGEBOX_DIRS and __EDGEBOX_COMMANDS are passed through

    child.spawn() catch |err| {
        std.debug.print("Process spawn failed: {}\n", .{err});
        return -1;
    };

    // Read stdout/stderr before wait (pipes must be drained first)
    var stdout_data: ?[]u8 = null;
    var stderr_data: ?[]u8 = null;
    var read_buf: [4096]u8 = undefined;

    if (child.stdout) |*stdout_file| {
        var stdout_list = std.ArrayListUnmanaged(u8){};
        while (true) {
            const n = stdout_file.read(&read_buf) catch break;
            if (n == 0) break;
            stdout_list.appendSlice(allocator, read_buf[0..n]) catch break;
        }
        if (stdout_list.items.len > 0) {
            stdout_data = stdout_list.toOwnedSlice(allocator) catch null;
        }
    }
    if (child.stderr) |*stderr_file| {
        var stderr_list = std.ArrayListUnmanaged(u8){};
        while (true) {
            const n = stderr_file.read(&read_buf) catch break;
            if (n == 0) break;
            stderr_list.appendSlice(allocator, read_buf[0..n]) catch break;
        }
        if (stderr_list.items.len > 0) {
            stderr_data = stderr_list.toOwnedSlice(allocator) catch null;
        }
    }

    // Wait for process to terminate
    const term = child.wait() catch |err| {
        std.debug.print("Process wait failed: {}\n", .{err});
        return -1;
    };

    // Store results
    process_state.exit_code = switch (term) {
        .Exited => |code| @intCast(code),
        .Signal => -1,
        .Stopped => -1,
        .Unknown => -1,
    };
    process_state.stdout_buf = stdout_data;
    process_state.stderr_buf = stderr_data;

    return 0;
}

/// wasmedge_process_get_exit_code() -> i32
fn processGetExitCode(_: c.wasm_exec_env_t) i32 {
    return process_state.exit_code;
}

/// wasmedge_process_get_stdout_len() -> u32
fn processGetStdoutLen(_: c.wasm_exec_env_t) u32 {
    return if (process_state.stdout_buf) |buf| @intCast(buf.len) else 0;
}

/// wasmedge_process_get_stdout(buf_ptr)
fn processGetStdout(exec_env: c.wasm_exec_env_t, buf_ptr: u32) void {
    if (process_state.stdout_buf) |buf| {
        writeWasmBuffer(exec_env, buf_ptr, buf);
    }
}

/// wasmedge_process_get_stderr_len() -> u32
fn processGetStderrLen(_: c.wasm_exec_env_t) u32 {
    return if (process_state.stderr_buf) |buf| @intCast(buf.len) else 0;
}

/// wasmedge_process_get_stderr(buf_ptr)
fn processGetStderr(exec_env: c.wasm_exec_env_t, buf_ptr: u32) void {
    if (process_state.stderr_buf) |buf| {
        writeWasmBuffer(exec_env, buf_ptr, buf);
    }
}

// IMPORTANT: These must be global/static because WAMR retains references to them
var g_wasmedge_process_symbols = [_]NativeSymbol{
    .{ .symbol = "wasmedge_process_set_prog_name", .func_ptr = @constCast(@ptrCast(&processSetProgName)), .signature = "(ii)", .attachment = null },
    .{ .symbol = "wasmedge_process_add_arg", .func_ptr = @constCast(@ptrCast(&processAddArg)), .signature = "(ii)", .attachment = null },
    .{ .symbol = "wasmedge_process_add_env", .func_ptr = @constCast(@ptrCast(&processAddEnv)), .signature = "(iiii)", .attachment = null },
    .{ .symbol = "wasmedge_process_add_stdin", .func_ptr = @constCast(@ptrCast(&processAddStdin)), .signature = "(ii)", .attachment = null },
    .{ .symbol = "wasmedge_process_set_timeout", .func_ptr = @constCast(@ptrCast(&processSetTimeout)), .signature = "(i)", .attachment = null },
    .{ .symbol = "wasmedge_process_run", .func_ptr = @constCast(@ptrCast(&processRun)), .signature = "()i", .attachment = null },
    .{ .symbol = "wasmedge_process_get_exit_code", .func_ptr = @constCast(@ptrCast(&processGetExitCode)), .signature = "()i", .attachment = null },
    .{ .symbol = "wasmedge_process_get_stdout_len", .func_ptr = @constCast(@ptrCast(&processGetStdoutLen)), .signature = "()i", .attachment = null },
    .{ .symbol = "wasmedge_process_get_stdout", .func_ptr = @constCast(@ptrCast(&processGetStdout)), .signature = "(i)", .attachment = null },
    .{ .symbol = "wasmedge_process_get_stderr_len", .func_ptr = @constCast(@ptrCast(&processGetStderrLen)), .signature = "()i", .attachment = null },
    .{ .symbol = "wasmedge_process_get_stderr", .func_ptr = @constCast(@ptrCast(&processGetStderr)), .signature = "(i)", .attachment = null },
};

fn registerWasmedgeProcess() void {
    _ = c.wasm_runtime_register_natives("wasmedge_process", &g_wasmedge_process_symbols, g_wasmedge_process_symbols.len);
}

// =============================================================================
// EdgeBox host functions - dispatch pattern
// =============================================================================

// Opcodes for dispatch functions
const FILE_OP_READ_START: i32 = 0;
const FILE_OP_WRITE_START: i32 = 1;
const FILE_OP_POLL: i32 = 2;
const FILE_OP_RESULT_LEN: i32 = 3;
const FILE_OP_RESULT: i32 = 4;
const FILE_OP_FREE: i32 = 5;

const SPAWN_OP_START: i32 = 0;
const SPAWN_OP_POLL: i32 = 1;
const SPAWN_OP_OUTPUT_LEN: i32 = 2;
const SPAWN_OP_OUTPUT: i32 = 3;
const SPAWN_OP_FREE: i32 = 4;

const HTTP_OP_REQUEST: i32 = 0;
const HTTP_OP_GET_RESPONSE_LEN: i32 = 1;
const HTTP_OP_GET_RESPONSE: i32 = 2;
const HTTP_OP_START_ASYNC: i32 = 3;
const HTTP_OP_POLL: i32 = 4;
const HTTP_OP_RESPONSE_LEN: i32 = 5;
const HTTP_OP_RESPONSE: i32 = 6;
const HTTP_OP_FREE: i32 = 7;

// Max concurrent async operations
const MAX_ASYNC_OPS: usize = 64;

// Async file operation state
const FileOpStatus = enum { pending, complete, error_state };
const FileOp = enum { read, write };

const AsyncFileRequest = struct {
    id: u32,
    op: FileOp,
    status: FileOpStatus,
    data: ?[]u8,
    error_msg: ?[]const u8,
    bytes_written: usize,
};

var g_file_ops: [MAX_ASYNC_OPS]?AsyncFileRequest = [_]?AsyncFileRequest{null} ** MAX_ASYNC_OPS;
var g_next_file_id: u32 = 1;

// Async spawn operation state
const SpawnOpStatus = enum { pending, complete, error_state };

const AsyncSpawnRequest = struct {
    id: u32,
    status: SpawnOpStatus,
    exit_code: i32,
    stdout_data: ?[]u8,
    stderr_data: ?[]u8,
};

var g_spawn_ops: [MAX_ASYNC_OPS]?AsyncSpawnRequest = [_]?AsyncSpawnRequest{null} ** MAX_ASYNC_OPS;
var g_next_spawn_id: u32 = 1;

// HTTP response state
var g_http_response: ?[]u8 = null;
var g_http_status: i32 = 0;

// Helper to read string from WASM memory
fn readWasmMemory(exec_env: c.wasm_exec_env_t, ptr: u32, len: u32) ?[]const u8 {
    const module_inst = c.wasm_runtime_get_module_inst(exec_env);
    if (module_inst == null) return null;

    if (!c.wasm_runtime_validate_app_addr(module_inst, ptr, len)) return null;

    const native_ptr = c.wasm_runtime_addr_app_to_native(module_inst, ptr);
    if (native_ptr == null) return null;

    const bytes: [*]const u8 = @ptrCast(native_ptr);
    return bytes[0..len];
}

// Helper to write to WASM memory
fn writeWasmMemory(exec_env: c.wasm_exec_env_t, ptr: u32, data: []const u8) bool {
    const module_inst = c.wasm_runtime_get_module_inst(exec_env);
    if (module_inst == null) return false;

    if (!c.wasm_runtime_validate_app_addr(module_inst, ptr, @intCast(data.len))) return false;

    const native_ptr = c.wasm_runtime_addr_app_to_native(module_inst, ptr);
    if (native_ptr == null) return false;

    const dest: [*]u8 = @ptrCast(native_ptr);
    @memcpy(dest[0..data.len], data);
    return true;
}

// =============================================================================
// File Dispatch Implementation
// =============================================================================

fn fileDispatch(exec_env: c.wasm_exec_env_t, opcode: i32, a1: i32, a2: i32, a3: i32, a4: i32) i32 {
    return switch (opcode) {
        FILE_OP_READ_START => fileReadStart(exec_env, @bitCast(a1), @bitCast(a2)),
        FILE_OP_WRITE_START => fileWriteStart(exec_env, @bitCast(a1), @bitCast(a2), @bitCast(a3), @bitCast(a4)),
        FILE_OP_POLL => filePoll(@bitCast(a1)),
        FILE_OP_RESULT_LEN => fileResultLen(@bitCast(a1)),
        FILE_OP_RESULT => fileResult(exec_env, @bitCast(a1), @bitCast(a2)),
        FILE_OP_FREE => fileFree(@bitCast(a1)),
        else => -1,
    };
}

fn fileReadStart(exec_env: c.wasm_exec_env_t, path_ptr: u32, path_len: u32) i32 {
    const path = readWasmMemory(exec_env, path_ptr, path_len) orelse return -1;

    // Find free slot
    var slot_idx: ?usize = null;
    for (&g_file_ops, 0..) |*slot, i| {
        if (slot.* == null) {
            slot_idx = i;
            break;
        }
    }
    if (slot_idx == null) return -4; // Too many pending operations

    const request_id = g_next_file_id;
    g_next_file_id +%= 1;

    // Read file synchronously (async can be added later with threads)
    const file = std.fs.cwd().openFile(path, .{}) catch |err| {
        g_file_ops[slot_idx.?] = AsyncFileRequest{
            .id = request_id,
            .op = .read,
            .status = .error_state,
            .data = null,
            .error_msg = switch (err) {
                error.FileNotFound => "ENOENT",
                error.AccessDenied => "EACCES",
                else => "EIO",
            },
            .bytes_written = 0,
        };
        return @intCast(request_id);
    };
    defer file.close();

    const content = file.readToEndAlloc(allocator, 100 * 1024 * 1024) catch {
        g_file_ops[slot_idx.?] = AsyncFileRequest{
            .id = request_id,
            .op = .read,
            .status = .error_state,
            .data = null,
            .error_msg = "EIO",
            .bytes_written = 0,
        };
        return @intCast(request_id);
    };

    g_file_ops[slot_idx.?] = AsyncFileRequest{
        .id = request_id,
        .op = .read,
        .status = .complete,
        .data = content,
        .error_msg = null,
        .bytes_written = content.len,
    };

    return @intCast(request_id);
}

fn fileWriteStart(exec_env: c.wasm_exec_env_t, path_ptr: u32, path_len: u32, data_ptr: u32, data_len: u32) i32 {
    const path = readWasmMemory(exec_env, path_ptr, path_len) orelse return -1;
    const data = readWasmMemory(exec_env, data_ptr, data_len) orelse return -2;

    // Find free slot
    var slot_idx: ?usize = null;
    for (&g_file_ops, 0..) |*slot, i| {
        if (slot.* == null) {
            slot_idx = i;
            break;
        }
    }
    if (slot_idx == null) return -4;

    const request_id = g_next_file_id;
    g_next_file_id +%= 1;

    // Create parent directories if needed
    if (std.mem.lastIndexOf(u8, path, "/")) |last_slash| {
        if (last_slash > 0) {
            const parent = path[0..last_slash];
            if (path[0] == '/') {
                std.fs.makeDirAbsolute(parent) catch {};
            } else {
                std.fs.cwd().makePath(parent) catch {};
            }
        }
    }

    // Write file
    const abs_file = if (path[0] == '/')
        std.fs.openFileAbsolute(path, .{ .mode = .write_only })
    else
        std.fs.cwd().openFile(path, .{ .mode = .write_only });

    const file = abs_file catch |err| {
        // Try to create the file
        const create_result = if (path[0] == '/')
            std.fs.createFileAbsolute(path, .{})
        else
            std.fs.cwd().createFile(path, .{});

        const created_file = create_result catch {
            g_file_ops[slot_idx.?] = AsyncFileRequest{
                .id = request_id,
                .op = .write,
                .status = .error_state,
                .data = null,
                .error_msg = switch (err) {
                    error.FileNotFound => "ENOENT",
                    error.AccessDenied => "EACCES",
                    else => "EIO",
                },
                .bytes_written = 0,
            };
            return @intCast(request_id);
        };

        created_file.writeAll(data) catch {
            created_file.close();
            g_file_ops[slot_idx.?] = AsyncFileRequest{
                .id = request_id,
                .op = .write,
                .status = .error_state,
                .data = null,
                .error_msg = "EIO",
                .bytes_written = 0,
            };
            return @intCast(request_id);
        };
        created_file.close();

        g_file_ops[slot_idx.?] = AsyncFileRequest{
            .id = request_id,
            .op = .write,
            .status = .complete,
            .data = null,
            .error_msg = null,
            .bytes_written = data.len,
        };
        return @intCast(request_id);
    };

    file.writeAll(data) catch {
        file.close();
        g_file_ops[slot_idx.?] = AsyncFileRequest{
            .id = request_id,
            .op = .write,
            .status = .error_state,
            .data = null,
            .error_msg = "EIO",
            .bytes_written = 0,
        };
        return @intCast(request_id);
    };
    file.close();

    g_file_ops[slot_idx.?] = AsyncFileRequest{
        .id = request_id,
        .op = .write,
        .status = .complete,
        .data = null,
        .error_msg = null,
        .bytes_written = data.len,
    };

    return @intCast(request_id);
}

fn filePoll(request_id: u32) i32 {
    for (&g_file_ops) |*slot| {
        if (slot.*) |*req| {
            if (req.id == request_id) {
                return switch (req.status) {
                    .pending => 0,
                    .complete => 1,
                    .error_state => -1,
                };
            }
        }
    }
    return -2; // Not found
}

fn fileResultLen(request_id: u32) i32 {
    for (&g_file_ops) |*slot| {
        if (slot.*) |*req| {
            if (req.id == request_id) {
                if (req.data) |data| {
                    return @intCast(data.len);
                }
                return 0;
            }
        }
    }
    return -1;
}

fn fileResult(exec_env: c.wasm_exec_env_t, request_id: u32, dest_ptr: u32) i32 {
    for (&g_file_ops) |*slot| {
        if (slot.*) |*req| {
            if (req.id == request_id) {
                if (req.data) |data| {
                    if (!writeWasmMemory(exec_env, dest_ptr, data)) {
                        return -1;
                    }
                    return @intCast(data.len);
                }
                return 0;
            }
        }
    }
    return -1;
}

fn fileFree(request_id: u32) i32 {
    for (&g_file_ops) |*slot| {
        if (slot.*) |*req| {
            if (req.id == request_id) {
                if (req.data) |data| {
                    allocator.free(data);
                }
                slot.* = null;
                return 0;
            }
        }
    }
    return -1;
}

// =============================================================================
// Spawn Dispatch Implementation
// =============================================================================

fn spawnDispatch(exec_env: c.wasm_exec_env_t, opcode: i32, a1: i32, a2: i32, a3: i32, a4: i32) i32 {
    return switch (opcode) {
        SPAWN_OP_START => spawnStart(exec_env, @bitCast(a1), @bitCast(a2), @bitCast(a3), @bitCast(a4)),
        SPAWN_OP_POLL => spawnPoll(@bitCast(a1)),
        SPAWN_OP_OUTPUT_LEN => spawnOutputLen(@bitCast(a1)),
        SPAWN_OP_OUTPUT => spawnOutput(exec_env, @bitCast(a1), @bitCast(a2)),
        SPAWN_OP_FREE => spawnFree(@bitCast(a1)),
        else => -1,
    };
}

fn spawnStart(exec_env: c.wasm_exec_env_t, cmd_ptr: u32, cmd_len: u32, args_ptr: u32, args_len: u32) i32 {
    const cmd = readWasmMemory(exec_env, cmd_ptr, cmd_len) orelse return -1;
    const args_json = if (args_len > 0) readWasmMemory(exec_env, args_ptr, args_len) else null;

    // Find free slot
    var slot_idx: ?usize = null;
    for (&g_spawn_ops, 0..) |*slot, i| {
        if (slot.* == null) {
            slot_idx = i;
            break;
        }
    }
    if (slot_idx == null) return -4;

    const request_id = g_next_spawn_id;
    g_next_spawn_id +%= 1;

    // Build argument list
    var argv = std.ArrayListUnmanaged([]const u8){};
    defer argv.deinit(allocator);

    argv.append(allocator, cmd) catch return -1;

    // Parse JSON args array if provided
    if (args_json) |json| {
        // Simple JSON array parsing: ["arg1", "arg2", ...]
        var i: usize = 0;
        while (i < json.len) {
            // Skip to next string
            while (i < json.len and json[i] != '"') : (i += 1) {}
            if (i >= json.len) break;
            i += 1; // Skip opening quote

            const start = i;
            while (i < json.len and json[i] != '"') : (i += 1) {}
            if (i > start) {
                const arg = allocator.dupe(u8, json[start..i]) catch return -1;
                argv.append(allocator, arg) catch {
                    allocator.free(arg);
                    return -1;
                };
            }
            i += 1; // Skip closing quote
        }
    }

    // Execute the process
    var child = std.process.Child.init(argv.items, allocator);
    child.stdout_behavior = .Pipe;
    child.stderr_behavior = .Pipe;

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

    // Read stdout/stderr and wait for completion
    const stdout = child.stdout.?.readToEndAlloc(allocator, 10 * 1024 * 1024) catch null;
    const stderr = child.stderr.?.readToEndAlloc(allocator, 10 * 1024 * 1024) catch null;

    const result = child.wait() catch {
        g_spawn_ops[slot_idx.?] = AsyncSpawnRequest{
            .id = request_id,
            .status = .error_state,
            .exit_code = -1,
            .stdout_data = stdout,
            .stderr_data = stderr,
        };
        return @intCast(request_id);
    };

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
                return switch (req.status) {
                    .pending => 0,
                    .complete => 1,
                    .error_state => -1,
                };
            }
        }
    }
    return -2;
}

fn spawnOutputLen(request_id: u32) i32 {
    for (&g_spawn_ops) |*slot| {
        if (slot.*) |*req| {
            if (req.id == request_id) {
                var total: usize = 0;
                if (req.stdout_data) |data| total += data.len;
                if (req.stderr_data) |data| total += data.len;
                return @intCast(total);
            }
        }
    }
    return -1;
}

fn spawnOutput(exec_env: c.wasm_exec_env_t, request_id: u32, dest_ptr: u32) i32 {
    for (&g_spawn_ops) |*slot| {
        if (slot.*) |*req| {
            if (req.id == request_id) {
                // Return stdout (primary output)
                if (req.stdout_data) |data| {
                    if (!writeWasmMemory(exec_env, dest_ptr, data)) {
                        return -1;
                    }
                    return @intCast(data.len);
                }
                return 0;
            }
        }
    }
    return -1;
}

fn spawnFree(request_id: u32) i32 {
    for (&g_spawn_ops) |*slot| {
        if (slot.*) |*req| {
            if (req.id == request_id) {
                if (req.stdout_data) |data| allocator.free(data);
                if (req.stderr_data) |data| allocator.free(data);
                slot.* = null;
                return 0;
            }
        }
    }
    return -1;
}

// =============================================================================
// HTTP Dispatch Implementation
// =============================================================================

fn httpDispatch(exec_env: c.wasm_exec_env_t, opcode: i32, a1: i32, a2: i32, a3: i32, a4: i32, a5: i32, a6: i32, a7: i32, a8: i32) i32 {
    return switch (opcode) {
        HTTP_OP_REQUEST => httpRequest(exec_env, @bitCast(a1), @bitCast(a2), @bitCast(a3), @bitCast(a4), @bitCast(a5), @bitCast(a6), @bitCast(a7), @bitCast(a8)),
        HTTP_OP_GET_RESPONSE_LEN => httpGetResponseLen(),
        HTTP_OP_GET_RESPONSE => httpGetResponse(exec_env, @bitCast(a1)),
        else => -1,
    };
}

fn httpRequest(exec_env: c.wasm_exec_env_t, url_ptr: u32, url_len: u32, method_ptr: u32, method_len: u32, headers_ptr: u32, headers_len: u32, body_ptr: u32, body_len: u32) i32 {
    const url = readWasmMemory(exec_env, url_ptr, url_len) orelse return -1;
    const method = if (method_len > 0) readWasmMemory(exec_env, method_ptr, method_len) else null;
    const headers_str = if (headers_len > 0) readWasmMemory(exec_env, headers_ptr, headers_len) else null;
    const body = if (body_len > 0) readWasmMemory(exec_env, body_ptr, body_len) else null;

    const debug = std.process.getEnvVarOwned(allocator, "EDGEBOX_DEBUG") catch null;
    const show_debug = debug != null;
    if (debug) |d| allocator.free(d);

    if (show_debug) std.debug.print("[HTTP] Request to: {s}\n", .{url});

    // Free previous response
    if (g_http_response) |resp| {
        allocator.free(resp);
        g_http_response = null;
    }

    // Parse URL
    const uri = std.Uri.parse(url) catch |err| {
        if (show_debug) std.debug.print("[HTTP] URL parse error: {}\n", .{err});
        return -1;
    };

    if (show_debug) {
        const host_str = if (uri.host) |h| h.percent_encoded else "(none)";
        std.debug.print("[HTTP] Parsed URI, host={s}\n", .{host_str});
    }

    // Determine HTTP method
    const http_method: std.http.Method = if (std.mem.eql(u8, method orelse "GET", "POST"))
        .POST
    else if (std.mem.eql(u8, method orelse "GET", "PUT"))
        .PUT
    else if (std.mem.eql(u8, method orelse "GET", "DELETE"))
        .DELETE
    else if (std.mem.eql(u8, method orelse "GET", "PATCH"))
        .PATCH
    else
        .GET;

    // Build extra headers if provided
    var extra_headers = std.ArrayListUnmanaged(std.http.Header){};
    defer extra_headers.deinit(allocator);

    if (headers_str) |h| {
        // Parse headers from "Key: Value\r\nKey2: Value2" format
        var lines = std.mem.splitSequence(u8, h, "\r\n");
        while (lines.next()) |line| {
            if (std.mem.indexOf(u8, line, ": ")) |colon_pos| {
                const key = line[0..colon_pos];
                const value = line[colon_pos + 2 ..];
                extra_headers.append(allocator, .{ .name = key, .value = value }) catch {};
            }
        }
    }

    // Create HTTP client (Zig 0.15 API - lower level request API)
    var client = std.http.Client{ .allocator = allocator };
    defer client.deinit();

    if (show_debug) std.debug.print("[HTTP] Creating request...\n", .{});

    // Create and send request
    var req = client.request(http_method, uri, .{
        .extra_headers = extra_headers.items,
    }) catch |err| {
        if (show_debug) std.debug.print("[HTTP] Request creation error: {}\n", .{err});
        return -1;
    };
    defer req.deinit();

    if (show_debug) std.debug.print("[HTTP] Sending request...\n", .{});

    // Send body if present
    if (body) |b| {
        req.transfer_encoding = .{ .content_length = b.len };
        var body_writer = req.sendBodyUnflushed(&.{}) catch |err| {
            if (show_debug) std.debug.print("[HTTP] Body send error: {}\n", .{err});
            return -1;
        };
        body_writer.writer.writeAll(b) catch |err| {
            if (show_debug) std.debug.print("[HTTP] Body write error: {}\n", .{err});
            return -1;
        };
        body_writer.end() catch |err| {
            if (show_debug) std.debug.print("[HTTP] Body end error: {}\n", .{err});
            return -1;
        };
        if (req.connection) |conn| conn.flush() catch |err| {
            if (show_debug) std.debug.print("[HTTP] Flush error: {}\n", .{err});
            return -1;
        };
    } else {
        req.sendBodiless() catch |err| {
            if (show_debug) std.debug.print("[HTTP] Send bodiless error: {}\n", .{err});
            return -1;
        };
    }

    if (show_debug) std.debug.print("[HTTP] Receiving response head...\n", .{});

    // Receive response head
    var head_buf: [16 * 1024]u8 = undefined;
    var response = req.receiveHead(&head_buf) catch |err| {
        if (show_debug) std.debug.print("[HTTP] Receive head error: {}\n", .{err});
        return -1;
    };

    // Store status
    g_http_status = @intFromEnum(response.head.status);

    if (show_debug) std.debug.print("[HTTP] Status: {d}\n", .{g_http_status});

    // Read response body using allocRemaining (Zig 0.15 API)
    var reader = response.reader(&.{});
    const body_data = reader.allocRemaining(allocator, std.Io.Limit.limited(100 * 1024 * 1024)) catch |err| {
        if (show_debug) std.debug.print("[HTTP] Body read error: {}\n", .{err});
        return -1;
    };
    g_http_response = body_data;

    if (show_debug) std.debug.print("[HTTP] Response body length: {d}\n", .{body_data.len});

    return g_http_status;
}

fn httpGetResponseLen() i32 {
    if (g_http_response) |resp| {
        return @intCast(resp.len);
    }
    return 0;
}

fn httpGetResponse(exec_env: c.wasm_exec_env_t, dest_ptr: u32) i32 {
    if (g_http_response) |resp| {
        if (!writeWasmMemory(exec_env, dest_ptr, resp)) {
            return -1;
        }
        return @intCast(resp.len);
    }
    return 0;
}

// =============================================================================
// Zlib Dispatch - Basic implementation
// =============================================================================

fn zlibDispatch(exec_env: c.wasm_exec_env_t, opcode: i32, a1: i32, a2: i32) i32 {
    _ = exec_env;
    _ = opcode;
    _ = a1;
    _ = a2;
    // Zlib compression not critical for Claude CLI - return error to use JS fallback
    return -1;
}

// =============================================================================
// Crypto Dispatch - Basic implementation
// =============================================================================

fn cryptoDispatch(exec_env: c.wasm_exec_env_t, opcode: i32, a1: i32, a2: i32, a3: i32, a4: i32, a5: i32, a6: i32) i32 {
    _ = exec_env;
    _ = opcode;
    _ = a1;
    _ = a2;
    _ = a3;
    _ = a4;
    _ = a5;
    _ = a6;
    // Crypto not critical for Claude CLI - return error to use JS fallback
    return -1;
}

// =============================================================================
// Socket Dispatch - Not needed for Claude CLI
// =============================================================================

fn socketDispatch(exec_env: c.wasm_exec_env_t, opcode: i32, a1: i32, a2: i32, a3: i32) i32 {
    _ = exec_env;
    _ = opcode;
    _ = a1;
    _ = a2;
    _ = a3;
    // Socket not needed for Claude CLI
    return -1;
}

// Global symbol arrays for EdgeBox host functions (WAMR retains references)
var g_http_symbols = [_]NativeSymbol{
    .{ .symbol = "http_dispatch", .func_ptr = @constCast(@ptrCast(&httpDispatch)), .signature = "(iiiiiiiii)i", .attachment = null },
};
var g_spawn_symbols = [_]NativeSymbol{
    .{ .symbol = "spawn_dispatch", .func_ptr = @constCast(@ptrCast(&spawnDispatch)), .signature = "(iiiii)i", .attachment = null },
};
var g_file_symbols = [_]NativeSymbol{
    .{ .symbol = "file_dispatch", .func_ptr = @constCast(@ptrCast(&fileDispatch)), .signature = "(iiiii)i", .attachment = null },
};
var g_zlib_symbols = [_]NativeSymbol{
    .{ .symbol = "zlib_dispatch", .func_ptr = @constCast(@ptrCast(&zlibDispatch)), .signature = "(iii)i", .attachment = null },
};
var g_crypto_symbols = [_]NativeSymbol{
    .{ .symbol = "crypto_dispatch", .func_ptr = @constCast(@ptrCast(&cryptoDispatch)), .signature = "(iiiiiii)i", .attachment = null },
};
var g_socket_symbols = [_]NativeSymbol{
    .{ .symbol = "socket_dispatch", .func_ptr = @constCast(@ptrCast(&socketDispatch)), .signature = "(iiii)i", .attachment = null },
};

fn registerHostFunctions() void {
    _ = c.wasm_runtime_register_natives("edgebox_http", &g_http_symbols, g_http_symbols.len);
    _ = c.wasm_runtime_register_natives("edgebox_spawn", &g_spawn_symbols, g_spawn_symbols.len);
    _ = c.wasm_runtime_register_natives("edgebox_file", &g_file_symbols, g_file_symbols.len);
    _ = c.wasm_runtime_register_natives("edgebox_zlib", &g_zlib_symbols, g_zlib_symbols.len);
    _ = c.wasm_runtime_register_natives("edgebox_crypto", &g_crypto_symbols, g_crypto_symbols.len);
    _ = c.wasm_runtime_register_natives("edgebox_socket", &g_socket_symbols, g_socket_symbols.len);
    // Note: WASI socket imports (sock_open, sock_connect, sock_getaddrinfo) will show
    // warnings because WAMR's WASI doesn't implement them. These are benign for apps
    // that don't use sockets.
}
