/// EdgeBox Daemon (edgeboxd) - HTTP server with Wizer snapshot restore
/// Pre-loads WASM and restores Wizer memory snapshot between requests
///
/// Usage:
///   edgeboxd <file.wasm|dylib> [--port=8080]
const std = @import("std");
const builtin = @import("builtin");
const edgebox_run = @import("edgebox_run");
const c = edgebox_run.c;

const VERSION = "0.1.0";
const DEFAULT_PORT: u16 = 8080;

// Global state
var g_allocator: std.mem.Allocator = undefined;
var g_module_path_z: [4096:0]u8 = undefined;

// Wizer snapshot - initial memory state
var g_initial_memory: []u8 = undefined;
var g_initial_memory_size: usize = 0;

// Pre-loaded AST module
var g_ast_module: ?*c.WasmEdge_ASTModuleContext = null;
var g_conf: ?*c.WasmEdge_ConfigureContext = null;

// Host modules (registered once)
var g_socket_bridge: ?*c.WasmEdge_ModuleInstanceContext = null;
var g_spawn_bridge: ?*c.WasmEdge_ModuleInstanceContext = null;
var g_file_bridge: ?*c.WasmEdge_ModuleInstanceContext = null;
var g_zlib_bridge: ?*c.WasmEdge_ModuleInstanceContext = null;
var g_crypto_bridge: ?*c.WasmEdge_ModuleInstanceContext = null;
var g_http_bridge: ?*c.WasmEdge_ModuleInstanceContext = null;
var g_process_stub: ?*c.WasmEdge_ModuleInstanceContext = null;

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    g_allocator = gpa.allocator();

    var args_iter = std.process.args();
    _ = args_iter.next();

    var wasm_path: ?[]const u8 = null;
    var port: u16 = DEFAULT_PORT;

    while (args_iter.next()) |arg| {
        if (std.mem.startsWith(u8, arg, "--port=")) {
            port = std.fmt.parseInt(u16, arg[7..], 10) catch DEFAULT_PORT;
        } else if (std.mem.startsWith(u8, arg, "-p=")) {
            port = std.fmt.parseInt(u16, arg[3..], 10) catch DEFAULT_PORT;
        } else if (std.mem.eql(u8, arg, "--help") or std.mem.eql(u8, arg, "-h")) {
            printUsage();
            return;
        } else if (std.mem.eql(u8, arg, "--version") or std.mem.eql(u8, arg, "-v")) {
            std.debug.print("edgeboxd {s}\n", .{VERSION});
            return;
        } else if (!std.mem.startsWith(u8, arg, "-")) {
            wasm_path = arg;
        }
    }

    if (wasm_path == null) {
        std.debug.print("Error: WASM file required\n\n", .{});
        printUsage();
        std.process.exit(1);
    }

    try startServer(wasm_path.?, port);
}

fn printUsage() void {
    std.debug.print(
        \\EdgeBox Daemon - HTTP server with Wizer snapshot restore
        \\
        \\Usage:
        \\  edgeboxd <file.wasm|dylib> [options]
        \\
        \\Options:
        \\  --port=PORT   HTTP port (default: 8080)
        \\  --help        Show this help
        \\
        \\The daemon loads Wizer-initialized WASM once, captures the initial
        \\memory state, and restores it between requests for fast execution.
        \\
    , .{});
}

fn startServer(wasm_path: []const u8, port: u16) !void {
    // Check for AOT version
    var module_path: []const u8 = wasm_path;
    var aot_path_buf: [4096]u8 = undefined;

    if (std.mem.endsWith(u8, wasm_path, ".wasm")) {
        const base = wasm_path[0 .. wasm_path.len - 5];
        const aot_ext = if (builtin.os.tag == .macos) ".dylib" else ".so";
        if (std.fmt.bufPrint(&aot_path_buf, "{s}{s}", .{ base, aot_ext })) |aot_path| {
            if (std.fs.cwd().access(aot_path, .{})) |_| {
                module_path = aot_path;
            } else |_| {}
        } else |_| {}
    }

    @memcpy(g_module_path_z[0..module_path.len], module_path);
    g_module_path_z[module_path.len] = 0;

    std.debug.print("[edgeboxd] Loading {s}...\n", .{module_path});

    // Initialize WasmEdge
    c.WasmEdge_LogSetErrorLevel();

    g_conf = c.WasmEdge_ConfigureCreate();
    c.WasmEdge_ConfigureAddHostRegistration(g_conf, c.WasmEdge_HostRegistration_Wasi);

    // Create host function bridges (once)
    edgebox_run.initTypes();
    g_socket_bridge = edgebox_run.createSocketBridge();
    g_spawn_bridge = edgebox_run.createSpawnBridge();
    g_file_bridge = edgebox_run.createFileBridge();
    g_zlib_bridge = edgebox_run.createZlibBridge();
    g_crypto_bridge = edgebox_run.createCryptoBridge();
    g_http_bridge = edgebox_run.createHttpBridge();
    g_process_stub = edgebox_run.createProcessStub();

    // Load AST module once
    const loader = c.WasmEdge_LoaderCreate(g_conf);
    defer c.WasmEdge_LoaderDelete(loader);

    var result = c.WasmEdge_LoaderParseFromFile(loader, &g_ast_module, &g_module_path_z);
    if (!c.WasmEdge_ResultOK(result)) {
        std.debug.print("[edgeboxd] Failed to load: {s}\n", .{c.WasmEdge_ResultGetMessage(result)});
        std.process.exit(1);
    }

    const validator = c.WasmEdge_ValidatorCreate(g_conf);
    defer c.WasmEdge_ValidatorDelete(validator);
    result = c.WasmEdge_ValidatorValidate(validator, g_ast_module);
    if (!c.WasmEdge_ResultOK(result)) {
        std.debug.print("[edgeboxd] Failed to validate: {s}\n", .{c.WasmEdge_ResultGetMessage(result)});
        std.process.exit(1);
    }

    // Initialize warm instance for fork-based execution
    try initWarmInstance();

    // Create server
    const server = try std.posix.socket(std.posix.AF.INET, std.posix.SOCK.STREAM, 0);
    defer std.posix.close(server);

    const optval: c_int = 1;
    try std.posix.setsockopt(server, std.posix.SOL.SOCKET, std.posix.SO.REUSEADDR, std.mem.asBytes(&optval));

    var addr: std.posix.sockaddr.in = .{
        .family = std.posix.AF.INET,
        .port = std.mem.nativeToBig(u16, port),
        .addr = 0,
    };

    try std.posix.bind(server, @ptrCast(&addr), @sizeOf(std.posix.sockaddr.in));
    try std.posix.listen(server, 128);

    std.debug.print("[edgeboxd] Listening on http://localhost:{}\n", .{port});
    std.debug.print("[edgeboxd] Ready - fork-based isolation (copy-on-write)\n", .{});

    while (true) {
        const client = std.posix.accept(server, null, null, 0) catch continue;
        handleRequest(client);
        std.posix.close(client);
    }
}

fn captureInitialMemory() !void {
    // Create temp executor/store to instantiate and capture memory
    const executor = c.WasmEdge_ExecutorCreate(g_conf, null);
    defer c.WasmEdge_ExecutorDelete(executor);

    const store = c.WasmEdge_StoreCreate();
    defer c.WasmEdge_StoreDelete(store);

    // Register WASI
    const wasi_module = c.WasmEdge_ModuleInstanceCreateWASI(null, 0, null, 0, null, 0);
    _ = c.WasmEdge_ExecutorRegisterImport(executor, store, wasi_module);

    // Register host bridges
    if (g_socket_bridge) |b| _ = c.WasmEdge_ExecutorRegisterImport(executor, store, b);
    if (g_spawn_bridge) |b| _ = c.WasmEdge_ExecutorRegisterImport(executor, store, b);
    if (g_file_bridge) |b| _ = c.WasmEdge_ExecutorRegisterImport(executor, store, b);
    if (g_zlib_bridge) |b| _ = c.WasmEdge_ExecutorRegisterImport(executor, store, b);
    if (g_crypto_bridge) |b| _ = c.WasmEdge_ExecutorRegisterImport(executor, store, b);
    if (g_http_bridge) |b| _ = c.WasmEdge_ExecutorRegisterImport(executor, store, b);
    if (g_process_stub) |b| _ = c.WasmEdge_ExecutorRegisterImport(executor, store, b);

    // Instantiate
    var module_inst: ?*c.WasmEdge_ModuleInstanceContext = null;
    const result = c.WasmEdge_ExecutorInstantiate(executor, &module_inst, store, g_ast_module);
    if (!c.WasmEdge_ResultOK(result)) {
        std.debug.print("[edgeboxd] Failed to instantiate: {s}\n", .{c.WasmEdge_ResultGetMessage(result)});
        std.process.exit(1);
    }
    defer c.WasmEdge_ModuleInstanceDelete(module_inst);

    // Capture memory
    const mem_name = c.WasmEdge_StringCreateByCString("memory");
    defer c.WasmEdge_StringDelete(mem_name);
    const memory = c.WasmEdge_ModuleInstanceFindMemory(module_inst, mem_name);

    if (memory != null) {
        const page_size = c.WasmEdge_MemoryInstanceGetPageSize(memory);
        g_initial_memory_size = page_size * 65536;

        g_initial_memory = try g_allocator.alloc(u8, g_initial_memory_size);
        const mem_ptr = c.WasmEdge_MemoryInstanceGetPointer(memory, 0, @intCast(g_initial_memory_size));
        if (mem_ptr != null) {
            @memcpy(g_initial_memory, mem_ptr[0..g_initial_memory_size]);
        }
    }
}

fn handleRequest(client: std.posix.fd_t) void {
    var buf: [4096]u8 = undefined;
    _ = std.posix.read(client, &buf) catch return;

    const start = std.time.nanoTimestamp();

    // Fork-based isolation: child handles request, parent waits
    // This gives us copy-on-write memory semantics - no memcpy needed!
    const stdout_pipe = std.posix.pipe() catch {
        sendError(client, "Pipe failed");
        return;
    };

    const fork_result = std.posix.fork() catch {
        std.posix.close(stdout_pipe[0]);
        std.posix.close(stdout_pipe[1]);
        sendError(client, "Fork failed");
        return;
    };

    if (fork_result == 0) {
        // Child process - handle the request
        std.posix.close(stdout_pipe[0]); // Close read end

        // Redirect stdout to pipe
        std.posix.dup2(stdout_pipe[1], std.posix.STDOUT_FILENO) catch std.process.exit(1);
        std.posix.close(stdout_pipe[1]);

        // Execute using pre-instantiated module (copy-on-write!)
        executeInChild();

        std.process.exit(0);
    }

    // Parent process - collect output
    std.posix.close(stdout_pipe[1]); // Close write end

    // Wait for child
    _ = std.posix.waitpid(@intCast(fork_result), 0);

    const elapsed_ns = std.time.nanoTimestamp() - start;
    const elapsed_ms = @as(f64, @floatFromInt(elapsed_ns)) / 1_000_000.0;

    // Read output
    var output: [65536]u8 = undefined;
    var output_len: usize = 0;
    while (output_len < output.len) {
        const n = std.posix.read(stdout_pipe[0], output[output_len..]) catch break;
        if (n == 0) break;
        output_len += n;
    }
    std.posix.close(stdout_pipe[0]);

    // Send response
    var response: [131072]u8 = undefined;
    const http = std.fmt.bufPrint(&response, "HTTP/1.1 200 OK\r\nContent-Type: text/plain\r\nContent-Length: {}\r\nX-Exec-Time: {d:.2}ms\r\nConnection: close\r\n\r\n{s}", .{ output_len, elapsed_ms, output[0..output_len] }) catch {
        sendError(client, "Response too large");
        return;
    };
    _ = std.posix.write(client, http) catch {};

    std.debug.print("[edgeboxd] {d:.2}ms, {} bytes\n", .{ elapsed_ms, output_len });
}

// Pre-instantiated module for fork-based execution
var g_warm_executor: ?*c.WasmEdge_ExecutorContext = null;
var g_warm_store: ?*c.WasmEdge_StoreContext = null;
var g_warm_module: ?*c.WasmEdge_ModuleInstanceContext = null;

fn initWarmInstance() !void {
    g_warm_executor = c.WasmEdge_ExecutorCreate(g_conf, null);
    g_warm_store = c.WasmEdge_StoreCreate();

    // Register WASI
    var wasi_args: [1][*c]const u8 = .{"edgeboxd"};
    var cwd_buf: [4096]u8 = undefined;
    const cwd = std.process.getCwd(&cwd_buf) catch ".";
    var preopen_buf: [8192]u8 = undefined;
    const preopen = std.fmt.bufPrintZ(&preopen_buf, "{s}:{s}", .{ cwd, cwd }) catch ".:.";
    var preopens: [1][*c]const u8 = .{preopen.ptr};

    const wasi_module = c.WasmEdge_ModuleInstanceCreateWASI(&wasi_args, 1, null, 0, &preopens, 1);
    _ = c.WasmEdge_ExecutorRegisterImport(g_warm_executor, g_warm_store, wasi_module);

    // Register host bridges
    if (g_socket_bridge) |b| _ = c.WasmEdge_ExecutorRegisterImport(g_warm_executor, g_warm_store, b);
    if (g_spawn_bridge) |b| _ = c.WasmEdge_ExecutorRegisterImport(g_warm_executor, g_warm_store, b);
    if (g_file_bridge) |b| _ = c.WasmEdge_ExecutorRegisterImport(g_warm_executor, g_warm_store, b);
    if (g_zlib_bridge) |b| _ = c.WasmEdge_ExecutorRegisterImport(g_warm_executor, g_warm_store, b);
    if (g_crypto_bridge) |b| _ = c.WasmEdge_ExecutorRegisterImport(g_warm_executor, g_warm_store, b);
    if (g_http_bridge) |b| _ = c.WasmEdge_ExecutorRegisterImport(g_warm_executor, g_warm_store, b);
    if (g_process_stub) |b| _ = c.WasmEdge_ExecutorRegisterImport(g_warm_executor, g_warm_store, b);

    // Instantiate once
    const result = c.WasmEdge_ExecutorInstantiate(g_warm_executor, &g_warm_module, g_warm_store, g_ast_module);
    if (!c.WasmEdge_ResultOK(result)) {
        std.debug.print("[edgeboxd] Failed to instantiate warm instance: {s}\n", .{c.WasmEdge_ResultGetMessage(result)});
        std.process.exit(1);
    }
}

fn executeInChild() void {
    // Execute _start on the pre-instantiated module
    // Memory is copy-on-write from parent, so this is fast!
    const func_name = c.WasmEdge_StringCreateByCString("_start");
    const func = c.WasmEdge_ModuleInstanceFindFunction(g_warm_module, func_name);
    if (func != null) {
        _ = c.WasmEdge_ExecutorInvoke(g_warm_executor, func, null, 0, null, 0);
    }
    c.WasmEdge_StringDelete(func_name);
}

fn sendError(client: std.posix.fd_t, msg: []const u8) void {
    var response: [256]u8 = undefined;
    const http = std.fmt.bufPrint(&response, "HTTP/1.1 500 Error\r\nContent-Type: text/plain\r\nContent-Length: {}\r\n\r\n{s}", .{ msg.len, msg }) catch return;
    _ = std.posix.write(client, http) catch {};
}
