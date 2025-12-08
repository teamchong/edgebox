/// EdgeBox Daemon (edgeboxd) - HTTP server with pre-loaded WASM runtime
/// Provides ~5ms response times by keeping WasmEdge VM in memory
///
/// Usage:
///   edgeboxd <file.wasm|dylib> [--port=8080]
///   edgeboxd bench/hello.dylib --port=8080
const std = @import("std");
const builtin = @import("builtin");
const c = @cImport({
    @cInclude("wasmedge/wasmedge.h");
});

const VERSION = "0.1.0";
const DEFAULT_PORT: u16 = 8080;

pub fn main() !void {
    var args_iter = std.process.args();
    _ = args_iter.next(); // Skip program name

    var wasm_path: ?[]const u8 = null;
    var port: u16 = DEFAULT_PORT;

    // Parse args
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

    try startHttpServer(wasm_path.?, port);
}

fn printUsage() void {
    std.debug.print(
        \\EdgeBox Daemon - HTTP server with pre-loaded WASM runtime
        \\
        \\Usage:
        \\  edgeboxd <file.wasm|dylib> [options]
        \\
        \\Options:
        \\  --port=PORT, -p=PORT   HTTP port (default: 8080)
        \\  --help, -h             Show this help
        \\  --version, -v          Show version
        \\
        \\Examples:
        \\  edgeboxd bench/hello.dylib              Start on port 8080
        \\  edgeboxd bench/hello.dylib --port=3000  Start on port 3000
        \\
        \\Test:
        \\  curl http://localhost:8080/
        \\
    , .{});
}

fn startHttpServer(wasm_path: []const u8, port: u16) !void {
    // Check for AOT version
    var aot_path_buf: [4096]u8 = undefined;
    var module_path: []const u8 = wasm_path;

    if (std.mem.endsWith(u8, wasm_path, ".wasm")) {
        const base = wasm_path[0 .. wasm_path.len - 5];
        const aot_ext = if (builtin.os.tag == .macos) ".dylib" else ".so";
        if (std.fmt.bufPrint(&aot_path_buf, "{s}{s}", .{ base, aot_ext })) |aot_path| {
            if (std.fs.cwd().access(aot_path, .{})) |_| {
                module_path = aot_path;
            } else |_| {}
        } else |_| {}
    }

    std.debug.print("[edgeboxd] Loading {s}...\n", .{module_path});

    // Initialize WasmEdge
    c.WasmEdge_LogSetErrorLevel();

    const conf = c.WasmEdge_ConfigureCreate();
    defer c.WasmEdge_ConfigureDelete(conf);
    c.WasmEdge_ConfigureAddHostRegistration(conf, c.WasmEdge_HostRegistration_Wasi);

    const vm = c.WasmEdge_VMCreate(conf, null);
    defer c.WasmEdge_VMDelete(vm);

    // Load and validate WASM
    var module_path_z: [4096]u8 = undefined;
    @memcpy(module_path_z[0..module_path.len], module_path);
    module_path_z[module_path.len] = 0;

    var result = c.WasmEdge_VMLoadWasmFromFile(vm, &module_path_z);
    if (!c.WasmEdge_ResultOK(result)) {
        std.debug.print("[edgeboxd] Failed to load: {s}\n", .{c.WasmEdge_ResultGetMessage(result)});
        std.process.exit(1);
    }

    result = c.WasmEdge_VMValidate(vm);
    if (!c.WasmEdge_ResultOK(result)) {
        std.debug.print("[edgeboxd] Failed to validate: {s}\n", .{c.WasmEdge_ResultGetMessage(result)});
        std.process.exit(1);
    }

    std.debug.print("[edgeboxd] WASM loaded and validated\n", .{});

    // Create TCP socket
    const server = std.posix.socket(std.posix.AF.INET, std.posix.SOCK.STREAM, 0) catch |err| {
        std.debug.print("[edgeboxd] Failed to create socket: {}\n", .{err});
        std.process.exit(1);
    };
    defer std.posix.close(server);

    // Set SO_REUSEADDR
    const optval: c_int = 1;
    std.posix.setsockopt(server, std.posix.SOL.SOCKET, std.posix.SO.REUSEADDR, std.mem.asBytes(&optval)) catch {};

    var addr: std.posix.sockaddr.in = .{
        .family = std.posix.AF.INET,
        .port = std.mem.nativeToBig(u16, port),
        .addr = 0, // INADDR_ANY
    };

    std.posix.bind(server, @ptrCast(&addr), @sizeOf(std.posix.sockaddr.in)) catch |err| {
        std.debug.print("[edgeboxd] Failed to bind port {}: {}\n", .{ port, err });
        std.process.exit(1);
    };

    std.posix.listen(server, 128) catch |err| {
        std.debug.print("[edgeboxd] Failed to listen: {}\n", .{err});
        std.process.exit(1);
    };

    std.debug.print("[edgeboxd] Listening on http://localhost:{}\n", .{port});
    std.debug.print("[edgeboxd] Ready for requests (Ctrl+C to stop)\n", .{});

    // Accept loop
    while (true) {
        const client = std.posix.accept(server, null, null, 0) catch |err| {
            std.debug.print("[edgeboxd] Accept error: {}\n", .{err});
            continue;
        };
        defer std.posix.close(client);

        // Read HTTP request
        var buf: [8192]u8 = undefined;
        const n = std.posix.read(client, &buf) catch continue;
        if (n == 0) continue;

        const request = buf[0..n];

        // Parse HTTP request - extract path from "GET /path HTTP/1.1"
        var lines = std.mem.splitScalar(u8, request, '\n');
        const first_line = lines.next() orelse continue;

        var parts = std.mem.splitScalar(u8, first_line, ' ');
        _ = parts.next(); // Skip method (GET)
        const path = parts.next() orelse continue;

        // Remove leading slash
        const script_path = if (path.len > 1 and path[0] == '/') path[1..] else path;

        // Trim any trailing \r
        const clean_path = std.mem.trimRight(u8, script_path, "\r");

        std.debug.print("[edgeboxd] GET /{s}\n", .{clean_path});

        // Time the execution
        const start = std.time.nanoTimestamp();

        // Get WASI module and configure for this request
        const wasi_module = c.WasmEdge_VMGetImportModuleContext(vm, c.WasmEdge_HostRegistration_Wasi);

        // Setup WASI args
        var wasi_args: [64][*c]const u8 = undefined;
        var wasi_argc: usize = 0;

        var script_path_z: [4096]u8 = undefined;
        @memcpy(script_path_z[0..clean_path.len], clean_path);
        script_path_z[clean_path.len] = 0;
        wasi_args[wasi_argc] = &script_path_z;
        wasi_argc += 1;

        // Preopens
        var preopens: [3][*c]const u8 = undefined;
        preopens[0] = ".:.";
        preopens[1] = "/tmp:/tmp";

        var cwd_buf: [1024]u8 = undefined;
        const cwd = std.process.getCwd(&cwd_buf) catch ".";
        var cwd_preopen_buf: [2048]u8 = undefined;
        const cwd_preopen = std.fmt.bufPrintZ(&cwd_preopen_buf, "{s}:{s}", .{ cwd, cwd }) catch ".:.";
        preopens[2] = cwd_preopen.ptr;

        c.WasmEdge_ModuleInstanceInitWASI(wasi_module, &wasi_args, @intCast(wasi_argc), null, 0, &preopens, 3);

        // Instantiate
        result = c.WasmEdge_VMInstantiate(vm);
        if (!c.WasmEdge_ResultOK(result)) {
            const http_err = "HTTP/1.1 500 Error\r\nContent-Length: 18\r\n\r\nInstantiate failed";
            _ = std.posix.write(client, http_err) catch {};
            continue;
        }

        // Execute _start
        const func_name = c.WasmEdge_StringCreateByCString("_start");
        defer c.WasmEdge_StringDelete(func_name);

        result = c.WasmEdge_VMExecute(vm, func_name, null, 0, null, 0);

        const elapsed = std.time.nanoTimestamp() - start;
        const elapsed_ms = @as(f64, @floatFromInt(elapsed)) / 1_000_000.0;

        // Send HTTP response
        var response_buf: [512]u8 = undefined;
        const body = std.fmt.bufPrint(&response_buf, "OK {d:.2}ms\n", .{elapsed_ms}) catch "OK\n";
        var http_response_buf: [1024]u8 = undefined;
        const http_response = std.fmt.bufPrint(&http_response_buf, "HTTP/1.1 200 OK\r\nContent-Type: text/plain\r\nContent-Length: {}\r\nConnection: close\r\n\r\n{s}", .{ body.len, body }) catch "HTTP/1.1 200 OK\r\n\r\nOK\n";
        _ = std.posix.write(client, http_response) catch {};

        std.debug.print("[edgeboxd] Completed in {d:.2}ms\n", .{elapsed_ms});
    }
}
