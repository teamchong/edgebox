/// EdgeBox Minimal Runner - Low-Level API with mmap
/// Includes host-side HTTP bridge for network requests from WASM
const std = @import("std");
const c = @cImport({
    @cInclude("wasmedge/wasmedge.h");
});

// Global state for HTTP bridge
var g_memory: ?*c.WasmEdge_MemoryInstanceContext = null;
var g_http_response: ?[]u8 = null;
var g_http_allocator: std.mem.Allocator = undefined;

// HTTP domain permissions (loaded from .edgebox.json)
var g_http_allowed_domains: ?[][]const u8 = null;
var g_http_denied_domains: ?[][]const u8 = null;

/// Extract domain from URL (e.g., "https://api.anthropic.com/v1/messages" -> "api.anthropic.com")
fn extractDomain(url: []const u8) ?[]const u8 {
    // Skip scheme
    var start: usize = 0;
    if (std.mem.startsWith(u8, url, "https://")) {
        start = 8;
    } else if (std.mem.startsWith(u8, url, "http://")) {
        start = 7;
    }

    // Find end of domain (first / or : after scheme)
    var end = start;
    while (end < url.len and url[end] != '/' and url[end] != ':') {
        end += 1;
    }

    if (end > start) {
        return url[start..end];
    }
    return null;
}

/// Check if domain matches a pattern (supports wildcard like "*.sentry.io")
fn domainMatches(domain: []const u8, pattern: []const u8) bool {
    if (std.mem.startsWith(u8, pattern, "*.")) {
        // Wildcard match: "*.sentry.io" matches "ingest.sentry.io"
        const suffix = pattern[1..]; // ".sentry.io"
        return std.mem.endsWith(u8, domain, suffix);
    }
    return std.mem.eql(u8, domain, pattern);
}

/// Check if URL is allowed by HTTP permissions
fn isUrlAllowed(url: []const u8) bool {
    const domain = extractDomain(url) orelse return false;

    // Check deny list first
    if (g_http_denied_domains) |denied| {
        for (denied) |pattern| {
            if (domainMatches(domain, pattern)) {
                return false;
            }
        }
    }

    // Check allow list (if empty, allow all)
    if (g_http_allowed_domains) |allowed| {
        if (allowed.len == 0) return true; // Empty allow list = allow all
        for (allowed) |pattern| {
            if (domainMatches(domain, pattern)) {
                return true;
            }
        }
        return false; // Not in allow list
    }

    return true; // No allow list configured = allow all
}

/// Load HTTP permissions from .edgebox.json
fn loadHttpPermissions(allocator: std.mem.Allocator) void {
    const config_file = std.fs.cwd().openFile(".edgebox.json", .{}) catch return;
    defer config_file.close();

    const content = config_file.readToEndAlloc(allocator, 1024 * 1024) catch return;
    defer allocator.free(content);

    var parsed = std.json.parseFromSlice(std.json.Value, allocator, content, .{}) catch return;
    defer parsed.deinit();

    if (parsed.value != .object) return;

    const http_obj = parsed.value.object.get("http") orelse return;
    if (http_obj != .object) return;

    // Parse allow list
    if (http_obj.object.get("allow")) |allow_arr| {
        if (allow_arr == .array) {
            var domains = allocator.alloc([]const u8, allow_arr.array.items.len) catch return;
            var count: usize = 0;
            for (allow_arr.array.items) |item| {
                if (item == .string) {
                    domains[count] = allocator.dupe(u8, item.string) catch continue;
                    count += 1;
                }
            }
            g_http_allowed_domains = domains[0..count];
        }
    }

    // Parse deny list
    if (http_obj.object.get("deny")) |deny_arr| {
        if (deny_arr == .array) {
            var domains = allocator.alloc([]const u8, deny_arr.array.items.len) catch return;
            var count: usize = 0;
            for (deny_arr.array.items) |item| {
                if (item == .string) {
                    domains[count] = allocator.dupe(u8, item.string) catch continue;
                    count += 1;
                }
            }
            g_http_denied_domains = domains[0..count];
        }
    }
}

fn stubVoid(_: ?*anyopaque, _: ?*const c.WasmEdge_CallingFrameContext, _: [*c]const c.WasmEdge_Value, _: [*c]c.WasmEdge_Value) callconv(.c) c.WasmEdge_Result {
    return c.WasmEdge_Result_Success;
}

fn stubZero(_: ?*anyopaque, _: ?*const c.WasmEdge_CallingFrameContext, _: [*c]const c.WasmEdge_Value, ret: [*c]c.WasmEdge_Value) callconv(.c) c.WasmEdge_Result {
    ret[0] = c.WasmEdge_ValueGenI32(0);
    return c.WasmEdge_Result_Success;
}

// ============================================================================
// HTTP Bridge - Host-side HTTP request handler
// ============================================================================

/// Read string from WASM memory
fn readWasmString(frame: ?*const c.WasmEdge_CallingFrameContext, ptr: u32, len: u32) ?[]const u8 {
    const mem = c.WasmEdge_CallingFrameGetMemoryInstance(frame, 0) orelse return null;
    const data = c.WasmEdge_MemoryInstanceGetPointer(mem, ptr, len);
    if (data == null) return null;
    return data[0..len];
}

/// Write data to WASM memory
fn writeWasmMemory(frame: ?*const c.WasmEdge_CallingFrameContext, ptr: u32, data: []const u8) bool {
    const mem = c.WasmEdge_CallingFrameGetMemoryInstance(frame, 0) orelse return false;
    const dest = c.WasmEdge_MemoryInstanceGetPointer(mem, ptr, @intCast(data.len));
    if (dest == null) return false;
    @memcpy(dest[0..data.len], data);
    return true;
}

/// Host function: edgebox_http_request
/// Args: url_ptr, url_len, method_ptr, method_len, headers_ptr, headers_len, body_ptr, body_len
/// Returns: status_code (negative on error), response stored in global buffer
fn hostHttpRequest(_: ?*anyopaque, frame: ?*const c.WasmEdge_CallingFrameContext, args: [*c]const c.WasmEdge_Value, ret: [*c]c.WasmEdge_Value) callconv(.c) c.WasmEdge_Result {
    const url_ptr: u32 = @bitCast(c.WasmEdge_ValueGetI32(args[0]));
    const url_len: u32 = @bitCast(c.WasmEdge_ValueGetI32(args[1]));
    const method_ptr: u32 = @bitCast(c.WasmEdge_ValueGetI32(args[2]));
    const method_len: u32 = @bitCast(c.WasmEdge_ValueGetI32(args[3]));
    const headers_ptr: u32 = @bitCast(c.WasmEdge_ValueGetI32(args[4]));
    const headers_len: u32 = @bitCast(c.WasmEdge_ValueGetI32(args[5]));
    const body_ptr: u32 = @bitCast(c.WasmEdge_ValueGetI32(args[6]));
    const body_len: u32 = @bitCast(c.WasmEdge_ValueGetI32(args[7]));

    // Read strings from WASM memory
    const url = readWasmString(frame, url_ptr, url_len) orelse {
        ret[0] = c.WasmEdge_ValueGenI32(-1);
        return c.WasmEdge_Result_Success;
    };
    const method = readWasmString(frame, method_ptr, method_len) orelse "GET";
    const headers_json = if (headers_len > 0) readWasmString(frame, headers_ptr, headers_len) else null;
    const body = if (body_len > 0) readWasmString(frame, body_ptr, body_len) else null;

    // Check if URL is allowed by .edgebox.json http permissions
    if (!isUrlAllowed(url)) {
        std.debug.print("[HTTP Bridge] Domain not allowed: {s}\n", .{extractDomain(url) orelse "unknown"});
        ret[0] = c.WasmEdge_ValueGenI32(-3); // Permission denied
        return c.WasmEdge_Result_Success;
    }

    // Free previous response
    if (g_http_response) |resp| {
        g_http_allocator.free(resp);
        g_http_response = null;
    }

    // Make HTTP request using Zig's std.http.Client
    const status = makeHttpRequest(url, method, headers_json, body) catch |err| {
        std.debug.print("[HTTP Bridge] Request failed: {}\n", .{err});
        ret[0] = c.WasmEdge_ValueGenI32(-2);
        return c.WasmEdge_Result_Success;
    };

    ret[0] = c.WasmEdge_ValueGenI32(@intCast(status));
    return c.WasmEdge_Result_Success;
}

/// Host function: edgebox_http_get_response_len
/// Returns the length of the last HTTP response
fn hostHttpGetResponseLen(_: ?*anyopaque, _: ?*const c.WasmEdge_CallingFrameContext, _: [*c]const c.WasmEdge_Value, ret: [*c]c.WasmEdge_Value) callconv(.c) c.WasmEdge_Result {
    const len: i32 = if (g_http_response) |resp| @intCast(resp.len) else 0;
    ret[0] = c.WasmEdge_ValueGenI32(len);
    return c.WasmEdge_Result_Success;
}

/// Host function: edgebox_http_get_response
/// Copies the HTTP response to WASM memory at the given pointer
fn hostHttpGetResponse(_: ?*anyopaque, frame: ?*const c.WasmEdge_CallingFrameContext, args: [*c]const c.WasmEdge_Value, ret: [*c]c.WasmEdge_Value) callconv(.c) c.WasmEdge_Result {
    const dest_ptr: u32 = @bitCast(c.WasmEdge_ValueGetI32(args[0]));

    if (g_http_response) |resp| {
        if (writeWasmMemory(frame, dest_ptr, resp)) {
            ret[0] = c.WasmEdge_ValueGenI32(@intCast(resp.len));
            return c.WasmEdge_Result_Success;
        }
    }
    ret[0] = c.WasmEdge_ValueGenI32(0);
    return c.WasmEdge_Result_Success;
}

/// Make actual HTTP request using curl subprocess
/// This is simpler and more reliable than using Zig's HTTP client directly
fn makeHttpRequest(url: []const u8, method: []const u8, headers_json: ?[]const u8, body: ?[]const u8) !u16 {
    _ = headers_json; // TODO: parse and pass headers to curl

    // Build curl command - use fixed array since we know max args
    var args_buf: [20][]const u8 = undefined;
    var args_len: usize = 0;

    args_buf[args_len] = "curl";
    args_len += 1;
    args_buf[args_len] = "-s"; // Silent
    args_len += 1;
    args_buf[args_len] = "-S"; // Show errors
    args_len += 1;
    args_buf[args_len] = "-w"; // Write out status code
    args_len += 1;
    args_buf[args_len] = "\n%{http_code}";
    args_len += 1;
    args_buf[args_len] = "-X";
    args_len += 1;
    args_buf[args_len] = method;
    args_len += 1;

    // Add body if present
    if (body) |b| {
        args_buf[args_len] = "-d";
        args_len += 1;
        args_buf[args_len] = b;
        args_len += 1;
        args_buf[args_len] = "-H";
        args_len += 1;
        args_buf[args_len] = "Content-Type: application/json";
        args_len += 1;
    }

    args_buf[args_len] = url;
    args_len += 1;

    // Run curl
    var child = std.process.Child.init(args_buf[0..args_len], g_http_allocator);
    child.stdout_behavior = .Pipe;
    child.stderr_behavior = .Pipe;

    try child.spawn();

    // Read output using File's readToEndAlloc
    const stdout = try child.stdout.?.readToEndAlloc(g_http_allocator, 10 * 1024 * 1024);
    defer g_http_allocator.free(stdout);

    const result = try child.wait();

    if (result.Exited != 0) {
        return error.CurlFailed;
    }

    // Parse status code from last line
    var status_code: u16 = 0;
    var response_body: []const u8 = stdout;

    if (std.mem.lastIndexOf(u8, stdout, "\n")) |last_newline| {
        response_body = stdout[0..last_newline];
        const status_str = stdout[last_newline + 1 ..];
        status_code = std.fmt.parseInt(u16, std.mem.trim(u8, status_str, " \n\r"), 10) catch 0;
    }

    // Build JSON response
    var json_buf = std.ArrayListUnmanaged(u8){};
    defer json_buf.deinit(g_http_allocator);

    const writer = json_buf.writer(g_http_allocator);
    try writer.print("{{\"status\":{d},\"ok\":{s},\"body\":", .{
        status_code,
        if (status_code >= 200 and status_code < 300) "true" else "false",
    });

    // JSON-escape the body (write as quoted string)
    try writer.writeByte('"');
    for (response_body) |ch| {
        switch (ch) {
            '"' => try writer.writeAll("\\\""),
            '\\' => try writer.writeAll("\\\\"),
            '\n' => try writer.writeAll("\\n"),
            '\r' => try writer.writeAll("\\r"),
            '\t' => try writer.writeAll("\\t"),
            else => {
                if (ch < 0x20) {
                    try writer.print("\\u{x:0>4}", .{ch});
                } else {
                    try writer.writeByte(ch);
                }
            },
        }
    }
    try writer.writeByte('"');
    try writer.writeAll(",\"headers\":{}}");

    // Store response globally
    g_http_response = try json_buf.toOwnedSlice(g_http_allocator);

    return status_code;
}

/// Create the edgebox_http host module
fn createHttpBridge() ?*c.WasmEdge_ModuleInstanceContext {
    initTypes();
    const mn = c.WasmEdge_StringCreateByCString("edgebox_http");
    defer c.WasmEdge_StringDelete(mn);
    const m = c.WasmEdge_ModuleInstanceCreate(mn) orelse return null;

    // edgebox_http_request(url_ptr, url_len, method_ptr, method_len, headers_ptr, headers_len, body_ptr, body_len) -> status
    const params_8i32 = [_]c.WasmEdge_ValType{ g_i32, g_i32, g_i32, g_i32, g_i32, g_i32, g_i32, g_i32 };
    const ret_i32 = [_]c.WasmEdge_ValType{g_i32};
    addFunc(m, "request", &params_8i32, &ret_i32, hostHttpRequest);

    // edgebox_http_get_response_len() -> len
    addFunc(m, "get_response_len", &.{}, &ret_i32, hostHttpGetResponseLen);

    // edgebox_http_get_response(dest_ptr) -> bytes_written
    const params_1i32 = [_]c.WasmEdge_ValType{g_i32};
    addFunc(m, "get_response", &params_1i32, &ret_i32, hostHttpGetResponse);

    return m;
}

inline fn addFunc(m: ?*c.WasmEdge_ModuleInstanceContext, name: [*:0]const u8, p: []const c.WasmEdge_ValType, r: []const c.WasmEdge_ValType, f: c.WasmEdge_HostFunc_t) void {
    const ft = c.WasmEdge_FunctionTypeCreate(p.ptr, @intCast(p.len), r.ptr, @intCast(r.len));
    const fi = c.WasmEdge_FunctionInstanceCreate(ft, f, null, 0);
    const fn_name = c.WasmEdge_StringCreateByCString(name);
    c.WasmEdge_ModuleInstanceAddFunction(m, fn_name, fi);
    c.WasmEdge_StringDelete(fn_name);
    c.WasmEdge_FunctionTypeDelete(ft);
}

// Pre-create commonly used types to avoid repeated allocations
var g_i32: c.WasmEdge_ValType = undefined;
var g_types_init = false;

fn initTypes() void {
    if (!g_types_init) {
        g_i32 = c.WasmEdge_ValTypeGenI32();
        g_types_init = true;
    }
}

fn createProcessStub() ?*c.WasmEdge_ModuleInstanceContext {
    initTypes();
    const mn = c.WasmEdge_StringCreateByCString("wasmedge_process");
    defer c.WasmEdge_StringDelete(mn);
    const m = c.WasmEdge_ModuleInstanceCreate(mn) orelse return null;
    const ii = [_]c.WasmEdge_ValType{ g_i32, g_i32 };
    const iiii = [_]c.WasmEdge_ValType{ g_i32, g_i32, g_i32, g_i32 };
    const ri = [_]c.WasmEdge_ValType{g_i32};
    addFunc(m, "wasmedge_process_set_prog_name", &ii, &.{}, stubVoid);
    addFunc(m, "wasmedge_process_add_arg", &ii, &.{}, stubVoid);
    addFunc(m, "wasmedge_process_add_stdin", &ii, &.{}, stubVoid);
    addFunc(m, "wasmedge_process_add_env", &iiii, &.{}, stubVoid);
    addFunc(m, "wasmedge_process_set_timeout", &ri, &.{}, stubVoid);
    addFunc(m, "wasmedge_process_get_stdout", &ri, &.{}, stubVoid);
    addFunc(m, "wasmedge_process_get_stderr", &ri, &.{}, stubVoid);
    addFunc(m, "wasmedge_process_run", &.{}, &ri, stubZero);
    addFunc(m, "wasmedge_process_get_exit_code", &.{}, &ri, stubZero);
    addFunc(m, "wasmedge_process_get_stdout_len", &.{}, &ri, stubZero);
    addFunc(m, "wasmedge_process_get_stderr_len", &.{}, &ri, stubZero);
    return m;
}

const TIMING = false; // Set to true for timing debug

fn timer() i64 {
    return std.time.microTimestamp();
}

fn printTiming(label: []const u8, start: i64) i64 {
    if (TIMING) {
        const now = timer();
        std.debug.print("{s}: {d}us\n", .{ label, now - start });
        return now;
    }
    return start;
}

pub fn main() !void {
    // Initialize global allocator for HTTP bridge
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    g_http_allocator = gpa.allocator();

    // Load HTTP permissions from .edgebox.json
    loadHttpPermissions(g_http_allocator);

    var t = timer();
    var args_iter = std.process.args();
    _ = args_iter.next();
    const path = args_iter.next() orelse {
        std.debug.print("Usage: edgebox <file.wasm|dylib>\n", .{});
        return;
    };
    t = printTiming("args", t);

    var wasi_args: [64][*c]const u8 = undefined;
    var argc: usize = 0;
    wasi_args[argc] = path.ptr;
    argc += 1;
    while (args_iter.next()) |a| {
        if (argc < 64) {
            wasi_args[argc] = a.ptr;
            argc += 1;
        }
    }

    c.WasmEdge_LogSetErrorLevel();
    const conf = c.WasmEdge_ConfigureCreate();
    defer c.WasmEdge_ConfigureDelete(conf);

    // Remove all unused proposals for faster cold start
    inline for (.{
        c.WasmEdge_Proposal_Threads,
        c.WasmEdge_Proposal_TailCall,
        c.WasmEdge_Proposal_ExceptionHandling,
        c.WasmEdge_Proposal_Memory64,
        c.WasmEdge_Proposal_ExtendedConst,
        c.WasmEdge_Proposal_Component,
        c.WasmEdge_Proposal_FunctionReferences,
        c.WasmEdge_Proposal_GC,
        c.WasmEdge_Proposal_MultiMemories,
        c.WasmEdge_Proposal_RelaxSIMD,
        c.WasmEdge_Proposal_Annotations,
    }) |p| c.WasmEdge_ConfigureRemoveProposal(conf, p);
    t = printTiming("config", t);

    const loader = c.WasmEdge_LoaderCreate(conf) orelse return error.LoaderFailed;
    defer c.WasmEdge_LoaderDelete(loader);
    t = printTiming("loader", t);

    var ast: ?*c.WasmEdge_ASTModuleContext = null;
    var res: c.WasmEdge_Result = undefined;
    var mapped: ?[]align(std.heap.page_size_min) u8 = null;

    const path_str = std.mem.span(path.ptr);
    const is_dylib = std.mem.endsWith(u8, path_str, ".dylib") or std.mem.endsWith(u8, path_str, ".so");
    const is_js = std.mem.endsWith(u8, path_str, ".js") or std.mem.endsWith(u8, path_str, ".cjs") or std.mem.endsWith(u8, path_str, ".mjs");

    // For .js files, use the edgebox-base.wasm module and pass JS file as argument
    var wasm_path_buf: [4096]u8 = undefined;
    var actual_path: [*c]const u8 = path.ptr;
    if (is_js) {
        // Find edgebox-base.wasm in the same directory as this executable
        const exe_path = std.fs.selfExePath(&wasm_path_buf) catch {
            // Fallback to looking in current directory
            @memcpy(wasm_path_buf[0..18], "edgebox-base.wasm\x00");
            actual_path = @ptrCast(&wasm_path_buf);
            return; // Will fail with proper error
        };
        // Find directory of executable
        var dir_end: usize = 0;
        for (exe_path, 0..) |byte, i| {
            if (byte == '/') dir_end = i;
        }
        if (dir_end > 0) {
            @memcpy(wasm_path_buf[0..dir_end], exe_path[0..dir_end]);
            @memcpy(wasm_path_buf[dir_end .. dir_end + 19], "/edgebox-base.wasm\x00");
            actual_path = @ptrCast(&wasm_path_buf);
        } else {
            @memcpy(wasm_path_buf[0..18], "edgebox-base.wasm\x00");
            actual_path = @ptrCast(&wasm_path_buf);
        }
        // Update wasi_args to include the JS file as first real argument
        // wasi_args[0] is the wasm module path, wasi_args[1..] are the JS args
        // We need to shift: wasm_module, js_file, original_args...
        if (argc < 63) {
            // Shift existing args right
            var i: usize = argc;
            while (i > 0) : (i -= 1) {
                wasi_args[i] = wasi_args[i - 1];
            }
            wasi_args[0] = path.ptr; // JS file as first arg
            argc += 1;
        }
    }

    // mmap only works for .wasm files, not AOT .dylib/.so
    if (!is_dylib and !is_js) {
        const file = std.fs.cwd().openFile(path_str, .{}) catch null;
        if (file) |f| {
            defer f.close();
            const size = f.getEndPos() catch 0;
            if (size > 0) {
                mapped = std.posix.mmap(null, size, std.posix.PROT.READ, .{ .TYPE = .PRIVATE }, f.handle, 0) catch null;
                if (mapped) |m| {
                    res = c.WasmEdge_LoaderParseFromBuffer(loader, &ast, m.ptr, @intCast(m.len));
                }
            }
        }
    }

    // Fallback to file-based loading (required for dylib/so or js files)
    if (ast == null) {
        if (mapped) |m| std.posix.munmap(m);
        mapped = null;
        res = c.WasmEdge_LoaderParseFromFile(loader, &ast, actual_path);
    }
    defer if (mapped) |m| std.posix.munmap(m);
    t = printTiming("parse", t);

    if (!c.WasmEdge_ResultOK(res) or ast == null) return error.ParseFailed;
    defer c.WasmEdge_ASTModuleDelete(ast);

    const validator = c.WasmEdge_ValidatorCreate(conf) orelse return error.ValidatorFailed;
    defer c.WasmEdge_ValidatorDelete(validator);
    res = c.WasmEdge_ValidatorValidate(validator, ast);
    if (!c.WasmEdge_ResultOK(res)) return error.ValidationFailed;
    t = printTiming("validate", t);

    const executor = c.WasmEdge_ExecutorCreate(conf, null) orelse return error.ExecutorFailed;
    defer c.WasmEdge_ExecutorDelete(executor);
    const store = c.WasmEdge_StoreCreate() orelse return error.StoreFailed;
    defer c.WasmEdge_StoreDelete(store);
    t = printTiming("executor", t);

    // Preopened directories for WASI - need wide access for Claude CLI file operations
    // Format is "guest_path:host_path"
    var preopens: [5][*c]const u8 = undefined;
    var preopen_bufs: [5][512]u8 = undefined;
    var preopen_count: usize = 0;

    // Always preopen current directory
    preopens[preopen_count] = ".:.";
    preopen_count += 1;

    // Preopen /tmp for temp files
    preopens[preopen_count] = "/tmp:/tmp";
    preopen_count += 1;

    // Preopen home directory if available
    if (std.posix.getenv("HOME")) |home| {
        const formatted = std.fmt.bufPrintZ(&preopen_bufs[preopen_count], "{s}:{s}", .{ home, home }) catch null;
        if (formatted) |f| {
            preopens[preopen_count] = f.ptr;
            preopen_count += 1;
        }
    }

    // Try to preopen root - may fail on some systems
    preopens[preopen_count] = "/:/";
    preopen_count += 1;

    // Pass through important environment variables to WASI
    // Use static buffers to keep strings alive for the WASI call
    var env_vars: [16][*c]const u8 = undefined;
    var env_bufs: [16][1024]u8 = undefined;
    var env_count: usize = 0;
    const important_vars = [_][]const u8{ "HOME", "PWD", "USER", "PATH", "TMPDIR", "ANTHROPIC_API_KEY", "TERM", "SHELL", "HOSTNAME", "EDGEBOX_DEBUG" };
    for (important_vars) |name| {
        if (std.posix.getenv(name)) |val| {
            // Format: "NAME=VALUE"
            if (env_count < 16) {
                const formatted = std.fmt.bufPrintZ(&env_bufs[env_count], "{s}={s}", .{ name, val }) catch continue;
                env_vars[env_count] = formatted.ptr;
                env_count += 1;
            }
        }
    }

    const wasi = c.WasmEdge_ModuleInstanceCreateWASI(&wasi_args, @intCast(argc), if (env_count > 0) &env_vars else null, @intCast(env_count), &preopens, @intCast(preopen_count)) orelse return error.WasiFailed;
    defer c.WasmEdge_ModuleInstanceDelete(wasi);
    _ = c.WasmEdge_ExecutorRegisterImport(executor, store, wasi);
    t = printTiming("wasi", t);

    const proc = createProcessStub() orelse return error.ProcessFailed;
    defer c.WasmEdge_ModuleInstanceDelete(proc);
    _ = c.WasmEdge_ExecutorRegisterImport(executor, store, proc);
    t = printTiming("proc", t);

    // Register HTTP bridge for network requests
    const http = createHttpBridge() orelse return error.HttpBridgeFailed;
    defer c.WasmEdge_ModuleInstanceDelete(http);
    _ = c.WasmEdge_ExecutorRegisterImport(executor, store, http);
    t = printTiming("http", t);

    var mod: ?*c.WasmEdge_ModuleInstanceContext = null;
    res = c.WasmEdge_ExecutorInstantiate(executor, &mod, store, ast);
    if (!c.WasmEdge_ResultOK(res)) return error.InstantiateFailed;
    defer c.WasmEdge_ModuleInstanceDelete(mod);
    t = printTiming("instantiate", t);

    const fn_name = c.WasmEdge_StringCreateByCString("_start");
    defer c.WasmEdge_StringDelete(fn_name);
    const func = c.WasmEdge_ModuleInstanceFindFunction(mod, fn_name) orelse return error.FuncNotFound;
    t = printTiming("findfunc", t);
    res = c.WasmEdge_ExecutorInvoke(executor, func, null, 0, null, 0);
    _ = printTiming("exec", t);
    if (!c.WasmEdge_ResultOK(res)) {
        const msg = c.WasmEdge_ResultGetMessage(res);
        if (msg != null and std.mem.indexOf(u8, std.mem.span(msg), "terminated") == null) {
            return error.ExecFailed;
        }
    }
}
