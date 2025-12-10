/// EdgeBox Minimal Runner - Low-Level API with mmap
const std = @import("std");
const c = @cImport({
    @cInclude("wasmedge/wasmedge.h");
});

fn stubVoid(_: ?*anyopaque, _: ?*const c.WasmEdge_CallingFrameContext, _: [*c]const c.WasmEdge_Value, _: [*c]c.WasmEdge_Value) callconv(.c) c.WasmEdge_Result {
    return c.WasmEdge_Result_Success;
}

fn stubZero(_: ?*anyopaque, _: ?*const c.WasmEdge_CallingFrameContext, _: [*c]const c.WasmEdge_Value, ret: [*c]c.WasmEdge_Value) callconv(.c) c.WasmEdge_Result {
    ret[0] = c.WasmEdge_ValueGenI32(0);
    return c.WasmEdge_Result_Success;
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
