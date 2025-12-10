/// EdgeBox Embedded Runner - Single file deployment
/// Embeds WASM bytecode directly in the binary for instant startup (no file I/O)
const std = @import("std");
const c = @cImport({
    @cInclude("wasmedge/wasmedge.h");
});

// Embedded WASM bytes - set by copying wizered WASM to src/embedded_wasm.bin
const embedded_wasm = @embedFile("embedded_wasm.bin");

// Global types (initialized once)
var g_i32: c.WasmEdge_ValType = undefined;
var g_types_init: bool = false;

fn initTypes() void {
    if (g_types_init) return;
    g_i32 = c.WasmEdge_ValTypeGenI32();
    g_types_init = true;
}

inline fn addFunc(m: ?*c.WasmEdge_ModuleInstanceContext, name: [*:0]const u8, p: []const c.WasmEdge_ValType, r: []const c.WasmEdge_ValType, f: c.WasmEdge_HostFunc_t) void {
    const ft = c.WasmEdge_FunctionTypeCreate(p.ptr, @intCast(p.len), r.ptr, @intCast(r.len));
    const fi = c.WasmEdge_FunctionInstanceCreate(ft, f, null, 0);
    const fn_name = c.WasmEdge_StringCreateByCString(name);
    c.WasmEdge_ModuleInstanceAddFunction(m, fn_name, fi);
    c.WasmEdge_StringDelete(fn_name);
}

// Stub functions for unused bridges
fn stubVoid(_: ?*anyopaque, _: ?*const c.WasmEdge_CallingFrameContext, _: [*c]const c.WasmEdge_Value, _: [*c]c.WasmEdge_Value) callconv(.c) c.WasmEdge_Result {
    return c.WasmEdge_Result_Success;
}

fn stubZero(_: ?*anyopaque, _: ?*const c.WasmEdge_CallingFrameContext, _: [*c]const c.WasmEdge_Value, ret: [*c]c.WasmEdge_Value) callconv(.c) c.WasmEdge_Result {
    ret[0] = c.WasmEdge_ValueGenI32(0);
    return c.WasmEdge_Result_Success;
}

fn stubNegOne(_: ?*anyopaque, _: ?*const c.WasmEdge_CallingFrameContext, _: [*c]const c.WasmEdge_Value, ret: [*c]c.WasmEdge_Value) callconv(.c) c.WasmEdge_Result {
    ret[0] = c.WasmEdge_ValueGenI32(-1);
    return c.WasmEdge_Result_Success;
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

// Stub bridges for the 6 dispatch modules (they just return -1 since hello.js doesn't use them)
fn createHttpBridge() ?*c.WasmEdge_ModuleInstanceContext {
    initTypes();
    const mn = c.WasmEdge_StringCreateByCString("edgebox_http");
    defer c.WasmEdge_StringDelete(mn);
    const m = c.WasmEdge_ModuleInstanceCreate(mn) orelse return null;
    const params = [_]c.WasmEdge_ValType{ g_i32, g_i32, g_i32, g_i32, g_i32, g_i32, g_i32, g_i32, g_i32 };
    const ret_i32 = [_]c.WasmEdge_ValType{g_i32};
    addFunc(m, "http_dispatch", &params, &ret_i32, stubNegOne);
    return m;
}

fn createSpawnBridge() ?*c.WasmEdge_ModuleInstanceContext {
    initTypes();
    const mn = c.WasmEdge_StringCreateByCString("edgebox_spawn");
    defer c.WasmEdge_StringDelete(mn);
    const m = c.WasmEdge_ModuleInstanceCreate(mn) orelse return null;
    const params = [_]c.WasmEdge_ValType{ g_i32, g_i32, g_i32, g_i32, g_i32 };
    const ret_i32 = [_]c.WasmEdge_ValType{g_i32};
    addFunc(m, "spawn_dispatch", &params, &ret_i32, stubNegOne);
    return m;
}

fn createFileBridge() ?*c.WasmEdge_ModuleInstanceContext {
    initTypes();
    const mn = c.WasmEdge_StringCreateByCString("edgebox_file");
    defer c.WasmEdge_StringDelete(mn);
    const m = c.WasmEdge_ModuleInstanceCreate(mn) orelse return null;
    const params = [_]c.WasmEdge_ValType{ g_i32, g_i32, g_i32, g_i32, g_i32 };
    const ret_i32 = [_]c.WasmEdge_ValType{g_i32};
    addFunc(m, "file_dispatch", &params, &ret_i32, stubNegOne);
    return m;
}

fn createZlibBridge() ?*c.WasmEdge_ModuleInstanceContext {
    initTypes();
    const mn = c.WasmEdge_StringCreateByCString("edgebox_zlib");
    defer c.WasmEdge_StringDelete(mn);
    const m = c.WasmEdge_ModuleInstanceCreate(mn) orelse return null;
    const params = [_]c.WasmEdge_ValType{ g_i32, g_i32, g_i32 };
    const ret_i32 = [_]c.WasmEdge_ValType{g_i32};
    addFunc(m, "zlib_dispatch", &params, &ret_i32, stubNegOne);
    return m;
}

fn createCryptoBridge() ?*c.WasmEdge_ModuleInstanceContext {
    initTypes();
    const mn = c.WasmEdge_StringCreateByCString("edgebox_crypto");
    defer c.WasmEdge_StringDelete(mn);
    const m = c.WasmEdge_ModuleInstanceCreate(mn) orelse return null;
    const params = [_]c.WasmEdge_ValType{ g_i32, g_i32, g_i32, g_i32, g_i32, g_i32, g_i32 };
    const ret_i32 = [_]c.WasmEdge_ValType{g_i32};
    addFunc(m, "crypto_dispatch", &params, &ret_i32, stubNegOne);
    return m;
}

fn createSocketBridge() ?*c.WasmEdge_ModuleInstanceContext {
    initTypes();
    const mn = c.WasmEdge_StringCreateByCString("edgebox_socket");
    defer c.WasmEdge_StringDelete(mn);
    const m = c.WasmEdge_ModuleInstanceCreate(mn) orelse return null;
    const params = [_]c.WasmEdge_ValType{ g_i32, g_i32, g_i32, g_i32 };
    const ret_i32 = [_]c.WasmEdge_ValType{g_i32};
    addFunc(m, "socket_dispatch", &params, &ret_i32, stubNegOne);
    return m;
}

pub fn main() !void {
    const start_time = std.time.microTimestamp();

    // Parse args
    var args_iter = std.process.args();
    _ = args_iter.next(); // skip exe name

    var wasi_args: [64][*c]const u8 = undefined;
    var argc: usize = 0;
    wasi_args[argc] = "embedded";
    argc += 1;
    while (args_iter.next()) |a| {
        if (argc < 64) {
            wasi_args[argc] = a.ptr;
            argc += 1;
        }
    }

    // Setup WasmEdge with minimal proposals
    c.WasmEdge_LogSetErrorLevel();
    const conf = c.WasmEdge_ConfigureCreate();
    defer c.WasmEdge_ConfigureDelete(conf);

    // Remove unused proposals for faster startup
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

    // Load WASM from embedded buffer (no file I/O!)
    const loader = c.WasmEdge_LoaderCreate(conf) orelse return error.LoaderFailed;
    defer c.WasmEdge_LoaderDelete(loader);

    var ast: ?*c.WasmEdge_ASTModuleContext = null;
    const res = c.WasmEdge_LoaderParseFromBuffer(loader, &ast, embedded_wasm.ptr, embedded_wasm.len);
    if (!c.WasmEdge_ResultOK(res)) {
        std.debug.print("Failed to parse embedded WASM\n", .{});
        return error.ParseFailed;
    }
    defer c.WasmEdge_ASTModuleDelete(ast);

    const parse_time = std.time.microTimestamp();

    // Validate
    const validator = c.WasmEdge_ValidatorCreate(conf) orelse return error.ValidatorFailed;
    defer c.WasmEdge_ValidatorDelete(validator);
    const vres = c.WasmEdge_ValidatorValidate(validator, ast);
    if (!c.WasmEdge_ResultOK(vres)) return error.ValidateFailed;

    // Execute
    const executor = c.WasmEdge_ExecutorCreate(conf, null) orelse return error.ExecutorFailed;
    defer c.WasmEdge_ExecutorDelete(executor);

    const store = c.WasmEdge_StoreCreate() orelse return error.StoreFailed;
    defer c.WasmEdge_StoreDelete(store);

    // Setup WASI
    var preopens = [_][*c]const u8{"."};
    const wasi = c.WasmEdge_ModuleInstanceCreateWASI(
        &wasi_args,
        @intCast(argc),
        null,
        0,
        &preopens,
        1,
    ) orelse return error.WasiFailed;
    defer c.WasmEdge_ModuleInstanceDelete(wasi);
    _ = c.WasmEdge_ExecutorRegisterImport(executor, store, wasi);

    // Register stub bridges
    const proc = createProcessStub() orelse return error.ProcessFailed;
    defer c.WasmEdge_ModuleInstanceDelete(proc);
    _ = c.WasmEdge_ExecutorRegisterImport(executor, store, proc);

    const http = createHttpBridge() orelse return error.HttpBridgeFailed;
    defer c.WasmEdge_ModuleInstanceDelete(http);
    _ = c.WasmEdge_ExecutorRegisterImport(executor, store, http);

    const spawn_bridge = createSpawnBridge() orelse return error.SpawnBridgeFailed;
    defer c.WasmEdge_ModuleInstanceDelete(spawn_bridge);
    _ = c.WasmEdge_ExecutorRegisterImport(executor, store, spawn_bridge);

    const file_bridge = createFileBridge() orelse return error.FileBridgeFailed;
    defer c.WasmEdge_ModuleInstanceDelete(file_bridge);
    _ = c.WasmEdge_ExecutorRegisterImport(executor, store, file_bridge);

    const zlib_bridge = createZlibBridge() orelse return error.ZlibBridgeFailed;
    defer c.WasmEdge_ModuleInstanceDelete(zlib_bridge);
    _ = c.WasmEdge_ExecutorRegisterImport(executor, store, zlib_bridge);

    const crypto_bridge = createCryptoBridge() orelse return error.CryptoBridgeFailed;
    defer c.WasmEdge_ModuleInstanceDelete(crypto_bridge);
    _ = c.WasmEdge_ExecutorRegisterImport(executor, store, crypto_bridge);

    const socket_bridge = createSocketBridge() orelse return error.SocketBridgeFailed;
    defer c.WasmEdge_ModuleInstanceDelete(socket_bridge);
    _ = c.WasmEdge_ExecutorRegisterImport(executor, store, socket_bridge);

    // Instantiate
    var mod: ?*c.WasmEdge_ModuleInstanceContext = null;
    const ires = c.WasmEdge_ExecutorInstantiate(executor, &mod, store, ast);
    if (!c.WasmEdge_ResultOK(ires)) return error.InstantiateFailed;
    defer c.WasmEdge_ModuleInstanceDelete(mod);

    const ready_time = std.time.microTimestamp();
    std.debug.print("startup: {d}us (parse: {d}us from memory)\n", .{ ready_time - start_time, parse_time - start_time });

    // Run
    const fn_name = c.WasmEdge_StringCreateByCString("_start");
    defer c.WasmEdge_StringDelete(fn_name);
    const func = c.WasmEdge_ModuleInstanceFindFunction(mod, fn_name) orelse return error.FuncNotFound;
    const eres = c.WasmEdge_ExecutorInvoke(executor, func, null, 0, null, 0);

    if (!c.WasmEdge_ResultOK(eres)) {
        const msg = c.WasmEdge_ResultGetMessage(eres);
        if (msg != null and std.mem.indexOf(u8, std.mem.span(msg), "terminated") == null) {
            return error.ExecFailed;
        }
    }
}
