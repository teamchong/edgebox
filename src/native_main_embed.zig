/// Native Entry Point with Embedded Bytecode
/// Uses @embedFile for raw bytecode - no C file generation
/// Solves OOM from parsing 321MB C hex arrays
const std = @import("std");
const builtin = @import("builtin");

// QuickJS C API - use shared import for type compatibility with polyfills
const quickjs = @import("quickjs_core.zig");
const qjs = quickjs.c;

// Zig 0.15 compatible JS_TRUE
inline fn jsTrue() qjs.JSValue {
    return quickjs.jsTrue();
}

// Native polyfills
const path_polyfill = @import("polyfills/path.zig");
const process_polyfill = @import("polyfills/process.zig");
const console_polyfill = @import("polyfills/console.zig");
const buffer_polyfill = @import("polyfills/buffer.zig");
const util_polyfill = @import("polyfills/util.zig");
const encoding_polyfill = @import("polyfills/encoding.zig");
const require_polyfill = @import("polyfills/require.zig");
const dns_polyfill = @import("polyfills/dns.zig");
const child_process_polyfill = @import("polyfills/child_process.zig");
const net_polyfill = @import("polyfills/net.zig");
const tls_polyfill = @import("polyfills/tls.zig");
const querystring_polyfill = @import("polyfills/querystring.zig");
const compression_polyfill = @import("polyfills/compression.zig");
const globals_polyfill = @import("polyfills/globals.zig");
const os_polyfill = @import("polyfills/os.zig");
const fs_polyfill = @import("polyfills/fs.zig");
const crypto_polyfill = @import("polyfills/crypto.zig");
const tty_polyfill = @import("polyfills/tty.zig");
const assert_polyfill = @import("polyfills/assert.zig");
const string_decoder_polyfill = @import("polyfills/string_decoder.zig");
const native_bindings = @import("native_bindings.zig");

// Zig native registry (replaces C frozen_runtime.c registry)
const native_shapes_registry = @import("freeze/native_shapes.zig");
comptime {
    _ = native_shapes_registry.native_registry_init;
    _ = native_shapes_registry.native_node_register;
    _ = native_shapes_registry.native_node_lookup;
}

// Native dispatch for frozen functions - must be linked even with --no-freeze
// because QuickJS C code unconditionally calls frozen_dispatch_lookup
// Use module name (not path) to match how frozen_module.zig imports it
const native_dispatch = @import("native_dispatch");
comptime {
    _ = &native_dispatch.frozen_dispatch_lookup;
}

// Frozen module (generated Zig frozen functions)
const frozen_module = @import("frozen_module");

// Native arena allocator for QuickJS (faster than libc malloc for TSC workloads)
const native_arena = @import("native_arena.zig");
comptime {
    _ = &frozen_module.frozen_init_c;
}

// Zig hot paths
const zig_hotpaths = @import("zig_hotpaths");
comptime {
    _ = &zig_hotpaths;
}

// Embed raw bytecode - provided via anonymous import from build.zig
const bytecode_module = @import("bytecode");
const bytecode: []const u8 = bytecode_module.data;

// Allocator configuration - set via -Dallocator=c|arena|gpa at compile time
const allocator_config = @import("allocator_config");

// Allocator instances (only used if selected)
var gpa_instance = std.heap.GeneralPurposeAllocator(.{}){};
var arena_instance: ?std.heap.ArenaAllocator = null;

fn getAllocator() std.mem.Allocator {
    return switch (allocator_config.allocator_type) {
        .gpa => gpa_instance.allocator(),
        .arena => blk: {
            if (arena_instance == null) {
                arena_instance = std.heap.ArenaAllocator.init(std.heap.page_allocator);
            }
            break :blk arena_instance.?.allocator();
        },
        .c => std.heap.c_allocator,
    };
}

pub fn main() !void {
    const allocator = getAllocator();

    // Cleanup arena on exit if used
    defer if (arena_instance) |*arena| arena.deinit();

    // Initialize native bindings
    native_bindings.init(allocator);

    // Create QuickJS runtime
    // Use arena allocator for TSC-like batch workloads (enabled via -Dqjs_arena=true)
    const rt = if (allocator_config.qjs_arena) blk: {
        native_arena.init();
        const malloc_funcs = qjs.JSMallocFunctions{
            .js_calloc = native_arena.js_calloc,
            .js_malloc = native_arena.js_malloc,
            .js_free = native_arena.js_free,
            .js_realloc = native_arena.js_realloc,
            .js_malloc_usable_size = native_arena.js_malloc_usable_size,
        };
        break :blk qjs.JS_NewRuntime2(&malloc_funcs, null) orelse {
            std.debug.print("Failed to create QuickJS runtime with arena\n", .{});
            return error.RuntimeCreationFailed;
        };
    } else qjs.JS_NewRuntime() orelse {
        std.debug.print("Failed to create QuickJS runtime\n", .{});
        return error.RuntimeCreationFailed;
    };
    defer {
        qjs.JS_FreeRuntime(rt);
        if (allocator_config.qjs_arena) {
            native_arena.deinit();
        }
    }

    // Set memory limit (4GB)
    qjs.JS_SetMemoryLimit(rt, 4 * 1024 * 1024 * 1024);

    // Create context
    const ctx = qjs.JS_NewContext(rt) orelse {
        std.debug.print("Failed to create QuickJS context\n", .{});
        return error.ContextCreationFailed;
    };
    defer qjs.JS_FreeContext(ctx);

    // Initialize std module (for console, etc)
    qjs.js_std_init_handlers(rt);
    qjs.JS_SetModuleLoaderFunc(rt, null, qjs.js_module_loader, null);
    _ = qjs.js_init_module_std(ctx, "std");
    _ = qjs.js_init_module_os(ctx, "os");

    // Expose QuickJS std and os modules as global objects for polyfills
    // This is needed because the bundled code can't use ES module imports
    {
        const global = qjs.JS_GetGlobalObject(ctx);
        defer qjs.JS_FreeValue(ctx, global);

        // Execute code to import modules and expose them globally
        const init_code =
            \\import * as std from 'std';
            \\import * as os from 'os';
            \\globalThis.std = std;
            \\globalThis.os = os;
            \\globalThis._os = os;
        ;
        const init_result = qjs.JS_Eval(ctx, init_code, init_code.len, "<init>", qjs.JS_EVAL_TYPE_MODULE);
        if (qjs.JS_IsException(init_result)) {
            // Print but don't fail - some builds may not have these modules
            // Debug output disabled for production
            printException(ctx);
        }
        // Success case: no debug output needed
        qjs.JS_FreeValue(ctx, init_result);
    }

    // Register native polyfills
    registerPolyfills(ctx, allocator);

    // Set process.argv from command-line arguments
    {
        const args = std.process.argsAlloc(allocator) catch &[_][:0]const u8{};
        defer if (args.len > 0) allocator.free(args);
        // Skip first arg (executable path), and skip "--" separator if present
        // The "--" is a Unix convention to separate tool args from script args
        var script_args_start: usize = 1;
        if (args.len > 1 and std.mem.eql(u8, args[1], "--")) {
            script_args_start = 2; // Skip the "--"
        }
        if (args.len > script_args_start) {
            process_polyfill.setArgv(ctx, args[script_args_start..]);
        } else {
            process_polyfill.setArgv(ctx, &[_][:0]const u8{});
        }
    }

    // Set cwd to actual current working directory
    if (std.fs.cwd().realpathAlloc(allocator, ".")) |cwd_path| {
        native_bindings.setCwd(cwd_path);
    } else |_| {
        // Fallback to "/" if cwd() fails
    }

    // Register native bindings (fs, crypto, fast_transpile, etc.)
    native_bindings.registerAll(ctx);

    // Register frozen functions BEFORE executing bytecode
    // Cast needed because frozen_module uses zig_runtime.JSContext opaque type
    _ = frozen_module.frozen_init_c(@ptrCast(ctx));

    // Set __frozen_init_complete flag
    {
        const global = qjs.JS_GetGlobalObject(ctx);
        defer qjs.JS_FreeValue(ctx, global);
        _ = qjs.JS_SetPropertyStr(ctx, global, "__frozen_init_complete", jsTrue());
        // Frozen functions enabled
    }

    // Load bytecode
    const obj = qjs.JS_ReadObject(ctx, bytecode.ptr, bytecode.len, qjs.JS_READ_OBJ_BYTECODE);
    if (qjs.JS_IsException(obj)) {
        printException(ctx);
        return error.BytecodeLoadFailed;
    }

    // Execute
    const tag = qjs.JS_VALUE_GET_TAG(obj);
    var result: qjs.JSValue = undefined;

    if (tag == -3) { // JS_TAG_MODULE
        if (qjs.JS_ResolveModule(ctx, obj) < 0) {
            qjs.JS_FreeValue(ctx, obj);
            printException(ctx);
            return error.ModuleResolveFailed;
        }
        if (qjs.js_module_set_import_meta(ctx, obj, false, true) < 0) {
            printException(ctx);
            return error.ModuleMetaFailed;
        }
        result = qjs.JS_EvalFunction(ctx, obj);
        _ = qjs.js_std_loop(ctx);
    } else {
        result = qjs.JS_EvalFunction(ctx, obj);
        _ = qjs.js_std_loop(ctx);
    }

    if (qjs.JS_IsException(result)) {
        printException(ctx);
        qjs.JS_FreeValue(ctx, result);
        return error.ExecutionFailed;
    }

    qjs.JS_FreeValue(ctx, result);
}

fn registerPolyfills(ctx: *qjs.JSContext, allocator: std.mem.Allocator) void {
    _ = allocator; // Polyfills no longer need allocator

    // All polyfills now take only ctx and get global internally
    // Cast needed because polyfills use different cImport (distinct opaque types)
    // Register require FIRST so other polyfills can use it
    require_polyfill.register(@ptrCast(ctx));
    console_polyfill.register(@ptrCast(ctx));
    process_polyfill.register(@ptrCast(ctx));
    buffer_polyfill.register(@ptrCast(ctx));
    path_polyfill.register(@ptrCast(ctx));
    util_polyfill.register(@ptrCast(ctx));
    encoding_polyfill.register(@ptrCast(ctx));
    dns_polyfill.register(@ptrCast(ctx));
    child_process_polyfill.register(@ptrCast(ctx));
    net_polyfill.register(@ptrCast(ctx));
    tls_polyfill.register(@ptrCast(ctx));
    querystring_polyfill.register(@ptrCast(ctx));
    compression_polyfill.register(@ptrCast(ctx));
    globals_polyfill.register(@ptrCast(ctx));
    os_polyfill.register(@ptrCast(ctx));
    fs_polyfill.register(@ptrCast(ctx));
    crypto_polyfill.register(@ptrCast(ctx));
    tty_polyfill.register(@ptrCast(ctx));
    assert_polyfill.register(@ptrCast(ctx));
    string_decoder_polyfill.register(@ptrCast(ctx));
}

fn printException(ctx: *qjs.JSContext) void {
    const exc = qjs.JS_GetException(ctx);
    defer qjs.JS_FreeValue(ctx, exc);

    const str = qjs.JS_ToCString(ctx, exc);
    if (str != null) {
        std.debug.print("Exception: {s}\n", .{str});
        qjs.JS_FreeCString(ctx, str);
    }

    // Print stack trace if available
    const stack = qjs.JS_GetPropertyStr(ctx, exc, "stack");
    if (!qjs.JS_IsUndefined(stack)) {
        const stack_str = qjs.JS_ToCString(ctx, stack);
        if (stack_str != null) {
            std.debug.print("Stack: {s}\n", .{stack_str});
            qjs.JS_FreeCString(ctx, stack_str);
        }
    }
    qjs.JS_FreeValue(ctx, stack);
}
