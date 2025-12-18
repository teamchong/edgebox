//! WASM Import Loader - Multi-instance WASM module loading
//!
//! Enables JS code to import WASM modules with zero-overhead calls:
//!   import * as math from "./math.wasm";
//!   math.add(5, 3); // Direct WASM-to-WASM call
//!
//! Architecture:
//! ┌─────────────────────────────────────┐
//! │  WAMR Runtime (Host)                │
//! ├─────────────────────────────────────┤
//! │  WASM Instance #1: QuickJS + User JS│
//! │  - Main sandbox                      │
//! ├─────────────────────────────────────┤
//! │  WASM Instance #2+: User WASM modules│
//! │  - Loaded on-demand via import       │
//! │  - Direct function linkage           │
//! └─────────────────────────────────────┘

const std = @import("std");

// Import WAMR C API
pub const c = @cImport({
    @cInclude("wasm_export.h");
});

/// Loaded WASM module registry
pub const WasmModuleRegistry = struct {
    allocator: std.mem.Allocator,
    modules: std.StringHashMapUnmanaged(*LoadedModule),

    const LoadedModule = struct {
        path: []const u8,
        module: c.wasm_module_t,
        instance: c.wasm_module_inst_t,
        exports: std.StringHashMapUnmanaged(WasmExport),

        const WasmExport = struct {
            name: []const u8,
            func_ptr: c.wasm_function_inst_t,
            signature: FuncSignature,
        };

        const FuncSignature = struct {
            param_count: u32,
            param_types: [16]c.wasm_valkind_t, // Max 16 params
            result_count: u32,
            result_types: [16]c.wasm_valkind_t, // Max 16 results
        };
    };

    pub fn init(allocator: std.mem.Allocator) WasmModuleRegistry {
        return .{
            .allocator = allocator,
            .modules = .{},
        };
    }

    pub fn deinit(self: *WasmModuleRegistry) void {
        var iter = self.modules.iterator();
        while (iter.next()) |entry| {
            const module = entry.value_ptr.*;

            // Free exports
            module.exports.deinit(self.allocator);

            // Deinstantiate module
            c.wasm_runtime_deinstantiate(module.instance);
            c.wasm_runtime_unload(module.module);

            // Free path
            self.allocator.free(module.path);
            self.allocator.destroy(module);
        }
        self.modules.deinit(self.allocator);
    }

    /// Load a WASM module from file path
    pub fn loadModule(self: *WasmModuleRegistry, path: []const u8) !*LoadedModule {
        // Check if already loaded
        if (self.modules.get(path)) |existing| {
            return existing;
        }

        // Read WASM file
        const wasm_bytes = try std.fs.cwd().readFileAlloc(
            self.allocator,
            path,
            10 * 1024 * 1024, // Max 10MB WASM file
        );
        defer self.allocator.free(wasm_bytes);

        // Load WASM module
        var error_buf: [256]u8 = undefined;
        const module = c.wasm_runtime_load(
            @constCast(wasm_bytes.ptr),
            @intCast(wasm_bytes.len),
            &error_buf,
            error_buf.len,
        );
        if (module == null) {
            std.debug.print("[wasm_import] Failed to load {s}: {s}\n", .{ path, &error_buf });
            return error.WasmLoadFailed;
        }
        errdefer c.wasm_runtime_unload(module);

        // Instantiate module with reasonable defaults
        const inst_args = c.InstantiationArgs{
            .default_stack_size = 128 * 1024, // 128KB stack
            .host_managed_heap_size = 32 * 1024 * 1024, // 32MB heap
            .max_memory_pages = 1024, // 64MB max memory
        };
        const instance = c.wasm_runtime_instantiate_ex(
            module,
            &inst_args,
            &error_buf,
            error_buf.len,
        );
        if (instance == null) {
            std.debug.print("[wasm_import] Failed to instantiate {s}: {s}\n", .{ path, &error_buf });
            return error.WasmInstantiateFailed;
        }
        errdefer c.wasm_runtime_deinstantiate(instance);

        // Create LoadedModule
        const loaded = try self.allocator.create(LoadedModule);
        errdefer self.allocator.destroy(loaded);

        loaded.* = .{
            .path = try self.allocator.dupe(u8, path),
            .module = module,
            .instance = instance,
            .exports = .{},
        };

        // Extract exports
        try self.extractExports(loaded);

        // Register in map
        try self.modules.put(self.allocator, loaded.path, loaded);

        std.debug.print("[wasm_import] Loaded {s} with {d} exports\n", .{ path, loaded.exports.count() });
        return loaded;
    }

    /// Extract function exports from a loaded module
    fn extractExports(self: *WasmModuleRegistry, loaded_module: *LoadedModule) !void {
        // Get export count from module (not instance)
        const export_count = c.wasm_runtime_get_export_count(loaded_module.module);
        if (export_count == 0) return;

        var i: u32 = 0;
        while (i < export_count) : (i += 1) {
            // Get export information by index
            var export_info: c.wasm_export_t = undefined;
            c.wasm_runtime_get_export_type(
                loaded_module.module,
                @intCast(i),
                &export_info,
            );

            // Only process function exports
            if (export_info.kind != c.WASM_IMPORT_EXPORT_KIND_FUNC) continue;

            const export_name = std.mem.sliceTo(@as([*:0]const u8, @ptrCast(export_info.name)), 0);

            // Lookup function in instance
            const func = c.wasm_runtime_lookup_function(loaded_module.instance, export_info.name);
            if (func == null) continue;

            // Get function signature
            var sig: LoadedModule.FuncSignature = undefined;
            try self.getFunctionSignature(loaded_module.instance, func, &sig);

            // Store export
            const name_copy = try self.allocator.dupe(u8, export_name);
            try loaded_module.exports.put(self.allocator, name_copy, .{
                .name = name_copy,
                .func_ptr = func,
                .signature = sig,
            });
        }
    }

    /// Get function signature (param/result types)
    fn getFunctionSignature(
        self: *WasmModuleRegistry,
        instance: c.wasm_module_inst_t,
        func: c.wasm_function_inst_t,
        sig: *LoadedModule.FuncSignature,
    ) !void {
        _ = self;
        _ = instance;
        _ = func;

        // TODO: Use wasm_runtime_get_func_type to get signature
        // For now, assume simple signatures: (i32, i32) -> i32
        sig.param_count = 2;
        sig.param_types[0] = c.WASM_I32;
        sig.param_types[1] = c.WASM_I32;
        sig.result_count = 1;
        sig.result_types[0] = c.WASM_I32;
    }

    /// Call a WASM function by name
    pub fn callFunction(
        self: *WasmModuleRegistry,
        module: *LoadedModule,
        func_name: []const u8,
        args: []const c.wasm_val_t,
        results: []c.wasm_val_t,
    ) !void {
        _ = self;
        const export_func = module.exports.get(func_name) orelse return error.FunctionNotFound;

        // Create execution environment for this module
        const exec_env = c.wasm_runtime_create_exec_env(module.instance, 128 * 1024);
        if (exec_env == null) return error.ExecEnvCreateFailed;
        defer c.wasm_runtime_destroy_exec_env(exec_env);

        // Call function
        const success = c.wasm_runtime_call_wasm_a(
            exec_env,
            export_func.func_ptr,
            @intCast(results.len),
            results.ptr,
            @intCast(args.len),
            @constCast(args.ptr),
        );

        if (!success) {
            const exception = c.wasm_runtime_get_exception(module.instance);
            if (exception) |exc| {
                std.debug.print("[wasm_import] Call failed: {s}\n", .{exc});
            }
            return error.WasmCallFailed;
        }
    }
};

/// Global registry (initialized by main WASM runtime)
var g_registry: ?*WasmModuleRegistry = null;

pub fn initGlobalRegistry(allocator: std.mem.Allocator) !void {
    if (g_registry != null) return; // Already initialized

    const registry = try allocator.create(WasmModuleRegistry);
    registry.* = WasmModuleRegistry.init(allocator);
    g_registry = registry;
}

pub fn deinitGlobalRegistry(allocator: std.mem.Allocator) void {
    if (g_registry) |registry| {
        registry.deinit();
        allocator.destroy(registry);
        g_registry = null;
    }
}

pub fn getGlobalRegistry() ?*WasmModuleRegistry {
    return g_registry;
}
