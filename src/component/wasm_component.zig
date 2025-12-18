/// WASM Component Loader - Load user WASM modules as components
/// Provides zero-copy ABI integration with deduplication
const std = @import("std");
const c = @cImport({
    @cInclude("wasm_export.h");
});

const Allocator = std.mem.Allocator;

/// A loaded WASM component (user module)
pub const WasmComponent = struct {
    path: []const u8, // Deduplicated by this path
    module: c.wasm_module_t,
    instance: c.wasm_module_inst_t,
    exports: std.StringHashMapUnmanaged(WasmExport),

    pub const WasmExport = struct {
        name: []const u8,
        func: c.wasm_function_inst_t,
        param_count: u32,
        result_count: u32,
    };

    pub fn deinit(self: *WasmComponent, allocator: Allocator) void {
        // Free exports
        var iter = self.exports.iterator();
        while (iter.next()) |entry| {
            allocator.free(entry.key_ptr.*);
            allocator.free(entry.value_ptr.name);
        }
        self.exports.deinit(allocator);

        // Free path
        allocator.free(self.path);

        // Destroy WAMR instance and module
        if (self.instance) |inst| {
            c.wasm_runtime_deinstantiate(inst);
        }
        if (self.module) |mod| {
            c.wasm_runtime_unload(mod);
        }
    }
};

/// Global registry of loaded WASM components (deduplicated by path)
pub const ComponentRegistry = struct {
    allocator: Allocator,
    components: std.StringHashMapUnmanaged(*WasmComponent),

    pub fn init(allocator: Allocator) ComponentRegistry {
        return .{
            .allocator = allocator,
            .components = .{},
        };
    }

    pub fn deinit(self: *ComponentRegistry) void {
        var iter = self.components.iterator();
        while (iter.next()) |entry| {
            entry.value_ptr.*.deinit(self.allocator);
            self.allocator.destroy(entry.value_ptr.*);
        }
        self.components.deinit(self.allocator);
    }

    /// Load a WASM component from file path (deduplicated)
    /// Returns existing component if already loaded
    pub fn loadComponent(self: *ComponentRegistry, path: []const u8) !*WasmComponent {
        // Check if already loaded (deduplication)
        if (self.components.get(path)) |component| {
            return component;
        }

        // Read WASM file
        const file = try std.fs.cwd().openFile(path, .{});
        defer file.close();

        const file_size = try file.getEndPos();
        const buffer = try self.allocator.alloc(u8, file_size);
        defer self.allocator.free(buffer);

        const bytes_read = try file.readAll(buffer);
        if (bytes_read != file_size) return error.IncompleteRead;

        // Load WASM module
        var error_buf: [128]u8 = undefined;
        const module = c.wasm_runtime_load(
            buffer.ptr,
            @intCast(buffer.len),
            &error_buf,
            @intCast(error_buf.len),
        ) orelse {
            std.debug.print("[WASM Component] Failed to load {s}: {s}\n", .{ path, error_buf });
            return error.WasmLoadFailed;
        };
        errdefer c.wasm_runtime_unload(module);

        // Instantiate module (64KB stack, 64KB heap)
        const instance = c.wasm_runtime_instantiate(
            module,
            64 * 1024, // stack_size
            64 * 1024, // heap_size
            &error_buf,
            @intCast(error_buf.len),
        ) orelse {
            std.debug.print("[WASM Component] Failed to instantiate {s}: {s}\n", .{ path, error_buf });
            return error.WasmInstantiateFailed;
        };
        errdefer c.wasm_runtime_deinstantiate(instance);

        // Create component
        const component = try self.allocator.create(WasmComponent);
        errdefer self.allocator.destroy(component);

        component.* = .{
            .path = try self.allocator.dupe(u8, path),
            .module = module,
            .instance = instance,
            .exports = .{},
        };
        errdefer component.deinit(self.allocator);

        // Enumerate exports
        try self.enumerateExports(component);

        // Store in registry (deduplicated by path)
        try self.components.put(self.allocator, component.path, component);

        std.debug.print("[WASM Component] Loaded {s} with {d} exports\n", .{ path, component.exports.count() });

        return component;
    }

    /// Enumerate all exports from a WASM component
    fn enumerateExports(self: *ComponentRegistry, component: *WasmComponent) !void {
        // Get export count
        const export_count = c.wasm_runtime_get_export_count(component.instance);
        if (export_count == 0) return;

        // Iterate through exports
        var i: u32 = 0;
        while (i < export_count) : (i += 1) {
            var name_buf: [256]u8 = undefined;
            const name_len = c.wasm_runtime_get_export_name(
                component.instance,
                i,
                &name_buf,
                @intCast(name_buf.len),
            );
            if (name_len == 0) continue;

            const name = name_buf[0..@as(usize, @intCast(name_len))];

            // Check if it's a function export
            const func = c.wasm_runtime_lookup_function(
                component.instance,
                name.ptr,
            ) orelse continue;

            // Get function signature (param/result counts)
            // For now, assume all exports are functions
            // TODO: Get actual signature from WAMR
            const wasm_export = WasmComponent.WasmExport{
                .name = try self.allocator.dupe(u8, name),
                .func = func,
                .param_count = 2, // TODO: Get from signature
                .result_count = 1, // TODO: Get from signature
            };

            const key = try self.allocator.dupe(u8, name);
            try component.exports.put(self.allocator, key, wasm_export);
        }
    }

    /// Call a WASM component export function
    pub fn callExport(
        self: *ComponentRegistry,
        component: *WasmComponent,
        func_name: []const u8,
        args: []const i32,
    ) !i32 {
        _ = self;

        const wasm_export = component.exports.get(func_name) orelse return error.ExportNotFound;

        // Prepare WASM arguments
        var wasm_args: [16]c.wasm_val_t = undefined;
        const arg_count = @min(args.len, wasm_export.param_count);

        for (0..arg_count) |i| {
            wasm_args[i].kind = c.WASM_I32;
            wasm_args[i].of.i32 = args[i];
        }

        // Prepare result buffer
        var wasm_results: [1]c.wasm_val_t = undefined;
        wasm_results[0].kind = c.WASM_I32;

        // Call function
        const success = c.wasm_runtime_call_wasm_a(
            c.wasm_runtime_get_exec_env_singleton(component.instance),
            wasm_export.func,
            @intCast(wasm_export.result_count),
            &wasm_results,
            @intCast(arg_count),
            &wasm_args,
        );

        if (!success) {
            return error.WasmCallFailed;
        }

        return wasm_results[0].of.i32;
    }
};

// Global registry instance
var g_registry: ?*ComponentRegistry = null;

pub fn initGlobalRegistry(allocator: Allocator) !void {
    if (g_registry != null) return;

    const registry = try allocator.create(ComponentRegistry);
    registry.* = ComponentRegistry.init(allocator);
    g_registry = registry;
}

pub fn deinitGlobalRegistry(allocator: Allocator) void {
    if (g_registry) |registry| {
        registry.deinit();
        allocator.destroy(registry);
        g_registry = null;
    }
}

pub fn getGlobalRegistry() ?*ComponentRegistry {
    return g_registry;
}
