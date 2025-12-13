/// EdgeBox Standard Library Host Functions
///
/// WASI-style architecture for high-performance stdlib operations.
/// These are TRUSTED host functions (like WASI) - not user code.
///
/// Architecture:
/// - Module-based imports (clean namespace): edgebox_std
/// - Handle-based resource management (integers, not raw pointers)
/// - Bounds-checked memory access at the boundary
///
/// Usage from WASM:
///   extern int map_new() __attribute__((import_module("edgebox_std"), import_name("map_new")));
///
const std = @import("std");

// Import WAMR C API (same as edgebox_wamr.zig)
const c = @cImport({
    @cInclude("wasm_export.h");
});

const NativeSymbol = c.NativeSymbol;
const allocator = std.heap.page_allocator;

// ============================================================================
// Handle Table (Resource Management)
// ============================================================================
// We don't pass raw pointers to WASM - we use integer handles.
// This maintains sandbox safety while providing fast native operations.

const MAX_HANDLES = 1024;

// Map handles (string -> i32)
var maps: [MAX_HANDLES]?std.StringHashMap(i32) = [_]?std.StringHashMap(i32){null} ** MAX_HANDLES;
var next_map_handle: u32 = 0;

// Array handles (dynamic arrays of i32)
// Using ArrayListUnmanaged since we manage allocation ourselves
var arrays: [MAX_HANDLES]?std.ArrayListUnmanaged(i32) = [_]?std.ArrayListUnmanaged(i32){null} ** MAX_HANDLES;
var next_array_handle: u32 = 0;

// ============================================================================
// WASM Memory Access Helpers
// ============================================================================

/// Read bytes from WASM linear memory with bounds checking
fn readWasmMemory(exec_env: c.wasm_exec_env_t, ptr: u32, len: u32) ?[]const u8 {
    if (len == 0) return "";
    const module_inst = c.wasm_runtime_get_module_inst(exec_env);
    if (!c.wasm_runtime_validate_app_addr(module_inst, ptr, len)) {
        return null; // Out of bounds
    }
    const native_addr: ?*u8 = @ptrCast(c.wasm_runtime_addr_app_to_native(module_inst, ptr));
    if (native_addr == null) return null;
    return @as([*]const u8, @ptrCast(native_addr.?))[0..len];
}

/// Write bytes to WASM linear memory with bounds checking
fn writeWasmMemory(exec_env: c.wasm_exec_env_t, ptr: u32, data: []const u8) bool {
    if (data.len == 0) return true;
    const module_inst = c.wasm_runtime_get_module_inst(exec_env);
    if (!c.wasm_runtime_validate_app_addr(module_inst, ptr, @intCast(data.len))) {
        return false; // Out of bounds
    }
    const native_addr: ?*u8 = @ptrCast(c.wasm_runtime_addr_app_to_native(module_inst, ptr));
    if (native_addr == null) return false;
    const dest = @as([*]u8, @ptrCast(native_addr.?))[0..data.len];
    @memcpy(dest, data);
    return true;
}

// ============================================================================
// Map Host Functions
// ============================================================================

/// Create a new empty map. Returns handle or -1 on error.
pub fn map_new(exec_env: c.wasm_exec_env_t) callconv(.c) i32 {
    _ = exec_env;
    if (next_map_handle >= MAX_HANDLES) return -1;

    maps[next_map_handle] = std.StringHashMap(i32).init(allocator);
    const handle = next_map_handle;
    next_map_handle += 1;
    return @intCast(handle);
}

/// Set a key-value pair in the map. Returns 0 on success, -1 on error.
/// Note: Keys are copied into the map since WASM memory is untrusted.
pub fn map_set(exec_env: c.wasm_exec_env_t, handle: i32, key_ptr: u32, key_len: u32, val: i32) callconv(.c) i32 {
    if (handle < 0 or handle >= MAX_HANDLES) return -1;
    const map_ptr = &(maps[@intCast(handle)] orelse return -1);

    // Read key from WASM memory (with bounds check)
    const key_slice = readWasmMemory(exec_env, key_ptr, key_len) orelse return -1;

    // Copy key since WASM memory could be modified/freed
    const key_copy = allocator.dupe(u8, key_slice) catch return -1;

    // Check if we're replacing an existing key (need to free old key copy)
    if (map_ptr.fetchRemove(key_copy)) |kv| {
        allocator.free(kv.key);
    }

    map_ptr.put(key_copy, val) catch {
        allocator.free(key_copy);
        return -1;
    };
    return 0;
}

/// Get value for a key. Returns value or -1 if not found.
pub fn map_get(exec_env: c.wasm_exec_env_t, handle: i32, key_ptr: u32, key_len: u32) callconv(.c) i32 {
    if (handle < 0 or handle >= MAX_HANDLES) return -1;
    const map = maps[@intCast(handle)] orelse return -1;

    const key_slice = readWasmMemory(exec_env, key_ptr, key_len) orelse return -1;
    return map.get(key_slice) orelse -1;
}

/// Check if key exists in map. Returns 1 if found, 0 if not found, -1 on error.
pub fn map_has(exec_env: c.wasm_exec_env_t, handle: i32, key_ptr: u32, key_len: u32) callconv(.c) i32 {
    if (handle < 0 or handle >= MAX_HANDLES) return -1;
    const map = maps[@intCast(handle)] orelse return -1;

    const key_slice = readWasmMemory(exec_env, key_ptr, key_len) orelse return -1;
    return if (map.contains(key_slice)) 1 else 0;
}

/// Delete a key from the map. Returns 1 if key was present, 0 if not, -1 on error.
pub fn map_delete(exec_env: c.wasm_exec_env_t, handle: i32, key_ptr: u32, key_len: u32) callconv(.c) i32 {
    if (handle < 0 or handle >= MAX_HANDLES) return -1;
    const map_ptr = &(maps[@intCast(handle)] orelse return -1);

    const key_slice = readWasmMemory(exec_env, key_ptr, key_len) orelse return -1;
    if (map_ptr.fetchRemove(key_slice)) |kv| {
        allocator.free(kv.key);
        return 1;
    }
    return 0;
}

/// Get number of entries in map. Returns count or -1 on error.
pub fn map_len(exec_env: c.wasm_exec_env_t, handle: i32) callconv(.c) i32 {
    _ = exec_env;
    if (handle < 0 or handle >= MAX_HANDLES) return -1;
    const map = maps[@intCast(handle)] orelse return -1;
    return @intCast(map.count());
}

/// Clear all entries from map (but keep the handle valid). Returns 0 on success.
pub fn map_clear(exec_env: c.wasm_exec_env_t, handle: i32) callconv(.c) i32 {
    _ = exec_env;
    if (handle < 0 or handle >= MAX_HANDLES) return -1;
    const map_ptr = &(maps[@intCast(handle)] orelse return -1);

    // Free all key copies
    var iter = map_ptr.iterator();
    while (iter.next()) |entry| {
        allocator.free(entry.key_ptr.*);
    }
    map_ptr.clearRetainingCapacity();
    return 0;
}

/// Free a map and release its handle. Returns 0 on success.
pub fn map_free(exec_env: c.wasm_exec_env_t, handle: i32) callconv(.c) i32 {
    _ = exec_env;
    if (handle < 0 or handle >= MAX_HANDLES) return -1;
    if (maps[@intCast(handle)]) |*map| {
        // Free all key copies
        var iter = map.iterator();
        while (iter.next()) |entry| {
            allocator.free(entry.key_ptr.*);
        }
        map.deinit();
        maps[@intCast(handle)] = null;
        return 0;
    }
    return -1;
}

// ============================================================================
// Array Host Functions
// ============================================================================

/// Create a new empty array. Returns handle or -1 on error.
pub fn array_new(exec_env: c.wasm_exec_env_t) callconv(.c) i32 {
    _ = exec_env;
    if (next_array_handle >= MAX_HANDLES) return -1;

    arrays[next_array_handle] = .{}; // ArrayListUnmanaged empty init
    const handle = next_array_handle;
    next_array_handle += 1;
    return @intCast(handle);
}

/// Push a value to the end of array. Returns 0 on success, -1 on error.
pub fn array_push(exec_env: c.wasm_exec_env_t, handle: i32, val: i32) callconv(.c) i32 {
    _ = exec_env;
    if (handle < 0 or handle >= MAX_HANDLES) return -1;
    if (arrays[@intCast(handle)]) |*arr| {
        arr.append(allocator, val) catch return -1;
        return 0;
    }
    return -1;
}

/// Pop a value from the end of array. Returns the value or -1 if empty.
pub fn array_pop(exec_env: c.wasm_exec_env_t, handle: i32) callconv(.c) i32 {
    _ = exec_env;
    if (handle < 0 or handle >= MAX_HANDLES) return -1;
    if (arrays[@intCast(handle)]) |*arr| {
        return arr.pop() orelse -1;
    }
    return -1;
}

/// Get value at index. Returns value or -1 if out of bounds.
pub fn array_get(exec_env: c.wasm_exec_env_t, handle: i32, idx: i32) callconv(.c) i32 {
    _ = exec_env;
    if (handle < 0 or handle >= MAX_HANDLES) return -1;
    if (idx < 0) return -1;
    if (arrays[@intCast(handle)]) |arr| {
        if (@as(usize, @intCast(idx)) >= arr.items.len) return -1;
        return arr.items[@intCast(idx)];
    }
    return -1;
}

/// Set value at index. Returns 0 on success, -1 if out of bounds.
pub fn array_set(exec_env: c.wasm_exec_env_t, handle: i32, idx: i32, val: i32) callconv(.c) i32 {
    _ = exec_env;
    if (handle < 0 or handle >= MAX_HANDLES) return -1;
    if (idx < 0) return -1;
    if (arrays[@intCast(handle)]) |*arr| {
        if (@as(usize, @intCast(idx)) >= arr.items.len) return -1;
        arr.items[@intCast(idx)] = val;
        return 0;
    }
    return -1;
}

/// Get array length. Returns length or -1 on error.
pub fn array_len(exec_env: c.wasm_exec_env_t, handle: i32) callconv(.c) i32 {
    _ = exec_env;
    if (handle < 0 or handle >= MAX_HANDLES) return -1;
    if (arrays[@intCast(handle)]) |arr| {
        return @intCast(arr.items.len);
    }
    return -1;
}

/// Sort array in ascending order. Returns 0 on success.
pub fn array_sort(exec_env: c.wasm_exec_env_t, handle: i32) callconv(.c) i32 {
    _ = exec_env;
    if (handle < 0 or handle >= MAX_HANDLES) return -1;
    if (arrays[@intCast(handle)]) |*arr| {
        std.mem.sort(i32, arr.items, {}, std.sort.asc(i32));
        return 0;
    }
    return -1;
}

/// Sort array in descending order. Returns 0 on success.
pub fn array_sort_desc(exec_env: c.wasm_exec_env_t, handle: i32) callconv(.c) i32 {
    _ = exec_env;
    if (handle < 0 or handle >= MAX_HANDLES) return -1;
    if (arrays[@intCast(handle)]) |*arr| {
        std.mem.sort(i32, arr.items, {}, std.sort.desc(i32));
        return 0;
    }
    return -1;
}

/// Reverse array in place. Returns 0 on success.
pub fn array_reverse(exec_env: c.wasm_exec_env_t, handle: i32) callconv(.c) i32 {
    _ = exec_env;
    if (handle < 0 or handle >= MAX_HANDLES) return -1;
    if (arrays[@intCast(handle)]) |*arr| {
        std.mem.reverse(i32, arr.items);
        return 0;
    }
    return -1;
}

/// Clear all elements from array (but keep the handle valid). Returns 0 on success.
pub fn array_clear(exec_env: c.wasm_exec_env_t, handle: i32) callconv(.c) i32 {
    _ = exec_env;
    if (handle < 0 or handle >= MAX_HANDLES) return -1;
    if (arrays[@intCast(handle)]) |*arr| {
        arr.clearRetainingCapacity();
        return 0;
    }
    return -1;
}

/// Find first index of value. Returns index or -1 if not found.
pub fn array_index_of(exec_env: c.wasm_exec_env_t, handle: i32, val: i32) callconv(.c) i32 {
    _ = exec_env;
    if (handle < 0 or handle >= MAX_HANDLES) return -1;
    if (arrays[@intCast(handle)]) |arr| {
        for (arr.items, 0..) |item, i| {
            if (item == val) return @intCast(i);
        }
    }
    return -1;
}

/// Free array and release its handle. Returns 0 on success.
pub fn array_free(exec_env: c.wasm_exec_env_t, handle: i32) callconv(.c) i32 {
    _ = exec_env;
    if (handle < 0 or handle >= MAX_HANDLES) return -1;
    if (arrays[@intCast(handle)]) |*arr| {
        arr.deinit(allocator);
        arrays[@intCast(handle)] = null;
        return 0;
    }
    return -1;
}

// ============================================================================
// Dispatch Function for QuickJS Native Bindings
// ============================================================================
// This provides a single entry point for WASM to call stdlib operations,
// following the pattern used by http_dispatch, file_dispatch, etc.

// Array operation opcodes (0-9)
pub const STDLIB_OP_ARRAY_NEW: u32 = 0;
pub const STDLIB_OP_ARRAY_PUSH: u32 = 1;
pub const STDLIB_OP_ARRAY_POP: u32 = 2;
pub const STDLIB_OP_ARRAY_GET: u32 = 3;
pub const STDLIB_OP_ARRAY_SET: u32 = 4;
pub const STDLIB_OP_ARRAY_LEN: u32 = 5;
pub const STDLIB_OP_ARRAY_SORT: u32 = 6;
pub const STDLIB_OP_ARRAY_FREE: u32 = 7;
pub const STDLIB_OP_ARRAY_SORT_DESC: u32 = 8;
pub const STDLIB_OP_ARRAY_REVERSE: u32 = 9;

// Map operation opcodes (10-17)
// Note: Array opcodes 18-19 are after map opcodes
pub const STDLIB_OP_MAP_NEW: u32 = 10;
pub const STDLIB_OP_MAP_SET: u32 = 11; // (handle, key_ptr, key_len, value)
pub const STDLIB_OP_MAP_GET: u32 = 12; // (handle, key_ptr, key_len, _)
pub const STDLIB_OP_MAP_HAS: u32 = 13;
pub const STDLIB_OP_MAP_DELETE: u32 = 14;
pub const STDLIB_OP_MAP_LEN: u32 = 15;
pub const STDLIB_OP_MAP_FREE: u32 = 16;
pub const STDLIB_OP_MAP_CLEAR: u32 = 17;

// Additional Array opcodes (18-19)
pub const STDLIB_OP_ARRAY_CLEAR: u32 = 18;
pub const STDLIB_OP_ARRAY_INDEX_OF: u32 = 19;

/// Dispatch function for stdlib operations
/// Signature: (opcode: u32, a1: u32, a2: u32, a3: u32, a4: u32) -> i32
/// For Map string key operations: a2=key_ptr, a3=key_len
pub fn stdlib_dispatch(exec_env: c.wasm_exec_env_t, opcode: u32, a1: u32, a2: u32, a3: u32, a4: u32) callconv(.c) i32 {
    return switch (opcode) {
        // Array operations
        STDLIB_OP_ARRAY_NEW => array_new(exec_env),
        STDLIB_OP_ARRAY_PUSH => array_push(exec_env, @intCast(a1), @intCast(a2)),
        STDLIB_OP_ARRAY_POP => array_pop(exec_env, @intCast(a1)),
        STDLIB_OP_ARRAY_GET => array_get(exec_env, @intCast(a1), @intCast(a2)),
        STDLIB_OP_ARRAY_SET => array_set(exec_env, @intCast(a1), @intCast(a2), @intCast(a3)),
        STDLIB_OP_ARRAY_LEN => array_len(exec_env, @intCast(a1)),
        STDLIB_OP_ARRAY_SORT => array_sort(exec_env, @intCast(a1)),
        STDLIB_OP_ARRAY_FREE => array_free(exec_env, @intCast(a1)),
        STDLIB_OP_ARRAY_SORT_DESC => array_sort_desc(exec_env, @intCast(a1)),
        STDLIB_OP_ARRAY_REVERSE => array_reverse(exec_env, @intCast(a1)),
        STDLIB_OP_ARRAY_CLEAR => array_clear(exec_env, @intCast(a1)),
        STDLIB_OP_ARRAY_INDEX_OF => array_index_of(exec_env, @intCast(a1), @intCast(a2)),
        // Map operations (a2=key_ptr, a3=key_len for string key ops)
        STDLIB_OP_MAP_NEW => map_new(exec_env),
        STDLIB_OP_MAP_SET => map_set(exec_env, @intCast(a1), a2, a3, @intCast(a4)),
        STDLIB_OP_MAP_GET => map_get(exec_env, @intCast(a1), a2, a3),
        STDLIB_OP_MAP_HAS => map_has(exec_env, @intCast(a1), a2, a3),
        STDLIB_OP_MAP_DELETE => map_delete(exec_env, @intCast(a1), a2, a3),
        STDLIB_OP_MAP_LEN => map_len(exec_env, @intCast(a1)),
        STDLIB_OP_MAP_FREE => map_free(exec_env, @intCast(a1)),
        STDLIB_OP_MAP_CLEAR => map_clear(exec_env, @intCast(a1)),
        else => -1,
    };
}

// ============================================================================
// Symbol Registration for WAMR
// ============================================================================

/// NativeSymbol array for edgebox_std module registration
/// IMPORTANT: These must be global/static because WAMR retains references to them
pub var g_std_symbols = [_]NativeSymbol{
    // Map operations (string -> i32)
    .{ .symbol = "map_new", .func_ptr = @ptrCast(@constCast(&map_new)), .signature = "()i", .attachment = null },
    .{ .symbol = "map_set", .func_ptr = @ptrCast(@constCast(&map_set)), .signature = "(iiii)i", .attachment = null },
    .{ .symbol = "map_get", .func_ptr = @ptrCast(@constCast(&map_get)), .signature = "(iii)i", .attachment = null },
    .{ .symbol = "map_has", .func_ptr = @ptrCast(@constCast(&map_has)), .signature = "(iii)i", .attachment = null },
    .{ .symbol = "map_delete", .func_ptr = @ptrCast(@constCast(&map_delete)), .signature = "(iii)i", .attachment = null },
    .{ .symbol = "map_len", .func_ptr = @ptrCast(@constCast(&map_len)), .signature = "(i)i", .attachment = null },
    .{ .symbol = "map_clear", .func_ptr = @ptrCast(@constCast(&map_clear)), .signature = "(i)i", .attachment = null },
    .{ .symbol = "map_free", .func_ptr = @ptrCast(@constCast(&map_free)), .signature = "(i)i", .attachment = null },

    // Array operations (dynamic i32 array)
    .{ .symbol = "array_new", .func_ptr = @ptrCast(@constCast(&array_new)), .signature = "()i", .attachment = null },
    .{ .symbol = "array_push", .func_ptr = @ptrCast(@constCast(&array_push)), .signature = "(ii)i", .attachment = null },
    .{ .symbol = "array_pop", .func_ptr = @ptrCast(@constCast(&array_pop)), .signature = "(i)i", .attachment = null },
    .{ .symbol = "array_get", .func_ptr = @ptrCast(@constCast(&array_get)), .signature = "(ii)i", .attachment = null },
    .{ .symbol = "array_set", .func_ptr = @ptrCast(@constCast(&array_set)), .signature = "(iii)i", .attachment = null },
    .{ .symbol = "array_len", .func_ptr = @ptrCast(@constCast(&array_len)), .signature = "(i)i", .attachment = null },
    .{ .symbol = "array_sort", .func_ptr = @ptrCast(@constCast(&array_sort)), .signature = "(i)i", .attachment = null },
    .{ .symbol = "array_sort_desc", .func_ptr = @ptrCast(@constCast(&array_sort_desc)), .signature = "(i)i", .attachment = null },
    .{ .symbol = "array_reverse", .func_ptr = @ptrCast(@constCast(&array_reverse)), .signature = "(i)i", .attachment = null },
    .{ .symbol = "array_clear", .func_ptr = @ptrCast(@constCast(&array_clear)), .signature = "(i)i", .attachment = null },
    .{ .symbol = "array_index_of", .func_ptr = @ptrCast(@constCast(&array_index_of)), .signature = "(ii)i", .attachment = null },
    .{ .symbol = "array_free", .func_ptr = @ptrCast(@constCast(&array_free)), .signature = "(i)i", .attachment = null },
};

/// Dispatch symbol for QuickJS native bindings (via wasm_main.zig)
/// Registered separately so wasm_main can import "edgebox_stdlib" "stdlib_dispatch"
var g_std_dispatch_symbols = [_]NativeSymbol{
    .{ .symbol = "stdlib_dispatch", .func_ptr = @ptrCast(@constCast(&stdlib_dispatch)), .signature = "(iiiii)i", .attachment = null },
};

/// Register edgebox_std module with WAMR runtime
/// Call this from registerHostFunctions() in edgebox_wamr.zig
pub fn registerStdlib() void {
    // Individual function symbols (for direct WASM imports)
    _ = c.wasm_runtime_register_natives("edgebox_std", &g_std_symbols, g_std_symbols.len);
    // Dispatch function (for QuickJS native bindings via wasm_main.zig)
    _ = c.wasm_runtime_register_natives("edgebox_stdlib", &g_std_dispatch_symbols, g_std_dispatch_symbols.len);
}

// ============================================================================
// Cleanup function for runtime shutdown
// ============================================================================

/// Free all allocated resources (call before runtime shutdown)
pub fn cleanup() void {
    // Free all maps
    for (&maps) |*maybe_map| {
        if (maybe_map.*) |*map| {
            var iter = map.iterator();
            while (iter.next()) |entry| {
                allocator.free(entry.key_ptr.*);
            }
            map.deinit();
            maybe_map.* = null;
        }
    }
    next_map_handle = 0;

    // Free all arrays
    for (&arrays) |*maybe_arr| {
        if (maybe_arr.*) |*arr| {
            arr.deinit(allocator);
            maybe_arr.* = null;
        }
    }
    next_array_handle = 0;
}
