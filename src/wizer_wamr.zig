/// Wizer: WebAssembly Pre-initializer (WAMR Implementation)
///
/// This module reimplements Wizer functionality using WAMR (WebAssembly Micro Runtime).
/// It pre-initializes WASM modules by:
/// 1. Running the initialization function (wizer_init)
/// 2. Snapshotting memory and globals
/// 3. Rewriting the WASM binary with pre-initialized state
///
/// This enables instant startup by embedding initialized state in the WASM binary.
///
/// Based on: https://github.com/bytecodealliance/wasmtime/tree/main/crates/wizer
const std = @import("std");

// WAMR C API bindings
const c = @cImport({
    @cInclude("wasm_export.h");
});

// C stdio for fflush (ensure output is visible even if process crashes)
const stdio = @cImport({
    @cInclude("stdio.h");
});

/// Flush stderr to ensure debug output is visible even on crash
fn flushStderr() void {
    // stdio.stderr is a function that returns the FILE* on some platforms
    const stderr_file = stdio.stderr;
    if (@typeInfo(@TypeOf(stderr_file)) == .pointer) {
        _ = stdio.fflush(stderr_file);
    } else {
        _ = stdio.fflush(stderr_file());
    }
}

const Allocator = std.mem.Allocator;

/// Maximum number of data segments to emit (engines have limits)
/// WASM spec allows unlimited, but engines may have practical limits
/// 500 segments provides good balance between file size and compatibility
const MAX_DATA_SEGMENTS: usize = 500;

/// Minimum overhead of defining a new active data segment:
/// - 1 byte: memory index LEB
/// - 2 bytes: offset expression (i32.const + LEB)
/// - 1 byte: data length LEB
const MIN_ACTIVE_SEGMENT_OVERHEAD: usize = 4;

/// WASM Section IDs (from WebAssembly spec)
pub const SectionId = enum(u8) {
    custom = 0,
    type = 1,
    import = 2,
    function = 3,
    table = 4,
    memory = 5,
    global = 6,
    @"export" = 7,
    start = 8,
    element = 9,
    code = 10,
    data = 11,
    data_count = 12,
};

/// A data segment for memory initialization
pub const DataSegment = struct {
    memory_index: u32,
    offset: u64,
    data: []const u8,
    is64: bool,

    pub fn deinit(self: *DataSegment, allocator: Allocator) void {
        allocator.free(self.data);
    }
};

/// Global value snapshot
pub const GlobalSnapshot = struct {
    index: u32,
    value: SnapshotVal,
    name: []const u8, // Global name (e.g., "__stack_pointer")
};

/// Snapshot value types
pub const SnapshotVal = union(enum) {
    i32: i32,
    i64: i64,
    f32: u32,
    f64: u64,
    v128: u128,
};

/// Memory snapshot result
pub const MemorySnapshot = struct {
    min_pages: u64,
    segments: std.ArrayListUnmanaged(DataSegment),

    pub fn deinit(self: *MemorySnapshot, allocator: Allocator) void {
        for (self.segments.items) |*seg| {
            seg.deinit(allocator);
        }
        self.segments.deinit(allocator);
    }
};

/// Complete snapshot of WASM state
pub const Snapshot = struct {
    globals: std.ArrayListUnmanaged(GlobalSnapshot),
    memories: std.ArrayListUnmanaged(MemorySnapshot),
    allocator: Allocator,

    pub fn init(allocator: Allocator) Snapshot {
        return .{
            .globals = .{},
            .memories = .{},
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *Snapshot) void {
        self.globals.deinit(self.allocator);
        for (self.memories.items) |*mem| {
            mem.deinit(self.allocator);
        }
        self.memories.deinit(self.allocator);
    }
};

/// Wizer configuration
pub const WizerConfig = struct {
    init_func: []const u8 = "wizer_init",
    keep_init_func: bool = false,
    allow_wasi: bool = true,
};

/// Main Wizer struct using WAMR
pub const Wizer = struct {
    allocator: Allocator,
    config: WizerConfig,

    pub fn init(allocator: Allocator, config: WizerConfig) Wizer {
        return .{
            .allocator = allocator,
            .config = config,
        };
    }

    /// Run the complete wizer process: load, init, snapshot, rewrite
    pub fn run(self: *Wizer, wasm_path: []const u8, output_path: []const u8) !void {
        std.debug.print("[wizer-wamr] Loading {s}\n", .{wasm_path});
        flushStderr();

        // 1. Initialize WAMR runtime (use simple init for cross-platform compatibility)
        if (!c.wasm_runtime_init()) {
            std.debug.print("[wizer-wamr] Failed to initialize WAMR runtime\n", .{});
            flushStderr();
            return error.RuntimeInitFailed;
        }
        defer c.wasm_runtime_destroy();

        // Register stub host functions that wizer_init might need
        self.registerStubFunctions();

        // 2. Load WASM file
        const wasm_data = try std.fs.cwd().readFileAlloc(self.allocator, wasm_path, 100 * 1024 * 1024);
        defer self.allocator.free(wasm_data);

        var error_buf: [256]u8 = undefined;

        const module = c.wasm_runtime_load(wasm_data.ptr, @intCast(wasm_data.len), &error_buf, error_buf.len);
        if (module == null) {
            std.debug.print("[wizer-wamr] ERROR: Failed to load WASM module\n", .{});
            std.debug.print("[wizer-wamr] WAMR error: {s}\n", .{&error_buf});
            std.debug.print("[wizer-wamr] WASM file: {s} ({d} bytes)\n", .{ wasm_path, wasm_data.len });
            std.debug.print("[wizer-wamr] This usually means:\n", .{});
            std.debug.print("[wizer-wamr]   - WASM uses features not supported by WAMR interpreter\n", .{});
            std.debug.print("[wizer-wamr]   - WASM is corrupted or invalid\n", .{});
            std.debug.print("[wizer-wamr]   - WAMR was built without required features (check build-wamr.sh)\n", .{});
            flushStderr();
            return error.LoadFailed;
        }
        std.debug.print("[wizer-wamr] WASM module loaded successfully\n", .{});
        flushStderr();
        defer c.wasm_runtime_unload(module);

        // Set WASI args
        c.wasm_runtime_set_wasi_args(module, null, 0, null, 0, null, 0, null, 0);

        // 3. Instantiate module with sufficient memory for large WASM modules
        // Stack: 8MB for deep call stacks during initialization
        // Heap: dynamically sized based on WASM file size (min 64MB, or 4x WASM size)
        const stack_size: u32 = 8 * 1024 * 1024;
        const min_heap: u32 = 64 * 1024 * 1024;
        const dynamic_heap: u32 = @intCast(@min(wasm_data.len * 4, std.math.maxInt(u32)));
        const heap_size: u32 = @max(min_heap, dynamic_heap);

        std.debug.print("[wizer-wamr] Instantiating module (stack: {d}MB, heap: {d}MB)...\n", .{ stack_size / 1024 / 1024, heap_size / 1024 / 1024 });
        flushStderr();

        const module_inst = c.wasm_runtime_instantiate(module, stack_size, heap_size, &error_buf, error_buf.len);
        if (module_inst == null) {
            std.debug.print("[wizer-wamr] ERROR: Failed to instantiate module\n", .{});
            std.debug.print("[wizer-wamr] WAMR error: {s}\n", .{&error_buf});
            std.debug.print("[wizer-wamr] Stack: {d}MB, Heap: {d}MB\n", .{ stack_size / 1024 / 1024, heap_size / 1024 / 1024 });
            std.debug.print("[wizer-wamr] This usually means:\n", .{});
            std.debug.print("[wizer-wamr]   - Insufficient memory (try increasing heap/stack)\n", .{});
            std.debug.print("[wizer-wamr]   - Missing host functions (import not satisfied)\n", .{});
            flushStderr();
            return error.InstantiateFailed;
        }
        std.debug.print("[wizer-wamr] Module instantiated successfully\n", .{});
        flushStderr();
        defer c.wasm_runtime_deinstantiate(module_inst);

        // Create execution environment
        std.debug.print("[wizer-wamr] Creating execution environment...\n", .{});
        flushStderr();

        const exec_env = c.wasm_runtime_create_exec_env(module_inst, stack_size);
        if (exec_env == null) {
            std.debug.print("[wizer-wamr] Failed to create exec env\n", .{});
            flushStderr();
            return error.ExecEnvFailed;
        }
        std.debug.print("[wizer-wamr] Execution environment created\n", .{});
        flushStderr();
        defer c.wasm_runtime_destroy_exec_env(exec_env);

        // 4. Run the initialization function
        std.debug.print("[wizer-wamr] Running {s}()\n", .{self.config.init_func});
        flushStderr();
        try self.runInitFunc(module_inst, exec_env);
        std.debug.print("[wizer-wamr] Init function completed successfully\n", .{});
        flushStderr();

        // 5. Snapshot the state
        std.debug.print("[wizer-wamr] Snapshotting state\n", .{});
        flushStderr();
        var snapshot = try self.snapshotState(module_inst);
        defer snapshot.deinit();

        // 6. Rewrite WASM with snapshot
        // NOTE: wasm_runtime_load may modify the buffer, so we need to re-read it
        std.debug.print("[wizer-wamr] Rewriting WASM\n", .{});
        flushStderr();
        const original_wasm = try std.fs.cwd().readFileAlloc(self.allocator, wasm_path, 100 * 1024 * 1024);
        defer self.allocator.free(original_wasm);
        const rewritten = try self.rewrite(original_wasm, &snapshot);
        defer self.allocator.free(rewritten);

        // 7. Write output
        try std.fs.cwd().writeFile(.{ .sub_path = output_path, .data = rewritten });
        std.debug.print("[wizer-wamr] Wrote {s} ({d} bytes)\n", .{ output_path, rewritten.len });
        flushStderr();
    }

    fn registerStubFunctions(self: *Wizer) void {
        _ = self;
        // Register stub functions for imports that wizer_init might need
        // These are no-ops during initialization
        // NOTE: Using global mutable arrays (g_*_symbols) because WAMR modifies them in-place

        _ = c.wasm_runtime_register_natives("edgebox_process", &g_process_symbols, g_process_symbols.len);
        _ = c.wasm_runtime_register_natives("edgebox_http", &g_http_symbols, g_http_symbols.len);
        _ = c.wasm_runtime_register_natives("edgebox_spawn", &g_spawn_symbols, g_spawn_symbols.len);
        _ = c.wasm_runtime_register_natives("edgebox_file", &g_file_symbols, g_file_symbols.len);
        _ = c.wasm_runtime_register_natives("edgebox_zlib", &g_zlib_symbols, g_zlib_symbols.len);
        _ = c.wasm_runtime_register_natives("edgebox_crypto", &g_crypto_symbols, g_crypto_symbols.len);
        _ = c.wasm_runtime_register_natives("edgebox_socket", &g_socket_symbols, g_socket_symbols.len);
        _ = c.wasm_runtime_register_natives("edgebox_stdlib", &g_stdlib_symbols, g_stdlib_symbols.len);
        _ = c.wasm_runtime_register_natives("edgebox_wasm_component", &g_wasm_component_symbols, g_wasm_component_symbols.len);
        _ = c.wasm_runtime_register_natives("edgebox_gpu", &g_gpu_symbols, g_gpu_symbols.len);
    }

    fn runInitFunc(self: *Wizer, module_inst: c.wasm_module_inst_t, exec_env: c.wasm_exec_env_t) !void {
        // Find the init function
        const init_func_z = try self.allocator.dupeZ(u8, self.config.init_func);
        defer self.allocator.free(init_func_z);

        std.debug.print("[wizer-wamr] Looking up function '{s}'...\n", .{self.config.init_func});
        flushStderr();

        const init_func = c.wasm_runtime_lookup_function(module_inst, init_func_z.ptr);
        if (init_func == null) {
            std.debug.print("[wizer-wamr] Init function '{s}' not found\n", .{self.config.init_func});
            flushStderr();
            return error.InitFuncNotFound;
        }

        std.debug.print("[wizer-wamr] Calling wasm_runtime_call_wasm...\n", .{});
        flushStderr();

        if (!c.wasm_runtime_call_wasm(exec_env, init_func, 0, null)) {
            const exception = c.wasm_runtime_get_exception(module_inst);
            if (exception != null) {
                std.debug.print("[wizer-wamr] Init function failed: {s}\n", .{exception});
            } else {
                std.debug.print("[wizer-wamr] Init function failed (no exception message)\n", .{});
            }
            flushStderr();
            return error.InitFailed;
        }
    }

    fn snapshotState(self: *Wizer, module_inst: c.wasm_module_inst_t) !Snapshot {
        var snapshot = Snapshot.init(self.allocator);
        errdefer snapshot.deinit();

        // Snapshot critical exported globals (e.g., __stack_pointer)
        try self.snapshotExportedGlobals(module_inst, &snapshot);

        // Get default memory instance
        const mem_inst = c.wasm_runtime_get_default_memory(module_inst);
        if (mem_inst != null) {
            const mem_snap = try self.snapshotMemory(mem_inst.?);
            try snapshot.memories.append(self.allocator, mem_snap);
        }

        return snapshot;
    }

    /// Snapshot exported global variables (especially __stack_pointer)
    fn snapshotExportedGlobals(self: *Wizer, module_inst: c.wasm_module_inst_t, snapshot: *Snapshot) !void {
        // List of critical globals to snapshot
        const critical_globals = [_][]const u8{
            "__stack_pointer",
            "__heap_base",
            "__heap_end",
            "__data_end",
            "__dso_handle",
            "__global_base",
        };

        for (critical_globals, 0..) |name, idx| {
            const name_z = try self.allocator.dupeZ(u8, name);
            defer self.allocator.free(name_z);

            var global_inst: c.wasm_global_inst_t = undefined;
            if (c.wasm_runtime_get_export_global_inst(module_inst, name_z.ptr, &global_inst)) {
                // Read the global value based on its type
                const val = switch (global_inst.kind) {
                    c.WASM_I32 => blk: {
                        const ptr: *i32 = @alignCast(@ptrCast(global_inst.global_data));
                        std.debug.print("[wizer-wamr] Global '{s}' (i32) = {d}\n", .{ name, ptr.* });
                        break :blk SnapshotVal{ .i32 = ptr.* };
                    },
                    c.WASM_I64 => blk: {
                        const ptr: *i64 = @alignCast(@ptrCast(global_inst.global_data));
                        std.debug.print("[wizer-wamr] Global '{s}' (i64) = {d}\n", .{ name, ptr.* });
                        break :blk SnapshotVal{ .i64 = ptr.* };
                    },
                    c.WASM_F32 => blk: {
                        const ptr: *u32 = @alignCast(@ptrCast(global_inst.global_data));
                        break :blk SnapshotVal{ .f32 = ptr.* };
                    },
                    c.WASM_F64 => blk: {
                        const ptr: *u64 = @alignCast(@ptrCast(global_inst.global_data));
                        break :blk SnapshotVal{ .f64 = ptr.* };
                    },
                    else => continue, // Skip unsupported types
                };

                try snapshot.globals.append(self.allocator, .{
                    .index = @intCast(idx), // Will be resolved during rewrite
                    .value = val,
                    .name = name,
                });
            }
        }

        std.debug.print("[wizer-wamr] Captured {d} global values\n", .{snapshot.globals.items.len});
    }

    /// Snapshot memory using Wizer's algorithm:
    /// 1. Scan each page to find non-zero regions
    /// 2. Merge adjacent segments with small gaps (< 4 bytes)
    /// 3. If too many segments, merge smallest gaps first
    fn snapshotMemory(self: *Wizer, mem_inst: c.wasm_memory_inst_t) !MemorySnapshot {
        const page_size: usize = 65536; // 64KB per WASM page
        const num_pages = c.wasm_memory_get_cur_page_count(mem_inst);
        const mem_size = num_pages * page_size;

        std.debug.print("[wizer-wamr] Memory: {d} pages ({d} KB)\n", .{ num_pages, mem_size / 1024 });

        var result = MemorySnapshot{
            .min_pages = num_pages,
            .segments = .{},
        };
        errdefer result.deinit(self.allocator);

        if (mem_size == 0) {
            return result;
        }

        // Get pointer to entire memory
        const mem_ptr: ?[*]u8 = @ptrCast(c.wasm_memory_get_base_address(mem_inst));
        if (mem_ptr == null) {
            return result;
        }

        const memory = mem_ptr.?[0..mem_size];

        // Temporary list of segment ranges (before data copy)
        var ranges = std.ArrayListUnmanaged(Range){};
        defer ranges.deinit(self.allocator);

        // Step 1: Find non-zero regions per page
        var page_idx: usize = 0;
        while (page_idx < num_pages) : (page_idx += 1) {
            const page_start = page_idx * page_size;
            const page_end = page_start + page_size;
            var start = page_start;

            while (start < page_end) {
                // Find first non-zero byte
                while (start < page_end and memory[start] == 0) {
                    start += 1;
                }
                if (start >= page_end) break;

                // Find end of non-zero region
                var end = start;
                while (end < page_end and memory[end] != 0) {
                    end += 1;
                }

                try ranges.append(self.allocator, .{ .start = start, .end = end });
                start = end;
            }
        }

        if (ranges.items.len == 0) {
            return result;
        }

        // Step 2: Sort by start offset (already sorted since we iterate sequentially)

        // Step 3: Merge adjacent segments with small gaps (< MIN_ACTIVE_SEGMENT_OVERHEAD)
        var merged = std.ArrayListUnmanaged(Range){};
        defer merged.deinit(self.allocator);

        try merged.append(self.allocator, ranges.items[0]);
        for (ranges.items[1..]) |b| {
            const a = &merged.items[merged.items.len - 1];

            // Merge if gap is small
            const gap = b.start - a.end;
            if (gap <= MIN_ACTIVE_SEGMENT_OVERHEAD) {
                a.end = b.end;
            } else {
                try merged.append(self.allocator, b);
            }
        }

        // Step 4: If too many segments, merge smallest gaps first
        if (merged.items.len > MAX_DATA_SEGMENTS) {
            try self.removeExcessSegments(&merged);
        }

        // Step 5: Copy actual data for final segments
        var max_offset: usize = 0;
        var min_offset: usize = std.math.maxInt(usize);
        for (merged.items) |range| {
            const data = try self.allocator.dupe(u8, memory[range.start..range.end]);
            try result.segments.append(self.allocator, .{
                .memory_index = 0,
                .offset = range.start,
                .data = data,
                .is64 = false,
            });
            if (range.end > max_offset) max_offset = range.end;
            if (range.start < min_offset) min_offset = range.start;
        }

        // Debug: Check value at critical address (_start init flag)
        // The actual address is from _start disassembly: i32.load 2 134676480
        // NOTE: We do NOT set this flag - _start needs to run its initialization
        // which includes calling __wasm_call_ctors, main, etc.
        const init_flag_addr: usize = 134676480;
        if (init_flag_addr < mem_size) {
            const init_flag_value = memory[init_flag_addr];
            std.debug.print("[wizer-wamr] DEBUG: _start init flag at 0x{x} = {d} (should be 0)\n", .{ init_flag_addr, init_flag_value });
        }

        // Step 6: Check for isolated non-zero bytes that might have been missed
        // (e.g., init flags that are surrounded by zeros)
        // Scan the gap regions for any non-zero data
        var prev_end: usize = 0;
        for (merged.items) |range| {
            if (range.start > prev_end) {
                // There's a gap - scan it for non-zero bytes
                const gap_start = prev_end;
                const gap_end = range.start;
                var i = gap_start;
                while (i < gap_end) : (i += 1) {
                    if (memory[i] != 0) {
                        // Found non-zero byte in gap! Create a segment for it
                        var end = i + 1;
                        // Extend to include adjacent non-zero bytes
                        while (end < gap_end and memory[end] != 0) : (end += 1) {}

                        if (result.segments.items.len < MAX_DATA_SEGMENTS) {
                            const gap_data = try self.allocator.dupe(u8, memory[i..end]);
                            try result.segments.append(self.allocator, .{
                                .memory_index = 0,
                                .offset = i,
                                .data = gap_data,
                                .is64 = false,
                            });
                            std.debug.print("[wizer-wamr] Found missed data in gap at offset {d} (0x{x}), size={d}\n", .{ i, i, end - i });
                        }
                        i = end;
                    }
                }
            }
            prev_end = range.end;
        }

        std.debug.print("[wizer-wamr] Found {d} data segments\n", .{result.segments.items.len});
        std.debug.print("[wizer-wamr] Memory range: {d} ({d}MB) to {d} ({d}MB)\n", .{
            min_offset, min_offset / (1024 * 1024),
            max_offset, max_offset / (1024 * 1024)
        });

        // Debug: print first 5 segments
        const print_count = @min(5, result.segments.items.len);
        for (result.segments.items[0..print_count], 0..) |seg, i| {
            std.debug.print("[wizer-wamr] Segment {d}: offset={d} (0x{x}), size={d}\n", .{
                i, seg.offset, seg.offset, seg.data.len
            });
        }
        if (result.segments.items.len > 5) {
            std.debug.print("[wizer-wamr] ... and {d} more segments\n", .{result.segments.items.len - 5});
        }

        return result;
    }

    const Range = struct { start: usize, end: usize };

    fn removeExcessSegments(self: *Wizer, segments: *std.ArrayListUnmanaged(Range)) !void {
        _ = self;
        while (segments.items.len > MAX_DATA_SEGMENTS) {
            // Find the smallest gap and merge those two segments
            var min_gap: usize = std.math.maxInt(usize);
            var min_idx: usize = 0;

            for (segments.items[0 .. segments.items.len - 1], 0..) |seg, idx| {
                const next = segments.items[idx + 1];
                const gap = next.start - seg.end;
                if (gap < min_gap) {
                    min_gap = gap;
                    min_idx = idx;
                }
            }

            // Merge segment at min_idx with the next segment
            segments.items[min_idx].end = segments.items[min_idx + 1].end;
            _ = segments.orderedRemove(min_idx + 1);
        }
    }

    /// Rewrite the WASM binary with snapshotted state
    pub fn rewrite(self: *Wizer, original: []const u8, snapshot: *const Snapshot) ![]u8 {

        var output = std.ArrayListUnmanaged(u8){};
        errdefer output.deinit(self.allocator);

        // Validate magic number and version
        if (original.len < 8) return error.InvalidWasm;
        if (!std.mem.eql(u8, original[0..4], "\x00asm")) return error.InvalidWasm;

        // PASS 1: Parse export section to build global name -> index map
        var global_name_to_idx = std.StringHashMap(u32).init(self.allocator);
        defer global_name_to_idx.deinit();

        var scan_pos: usize = 8;
        while (scan_pos < original.len) {
            const section_id = original[scan_pos];
            scan_pos += 1;
            const section_len = readLEB128u32(original, &scan_pos) catch break;
            if (scan_pos + section_len > original.len) break;

            if (@as(SectionId, @enumFromInt(section_id)) == .@"export") {
                var exp_pos: usize = 0;
                const export_data = original[scan_pos .. scan_pos + section_len];
                const num_exports = readLEB128u32(export_data, &exp_pos) catch break;

                var i: u32 = 0;
                while (i < num_exports) : (i += 1) {
                    const name_len = readLEB128u32(export_data, &exp_pos) catch break;
                    if (exp_pos + name_len > export_data.len) break;
                    const name = export_data[exp_pos .. exp_pos + name_len];
                    exp_pos += name_len;

                    if (exp_pos >= export_data.len) break;
                    const kind = export_data[exp_pos];
                    exp_pos += 1;
                    const index = readLEB128u32(export_data, &exp_pos) catch break;

                    // kind == 3 means global export
                    if (kind == 3) {
                        try global_name_to_idx.put(name, index);
                        std.debug.print("[wizer-wamr] Found global export: '{s}' -> index {d}\n", .{ name, index });
                    }
                }
                break;
            }
            scan_pos += section_len;
        }

        // Copy magic and version
        try output.appendSlice(self.allocator, original[0..8]);

        // PASS 2: Rewrite sections
        var pos: usize = 8;
        var added_data_section = false;
        var data_count_added = false;

        var section_count: usize = 0;
        while (pos < original.len) {
            const section_id = original[pos];
            pos += 1;

            const section_len = readLEB128u32(original, &pos) catch |err| {
                std.debug.print("[wizer-wamr] Error reading section len at pos {d}, section_id={d}: {}\n", .{ pos, section_id, err });
                return err;
            };
            if (pos + section_len > original.len) {
                std.debug.print("[wizer-wamr] Section {d} extends past end: pos={d}, len={d}, total={d}\n", .{ section_id, pos, section_len, original.len });
                return error.InvalidWasm;
            }
            const section_data = original[pos .. pos + section_len];
            const section_end = pos + section_len;

            section_count += 1;

            switch (@as(SectionId, @enumFromInt(section_id))) {
                .memory => {
                    try self.rewriteMemorySection(&output, section_data, snapshot);
                },
                .global => {
                    try self.rewriteGlobalSection(&output, section_data, snapshot, &global_name_to_idx);
                },
                .@"export" => {
                    try self.rewriteExportSection(&output, section_data);
                },
                .start => {
                    // Skip start section - already ran
                },
                .data_count => {
                    const mem_segments = if (snapshot.memories.items.len > 0)
                        snapshot.memories.items[0].segments.items.len
                    else
                        0;
                    try self.writeDataCountSection(&output, @intCast(mem_segments));
                    data_count_added = true;
                },
                .data => {
                    try self.writeDataSection(&output, snapshot);
                    added_data_section = true;
                },
                else => {
                    // Copy section as-is
                    try output.append(self.allocator, section_id);
                    try writeLEB128u32(self.allocator, &output, section_len);
                    try output.appendSlice(self.allocator, section_data);
                },
            }

            pos = section_end;
        }

        // Add data section if not present in original
        if (!added_data_section and snapshot.memories.items.len > 0) {
            if (!data_count_added) {
                const mem_segments = snapshot.memories.items[0].segments.items.len;
                try self.writeDataCountSection(&output, @intCast(mem_segments));
            }
            try self.writeDataSection(&output, snapshot);
        }

        return output.toOwnedSlice(self.allocator);
    }

    fn rewriteMemorySection(self: *Wizer, output: *std.ArrayListUnmanaged(u8), section_data: []const u8, snapshot: *const Snapshot) !void {
        // We MUST update memory limits to match captured state.
        // Data segments have offsets up to max_offset which may exceed original min_pages.
        // If min_pages < required pages for data segments, instantiation will fail with
        // "out of bounds memory access" when WASM runtime tries to load data segments.
        _ = section_data;

        var section = std.ArrayListUnmanaged(u8){};
        defer section.deinit(self.allocator);

        // Write number of memories (always 1 for WASM MVP)
        try writeLEB128u32(self.allocator, &section, 1);

        if (snapshot.memories.items.len > 0) {
            const mem = snapshot.memories.items[0];
            // Use captured min_pages with max=65536 (4GB WASM32 limit)
            // Important: must include max, otherwise some runtimes may have issues
            std.debug.print("[wizer-wamr] Writing memory section: initial={d} pages ({d} MB), max=65536 pages (4GB)\n", .{ mem.min_pages, mem.min_pages * 65536 / 1024 / 1024 });
            flushStderr();

            // Also verify data segments fit within the memory
            var max_data_end: u64 = 0;
            for (mem.segments.items) |seg| {
                const seg_end = seg.offset + seg.data.len;
                if (seg_end > max_data_end) max_data_end = seg_end;
            }
            const pages_needed = (max_data_end + 65535) / 65536;
            std.debug.print("[wizer-wamr] Data extends to {d} bytes ({d} MB), needs {d} pages\n", .{ max_data_end, max_data_end / 1024 / 1024, pages_needed });
            if (pages_needed > mem.min_pages) {
                std.debug.print("[wizer-wamr] WARNING: Data needs {d} pages but memory only has {d} pages!\n", .{ pages_needed, mem.min_pages });
            }
            flushStderr();

            try section.append(self.allocator, 0x01); // flags: 1 = has max
            try writeLEB128u32(self.allocator, &section, @intCast(mem.min_pages)); // initial
            try writeLEB128u32(self.allocator, &section, 65536); // max = 4GB
        } else {
            // Fallback: no memory snapshot, keep minimal
            try section.append(self.allocator, 0x01); // has max
            try writeLEB128u32(self.allocator, &section, 1);
            try writeLEB128u32(self.allocator, &section, 65536);
        }

        // Write section header
        try output.append(self.allocator, @intFromEnum(SectionId.memory));
        try writeLEB128u32(self.allocator, output, @intCast(section.items.len));
        try output.appendSlice(self.allocator, section.items);
    }

    fn rewriteGlobalSection(self: *Wizer, output: *std.ArrayListUnmanaged(u8), section_data: []const u8, snapshot: *const Snapshot, global_name_to_idx: *const std.StringHashMap(u32)) !void {
        // Build map of global index -> new value from snapshot
        var global_updates = std.AutoHashMap(u32, SnapshotVal).init(self.allocator);
        defer global_updates.deinit();

        for (snapshot.globals.items) |g| {
            if (global_name_to_idx.get(g.name)) |idx| {
                try global_updates.put(idx, g.value);
                std.debug.print("[wizer-wamr] Will update global {d} ({s})\n", .{ idx, g.name });
            }
        }

        if (global_updates.count() == 0) {
            // No updates needed, copy as-is
            try output.append(self.allocator, @intFromEnum(SectionId.global));
            try writeLEB128u32(self.allocator, output, @intCast(section_data.len));
            try output.appendSlice(self.allocator, section_data);
            return;
        }

        // Parse and rewrite global section
        var section = std.ArrayListUnmanaged(u8){};
        defer section.deinit(self.allocator);

        var pos: usize = 0;
        const num_globals = try readLEB128u32(section_data, &pos);
        try writeLEB128u32(self.allocator, &section, num_globals);

        var global_idx: u32 = 0;
        while (global_idx < num_globals) : (global_idx += 1) {
            // Global type: valtype mutability
            if (pos >= section_data.len) return error.InvalidWasm;
            const valtype = section_data[pos];
            pos += 1;
            try section.append(self.allocator, valtype);

            if (pos >= section_data.len) return error.InvalidWasm;
            const mutability = section_data[pos];
            pos += 1;
            try section.append(self.allocator, mutability);

            // Init expression - check if we need to update this global
            if (global_updates.get(global_idx)) |new_val| {
                // Skip original init expression
                try self.skipInitExpr(section_data, &pos);

                // Write new init expression with updated value
                switch (new_val) {
                    .i32 => |v| {
                        try section.append(self.allocator, 0x41); // i32.const
                        try writeLEB128i32(self.allocator, &section, v);
                    },
                    .i64 => |v| {
                        try section.append(self.allocator, 0x42); // i64.const
                        try writeLEB128i64(self.allocator, &section, v);
                    },
                    .f32 => |v| {
                        try section.append(self.allocator, 0x43); // f32.const
                        try section.appendSlice(self.allocator, std.mem.asBytes(&v));
                    },
                    .f64 => |v| {
                        try section.append(self.allocator, 0x44); // f64.const
                        try section.appendSlice(self.allocator, std.mem.asBytes(&v));
                    },
                    .v128 => |_| {
                        // v128 not supported in init expr, skip
                        return error.UnsupportedGlobalType;
                    },
                }
                try section.append(self.allocator, 0x0b); // end
            } else {
                // Copy original init expression
                const expr_start = pos;
                try self.skipInitExpr(section_data, &pos);
                try section.appendSlice(self.allocator, section_data[expr_start..pos]);
            }
        }

        try output.append(self.allocator, @intFromEnum(SectionId.global));
        try writeLEB128u32(self.allocator, output, @intCast(section.items.len));
        try output.appendSlice(self.allocator, section.items);
    }

    /// Skip over an init expression (until 0x0b end opcode)
    fn skipInitExpr(_: *Wizer, data: []const u8, pos: *usize) !void {
        while (pos.* < data.len) {
            const opcode = data[pos.*];
            pos.* += 1;

            if (opcode == 0x0b) return; // end

            // Skip operand based on opcode
            switch (opcode) {
                0x41 => _ = try readLEB128i32(data, pos), // i32.const
                0x42 => _ = try readLEB128i64(data, pos), // i64.const
                0x43 => pos.* += 4, // f32.const
                0x44 => pos.* += 8, // f64.const
                0x23 => _ = try readLEB128u32(data, pos), // global.get
                0xd2 => _ = try readLEB128u32(data, pos), // ref.func
                0xd0 => pos.* += 1, // ref.null
                0xfb => {
                    // Extended init opcodes (GC proposal)
                    _ = try readLEB128u32(data, pos);
                    // May have additional operands, but for now skip
                },
                else => {
                    // Unknown opcode in init expr
                    std.debug.print("[wizer-wamr] Unknown init opcode: 0x{x}\n", .{opcode});
                },
            }
        }
        return error.UnterminatedInitExpr;
    }

    fn rewriteExportSection(self: *Wizer, output: *std.ArrayListUnmanaged(u8), section_data: []const u8) !void {
        var section = std.ArrayListUnmanaged(u8){};
        defer section.deinit(self.allocator);

        var pos: usize = 0;
        const num_exports = try readLEB128u32(section_data, &pos);

        var kept_exports = std.ArrayListUnmanaged(u8){};
        defer kept_exports.deinit(self.allocator);
        var kept_count: u32 = 0;

        var i: u32 = 0;
        while (i < num_exports) : (i += 1) {
            const name_len = try readLEB128u32(section_data, &pos);
            if (pos + name_len > section_data.len) return error.UnexpectedEof;
            const name = section_data[pos .. pos + name_len];
            pos += name_len;

            if (pos >= section_data.len) return error.UnexpectedEof;
            const kind = section_data[pos];
            pos += 1;

            const index = try readLEB128u32(section_data, &pos);

            // Skip wizer_init and _initialize exports
            if (std.mem.eql(u8, name, self.config.init_func) or
                std.mem.eql(u8, name, "_initialize"))
            {
                continue;
            }

            // Keep this export
            try writeLEB128u32(self.allocator, &kept_exports, name_len);
            try kept_exports.appendSlice(self.allocator, name);
            try kept_exports.append(self.allocator, kind);
            try writeLEB128u32(self.allocator, &kept_exports, index);
            kept_count += 1;
        }

        // Write section
        try writeLEB128u32(self.allocator, &section, kept_count);
        try section.appendSlice(self.allocator, kept_exports.items);

        try output.append(self.allocator, @intFromEnum(SectionId.@"export"));
        try writeLEB128u32(self.allocator, output, @intCast(section.items.len));
        try output.appendSlice(self.allocator, section.items);
    }

    fn writeDataCountSection(self: *Wizer, output: *std.ArrayListUnmanaged(u8), count: u32) !void {
        var section = std.ArrayListUnmanaged(u8){};
        defer section.deinit(self.allocator);

        try writeLEB128u32(self.allocator, &section, count);

        try output.append(self.allocator, @intFromEnum(SectionId.data_count));
        try writeLEB128u32(self.allocator, output, @intCast(section.items.len));
        try output.appendSlice(self.allocator, section.items);
    }

    fn writeDataSection(self: *Wizer, output: *std.ArrayListUnmanaged(u8), snapshot: *const Snapshot) !void {
        var section = std.ArrayListUnmanaged(u8){};
        defer section.deinit(self.allocator);

        // DEBUG: Try with empty data section to isolate the issue
        // Set to true to test if data segments are causing the issue
        const USE_EMPTY_DATA = std.mem.eql(u8, std.fs.path.basename(std.posix.getenv("WIZER_EMPTY_DATA") orelse ""), "1");
        if (USE_EMPTY_DATA) {
            try writeLEB128u32(self.allocator, &section, 0);
            try output.append(self.allocator, @intFromEnum(SectionId.data));
            try writeLEB128u32(self.allocator, output, @intCast(section.items.len));
            try output.appendSlice(self.allocator, section.items);
            return;
        }

        if (snapshot.memories.items.len == 0) {
            try writeLEB128u32(self.allocator, &section, 0);
        } else {
            const segments = snapshot.memories.items[0].segments.items;
            try writeLEB128u32(self.allocator, &section, @intCast(segments.len));

            for (segments) |seg| {
                // Active segment: 0x00 = memory 0 with i32 offset
                try section.append(self.allocator, 0x00);

                // Offset expression: i32.const <offset> end
                // For offsets >= 2GB, we need to bit-cast to i32 (wraps to negative)
                try section.append(self.allocator, 0x41); // i32.const
                const offset_u32: u32 = @intCast(seg.offset);
                const offset_i32: i32 = @bitCast(offset_u32);
                try writeLEB128i32(self.allocator, &section, offset_i32);
                try section.append(self.allocator, 0x0b); // end

                // Data length and bytes
                try writeLEB128u32(self.allocator, &section, @intCast(seg.data.len));
                try section.appendSlice(self.allocator, seg.data);
            }
        }

        try output.append(self.allocator, @intFromEnum(SectionId.data));
        try writeLEB128u32(self.allocator, output, @intCast(section.items.len));
        try output.appendSlice(self.allocator, section.items);
    }
};

// Stub functions for host imports during initialization
fn stubVoid1(_: c.wasm_exec_env_t, _: i32) void {}
fn stubVoid2(_: c.wasm_exec_env_t, _: i32, _: i32) void {}
fn stubVoid4(_: c.wasm_exec_env_t, _: i32, _: i32, _: i32, _: i32) void {}
fn stubInt0(_: c.wasm_exec_env_t) i32 {
    return 0;
}
fn stubInt1(_: c.wasm_exec_env_t, _: i32) i32 {
    return -1;
}
fn stubInt2(_: c.wasm_exec_env_t, _: i32, _: i32) i32 {
    return -1;
}
fn stubInt3(_: c.wasm_exec_env_t, _: i32, _: i32, _: i32) i32 {
    return -1;
}
fn stubInt4(_: c.wasm_exec_env_t, _: i32, _: i32, _: i32, _: i32) i32 {
    return -1;
}
fn stubInt5(_: c.wasm_exec_env_t, _: i32, _: i32, _: i32, _: i32, _: i32) i32 {
    return -1;
}
fn stubInt7(_: c.wasm_exec_env_t, _: i32, _: i32, _: i32, _: i32, _: i32, _: i32, _: i32) i32 {
    return -1;
}
fn stubInt9(_: c.wasm_exec_env_t, _: i32, _: i32, _: i32, _: i32, _: i32, _: i32, _: i32, _: i32, _: i32) i32 {
    return -1;
}

// IMPORTANT: These must be global/static and mutable (var) because WAMR modifies them in-place
// and retains references to them. Using const local arrays causes crashes on Linux.
const NativeSymbol = c.NativeSymbol;

var g_process_symbols = [_]NativeSymbol{
    .{ .symbol = "edgebox_process_set_prog_name", .func_ptr = @constCast(@ptrCast(&stubVoid2)), .signature = "(ii)", .attachment = null },
    .{ .symbol = "edgebox_process_add_arg", .func_ptr = @constCast(@ptrCast(&stubVoid2)), .signature = "(ii)", .attachment = null },
    .{ .symbol = "edgebox_process_add_env", .func_ptr = @constCast(@ptrCast(&stubVoid4)), .signature = "(iiii)", .attachment = null },
    .{ .symbol = "edgebox_process_add_stdin", .func_ptr = @constCast(@ptrCast(&stubVoid2)), .signature = "(ii)", .attachment = null },
    .{ .symbol = "edgebox_process_set_timeout", .func_ptr = @constCast(@ptrCast(&stubVoid1)), .signature = "(i)", .attachment = null },
    .{ .symbol = "edgebox_process_run", .func_ptr = @constCast(@ptrCast(&stubInt0)), .signature = "()i", .attachment = null },
    .{ .symbol = "edgebox_process_get_exit_code", .func_ptr = @constCast(@ptrCast(&stubInt0)), .signature = "()i", .attachment = null },
    .{ .symbol = "edgebox_process_get_stdout_len", .func_ptr = @constCast(@ptrCast(&stubInt0)), .signature = "()i", .attachment = null },
    .{ .symbol = "edgebox_process_get_stdout", .func_ptr = @constCast(@ptrCast(&stubVoid1)), .signature = "(i)", .attachment = null },
    .{ .symbol = "edgebox_process_get_stderr_len", .func_ptr = @constCast(@ptrCast(&stubInt0)), .signature = "()i", .attachment = null },
    .{ .symbol = "edgebox_process_get_stderr", .func_ptr = @constCast(@ptrCast(&stubVoid1)), .signature = "(i)", .attachment = null },
};

var g_http_symbols = [_]NativeSymbol{
    .{ .symbol = "http_dispatch", .func_ptr = @constCast(@ptrCast(&stubInt9)), .signature = "(iiiiiiiii)i", .attachment = null },
};

var g_spawn_symbols = [_]NativeSymbol{
    .{ .symbol = "spawn_dispatch", .func_ptr = @constCast(@ptrCast(&stubInt5)), .signature = "(iiiii)i", .attachment = null },
};

var g_file_symbols = [_]NativeSymbol{
    .{ .symbol = "file_dispatch", .func_ptr = @constCast(@ptrCast(&stubInt5)), .signature = "(iiiii)i", .attachment = null },
};

var g_zlib_symbols = [_]NativeSymbol{
    .{ .symbol = "zlib_dispatch", .func_ptr = @constCast(@ptrCast(&stubInt3)), .signature = "(iii)i", .attachment = null },
};

var g_crypto_symbols = [_]NativeSymbol{
    .{ .symbol = "crypto_dispatch", .func_ptr = @constCast(@ptrCast(&stubInt7)), .signature = "(iiiiiii)i", .attachment = null },
};

var g_socket_symbols = [_]NativeSymbol{
    .{ .symbol = "socket_dispatch", .func_ptr = @constCast(@ptrCast(&stubInt4)), .signature = "(iiii)i", .attachment = null },
};

var g_stdlib_symbols = [_]NativeSymbol{
    .{ .symbol = "stdlib_dispatch", .func_ptr = @constCast(@ptrCast(&stubInt5)), .signature = "(iiiii)i", .attachment = null },
};

var g_wasm_component_symbols = [_]NativeSymbol{
    .{ .symbol = "wasm_component_load", .func_ptr = @constCast(@ptrCast(&stubInt2)), .signature = "(ii)i", .attachment = null },
    .{ .symbol = "wasm_component_export_count", .func_ptr = @constCast(@ptrCast(&stubInt1)), .signature = "(i)i", .attachment = null },
    .{ .symbol = "wasm_component_export_name", .func_ptr = @constCast(@ptrCast(&stubInt4)), .signature = "(iiii)i", .attachment = null },
    .{ .symbol = "wasm_component_call", .func_ptr = @constCast(@ptrCast(&stubInt5)), .signature = "(iiiii)i", .attachment = null },
};

var g_gpu_symbols = [_]NativeSymbol{
    .{ .symbol = "gpu_dispatch", .func_ptr = @constCast(@ptrCast(&stubInt7)), .signature = "(iiiiiii)i", .attachment = null },
};

// LEB128 encoding/decoding helpers
fn readLEB128u32(data: []const u8, pos: *usize) !u32 {
    var result: u32 = 0;
    var shift: u32 = 0;

    while (shift < 35) { // u32 max is 5 bytes = 35 bits
        if (pos.* >= data.len) return error.UnexpectedEof;
        const byte = data[pos.*];
        pos.* += 1;

        result |= @as(u32, byte & 0x7f) << @intCast(shift);
        if (byte & 0x80 == 0) break;
        shift += 7;
    }

    return result;
}

fn writeLEB128u32(allocator: Allocator, output: *std.ArrayListUnmanaged(u8), value: u32) !void {
    var v = value;
    while (true) {
        const byte: u8 = @truncate(v & 0x7f);
        v >>= 7;
        if (v == 0) {
            try output.append(allocator, byte);
            break;
        } else {
            try output.append(allocator, byte | 0x80);
        }
    }
}

fn writeLEB128i32(allocator: Allocator, output: *std.ArrayListUnmanaged(u8), value: i32) !void {
    var v = value;
    while (true) {
        const byte: u8 = @truncate(@as(u32, @bitCast(v)) & 0x7f);
        v >>= 7;

        const sign_bit = (byte & 0x40) != 0;
        if ((v == 0 and !sign_bit) or (v == -1 and sign_bit)) {
            try output.append(allocator, byte);
            break;
        } else {
            try output.append(allocator, byte | 0x80);
        }
    }
}

fn readLEB128i32(data: []const u8, pos: *usize) !i32 {
    var result: i32 = 0;
    var shift: u5 = 0;
    var byte: u8 = undefined;

    while (true) {
        if (pos.* >= data.len) return error.UnexpectedEof;
        byte = data[pos.*];
        pos.* += 1;

        result |= @as(i32, @intCast(byte & 0x7f)) << shift;
        shift +|= 7;
        if (byte & 0x80 == 0) break;
    }

    // Sign extend if negative
    if (shift < 32 and (byte & 0x40) != 0) {
        result |= @as(i32, -1) << shift;
    }

    return result;
}

fn writeLEB128i64(allocator: Allocator, output: *std.ArrayListUnmanaged(u8), value: i64) !void {
    var v = value;
    while (true) {
        const byte: u8 = @truncate(@as(u64, @bitCast(v)) & 0x7f);
        v >>= 7;

        const sign_bit = (byte & 0x40) != 0;
        if ((v == 0 and !sign_bit) or (v == -1 and sign_bit)) {
            try output.append(allocator, byte);
            break;
        } else {
            try output.append(allocator, byte | 0x80);
        }
    }
}

fn readLEB128i64(data: []const u8, pos: *usize) !i64 {
    var result: i64 = 0;
    var shift: u6 = 0;
    var byte: u8 = undefined;

    while (true) {
        if (pos.* >= data.len) return error.UnexpectedEof;
        byte = data[pos.*];
        pos.* += 1;

        result |= @as(i64, @intCast(byte & 0x7f)) << shift;
        shift +|= 7;
        if (byte & 0x80 == 0) break;
    }

    // Sign extend if negative
    if (shift < 64 and (byte & 0x40) != 0) {
        result |= @as(i64, -1) << shift;
    }

    return result;
}

/// Run wizer snapshot from command line args
pub fn runSnapshot(allocator: Allocator, args: []const [:0]const u8) !void {
    if (args.len < 2) {
        std.debug.print("Usage: edgeboxc snapshot <input.wasm> <output.wasm> [--init-func=name]\n", .{});
        return error.InvalidArgs;
    }

    var config = WizerConfig{};

    // Parse --init-func
    if (args.len > 2) {
        for (args[2..]) |arg| {
            if (std.mem.startsWith(u8, arg, "--init-func=")) {
                config.init_func = arg[12..];
            }
        }
    }

    var wizer = Wizer.init(allocator, config);
    try wizer.run(args[0], args[1]);
}

/// Standalone CLI entry point
pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    if (args.len < 3) {
        std.debug.print("Usage: edgebox-wizer <input.wasm> <output.wasm> [--init-func=name]\n", .{});
        return;
    }

    var config = WizerConfig{};

    // Parse --init-func
    for (args[3..]) |arg| {
        if (std.mem.startsWith(u8, arg, "--init-func=")) {
            config.init_func = arg[12..];
        }
    }

    var wizer = Wizer.init(allocator, config);
    try wizer.run(args[1], args[2]);
}

test "LEB128 encoding" {
    const allocator = std.testing.allocator;

    var buf = std.ArrayListUnmanaged(u8){};
    defer buf.deinit(allocator);

    try writeLEB128u32(allocator, &buf, 624485);
    try std.testing.expectEqualSlices(u8, &[_]u8{ 0xe5, 0x8e, 0x26 }, buf.items);
}
