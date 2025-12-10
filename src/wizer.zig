/// Wizer: WebAssembly Pre-initializer (Pure Zig Implementation)
///
/// This module reimplements Wizer functionality in Zig, removing the Rust dependency.
/// It pre-initializes WASM modules by:
/// 1. Running the initialization function (wizer_init)
/// 2. Snapshotting memory and globals
/// 3. Rewriting the WASM binary with pre-initialized state
///
/// This enables instant startup by embedding initialized state in the WASM binary.
///
/// Based on: https://github.com/bytecodealliance/wasmtime/tree/main/crates/wizer
///
/// Key differences from Rust Wizer:
/// - Uses WasmEdge instead of Wasmtime (no instrumentation pass needed)
/// - Simpler: WasmEdge gives direct memory access without extra exports
/// - Integrated with EdgeBox's existing WasmEdge infrastructure
const std = @import("std");

// WasmEdge C API bindings
const c = @cImport({
    @cInclude("wasmedge/wasmedge.h");
});

const Allocator = std.mem.Allocator;

/// Maximum number of data segments to emit (engines have limits)
const MAX_DATA_SEGMENTS: usize = 10_000;

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

/// Main Wizer struct
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
        std.debug.print("[wizer] Loading {s}\n", .{wasm_path});

        // 1. Load and instantiate the WASM module
        const vm = try self.createVM();
        defer c.WasmEdge_VMDelete(vm);

        // Load the module
        var path_buf: [4096]u8 = undefined;
        @memcpy(path_buf[0..wasm_path.len], wasm_path);
        path_buf[wasm_path.len] = 0;

        var result = c.WasmEdge_VMLoadWasmFromFile(vm, &path_buf);
        if (!c.WasmEdge_ResultOK(result)) {
            std.debug.print("[wizer] Failed to load WASM: {s}\n", .{c.WasmEdge_ResultGetMessage(result)});
            return error.LoadFailed;
        }

        result = c.WasmEdge_VMValidate(vm);
        if (!c.WasmEdge_ResultOK(result)) {
            std.debug.print("[wizer] Validation failed: {s}\n", .{c.WasmEdge_ResultGetMessage(result)});
            return error.ValidationFailed;
        }

        result = c.WasmEdge_VMInstantiate(vm);
        if (!c.WasmEdge_ResultOK(result)) {
            std.debug.print("[wizer] Instantiation failed: {s}\n", .{c.WasmEdge_ResultGetMessage(result)});
            return error.InstantiateFailed;
        }

        // 2. Run the initialization function
        std.debug.print("[wizer] Running {s}()\n", .{self.config.init_func});
        try self.runInitFunc(vm);

        // 3. Snapshot the state
        std.debug.print("[wizer] Snapshotting state\n", .{});
        var snapshot = try self.snapshotState(vm);
        defer snapshot.deinit();

        // 4. Read original WASM and rewrite with snapshot
        std.debug.print("[wizer] Rewriting WASM\n", .{});
        const original_wasm = try std.fs.cwd().readFileAlloc(self.allocator, wasm_path, 100 * 1024 * 1024);
        defer self.allocator.free(original_wasm);

        const rewritten = try self.rewrite(original_wasm, &snapshot);
        defer self.allocator.free(rewritten);

        // 5. Write output
        try std.fs.cwd().writeFile(.{ .sub_path = output_path, .data = rewritten });
        std.debug.print("[wizer] Wrote {s} ({d} bytes)\n", .{ output_path, rewritten.len });
    }

    fn createVM(self: *Wizer) !*c.WasmEdge_VMContext {
        _ = self;
        const conf = c.WasmEdge_ConfigureCreate();
        defer c.WasmEdge_ConfigureDelete(conf);

        // Enable bulk memory operations (required for data segments)
        c.WasmEdge_ConfigureAddProposal(conf, c.WasmEdge_Proposal_BulkMemoryOperations);
        c.WasmEdge_ConfigureAddProposal(conf, c.WasmEdge_Proposal_SIMD);

        // Enable WASI
        c.WasmEdge_ConfigureAddHostRegistration(conf, c.WasmEdge_HostRegistration_Wasi);

        const vm = c.WasmEdge_VMCreate(conf, null);
        if (vm == null) {
            return error.VMCreateFailed;
        }

        // Initialize WASI
        const wasi = c.WasmEdge_VMGetImportModuleContext(vm, c.WasmEdge_HostRegistration_Wasi);
        if (wasi != null) {
            c.WasmEdge_ModuleInstanceInitWASI(wasi, null, 0, null, 0, null, 0);
        }

        return vm.?;
    }

    fn runInitFunc(self: *Wizer, vm: *c.WasmEdge_VMContext) !void {
        var func_name_buf: [256]u8 = undefined;
        @memcpy(func_name_buf[0..self.config.init_func.len], self.config.init_func);

        const func_name = c.WasmEdge_StringCreateByBuffer(
            &func_name_buf,
            @intCast(self.config.init_func.len),
        );
        defer c.WasmEdge_StringDelete(func_name);

        const result = c.WasmEdge_VMExecute(vm, func_name, null, 0, null, 0);
        if (!c.WasmEdge_ResultOK(result)) {
            std.debug.print("[wizer] Init function failed: {s}\n", .{c.WasmEdge_ResultGetMessage(result)});
            return error.InitFailed;
        }
    }

    fn snapshotState(self: *Wizer, vm: *c.WasmEdge_VMContext) !Snapshot {
        var snapshot = Snapshot.init(self.allocator);
        errdefer snapshot.deinit();

        // Get the module instance
        const store = c.WasmEdge_VMGetStoreContext(vm);
        if (store == null) {
            return error.NoStore;
        }

        // Snapshot memory
        const mem_name = c.WasmEdge_StringCreateByCString("memory");
        defer c.WasmEdge_StringDelete(mem_name);

        // Find the active module
        var mod_names: [16]c.WasmEdge_String = undefined;
        const mod_count = c.WasmEdge_StoreListModule(store, &mod_names, 16);

        if (mod_count > 0) {
            // Get the first registered module (usually "")
            const module = c.WasmEdge_StoreFindModule(store, mod_names[0]);
            if (module != null) {
                const mem = c.WasmEdge_ModuleInstanceFindMemory(module, mem_name);
                if (mem != null) {
                    const mem_snap = try self.snapshotMemory(mem.?);
                    try snapshot.memories.append(self.allocator, mem_snap);
                }
            }
        }

        return snapshot;
    }

    /// Snapshot memory using Wizer's algorithm:
    /// 1. Scan each page in parallel to find non-zero regions
    /// 2. Merge adjacent segments with small gaps (< 4 bytes)
    /// 3. If too many segments, merge smallest gaps first
    fn snapshotMemory(self: *Wizer, mem: *c.WasmEdge_MemoryInstanceContext) !MemorySnapshot {
        const page_size: usize = 65536; // 64KB per WASM page
        const num_pages = c.WasmEdge_MemoryInstanceGetPageSize(mem);
        const mem_size = num_pages * page_size;

        std.debug.print("[wizer] Memory: {d} pages ({d} KB)\n", .{ num_pages, mem_size / 1024 });

        var result = MemorySnapshot{
            .min_pages = num_pages,
            .segments = .{},
        };
        errdefer result.deinit(self.allocator);

        if (mem_size == 0) {
            return result;
        }

        // Get pointer to entire memory
        const mem_ptr = c.WasmEdge_MemoryInstanceGetPointer(mem, 0, @intCast(mem_size));
        if (mem_ptr == null) {
            return result;
        }

        const memory = mem_ptr[0..mem_size];

        // Temporary list of segment ranges (before data copy)
        var ranges = std.ArrayListUnmanaged(Range){};
        defer ranges.deinit(self.allocator);

        // Step 1: Find non-zero regions per page (Wizer does this in parallel)
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

            // Merge if same memory and gap is small
            const gap = b.start - a.end;
            if (gap <= MIN_ACTIVE_SEGMENT_OVERHEAD) {
                a.end = b.end;
            } else {
                try merged.append(self.allocator, b);
            }
        }

        // Step 4: If too many segments, merge smallest gaps first
        if (merged.items.len > MAX_DATA_SEGMENTS) {
            try self.removeExcessSegments(self.allocator, &merged);
        }

        // Step 5: Copy actual data for final segments
        for (merged.items) |range| {
            const data = try self.allocator.dupe(u8, memory[range.start..range.end]);
            try result.segments.append(self.allocator, .{
                .memory_index = 0,
                .offset = range.start,
                .data = data,
                .is64 = false,
            });
        }

        std.debug.print("[wizer] Found {d} data segments\n", .{result.segments.items.len});
        return result;
    }

    /// Merge segments with smallest gaps to get under MAX_DATA_SEGMENTS limit
    const Range = struct { start: usize, end: usize };

    fn removeExcessSegments(_: *Wizer, allocator: Allocator, segments: *std.ArrayListUnmanaged(Range)) !void {
        const excess = segments.items.len - MAX_DATA_SEGMENTS;
        if (excess == 0) return;

        // Find smallest gaps
        const GapIndex = struct { gap: u32, index: u32 };
        var gaps = std.ArrayListUnmanaged(GapIndex){};
        defer gaps.deinit(allocator);

        for (segments.items[0 .. segments.items.len - 1], 0..) |seg, idx| {
            const next = segments.items[idx + 1];
            const gap = next.start - seg.end;
            if (gap <= std.math.maxInt(u32)) {
                try gaps.append(allocator, .{
                    .gap = @intCast(gap),
                    .index = @intCast(idx),
                });
            }
        }

        // Sort by gap size (ascending)
        std.mem.sort(GapIndex, gaps.items, {}, struct {
            fn lessThan(_: void, a: GapIndex, b: GapIndex) bool {
                return a.gap < b.gap;
            }
        }.lessThan);

        // Take smallest gaps and merge
        const to_merge = @min(excess, gaps.items.len);
        var to_remove = std.ArrayListUnmanaged(usize){};
        defer to_remove.deinit(allocator);

        for (gaps.items[0..to_merge]) |gap_info| {
            const idx = gap_info.index;
            // Merge segment[idx] with segment[idx+1]
            segments.items[idx].end = segments.items[idx + 1].end;
            try to_remove.append(allocator, idx + 1);
        }

        // Sort indices to remove in descending order
        std.mem.sort(usize, to_remove.items, {}, struct {
            fn lessThan(_: void, a: usize, b: usize) bool {
                return a > b;
            }
        }.lessThan);

        // Remove merged segments (in reverse order to preserve indices)
        for (to_remove.items) |idx| {
            _ = segments.orderedRemove(idx);
        }
    }

    /// Rewrite the WASM binary with snapshotted state
    pub fn rewrite(self: *Wizer, original: []const u8, snapshot: *const Snapshot) ![]u8 {
        var output = std.ArrayListUnmanaged(u8){};
        errdefer output.deinit(self.allocator);

        // Validate magic number and version
        if (original.len < 8) return error.InvalidWasm;
        if (!std.mem.eql(u8, original[0..4], "\x00asm")) return error.InvalidWasm;

        // Copy magic and version
        try output.appendSlice(self.allocator, original[0..8]);

        var pos: usize = 8;
        var added_data_section = false;
        var data_count_added = false;

        while (pos < original.len) {
            const section_id = original[pos];
            pos += 1;

            const section_len = try readLEB128u32(original, &pos);
            const section_data = original[pos .. pos + section_len];
            const section_end = pos + section_len;

            switch (@as(SectionId, @enumFromInt(section_id))) {
                .memory => {
                    // Rewrite memory section with new minimum sizes
                    try self.rewriteMemorySection(&output, section_data, snapshot);
                },
                .global => {
                    // Rewrite globals with snapshotted values
                    try self.rewriteGlobalSection(&output, section_data, snapshot);
                },
                .@"export" => {
                    // Remove wizer_init export
                    try self.rewriteExportSection(&output, section_data);
                },
                .start => {
                    // Skip start section - already ran
                },
                .data_count => {
                    // Update data count
                    const mem_segments = if (snapshot.memories.items.len > 0)
                        snapshot.memories.items[0].segments.items.len
                    else
                        0;
                    try self.writeDataCountSection(&output, @intCast(mem_segments));
                    data_count_added = true;
                },
                .data => {
                    // Replace with snapshotted data
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
        _ = section_data;

        var section = std.ArrayListUnmanaged(u8){};
        defer section.deinit(self.allocator);

        // Write number of memories
        try writeLEB128u32(self.allocator, &section, @intCast(snapshot.memories.items.len));

        for (snapshot.memories.items) |mem| {
            // Memory type: limits
            // flags: 0 = min only, 1 = min and max
            try section.append(self.allocator, 0x00); // flags: just min
            try writeLEB128u32(self.allocator, &section, @intCast(mem.min_pages));
        }

        // Write section header
        try output.append(self.allocator, @intFromEnum(SectionId.memory));
        try writeLEB128u32(self.allocator, output, @intCast(section.items.len));
        try output.appendSlice(self.allocator, section.items);
    }

    fn rewriteGlobalSection(self: *Wizer, output: *std.ArrayListUnmanaged(u8), section_data: []const u8, _: *const Snapshot) !void {
        // For now, just copy globals as-is
        // TODO: Update mutable globals with snapshotted values
        // (Wizer updates mutable globals with their snapshotted values,
        // but for simplicity we copy as-is since memory is the main state)
        try output.append(self.allocator, @intFromEnum(SectionId.global));
        try writeLEB128u32(self.allocator, output, @intCast(section_data.len));
        try output.appendSlice(self.allocator, section_data);
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
            const name = section_data[pos .. pos + name_len];
            pos += name_len;

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

        if (snapshot.memories.items.len == 0) {
            try writeLEB128u32(self.allocator, &section, 0);
        } else {
            const segments = snapshot.memories.items[0].segments.items;
            try writeLEB128u32(self.allocator, &section, @intCast(segments.len));

            for (segments) |seg| {
                // Active segment: 0x00 = memory 0 with i32 offset
                try section.append(self.allocator, 0x00);

                // Offset expression: i32.const <offset> end
                try section.append(self.allocator, 0x41); // i32.const
                try writeLEB128i32(self.allocator, &section, @intCast(seg.offset));
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

// LEB128 encoding/decoding helpers
fn readLEB128u32(data: []const u8, pos: *usize) !u32 {
    var result: u32 = 0;
    var shift: u5 = 0;

    while (true) {
        if (pos.* >= data.len) return error.UnexpectedEof;
        const byte = data[pos.*];
        pos.* += 1;

        result |= @as(u32, byte & 0x7f) << shift;
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

        // Check if we're done
        const sign_bit = (byte & 0x40) != 0;
        if ((v == 0 and !sign_bit) or (v == -1 and sign_bit)) {
            try output.append(allocator, byte);
            break;
        } else {
            try output.append(allocator, byte | 0x80);
        }
    }
}

/// Run wizer snapshot from command line args
/// Called by edgeboxc snapshot command
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

// Standalone CLI entry point (for zig build wizer)
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
