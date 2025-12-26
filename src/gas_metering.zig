// Gas Metering for WASM AOT Execution
//
// Instruments WASM modules with gas counting at basic block boundaries.
// Uses the "mutable global" approach for best performance (~2-5% overhead).
//
// Based on:
// - https://github.com/paritytech/wasm-instrument
// - https://agryaznov.com/posts/wasm-gas-metering/
//
// How it works:
// 1. Add exported mutable global `$__gas_left` (i64)
// 2. At each basic block entry, inject gas deduction + check
// 3. Trap with `unreachable` if gas exhausted
//
// Basic Block Boundaries:
// - Function entry
// - After `loop` (loop body is a new block)
// - After `if` and `else` (branches are new blocks)
// - After `block` (block body is a new block)
//
// The host sets initial gas via the exported global before calling WASM.

const std = @import("std");

/// Cost schedule for WASM instructions (in gas units)
/// Based on benchmarks - these are relative costs
pub const InstructionCosts = struct {
    // Control flow
    pub const block: u64 = 0; // Free - just structure
    pub const loop_op: u64 = 0; // Free - cost is in the body
    pub const if_op: u64 = 1;
    pub const br: u64 = 2;
    pub const br_if: u64 = 2;
    pub const br_table: u64 = 3;
    pub const call: u64 = 10;
    pub const call_indirect: u64 = 15;
    pub const return_op: u64 = 1;
    pub const unreachable_op: u64 = 1;

    // Constants
    pub const const_op: u64 = 1;

    // Local/Global
    pub const local_get: u64 = 1;
    pub const local_set: u64 = 1;
    pub const local_tee: u64 = 1;
    pub const global_get: u64 = 2;
    pub const global_set: u64 = 2;

    // Memory
    pub const load: u64 = 3;
    pub const store: u64 = 3;
    pub const memory_size: u64 = 2;
    pub const memory_grow: u64 = 1000; // Expensive!

    // Numeric - i32
    pub const i32_binop: u64 = 1; // add, sub, mul, and, or, xor, shl, shr
    pub const i32_divmod: u64 = 5; // div, rem
    pub const i32_cmp: u64 = 1; // eq, ne, lt, gt, le, ge
    pub const i32_unop: u64 = 1; // clz, ctz, popcnt, eqz

    // Numeric - i64
    pub const i64_binop: u64 = 1;
    pub const i64_divmod: u64 = 8;
    pub const i64_cmp: u64 = 1;
    pub const i64_unop: u64 = 1;

    // Numeric - f32/f64
    pub const f32_binop: u64 = 2;
    pub const f64_binop: u64 = 2;
    pub const f32_unop: u64 = 2;
    pub const f64_unop: u64 = 2;

    // Conversions
    pub const conversion: u64 = 2;

    // SIMD (v128)
    pub const simd_op: u64 = 3;

    // Bulk memory
    pub const memory_copy: u64 = 3; // Per byte cost added separately
    pub const memory_fill: u64 = 2;
    pub const memory_init: u64 = 3;
    pub const data_drop: u64 = 1;
    pub const table_copy: u64 = 3;
    pub const table_init: u64 = 3;
    pub const elem_drop: u64 = 1;
    pub const table_get: u64 = 2;
    pub const table_set: u64 = 2;
    pub const table_grow: u64 = 100;
    pub const table_size: u64 = 1;
    pub const table_fill: u64 = 2;

    // Default for unknown opcodes
    pub const default: u64 = 1;

    /// Get cost for an opcode
    pub fn getCost(opcode: u8, second_byte: ?u8) u64 {
        return switch (opcode) {
            // Control flow
            0x00 => unreachable_op, // unreachable
            0x01 => 0, // nop
            0x02 => block, // block
            0x03 => loop_op, // loop
            0x04 => if_op, // if
            0x05 => 0, // else (structure)
            0x0B => 0, // end (structure)
            0x0C => br, // br
            0x0D => br_if, // br_if
            0x0E => br_table, // br_table
            0x0F => return_op, // return
            0x10 => call, // call
            0x11 => call_indirect, // call_indirect

            // Reference types
            0xD0 => const_op, // ref.null
            0xD1 => i32_cmp, // ref.is_null
            0xD2 => const_op, // ref.func

            // Parametric instructions
            0x1A => 1, // drop
            0x1B => 1, // select
            0x1C => 1, // select t

            // Variable access
            0x20 => local_get, // local.get
            0x21 => local_set, // local.set
            0x22 => local_tee, // local.tee
            0x23 => global_get, // global.get
            0x24 => global_set, // global.set

            // Table operations (0x25, 0x26)
            0x25 => table_get,
            0x26 => table_set,

            // Memory operations
            0x28...0x3E => load, // i32.load through i64.store32
            0x3F => memory_size, // memory.size
            0x40 => memory_grow, // memory.grow

            // Constants
            0x41 => const_op, // i32.const
            0x42 => const_op, // i64.const
            0x43 => const_op, // f32.const
            0x44 => const_op, // f64.const

            // i32 comparisons
            0x45 => i32_unop, // i32.eqz
            0x46...0x4F => i32_cmp, // i32.eq through i32.ge_u

            // i64 comparisons
            0x50 => i64_unop, // i64.eqz
            0x51...0x5A => i64_cmp, // i64.eq through i64.ge_u

            // f32 comparisons
            0x5B...0x60 => i32_cmp, // f32.eq through f32.ge

            // f64 comparisons
            0x61...0x66 => i64_cmp, // f64.eq through f64.ge

            // i32 arithmetic
            0x67...0x69 => i32_unop, // i32.clz, ctz, popcnt
            0x6A...0x6C => i32_binop, // i32.add, sub, mul
            0x6D...0x70 => i32_divmod, // i32.div_s through i32.rem_u
            0x71...0x78 => i32_binop, // i32.and through i32.rotr

            // i64 arithmetic
            0x79...0x7B => i64_unop, // i64.clz, ctz, popcnt
            0x7C...0x7E => i64_binop, // i64.add, sub, mul
            0x7F...0x82 => i64_divmod, // i64.div_s through i64.rem_u
            0x83...0x8A => i64_binop, // i64.and through i64.rotr

            // f32 arithmetic
            0x8B...0x98 => f32_binop, // f32.abs through f32.copysign

            // f64 arithmetic
            0x99...0xA6 => f64_binop, // f64.abs through f64.copysign

            // Conversions
            0xA7...0xBF => conversion, // i32.wrap_i64 through f64.reinterpret_i64

            // Extended opcodes (0xFC prefix)
            0xFC => getExtendedCost(second_byte),

            // SIMD (0xFD prefix)
            0xFD => simd_op,

            else => default,
        };
    }

    fn getExtendedCost(second_byte: ?u8) u64 {
        const byte = second_byte orelse return default;
        return switch (byte) {
            0x00...0x07 => conversion, // i32.trunc_sat_* through i64.trunc_sat_*
            0x08 => memory_init, // memory.init
            0x09 => data_drop, // data.drop
            0x0A => memory_copy, // memory.copy
            0x0B => memory_fill, // memory.fill
            0x0C => table_init, // table.init
            0x0D => elem_drop, // elem.drop
            0x0E => table_copy, // table.copy
            0x0F => table_grow, // table.grow
            0x10 => table_size, // table.size
            0x11 => table_fill, // table.fill
            else => default,
        };
    }
};

/// Gas metering configuration
pub const GasMeteringConfig = struct {
    /// Name of the exported gas global
    gas_global_name: []const u8 = "__gas_left",
    /// Whether to trap on gas exhaustion (vs just setting to negative)
    trap_on_exhaustion: bool = true,
    /// Cost multiplier (for adjusting granularity)
    cost_multiplier: u64 = 1,
};

/// Result of gas metering instrumentation
pub const MeteringResult = struct {
    /// Instrumented WASM binary
    binary: []u8,
    /// Original size
    original_size: usize,
    /// Instrumented size
    instrumented_size: usize,
    /// Number of metering points injected
    metering_points: usize,
    /// Estimated gas cost per full execution (sum of all block costs)
    estimated_total_cost: u64,
};

/// Low-level WASM instrumentation using pure Zig
/// This parses and modifies WASM bytecode directly for precise control
pub const WasmInstrumenter = struct {
    allocator: std.mem.Allocator,
    wasm: []const u8,
    output: std.ArrayListUnmanaged(u8) = .{},

    // Parsed sections
    type_section: ?SectionInfo = null,
    import_section: ?SectionInfo = null,
    function_section: ?SectionInfo = null,
    table_section: ?SectionInfo = null,
    memory_section: ?SectionInfo = null,
    global_section: ?SectionInfo = null,
    export_section: ?SectionInfo = null,
    start_section: ?SectionInfo = null,
    element_section: ?SectionInfo = null,
    code_section: ?SectionInfo = null,
    data_section: ?SectionInfo = null,
    data_count_section: ?SectionInfo = null,

    // All sections in order for reconstruction
    sections: std.ArrayListUnmanaged(SectionEntry) = .{},

    // Counters
    num_imported_globals: u32 = 0,
    gas_global_index: u32 = 0,
    num_existing_globals: u32 = 0,

    const SectionInfo = struct {
        start: usize, // Start of section data (after section header)
        len: usize, // Length of section data
    };

    const SectionEntry = struct {
        id: u8,
        header_start: usize, // Start of section (including id byte)
        data_start: usize, // Start of section data
        data_len: usize, // Length of section data
    };

    const Self = @This();

    pub fn init(allocator: std.mem.Allocator, wasm: []const u8) Self {
        return Self{
            .allocator = allocator,
            .wasm = wasm,
        };
    }

    pub fn deinit(self: *Self) void {
        self.output.deinit(self.allocator);
        self.sections.deinit(self.allocator);
    }

    /// Parse WASM sections
    pub fn parse(self: *Self) !void {
        if (self.wasm.len < 8) return error.InvalidWasm;

        // Check magic number and version
        if (!std.mem.eql(u8, self.wasm[0..4], "\x00asm")) {
            return error.InvalidMagic;
        }
        if (!std.mem.eql(u8, self.wasm[4..8], &[_]u8{ 0x01, 0x00, 0x00, 0x00 })) {
            return error.UnsupportedVersion;
        }

        var pos: usize = 8;
        while (pos < self.wasm.len) {
            const header_start = pos;
            const section_id = self.wasm[pos];
            pos += 1;

            const section_size = try readLeb128u32(self.wasm, &pos);
            const data_start = pos;
            const section_end = pos + section_size;

            // Store section entry
            try self.sections.append(self.allocator, .{
                .id = section_id,
                .header_start = header_start,
                .data_start = data_start,
                .data_len = section_size,
            });

            const section_info = SectionInfo{ .start = data_start, .len = section_size };

            switch (section_id) {
                1 => self.type_section = section_info,
                2 => {
                    self.import_section = section_info;
                    // Count imported globals
                    self.num_imported_globals = try countImportedGlobals(self.wasm[data_start..section_end]);
                },
                3 => self.function_section = section_info,
                4 => self.table_section = section_info,
                5 => self.memory_section = section_info,
                6 => {
                    self.global_section = section_info;
                    // Count existing globals
                    var gpos: usize = 0;
                    self.num_existing_globals = try readLeb128u32(self.wasm[data_start..section_end], &gpos);
                },
                7 => self.export_section = section_info,
                8 => self.start_section = section_info,
                9 => self.element_section = section_info,
                10 => self.code_section = section_info,
                11 => self.data_section = section_info,
                12 => self.data_count_section = section_info,
                else => {},
            }

            pos = section_end;
        }
    }

    /// Instrument the WASM module with gas metering
    pub fn instrument(self: *Self, config: GasMeteringConfig) !MeteringResult {
        try self.parse();

        // The gas global will be added after imported globals
        // It will be the first defined global (before any existing defined globals)
        self.gas_global_index = self.num_imported_globals;

        // Write WASM header
        try self.output.appendSlice(self.allocator, "\x00asm");
        try self.output.appendSlice(self.allocator, &[_]u8{ 0x01, 0x00, 0x00, 0x00 });

        var metering_points: usize = 0;
        var estimated_total_cost: u64 = 0;

        var global_section_written = false;
        var export_section_written = false;

        // Process each section in order
        for (self.sections.items) |section| {
            switch (section.id) {
                6 => {
                    // Global section - inject our gas global at the beginning
                    try self.writeGlobalSectionWithGas(
                        self.wasm[section.data_start .. section.data_start + section.data_len],
                        config,
                    );
                    global_section_written = true;
                },
                7 => {
                    // Export section - add gas global export
                    try self.writeExportSectionWithGas(
                        self.wasm[section.data_start .. section.data_start + section.data_len],
                        config,
                    );
                    export_section_written = true;
                },
                10 => {
                    // Code section - instrument functions
                    // First, ensure global section exists (insert before code if missing)
                    if (!global_section_written) {
                        try self.writeNewGlobalSection(config);
                        global_section_written = true;
                    }
                    if (!export_section_written) {
                        try self.writeNewExportSection(config);
                        export_section_written = true;
                    }

                    const result = try self.writeCodeSectionWithMetering(
                        self.wasm[section.data_start .. section.data_start + section.data_len],
                        config,
                    );
                    metering_points = result.metering_points;
                    estimated_total_cost = result.estimated_cost;
                },
                else => {
                    // Copy section as-is
                    const full_section = self.wasm[section.header_start .. section.data_start + section.data_len];
                    try self.output.appendSlice(self.allocator, full_section);
                },
            }
        }

        const binary = try self.allocator.dupe(u8, self.output.items);

        return MeteringResult{
            .binary = binary,
            .original_size = self.wasm.len,
            .instrumented_size = binary.len,
            .metering_points = metering_points,
            .estimated_total_cost = estimated_total_cost,
        };
    }

    fn writeNewGlobalSection(self: *Self, config: GasMeteringConfig) !void {
        _ = config;

        var new_section = std.ArrayListUnmanaged(u8){};
        defer new_section.deinit(self.allocator);

        // 1 global
        try writeLeb128u32(self.allocator, &new_section, 1);

        // Gas global: (global (mut i64) (i64.const 0))
        try new_section.append(self.allocator, 0x7E); // i64
        try new_section.append(self.allocator, 0x01); // mutable
        try new_section.append(self.allocator, 0x42); // i64.const
        try new_section.append(self.allocator, 0x00); // 0 (sleb128)
        try new_section.append(self.allocator, 0x0B); // end

        // Write section
        try self.output.append(self.allocator, 6); // Global section ID
        try writeLeb128u32(self.allocator, &self.output, @intCast(new_section.items.len));
        try self.output.appendSlice(self.allocator, new_section.items);
    }

    fn writeNewExportSection(self: *Self, config: GasMeteringConfig) !void {
        var new_section = std.ArrayListUnmanaged(u8){};
        defer new_section.deinit(self.allocator);

        // 1 export
        try writeLeb128u32(self.allocator, &new_section, 1);

        // Export gas global
        const name = config.gas_global_name;
        try writeLeb128u32(self.allocator, &new_section, @intCast(name.len));
        try new_section.appendSlice(self.allocator, name);
        try new_section.append(self.allocator, 0x03); // Global export kind
        try writeLeb128u32(self.allocator, &new_section, self.gas_global_index);

        // Write section
        try self.output.append(self.allocator, 7); // Export section ID
        try writeLeb128u32(self.allocator, &self.output, @intCast(new_section.items.len));
        try self.output.appendSlice(self.allocator, new_section.items);
    }

    fn writeGlobalSectionWithGas(self: *Self, section_data: []const u8, config: GasMeteringConfig) !void {
        _ = config;

        // Parse existing globals
        var pos: usize = 0;
        const num_globals = try readLeb128u32(section_data, &pos);

        // Create new section with gas global prepended
        var new_section = std.ArrayListUnmanaged(u8){};
        defer new_section.deinit(self.allocator);

        // Write new count (original + 1)
        try writeLeb128u32(self.allocator, &new_section, num_globals + 1);

        // Write gas global FIRST: (global (mut i64) (i64.const 0))
        try new_section.append(self.allocator, 0x7E); // i64
        try new_section.append(self.allocator, 0x01); // mutable
        try new_section.append(self.allocator, 0x42); // i64.const
        try new_section.append(self.allocator, 0x00); // 0 (sleb128)
        try new_section.append(self.allocator, 0x0B); // end

        // Copy existing globals
        try new_section.appendSlice(self.allocator, section_data[pos..]);

        // Write section
        try self.output.append(self.allocator, 6); // Global section ID
        try writeLeb128u32(self.allocator, &self.output, @intCast(new_section.items.len));
        try self.output.appendSlice(self.allocator, new_section.items);
    }

    fn writeExportSectionWithGas(self: *Self, section_data: []const u8, config: GasMeteringConfig) !void {
        var pos: usize = 0;
        const num_exports = try readLeb128u32(section_data, &pos);

        var new_section = std.ArrayListUnmanaged(u8){};
        defer new_section.deinit(self.allocator);

        // Write new count (original + 1)
        try writeLeb128u32(self.allocator, &new_section, num_exports + 1);

        // Write gas global export FIRST
        const name = config.gas_global_name;
        try writeLeb128u32(self.allocator, &new_section, @intCast(name.len));
        try new_section.appendSlice(self.allocator, name);
        try new_section.append(self.allocator, 0x03); // Global export kind
        try writeLeb128u32(self.allocator, &new_section, self.gas_global_index);

        // Copy existing exports, adjusting global indices
        // Need to parse and rewrite each export to fix global references
        var i: u32 = 0;
        while (i < num_exports) : (i += 1) {
            // Read name
            const name_len = try readLeb128u32(section_data, &pos);
            const export_name = section_data[pos .. pos + name_len];
            pos += name_len;

            // Read kind
            const kind = section_data[pos];
            pos += 1;

            // Read index
            const index = try readLeb128u32(section_data, &pos);

            // Write name
            try writeLeb128u32(self.allocator, &new_section, name_len);
            try new_section.appendSlice(self.allocator, export_name);

            // Write kind
            try new_section.append(self.allocator, kind);

            // Write index (adjust if it's a global reference)
            if (kind == 0x03) {
                // Global export - adjust index (add 1 because we inserted gas global)
                try writeLeb128u32(self.allocator, &new_section, index + 1);
            } else {
                try writeLeb128u32(self.allocator, &new_section, index);
            }
        }

        // Write section
        try self.output.append(self.allocator, 7); // Export section ID
        try writeLeb128u32(self.allocator, &self.output, @intCast(new_section.items.len));
        try self.output.appendSlice(self.allocator, new_section.items);
    }

    const CodeMeteringResult = struct {
        metering_points: usize,
        estimated_cost: u64,
    };

    fn writeCodeSectionWithMetering(self: *Self, section_data: []const u8, config: GasMeteringConfig) !CodeMeteringResult {
        var pos: usize = 0;
        const num_functions = try readLeb128u32(section_data, &pos);

        var new_section = std.ArrayListUnmanaged(u8){};
        defer new_section.deinit(self.allocator);

        try writeLeb128u32(self.allocator, &new_section, num_functions);

        var total_metering_points: usize = 0;
        var total_estimated_cost: u64 = 0;

        // Process each function
        var i: u32 = 0;
        while (i < num_functions) : (i += 1) {
            const func_size = try readLeb128u32(section_data, &pos);
            const func_end = pos + func_size;
            const func_body = section_data[pos..func_end];

            // Instrument this function
            const result = try self.instrumentFunction(func_body, config);
            defer self.allocator.free(result.instrumented_body);

            // Write instrumented function
            try writeLeb128u32(self.allocator, &new_section, @intCast(result.instrumented_body.len));
            try new_section.appendSlice(self.allocator, result.instrumented_body);

            total_metering_points += result.metering_points;
            total_estimated_cost += result.estimated_cost;

            pos = func_end;
        }

        // Write section
        try self.output.append(self.allocator, 10); // Code section ID
        try writeLeb128u32(self.allocator, &self.output, @intCast(new_section.items.len));
        try self.output.appendSlice(self.allocator, new_section.items);

        return CodeMeteringResult{
            .metering_points = total_metering_points,
            .estimated_cost = total_estimated_cost,
        };
    }

    const FunctionInstrumentResult = struct {
        instrumented_body: []u8,
        metering_points: usize,
        estimated_cost: u64,
    };

    /// Instrument a function with basic block level gas metering
    fn instrumentFunction(self: *Self, func_body: []const u8, config: GasMeteringConfig) !FunctionInstrumentResult {
        var output = std.ArrayListUnmanaged(u8){};
        errdefer output.deinit(self.allocator);

        var pos: usize = 0;

        // Parse locals declaration
        const num_local_decls = try readLeb128u32(func_body, &pos);

        // Skip past local declarations
        var j: u32 = 0;
        while (j < num_local_decls) : (j += 1) {
            _ = try readLeb128u32(func_body, &pos); // count
            pos += 1; // type
        }

        // Copy locals declaration as-is
        try output.appendSlice(self.allocator, func_body[0..pos]);

        const code_start = pos;
        var metering_points: usize = 0;
        var total_cost: u64 = 0;

        // First pass: analyze basic blocks and their costs
        const blocks = try self.analyzeBasicBlocks(func_body[code_start..], config);
        defer self.allocator.free(blocks);

        // Second pass: emit instrumented code
        pos = code_start;
        var block_index: usize = 0;
        var control_depth: u32 = 0;

        while (pos < func_body.len) {
            const opcode = func_body[pos];

            // Check if this is a block boundary
            const is_block_start = block_index < blocks.len and
                (pos - code_start) == blocks[block_index].start_offset;

            if (is_block_start) {
                const block_cost = blocks[block_index].cost * config.cost_multiplier;
                total_cost += block_cost;

                // Inject gas check at block entry
                try self.injectGasCheck(&output, block_cost, config);
                metering_points += 1;

                block_index += 1;
            }

            // Track control flow depth
            switch (opcode) {
                0x02, 0x03, 0x04 => control_depth += 1, // block, loop, if
                0x0B => {
                    if (control_depth > 0) {
                        control_depth -= 1;
                    } else {
                        // Function-final end - copy it and break
                        try output.append(self.allocator, opcode);
                        break;
                    }
                },
                else => {},
            }

            // Handle the opcode
            const opcode_end = try self.getInstructionEnd(func_body, pos);

            // For global.get/set, we need to adjust the global index
            if (opcode == 0x23 or opcode == 0x24) {
                // global.get or global.set
                try output.append(self.allocator, opcode);
                var idx_pos = pos + 1;
                const global_idx = try readLeb128u32(func_body, &idx_pos);
                // Adjust index if it's a defined global (not imported)
                if (global_idx >= self.num_imported_globals) {
                    try writeLeb128u32(self.allocator, &output, global_idx + 1);
                } else {
                    try writeLeb128u32(self.allocator, &output, global_idx);
                }
                pos = opcode_end;
            } else if (opcode != 0x0B) {
                // Copy instruction as-is (0x0B is handled above)
                try output.appendSlice(self.allocator, func_body[pos..opcode_end]);
                pos = opcode_end;
            } else {
                // 0x0B (end) for nested blocks - just copy and continue
                try output.append(self.allocator, opcode);
                pos = opcode_end;
            }
        }

        return FunctionInstrumentResult{
            .instrumented_body = try output.toOwnedSlice(self.allocator),
            .metering_points = metering_points,
            .estimated_cost = total_cost,
        };
    }

    /// Represents a basic block for gas metering
    const BasicBlock = struct {
        start_offset: usize, // Offset from code start
        cost: u64, // Accumulated cost of instructions in this block
    };

    /// Analyze function body to find basic blocks and their costs
    fn analyzeBasicBlocks(self: *Self, code: []const u8, config: GasMeteringConfig) ![]BasicBlock {
        _ = config;
        var blocks = std.ArrayListUnmanaged(BasicBlock){};
        errdefer blocks.deinit(self.allocator);

        // Function entry is always a basic block
        try blocks.append(self.allocator, .{ .start_offset = 0, .cost = 0 });

        var pos: usize = 0;
        var control_depth: u32 = 0;
        var current_cost: u64 = 0;

        while (pos < code.len) {
            const opcode = code[pos];

            // Get instruction cost
            const second_byte: ?u8 = if (pos + 1 < code.len) code[pos + 1] else null;
            const cost = InstructionCosts.getCost(opcode, second_byte);
            current_cost += cost;

            // Update the current block's cost
            if (blocks.items.len > 0) {
                blocks.items[blocks.items.len - 1].cost = current_cost;
            }

            // Find instruction end
            pos = try self.getInstructionEnd(code, pos);

            // Check for control flow instructions that create new blocks
            switch (opcode) {
                0x02 => {
                    // block - new block after the block instruction
                    control_depth += 1;
                    try blocks.append(self.allocator, .{ .start_offset = pos, .cost = 0 });
                    current_cost = 0;
                },
                0x03 => {
                    // loop - new block after the loop instruction
                    // Loop body is a new basic block (can be jumped to)
                    control_depth += 1;
                    try blocks.append(self.allocator, .{ .start_offset = pos, .cost = 0 });
                    current_cost = 0;
                },
                0x04 => {
                    // if - new block for the then branch
                    control_depth += 1;
                    try blocks.append(self.allocator, .{ .start_offset = pos, .cost = 0 });
                    current_cost = 0;
                },
                0x05 => {
                    // else - new block for the else branch
                    try blocks.append(self.allocator, .{ .start_offset = pos, .cost = 0 });
                    current_cost = 0;
                },
                0x0B => {
                    // end
                    if (control_depth > 0) {
                        control_depth -= 1;
                        // After end, start new block (continuation)
                        if (pos < code.len) {
                            try blocks.append(self.allocator, .{ .start_offset = pos, .cost = 0 });
                            current_cost = 0;
                        }
                    } else {
                        // Function end
                        break;
                    }
                },
                0x0C, 0x0D, 0x0E => {
                    // br, br_if, br_table - after these, start new block
                    if (pos < code.len and code[pos] != 0x0B) {
                        try blocks.append(self.allocator, .{ .start_offset = pos, .cost = 0 });
                        current_cost = 0;
                    }
                },
                0x0F => {
                    // return - after return, start new block (unreachable code)
                    if (pos < code.len and code[pos] != 0x0B) {
                        try blocks.append(self.allocator, .{ .start_offset = pos, .cost = 0 });
                        current_cost = 0;
                    }
                },
                else => {},
            }
        }

        // Filter out zero-cost blocks and merge consecutive blocks
        var result = std.ArrayListUnmanaged(BasicBlock){};
        errdefer result.deinit(self.allocator);

        for (blocks.items) |block| {
            if (block.cost > 0) {
                try result.append(self.allocator, block);
            } else if (result.items.len == 0) {
                // Always include the first block even if zero cost
                try result.append(self.allocator, block);
            }
        }

        blocks.deinit(self.allocator);
        return try result.toOwnedSlice(self.allocator);
    }

    /// Get the end position of an instruction (after all operands)
    fn getInstructionEnd(self: *Self, code: []const u8, pos: usize) !usize {
        _ = self;
        if (pos >= code.len) return error.UnexpectedEof;

        const opcode = code[pos];
        var end = pos + 1;

        switch (opcode) {
            // No operands
            0x00, 0x01, 0x05, 0x0B, 0x0F, 0x1A, 0x1B => {},

            // Block type (single byte or s33)
            0x02, 0x03, 0x04 => {
                // blocktype: either 0x40 (void) or a valtype or s33 for functype
                const blocktype = code[end];
                if (blocktype == 0x40 or (blocktype >= 0x7C and blocktype <= 0x7F)) {
                    end += 1; // Simple type
                } else {
                    // Could be s33 encoded type index
                    _ = try readLeb128s33(code, &end);
                }
            },

            // LEB128 u32 operand
            0x0C, 0x0D, 0x10, 0x20, 0x21, 0x22, 0x23, 0x24, 0x25, 0x26 => {
                _ = try readLeb128u32(code, &end);
            },

            // br_table: vec(label) + default label
            0x0E => {
                const count = try readLeb128u32(code, &end);
                var i: u32 = 0;
                while (i <= count) : (i += 1) {
                    _ = try readLeb128u32(code, &end);
                }
            },

            // call_indirect: typeidx + tableidx
            0x11 => {
                _ = try readLeb128u32(code, &end);
                _ = try readLeb128u32(code, &end);
            },

            // select with types
            0x1C => {
                const count = try readLeb128u32(code, &end);
                end += count; // Skip type bytes
            },

            // Memory instructions with memarg
            0x28...0x3E => {
                _ = try readLeb128u32(code, &end); // align
                _ = try readLeb128u32(code, &end); // offset
            },

            // memory.size, memory.grow
            0x3F, 0x40 => {
                end += 1; // Memory index (always 0x00)
            },

            // i32.const
            0x41 => {
                _ = try readLeb128s32(code, &end);
            },

            // i64.const
            0x42 => {
                _ = try readLeb128s64(code, &end);
            },

            // f32.const
            0x43 => {
                end += 4;
            },

            // f64.const
            0x44 => {
                end += 8;
            },

            // ref.null
            0xD0 => {
                end += 1; // reftype
            },

            // ref.func
            0xD2 => {
                _ = try readLeb128u32(code, &end);
            },

            // Extended opcodes (0xFC prefix)
            0xFC => {
                const ext_opcode = try readLeb128u32(code, &end);
                switch (ext_opcode) {
                    0x00...0x07 => {}, // saturating truncation (no extra operands after opcode)
                    0x08 => {
                        // memory.init
                        _ = try readLeb128u32(code, &end); // dataidx
                        end += 1; // memidx (always 0x00)
                    },
                    0x09 => {
                        // data.drop
                        _ = try readLeb128u32(code, &end);
                    },
                    0x0A => {
                        // memory.copy
                        end += 2; // src memidx, dst memidx
                    },
                    0x0B => {
                        // memory.fill
                        end += 1; // memidx
                    },
                    0x0C => {
                        // table.init
                        _ = try readLeb128u32(code, &end); // elemidx
                        _ = try readLeb128u32(code, &end); // tableidx
                    },
                    0x0D => {
                        // elem.drop
                        _ = try readLeb128u32(code, &end);
                    },
                    0x0E => {
                        // table.copy
                        _ = try readLeb128u32(code, &end); // dst tableidx
                        _ = try readLeb128u32(code, &end); // src tableidx
                    },
                    0x0F, 0x10, 0x11 => {
                        // table.grow, table.size, table.fill
                        _ = try readLeb128u32(code, &end); // tableidx
                    },
                    else => {},
                }
            },

            // SIMD (0xFD prefix)
            0xFD => {
                const simd_opcode = try readLeb128u32(code, &end);
                // Most SIMD instructions have memarg or lane operands
                if (simd_opcode <= 11) {
                    // v128.load variants - have memarg
                    _ = try readLeb128u32(code, &end); // align
                    _ = try readLeb128u32(code, &end); // offset
                } else if (simd_opcode >= 84 and simd_opcode <= 91) {
                    // v128.load*_lane - memarg + lane
                    _ = try readLeb128u32(code, &end);
                    _ = try readLeb128u32(code, &end);
                    end += 1; // lane
                } else if (simd_opcode == 12) {
                    // v128.const - 16 bytes
                    end += 16;
                } else if (simd_opcode == 13) {
                    // i8x16.shuffle - 16 lane indices
                    end += 16;
                } else if (simd_opcode >= 21 and simd_opcode <= 34) {
                    // extract/replace lane ops - 1 byte lane
                    end += 1;
                }
            },

            // Simple opcodes with no operands
            0x45...0xC4 => {},

            else => {}, // Unknown opcode, assume no operands
        }

        return end;
    }

    /// Inject gas check code
    fn injectGasCheck(self: *Self, output: *std.ArrayListUnmanaged(u8), cost: u64, config: GasMeteringConfig) !void {
        // global.get $gas_left
        try output.append(self.allocator, 0x23);
        try writeLeb128u32(self.allocator, output, self.gas_global_index);

        // i64.const <cost>
        try output.append(self.allocator, 0x42);
        try writeSleb128i64(self.allocator, output, @intCast(cost));

        // i64.sub
        try output.append(self.allocator, 0x7D);

        // global.set $gas_left
        try output.append(self.allocator, 0x24);
        try writeLeb128u32(self.allocator, output, self.gas_global_index);

        if (config.trap_on_exhaustion) {
            // global.get $gas_left
            try output.append(self.allocator, 0x23);
            try writeLeb128u32(self.allocator, output, self.gas_global_index);

            // i64.const 0
            try output.append(self.allocator, 0x42);
            try output.append(self.allocator, 0x00);

            // i64.lt_s
            try output.append(self.allocator, 0x53);

            // if (with empty type)
            try output.append(self.allocator, 0x04);
            try output.append(self.allocator, 0x40); // void block type

            // unreachable
            try output.append(self.allocator, 0x00);

            // end
            try output.append(self.allocator, 0x0B);
        }
    }
};

// LEB128 encoding/decoding helpers
fn readLeb128u32(data: []const u8, pos: *usize) !u32 {
    var result: u32 = 0;
    var shift: u5 = 0;
    while (true) {
        if (pos.* >= data.len) return error.UnexpectedEof;
        const byte = data[pos.*];
        pos.* += 1;
        result |= @as(u32, byte & 0x7F) << shift;
        if (byte & 0x80 == 0) break;
        shift +%= 7;
    }
    return result;
}

fn readLeb128s32(data: []const u8, pos: *usize) !i32 {
    var result: i32 = 0;
    var shift: u5 = 0;
    var byte: u8 = 0;
    while (true) {
        if (pos.* >= data.len) return error.UnexpectedEof;
        byte = data[pos.*];
        pos.* += 1;
        result |= @as(i32, @intCast(byte & 0x7F)) << shift;
        shift +%= 7;
        if (byte & 0x80 == 0) break;
    }
    // Sign extend if needed
    if (shift < 32 and (byte & 0x40) != 0) {
        result |= @as(i32, -1) << shift;
    }
    return result;
}

fn readLeb128s33(data: []const u8, pos: *usize) !i64 {
    var result: i64 = 0;
    var shift: u6 = 0;
    var byte: u8 = 0;
    while (true) {
        if (pos.* >= data.len) return error.UnexpectedEof;
        byte = data[pos.*];
        pos.* += 1;
        result |= @as(i64, @intCast(byte & 0x7F)) << shift;
        shift +%= 7;
        if (byte & 0x80 == 0) break;
    }
    if (shift < 64 and (byte & 0x40) != 0) {
        result |= @as(i64, -1) << shift;
    }
    return result;
}

fn readLeb128s64(data: []const u8, pos: *usize) !i64 {
    var result: i64 = 0;
    var shift: u6 = 0;
    var byte: u8 = 0;
    while (true) {
        if (pos.* >= data.len) return error.UnexpectedEof;
        byte = data[pos.*];
        pos.* += 1;
        result |= @as(i64, @intCast(byte & 0x7F)) << shift;
        shift +%= 7;
        if (byte & 0x80 == 0) break;
    }
    if (shift < 64 and (byte & 0x40) != 0) {
        result |= @as(i64, -1) << shift;
    }
    return result;
}

fn writeLeb128u32(allocator: std.mem.Allocator, output: *std.ArrayListUnmanaged(u8), value: u32) !void {
    var v = value;
    while (true) {
        const byte: u8 = @truncate(v & 0x7F);
        v >>= 7;
        if (v != 0) {
            try output.append(allocator, byte | 0x80);
        } else {
            try output.append(allocator, byte);
            break;
        }
    }
}

fn writeSleb128i64(allocator: std.mem.Allocator, output: *std.ArrayListUnmanaged(u8), value: i64) !void {
    var v = value;
    while (true) {
        const byte: u8 = @truncate(@as(u64, @bitCast(v)) & 0x7F);
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

fn countImportedGlobals(import_section: []const u8) !u32 {
    var pos: usize = 0;
    const num_imports = try readLeb128u32(import_section, &pos);

    var num_globals: u32 = 0;
    var i: u32 = 0;
    while (i < num_imports) : (i += 1) {
        // Skip module name
        const mod_len = try readLeb128u32(import_section, &pos);
        pos += mod_len;
        // Skip field name
        const field_len = try readLeb128u32(import_section, &pos);
        pos += field_len;
        // Import kind
        const kind = import_section[pos];
        pos += 1;
        switch (kind) {
            0x00 => { // func
                _ = try readLeb128u32(import_section, &pos);
            },
            0x01 => { // table
                pos += 1; // elem type
                const flags = import_section[pos];
                pos += 1;
                _ = try readLeb128u32(import_section, &pos); // initial
                if (flags & 1 != 0) {
                    _ = try readLeb128u32(import_section, &pos); // max
                }
            },
            0x02 => { // memory
                const flags = import_section[pos];
                pos += 1;
                _ = try readLeb128u32(import_section, &pos);
                if (flags & 1 != 0) {
                    _ = try readLeb128u32(import_section, &pos);
                }
            },
            0x03 => { // global
                num_globals += 1;
                pos += 1; // type
                pos += 1; // mutability
            },
            else => return error.InvalidImportKind,
        }
    }
    return num_globals;
}

/// CLI entry point for gas metering tool
pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    if (args.len < 3) {
        std.debug.print("Usage: edgebox-gas-meter <input.wasm> <output.wasm> [options]\n", .{});
        std.debug.print("\nOptions:\n", .{});
        std.debug.print("  --cost-multiplier <n>  Multiply all costs by n (default: 1)\n", .{});
        std.debug.print("  --no-trap              Don't trap on exhaustion, just go negative\n", .{});
        std.process.exit(1);
    }

    const input_path = args[1];
    const output_path = args[2];

    var config = GasMeteringConfig{};

    // Parse options
    var i: usize = 3;
    while (i < args.len) : (i += 1) {
        if (std.mem.eql(u8, args[i], "--cost-multiplier") and i + 1 < args.len) {
            config.cost_multiplier = try std.fmt.parseInt(u64, args[i + 1], 10);
            i += 1;
        } else if (std.mem.eql(u8, args[i], "--no-trap")) {
            config.trap_on_exhaustion = false;
        }
    }

    // Read input
    const input_file = try std.fs.cwd().openFile(input_path, .{});
    defer input_file.close();
    const wasm_input = try input_file.readToEndAlloc(allocator, 100 * 1024 * 1024);
    defer allocator.free(wasm_input);

    // Instrument
    var instrumenter = WasmInstrumenter.init(allocator, wasm_input);
    defer instrumenter.deinit();

    const result = try instrumenter.instrument(config);
    defer allocator.free(result.binary);

    // Write output
    const output_file = try std.fs.cwd().createFile(output_path, .{});
    defer output_file.close();
    try output_file.writeAll(result.binary);

    const overhead = @as(f64, @floatFromInt(result.instrumented_size - result.original_size)) /
        @as(f64, @floatFromInt(result.original_size)) * 100;

    std.debug.print("Gas metering instrumentation complete:\n", .{});
    std.debug.print("  Input:  {s} ({} bytes)\n", .{ input_path, result.original_size });
    std.debug.print("  Output: {s} ({} bytes, +{d:.1}%)\n", .{ output_path, result.instrumented_size, overhead });
    std.debug.print("  Metering points: {}\n", .{result.metering_points});
    std.debug.print("  Estimated total gas: {}\n", .{result.estimated_total_cost});
}

test "basic gas metering" {
    const allocator = std.testing.allocator;

    // Minimal valid WASM module with type, function, global, export, and code sections
    const wasm = &[_]u8{
        0x00, 0x61, 0x73, 0x6D, // magic
        0x01, 0x00, 0x00, 0x00, // version
        // Type section
        0x01, 0x04, 0x01, 0x60, 0x00, 0x00, // type section: 1 func type () -> ()
        // Function section
        0x03, 0x02, 0x01, 0x00, // function section: 1 func with type 0
        // Global section
        0x06, 0x06, 0x01, 0x7F, 0x01, 0x41, 0x00, 0x0B, // 1 mutable i32 global = 0
        // Export section
        0x07, 0x08, 0x01, 0x04, 0x74, 0x65, 0x73, 0x74, 0x00, 0x00, // export "test" func 0
        // Code section
        0x0A, 0x04, 0x01, 0x02, 0x00, 0x0B, // 1 func body: no locals, end
    };

    var instrumenter = WasmInstrumenter.init(allocator, wasm);
    defer instrumenter.deinit();

    const result = try instrumenter.instrument(.{});
    defer allocator.free(result.binary);

    try std.testing.expect(result.instrumented_size > result.original_size);
    try std.testing.expect(result.metering_points >= 1);
}

test "gas metering with control flow" {
    const allocator = std.testing.allocator;

    // WASM module with a loop
    // (func (loop (br 0)))
    const wasm = &[_]u8{
        0x00, 0x61, 0x73, 0x6D, // magic
        0x01, 0x00, 0x00, 0x00, // version
        // Type section
        0x01, 0x04, 0x01, 0x60, 0x00, 0x00, // () -> ()
        // Function section
        0x03, 0x02, 0x01, 0x00, // 1 func with type 0
        // Export section
        0x07, 0x08, 0x01, 0x04, 0x6d, 0x61, 0x69, 0x6e, 0x00, 0x00, // export "main" func 0
        // Code section
        0x0A, 0x09, 0x01, // 1 function, 7 bytes
        0x07, // func body size
        0x00, // no locals
        0x03, 0x40, // loop (void)
        0x0C, 0x00, // br 0
        0x0B, // end loop
        0x0B, // end func
    };

    var instrumenter = WasmInstrumenter.init(allocator, wasm);
    defer instrumenter.deinit();

    const result = try instrumenter.instrument(.{});
    defer allocator.free(result.binary);

    // Should have metering points for: function entry + loop body
    try std.testing.expect(result.metering_points >= 2);
}

test "instruction costs" {
    // Test that cost lookup works
    try std.testing.expectEqual(@as(u64, 10), InstructionCosts.getCost(0x10, null)); // call
    try std.testing.expectEqual(@as(u64, 1000), InstructionCosts.getCost(0x40, null)); // memory.grow
    try std.testing.expectEqual(@as(u64, 1), InstructionCosts.getCost(0x41, null)); // i32.const
    try std.testing.expectEqual(@as(u64, 5), InstructionCosts.getCost(0x6D, null)); // i32.div_s
}
