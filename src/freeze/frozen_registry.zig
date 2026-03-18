//! Frozen Function Registry
//!
//! Single ABI entry point for registering all frozen functions with QuickJS.
//! Generates native Zig code from QuickJS bytecode for machine-code speed execution.
//!
//! Usage:
//!   1. analyzeModule() - Parse bytecode and identify freezable functions
//!   2. generateModuleZig() - Generate native Zig implementations
//!   3. generateClosureManifest() - Generate closure variable manifest for hooks
//!
//! The generated Zig code is compiled into the binary for direct native execution,
//! replacing the interpreted bytecode with machine code.

const std = @import("std");
const jsvalue = @import("zig_jsvalue.zig");
pub const module_parser = @import("module_parser.zig");
const bytecode_parser = @import("bytecode_parser.zig");
const cfg_builder = @import("cfg_builder.zig");
const llvm_codegen = @import("llvm_codegen.zig");
const numeric_handlers = @import("numeric_handlers.zig");
const call_profile = @import("call_profile.zig");

const JSValue = jsvalue.JSValue;
const JSContext = jsvalue.JSContext;
const FrozenFn = jsvalue.FrozenFn;
const Allocator = std.mem.Allocator;

/// Look up a function's call count in the PGO profile map.
/// Key format matches call_profile.zig dump: "name@line_num"
/// Detect L2 cache size per core from /sys/devices/system/cpu.
/// Returns bytes, or 0 if detection fails. Falls back to 1.5MB default.
fn detectL2CacheSize() u64 {
    // Try Linux sysfs: /sys/devices/system/cpu/cpu0/cache/index2/size
    const f = std.fs.openFileAbsolute("/sys/devices/system/cpu/cpu0/cache/index2/size", .{}) catch return 1536 * 1024;
    defer f.close();
    var buf: [64]u8 = undefined;
    const len = f.read(&buf) catch return 1536 * 1024;
    const content = std.mem.trim(u8, buf[0..len], &[_]u8{ ' ', '\n', '\r', '\t' });
    if (content.len == 0) return 1536 * 1024;
    // Parse "1536K" or "2048K" format
    const last = content[content.len - 1];
    if (last == 'K' or last == 'k') {
        const n = std.fmt.parseInt(u64, content[0 .. content.len - 1], 10) catch return 1536 * 1024;
        return n * 1024;
    } else if (last == 'M' or last == 'm') {
        const n = std.fmt.parseInt(u64, content[0 .. content.len - 1], 10) catch return 1536 * 1024;
        return n * 1024 * 1024;
    }
    return std.fmt.parseInt(u64, content, 10) catch 1536 * 1024;
}

fn lookupProfile(map: *const std.StringHashMapUnmanaged(u64), name: []const u8, line_num: u32) u64 {
    var buf: [256]u8 = undefined;
    // Try exact match first (name@line_num)
    const key = std.fmt.bufPrint(&buf, "{s}@{d}", .{ name, line_num }) catch return 0;
    if (map.get(key)) |c| return c;
    // Fallback: profile often has line_num=0 (bundled JS strips line info)
    if (line_num != 0) {
        const key0 = std.fmt.bufPrint(&buf, "{s}@0", .{name}) catch return 0;
        if (map.get(key0)) |c| return c;
    }
    return 0;
}

/// Sanitize a name to be a valid Zig identifier
/// Replaces colons and other invalid characters with underscores
fn sanitizeName(name: []const u8, buf: []u8) []const u8 {
    var i: usize = 0;
    for (name) |c| {
        if (i >= buf.len) break;
        // Replace invalid Zig identifier chars with underscore
        // Includes: #@:-./ and any non-alphanumeric except _
        const is_valid = (c >= 'a' and c <= 'z') or (c >= 'A' and c <= 'Z') or
            (c >= '0' and c <= '9') or c == '_';
        buf[i] = if (is_valid) c else '_';
        i += 1;
    }
    return buf[0..i];
}

// ============================================================================
// Comptime Function Generation from Bytecode
// ============================================================================

// ============================================================================
// Runtime Module Generation API
// ============================================================================

/// Analyzed function ready for freezing
pub const AnalyzedFunction = struct {
    /// Function name (from atom table)
    name: []const u8,
    /// Raw bytecode
    bytecode: []const u8,
    /// Parsed instructions
    instructions: []const bytecode_parser.Instruction,
    /// Argument count
    arg_count: u32,
    /// Local variable count
    var_count: u32,
    /// Stack size (from bytecode, for sizing the operand stack array)
    stack_size: u32 = 256,
    /// Whether function calls itself recursively
    is_self_recursive: bool,
    /// Whether function can be frozen
    can_freeze: bool,
    /// Constants from bytecode (owned copy)
    constants: []const module_parser.ConstValue,
    /// Atom strings from module (shared reference to ModuleAnalysis.atom_strings)
    atom_strings: []const []const u8,
    /// Closure variables (owned copy of names and metadata)
    closure_vars: []const module_parser.ClosureVarInfo = &.{},
    /// Line number from debug info (for name@line_num dispatch key)
    line_num: u32 = 0,
    /// Parser index for index-based dispatch (position in parser.functions)
    /// Matches the JS_ReadFunctionTag counter during bytecode deserialization
    parser_index: u32 = 0,
};

/// Result of analyzing a module for freezable functions
pub const ModuleAnalysis = struct {
    allocator: Allocator,
    /// All functions found in module
    functions: std.ArrayListUnmanaged(AnalyzedFunction),
    /// Functions that can be frozen
    freezable_count: usize,
    /// Atom strings from module (owned copies)
    atom_strings: []const []const u8,

    pub fn deinit(self: *ModuleAnalysis) void {
        for (self.functions.items) |func| {
            self.allocator.free(func.instructions);
            // Free duplicated name (safe because we always dupe in analyzeModule)
            if (func.name.len > 0) {
                self.allocator.free(func.name);
            }
            // Free function's constants copy
            self.allocator.free(func.constants);
        }
        self.functions.deinit(self.allocator);

        // Free atom strings copies
        for (self.atom_strings) |atom| {
            self.allocator.free(atom);
        }
        self.allocator.free(self.atom_strings);
    }

    /// Get only the freezable functions
    pub fn getFreezable(self: *const ModuleAnalysis) []const AnalyzedFunction {
        var result = std.ArrayListUnmanaged(AnalyzedFunction){};
        for (self.functions.items) |func| {
            if (func.can_freeze) {
                result.append(self.allocator, func) catch continue;
            }
        }
        return result.items;
    }
};

/// Analyze a module bytecode and identify freezable functions
/// This is the main entry point for runtime freeze analysis
pub fn analyzeModule(
    allocator: Allocator,
    bytecode: []const u8,
) !ModuleAnalysis {
    var result = ModuleAnalysis{
        .allocator = allocator,
        .functions = .{},
        .freezable_count = 0,
        .atom_strings = &.{}, // Initialize empty, will be set after parser
    };
    errdefer result.deinit();

    // Parse module using existing module_parser
    var parser = module_parser.ModuleParser.init(allocator, bytecode);
    defer parser.deinit();
    try parser.parse();

    // COPY atom table before parser is freed
    const atom_strings_copy = try allocator.alloc([]const u8, parser.atom_strings.items.len);
    errdefer {
        for (atom_strings_copy, 0..) |atom, i| {
            if (i < atom_strings_copy.len) allocator.free(atom);
        }
        allocator.free(atom_strings_copy);
    }
    for (parser.atom_strings.items, 0..) |atom, i| {
        atom_strings_copy[i] = try allocator.dupe(u8, atom);
    }
    result.atom_strings = atom_strings_copy;

    // Analyze each function — only Phase 1 functions get valid parser_index values
    // Phase 2 (byte-scanned) functions use sentinel 0xFFFFFFFF (never matched at runtime)
    for (parser.functions.items, 0..) |func_info, parser_idx| {
        // Parse instructions - skip functions that fail to parse
        var bc_parser = bytecode_parser.BytecodeParser.init(func_info.bytecode);
        const instructions = bc_parser.parseAll(allocator) catch {
            // Some functions may have malformed bytecode (e.g., from scanning)
            // Skip them gracefully instead of failing the entire freeze
            continue;
        };

        // Check if can freeze
        const freeze_check = bytecode_parser.canFreezeFunction(instructions);

        // Self-recursion detection is DISABLED for now.
        // Get function name - must duplicate since parser will be freed
        // Use getAtomByIndex to properly look up built-in atoms like <eval>
        const parser_name = parser.getAtomByIndex(func_info.name_atom) orelse "anonymous";

        // Skip functions with invalid names (contains spaces, not valid C identifier)
        // These are often error messages that got captured as function names due to QuickJS quirks
        var has_invalid_char = false;
        for (parser_name) |c| {
            if (c == ' ' or c == '\'' or c == '"' or c == '(' or c == ')' or c == '{' or c == '}') {
                has_invalid_char = true;
                break;
            }
        }
        if (has_invalid_char) {
            // Skip this function - it has an invalid name
            continue;
        }

        const name = try allocator.dupe(u8, parser_name);

        // Detect self-recursion using closure analysis:
        // If a function has a closure variable with the same name as the function, it's self-recursive.
        // This is safe because QuickJS creates a closure var when a function references itself by name.
        // Example: function fib(n) { return fib(n-1) + fib(n-2); }
        // -> closureVars: [{"name": "fib", "var_idx": 0, "is_const": false}]
        var is_self_recursive = false;
        for (func_info.closure_vars) |cv| {
            if (std.mem.eql(u8, cv.name, parser_name)) {
                is_self_recursive = true;
                break;
            }
        }

        // Multi-arg tail recursion is supported - codegen saves all args to temps before reassigning
        const can_freeze_final = freeze_check.can_freeze;

        // COPY constants for this function
        const constants_copy = try allocator.alloc(module_parser.ConstValue, func_info.constants.len);
        for (func_info.constants, 0..) |const_val, i| {
            constants_copy[i] = const_val; // ConstValue should be copyable
        }

        // COPY closure vars (names need to be duped since parser will be freed)
        const closure_vars_copy = try allocator.alloc(module_parser.ClosureVarInfo, func_info.closure_vars.len);
        for (func_info.closure_vars, 0..) |cv, i| {
            closure_vars_copy[i] = .{
                .name = try allocator.dupe(u8, cv.name),
                .var_idx = cv.var_idx,
                .is_const = cv.is_const,
                .is_lexical = cv.is_lexical,
                .is_local = cv.is_local,
            };
        }

        try result.functions.append(allocator, .{
            .name = name,
            .bytecode = func_info.bytecode,
            .instructions = instructions,
            .arg_count = func_info.arg_count,
            .var_count = func_info.var_count,
            .stack_size = func_info.stack_size,
            .is_self_recursive = is_self_recursive,
            .can_freeze = can_freeze_final,
            .constants = constants_copy,
            .atom_strings = atom_strings_copy,
            .closure_vars = closure_vars_copy,
            .line_num = func_info.line_num,
            .parser_index = if (parser_idx < parser.phase1_count) @intCast(parser_idx) else 0xFFFFFFFF,
        });

        if (can_freeze_final) {
            result.freezable_count += 1;
        }
    }

    std.debug.print("[freeze] Module parser found {d} total functions (phase1: {d}, phase2: {d}), {d} analyzable\n", .{
        result.functions.items.len, parser.phase1_count, parser.functions.items.len - parser.phase1_count, result.freezable_count,
    });

    return result;
}

// Debug flag - set to true for verbose freeze logging
const FREEZE_DEBUG = false;

/// Quick scan for killer opcodes that prevent freezing
/// This avoids expensive CFG building for functions that will be skipped anyway
/// NOTE: Closure READ, WRITE, and CREATION are now supported via var_refs passed from native_dispatch
fn hasKillerOpcodes(instructions: []const bytecode_parser.Instruction) bool {
    for (instructions) |instr| {
        switch (instr.opcode) {
            // Invalid/unknown opcodes (byte values 248-255 or actual OP_invalid)
            .invalid => {
                if (FREEZE_DEBUG) std.debug.print("[freeze-zig] Unknown/invalid opcode at PC {d}\n", .{instr.pc});
                return true;
            },
            // NOTE: Closure READ (.get_var_ref*) is now ALLOWED - we pass var_refs from dispatch
            // NOTE: Closure WRITE (.put_var_ref*, .set_var_ref*) is now ALLOWED - we use setClosureVar
            // NOTE: Closure CREATION (.fclosure*) is now ALLOWED - we use js_frozen_create_closure
            // Eval/with - dynamic scope
            .eval, .with_get_var, .with_put_var, .with_delete_var,
            // Generators (yield not supported, but await IS supported via trampoline)
            .initial_yield, .yield, .yield_star,
            // Variable references (requires get_var_ref which needs JSStackFrame)
            .make_loc_ref, .make_arg_ref, .make_var_ref_ref, .make_var_ref,
            // put_ref_value is dependent on make_*_ref (pushes 2 values)
            .put_ref_value,
            // Other unsupported
            .import,
            => {
                if (FREEZE_DEBUG) std.debug.print("[freeze-zig] Killer opcode found: {s}\n", .{@tagName(instr.opcode)});
                return true;
            },
            else => {},
        }
    }
    return false;
}

// NOTE: generateFrozenZig() and generateFrozenZigGeneral() removed.
// LLVM backend (llvm_codegen.zig) is the only codegen path.

/// Sharded output for parallel compilation
pub const ShardedOutput = struct {
    /// Main file (frozen_module.zig) with init function and LLVM shard extern declarations
    main: []u8,
    /// Number of LLVM IR shards generated
    llvm_shard_count: usize = 0,

    pub fn deinit(self: *ShardedOutput, allocator: Allocator) void {
        allocator.free(self.main);
    }
};

/// Generate frozen module via LLVM IR backend.
/// All freezable functions are compiled as LLVM IR → .o shard files.
/// Returns a main frozen_module.zig with extern declarations for LLVM shard init functions.
pub fn generateModuleZigShardedWithBackend(
    allocator: Allocator,
    analysis: *const ModuleAnalysis,
    max_functions: u32,
    profile_path: ?[]const u8,
    output_dir: ?[]const u8,
    code_budget: u32,
    wasm_only: bool,
) !ShardedOutput {
    // Collect all freezable functions
    const GeneratedFunc = struct {
        func: AnalyzedFunction,
        idx: usize,
    };
    var generated_all = std.ArrayListUnmanaged(GeneratedFunc){};
    defer generated_all.deinit(allocator);
    var generated_all_full_len: usize = 0; // Before L2 truncation (for WASM scan)

    // Filter: skip functions with unsupported opcodes or unbuildable CFGs
    for (analysis.functions.items, 0..) |func, idx| {
        if (hasKillerOpcodes(func.instructions)) continue;
        // DEBUG: skip specific parser indices for diagnosis
        // DEBUG: no skip — re-enabled for diagnosis
        var cfg = cfg_builder.buildCFG(allocator, func.instructions) catch continue;
        cfg.deinit();
        try generated_all.append(allocator, .{ .func = func, .idx = idx });
    }

    // PGO: sort functions by profile hotness so top-N truncation keeps the hottest
    if (profile_path) |pp| {
        if (call_profile.parseProfileJson(allocator, pp)) |pm_val| {
            var pm = pm_val;
            defer {
                var it = pm.iterator();
                while (it.next()) |kv| allocator.free(kv.key_ptr.*);
                pm.deinit(allocator);
            }

            // Sort by descending call count (hottest first)
            const SortCtx = struct {
                map: *const std.StringHashMapUnmanaged(u64),
            };
            const ctx = SortCtx{ .map = &pm };
            std.mem.sort(GeneratedFunc, generated_all.items, ctx, struct {
                fn lessThan(c: SortCtx, a: GeneratedFunc, b: GeneratedFunc) bool {
                    const a_count = lookupProfile(c.map, a.func.name, a.func.line_num);
                    const b_count = lookupProfile(c.map, b.func.name, b.func.line_num);
                    return a_count > b_count; // descending: hottest first
                }
            }.lessThan);

            // Print PGO statistics
            var total_calls: u64 = 0;
            for (generated_all.items) |gf| {
                total_calls += lookupProfile(&pm, gf.func.name, gf.func.line_num);
            }

            const effective_limit = if (max_functions > 0 and generated_all.items.len > max_functions)
                max_functions
            else
                generated_all.items.len;

            var hot_calls: u64 = 0;
            for (generated_all.items[0..effective_limit]) |gf| {
                hot_calls += lookupProfile(&pm, gf.func.name, gf.func.line_num);
            }

            if (total_calls > 0) {
                const pct = @as(f64, @floatFromInt(hot_calls)) / @as(f64, @floatFromInt(total_calls)) * 100.0;
                std.debug.print("[freeze-pgo] Sorted {d} functions by profile hotness\n", .{generated_all.items.len});
                std.debug.print("[freeze-pgo] Top {d} functions cover {d:.1}% of total calls ({d} / {d})\n", .{ effective_limit, pct, hot_calls, total_calls });
                if (effective_limit > 0) {
                    const coldest = generated_all.items[effective_limit - 1];
                    const coldest_count = lookupProfile(&pm, coldest.func.name, coldest.func.line_num);
                    std.debug.print("[freeze-pgo] Coldest included: {s}@{d} ({d} calls)\n", .{ coldest.func.name, coldest.func.line_num, coldest_count });
                }
                if (generated_all.items.len > effective_limit) {
                    std.debug.print("[freeze-pgo] Skipping {d} cold functions (interpreter fallback)\n", .{ generated_all.items.len - effective_limit });
                }
            } else {
                std.debug.print("[freeze-pgo] Sorted {d} functions by profile (no matching call counts found)\n", .{generated_all.items.len});
            }
        }
    }

    // Selective freezing: limit by code budget or function count
    // Code budget auto-sizing: estimate native code size per function from bytecode instruction count
    // Empirical ratio: ~7 bytes of native code per bytecode instruction (from TSC measurements)
    const BYTES_PER_INSTRUCTION: u32 = 7;

    var effective_max = max_functions;
    // Sort by instruction count (largest first) when no PGO profile is provided.
    // This ensures that the L2 cache budget is filled with the most impactful functions.
    if (profile_path == null) {
        std.mem.sort(GeneratedFunc, generated_all.items, {}, struct {
            fn lessThan(_: void, a: GeneratedFunc, b: GeneratedFunc) bool {
                return a.func.instructions.len > b.func.instructions.len; // descending
            }
        }.lessThan);
    }

    if (effective_max == 0) {
        const budget = if (code_budget > 0) @as(u64, code_budget) else detectL2CacheSize();
        const label: []const u8 = if (code_budget > 0) "code budget" else "L2 cache";
        if (budget > 0) {
            var cumulative_size: u64 = 0;
            for (generated_all.items, 0..) |gf, i| {
                const est_size = @as(u64, gf.func.instructions.len) * BYTES_PER_INSTRUCTION;
                cumulative_size += est_size;
                if (cumulative_size > budget) {
                    effective_max = @intCast(i);
                    std.debug.print("[freeze-pgo] Auto-sized to {d} functions for {s} ({d}KB, {d}KB estimated code)\n", .{
                        effective_max, label, budget / 1024, (cumulative_size - est_size) / 1024,
                    });
                    break;
                }
            }
            if (effective_max == 0) {
                std.debug.print("[freeze-pgo] All {d} functions fit within {s} ({d}KB, {d}KB estimated)\n", .{
                    generated_all.items.len, label, budget / 1024, cumulative_size / 1024,
                });
            }
        }
    }

    if (effective_max > 0 and generated_all.items.len > effective_max) {
        const total_before = generated_all.items.len;
        // Print boundary function for debugging
        if (effective_max < total_before) {
            const boundary = generated_all.items[effective_max - 1];
            std.debug.print("[freeze] Boundary func #{d}: {s}@{d} ({d} instrs)\n", .{
                effective_max, boundary.func.name, boundary.func.line_num, boundary.func.instructions.len,
            });
        }
        // DEBUG: Count functions with specific opcodes
        {
            var insert2_count: usize = 0;
            var put_arg_count: usize = 0;
            var call_ctor_count: usize = 0;
            var nested_for_of_count: usize = 0;
            var array_from_count: usize = 0;
            var put_loc_check_count: usize = 0;
            for (generated_all.items[0..effective_max]) |gf| {
                var has_insert2 = false;
                var has_put_arg = false;
                var has_call_ctor = false;
                var for_of_depth: usize = 0;
                var max_for_of_depth: usize = 0;
                var has_array_from = false;
                var has_put_loc_check = false;
                for (gf.func.instructions) |instr| {
                    switch (instr.opcode) {
                        .insert2 => has_insert2 = true,
                        .put_arg0, .put_arg1, .put_arg2, .put_arg3, .put_arg => has_put_arg = true,
                        .call_constructor => has_call_ctor = true,
                        .for_of_start => { for_of_depth += 1; if (for_of_depth > max_for_of_depth) max_for_of_depth = for_of_depth; },
                        .iterator_close => { if (for_of_depth > 0) for_of_depth -= 1; },
                        .array_from => has_array_from = true,
                        .put_loc_check => has_put_loc_check = true,
                        else => {},
                    }
                }
                if (has_insert2) insert2_count += 1;
                if (has_put_arg) put_arg_count += 1;
                if (has_call_ctor) call_ctor_count += 1;
                if (max_for_of_depth >= 2) nested_for_of_count += 1;
                if (has_array_from) array_from_count += 1;
                if (has_put_loc_check) put_loc_check_count += 1;
            }
            std.debug.print("[freeze-stats] In first {d} funcs: insert2={d} put_arg={d} call_ctor={d} nested_for_of={d} array_from={d} put_loc_check={d}\n", .{
                effective_max, insert2_count, put_arg_count, call_ctor_count, nested_for_of_count, array_from_count, put_loc_check_count,
            });
        }
        generated_all_full_len = generated_all.items.len;
        generated_all.items.len = effective_max;
        std.debug.print("[freeze] Limited to {d} of {d} freezable functions\n", .{ effective_max, total_before });
    }

    // Generate int32 functions as LLVM IR .o files
    var llvm_shard_count: usize = 0;
    var int32_llvm_shard_count: usize = 0;
    {
        const cache_dir = output_dir orelse "zig-out/cache";

        // Collect int32 functions for LLVM IR generation
        var llvm_funcs = std.ArrayListUnmanaged(llvm_codegen.ShardFunction){};
        defer llvm_funcs.deinit(allocator);

        var llvm_func_names = std.ArrayListUnmanaged([]u8){};
        defer {
            for (llvm_func_names.items) |name| allocator.free(name);
            llvm_func_names.deinit(allocator);
        }

        // Build CFGs for int32 functions
        var llvm_cfgs = std.ArrayListUnmanaged(cfg_builder.CFG){};
        defer {
            for (llvm_cfgs.items) |*cfg_item| cfg_item.deinit();
            llvm_cfgs.deinit(allocator);
        }

        // Two-pass approach to avoid pointer invalidation:
        // Pass 1: Collect int32 function info and build CFGs
        const Int32FuncInfo = struct {
            gf_idx: usize,
            cfg_idx: usize,
            func_name_idx: usize,
        };
        var int32_infos = std.ArrayListUnmanaged(Int32FuncInfo){};
        defer int32_infos.deinit(allocator);

        for (generated_all.items, 0..) |gf, gi| {
            if (!isPureInt32Function(gf.func)) continue;

            // Build CFG for this function
            const cfg = cfg_builder.buildCFG(allocator, gf.func.instructions) catch continue;
            const cfg_idx = llvm_cfgs.items.len;
            try llvm_cfgs.append(allocator, cfg);

            // Sanitize name for LLVM symbol
            var sanitized_buf: [256]u8 = undefined;
            const sanitized = sanitizeName(gf.func.name, &sanitized_buf);
            const llvm_func_name = try std.fmt.allocPrint(allocator, "__frozen_{d}_{s}", .{ gf.idx, sanitized });
            const func_name_idx = llvm_func_names.items.len;
            try llvm_func_names.append(allocator, llvm_func_name);

            try int32_infos.append(allocator, .{
                .gf_idx = gi,
                .cfg_idx = cfg_idx,
                .func_name_idx = func_name_idx,
            });
        }

        // Pass 2: Build ShardFunction structs (safe — llvm_cfgs won't grow anymore)
        for (int32_infos.items) |info| {
            const gf = generated_all.items[info.gf_idx];
            try llvm_funcs.append(allocator, .{
                .name = gf.func.name,
                .func_index = gf.idx,
                .parser_index = gf.func.parser_index,
                .line_num = gf.func.line_num,
                .llvm_func_name = llvm_func_names.items[info.func_name_idx],
                .func = gf.func,
                .cfg = &llvm_cfgs.items[info.cfg_idx],
            });
        }

        if (llvm_funcs.items.len > 0) {
            std.debug.print("[freeze] LLVM IR: {d} int32 functions identified for compilation\n", .{llvm_funcs.items.len});
            const FUNCS_PER_SHARD = 100;
            var func_offset: usize = 0;

            while (func_offset < llvm_funcs.items.len) {
                const end = @min(func_offset + FUNCS_PER_SHARD, llvm_funcs.items.len);
                const shard_funcs = llvm_funcs.items[func_offset..end];

                var obj_path_buf: [4096]u8 = undefined;
                const obj_path = std.fmt.bufPrintZ(&obj_path_buf, "{s}/frozen_llvm_shard_{d}.o", .{ cache_dir, llvm_shard_count }) catch {
                    func_offset = end;
                    continue;
                };

                const result = llvm_codegen.generateShard(allocator, shard_funcs, llvm_shard_count, obj_path) catch |err| {
                    std.debug.print("[freeze] LLVM shard {d} failed: {}\n", .{ llvm_shard_count, err });
                    func_offset = end;
                    continue;
                };

                if (result.has_functions) {
                    std.debug.print("[freeze] LLVM shard {d}: {d} int32 functions → {s}\n", .{ llvm_shard_count, result.func_count, obj_path });
                    llvm_shard_count += 1;
                }

                func_offset = end;
            }

            if (llvm_shard_count > 0) {
                std.debug.print("[freeze] LLVM IR: generated {d} shard .o files with int32 functions\n", .{llvm_shard_count});
            }

            // Track int32 shard count (thin shards follow after these)
            int32_llvm_shard_count = llvm_shard_count;

            // NOTE: Int32 functions are NOT removed from generated_all.
            // They remain in Zig shards for cross-platform compatibility.
            // The LLVM .o files re-register the same dispatch keys with LLVM-compiled versions.
        }

        // Generate standalone WASM for numeric functions (i32 + f64 + array, no QuickJS runtime)
        // These can run directly in V8/workerd at near-native speed.
        // Runs independently of whether there are int32 shard functions.
        if (std.posix.getenv("EDGEBOX_WASM_DEBUG") != null) {
            std.debug.print("[wasm-scan] generated_all has {d} functions:\n", .{generated_all.items.len});
            for (generated_all.items) |gf| {
                std.debug.print("  [{d}] '{s}' (namelen={d}) args={d} vars={d}\n", .{ gf.idx, gf.func.name, gf.func.name.len, gf.func.arg_count, gf.func.var_count });
            }
        }
        {
                // Collect ALL numeric-tier functions (i32 already in llvm_funcs, plus f64)
                var wasm_funcs = std.ArrayListUnmanaged(llvm_codegen.ShardFunction){};
                defer wasm_funcs.deinit(allocator);

                // Include existing i32 functions
                for (llvm_funcs.items) |sf| {
                    try wasm_funcs.append(allocator, sf);
                }

                // Scan for f64-tier functions (have div or other float-introducing ops)
                var f64_cfgs = std.ArrayListUnmanaged(cfg_builder.CFG){};
                defer {
                    for (f64_cfgs.items) |*cfg_item| cfg_item.deinit();
                    f64_cfgs.deinit(allocator);
                }
                var f64_func_names = std.ArrayListUnmanaged([]u8){};
                defer {
                    for (f64_func_names.items) |name| allocator.free(name);
                    f64_func_names.deinit(allocator);
                }

                const F64Info = struct { gf_idx: usize, cfg_idx: usize, name_idx: usize };
                var f64_infos = std.ArrayListUnmanaged(F64Info){};
                defer f64_infos.deinit(allocator);

                var extra_i32_count: usize = 0;
                for (generated_all.items, 0..) |gf, gi| {
                    // Skip functions already in int32 shard
                    if (isPureInt32Function(gf.func)) continue;

                    // Check if it qualifies for any numeric tier
                    const tier = analyzeNumericTier(gf.func) orelse continue;

                    const cfg_val = cfg_builder.buildCFG(allocator, gf.func.instructions) catch continue;
                    const cfg_idx = f64_cfgs.items.len;
                    try f64_cfgs.append(allocator, cfg_val);

                    var sanitized_buf: [256]u8 = undefined;
                    const sanitized = sanitizeName(gf.func.name, &sanitized_buf);
                    const llvm_func_name = try std.fmt.allocPrint(allocator, "__frozen_{d}_{s}", .{ gf.idx, sanitized });
                    const name_idx = f64_func_names.items.len;
                    try f64_func_names.append(allocator, llvm_func_name);

                    try f64_infos.append(allocator, .{ .gf_idx = gi, .cfg_idx = cfg_idx, .name_idx = name_idx });

                    if (tier == .i32) extra_i32_count += 1;
                }

                // Build ShardFunctions (safe — f64_cfgs won't grow)
                for (f64_infos.items) |info| {
                    const gf = generated_all.items[info.gf_idx];
                    const tier = analyzeNumericTier(gf.func) orelse continue;
                    // Capture struct info if detected (must copy — static cache is overwritten)
                    var struct_layout_ptr: ?*const numeric_handlers.StructArgInfo = null;
                    if (getLastStructInfo()) |si| {
                        const copy = try allocator.create(numeric_handlers.StructArgInfo);
                        copy.* = si.*;
                        struct_layout_ptr = copy;
                    }
                    try wasm_funcs.append(allocator, .{
                        .name = gf.func.name,
                        .func_index = gf.idx,
                        .parser_index = gf.func.parser_index,
                        .line_num = gf.func.line_num,
                        .llvm_func_name = f64_func_names.items[info.name_idx],
                        .func = gf.func,
                        .cfg = &f64_cfgs.items[info.cfg_idx],
                        .value_kind = tier,
                        .struct_layout = struct_layout_ptr,
                    });
                }

                if (f64_infos.items.len > 0) {
                    const f64_count = f64_infos.items.len - extra_i32_count;
                    std.debug.print("[freeze] Numeric extra: {d} functions for WASM ({d} f64, {d} i32 with loops)\n", .{ f64_infos.items.len, f64_count, extra_i32_count });
                }

                // === Cross-function call pass ===
                // Functions that call other numeric WASM functions (e.g., countPrimes -> isPrime)
                // These were rejected by analyzeNumericTier (non-recursive with get_var+call)
                // but qualify when the callee is already in the WASM set.
                // NOTE: cross_cfgs/cross_names must outlive wasm_funcs (pointers stored in ShardFunction)
                var cross_cfgs = std.ArrayListUnmanaged(cfg_builder.CFG){};
                defer {
                    for (cross_cfgs.items) |*ci| ci.deinit();
                    cross_cfgs.deinit(allocator);
                }
                var cross_names = std.ArrayListUnmanaged([]u8){};
                defer {
                    for (cross_names.items) |cn| allocator.free(cn);
                    cross_names.deinit(allocator);
                }
                {
                    // Build set of existing WASM function names
                    var wasm_name_set = std.ArrayListUnmanaged([]const u8){};
                    defer wasm_name_set.deinit(allocator);
                    for (wasm_funcs.items) |sf| {
                        try wasm_name_set.append(allocator, sf.name);
                    }

                    const CrossInfo = struct { gf_idx: usize, cfg_idx: usize, name_idx: usize };
                    var cross_infos = std.ArrayListUnmanaged(CrossInfo){};
                    defer cross_infos.deinit(allocator);

                    const cross_debug = std.posix.getenv("EDGEBOX_WASM_DEBUG") != null;
                    for (generated_all.items, 0..) |gf, gi| {
                        // Skip functions already in WASM set
                        if (isPureInt32Function(gf.func)) continue;
                        if (analyzeNumericTier(gf.func) != null) continue;

                        if (cross_debug and gf.func.name.len > 0) std.debug.print("[wasm-cross-scan] {s}: checking cross-call (args={d}, vars={d})\n", .{ gf.func.name, gf.func.arg_count, gf.func.var_count });
                        // Check if it qualifies with cross-call context
                        _ = analyzeNumericTierWithCrossCall(gf.func, wasm_name_set.items) orelse continue;

                        const cfg_val = cfg_builder.buildCFG(allocator, gf.func.instructions) catch continue;
                        const cfg_idx = cross_cfgs.items.len;
                        try cross_cfgs.append(allocator, cfg_val);

                        var sanitized_buf: [256]u8 = undefined;
                        const sanitized = sanitizeName(gf.func.name, &sanitized_buf);
                        const llvm_func_name = try std.fmt.allocPrint(allocator, "__frozen_{d}_{s}", .{ gf.idx, sanitized });
                        const name_idx = cross_names.items.len;
                        try cross_names.append(allocator, llvm_func_name);

                        try cross_infos.append(allocator, .{ .gf_idx = gi, .cfg_idx = cfg_idx, .name_idx = name_idx });

                        // Immediately add to wasm_name_set so subsequent functions in THIS
                        // iteration can see it. Enables multi-level cross-call chains:
                        // validateVariant(leaf) → validateVariants → validateItem → validateOrder
                        try wasm_name_set.append(allocator, gf.func.name);
                    }

                    // Build ShardFunctions for cross-call candidates (safe — cross_cfgs won't grow)
                    for (cross_infos.items) |info| {
                        const gf = generated_all.items[info.gf_idx];
                        const tier = analyzeNumericTierWithCrossCall(gf.func, wasm_name_set.items) orelse continue;
                        try wasm_funcs.append(allocator, .{
                            .name = gf.func.name,
                            .func_index = gf.idx,
                            .parser_index = gf.func.parser_index,
                            .line_num = gf.func.line_num,
                            .llvm_func_name = cross_names.items[info.name_idx],
                            .func = gf.func,
                            .cfg = &cross_cfgs.items[info.cfg_idx],
                            .value_kind = tier,
                        });
                    }

                    if (cross_infos.items.len > 0) {
                        std.debug.print("[freeze] Cross-call: {d} functions added to WASM (call other numeric functions)\n", .{cross_infos.items.len});
                    }
                }

                // Apply WASM function limit for bisecting codegen bugs
                if (std.posix.getenv("EDGEBOX_WASM_LIMIT")) |lim| {
                    const max_funcs = std.fmt.parseInt(usize, lim, 10) catch wasm_funcs.items.len;
                    if (max_funcs < wasm_funcs.items.len) {
                        std.debug.print("[wasm-limit] Truncating {d} → {d} WASM functions\n", .{ wasm_funcs.items.len, max_funcs });
                        wasm_funcs.items.len = max_funcs;
                    }
                }

                var standalone_path_buf: [4096]u8 = undefined;
                const standalone_path = std.fmt.bufPrintZ(&standalone_path_buf, "{s}/standalone.o", .{cache_dir}) catch null;
                if (standalone_path) |sp| {
                    if (llvm_codegen.generateStandaloneWasm(allocator, wasm_funcs.items, sp)) |standalone_result| {
                        if (standalone_result.has_functions) {
                            std.debug.print("[freeze] Standalone WASM: {d} numeric functions → {s}\n", .{
                                standalone_result.func_count, sp,
                            });
                            // Write manifest with type info for worker generation
                            var manifest_path_buf: [4096]u8 = undefined;
                            const manifest_path = std.fmt.bufPrint(&manifest_path_buf, "{s}/standalone_manifest.json", .{cache_dir}) catch null;
                            if (manifest_path) |mp| {
                                if (std.fs.cwd().createFile(mp, .{})) |mf| {
                                    defer mf.close();
                                    mf.writeAll("[") catch {};

                                    // Build name→array_args map for cross-call propagation
                                    const ArrayInfo = struct { array_args: u8, mutated_args: u8 };
                                    var array_info_map = std.StringHashMapUnmanaged(ArrayInfo){};
                                    defer array_info_map.deinit(allocator);
                                    for (wasm_funcs.items) |sf| {
                                        const aa = numeric_handlers.detectArrayArgs(sf.func.instructions, sf.func.arg_count);
                                        const ma = numeric_handlers.detectMutatedArgs(sf.func.instructions, sf.func.arg_count);
                                        array_info_map.put(allocator, sf.name, .{ .array_args = aa, .mutated_args = ma }) catch {};
                                    }

                                    var first = true;
                                    for (wasm_funcs.items) |sf| {
                                        if (!first) mf.writeAll(",") catch {};
                                        first = false;
                                        const type_str: []const u8 = switch (sf.value_kind) {
                                            .i32 => "i32",
                                            .f64 => "f64",
                                        };
                                        const recursive: u8 = if (sf.func.is_self_recursive) 1 else 0;
                                        var array_args_mask = numeric_handlers.detectArrayArgs(sf.func.instructions, sf.func.arg_count);
                                        var mutated_args_mask = numeric_handlers.detectMutatedArgs(sf.func.instructions, sf.func.arg_count);

                                        // Propagate array_args from callees to cross-callers
                                        // If this function calls vn(x, xi, y, yi, n), and vn has array_args=0b10001 (x and y),
                                        // then this function's args that map to vn's array args also need copying.
                                        // Propagate array_args/mutated_args from callees to cross-callers
                                        if (array_args_mask == 0) {
                                            for (sf.func.instructions) |instr| {
                                                if (numeric_handlers.getHandler(instr.opcode).pattern != .self_ref) continue;
                                                const cn = resolveCalleeName(sf.func, instr) orelse continue;
                                                const info = array_info_map.get(cn) orelse continue;
                                                if (info.array_args != 0) {
                                                    array_args_mask = info.array_args;
                                                    mutated_args_mask = info.mutated_args;
                                                }
                                            }
                                        }

                                        var read_array_args_mask = numeric_handlers.detectReadArrayArgs(sf.func.instructions, sf.func.arg_count);
                                        const length_args_mask = numeric_handlers.detectLengthArgs(sf.func.instructions, sf.func.arg_count);
                                        const has_loop: u8 = if (numeric_handlers.detectHasLoop(sf.func.instructions)) 1 else 0;
                                        const has_bitwise: u8 = if (numeric_handlers.detectHasBitwise(sf.func.instructions)) 1 else 0;

                                        // Propagate read_array_args from callees (cross-callers only)
                                        if (read_array_args_mask == 0 and array_args_mask != 0 and array_info_map.get(sf.name) == null) {
                                            for (sf.func.instructions) |instr| {
                                                if (numeric_handlers.getHandler(instr.opcode).pattern != .self_ref) continue;
                                                const cn = resolveCalleeName(sf.func, instr) orelse continue;
                                                const info = array_info_map.get(cn) orelse continue;
                                                read_array_args_mask = info.array_args;
                                            }
                                        }

                                        // Use synthetic name for anonymous functions
                                        var synth_buf: [32]u8 = undefined;
                                        const manifest_name = if (sf.name.len == 0)
                                            std.fmt.bufPrint(&synth_buf, "__anon_L{d}", .{sf.func.line_num}) catch sf.name
                                        else
                                            sf.name;
                                        // Detect struct args for manifest
                                        const struct_args_info = numeric_handlers.detectStructArgs(sf.func.instructions, sf.func.arg_count);
                                        const struct_args_mask: u8 = if (struct_args_info) |si| si.struct_args else 0;
                                        const aos_mask: u8 = if (struct_args_info) |si| si.array_of_struct_args else 0;

                                        var entry_buf: [1024]u8 = undefined;
                                        const entry = std.fmt.bufPrint(&entry_buf, "{{\"name\":\"{s}\",\"args\":{d},\"type\":\"{s}\",\"instrs\":{d},\"recursive\":{d},\"array_args\":{d},\"mutated_args\":{d},\"read_array_args\":{d},\"length_args\":{d},\"has_loop\":{d},\"has_bitwise\":{d},\"line\":{d},\"struct_args\":{d},\"array_of_struct_args\":{d}", .{ manifest_name, sf.func.arg_count, type_str, sf.func.instructions.len, recursive, array_args_mask, mutated_args_mask, read_array_args_mask, length_args_mask, has_loop, has_bitwise, sf.func.line_num, struct_args_mask, aos_mask }) catch continue;
                                        mf.writeAll(entry) catch {};

                                        // Write struct field names for each struct/array-of-struct arg
                                        if (struct_args_info) |si| {
                                            const combined_mask = si.struct_args | si.array_of_struct_args;
                                            mf.writeAll(",\"struct_fields\":{") catch {};
                                            var first_arg = true;
                                            for (0..8) |ai| {
                                                if (combined_mask & (@as(u8, 1) << @intCast(ai)) == 0) continue;
                                                if (!first_arg) mf.writeAll(",") catch {};
                                                first_arg = false;
                                                var arg_buf: [8]u8 = undefined;
                                                const arg_key = std.fmt.bufPrint(&arg_buf, "\"{d}\":[", .{ai}) catch continue;
                                                mf.writeAll(arg_key) catch {};
                                                for (si.field_atoms[ai][0..si.field_counts[ai]], 0..) |atom, fi| {
                                                    if (fi > 0) mf.writeAll(",") catch {};
                                                    mf.writeAll("\"") catch {};
                                                    if (resolveAtomToName(sf.func, atom)) |name| {
                                                        mf.writeAll(name) catch {};
                                                    } else {
                                                        var atom_buf: [32]u8 = undefined;
                                                        const atom_str = std.fmt.bufPrint(&atom_buf, "atom{d}", .{atom}) catch continue;
                                                        mf.writeAll(atom_str) catch {};
                                                    }
                                                    mf.writeAll("\"") catch {};
                                                }
                                                mf.writeAll("]") catch {};
                                            }
                                            mf.writeAll("}") catch {};
                                            // Batch variant is always generated for struct functions
                                            mf.writeAll(",\"has_batch\":1") catch {};
                                        }

                                        mf.writeAll("}") catch {};
                                    }
                                    mf.writeAll("]") catch {};
                                } else |_| {}
                            }
                        }
                    } else |err| {
                        std.debug.print("[freeze] Standalone WASM generation failed: {}\n", .{err});
                    }
                }
            }

        // ===== Phase 2: Thin codegen for ALL remaining functions =====
        // Skip when wasm_only — worker path only needs standalone WASM (numeric kernels).
        // Thin shards are for native binary and WASM-static paths only.
        if (!wasm_only) {
        // Collect non-int32 functions for LLVM thin codegen
        var thin_llvm_funcs = std.ArrayListUnmanaged(llvm_codegen.ThinShardFunction){};
        defer thin_llvm_funcs.deinit(allocator);

        var thin_func_names = std.ArrayListUnmanaged([]u8){};
        defer {
            for (thin_func_names.items) |name| allocator.free(name);
            thin_func_names.deinit(allocator);
        }

        var thin_cfgs = std.ArrayListUnmanaged(cfg_builder.CFG){};
        defer {
            for (thin_cfgs.items) |*cfg_item| cfg_item.deinit();
            thin_cfgs.deinit(allocator);
        }

        const ThinFuncInfo = struct {
            gf_idx: usize,
            cfg_idx: usize,
            func_name_idx: usize,
        };
        var thin_infos = std.ArrayListUnmanaged(ThinFuncInfo){};
        defer thin_infos.deinit(allocator);

        var thin_skipped_int32: usize = 0;
        for (generated_all.items, 0..) |gf, gi| {
            // Skip int32 functions (already handled above in int32 shard for native)
            if (isPureInt32Function(gf.func)) {
                thin_skipped_int32 += 1;
                continue;
            }


            const cfg = cfg_builder.buildCFG(allocator, gf.func.instructions) catch continue;
            const cfg_idx = thin_cfgs.items.len;
            try thin_cfgs.append(allocator, cfg);

            var sanitized_buf: [256]u8 = undefined;
            const sanitized = sanitizeName(gf.func.name, &sanitized_buf);
            const llvm_func_name = try std.fmt.allocPrint(allocator, "__frozen_{d}_{s}", .{ gf.idx, sanitized });
            const func_name_idx = thin_func_names.items.len;
            try thin_func_names.append(allocator, llvm_func_name);

            try thin_infos.append(allocator, .{
                .gf_idx = gi,
                .cfg_idx = cfg_idx,
                .func_name_idx = func_name_idx,
            });
        }

        // Pass 2: Build ThinShardFunction structs
        for (thin_infos.items) |info| {
            const gf = generated_all.items[info.gf_idx];
            try thin_llvm_funcs.append(allocator, .{
                .name = gf.func.name,
                .func_index = gf.idx,
                .parser_index = gf.func.parser_index,
                .line_num = gf.func.line_num,
                .llvm_func_name = thin_func_names.items[info.func_name_idx],
                .func = gf.func,
                .cfg = &thin_cfgs.items[info.cfg_idx],
            });
        }

        if (thin_llvm_funcs.items.len > 0) {
            std.debug.print("[freeze] LLVM thin: {d} functions identified for compilation\n", .{thin_llvm_funcs.items.len});
            const THIN_FUNCS_PER_SHARD = 200;
            var func_offset: usize = 0;

            while (func_offset < thin_llvm_funcs.items.len) {
                const end = @min(func_offset + THIN_FUNCS_PER_SHARD, thin_llvm_funcs.items.len);
                const shard_funcs = thin_llvm_funcs.items[func_offset..end];

                var obj_path_buf: [4096]u8 = undefined;
                const obj_path = std.fmt.bufPrintZ(&obj_path_buf, "{s}/frozen_llvm_shard_{d}.o", .{ cache_dir, llvm_shard_count }) catch {
                    func_offset = end;
                    continue;
                };

                const result = llvm_codegen.generateThinShard(allocator, shard_funcs, llvm_shard_count, obj_path) catch |err| {
                    std.debug.print("[freeze] LLVM thin shard {d} failed: {}\n", .{ llvm_shard_count, err });
                    func_offset = end;
                    continue;
                };

                if (result.has_functions) {
                    std.debug.print("[freeze] LLVM thin shard {d}: {d} functions → {s}\n", .{ llvm_shard_count, result.func_count, obj_path });
                    llvm_shard_count += 1;
                }

                func_offset = end;
            }

            if (llvm_shard_count > 0) {
                std.debug.print("[freeze] LLVM IR total: {d} shard .o files (int32 + thin)\n", .{llvm_shard_count});
            }

            // Also generate WASM .o files (thin-only, int32 tier is native-only)
            // WASM shard numbering starts at int32_llvm_shard_count so shard init function
            // names match native thin shard numbers (e.g., frozen_init_llvm_shard_1 for
            // both native and WASM when there is 1 int32 shard).
            var wasm_shard_count: usize = int32_llvm_shard_count;
            {
                var wasm_func_offset: usize = 0;
                while (wasm_func_offset < thin_llvm_funcs.items.len) {
                    const wasm_end = @min(wasm_func_offset + THIN_FUNCS_PER_SHARD, thin_llvm_funcs.items.len);
                    const wasm_shard_funcs = thin_llvm_funcs.items[wasm_func_offset..wasm_end];

                    var wasm_obj_path_buf: [4096]u8 = undefined;
                    const wasm_obj_path = std.fmt.bufPrintZ(&wasm_obj_path_buf, "{s}/frozen_llvm_shard_wasm_{d}.o", .{ cache_dir, wasm_shard_count }) catch {
                        wasm_func_offset = wasm_end;
                        continue;
                    };

                    const wasm_result = llvm_codegen.generateThinShardWasm(allocator, wasm_shard_funcs, wasm_shard_count, wasm_obj_path) catch |err| {
                        std.debug.print("[freeze] LLVM WASM shard {d} failed: {}\n", .{ wasm_shard_count, err });
                        wasm_func_offset = wasm_end;
                        continue;
                    };

                    if (wasm_result.has_functions) {
                        std.debug.print("[freeze] LLVM WASM shard {d}: {d} functions → {s}\n", .{ wasm_shard_count, wasm_result.func_count, wasm_obj_path });
                        wasm_shard_count += 1;
                    }

                    wasm_func_offset = wasm_end;
                }
                if (wasm_shard_count > int32_llvm_shard_count) {
                    std.debug.print("[freeze] LLVM WASM: {d} shard .o files\n", .{wasm_shard_count - int32_llvm_shard_count});
                }
            }
        }
    }
    } // end if (!wasm_only)

    std.debug.print("[freeze] LLVM backend: {d} functions ({d} int32 + {d} thin) in {d} shard .o files\n", .{
        generated_all.items.len,
        int32_llvm_shard_count,
        llvm_shard_count - int32_llvm_shard_count,
        llvm_shard_count,
    });

    // Detect allocation sites: functions that create object literals with fixed shapes.
    // These are candidates for SOA transform — rewrite to allocate in WASM linear memory.
    if (output_dir) |cd| {
        var alloc_manifest_path_buf: [4096]u8 = undefined;
        const alloc_manifest_path = std.fmt.bufPrint(&alloc_manifest_path_buf, "{s}/alloc_manifest.json", .{cd}) catch null;
        if (alloc_manifest_path) |amp| {
            if (std.fs.cwd().createFile(amp, .{})) |amf| {
                defer amf.close();
                amf.writeAll("[") catch {};
                var alloc_count: usize = 0;
                for (analysis.functions.items) |func| {
                    const alloc_info = numeric_handlers.detectAllocSites(func.instructions) orelse continue;
                    if (alloc_count > 0) amf.writeAll(",") catch {};
                    // Use synthetic name for anonymous functions
                    var synth_buf: [32]u8 = undefined;
                    const alloc_name = if (func.name.len == 0)
                        std.fmt.bufPrint(&synth_buf, "__anon_L{d}", .{func.line_num}) catch func.name
                    else
                        func.name;
                    var entry_buf: [1024]u8 = undefined;
                    const entry = std.fmt.bufPrint(&entry_buf, "{{\"name\":\"{s}\",\"line\":{d},\"args\":{d},\"pass_through\":{s},\"alloc_fields\":[", .{
                        alloc_name, func.line_num, func.arg_count,
                        if (alloc_info.pass_through) "true" else "false",
                    }) catch continue;
                    amf.writeAll(entry) catch {};
                    for (alloc_info.field_atoms[0..alloc_info.field_count], 0..) |atom, fi| {
                        if (fi > 0) amf.writeAll(",") catch {};
                        amf.writeAll("\"") catch {};
                        if (resolveAtomToName(func, atom)) |name| {
                            amf.writeAll(name) catch {};
                        } else {
                            var atom_buf: [32]u8 = undefined;
                            const atom_str = std.fmt.bufPrint(&atom_buf, "atom{d}", .{atom}) catch continue;
                            amf.writeAll(atom_str) catch {};
                        }
                        amf.writeAll("\"") catch {};
                    }
                    amf.writeAll("],\"arg_indices\":[") catch {};
                    for (alloc_info.arg_indices[0..alloc_info.field_count], 0..) |ai, fi| {
                        if (fi > 0) amf.writeAll(",") catch {};
                        var ai_buf: [8]u8 = undefined;
                        const ai_str = std.fmt.bufPrint(&ai_buf, "{d}", .{ai}) catch continue;
                        amf.writeAll(ai_str) catch {};
                    }
                    amf.writeAll("]}") catch {};
                    alloc_count += 1;
                }
                amf.writeAll("]") catch {};
                if (alloc_count > 0) {
                    std.debug.print("[freeze] Alloc sites: {d} factory functions with fixed shapes\n", .{alloc_count});
                }
            } else |_| {}
        }

        // Also emit struct read sites — functions that read obj.field on their args.
        // This is the SAME detectStructArgs analysis, but emitted for ALL functions,
        // not just the ones that pass numeric tier filtering.
        var read_manifest_path_buf: [4096]u8 = undefined;
        const read_manifest_path = std.fmt.bufPrint(&read_manifest_path_buf, "{s}/read_manifest.json", .{cd}) catch null;
        if (read_manifest_path) |rmp| {
            if (std.fs.cwd().createFile(rmp, .{})) |rmf| {
                defer rmf.close();
                rmf.writeAll("[") catch {};
                var read_count: usize = 0;
                for (analysis.functions.items) |func| {
                    const si = numeric_handlers.detectStructArgs(func.instructions, func.arg_count) orelse continue;
                    if (read_count > 0) rmf.writeAll(",") catch {};
                    var synth_buf2: [32]u8 = undefined;
                    const read_name = if (func.name.len == 0)
                        std.fmt.bufPrint(&synth_buf2, "__anon_L{d}", .{func.line_num}) catch func.name
                    else
                        func.name;
                    var entry_buf2: [512]u8 = undefined;
                    const combined = si.struct_args | si.array_of_struct_args;
                    const entry2 = std.fmt.bufPrint(&entry_buf2, "{{\"name\":\"{s}\",\"line\":{d},\"struct_args\":{d},\"read_fields\":{{", .{
                        read_name, func.line_num, combined,
                    }) catch continue;
                    rmf.writeAll(entry2) catch {};
                    var first_arg2 = true;
                    for (0..@min(func.arg_count, 8)) |ai| {
                        if (combined & (@as(u8, 1) << @intCast(ai)) == 0) continue;
                        if (!first_arg2) rmf.writeAll(",") catch {};
                        first_arg2 = false;
                        var arg_buf2: [8]u8 = undefined;
                        const arg_key2 = std.fmt.bufPrint(&arg_buf2, "\"{d}\":[", .{ai}) catch continue;
                        rmf.writeAll(arg_key2) catch {};
                        for (si.field_atoms[ai][0..si.field_counts[ai]], 0..) |atom, fi| {
                            if (fi > 0) rmf.writeAll(",") catch {};
                            rmf.writeAll("\"") catch {};
                            if (resolveAtomToName(func, atom)) |name| {
                                rmf.writeAll(name) catch {};
                            } else {
                                var atom_buf2: [32]u8 = undefined;
                                const atom_str2 = std.fmt.bufPrint(&atom_buf2, "atom{d}", .{atom}) catch continue;
                                rmf.writeAll(atom_str2) catch {};
                            }
                            rmf.writeAll("\"") catch {};
                        }
                        rmf.writeAll("]") catch {};
                    }
                    rmf.writeAll("}}") catch {};
                    read_count += 1;
                }
                rmf.writeAll("]") catch {};
                if (read_count > 0) {
                    std.debug.print("[freeze] Read sites: {d} functions read struct fields from args\n", .{read_count});
                }
            } else |_| {}
        }
    }

    // Generate main frozen_module.zig with extern declarations for LLVM shard init functions
    var main_output = std.ArrayListUnmanaged(u8){};
    errdefer main_output.deinit(allocator);

    try main_output.appendSlice(allocator,
        \\//! Auto-generated frozen functions main module
        \\//! DO NOT EDIT - generated by EdgeBox freeze system
        \\//!
        \\//! All frozen functions are compiled via LLVM IR → .o shard files.
        \\//! This file declares extern init functions for each LLVM shard.
        \\
        \\const std = @import("std");
        \\const zig_runtime = @import("zig_runtime");
        \\const math_polyfill = @import("math_polyfill");
        \\const native_dispatch = @import("native_dispatch");
        \\const JSValue = zig_runtime.JSValue;
        \\const JSContext = zig_runtime.JSContext;
        \\
        \\
    );

    // Add WASM32 detection for conditional shard init
    try main_output.appendSlice(allocator,
        \\const is_wasm32 = @import("builtin").cpu.arch == .wasm32;
        \\
    );

    // Declare extern functions for LLVM shard inits (from LLVM-compiled .o files)
    // Int32 shards (0..int32_llvm_shard_count-1) are native-only — skip on WASM
    if (llvm_shard_count > 0) {
        for (0..llvm_shard_count) |i| {
            var extern_buf: [512]u8 = undefined;
            if (i < int32_llvm_shard_count) {
                // Int32 shard: native-only, return 0 on WASM
                // Use @extern to reference original symbol name without naming conflict
                const extern_line = std.fmt.bufPrint(&extern_buf,
                    \\fn call_shard_init_{d}() c_int {{
                    \\    if (comptime is_wasm32) return 0;
                    \\    const f = @extern(*const fn () callconv(.c) c_int, .{{ .name = "frozen_init_llvm_shard_{d}" }});
                    \\    return f();
                    \\}}
                    \\
                , .{ i, i }) catch continue;
                try main_output.appendSlice(allocator, extern_line);
            } else {
                // Thin shard: available on both native and WASM
                const extern_line = std.fmt.bufPrint(&extern_buf, "extern fn frozen_init_llvm_shard_{d}() callconv(.c) c_int;\n", .{i}) catch continue;
                try main_output.appendSlice(allocator, extern_line);
            }
        }
    }

    // Native registry functions and init
    try main_output.appendSlice(allocator,
        \\
        \\// Native registry functions (from native_shapes.zig)
        \\extern fn native_registry_init() void;
        \\extern fn native_registry_count() c_int;
        \\extern fn native_node_register(js_addr: u64, kind: i32, flags: i32, pos: i32, end: i32) ?*anyopaque;
        \\
        \\fn registerNodeImpl(ctx: *zig_runtime.JSContext, _: zig_runtime.JSValue, argc: c_int, argv: [*]zig_runtime.JSValue) callconv(.c) zig_runtime.JSValue {
        \\    if (argc < 5) return zig_runtime.JSValue.UNDEFINED;
        \\    const obj = argv[0];
        \\    if (!obj.isObject()) return zig_runtime.JSValue.UNDEFINED;
        \\    var kind: i32 = 0;
        \\    var flags: i32 = 0;
        \\    var pos: i32 = 0;
        \\    var end: i32 = 0;
        \\    _ = zig_runtime.quickjs.JS_ToInt32(ctx, &kind, argv[1]);
        \\    _ = zig_runtime.quickjs.JS_ToInt32(ctx, &flags, argv[2]);
        \\    _ = zig_runtime.quickjs.JS_ToInt32(ctx, &pos, argv[3]);
        \\    _ = zig_runtime.quickjs.JS_ToInt32(ctx, &end, argv[4]);
        \\    // Use full 64-bit pointer address (matches jsvalueToAddr in frozen_helpers.zig)
        \\    const ptr_addr: u64 = @intFromPtr(obj.getPtr());
        \\    const node = native_node_register(ptr_addr, kind, flags, pos, end);
        \\    return if (node != null) zig_runtime.JSValue.TRUE else zig_runtime.JSValue.FALSE;
        \\}
        \\
        \\fn registryCountImpl(_: *zig_runtime.JSContext, _: zig_runtime.JSValue, _: c_int, _: [*]zig_runtime.JSValue) callconv(.c) zig_runtime.JSValue {
        \\    return zig_runtime.JSValue.newInt(native_registry_count());
        \\}
        \\
        \\pub fn frozen_init(ctx: *zig_runtime.JSContext) c_int {
        \\    const qjs = zig_runtime.quickjs;
        \\    const global = qjs.JS_GetGlobalObject(ctx);
        \\    defer qjs.JS_FreeValue(ctx, global);
        \\    var count: c_int = 0;
        \\
        \\    native_registry_init();
        \\    _ = qjs.JS_SetPropertyStr(ctx, global, "__edgebox_register_node",
        \\        qjs.JS_NewCFunction(ctx, @ptrCast(&registerNodeImpl), "__edgebox_register_node", 5));
        \\    _ = qjs.JS_SetPropertyStr(ctx, global, "__edgebox_registry_count",
        \\        qjs.JS_NewCFunction(ctx, @ptrCast(&registryCountImpl), "__edgebox_registry_count", 0));
        \\    math_polyfill.register(ctx);
        \\
        \\    // Call each LLVM shard's init function
        \\
    );

    for (0..llvm_shard_count) |i| {
        var call_buf: [128]u8 = undefined;
        if (i < int32_llvm_shard_count) {
            // Int32 shard: use wrapper that returns 0 on WASM
            const call_line = std.fmt.bufPrint(&call_buf, "    count += call_shard_init_{d}();\n", .{i}) catch continue;
            try main_output.appendSlice(allocator, call_line);
        } else {
            // Thin shard: available on both native and WASM
            const call_line = std.fmt.bufPrint(&call_buf, "    count += frozen_init_llvm_shard_{d}();\n", .{i}) catch continue;
            try main_output.appendSlice(allocator, call_line);
        }
    }

    try main_output.appendSlice(allocator,
        \\
        \\    native_dispatch.enableDispatch();
        \\    return count;
        \\}
        \\
        \\pub export fn frozen_init_c(ctx: *zig_runtime.JSContext) callconv(.c) c_int {
        \\    return frozen_init(ctx);
        \\}
        \\
    );

    return ShardedOutput{
        .main = try main_output.toOwnedSlice(allocator),
        .llvm_shard_count = llvm_shard_count,
    };
}

/// Generate closure manifest JSON directly from analysis (no C code generation)
/// Returns null if no functions have closure vars
/// Format: {"functions":[{"name":"foo","closureVars":[{"n":"var1","c":false}]}]}
pub fn generateClosureManifest(
    allocator: Allocator,
    analysis: *const ModuleAnalysis,
    manifest_json: ?[]const u8,
) !?[]u8 {
    // Parse manifest if provided - only include functions that match
    var manifest: []ManifestFunction = &.{};
    defer if (manifest.len > 0) freeManifest(allocator, manifest);
    if (manifest_json) |json| {
        manifest = parseManifest(allocator, json) catch &.{};
    }

    // Collect functions with closure vars
    const ClosureFuncInfo = struct {
        name: []const u8,
        closure_vars: []const module_parser.ClosureVarInfo,
    };
    var funcs_with_closures = std.ArrayListUnmanaged(ClosureFuncInfo){};
    defer funcs_with_closures.deinit(allocator);

    for (analysis.functions.items) |func| {
        // Skip functions not in manifest (if manifest provided)
        if (manifest.len > 0) {
            var in_manifest = false;
            for (manifest) |mf| {
                if (std.mem.eql(u8, mf.name, func.name)) {
                    in_manifest = true;
                    break;
                }
            }
            if (!in_manifest) continue;
        }

        // Skip functions without closure vars
        if (func.closure_vars.len == 0) continue;

        // Skip self-recursive functions where the only closure var is self-reference
        if (func.is_self_recursive and func.closure_vars.len == 1) {
            if (std.mem.eql(u8, func.closure_vars[0].name, func.name)) continue;
        }

        try funcs_with_closures.append(allocator, .{
            .name = func.name,
            .closure_vars = func.closure_vars,
        });
    }

    // No closure functions found
    if (funcs_with_closures.items.len == 0) return null;

    // Generate JSON
    var output = std.ArrayListUnmanaged(u8){};
    errdefer output.deinit(allocator);

    try output.appendSlice(allocator, "{\"functions\":[");
    var first = true;
    for (funcs_with_closures.items) |func_info| {
        if (!first) try output.appendSlice(allocator, ",");
        first = false;

        try output.appendSlice(allocator, "{\"name\":\"");
        try output.appendSlice(allocator, func_info.name);
        try output.appendSlice(allocator, "\",\"closureVars\":[");

        var first_var = true;
        for (func_info.closure_vars) |cv| {
            // Skip self-reference in closure vars
            if (std.mem.eql(u8, cv.name, func_info.name)) continue;

            if (!first_var) try output.appendSlice(allocator, ",");
            first_var = false;
            try output.appendSlice(allocator, "{\"n\":\"");
            try output.appendSlice(allocator, cv.name);
            try output.appendSlice(allocator, "\",\"c\":");
            try output.appendSlice(allocator, if (cv.is_const) "true" else "false");
            try output.appendSlice(allocator, "}");
        }
        try output.appendSlice(allocator, "]}");
    }
    try output.appendSlice(allocator, "]}");

    const result = try output.toOwnedSlice(allocator);
    return result;
}

/// Manifest entry parsed from JSON
pub const ManifestFunction = struct {
    name: []const u8,
    argCount: u32,
    isSelfRecursive: bool,
};

/// Parse manifest JSON to get function names
pub fn parseManifest(allocator: Allocator, manifest_json: []const u8) ![]ManifestFunction {
    const parsed = std.json.parseFromSlice(std.json.Value, allocator, manifest_json, .{}) catch return &.{};
    defer parsed.deinit();

    const functions_val = parsed.value.object.get("functions") orelse return &.{};
    if (functions_val != .array) return &.{};

    var result = std.ArrayListUnmanaged(ManifestFunction){};
    for (functions_val.array.items) |func_val| {
        if (func_val != .object) continue;

        const name_val = func_val.object.get("name") orelse continue;
        if (name_val != .string) continue;

        const arg_count_val = func_val.object.get("argCount") orelse continue;
        const arg_count: u32 = switch (arg_count_val) {
            .integer => @intCast(@max(0, arg_count_val.integer)),
            else => continue,
        };

        const is_recursive = if (func_val.object.get("isSelfRecursive")) |v|
            (v == .bool and v.bool)
        else
            false;

        // Must dupe the name since parsed will be freed
        const name = allocator.dupe(u8, name_val.string) catch continue;
        try result.append(allocator, .{
            .name = name,
            .argCount = arg_count,
            .isSelfRecursive = is_recursive,
        });
    }

    return result.toOwnedSlice(allocator);
}

/// Free manifest functions
pub fn freeManifest(allocator: Allocator, manifest: []const ManifestFunction) void {
    for (manifest) |m| {
        allocator.free(m.name);
    }
    allocator.free(manifest);
}

// ============================================================================
// Native Static Build - Zig Hot Path Generation
// ============================================================================

/// Check if a function is pure int32 (can be compiled to Zig hot path)
/// Returns true if all operations are int32-safe.
/// Supports both self-recursive functions (like fib) and non-recursive
/// pure-integer functions (like isLineBreak, isDigit) with forward-only control flow.
pub fn isPureInt32Function(func: AnalyzedFunction) bool {
    // Must have 1-8 args
    if (func.arg_count < 1 or func.arg_count > 8) return false;

    // Guard: too many locals for non-recursive functions
    if (!func.is_self_recursive and func.var_count > 16) return false;

    // Check all instructions for non-int32 operations,
    // and require at least one integer-producing operation to prove
    // the function actually works with integers (not just pass-through).
    const int32_handlers = @import("int32_handlers.zig");
    var has_int32_op = false;
    for (func.instructions) |instr| {
        const handler = int32_handlers.getInt32Handler(instr.opcode);
        if (handler.pattern == .unsupported) {
            if (std.posix.getenv("EDGEBOX_INT32_DEBUG")) |_| {
                std.debug.print("[int32-reject] {s}: unsupported opcode '{s}'\n", .{ func.name, @tagName(instr.opcode) });
            }
            return false;
        }
        // These patterns prove the function actually computes with integers
        switch (handler.pattern) {
            .binary_arith_i32, .binary_cmp_i32, .bitwise_binary_i32,
            .unary_i32, .inc_dec_i32, .lnot_i32, .push_const_i32, .push_cpool_i32,
            .push_bool_i32, .call_self_i32, .tail_call_self_i32,
            .add_loc_i32, .inc_loc_i32,
            => has_int32_op = true,
            else => {},
        }
    }

    // Reject functions that are just pass-throughs (e.g., `function f(x) { return x; }`)
    // — they can be called with any type and int32 specialization would corrupt objects.
    if (!has_int32_op) return false;

    // Validate all cpool references are numeric (int32 or integer-valued float64).
    // Large hex constants like 0xEDB88320 are stored as float64 in QuickJS cpool
    // because they exceed i32 range, but are semantically integers.
    for (func.instructions) |instr| {
        if (instr.opcode == .push_const8 or instr.opcode == .push_const) {
            const idx: u32 = switch (instr.operand) {
                .const_idx => |a| a,
                else => return false,
            };
            if (idx >= func.constants.len) return false;
            switch (func.constants[idx]) {
                .int32 => {},
                .float64 => |v| {
                    // Accept float64 if it represents a whole number (e.g., 0xEDB88320)
                    if (v != @trunc(v)) return false; // not integer-valued
                },
                else => return false, // string/complex — not pure i32
            }
        }
    }

    // For non-recursive functions, reject recursive-only patterns (self_ref, call_self)
    // and verify forward-jump-only control flow.
    if (!func.is_self_recursive) {
        for (func.instructions) |instr| {
            const handler = int32_handlers.getInt32Handler(instr.opcode);
            if (handler.pattern == .self_ref_i32 or handler.pattern == .call_self_i32 or handler.pattern == .tail_call_self_i32) return false;
        }
        for (func.instructions) |instr| {
            const handler = int32_handlers.getInt32Handler(instr.opcode);
            if (handler.pattern == .if_false_i32 or handler.pattern == .if_true_i32 or handler.pattern == .goto_i32) {
                const label: i32 = switch (instr.operand) {
                    .label => |l| l,
                    else => continue,
                };
                if (label < 0) return false;
            }
        }
    }

    return true;
}

/// Analyze if a function qualifies for standalone numeric WASM compilation.
/// Supported Math.* intrinsics for standalone WASM compilation.
/// These map directly to WASM f64 intrinsics (f64.abs, f64.sqrt, etc.).
pub const MathIntrinsic = enum {
    // Unary (1 arg)
    abs, // Math.abs(x) → f64.abs
    sqrt, // Math.sqrt(x) → f64.sqrt
    floor, // Math.floor(x) → f64.floor
    ceil, // Math.ceil(x) → f64.ceil
    round, // Math.round(x) → floor(x + 0.5)
    trunc, // Math.trunc(x) → f64.trunc
    // Binary (2 args)
    min, // Math.min(a, b) → f64.min
    max, // Math.max(a, b) → f64.max
    pow, // Math.pow(a, b) → llvm.pow.f64

    pub fn arity(self: MathIntrinsic) u16 {
        return switch (self) {
            .abs, .sqrt, .floor, .ceil, .round, .trunc => 1,
            .min, .max, .pow => 2,
        };
    }
};

/// Match a closure variable name to a known Math intrinsic.
/// Used for destructured patterns like `const {sqrt, pow} = Math`.
pub fn matchMathName(name: []const u8) ?MathIntrinsic {
    if (std.mem.eql(u8, name, "abs")) return .abs;
    if (std.mem.eql(u8, name, "sqrt")) return .sqrt;
    if (std.mem.eql(u8, name, "floor")) return .floor;
    if (std.mem.eql(u8, name, "ceil")) return .ceil;
    if (std.mem.eql(u8, name, "round")) return .round;
    if (std.mem.eql(u8, name, "trunc")) return .trunc;
    if (std.mem.eql(u8, name, "min")) return .min;
    if (std.mem.eql(u8, name, "max")) return .max;
    if (std.mem.eql(u8, name, "pow")) return .pow;
    return null;
}

/// State machine for tracking Math.* pattern sequences across instruction walks.
const MathSkipState = struct {
    pending_math_depth: u32 = 0,
    pending_destr_math: u32 = 0,
};

/// Try to advance past a Math.* pattern at position `i`.
/// Returns the new position after skipping if a pattern was matched, or null if no match.
/// `is_setup` is true when matching a pattern start (get_var("Math") or get_var_ref{N}),
/// false when matching a pattern completion (call_method or call{N}).
fn skipMathPattern(
    func: AnalyzedFunction,
    instructions: []const bytecode_parser.Instruction,
    i: usize,
    state: *MathSkipState,
) ?struct { new_i: usize, is_setup: bool } {
    if (detectMathSetup(func, instructions, i)) |_| {
        state.pending_math_depth += 1;
        return .{ .new_i = i + 2, .is_setup = true };
    }
    if (state.pending_math_depth > 0 and isMathCallMethod1(instructions, i)) {
        state.pending_math_depth -= 1;
        return .{ .new_i = i + 1, .is_setup = false };
    }
    if (detectDestructuredMathRef(func, instructions, i)) |_| {
        state.pending_destr_math += 1;
        return .{ .new_i = i + 1, .is_setup = true };
    }
    if (state.pending_destr_math > 0 and isDestructuredMathCallAny(instructions, i)) {
        state.pending_destr_math -= 1;
        return .{ .new_i = i + 1, .is_setup = false };
    }
    return null;
}

/// Detect a destructured Math call at instruction index `i`.
/// Pattern: get_var_ref{N} where closure_vars[N].name is a known Math method,
/// followed (after argument pushes) by call1/call2/call3/call.
/// This handles `const {sqrt, pow, floor} = Math; function f(x) { return sqrt(x); }`
/// Returns the intrinsic enum or null if not a destructured Math call.
pub fn detectDestructuredMathRef(func: AnalyzedFunction, instructions: []const bytecode_parser.Instruction, i: usize) ?MathIntrinsic {
    if (i >= instructions.len) return null;
    const instr = instructions[i];

    // Must be a get_var_ref opcode
    const var_ref_idx: u32 = switch (instr.opcode) {
        .get_var_ref0 => 0,
        .get_var_ref1 => 1,
        .get_var_ref2 => 2,
        .get_var_ref3 => 3,
        .get_var_ref => switch (instr.operand) {
            .u16 => |v| v,
            else => return null,
        },
        else => return null,
    };

    // Look up the closure variable name
    if (var_ref_idx >= func.closure_vars.len) return null;
    const cv_name = func.closure_vars[var_ref_idx].name;

    return matchMathName(cv_name);
}

/// Check if instruction at index `i` is call(N) where N matches the expected arity.
/// Used for destructured Math calls where the call opcode is `call` (not `call_method`).
/// Check if instruction at index `i` is any call opcode (call1/call2/call3/call/tail_call).
/// Used in pre-scan to match destructured Math calls without checking specific arity.
fn isDestructuredMathCallAny(instructions: []const bytecode_parser.Instruction, i: usize) bool {
    if (i >= instructions.len) return false;
    return switch (instructions[i].opcode) {
        .call1, .call2, .call3, .call, .tail_call => true,
        else => false,
    };
}

/// Detect a Math.* method setup starting at instruction index `i`.
/// Looks for the 2-opcode sequence: get_var("Math") → get_field2("method")
/// The actual call_method(1) comes LATER (after argument computation).
/// Returns the intrinsic enum or null if not a Math pattern.
pub fn detectMathSetup(func: AnalyzedFunction, instructions: []const bytecode_parser.Instruction, i: usize) ?MathIntrinsic {
    if (i + 1 >= instructions.len) return null;

    // Instruction 0: get_var("Math") or get_var_undef("Math")
    const instr0 = instructions[i];
    if (instr0.opcode != .get_var and instr0.opcode != .get_var_undef) return null;
    const atom_idx0: u32 = switch (instr0.operand) {
        .atom => |a| a,
        else => return null,
    };
    const var_name = resolveAtomToName(func, atom_idx0) orelse return null;
    if (!std.mem.eql(u8, var_name, "Math")) return null;

    // Instruction 1: get_field2("abs"/"sqrt"/...)
    const instr1 = instructions[i + 1];
    if (instr1.opcode != .get_field2) return null;
    const atom_idx1: u32 = switch (instr1.operand) {
        .atom => |a| a,
        else => return null,
    };
    const method_name = resolveAtomToName(func, atom_idx1) orelse return null;

    if (std.mem.eql(u8, method_name, "abs")) return .abs;
    if (std.mem.eql(u8, method_name, "sqrt")) return .sqrt;
    if (std.mem.eql(u8, method_name, "floor")) return .floor;
    if (std.mem.eql(u8, method_name, "ceil")) return .ceil;
    if (std.mem.eql(u8, method_name, "round")) return .round;
    if (std.mem.eql(u8, method_name, "trunc")) return .trunc;
    if (std.mem.eql(u8, method_name, "min")) return .min;
    if (std.mem.eql(u8, method_name, "max")) return .max;
    if (std.mem.eql(u8, method_name, "pow")) return .pow;
    return null;
}

/// Check if instruction at index `i` is call_method(N) or tail_call_method(N)
/// where N=1 (unary) or N=2 (binary) — the call part of a Math intrinsic.
/// QuickJS optimizes `return Math.sqrt(x)` → tail_call_method instead of call_method.
pub fn isMathCallMethod1(instructions: []const bytecode_parser.Instruction, i: usize) bool {
    if (i >= instructions.len) return false;
    const instr = instructions[i];
    if (instr.opcode != .call_method and instr.opcode != .tail_call_method) return false;
    const argc: u16 = switch (instr.operand) {
        .u16 => |v| v,
        else => return false,
    };
    return argc == 1 or argc == 2;
}

/// Returns the ValueKind (.i32 or .f64) or null if unsupported.
/// This extends isPureInt32Function to also detect float-only functions
/// (e.g., functions using div, which produces floats in JS).
///
/// Unlike isPureInt32Function, this allows backward jumps (while loops)
/// because LLVM generates proper basic blocks — no Zig hotpath dispatch issues.
///
/// Also detects Math.* intrinsic patterns (Math.abs, Math.sqrt, etc.)
/// and compiles them to WASM f64 intrinsics.
/// Thread-local cache for struct arg info detected during analysis.
/// Set by analyzeNumericTier when struct args are detected, read by codegen.
var struct_info_cache: numeric_handlers.StructArgInfo = undefined;
var has_struct_info: bool = false;

/// Get the most recently detected struct arg info (call after analyzeNumericTier)
pub fn getLastStructInfo() ?*const numeric_handlers.StructArgInfo {
    if (has_struct_info) {
        has_struct_info = false;
        return &struct_info_cache;
    }
    return null;
}

pub fn analyzeNumericTier(func: AnalyzedFunction) ?numeric_handlers.ValueKind {
    const debug = std.posix.getenv("EDGEBOX_WASM_DEBUG") != null;
    // Same preconditions as isPureInt32Function (but higher arg limit for WASM)
    if (func.arg_count < 1 or func.arg_count > 24) {
        if (debug) std.debug.print("[wasm-reject] {s}: arg_count={d}\n", .{ func.name, func.arg_count });
        return null;
    }
    if (func.var_count > 48) {
        if (debug) std.debug.print("[wasm-reject] {s}: var_count={d}\n", .{ func.name, func.var_count });
        return null;
    }

    // Pre-scan for Math.* intrinsic patterns (Math.floor(x), destructured sqrt(x), etc.)
    var has_math_intrinsics = false;
    {
        var math_state = MathSkipState{};
        var mi: usize = 0;
        while (mi < func.instructions.len) {
            if (skipMathPattern(func, func.instructions, mi, &math_state)) |result| {
                if (result.is_setup) has_math_intrinsics = true;
                mi = result.new_i;
            } else {
                mi += 1;
            }
        }
    }

    if (has_math_intrinsics) {
        // Math-aware analysis: iterate instructions, skipping Math.* patterns
        return analyzeFunctionWithMath(func, debug);
    }

    // Standard analysis (no Math patterns)
    const kind = numeric_handlers.analyzeFunction(func.instructions) orelse blk: {
        // Standard analysis failed — check if it's because of get_field (struct access)
        var has_field_get = false;
        for (func.instructions) |instr| {
            const handler = numeric_handlers.getHandler(instr.opcode);
            if (handler.pattern == .field_get) { has_field_get = true; continue; }
            if (handler.pattern == .unsupported) {
                if (debug) std.debug.print("[wasm-reject] {s}: unsupported opcode '{s}'\n", .{ func.name, @tagName(instr.opcode) });
                return null;
            }
        }
        if (!has_field_get) return null;

        // Try struct-aware analysis: detect which args are struct-typed
        const struct_args = numeric_handlers.detectStructArgs(func.instructions, func.arg_count) orelse {
            if (debug) std.debug.print("[wasm-reject] {s}: get_field on non-arg (not struct-eligible)\n", .{func.name});
            return null;
        };

        // Re-analyze with get_field allowed
        const struct_result = numeric_handlers.analyzeFunctionWithStructs(func.instructions) orelse {
            if (debug) std.debug.print("[wasm-reject] {s}: struct analysis failed\n", .{func.name});
            return null;
        };

        // Store struct info for later use by codegen and trampoline generation
        // (stored in thread-local for the current compilation pass)
        struct_info_cache = struct_args;
        has_struct_info = true;

        if (debug) {
            const combined = struct_args.struct_args | struct_args.array_of_struct_args;
            std.debug.print("[wasm-struct] {s}: struct=0x{x} aos=0x{x}, fields:", .{ func.name, struct_args.struct_args, struct_args.array_of_struct_args });
            for (0..8) |ai| {
                if (combined & (@as(u8, 1) << @intCast(ai)) != 0) {
                    const tag: []const u8 = if (struct_args.array_of_struct_args & (@as(u8, 1) << @intCast(ai)) != 0) "arr" else "arg";
                    std.debug.print(" {s}{d}=[", .{ tag, ai });
                    for (struct_args.field_atoms[ai][0..struct_args.field_counts[ai]], 0..) |atom, fi| {
                        if (fi > 0) std.debug.print(",", .{});
                        if (resolveAtomToName(func, atom)) |name| {
                            std.debug.print("{s}", .{name});
                        } else {
                            std.debug.print("atom{d}", .{atom});
                        }
                    }
                    std.debug.print("]", .{});
                }
            }
            std.debug.print("\n", .{});
        }

        break :blk struct_result.kind;
    };

    // Reject functions that capture closure variables (callbacks WASM can't call).
    if (func.closure_vars.len > 1) {
        if (debug) std.debug.print("[wasm-reject] {s}: {d} closure vars (likely calls JS callbacks)\n", .{ func.name, func.closure_vars.len });
        return null;
    }

    // Reject struct-arg functions with >1 field per struct arg.
    // Single-field structs (just .kind) are safe: the field is always numeric.
    // Multi-field structs may include object references (.left, .expression, .parent)
    // that can't be represented as i32 in WASM linear memory.
    if (has_struct_info) {
        if (getLastStructInfo()) |si| {
            const combined = si.struct_args | si.array_of_struct_args;
            for (0..@min(func.arg_count, 8)) |ai| {
                if (combined & (@as(u8, 1) << @intCast(ai)) != 0 and si.field_counts[ai] > 1) {
                    if (debug) std.debug.print("[wasm-reject] {s}: struct with {d} fields (may include object refs)\n", .{ func.name, si.field_counts[ai] });
                    return null;
                }
            }
        }
    }

    // For non-recursive functions, reject recursive-only patterns
    // (self_ref + call_self without the function being self-recursive)
    // Exception: get_var_ref{N} that refers to a destructured Math builtin is allowed
    if (!func.is_self_recursive) {
        var pending_destr: u32 = 0;
        var si: usize = 0;
        while (si < func.instructions.len) {
            // Skip destructured Math ref + its call
            if (detectDestructuredMathRef(func, func.instructions, si)) |_| {
                pending_destr += 1;
                si += 1;
                continue;
            }
            if (pending_destr > 0 and isDestructuredMathCallAny(func.instructions, si)) {
                pending_destr -= 1;
                si += 1;
                continue;
            }
            const handler = numeric_handlers.getHandler(func.instructions[si].opcode);
            if (handler.pattern == .self_ref or handler.pattern == .call_self or handler.pattern == .tail_call_self) {
                if (debug) std.debug.print("[wasm-reject] {s}: non-recursive with call pattern '{s}'\n", .{ func.name, @tagName(func.instructions[si].opcode) });
                return null;
            }
            si += 1;
        }
    }

    // Validate all cpool references are numeric (int32 or float64).
    // If any constant is float64, upgrade to f64 tier — the function
    // operates on float data (e.g. `var x = 1.5`, `var scale = 0.001`).
    var has_float_const = false;
    for (func.instructions) |instr| {
        if (instr.opcode == .push_const8 or instr.opcode == .push_const) {
            const idx: u32 = switch (instr.operand) {
                .const_idx => |a| a,
                else => return null,
            };
            if (idx >= func.constants.len) return null;
            switch (func.constants[idx]) {
                .int32 => {},
                .float64 => has_float_const = true,
                else => return null, // string/object/complex — can't compile to numeric WASM
            }
        }
    }

    const result_kind = if (has_float_const) .f64 else kind;
    if (debug) std.debug.print("[wasm-ACCEPT] {s}: tier={s} structs={}\n", .{ func.name, @tagName(result_kind), has_struct_info });
    return result_kind;
}

/// Analyze a function that contains Math.* intrinsic patterns.
/// Skips Math.* setup sequences:
///   Pattern 1: get_var("Math") + get_field2("method") ... call_method(N)
///   Pattern 2: get_var_ref{N} (destructured Math) ... call{N}
/// Checks all remaining opcodes with the standard numeric handler.
/// Math intrinsics always force f64 tier (they operate on doubles).
fn analyzeFunctionWithMath(func: AnalyzedFunction, debug: bool) ?numeric_handlers.ValueKind {
    var has_computing_op = false;
    var math_state = MathSkipState{};
    var i: usize = 0;
    while (i < func.instructions.len) {
        if (skipMathPattern(func, func.instructions, i, &math_state)) |result| {
            if (result.is_setup) has_computing_op = true;
            i = result.new_i;
            continue;
        }

        const instr = func.instructions[i];
        const handler = numeric_handlers.getHandler(instr.opcode);
        if (handler.pattern == .unsupported) {
            // tail_call_method is unsupported in general, but valid as part of Math patterns
            // (already handled above). If we get here, it's a non-Math tail_call_method.
            if (debug) std.debug.print("[wasm-reject] {s}: unsupported opcode '{s}' (math-aware)\n", .{ func.name, @tagName(instr.opcode) });
            return null;
        }

        switch (handler.pattern) {
            .binary_arith, .binary_cmp, .bitwise_binary, .unary, .inc_dec, .post_inc_dec, .lnot, .push_const, .push_cpool, .push_bool, .call_self, .tail_call_self, .add_loc, .inc_loc, .dec_loc => has_computing_op = true,
            .array_get, .array_get2, .array_put, .array_length => has_computing_op = true,
            else => {},
        }

        i += 1;
    }

    if (!has_computing_op) return null;

    // For non-recursive functions, check that self_ref opcodes are ONLY
    // part of Math.* patterns (not standalone function references)
    if (!func.is_self_recursive) {
        var math_state2 = MathSkipState{};
        var si: usize = 0;
        while (si < func.instructions.len) {
            if (skipMathPattern(func, func.instructions, si, &math_state2)) |result| {
                si = result.new_i;
                continue;
            }
            const handler = numeric_handlers.getHandler(func.instructions[si].opcode);
            if (handler.pattern == .self_ref or handler.pattern == .call_self or handler.pattern == .tail_call_self) {
                if (debug) std.debug.print("[wasm-reject] {s}: non-Math self_ref '{s}' in non-recursive function\n", .{ func.name, @tagName(func.instructions[si].opcode) });
                return null;
            }
            si += 1;
        }
    }

    // Validate cpool constants
    for (func.instructions) |instr| {
        if (instr.opcode == .push_const8 or instr.opcode == .push_const) {
            const idx: u32 = switch (instr.operand) {
                .const_idx => |a| a,
                else => return null,
            };
            if (idx >= func.constants.len) return null;
            switch (func.constants[idx]) {
                .int32, .float64 => {},
                else => return null,
            }
        }
    }

    // Math intrinsics always force f64 tier (abs, sqrt, floor, ceil work on doubles)
    return .f64;
}

/// Resolve an atom index to a function name string slice.
/// Used for cross-function call detection (atom → callee name).
fn resolveAtomToName(func: AnalyzedFunction, atom_idx: u32) ?[]const u8 {
    if (atom_idx < module_parser.JS_ATOM_END) {
        if (atom_idx < module_parser.BUILTIN_ATOMS.len) {
            const name = module_parser.BUILTIN_ATOMS[atom_idx];
            if (name.len > 0 and name[0] != '<') return name;
        }
        return null;
    }
    const adjusted_idx = atom_idx - module_parser.JS_ATOM_END;
    if (adjusted_idx < func.atom_strings.len) {
        const str = func.atom_strings[adjusted_idx];
        if (str.len > 0) return str;
    }
    return null;
}

/// Resolve callee name from an instruction (get_var → atom, get_var_ref0 → closure var).
fn resolveCalleeName(func: AnalyzedFunction, instr: bytecode_parser.Instruction) ?[]const u8 {
    if (instr.opcode == .get_var or instr.opcode == .get_var_undef) {
        const atom_val: u32 = switch (instr.operand) { .atom => |a| a, else => return null };
        return resolveAtomToName(func, atom_val);
    } else if (instr.opcode == .get_var_ref0) {
        if (func.closure_vars.len > 0) return func.closure_vars[0].name;
    } else if (instr.opcode == .get_var_ref1) {
        if (func.closure_vars.len > 1) return func.closure_vars[1].name;
    } else if (instr.opcode == .get_var_ref2) {
        if (func.closure_vars.len > 2) return func.closure_vars[2].name;
    } else if (instr.opcode == .get_var_ref3) {
        if (func.closure_vars.len > 3) return func.closure_vars[3].name;
    } else if (instr.opcode == .get_var_ref) {
        const idx: u32 = switch (instr.operand) { .var_ref => |v| v, .u16 => |v| v, .u8 => |v| v, else => return null };
        if (idx < func.closure_vars.len) return func.closure_vars[idx].name;
    }
    return null;
}

/// Like analyzeNumericTier, but accepts non-recursive functions that call
/// other known WASM functions (cross-function calls within the WASM module).
pub fn analyzeNumericTierWithCrossCall(
    func: AnalyzedFunction,
    wasm_func_names: []const []const u8,
) ?numeric_handlers.ValueKind {
    if (func.arg_count < 1 or func.arg_count > 24) return null;
    if (func.var_count > 48) return null;

    const kind = numeric_handlers.analyzeFunction(func.instructions) orelse return null;

    if (!func.is_self_recursive) {
        for (func.instructions, 0..) |instr, ii| {
            const handler = numeric_handlers.getHandler(instr.opcode);
            if (handler.pattern == .self_ref) {
                // Look ahead: is this a function call (→ call_self) or data access (→ get_array_el)?
                // get_var_ref0 is used for BOTH closure function calls AND closure array reads.
                // Only reject if it's actually a function call to a non-WASM function.
                var is_call = false;
                for (func.instructions[ii + 1 ..]) |next_instr| {
                    const nh = numeric_handlers.getHandler(next_instr.opcode);
                    if (nh.pattern == .call_self) { is_call = true; break; }
                    if (next_instr.opcode == .get_array_el or next_instr.opcode == .get_array_el2) break;
                    if (nh.pattern == .self_ref) break; // another ref before call — this one wasn't called
                }
                if (!is_call) continue; // data access (e.g., lookup table), not a function call

                if (resolveCalleeName(func, instr)) |cn| {
                    var found = false;
                    for (wasm_func_names) |wn| {
                        if (std.mem.eql(u8, cn, wn)) {
                            found = true;
                            break;
                        }
                    }
                    if (!found) {
                        const debug = std.posix.getenv("EDGEBOX_WASM_DEBUG") != null;
                        if (debug) std.debug.print("[wasm-cross-reject] {s}: callee '{s}' not in WASM set ({d} funcs)\n", .{ func.name, cn, wasm_func_names.len });
                        return null;
                    }
                } else {
                    const debug = std.posix.getenv("EDGEBOX_WASM_DEBUG") != null;
                    if (debug) std.debug.print("[wasm-cross-reject] {s}: could not resolve callee name\n", .{func.name});
                    return null;
                }
            }
            // call_self/tail_call_self: resolved at codegen time (target determined by pending_callee)
        }
    }

    return kind;
}

// ============================================================================
// Tests
// ============================================================================


