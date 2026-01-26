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
const hashmap_helper = @import("../utils/hashmap_helper.zig");
const jsvalue = @import("zig_jsvalue.zig");
const zig_codegen = @import("zig_codegen.zig");
const opcodes = @import("opcodes.zig");
pub const module_parser = @import("module_parser.zig");
const bytecode_parser = @import("bytecode_parser.zig");
const cfg_builder = @import("cfg_builder.zig");
const zig_hotpath_codegen = @import("zig_hotpath_codegen.zig");
const zig_codegen_full = @import("zig_codegen_full.zig");
const zig_codegen_relooper = @import("zig_codegen_relooper.zig");

const JSValue = jsvalue.JSValue;
const JSContext = jsvalue.JSContext;
const FrozenFn = jsvalue.FrozenFn;
const FrozenEntry = jsvalue.FrozenEntry;
const Allocator = std.mem.Allocator;

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

/// Generate frozen function entries at comptime from bytecode
/// This is the core generic solution - any bytecode → frozen functions
pub fn generateFromBytecode(
    comptime bytecode: []const u8,
    comptime module_name: []const u8,
) []const FrozenEntry {
    comptime {
        // Parse bytecode module header and extract functions
        const funcs = parseModuleFunctions(bytecode);

        // Generate entries for each freezable function
        var entries: [funcs.len]FrozenEntry = undefined;
        var entry_count: usize = 0;

        for (funcs, 0..) |func, idx| {
            // Check if function can be frozen
            if (canFreeze(func.bytecode)) {
                // Generate the frozen function
                const frozen_fn = zig_codegen.generateFrozen(func.bytecode, .{
                    .func_name = func.name,
                    .arg_count = func.arg_count,
                    .var_count = func.var_count,
                    .is_self_recursive = func.is_recursive,
                });

                // Create JS name
                var name_buf: [64]u8 = undefined;
                const js_name = std.fmt.bufPrint(&name_buf, "__frozen_{s}_{d}", .{ module_name, idx }) catch module_name;

                entries[entry_count] = .{
                    .name = js_name,
                    .func = frozen_fn,
                    .argc = func.arg_count,
                };
                entry_count += 1;
            }
        }

        return entries[0..entry_count];
    }
}

/// Function metadata extracted from bytecode
const FunctionMeta = struct {
    name: []const u8,
    bytecode: []const u8,
    arg_count: u8,
    var_count: u8,
    is_recursive: bool,
};

/// Parse QuickJS module to extract function metadata (comptime version)
/// NOTE: Comptime module parsing is not implemented - use runtime FrozenRegistry instead.
/// Runtime parsing via module_parser.zig is fully functional.
/// Comptime parsing would require porting the runtime parser to work at compile time,
/// which involves handling dynamic allocations and complex state - significant effort
/// for minimal benefit since frozen functions are already generated at build time.
fn parseModuleFunctions(comptime bytecode: []const u8) []const FunctionMeta {
    comptime {
        // Return empty - comptime frozen functions not supported
        // Use FrozenRegistry.initFromBytecode() for runtime frozen function registration
        _ = bytecode;
        return &.{};
    }
}

/// Check if bytecode can be frozen (no closures, async, etc.)
/// Comptime version for build-time generation
fn canFreeze(comptime bytecode: []const u8) bool {
    return canFreezeImpl(bytecode);
}

/// Implementation that works at both comptime and runtime
fn canFreezeImpl(bytecode: []const u8) bool {
    // Check each instruction for unfrozen opcodes
    var pc: usize = 0;
    while (pc < bytecode.len) {
        const op: opcodes.Opcode = @enumFromInt(bytecode[pc]);
        const info = opcodes.getInfo(op);

        // Check category
        if (info.category == .never_freeze) {
            return false;
        }

        pc += info.size;
    }
    return true;
}

// ============================================================================
// Build-time Configuration
// ============================================================================

/// Frozen entries generated at build time
/// In production: populated by build.zig from embedded bytecode
/// For now: empty (manual functions only during development)
const generated_entries: []const FrozenEntry = &.{};

// ============================================================================
// Registration API
// ============================================================================

/// Initialize all frozen functions (single ABI call)
/// Called once at startup
pub fn init(ctx: *JSContext) void {
    // Register all generated frozen functions
    for (generated_entries) |entry| {
        registerOne(ctx, entry);
    }
}

/// Register a single frozen function
fn registerOne(ctx: *JSContext, entry: FrozenEntry) void {
    const global = jsvalue.JS_GetGlobalObject(ctx);
    defer jsvalue.JS_FreeValue(ctx, global);

    const func_val = jsvalue.JS_NewCFunction2(ctx, entry.func, entry.name, entry.argc, 0, 0);
    _ = jsvalue.JS_SetPropertyStr(ctx, global, entry.name, func_val);
}

// NOTE: frozen_init is provided by frozen_functions.c (generated by freeze tool)
// The C version registers all project-specific frozen functions
// This Zig module only provides runtime helpers, not the init function

// ============================================================================
// Runtime API (for dynamic registration)
// ============================================================================

/// Register additional frozen functions at runtime
pub fn registerMany(ctx: *JSContext, entries: []const FrozenEntry) void {
    for (entries) |entry| {
        registerOne(ctx, entry);
    }
}

/// Get count of registered frozen functions
pub fn count() usize {
    return generated_entries.len;
}

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
    /// Whether function calls itself recursively
    is_self_recursive: bool,
    /// Index of self-reference in closure vars (-1 if none)
    self_ref_var_idx: i16 = -1,
    /// Whether function can be frozen
    can_freeze: bool,
    /// Reason if cannot freeze
    freeze_block_reason: ?[]const u8,
    /// Constants from bytecode (owned copy)
    constants: []const module_parser.ConstValue,
    /// Atom strings from module (shared reference to ModuleAnalysis.atom_strings)
    atom_strings: []const []const u8,
    /// Closure variables (owned copy of names and metadata)
    closure_vars: []const module_parser.ClosureVarInfo = &.{},
    /// Line number from debug info (for name@line_num dispatch key)
    line_num: u32 = 0,
    /// Explicit "use strict" directive (for proper 'this' handling)
    has_use_strict: bool = false,
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

    // Analyze each function
    for (parser.functions.items) |func_info| {
        // Parse instructions - skip functions that fail to parse
        var bc_parser = bytecode_parser.BytecodeParser.init(func_info.bytecode);
        const instructions = bc_parser.parseAll(allocator) catch |err| {
            // Some functions may have malformed bytecode (e.g., from scanning)
            // Skip them gracefully instead of failing the entire freeze
            if (false) { // Set to true for debugging
                std.debug.print("[freeze] Skipping function with parse error: {}\n", .{err});
            }
            continue;
        };

        // Check if can freeze
        const freeze_check = bytecode_parser.canFreezeFunction(instructions);

        // Self-recursion detection is DISABLED for now.
        // Get function name - must duplicate since parser will be freed
        const parser_name = parser.getAtomString(func_info.name_atom) orelse "anonymous";

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
        var self_ref_var_idx: i16 = -1;
        for (func_info.closure_vars, 0..) |cv, idx| {
            if (std.mem.eql(u8, cv.name, parser_name)) {
                is_self_recursive = true;
                self_ref_var_idx = @intCast(idx);
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
            };
        }

        try result.functions.append(allocator, .{
            .name = name,
            .bytecode = func_info.bytecode,
            .instructions = instructions,
            .arg_count = func_info.arg_count,
            .var_count = func_info.var_count,
            .is_self_recursive = is_self_recursive,
            .self_ref_var_idx = self_ref_var_idx,
            .can_freeze = can_freeze_final,
            .freeze_block_reason = freeze_check.reason,
            .constants = constants_copy,
            .atom_strings = atom_strings_copy, // Share reference
            .closure_vars = closure_vars_copy,
            .line_num = func_info.line_num, // For name@line_num dispatch key
            .has_use_strict = func_info.has_use_strict, // For proper 'this' handling
        });

        if (can_freeze_final) {
            result.freezable_count += 1;
        }
    }

    return result;
}

// Debug flag - set to true for verbose freeze logging
const FREEZE_DEBUG = true;

/// Quick scan for killer opcodes that prevent freezing
/// This avoids expensive CFG building for functions that will be skipped anyway
/// NOTE: Closure READ, WRITE, and CREATION are now supported via var_refs passed from native_dispatch
fn hasKillerOpcodes(instructions: []const bytecode_parser.Instruction, is_self_recursive: bool) bool {
    _ = is_self_recursive; // No longer used
    for (instructions) |instr| {
        switch (instr.opcode) {
            // NOTE: Closure READ (.get_var_ref*) is now ALLOWED - we pass var_refs from dispatch
            // NOTE: Closure WRITE (.put_var_ref*, .set_var_ref*) is now ALLOWED - we use setClosureVar
            // NOTE: Closure CREATION (.fclosure*) is now ALLOWED - we use js_frozen_create_closure
            // Eval/with - dynamic scope
            .eval, .with_get_var, .with_put_var, .with_delete_var,
            // Generators/async
            .initial_yield, .yield, .yield_star, .await,
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

/// Generate frozen Zig code for a single function
/// Returns null if function cannot be frozen
pub fn generateFrozenZig(
    allocator: Allocator,
    func: AnalyzedFunction,
    func_index: usize,
) !?[]u8 {
    // FAST-FAIL: Check for killer opcodes before building expensive CFG
    // Skip functions with ANY closure access (get_var_ref*, put_var_ref*, fclosure*)
    if (hasKillerOpcodes(func.instructions, func.is_self_recursive)) {
        if (FREEZE_DEBUG) std.debug.print("[freeze-zig] Fast-skip '{s}': has killer opcodes\n", .{func.name});
        return null;
    }

    // Build CFG from instructions
    // If CFG building fails due to stack depth conflicts, the function cannot be frozen
    var cfg = cfg_builder.buildCFG(allocator, func.instructions) catch |err| {
        if (err == error.StackDepthConflict) {
            if (FREEZE_DEBUG) std.debug.print("[freeze-zig] Skip '{s}': stack depth conflict in CFG\n", .{func.name});
            return null;
        }
        return err;
    };
    defer cfg.deinit();

    // Run contamination analysis for partial freeze support
    if (FREEZE_DEBUG) std.debug.print("[freeze-zig] Analyzing '{s}' for Zig codegen\n", .{func.name});
    cfg_builder.analyzeContamination(&cfg);

    // Check if we can freeze (fully or partially)
    const counts = cfg_builder.countBlocks(&cfg);
    const has_clean_blocks = counts.clean > 0;
    const has_contaminated_blocks = counts.contaminated > 0;

    // If no clean blocks at all, can't freeze
    if (!has_clean_blocks) return null;

    // Determine if this is a partial freeze (some contaminated blocks)
    const partial_freeze = has_contaminated_blocks;

    // For native-static (Zig) builds, skip partial freeze functions entirely
    // The fallback to interpreter won't work in native-static mode since there's no WAMR
    if (partial_freeze) {
        if (FREEZE_DEBUG) std.debug.print("[freeze] Skipping Zig codegen for '{s}': partial freeze not supported in native-static\n", .{func.name});
        return null;
    }

    // Analyze closure variables for this function
    var closure_usage = try cfg_builder.analyzeClosureVars(allocator, func.instructions);
    defer closure_usage.deinit(allocator);

    // For native-static, closure read AND write access is ALLOWED via var_refs from dispatch
    // ALLOW: self-recursive functions (self-ref handled via direct recursion)
    // ALLOW: closure access (common in tsc - captured vars from outer scope)
    // closure_usage.write_indices and closure_usage.all_indices are both passed to codegen

    // Format function name with index for uniqueness (no prefix, codegen adds __frozen_)
    // Sanitize the name to be a valid Zig identifier (replace colons, etc.)
    var sanitized_buf: [256]u8 = undefined;
    const sanitized_name = sanitizeName(func.name, &sanitized_buf);
    var name_buf: [256]u8 = undefined;
    const indexed_name = std.fmt.bufPrint(&name_buf, "{d}_{s}", .{ func_index, sanitized_name }) catch func.name;

    // Check if this is a pure int32 function - use hot path for 18x speedup!
    if (isPureInt32Function(func) and !partial_freeze) {
        // Use func_X_name format to ensure valid Zig identifier (no leading numbers)
        var hot_name_buf: [256]u8 = undefined;
        const hot_name = std.fmt.bufPrint(&hot_name_buf, "func_{d}_{s}", .{ func_index, sanitized_name }) catch func.name;

        // Generate pure int32 Zig hot path + JSValue wrapper
        var hot_gen = zig_hotpath_codegen.ZigHotPathGen.init(allocator, .{
            .name = hot_name,
            .arg_count = @intCast(func.arg_count),
            .cfg = &cfg,
            .is_self_recursive = func.is_self_recursive,
        });
        defer hot_gen.deinit();

        const hot_code = hot_gen.generate() catch |err| {
            // Fall back to general codegen if hot path fails
            std.debug.print("[freeze] Int32 hot path failed for '{s}': {}, falling back\n", .{ func.name, err });
            return generateFrozenZigGeneral(allocator, &cfg, indexed_name, func, partial_freeze, closure_usage.all_indices);
        };

        // Generate JSValue wrapper that calls the hot path
        var output = std.ArrayListUnmanaged(u8){};
        errdefer output.deinit(allocator);

        // Add the hot function
        try output.appendSlice(allocator, hot_code);
        try output.appendSlice(allocator, "\n");

        // Generate wrapper: extracts int32 args, calls hot func, boxes result
        try output.appendSlice(allocator, "pub fn __frozen_");
        try output.appendSlice(allocator, indexed_name);
        try output.appendSlice(allocator, "(ctx: *zig_runtime.JSContext, _: zig_runtime.JSValue, argc: c_int, argv: [*]zig_runtime.JSValue, var_refs: ?[*]*zig_runtime.JSVarRef, closure_var_count: c_int, cpool: ?[*]zig_runtime.JSValue) callconv(.c) zig_runtime.JSValue {\n");
        try output.appendSlice(allocator, "    _ = ctx;\n");
        try output.appendSlice(allocator, "    _ = var_refs;\n");
        try output.appendSlice(allocator, "    _ = closure_var_count;\n");
        try output.appendSlice(allocator, "    _ = cpool;\n");

        // Extract each argument as int32
        for (0..func.arg_count) |i| {
            var arg_buf: [128]u8 = undefined;
            const arg_line = std.fmt.bufPrint(&arg_buf,
                "    const n{d}: i32 = if ({d} < argc) argv[{d}].getInt() else 0;\n",
                .{ i, i, i }) catch continue;
            try output.appendSlice(allocator, arg_line);
        }

        // Call hot function (use hot_name which is a valid identifier)
        try output.appendSlice(allocator, "    const result = ");
        try output.appendSlice(allocator, hot_name);
        try output.appendSlice(allocator, "_hot(");
        for (0..func.arg_count) |i| {
            if (i > 0) try output.appendSlice(allocator, ", ");
            var n_buf: [16]u8 = undefined;
            const n_str = std.fmt.bufPrint(&n_buf, "n{d}", .{i}) catch "n0";
            try output.appendSlice(allocator, n_str);
        }
        try output.appendSlice(allocator, ");\n");
        try output.appendSlice(allocator, "    return zig_runtime.JSValue.newInt(result);\n");
        try output.appendSlice(allocator, "}\n");

        return try output.toOwnedSlice(allocator);
    }

    // General codegen for non-int32 functions
    // Pass closure_var_indices for read-only closure variable access
    return generateFrozenZigGeneral(allocator, &cfg, indexed_name, func, partial_freeze, closure_usage.all_indices);
}

/// General Zig codegen for functions that aren't pure int32
fn generateFrozenZigGeneral(
    allocator: Allocator,
    cfg: *cfg_builder.CFG,
    indexed_name: []const u8,
    func: AnalyzedFunction,
    partial_freeze: bool,
    closure_var_indices: []const u16,
) !?[]u8 {
    // Use the general Zig codegen
    // Check if this is a pure int32 function (fib-like) for optimization
    const is_pure_int32 = isPureInt32Function(func) and !partial_freeze;
    if (is_pure_int32) {
        if (FREEZE_DEBUG) std.debug.print("[freeze] {s}: detected as pure int32 function\n", .{func.name});
    }

    // Check for patterns that need Relooper: switch statements, complex control flow
    const use_relooper = blk: {
        // Switch patterns need Relooper for native Zig switch emission
        if (cfg_builder.hasSwitchPattern(cfg)) {
            if (FREEZE_DEBUG) std.debug.print("[freeze] {s}: switch pattern detected\n", .{func.name});
            break :blk true;
        }
        // put_array_el has stack ordering issues in block dispatch mode - use Relooper
        for (cfg.blocks.items) |block| {
            for (block.instructions) |instr| {
                if (instr.opcode == .put_array_el) {
                    if (FREEZE_DEBUG) std.debug.print("[freeze] {s}: put_array_el detected, using Relooper\n", .{func.name});
                    break :blk true;
                }
            }
        }
        // Complex control flow (multiple sibling loops, contaminated loops)
        const natural_loops = cfg_builder.detectNaturalLoops(cfg, allocator) catch break :blk false;
        const has_complex = cfg_builder.hasComplexControlFlow(cfg, natural_loops);
        // Clean up
        for (natural_loops) |loop| {
            allocator.free(loop.body_blocks);
        }
        allocator.free(natural_loops);
        if (has_complex) {
            if (FREEZE_DEBUG) std.debug.print("[freeze] {s}: complex control flow detected\n", .{func.name});
            break :blk true;
        }
        break :blk false;
    };

    if (use_relooper) {
        if (FREEZE_DEBUG) std.debug.print("[freeze] {s}: using Relooper\n", .{func.name});
        return zig_codegen_relooper.generateRelooper(allocator, .{
            .name = indexed_name,
            .arg_count = @intCast(func.arg_count),
            .var_count = @intCast(func.var_count),
            .cfg = cfg,
            .is_self_recursive = func.is_self_recursive,
            .self_ref_var_idx = func.self_ref_var_idx,
            .closure_var_indices = closure_var_indices, // CRITICAL: Pass closure indices for proper var_ref handling
            .atom_strings = func.atom_strings,
            .partial_freeze = partial_freeze,
            .js_name = func.name,
            .is_pure_int32 = is_pure_int32,
            .has_use_strict = func.has_use_strict, // For proper 'this' handling
        }) catch |err| {
            std.debug.print("[freeze] Relooper codegen error for '{s}': {}\n", .{ func.name, err });
            return null;
        };
    }

    // CV (CompressedValue) is the ONLY path - 8-byte NaN-boxed, 32-bit int/ptr inline
    var gen = zig_codegen_full.ZigCodeGen.init(allocator, .{
        .name = indexed_name,
        .arg_count = @intCast(func.arg_count),
        .var_count = @intCast(func.var_count),
        .cfg = cfg,
        .is_self_recursive = func.is_self_recursive,
        .self_ref_var_idx = func.self_ref_var_idx,
        .atom_strings = func.atom_strings,
        .partial_freeze = partial_freeze,
        .js_name = func.name, // Original JS name for fallback registration
        .is_pure_int32 = is_pure_int32, // Enable int32 specialization for fib-like functions
        .closure_var_indices = closure_var_indices, // Pass closure indices for proper var_ref handling
        .has_use_strict = func.has_use_strict, // For proper 'this' handling
    });
    defer gen.deinit();

    const zig_code = gen.generate() catch |err| {
        std.debug.print("[freeze] Zig codegen error for '{s}': {}\n", .{ func.name, err });
        return null;
    };

    // Return the generated code as mutable slice (already owned by caller via toOwnedSlice)
    // Note: toOwnedSlice returns []u8 but we store as []const u8
    return @constCast(zig_code);
}

/// Sharded output for parallel compilation
pub const ShardedOutput = struct {
    /// Main file that imports all shards and has init function
    main: []u8,
    /// Individual shard files (frozen_shard_0.zig, frozen_shard_1.zig, etc.)
    shards: [][]u8,
    /// Target size per shard in bytes
    bytes_per_shard: usize,

    pub fn deinit(self: *ShardedOutput, allocator: Allocator) void {
        allocator.free(self.main);
        for (self.shards) |shard| {
            allocator.free(shard);
        }
        allocator.free(self.shards);
    }
};

/// Generate frozen Zig code with sharding for parallel compilation
/// Returns multiple shard files + main file for large codebases
pub fn generateModuleZigSharded(
    allocator: Allocator,
    analysis: *const ModuleAnalysis,
    module_name: []const u8,
    manifest_json: ?[]const u8,
    bytes_per_shard: usize,
) !ShardedOutput {
    _ = module_name;

    // NOTE: We no longer use manifest for filtering - we freeze ALL freezable functions.
    // The manifest was previously used to filter which functions to freeze, but that caused
    // top-level functions without closures (like getPrimeFactors) to not be frozen.
    _ = manifest_json;

    // First pass: collect all functions to generate and their code
    const GeneratedFunc = struct {
        func: AnalyzedFunction,
        idx: usize,
        code: []u8,
    };
    var generated_all = std.ArrayListUnmanaged(GeneratedFunc){};
    defer {
        for (generated_all.items) |gf| allocator.free(gf.code);
        generated_all.deinit(allocator);
    }

    for (analysis.functions.items, 0..) |func, idx| {
        // Generate the function code for ALL freezable functions
        const result = generateFrozenZig(allocator, func, idx) catch continue;
        if (result) |zig_code| {
            try generated_all.append(allocator, .{ .func = func, .idx = idx, .code = zig_code });
        }
    }

    // Calculate total size and number of shards needed
    var total_size: usize = 0;
    for (generated_all.items) |gf| {
        total_size += gf.code.len;
    }
    const num_shards = if (total_size == 0) 1 else @max(1, (total_size + bytes_per_shard - 1) / bytes_per_shard);

    std.debug.print("[freeze] Sharding {d} functions ({d} KB) into ~{d} shards (~{d} KB each)\n", .{
        generated_all.items.len,
        total_size / 1024,
        num_shards,
        bytes_per_shard / 1024,
    });

    // Size-based sharding: distribute functions to keep shards under size limit
    var shard_contents = std.ArrayListUnmanaged(std.ArrayListUnmanaged(u8)){};
    defer {
        for (shard_contents.items) |*sc| sc.deinit(allocator);
        shard_contents.deinit(allocator);
    }

    // Track generated functions for init
    var generated_funcs = std.ArrayListUnmanaged(struct { name: []const u8, idx: usize, arg_count: u32, shard: usize, line_num: u32 }){};
    defer generated_funcs.deinit(allocator);

    const shard_header =
        \\//! Auto-generated frozen functions shard
        \\//! DO NOT EDIT - generated by EdgeBox freeze system
        \\
        \\const std = @import("std");
        \\const zig_runtime = @import("zig_runtime");
        \\const math_polyfill = @import("math_polyfill");
        \\const JSValue = zig_runtime.JSValue;
        \\const JSContext = zig_runtime.JSContext;
        \\
        \\// Increase eval quota for large shards (date-fns, etc.)
        \\comptime { @setEvalBranchQuota(1000000); }
        \\
        \\
    ;

    // Start first shard
    var current_shard = std.ArrayListUnmanaged(u8){};
    try current_shard.appendSlice(allocator, shard_header);
    var current_shard_idx: usize = 0;

    for (generated_all.items) |gf| {
        // Check if adding this function would exceed shard size
        // (except for the first function in a shard - always add at least one)
        if (current_shard.items.len > shard_header.len and
            current_shard.items.len + gf.code.len > bytes_per_shard)
        {
            // Finalize current shard
            try shard_contents.append(allocator, current_shard);
            current_shard_idx += 1;
            // Start new shard
            current_shard = std.ArrayListUnmanaged(u8){};
            try current_shard.appendSlice(allocator, shard_header);
        }

        // Add function to current shard
        try current_shard.appendSlice(allocator, gf.code);
        try current_shard.appendSlice(allocator, "\n");
        try generated_funcs.append(allocator, .{
            .name = gf.func.name,
            .idx = gf.idx,
            .arg_count = gf.func.arg_count,
            .shard = current_shard_idx,
            .line_num = gf.func.line_num,
        });
    }

    // Finalize last shard
    try shard_contents.append(allocator, current_shard);
    const actual_num_shards = shard_contents.items.len;

    // Convert to output format
    var shards = try allocator.alloc([]u8, actual_num_shards);
    errdefer {
        for (shards, 0..) |shard, i| {
            if (i < shards.len and shard.len > 0) allocator.free(shard);
        }
        allocator.free(shards);
    }

    for (shard_contents.items, 0..) |*sc, i| {
        shards[i] = try sc.toOwnedSlice(allocator);
    }

    std.debug.print("[freeze] Generated {d} functions across {d} shards\n", .{ generated_funcs.items.len, actual_num_shards });
    zig_codegen_full.printUnsupportedOpcodeReport();

    // Count ALL function names in the module (not just frozen ones)
    // This prevents name collision: if there are 2 functions named "foo",
    // one frozen and one not, we'd intercept calls to both with our frozen version
    var name_counts = hashmap_helper.StringHashMap(u32).init(allocator);
    defer name_counts.deinit();
    // Also track anonymous function line numbers for collision detection
    var anon_line_counts = std.AutoHashMap(u32, u32).init(allocator);
    defer anon_line_counts.deinit();
    for (analysis.functions.items) |func| {
        if (std.mem.eql(u8, func.name, "anonymous")) {
            const entry = try anon_line_counts.getOrPut(func.line_num);
            if (entry.found_existing) {
                entry.value_ptr.* += 1;
            } else {
                entry.value_ptr.* = 1;
            }
            continue;
        }
        const entry = try name_counts.getOrPut(func.name);
        if (entry.found_existing) {
            entry.value_ptr.* += 1;
        } else {
            entry.value_ptr.* = 1;
        }
    }

    // Generate main file with imports and init
    var main_output = std.ArrayListUnmanaged(u8){};
    errdefer main_output.deinit(allocator);

    try main_output.appendSlice(allocator,
        \\//! Auto-generated frozen functions main module
        \\//! DO NOT EDIT - generated by EdgeBox freeze system
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

    // Import all shards
    for (0..actual_num_shards) |i| {
        var import_buf: [128]u8 = undefined;
        const import_line = std.fmt.bufPrint(&import_buf, "const shard_{d} = @import(\"frozen_shard_{d}.zig\");\n", .{ i, i }) catch continue;
        try main_output.appendSlice(allocator, import_line);
    }

    // Native registry functions and init
    try main_output.appendSlice(allocator,
        \\
        \\// Native registry functions (from frozen_runtime.c)
        \\extern fn native_registry_init() void;
        \\extern fn native_registry_count() c_int;
        \\extern fn native_node_register32(js_addr32: u32, kind: i32, flags: i32, pos: i32, end: i32) ?*anyopaque;
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
        \\    const ptr_addr = @intFromPtr(obj.getPtr());
        \\    const addr32: u32 = @truncate(ptr_addr);
        \\    const node = native_node_register32(addr32, kind, flags, pos, end);
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
        \\    _ = &count;
        \\
        \\    native_registry_init();
        \\    _ = qjs.JS_SetPropertyStr(ctx, global, "__edgebox_register_node",
        \\        qjs.JS_NewCFunction(ctx, @ptrCast(&registerNodeImpl), "__edgebox_register_node", 5));
        \\    _ = qjs.JS_SetPropertyStr(ctx, global, "__edgebox_registry_count",
        \\        qjs.JS_NewCFunction(ctx, @ptrCast(&registryCountImpl), "__edgebox_registry_count", 0));
        \\    math_polyfill.register(ctx);
        \\
    );

    // Register each generated function with native dispatch using name@line_num key
    for (generated_funcs.items) |gf| {
        const is_anonymous = std.mem.eql(u8, gf.name, "anonymous");

        // Skip if collides with another function of same name (named) or same line (anonymous)
        if (is_anonymous) {
            if ((anon_line_counts.get(gf.line_num) orelse 0) > 1) continue;
        } else {
            if ((name_counts.get(gf.name) orelse 0) > 1) continue;
        }

        // Use gf.line_num directly (now available in the struct)
        const line_num = gf.line_num;

        var reg_buf: [512]u8 = undefined;
        // Sanitize name for Zig identifier (replace colons, etc.)
        var sanitized_buf: [256]u8 = undefined;
        const sanitized_name = sanitizeName(gf.name, &sanitized_buf);
        // Use name@line_num format to match QuickJS dispatch key
        const reg_line = std.fmt.bufPrint(&reg_buf,
            \\    // Register {s}@{d} with native dispatch
            \\    native_dispatch.register("{s}@{d}", &shard_{d}.__frozen_{d}_{s});
            \\    count += 1;
            \\
        , .{ gf.name, line_num, gf.name, line_num, gf.shard, gf.idx, sanitized_name }) catch continue;
        try main_output.appendSlice(allocator, reg_line);
    }

    // Enable dispatch after all registrations
    try main_output.appendSlice(allocator,
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
        .shards = shards,
        .bytes_per_shard = bytes_per_shard,
    };
}

/// Generate frozen Zig code for all freezable functions in a module
/// Returns a single Zig module with all functions
/// If manifest_json is provided, only generates functions that match the manifest
pub fn generateModuleZig(
    allocator: Allocator,
    analysis: *const ModuleAnalysis,
    module_name: []const u8,
    manifest_json: ?[]const u8,
) ![]u8 {
    _ = module_name;
    var output = std.ArrayListUnmanaged(u8){};
    errdefer output.deinit(allocator);

    // Parse manifest if provided - only generate functions that match
    var manifest: []ManifestFunction = &.{};
    defer if (manifest.len > 0) freeManifest(allocator, manifest);
    if (manifest_json) |json| {
        manifest = parseManifest(allocator, json) catch &.{};
    }

    // Header
    try output.appendSlice(allocator,
        \\//! Auto-generated frozen functions module
        \\//! DO NOT EDIT - generated by EdgeBox freeze system
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

    // Track which functions were actually generated (for registration later)
    var generated_indices = std.AutoHashMap(usize, void).init(allocator);
    defer generated_indices.deinit();

    // Debug: find all mandelbrot_compute functions
    if (FREEZE_DEBUG) std.debug.print("[freeze-zig] Searching for mandelbrot_compute in {d} functions:\n", .{analysis.functions.items.len});
    for (analysis.functions.items, 0..) |f, fi| {
        if (std.mem.eql(u8, f.name, "mandelbrot_compute")) {
            if (FREEZE_DEBUG) std.debug.print("  Found at index {d}: {d} instructions\n", .{ fi, f.instructions.len });
        }
    }

    // Generate each function
    var gen_count: usize = 0;
    for (analysis.functions.items, 0..) |func, idx| {
        // If manifest is provided, only generate manifest functions
        // This dramatically reduces compilation time for large bundles
        if (manifest.len > 0) {
            var is_manifest_func = false;
            for (manifest) |mf| {
                if (std.mem.eql(u8, mf.name, func.name)) {
                    is_manifest_func = true;
                    break;
                }
            }
            if (!is_manifest_func) continue;
        }

        if (FREEZE_DEBUG) std.debug.print("[freeze-zig] Generating '{s}' at index {d} (instr_count={d})\n", .{ func.name, idx, func.instructions.len });
        const result = generateFrozenZig(allocator, func, idx) catch |err| {
            if (FREEZE_DEBUG) std.debug.print("[freeze] Error generating Zig for '{s}': {}\n", .{ func.name, err });
            continue;
        };
        if (result) |zig_code| {
            defer allocator.free(zig_code);
            try output.appendSlice(allocator, zig_code);
            try output.appendSlice(allocator, "\n");
            gen_count += 1;
            try generated_indices.put(idx, {});
        }
    }

    std.debug.print("[freeze] Generated {d} Zig functions\n", .{gen_count});

    // Print report of any unsupported opcodes encountered
    zig_codegen_full.printUnsupportedOpcodeReport();

    // Count ALL function names (not just freezable ones) to prevent name collision
    // If there are 2 functions named "foo", one freezable and one not,
    // we'd intercept calls to both with our frozen version - must skip both
    var name_counts = hashmap_helper.StringHashMap(u32).init(allocator);
    defer name_counts.deinit();
    // Also track anonymous function line numbers for collision detection
    var anon_line_counts = std.AutoHashMap(u32, u32).init(allocator);
    defer anon_line_counts.deinit();
    for (analysis.functions.items) |func| {
        if (std.mem.eql(u8, func.name, "anonymous")) {
            const entry = try anon_line_counts.getOrPut(func.line_num);
            if (entry.found_existing) {
                entry.value_ptr.* += 1;
            } else {
                entry.value_ptr.* = 1;
            }
            continue;
        }
        const entry = try name_counts.getOrPut(func.name);
        if (entry.found_existing) {
            entry.value_ptr.* += 1;
        } else {
            entry.value_ptr.* = 1;
        }
    }

    // Generate init function that registers all frozen functions
    try output.appendSlice(allocator,
        \\
        \\// Native registry functions (from frozen_runtime.c)
        \\extern fn native_registry_init() void;
        \\extern fn native_registry_count() c_int;
        \\extern fn native_node_register32(js_addr32: u32, kind: i32, flags: i32, pos: i32, end: i32) ?*anyopaque;
        \\
        \\/// __edgebox_register_node - register a node in native registry
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
        \\    // Extract object pointer address as 32-bit hash key
        \\    const ptr_addr = @intFromPtr(obj.getPtr());
        \\    const addr32: u32 = @truncate(ptr_addr);
        \\    const node = native_node_register32(addr32, kind, flags, pos, end);
        \\    return if (node != null) zig_runtime.JSValue.TRUE else zig_runtime.JSValue.FALSE;
        \\}
        \\
        \\/// __edgebox_registry_count - get number of registered nodes
        \\fn registryCountImpl(_: *zig_runtime.JSContext, _: zig_runtime.JSValue, _: c_int, _: [*]zig_runtime.JSValue) callconv(.c) zig_runtime.JSValue {
        \\    return zig_runtime.JSValue.newInt(native_registry_count());
        \\}
        \\
        \\/// Initialize frozen functions - register them with QuickJS
        \\pub fn frozen_init(ctx: *zig_runtime.JSContext) c_int {
        \\    const qjs = zig_runtime.quickjs;
        \\    const global = qjs.JS_GetGlobalObject(ctx);
        \\    defer qjs.JS_FreeValue(ctx, global);
        \\    var count: c_int = 0;
        \\    _ = &count; // Silence unused warning when no functions are registered
        \\
        \\    // Initialize native node registry
        \\    native_registry_init();
        \\
        \\    // Register native shape functions
        \\    _ = qjs.JS_SetPropertyStr(ctx, global, "__edgebox_register_node",
        \\        qjs.JS_NewCFunction(ctx, @ptrCast(&registerNodeImpl), "__edgebox_register_node", 5));
        \\    _ = qjs.JS_SetPropertyStr(ctx, global, "__edgebox_registry_count",
        \\        qjs.JS_NewCFunction(ctx, @ptrCast(&registryCountImpl), "__edgebox_registry_count", 0));
        \\
        \\    // Register native Math polyfill (replaces QuickJS Math with native Zig implementations)
        \\    math_polyfill.register(ctx);
        \\
    );

    // Register each generated function with native dispatch using name@line_num key
    // QuickJS dispatches using "name@line_num" format (see quickjs.c:16598)
    for (analysis.functions.items, 0..) |func, idx| {
        // Only register functions that were actually generated (skip partial freeze, etc.)
        if (!generated_indices.contains(idx)) continue;
        if (!func.can_freeze) continue;

        const is_anonymous = std.mem.eql(u8, func.name, "anonymous");

        // Skip if collides with another function of same name (named) or same line (anonymous)
        if (is_anonymous) {
            if ((anon_line_counts.get(func.line_num) orelse 0) > 1) {
                if (FREEZE_DEBUG) std.debug.print("[freeze] Skipping duplicate anonymous at line: {d}\n", .{func.line_num});
                continue;
            }
        } else {
            if ((name_counts.get(func.name) orelse 0) > 1) {
                if (FREEZE_DEBUG) std.debug.print("[freeze] Skipping duplicate name: '{s}'\n", .{func.name});
                continue;
            }
        }

        var reg_buf: [512]u8 = undefined;
        // Sanitize name for Zig identifier (replace colons, etc.)
        var sanitized_buf: [256]u8 = undefined;
        const sanitized_name = sanitizeName(func.name, &sanitized_buf);
        // Use name@line_num format to match QuickJS dispatch key
        const reg_line = std.fmt.bufPrint(&reg_buf,
            \\    // Register {s}@{d} with native dispatch
            \\    native_dispatch.register("{s}@{d}", &__frozen_{d}_{s});
            \\    count += 1;
            \\
        , .{ func.name, func.line_num, func.name, func.line_num, idx, sanitized_name }) catch continue;
        try output.appendSlice(allocator, reg_line);
    }

    // Enable dispatch after all registrations
    try output.appendSlice(allocator,
        \\    native_dispatch.enableDispatch();
        \\    return count;
        \\}
        \\
        \\/// C-callable export for native builds (called from patched bundle_compiled.c)
        \\pub export fn frozen_init_c(ctx: *zig_runtime.JSContext) callconv(.c) c_int {
        \\    return frozen_init(ctx);
        \\}
        \\
    );

    return output.toOwnedSlice(allocator);
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
/// Returns true if all operations are int32-safe
pub fn isPureInt32Function(func: AnalyzedFunction) bool {
    // Must be self-recursive with 1-8 args
    if (!func.is_self_recursive) return false;
    if (func.arg_count < 1 or func.arg_count > 8) return false;

    // Check all instructions for non-int32 operations
    for (func.instructions) |instr| {
        const handler = @import("int32_handlers.zig").getInt32Handler(instr.opcode);
        if (handler.pattern == .unsupported) return false;
    }
    return true;
}

// ============================================================================
// Tests
// ============================================================================

test "canFreeze" {
    // Simple bytecode: push_1, return (should be freezable)
    const simple = [_]u8{
        @intFromEnum(opcodes.Opcode.push_1),
        @intFromEnum(opcodes.Opcode.@"return"),
    };
    try std.testing.expect(canFreeze(&simple));
}

test "empty registry" {
    try std.testing.expectEqual(@as(usize, 0), count());
}

test "generateFrozenZig simple function" {
    const allocator = std.testing.allocator;

    // Create a simple double(x) { return x * 2 } function
    // Bytecode: get_arg 0, push_i8 2, mul, return
    const instrs = &[_]bytecode_parser.Instruction{
        .{ .pc = 0, .opcode = .get_arg, .operand = .{ .arg = 0 }, .size = 2 },
        .{ .pc = 2, .opcode = .push_i8, .operand = .{ .i8 = 2 }, .size = 2 },
        .{ .pc = 4, .opcode = .mul, .operand = .{ .none = {} }, .size = 1 },
        .{ .pc = 5, .opcode = .@"return", .operand = .{ .none = {} }, .size = 1 },
    };

    const func = AnalyzedFunction{
        .name = "double",
        .bytecode = &.{},
        .instructions = instrs,
        .arg_count = 1,
        .var_count = 0,
        .is_self_recursive = false,
        .can_freeze = true,
        .freeze_block_reason = null,
        .constants = &.{},
        .atom_strings = &.{},
    };

    // Generate Zig code
    const result = try generateFrozenZig(allocator, func, 0);
    defer if (result) |zig_code| allocator.free(zig_code);

    // Verify we got some code
    try std.testing.expect(result != null);
    const zig_code = result.?;
    try std.testing.expect(zig_code.len > 0);

    // Verify it contains expected patterns
    try std.testing.expect(std.mem.indexOf(u8, zig_code, "__frozen_0_double") != null);
    try std.testing.expect(std.mem.indexOf(u8, zig_code, "zig_runtime") != null);

    // Print for debugging
    std.debug.print("\n=== Generated Zig ===\n{s}\n=== End ===\n", .{zig_code});
}
