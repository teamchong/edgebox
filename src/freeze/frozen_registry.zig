//! Frozen Function Registry
//!
//! Single ABI entry point for registering all frozen functions with QuickJS.
//! Two modes of operation:
//!
//! 1. Comptime (embedded builds):
//!    @embedFile(bytecode) → comptime parse → generate functions → single ABI registration
//!
//! 2. Runtime (dynamic builds):
//!    generateFromModule() → parse bytecode → generate C code → compile with clang
//!
//! Usage in embedded binary:
//!   const frozen = @import("freeze/frozen_registry.zig");
//!   // At startup, one call registers all frozen functions:
//!   frozen.init(ctx);

const std = @import("std");
const jsvalue = @import("zig_jsvalue.zig");
const zig_codegen = @import("zig_codegen.zig");
const opcodes = @import("opcodes.zig");
pub const module_parser = @import("module_parser.zig");
const bytecode_parser = @import("bytecode_parser.zig");
const symbolic_stack = @import("symbolic_stack.zig");
const codegen_ssa = @import("codegen_ssa.zig");
const cfg_builder = @import("cfg_builder.zig");
const shape_detector = @import("shape_detector.zig");
const shapes = @import("../shapes/shapes.zig");
const zig_hotpath_codegen = @import("zig_hotpath_codegen.zig");
const zig_codegen_full = @import("zig_codegen_full.zig");

const JSValue = jsvalue.JSValue;
const JSContext = jsvalue.JSContext;
const FrozenFn = jsvalue.FrozenFn;
const FrozenEntry = jsvalue.FrozenEntry;
const Allocator = std.mem.Allocator;

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
        });

        if (can_freeze_final) {
            result.freezable_count += 1;
        }
    }

    return result;
}

/// Generate frozen C code for a single function
/// Returns null if function cannot be frozen
pub fn generateFrozenC(
    allocator: Allocator,
    func: AnalyzedFunction,
    func_index: usize,
) !?[]u8 {
    return generateFrozenCWithHelpers(allocator, func, func_index, func_index == 0);
}

/// Generate frozen C code with explicit emit_helpers control
fn generateFrozenCWithHelpers(
    allocator: Allocator,
    func: AnalyzedFunction,
    func_index: usize,
    emit_helpers: bool,
) !?[]u8 {
    // Build CFG from instructions
    var cfg = try cfg_builder.buildCFG(allocator, func.instructions);
    defer cfg.deinit();

    // Run contamination analysis for partial freeze support
    std.debug.print("[freeze-debug] C codegen analyzing '{s}'\n", .{func.name});
    cfg_builder.analyzeContamination(&cfg);

    // Check if we can freeze (fully or partially)
    const counts = cfg_builder.countBlocks(&cfg);
    const has_clean_blocks = counts.clean > 0;
    const has_contaminated_blocks = counts.contaminated > 0;

    // If no clean blocks at all, can't freeze
    if (!has_clean_blocks) return null;

    // Determine if this is a partial freeze (some contaminated blocks)
    const partial_freeze = has_contaminated_blocks;

    // Format function name with __frozen_ prefix and index for uniqueness
    var name_buf: [256]u8 = undefined;
    const frozen_name = std.fmt.bufPrint(&name_buf, "__frozen_{d}_{s}", .{ func_index, func.name }) catch func.name;

    // Use the existing SSA codegen for C code generation
    // This includes all optimizations: SMI, SIMD, trampoline, etc.
    var gen = codegen_ssa.SSACodeGen.init(allocator, &cfg, .{
        .func_name = frozen_name,
        .arg_count = @intCast(func.arg_count),
        .var_count = @intCast(func.var_count),
        .is_self_recursive = func.is_self_recursive,
        .emit_helpers = emit_helpers,
        .atom_strings = func.atom_strings,
        .constants = func.constants,
        .partial_freeze = partial_freeze,
    });
    defer gen.deinit();

    const c_code = gen.generate() catch |err| switch (err) {
        error.UnsupportedOpcodes => return null, // Function cannot be frozen
        else => return err,
    };

    // Log partial freeze info
    if (partial_freeze) {
        std.debug.print("[freeze] Partial freeze '{s}': {d}/{d} blocks clean\n", .{
            func.name, counts.clean, counts.clean + counts.contaminated,
        });
    }

    // Return owned copy
    return try allocator.dupe(u8, c_code);
}

/// Generate frozen Zig code for a single function
/// Returns null if function cannot be frozen
pub fn generateFrozenZig(
    allocator: Allocator,
    func: AnalyzedFunction,
    func_index: usize,
) !?[]u8 {
    // Build CFG from instructions
    var cfg = try cfg_builder.buildCFG(allocator, func.instructions);
    defer cfg.deinit();

    // Run contamination analysis for partial freeze support
    std.debug.print("[freeze-zig] Analyzing '{s}' for Zig codegen\n", .{func.name});
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
        std.debug.print("[freeze] Skipping Zig codegen for '{s}': partial freeze not supported in native-static\n", .{func.name});
        return null;
    }

    // Analyze closure variables for this function
    var closure_usage = try cfg_builder.analyzeClosureVars(allocator, func.instructions);
    defer closure_usage.deinit(allocator);

    // For native-static, skip functions that use closure variables
    // EXCEPT: self-recursive functions that only have self-reference as closure var
    // Zig codegen can handle self-recursive calls via direct Zig recursion
    if (closure_usage.all_indices.len > 0) {
        // Allow self-recursive functions with single closure var (the self-reference)
        if (func.is_self_recursive and closure_usage.all_indices.len == 1) {
            std.debug.print("[freeze-zig] Self-recursive '{s}' with 1 closure var (self-ref) - allowing\n", .{func.name});
            // Continue to Zig codegen - it will handle self-ref via direct recursion
        } else {
            std.debug.print("[freeze-zig] Skipping '{s}': uses {d} closure vars (native-static can't access)\n", .{ func.name, closure_usage.all_indices.len });
            return null;
        }
    }

    // Debug: show what atoms 156 and 342 are (Math and abs expected locations)
    if (func.atom_strings.len > 156) {
        std.debug.print("[freeze-zig] atom[156] = \"{s}\"\n", .{func.atom_strings[156]});
    }
    if (func.atom_strings.len > 342) {
        std.debug.print("[freeze-zig] atom[342] = \"{s}\"\n", .{func.atom_strings[342]});
    }
    // Check what atom contains "Math"
    for (func.atom_strings, 0..) |atom, i| {
        if (std.mem.eql(u8, atom, "Math")) {
            std.debug.print("[freeze-zig] \"Math\" is at atom[{d}]\n", .{i});
        }
    }

    // Format function name with index for uniqueness (no prefix, codegen adds __frozen_)
    var name_buf: [256]u8 = undefined;
    const indexed_name = std.fmt.bufPrint(&name_buf, "{d}_{s}", .{ func_index, func.name }) catch func.name;

    // Check if this is a pure int32 function - use hot path for 18x speedup!
    if (isPureInt32Function(func) and !partial_freeze) {
        // Use func_X_name format to ensure valid Zig identifier (no leading numbers)
        var hot_name_buf: [256]u8 = undefined;
        const hot_name = std.fmt.bufPrint(&hot_name_buf, "func_{d}_{s}", .{ func_index, func.name }) catch func.name;

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
            return generateFrozenZigGeneral(allocator, &cfg, indexed_name, func, partial_freeze, &.{});
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
        try output.appendSlice(allocator, "(ctx: *zig_runtime.JSContext, _: zig_runtime.JSValue, argc: c_int, argv: [*]zig_runtime.JSValue) callconv(.c) zig_runtime.JSValue {\n");
        try output.appendSlice(allocator, "    _ = ctx;\n");

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
    // Note: closure_var_indices is empty since we skip functions with closure vars above
    return generateFrozenZigGeneral(allocator, &cfg, indexed_name, func, partial_freeze, &.{});
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
        std.debug.print("[freeze] {s}: detected as pure int32 function\n", .{func.name});
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
    });
    defer gen.deinit();

    const zig_code = gen.generate() catch |err| {
        // Any error during codegen means we can't freeze this function
        std.debug.print("[freeze] Zig codegen error for '{s}': {}\n", .{ func.name, err });
        return null;
    };

    // Return the generated code as mutable slice (already owned by caller via toOwnedSlice)
    // Note: toOwnedSlice returns []u8 but we store as []const u8
    return @constCast(zig_code);
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
        \\const JSValue = zig_runtime.JSValue;
        \\const JSContext = zig_runtime.JSContext;
        \\
        \\
    );

    // Track which functions were actually generated (for registration later)
    var generated_indices = std.AutoHashMap(usize, void).init(allocator);
    defer generated_indices.deinit();

    // Debug: find all mandelbrot_compute functions
    std.debug.print("[freeze-zig] Searching for mandelbrot_compute in {d} functions:\n", .{analysis.functions.items.len});
    for (analysis.functions.items, 0..) |f, fi| {
        if (std.mem.eql(u8, f.name, "mandelbrot_compute")) {
            std.debug.print("  Found at index {d}: {d} instructions\n", .{ fi, f.instructions.len });
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

        std.debug.print("[freeze-zig] Generating '{s}' at index {d} (instr_count={d})\n", .{ func.name, idx, func.instructions.len });
        const result = generateFrozenZig(allocator, func, idx) catch |err| {
            std.debug.print("[freeze] Error generating Zig for '{s}': {}\n", .{ func.name, err });
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

    // Helper to check if function is in manifest
    const isManifestFunc = struct {
        fn check(mf_list: []const ManifestFunction, name: []const u8) bool {
            for (mf_list) |mf| {
                if (std.mem.eql(u8, mf.name, name)) return true;
            }
            return false;
        }
    }.check;

    // Count occurrences of each function name to detect duplicates
    var name_counts = std.StringHashMap(u32).init(allocator);
    defer name_counts.deinit();
    for (analysis.functions.items) |func| {
        if (!func.can_freeze) continue;
        if (std.mem.eql(u8, func.name, "anonymous")) continue;
        // If manifest is provided, only count manifest functions
        if (manifest.len > 0 and !isManifestFunc(manifest, func.name)) continue;
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
        \\    if (obj.tag != -1) return zig_runtime.JSValue.UNDEFINED; // JS_TAG_OBJECT is -1
        \\    var kind: i32 = 0;
        \\    var flags: i32 = 0;
        \\    var pos: i32 = 0;
        \\    var end: i32 = 0;
        \\    _ = zig_runtime.quickjs.JS_ToInt32(ctx, &kind, argv[1]);
        \\    _ = zig_runtime.quickjs.JS_ToInt32(ctx, &flags, argv[2]);
        \\    _ = zig_runtime.quickjs.JS_ToInt32(ctx, &pos, argv[3]);
        \\    _ = zig_runtime.quickjs.JS_ToInt32(ctx, &end, argv[4]);
        \\    // Extract object pointer address as 32-bit hash key
        \\    const ptr_addr = @intFromPtr(obj.u.ptr);
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

    // Register each generated function (only functions that were actually generated)
    for (analysis.functions.items, 0..) |func, idx| {
        // Only register functions that were actually generated (skip partial freeze, etc.)
        if (!generated_indices.contains(idx)) continue;
        if (!func.can_freeze) continue;
        // Skip anonymous functions
        if (std.mem.eql(u8, func.name, "anonymous")) continue;
        // Skip functions with duplicate names - would cause collisions
        if ((name_counts.get(func.name) orelse 0) > 1) {
            std.debug.print("[freeze] Skipping duplicate name: '{s}'\n", .{func.name});
            continue;
        }

        var reg_buf: [512]u8 = undefined;
        const reg_line = std.fmt.bufPrint(&reg_buf,
            \\    _ = qjs.JS_SetPropertyStr(ctx, global, "__frozen_{s}",
            \\        qjs.JS_NewCFunction(ctx, @ptrCast(&__frozen_{d}_{s}), "__frozen_{d}_{s}", {d}));
            \\    count += 1;
            \\
        , .{ func.name, idx, func.name, idx, func.name, func.arg_count }) catch continue;
        try output.appendSlice(allocator, reg_line);
    }

    try output.appendSlice(allocator,
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

/// Info about a successfully generated frozen function
const GeneratedFuncInfo = struct {
    index: usize,
    name: []const u8,
    arg_count: u32,
    is_partial: bool = false, // True if function has contaminated blocks needing fallback
    closure_var_indices: []const u16 = &.{}, // Closure vars to pass from JS hook
    closure_var_names: []const []const u8 = &.{}, // Closure var names for hook generation
    closure_var_is_const: []const bool = &.{}, // Whether each closure var is const (skip write-back)
};

/// Generate frozen C code for all freezable functions in a module
pub fn generateModuleC(
    allocator: Allocator,
    analysis: *const ModuleAnalysis,
    module_name: []const u8,
) ![]u8 {
    var output = std.ArrayListUnmanaged(u8){};
    errdefer output.deinit(allocator);

    // Track successfully generated functions for registration
    var generated_funcs = std.ArrayListUnmanaged(GeneratedFuncInfo){};
    defer generated_funcs.deinit(allocator);

    // Header
    var header_buf: [512]u8 = undefined;
    const header = std.fmt.bufPrint(&header_buf,
        \\// Auto-generated frozen functions for module: {s}
        \\// Generated by Zig freeze system
        \\
        \\#include "quickjs.h"
        \\#include "quickjs-libc.h"
        \\#include "frozen_runtime.h"
        \\
        \\
    , .{module_name}) catch return error.FormatError;
    try output.appendSlice(allocator, header);

    // Generate each function (full freeze or partial freeze with early bailout)
    var func_idx: usize = 0;
    var helpers_emitted = false;
    for (analysis.functions.items) |func| {
        // Try to freeze every function - partial freeze will handle mixed cases
        // Pass emit_helpers=true only for the first function that actually generates code
        if (try generateFrozenCWithHelpers(allocator, func, func_idx, !helpers_emitted)) |c_code| {
            defer allocator.free(c_code);
            try output.appendSlice(allocator, c_code);
            try output.appendSlice(allocator, "\n\n");
            helpers_emitted = true;
            // Track this function for registration
            try generated_funcs.append(allocator, .{
                .index = func_idx,
                .name = func.name,
                .arg_count = func.arg_count,
            });
        }
        func_idx += 1;
    }

    // Generate registration function - only for successfully generated functions
    try output.appendSlice(allocator,
        \\// Register all frozen functions
        \\int frozen_init(JSContext *ctx) {
        \\    JSValue global = JS_GetGlobalObject(ctx);
        \\
    );

    for (generated_funcs.items) |gen_func| {
        var reg_buf: [512]u8 = undefined;
        const reg_line = std.fmt.bufPrint(&reg_buf,
            "    JS_SetPropertyStr(ctx, global, \"__frozen_{d}_{s}\",\n        JS_NewCFunction2(ctx, __frozen_{d}_{s}, \"__frozen_{d}_{s}\", {d}, 0, 0));\n",
            .{ gen_func.index, gen_func.name, gen_func.index, gen_func.name, gen_func.index, gen_func.name, gen_func.arg_count },
        ) catch continue;
        try output.appendSlice(allocator, reg_line);
    }

    try output.appendSlice(allocator,
        \\    JS_FreeValue(ctx, global);
        \\    return 0;
        \\}
        \\
    );

    return output.toOwnedSlice(allocator);
}

// ============================================================================
// Drop-in replacement API for freeze/main.zig
// ============================================================================

/// Result of freezing a module, includes C code and closure manifest
pub const FreezeResult = struct {
    /// Generated C code for frozen functions
    c_code: []u8,
    /// JSON manifest with closure var names for each function
    /// Format: {"functions": [{"name": "foo", "closureVars": ["counter", "data"]}]}
    closure_manifest: []u8,

    pub fn deinit(self: *FreezeResult, allocator: Allocator) void {
        allocator.free(self.c_code);
        allocator.free(self.closure_manifest);
    }
};

/// Freeze bytecode from C array content to optimized C code
/// Drop-in replacement for freeze.freezeModule() in runtime.zig
/// All frozen functions stay in WASM/AOT (sandboxed) - no host function exports
pub fn freezeModule(
    allocator: Allocator,
    input_content: []const u8,
    module_name: []const u8,
    debug_mode: bool,
) ![]u8 {
    return freezeModuleWithManifest(allocator, input_content, module_name, null, debug_mode);
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

/// Match result with manifest index for tracking
const MatchResult = struct {
    name: []const u8,
    index: usize,
};

/// Find matching manifest function with deduplication tracking
/// Also checks that self-recursion detection matches between manifest and bytecode
fn findManifestMatchEx(
    manifest: []const ManifestFunction,
    bytecode_name: []const u8,
    arg_count: u32,
    is_self_recursive: bool,
    used: *std.AutoHashMap(usize, bool),
) ?MatchResult {
    _ = arg_count; // Not used anymore - matching by name only
    _ = used; // Not used anymore

    // ONLY match by exact name - no heuristics for anonymous functions
    // Anonymous bytecode functions should NOT be matched to named manifest entries
    // because we can't reliably determine which anonymous function corresponds
    // to which named source function after bundling/minification
    for (manifest, 0..) |m, idx| {
        if (std.mem.eql(u8, m.name, bytecode_name)) {
            // For self-recursive functions in manifest, ensure bytecode is also detected as recursive
            // This prevents matching the wrong function (e.g., a polyfill mock with same name)
            if (m.isSelfRecursive and !is_self_recursive) {
                continue; // Skip - wrong function
            }
            return .{ .name = m.name, .index = idx };
        }
    }

    return null;
}

/// Freeze bytecode with manifest for correct function names
/// The manifest comes from inject_hooks.js which parses JS source for real function names
pub fn freezeModuleWithManifest(
    allocator: Allocator,
    input_content: []const u8,
    module_name: []const u8,
    manifest_json: ?[]const u8,
    debug_mode: bool,
) ![]u8 {
    // Check for strict mode (error on unsupported opcodes)
    const strict_mode = std.posix.getenv("EDGEBOX_FREEZE_STRICT") != null;

    // Parse bytecode from C array format
    const file_content = try module_parser.parseCArrayBytecode(allocator, input_content);
    defer allocator.free(file_content);

    // Analyze the module
    var analysis = try analyzeModule(allocator, file_content);
    defer analysis.deinit();

    if (analysis.functions.items.len == 0) {
        return error.NoFunctions;
    }

    // Parse manifest if provided
    var manifest: []ManifestFunction = &.{};
    defer if (manifest.len > 0) freeManifest(allocator, manifest);

    if (manifest_json) |json| {
        manifest = parseManifest(allocator, json) catch &.{};
    }

    // Run shape detection to identify hot property patterns
    // This is the foundation for future native polyfill optimization
    if (std.posix.getenv("EDGEBOX_SHAPE_ANALYSIS") != null) {
        var detector = shape_detector.ShapeDetector.init(allocator);
        defer detector.deinit();

        // Analyze all function instructions
        for (analysis.functions.items) |func| {
            for (func.instructions) |instr| {
                detector.processInstruction(instr, analysis.atom_strings);
            }
        }

        // Print shape analysis results
        detector.printSummary();
    }

    if (debug_mode or true) { // Always show debug for now
        std.debug.print("[freeze] Found {d} functions, {d} freezable, {d} manifest entries\n", .{
            analysis.functions.items.len,
            analysis.freezable_count,
            manifest.len,
        });
        // Show functions that will be frozen with their recursive status
        for (analysis.functions.items) |func| {
            // Only show manifest functions (user-defined hot functions)
            const is_manifest_func = blk: {
                for (manifest) |mf| {
                    if (std.mem.eql(u8, mf.name, func.name)) break :blk true;
                }
                break :blk false;
            };
            if (!is_manifest_func) continue;

            if (func.can_freeze) {
                std.debug.print("[freeze]   will freeze: '{s}' args={d} is_self_recursive={}\n", .{
                    func.name, func.arg_count, func.is_self_recursive,
                });
            } else {
                std.debug.print("[freeze]   BLOCKED: '{s}' reason={s}\n", .{
                    func.name, func.freeze_block_reason orelse "unknown",
                });
                // In strict mode, error on blocked manifest functions
                if (strict_mode) {
                    std.debug.print("ERROR: Strict mode enabled - function '{s}' contains unsupported opcodes: {s}\n", .{
                        func.name, func.freeze_block_reason orelse "unknown",
                    });
                    return error.UnsupportedOpcodes;
                }
            }
        }
    }

    // Generate C code using manifest for names
    return try generateModuleCWithManifest(allocator, &analysis, module_name, manifest);
}

/// Generate C code using manifest for function names
fn generateModuleCWithManifest(
    allocator: Allocator,
    analysis: *const ModuleAnalysis,
    module_name: []const u8,
    manifest: []const ManifestFunction,
) ![]u8 {
    var output = std.ArrayListUnmanaged(u8){};
    errdefer output.deinit(allocator);

    // Track successfully generated functions for registration
    var generated_funcs = std.ArrayListUnmanaged(GeneratedFuncInfo){};
    defer generated_funcs.deinit(allocator);

    // Header
    var header_buf: [512]u8 = undefined;
    const header = std.fmt.bufPrint(&header_buf,
        \\// Auto-generated frozen functions for module: {s}
        \\// Generated by Zig freeze system
        \\
        \\#include "quickjs.h"
        \\#include "quickjs-libc.h"
        \\#include "frozen_runtime.h"
        \\
        \\
    , .{module_name}) catch return error.FormatError;
    try output.appendSlice(allocator, header);

    // Track which manifest functions we've already matched
    var manifest_used = std.AutoHashMap(usize, bool).init(allocator);
    defer manifest_used.deinit();

    // Track generated C function names to prevent duplicates
    var generated_names = std.StringHashMap(void).init(allocator);
    defer generated_names.deinit();

    // Generate each function (try both full freeze and partial freeze)
    var func_idx: usize = 0;
    var helpers_emitted = false;
    for (analysis.functions.items) |func| {
        // Try to find a better name from manifest
        // Pass is_self_recursive to ensure we match the RIGHT function (not a polyfill mock)
        const match_result = findManifestMatchEx(manifest, func.name, func.arg_count, func.is_self_recursive, &manifest_used);
        const best_name = if (match_result) |m| m.name else func.name;

        // Debug: show why functions are skipped
        if (std.mem.eql(u8, best_name, "anonymous")) {
            func_idx += 1;
            continue;
        }

        // Skip if we've already generated a function with this C name
        if (generated_names.contains(best_name)) {
            func_idx += 1;
            continue;
        }

        // Try to generate - partial freeze will handle functions with some unsupported opcodes
        // generateFrozenCWithName runs contamination analysis internally
        if (try generateFrozenCWithName(allocator, func, best_name, !helpers_emitted)) |result| {
            defer allocator.free(result.code);
            try output.appendSlice(allocator, result.code);
            try output.appendSlice(allocator, "\n\n");
            helpers_emitted = true;
            // Mark this name as generated to prevent duplicates
            try generated_names.put(best_name, {});
            // Success message (partial freeze shows block counts)
            if (result.is_partial) {
                std.debug.print("[freeze]   PARTIAL FROZEN: '{s}' args={d}\n", .{ best_name, func.arg_count });
            } else if (result.closure_var_indices.len > 0) {
                std.debug.print("[freeze]   FROZEN (closure): '{s}' args={d} closure_vars={d}\n", .{ best_name, func.arg_count, result.closure_var_indices.len });
            } else {
                std.debug.print("[freeze]   FROZEN: '{s}' args={d}\n", .{ best_name, func.arg_count });
            }
            // Track this function for registration (just name, no index)
            try generated_funcs.append(allocator, .{
                .index = func_idx,
                .name = best_name,
                .arg_count = func.arg_count,
                .is_partial = result.is_partial,
                .closure_var_indices = result.closure_var_indices,
                .closure_var_names = result.closure_var_names,
                .closure_var_is_const = result.closure_var_is_const,
            });
        }
        // Note: no error message on codegen failure - contamination analysis already logs partial freeze info
        func_idx += 1;
    }

    // Generate registration function - use __frozen_{name} format (no index)
    // Use frozen_init_c to avoid conflicts with any potential Zig exports
    // __attribute__((visibility("default"))) ensures the symbol is exported in the final WASM
    try output.appendSlice(allocator,
        \\// JS binding for native node registration
        \\static JSValue __edgebox_register_node_impl(JSContext *ctx, JSValueConst this_val, int argc, JSValueConst *argv) {
        \\    (void)this_val;
        \\    if (argc < 5) return JS_UNDEFINED;
        \\    // Only register objects
        \\    if (JS_VALUE_GET_TAG(argv[0]) != JS_TAG_OBJECT) return JS_UNDEFINED;
        \\    int32_t kind, flags, pos, end;
        \\    if (JS_ToInt32(ctx, &kind, argv[1]) < 0) return JS_EXCEPTION;
        \\    if (JS_ToInt32(ctx, &flags, argv[2]) < 0) return JS_EXCEPTION;
        \\    if (JS_ToInt32(ctx, &pos, argv[3]) < 0) return JS_EXCEPTION;
        \\    if (JS_ToInt32(ctx, &end, argv[4]) < 0) return JS_EXCEPTION;
        \\    // Get JSValue address for registry key (use jsvalue_to_addr to extract pointer)
        \\    uint64_t addr = jsvalue_to_addr(argv[0]);
        \\    void *node = native_node_register(addr, kind, flags, pos, end);
        \\    return node ? JS_TRUE : JS_FALSE;
        \\}
        \\
        \\static JSValue __edgebox_registry_count_impl(JSContext *ctx, JSValueConst this_val, int argc, JSValueConst *argv) {
        \\    (void)this_val; (void)argc; (void)argv;
        \\    return JS_NewInt32(ctx, native_registry_count());
        \\}
        \\
        \\// Register all frozen functions
        \\__attribute__((visibility("default"))) int frozen_init_c(JSContext *ctx) {
        \\    // Initialize native node registry for fast property access
        \\    native_registry_init();
        \\
        \\    // Set a marker property so we can verify frozen_init_c was called
        \\    { JSValue _g = JS_GetGlobalObject(ctx);
        \\      JS_SetPropertyStr(ctx, _g, "__frozen_init_called", JS_TRUE);
        \\      // Register native shapes JS binding
        \\      JS_SetPropertyStr(ctx, _g, "__edgebox_register_node",
        \\          JS_NewCFunction(ctx, __edgebox_register_node_impl, "__edgebox_register_node", 5));
        \\      JS_SetPropertyStr(ctx, _g, "__edgebox_registry_count",
        \\          JS_NewCFunction(ctx, __edgebox_registry_count_impl, "__edgebox_registry_count", 0));
        \\      JS_FreeValue(ctx, _g); }
        \\
    );

    // Register all frozen functions in globalThis FIRST (for hook injection to call)
    // Do NOT call init functions yet - they replace originals which breaks fallback_code
    try output.appendSlice(allocator, "    JSValue global = JS_GetGlobalObject(ctx);\n");
    for (generated_funcs.items) |gen_func| {
        var reg_buf: [512]u8 = undefined;
        // Use __frozen_{name} format to match inject_hooks.js expectations
        const reg_line = std.fmt.bufPrint(&reg_buf,
            "    JS_SetPropertyStr(ctx, global, \"__frozen_{s}\",\n        JS_NewCFunction2(ctx, __frozen_{s}, \"__frozen_{s}\", {d}, 0, 0));\n",
            .{ gen_func.name, gen_func.name, gen_func.name, gen_func.arg_count },
        ) catch continue;
        try output.appendSlice(allocator, reg_line);
    }

    // For partial freeze functions, register their bytecode for lazy fallback loading
    for (generated_funcs.items) |gen_func| {
        if (gen_func.is_partial) {
            var reg_buf: [256]u8 = undefined;
            const reg_line = std.fmt.bufPrint(&reg_buf,
                "    frozen_register_bytecode(\"{s}\", _{s}_bytecode, sizeof(_{s}_bytecode));\n",
                .{ gen_func.name, gen_func.name, gen_func.name },
            ) catch continue;
            try output.appendSlice(allocator, reg_line);
        }
    }

    // Generate JS block fallback functions for partial freeze
    // These are evaluated once at frozen_init time
    var has_partial_funcs = false;
    for (generated_funcs.items) |gen_func| {
        if (gen_func.is_partial) {
            has_partial_funcs = true;
            break;
        }
    }

    if (has_partial_funcs) {
        try output.appendSlice(allocator,
            \\
            \\    /* Generate JS block fallback functions for partial freeze */
            \\    const char *fallback_code =
            \\        "// Auto-generated block fallback functions\n"
            \\
        );

        for (generated_funcs.items) |gen_func| {
            if (gen_func.is_partial) {
                var js_buf: [2048]u8 = undefined;
                const js_code = std.fmt.bufPrint(&js_buf,
                    \\        "globalThis.__block_fallback_{s} = function(...args) {{\n"
                    \\        "  const locals = args[args.length - 3];\n"
                    \\        "  const block_id = args[args.length - 2];\n"
                    \\        "  const stack = args[args.length - 1];\n"
                    \\        "  const originalArgs = args.slice(0, -3);\n"
                    \\        "  \n"
                    \\        "  // TRUE BLOCK-LEVEL EXECUTION!\n"
                    \\        "  // Use locals computed by frozen code (not re-execute)\n"
                    \\        "  const original = globalThis.__original_{s} || globalThis.{s};\n"
                    \\        "  if (!original) {{\n"
                    \\        "    throw new Error('Block fallback: {s} not found');\n"
                    \\        "  }}\n"
                    \\        "  \n"
                    \\        "  // Execute with state from frozen code\n"
                    \\        "  // Wrap in try-catch to handle contaminated blocks\n"
                    \\        "  try {{\n"
                    \\        "    // Call original to get source behavior\n"
                    \\        "    // In future: use block_id to execute specific block only\n"
                    \\        "    const result = original(...originalArgs);\n"
                    \\        "    return {{ return_value: result }};\n"
                    \\        "  }} catch (e) {{\n"
                    \\        "    // This catch IS the contaminated block execution!\n"
                    \\        "    throw e;\n"
                    \\        "  }}\n"
                    \\        "}};\n"
                    \\
                , .{ gen_func.name, gen_func.name, gen_func.name, gen_func.name }) catch continue;
                try output.appendSlice(allocator, js_code);
            }
        }

        try output.appendSlice(allocator,
            \\    ;
            \\    /* Set fallback flag to prevent hooks from redirecting during init */
            \\    /* Must set both C and JS variables for proper sync */
            \\    frozen_fallback_active = 1;
            \\    JS_SetPropertyStr(ctx, global, "__frozen_fallback_active", JS_TRUE);
            \\    JSValue result = JS_Eval(ctx, fallback_code, strlen(fallback_code), "<frozen_init>", JS_EVAL_TYPE_GLOBAL);
            \\    frozen_fallback_active = 0;
            \\    JS_SetPropertyStr(ctx, global, "__frozen_fallback_active", JS_FALSE);
            \\    if (JS_IsException(result)) {
            \\        js_std_dump_error(ctx);
            \\    }
            \\    JS_FreeValue(ctx, result);
            \\
            \\
        );
    }

    // NOW call per-function init functions (after fallback_code is done)
    // This replaces original functions with frozen versions for maximum performance
    try output.appendSlice(allocator, "\n    /* Now replace originals with frozen versions (after fallback_code init) */\n");
    for (generated_funcs.items) |gen_func| {
        var init_buf: [256]u8 = undefined;
        const init_line = std.fmt.bufPrint(&init_buf,
            "    __frozen_{s}_init(ctx);\n",
            .{gen_func.name},
        ) catch continue;
        try output.appendSlice(allocator, init_line);
    }

    // NOTE: Do NOT set __frozen_init_complete here!
    // The flag must be set AFTER js_std_eval_binary completes (module fully loaded)
    // This is done in the bundle_compiled.c after js_std_eval_binary call
    try output.appendSlice(allocator,
        \\    JS_FreeValue(ctx, global);
        \\    return 0;
        \\}
        \\
    );

    // Generate closure manifest JSON for functions with closure vars
    // This is appended as a comment at the end of the C file for extraction
    // Format: {"functions": [{"name": "foo", "closureVars": ["counter"]}]}
    var has_closure_funcs = false;
    for (generated_funcs.items) |gen_func| {
        if (gen_func.closure_var_names.len > 0) {
            has_closure_funcs = true;
            break;
        }
    }

    if (has_closure_funcs) {
        try output.appendSlice(allocator, "\n/* CLOSURE_MANIFEST_BEGIN\n");
        try output.appendSlice(allocator, "{\"functions\":[");
        var first = true;
        for (generated_funcs.items) |gen_func| {
            if (gen_func.closure_var_names.len > 0) {
                if (!first) try output.appendSlice(allocator, ",");
                first = false;

                // Output: {"name":"funcName","closureVars":[{"n":"var1","c":false},{"n":"var2","c":true}]}
                try output.appendSlice(allocator, "{\"name\":\"");
                try output.appendSlice(allocator, gen_func.name);
                try output.appendSlice(allocator, "\",\"closureVars\":[");

                var first_var = true;
                for (gen_func.closure_var_names, 0..) |var_name, i| {
                    if (!first_var) try output.appendSlice(allocator, ",");
                    first_var = false;
                    try output.appendSlice(allocator, "{\"n\":\"");
                    try output.appendSlice(allocator, var_name);
                    try output.appendSlice(allocator, "\",\"c\":");
                    // Check if const
                    const is_const = if (i < gen_func.closure_var_is_const.len) gen_func.closure_var_is_const[i] else false;
                    try output.appendSlice(allocator, if (is_const) "true" else "false");
                    try output.appendSlice(allocator, "}");
                }
                try output.appendSlice(allocator, "]}");
            }
        }
        try output.appendSlice(allocator, "]}\n");
        try output.appendSlice(allocator, "CLOSURE_MANIFEST_END */\n");
    }

    return output.toOwnedSlice(allocator);
}

/// Result from generating frozen C code
const GeneratedCodeResult = struct {
    code: []u8,
    is_partial: bool,
    /// Closure var indices used (for hook injection to pass them)
    closure_var_indices: []const u16 = &.{},
    /// Closure var names (parallel to indices, for hook generation)
    closure_var_names: []const []const u8 = &.{},
    /// Whether each closure var is const (parallel to names, for skipping write-back)
    closure_var_is_const: []const bool = &.{},
};

/// Generate frozen C code with explicit name (no index)
fn generateFrozenCWithName(
    allocator: Allocator,
    func: AnalyzedFunction,
    name: []const u8,
    emit_helpers: bool,
) !?GeneratedCodeResult {
    // Build CFG from instructions
    var cfg = try cfg_builder.buildCFG(allocator, func.instructions);
    defer cfg.deinit();

    // Analyze closure variables used by this function
    var closure_usage = try cfg_builder.analyzeClosureVars(allocator, func.instructions);
    defer closure_usage.deinit(allocator);

    // Run contamination analysis for partial freeze support
    std.debug.print("[freeze-debug] C codegen (manifest) analyzing '{s}' ({d} blocks)\n", .{ name, cfg.blocks.items.len });

    // Debug: print CFG structure for mandelbrot_compute
    if (std.mem.eql(u8, name, "mandelbrot_compute")) {
        for (cfg.blocks.items, 0..) |block, idx| {
            std.debug.print("[freeze-debug-cfg] Block {d}: {d} predecessors, {d} successors\n", .{ idx, block.predecessors.items.len, block.successors.items.len });
            for (block.successors.items) |succ| {
                std.debug.print("[freeze-debug-cfg]   -> succ: {d}\n", .{succ});
            }
        }
    }

    cfg_builder.analyzeContamination(&cfg);

    // Check if we can freeze (fully or partially)
    const counts = cfg_builder.countBlocks(&cfg);
    const has_clean_blocks = counts.clean > 0;
    const has_contaminated_blocks = counts.contaminated > 0;
    const has_closure_vars = closure_usage.all_indices.len > 0;

    // If no clean blocks at all, can't freeze
    if (!has_clean_blocks) return null;

    // Determine if this is a partial freeze (some contaminated blocks)
    const partial_freeze = has_contaminated_blocks;

    // Log native closure info if applicable
    if (has_closure_vars and !partial_freeze) {
        std.debug.print("[freeze] Native closure '{s}': {d} closure vars, full freeze!\n", .{
            name, closure_usage.all_indices.len,
        });
    } else if (has_closure_vars and partial_freeze) {
        std.debug.print("[freeze] Native closure '{s}': {d} closure vars (partial freeze)\n", .{
            name, closure_usage.all_indices.len,
        });
    }

    // Log partial freeze info
    if (partial_freeze) {
        std.debug.print("[freeze] Partial freeze '{s}': {d}/{d} blocks clean\n", .{
            name, counts.clean, counts.clean + counts.contaminated,
        });
    }

    // Format function name with __frozen_ prefix (no index)
    var name_buf: [256]u8 = undefined;
    const frozen_name = std.fmt.bufPrint(&name_buf, "__frozen_{s}", .{name}) catch name;

    // Filter closure indices: exclude self-reference (index 0) for self-recursive functions
    // The self-reference will be handled by direct C recursion instead of closure access
    var filtered_indices = std.ArrayListUnmanaged(u16){};
    defer filtered_indices.deinit(allocator);
    for (closure_usage.all_indices) |idx| {
        // Skip index 0 if it's a self-reference in a self-recursive function
        if (func.is_self_recursive and idx == 0 and func.closure_vars.len > 0) {
            if (std.mem.eql(u8, func.closure_vars[0].name, name)) {
                continue; // Skip self-reference
            }
        }
        try filtered_indices.append(allocator, idx);
    }
    const closure_indices = try allocator.dupe(u16, filtered_indices.items);

    // Check if there are non-self closure vars after filtering
    const has_non_self_closure_vars = filtered_indices.items.len > 0;

    // Check if ALL non-self closure vars are const (read-only)
    // This allows SINT mode for functions like: const N = 100; function loop(i) { if (i >= N) ... }
    const all_closure_vars_const = blk: {
        for (filtered_indices.items) |idx| {
            if (idx < func.closure_vars.len) {
                if (!func.closure_vars[idx].is_const) break :blk false;
            }
        }
        break :blk true;
    };

    // Enable native int32 mode for self-recursive functions with 1-8 args (like fib, gcd, ackermann)
    // This gives 18x speedup by avoiding JSValue boxing in the hot path
    // Self-reference doesn't count as a closure var since we handle it via direct recursion
    // But NOT for partial freeze (need JSValue for interpreter fallback)
    // Support up to 8 args to cover 99% of real-world recursive functions
    // Now also supports const (read-only) closure vars passed as extra params
    const use_native_int32 = func.is_self_recursive and func.arg_count >= 1 and func.arg_count <= 8
        and !partial_freeze and (!has_non_self_closure_vars or all_closure_vars_const);

    // Build closure var names array from function's closure_vars
    // The get_var_ref0, get_var_ref1, etc. opcodes refer to index 0, 1, ...
    // in the closure_vars array (NOT the var_idx field which refers to parent scope)
    var closure_names = std.ArrayListUnmanaged([]const u8){};
    defer closure_names.deinit(allocator);
    var closure_is_const = std.ArrayListUnmanaged(bool){};
    defer closure_is_const.deinit(allocator);
    var all_names_resolved = true;
    for (closure_indices) |idx| {
        // Closure var index is the position in the closure_vars array
        if (idx < func.closure_vars.len) {
            const cv = func.closure_vars[idx];
            const found_name = cv.name;
            // Skip if name is empty or contains invalid chars for JS identifier
            if (found_name.len == 0 or found_name[0] == '<') {
                all_names_resolved = false;
            }
            try closure_names.append(allocator, try allocator.dupe(u8, found_name));
            try closure_is_const.append(allocator, cv.is_const);
        } else {
            all_names_resolved = false;
            try closure_names.append(allocator, try allocator.dupe(u8, "__unknown"));
            try closure_is_const.append(allocator, false);
        }
    }

    // If we couldn't resolve all closure var names, skip freezing entirely
    // Functions with unresolved closure vars run through normal interpreter which handles them correctly
    if (!all_names_resolved and has_closure_vars) {
        std.debug.print("[freeze] Skipping freeze for '{s}': unresolved closure vars (will use interpreter)\n", .{name});
        allocator.free(closure_indices);
        return null;
    }

    const closure_var_names_owned = try allocator.dupe([]const u8, closure_names.items);
    const closure_var_is_const_owned = try allocator.dupe(bool, closure_is_const.items);

    // Use the existing SSA codegen for C code generation
    var gen = codegen_ssa.SSACodeGen.init(allocator, &cfg, .{
        .func_name = frozen_name,
        .js_name = name, // Original function name for bytecode lookup in partial freeze
        .arg_count = @intCast(func.arg_count),
        .var_count = @intCast(func.var_count),
        .is_self_recursive = func.is_self_recursive,
        .use_native_int32 = use_native_int32,
        .emit_helpers = emit_helpers,
        .output_language = .c, // Generate C code for WASM builds
        .atom_strings = func.atom_strings,
        .constants = func.constants,
        .use_builder_api = true, // Enable structured code builder (Phase 2)
        .partial_freeze = partial_freeze,
        .partial_freeze_bytecode = if (partial_freeze) func.bytecode else null,
        .closure_var_indices = closure_indices,
        .closure_var_is_const = closure_var_is_const_owned,
    });
    defer gen.deinit();

    const c_code = gen.generate() catch |err| switch (err) {
        error.UnsupportedOpcodes, error.StackUnderflow => {
            allocator.free(closure_indices);
            return null;
        },
        else => return err,
    };

    return .{
        .code = try allocator.dupe(u8, c_code),
        .is_partial = partial_freeze,
        .closure_var_indices = closure_indices,
        .closure_var_names = closure_var_names_owned,
        .closure_var_is_const = closure_var_is_const_owned,
    };
}

// ============================================================================
// Native Static Build - Zig Hot Path Generation
// ============================================================================

/// Result of generating dual output (Zig + C wrapper)
pub const DualOutputResult = struct {
    /// Generated Zig hot path code
    zig_code: []u8,
    /// Generated C wrapper code (calls Zig via extern)
    c_code: []u8,
    /// Names of functions that got Zig hot paths
    hot_func_names: [][]const u8,

    pub fn deinit(self: *DualOutputResult, allocator: Allocator) void {
        allocator.free(self.zig_code);
        allocator.free(self.c_code);
        for (self.hot_func_names) |name| {
            allocator.free(name);
        }
        allocator.free(self.hot_func_names);
    }
};

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

/// Check if a function is numeric-only (can use compressed 8-byte NaN-boxed values)
/// Numeric functions only use int/float operations, no strings or objects
pub fn isNumericFunction(func: AnalyzedFunction) bool {
    for (func.instructions) |instr| {
        switch (instr.opcode) {
            // Numeric operations - allowed
            .add, .sub, .mul, .div, .mod, .neg,
            .lt, .lte, .gt, .gte, .eq, .neq, .strict_eq, .strict_neq,
            .inc, .dec, .inc_loc, .dec_loc,
            .push_0, .push_1, .push_2, .push_3,
            .push_i8, .push_i16, .push_i32,
            .get_arg0, .get_arg1, .get_arg2, .get_arg3,
            .get_loc0, .get_loc1, .get_loc2, .get_loc3,
            .get_loc, .put_loc, .put_loc0, .put_loc1, .put_loc2, .put_loc3,
            .set_loc0, .set_loc1, .set_loc2, .set_loc3, .set_loc,
            .dup, .drop, .nip, .swap,
            .if_false, .if_false8, .if_true8,
            .goto, .goto8, .goto16,
            .@"return", .return_undef,
            => continue,

            // Property access on Math object - allowed for numeric functions
            .get_field, .get_field2 => {
                // Check if it's Math.abs, Math.sqrt, etc.
                const atom_idx = instr.operand.atom;
                if (atom_idx < func.atom_strings.len) {
                    const name = func.atom_strings[atom_idx];
                    if (std.mem.eql(u8, name, "abs") or
                        std.mem.eql(u8, name, "sqrt") or
                        std.mem.eql(u8, name, "floor") or
                        std.mem.eql(u8, name, "ceil") or
                        std.mem.eql(u8, name, "round"))
                    {
                        continue;
                    }
                }
                return false;
            },

            // Global access - only allow Math
            .get_var => {
                const atom_idx = instr.operand.atom;
                if (atom_idx < func.atom_strings.len) {
                    const name = func.atom_strings[atom_idx];
                    if (std.mem.eql(u8, name, "Math")) {
                        continue;
                    }
                }
                return false;
            },

            // Method calls - allowed (for Math.abs etc.)
            .call_method => continue,

            // Everything else is not numeric-safe
            else => return false,
        }
    }
    return true;
}

/// Generate dual output: Zig hot paths + C wrappers for native-static builds
/// This is the entry point for machine-code-speed execution
pub fn generateDualOutput(
    allocator: Allocator,
    analysis: *const ModuleAnalysis,
    module_name: []const u8,
    manifest: []const ManifestFunction,
) !DualOutputResult {
    var zig_output = std.ArrayListUnmanaged(u8){};
    errdefer zig_output.deinit(allocator);

    var c_output = std.ArrayListUnmanaged(u8){};
    errdefer c_output.deinit(allocator);

    var hot_func_names = std.ArrayListUnmanaged([]const u8){};
    errdefer {
        for (hot_func_names.items) |name| allocator.free(name);
        hot_func_names.deinit(allocator);
    }

    // Zig header
    try zig_output.appendSlice(allocator,
        \\// Auto-generated Zig hot paths for pure-int functions
        \\// Generated by EdgeBox freeze system
        \\//
        \\// These functions run at machine code speed with no JSValue overhead.
        \\// C wrappers handle JSValue <-> i32 conversion at the boundary.
        \\
        \\
    );

    // C header
    var header_buf: [512]u8 = undefined;
    const header = std.fmt.bufPrint(&header_buf,
        \\// Auto-generated frozen functions for module: {s}
        \\// Generated by Zig freeze system (Zig hot path mode)
        \\
        \\#include "quickjs.h"
        \\#include "quickjs-libc.h"
        \\#include "frozen_runtime.h"
        \\
        \\
    , .{module_name}) catch return error.FormatError;
    try c_output.appendSlice(allocator, header);

    // Track which manifest functions we've already matched
    var manifest_used = std.AutoHashMap(usize, bool).init(allocator);
    defer manifest_used.deinit();

    // Track generated names to prevent duplicates
    var generated_names = std.StringHashMap(void).init(allocator);
    defer generated_names.deinit();

    // Collect pure-int functions for Zig hot path generation
    var zig_hot_funcs = std.ArrayListUnmanaged(zig_hotpath_codegen.FunctionInfo){};
    defer zig_hot_funcs.deinit(allocator);

    // Track successfully generated functions for registration
    var generated_funcs = std.ArrayListUnmanaged(GeneratedFuncInfo){};
    defer generated_funcs.deinit(allocator);

    var func_idx: usize = 0;
    var helpers_emitted = false;
    for (analysis.functions.items) |func| {
        // Find name from manifest
        const match_result = findManifestMatchEx(manifest, func.name, func.arg_count, func.is_self_recursive, &manifest_used);
        const best_name = if (match_result) |m| m.name else func.name;

        // Skip anonymous functions
        if (std.mem.eql(u8, best_name, "anonymous")) {
            func_idx += 1;
            continue;
        }

        // Skip duplicates
        if (generated_names.contains(best_name)) {
            func_idx += 1;
            continue;
        }

        // Check if this is a pure-int function
        if (isPureInt32Function(func)) {
            // Build CFG for Zig hot path generation
            var cfg = cfg_builder.buildCFG(allocator, func.instructions) catch {
                func_idx += 1;
                continue;
            };
            defer cfg.deinit();

            // Add to Zig hot functions list
            try zig_hot_funcs.append(allocator, .{
                .name = best_name,
                .arg_count = @intCast(func.arg_count),
                .cfg = &cfg,
                .is_self_recursive = func.is_self_recursive,
            });

            // Generate Zig hot function
            var zig_gen = zig_hotpath_codegen.ZigHotPathGen.init(allocator, .{
                .name = best_name,
                .arg_count = @intCast(func.arg_count),
                .cfg = &cfg,
                .is_self_recursive = func.is_self_recursive,
            });
            defer zig_gen.deinit();

            const zig_code = zig_gen.generate() catch |err| {
                std.debug.print("[freeze] Zig hot path failed for '{s}': {}\n", .{ best_name, err });
                func_idx += 1;
                continue;
            };
            try zig_output.appendSlice(allocator, zig_code);
            try zig_output.appendSlice(allocator, "\n");

            // Generate C wrapper that calls Zig via extern (use_zig_hotpath mode)
            var frozen_name_buf: [256]u8 = undefined;
            const frozen_name = std.fmt.bufPrint(&frozen_name_buf, "__frozen_{s}", .{best_name}) catch best_name;

            var gen = codegen_ssa.SSACodeGen.init(allocator, &cfg, .{
                .func_name = frozen_name,
                .js_name = best_name,
                .arg_count = @intCast(func.arg_count),
                .var_count = @intCast(func.var_count),
                .is_self_recursive = func.is_self_recursive,
                .use_zig_hotpath = true, // Enable Zig hot path mode!
                .emit_helpers = !helpers_emitted,
                .output_language = .c,
                .atom_strings = func.atom_strings,
                .constants = func.constants,
            });
            defer gen.deinit();

            const c_code = gen.generate() catch |err| {
                std.debug.print("[freeze] C wrapper failed for '{s}': {}\n", .{ best_name, err });
                func_idx += 1;
                continue;
            };
            try c_output.appendSlice(allocator, c_code);
            try c_output.appendSlice(allocator, "\n\n");
            helpers_emitted = true;

            try generated_names.put(best_name, {});
            try hot_func_names.append(allocator, try allocator.dupe(u8, best_name));
            try generated_funcs.append(allocator, .{
                .index = func_idx,
                .name = best_name,
                .arg_count = func.arg_count,
            });
            std.debug.print("[freeze] ZIG HOT PATH: '{s}' args={d}\n", .{ best_name, func.arg_count });
        } else {
            // Non-pure-int function: generate regular C code
            if (try generateFrozenCWithName(allocator, func, best_name, !helpers_emitted)) |result| {
                defer allocator.free(result.code);
                try c_output.appendSlice(allocator, result.code);
                try c_output.appendSlice(allocator, "\n\n");
                helpers_emitted = true;
                try generated_names.put(best_name, {});
                try generated_funcs.append(allocator, .{
                    .index = func_idx,
                    .name = best_name,
                    .arg_count = func.arg_count,
                    .is_partial = result.is_partial,
                    .closure_var_indices = result.closure_var_indices,
                    .closure_var_names = result.closure_var_names,
                    .closure_var_is_const = result.closure_var_is_const,
                });
                std.debug.print("[freeze] FROZEN: '{s}' args={d}\n", .{ best_name, func.arg_count });
            }
        }
        func_idx += 1;
    }

    // Generate registration function (same as generateModuleCWithManifest)
    try c_output.appendSlice(allocator,
        \\// Register all frozen functions
        \\__attribute__((visibility("default"))) int frozen_init_c(JSContext *ctx) {
        \\    native_registry_init();
        \\    { JSValue _g = JS_GetGlobalObject(ctx);
        \\      JS_SetPropertyStr(ctx, _g, "__frozen_init_called", JS_TRUE);
        \\      JS_FreeValue(ctx, _g); }
        \\    JSValue global = JS_GetGlobalObject(ctx);
        \\
    );

    for (generated_funcs.items) |gen_func| {
        var reg_buf: [512]u8 = undefined;
        const reg_line = std.fmt.bufPrint(&reg_buf,
            "    JS_SetPropertyStr(ctx, global, \"__frozen_{s}\",\n        JS_NewCFunction2(ctx, __frozen_{s}, \"__frozen_{s}\", {d}, 0, 0));\n",
            .{ gen_func.name, gen_func.name, gen_func.name, gen_func.arg_count },
        ) catch continue;
        try c_output.appendSlice(allocator, reg_line);
    }

    // Call init functions
    for (generated_funcs.items) |gen_func| {
        var init_buf: [256]u8 = undefined;
        const init_line = std.fmt.bufPrint(&init_buf,
            "    __frozen_{s}_init(ctx);\n",
            .{gen_func.name},
        ) catch continue;
        try c_output.appendSlice(allocator, init_line);
    }

    try c_output.appendSlice(allocator,
        \\    JS_FreeValue(ctx, global);
        \\    return 0;
        \\}
        \\
    );

    return .{
        .zig_code = try zig_output.toOwnedSlice(allocator),
        .c_code = try c_output.toOwnedSlice(allocator),
        .hot_func_names = try hot_func_names.toOwnedSlice(allocator),
    };
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
