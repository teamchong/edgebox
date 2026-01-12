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
const zig_codegen = @import("zig_codegen.zig");
const opcodes = @import("opcodes.zig");
pub const module_parser = @import("module_parser.zig");
const bytecode_parser = @import("bytecode_parser.zig");
const cfg_builder = @import("cfg_builder.zig");
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
