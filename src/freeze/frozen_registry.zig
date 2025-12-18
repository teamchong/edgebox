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
const module_parser = @import("module_parser.zig");
const bytecode_parser = @import("bytecode_parser.zig");
const symbolic_stack = @import("symbolic_stack.zig");
const codegen_ssa = @import("codegen_ssa.zig");
const cfg_builder = @import("cfg_builder.zig");

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

/// Export for C ABI (called from WASM)
pub export fn frozen_init(ctx: *JSContext) c_int {
    init(ctx);
    return 0;
}

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
        // The previous approaches had bugs:
        // 1. get_var_ref0 + call pattern: False positive when calling different functions via closure
        // 2. tail_call detection: tail_call is used for ANY function in tail position, not just self
        // Without access to the closure structure, we can't reliably detect self-recursion.
        // Functions will still be frozen correctly, just without the tail-call-optimization.
        const is_self_recursive = false;

        // Get function name - must duplicate since parser will be freed
        const parser_name = parser.getAtomString(func_info.name_atom) orelse "anonymous";
        const name = try allocator.dupe(u8, parser_name);

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
    var header_buf: [256]u8 = undefined;
    const header = std.fmt.bufPrint(&header_buf,
        \\// Auto-generated frozen functions for module: {s}
        \\// Generated by Zig freeze system
        \\
        \\#include "quickjs.h"
        \\#include "quickjs-libc.h"
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
const ManifestFunction = struct {
    name: []const u8,
    argCount: u32,
    isSelfRecursive: bool,
};

/// Parse manifest JSON to get function names
fn parseManifest(allocator: Allocator, manifest_json: []const u8) ![]ManifestFunction {
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
fn freeManifest(allocator: Allocator, manifest: []ManifestFunction) void {
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
    var header_buf: [256]u8 = undefined;
    const header = std.fmt.bufPrint(&header_buf,
        \\// Auto-generated frozen functions for module: {s}
        \\// Generated by Zig freeze system
        \\
        \\#include "quickjs.h"
        \\#include "quickjs-libc.h"
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
    // Count actual frozen functions
    const frozen_count = generated_funcs.items.len;
    var count_buf: [64]u8 = undefined;
    const count_str = std.fmt.bufPrint(&count_buf, "{d}", .{frozen_count}) catch "?";

    try output.appendSlice(allocator, "// Register all frozen functions\nint frozen_init(JSContext *ctx) {\n    printf(\"[frozen_init] Registering ");
    try output.appendSlice(allocator, count_str);
    try output.appendSlice(allocator, " frozen functions\\n\");\n");

    // For partial freeze functions, register their bytecode for lazy fallback loading
    for (generated_funcs.items) |gen_func| {
        if (gen_func.is_partial) {
            var reg_buf: [256]u8 = undefined;
            // Note: bytecode registration is done in the _init function, call it to set up the registry entry
            // The actual function lookup happens lazily at fallback time
            const reg_line = std.fmt.bufPrint(&reg_buf,
                "    frozen_register_bytecode(\"{s}\", _{s}_bytecode, sizeof(_{s}_bytecode));\n",
                .{ gen_func.name, gen_func.name, gen_func.name },
            ) catch continue;
            try output.appendSlice(allocator, reg_line);
        }
    }

    // Call per-function init functions (sets up _this_func, constant pools, etc.)
    for (generated_funcs.items) |gen_func| {
        var init_buf: [256]u8 = undefined;
        const init_line = std.fmt.bufPrint(&init_buf,
            "    __frozen_{s}_init(ctx);\n",
            .{gen_func.name},
        ) catch continue;
        try output.appendSlice(allocator, init_line);
    }

    // Register all frozen functions in globalThis (for hook injection to call)
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
            \\    JSValue result = JS_Eval(ctx, fallback_code, strlen(fallback_code), "<frozen_init>", JS_EVAL_TYPE_GLOBAL);
            \\    if (JS_IsException(result)) {
            \\        js_std_dump_error(ctx);
            \\    }
            \\    JS_FreeValue(ctx, result);
            \\
            \\
        );
    }

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
    cfg_builder.analyzeContamination(&cfg);

    // Check if we can freeze (fully or partially)
    const counts = cfg_builder.countBlocks(&cfg);
    const has_clean_blocks = counts.clean > 0;
    const has_contaminated_blocks = counts.contaminated > 0;
    const has_closure_vars = closure_usage.all_indices.len > 0;

    // If no clean blocks at all, can't freeze
    if (!has_clean_blocks) return null;

    // Determine if this is a partial freeze (some contaminated blocks)
    var partial_freeze = has_contaminated_blocks;

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

    // Enable native int32 mode for self-recursive functions with 1 arg (like fib)
    // This gives 18x speedup by avoiding JSValue boxing in the hot path
    // But NOT for partial freeze (need JSValue for interpreter fallback)
    const use_native_int32 = func.is_self_recursive and func.arg_count == 1 and !partial_freeze and !has_closure_vars;

    // Dupe closure indices for codegen (will outlive this function)
    const closure_indices = try allocator.dupe(u16, closure_usage.all_indices);

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

    // If we couldn't resolve all closure var names, fall back to partial freeze
    // This ensures we don't generate invalid JS in the hook patching
    if (!all_names_resolved and has_closure_vars) {
        std.debug.print("[freeze] Warning: Could not resolve all closure var names for '{s}', using partial freeze\n", .{name});
        partial_freeze = true;
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
