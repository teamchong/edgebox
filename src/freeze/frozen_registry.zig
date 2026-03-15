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
const llvm_codegen = @import("llvm_codegen.zig");
const call_profile = @import("call_profile.zig");

const JSValue = jsvalue.JSValue;
const JSContext = jsvalue.JSContext;
const FrozenFn = jsvalue.FrozenFn;
const FrozenEntry = jsvalue.FrozenEntry;
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
    /// Stack size (from bytecode, for sizing the operand stack array)
    stack_size: u32 = 256,
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
    /// Function kind: 0=normal, 1=generator, 2=async, 3=async generator
    func_kind: u2 = 0,
    /// Local indices captured by child closures (for targeted reverse sync in relooper)
    captured_local_indices: []const u16 = &.{},
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
                .is_local = cv.is_local,
            };
        }

        // Compute which local indices are captured by child closures
        // This enables targeted reverse sync (only captured locals, not all) in the relooper.
        var captured_set = std.AutoHashMapUnmanaged(u16, void){};
        defer captured_set.deinit(allocator);
        for (func_info.constants) |c| {
            switch (c) {
                .child_func => |child| {
                    if (child.func_idx < parser.functions.items.len) {
                        const child_func = parser.functions.items[child.func_idx];
                        for (child_func.closure_vars) |cv| {
                            if (cv.is_local and cv.var_idx < func_info.var_count) {
                                captured_set.put(allocator, @intCast(cv.var_idx), {}) catch {};
                            }
                        }
                    }
                },
                else => {},
            }
        }
        var captured_list = std.ArrayListUnmanaged(u16){};
        var cap_it = captured_set.keyIterator();
        while (cap_it.next()) |key| {
            captured_list.append(allocator, key.*) catch {};
        }
        const captured_local_indices_slice = captured_list.toOwnedSlice(allocator) catch &.{};

        try result.functions.append(allocator, .{
            .name = name,
            .bytecode = func_info.bytecode,
            .instructions = instructions,
            .arg_count = func_info.arg_count,
            .var_count = func_info.var_count,
            .stack_size = func_info.stack_size,
            .is_self_recursive = is_self_recursive,
            .self_ref_var_idx = self_ref_var_idx,
            .can_freeze = can_freeze_final,
            .freeze_block_reason = freeze_check.reason,
            .constants = constants_copy,
            .atom_strings = atom_strings_copy, // Share reference
            .closure_vars = closure_vars_copy,
            .line_num = func_info.line_num, // For name@line_num dispatch key
            .has_use_strict = func_info.has_use_strict, // For proper 'this' handling
            .func_kind = func_info.func_kind, // 0=normal, 1=generator, 2=async, 3=async_generator
            .captured_local_indices = captured_local_indices_slice,
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
fn hasKillerOpcodes(instructions: []const bytecode_parser.Instruction, is_self_recursive: bool) bool {
    _ = is_self_recursive; // No longer used
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
            // Exception handling (requires bytecode PC which frozen code doesn't have)
            .@"catch", .nip_catch, .gosub, .ret,
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

    // Check if this is a pure int32 self-recursive function - use hot path for 18x speedup!
    // Non-recursive int32 functions (isLineBreak, isDigit, etc.) go through zig_codegen_full
    // which handles them via emitInt32Specialized with switch-based block dispatch.
    if (isPureInt32Function(func) and func.is_self_recursive and !partial_freeze) {
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
            .closure_var_count = @intCast(func.closure_vars.len),
            .atom_strings = func.atom_strings,
            .partial_freeze = partial_freeze,
            .js_name = func.name,
            .is_pure_int32 = is_pure_int32,
            .has_use_strict = func.has_use_strict, // For proper 'this' handling
            .is_async = func.func_kind >= 2, // async or async_generator
            .constants = func.constants, // For fclosure bytecode registration
            .stack_size = func.stack_size,
            .captured_local_indices = func.captured_local_indices,
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
        .closure_var_count = @intCast(func.closure_vars.len),
        .has_use_strict = func.has_use_strict, // For proper 'this' handling
        .is_async = func.func_kind >= 2, // async or async_generator
        .constants = func.constants, // For fclosure bytecode registration
        .stack_size = func.stack_size,
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

/// Generate thin-tier Zig code for a cold function.
/// Uses ThinCodeGen: one-line-per-opcode, no vstack, minimal code size.
/// Returns null if function cannot be frozen (killer opcodes, CFG issues).
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
    module_name: []const u8,
    manifest_json: ?[]const u8,
    max_functions: u32,
    profile_path: ?[]const u8,
    output_dir: ?[]const u8,
    code_budget: u32,
) !ShardedOutput {
    _ = module_name;
    _ = manifest_json;
    // Collect all freezable functions
    const GeneratedFunc = struct {
        func: AnalyzedFunction,
        idx: usize,
    };
    var generated_all = std.ArrayListUnmanaged(GeneratedFunc){};
    defer generated_all.deinit(allocator);

    // Filter: skip functions with unsupported opcodes or unbuildable CFGs
    for (analysis.functions.items, 0..) |func, idx| {
        if (hasKillerOpcodes(func.instructions, func.is_self_recursive)) continue;
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
    if (effective_max == 0 and code_budget > 0 and profile_path != null) {
        // Auto-compute function limit from code budget
        var cumulative_size: u64 = 0;
        for (generated_all.items, 0..) |gf, i| {
            const est_size = @as(u64, gf.func.instructions.len) * BYTES_PER_INSTRUCTION;
            cumulative_size += est_size;
            if (cumulative_size > code_budget) {
                effective_max = @intCast(i);
                std.debug.print("[freeze-pgo] Auto-sized to {d} functions for {d}KB code budget ({d}KB estimated)\n", .{
                    effective_max,
                    code_budget / 1024,
                    (cumulative_size - est_size) / 1024,
                });
                break;
            }
        }
        if (effective_max == 0) {
            // All functions fit within budget
            std.debug.print("[freeze-pgo] All {d} functions fit within {d}KB code budget ({d}KB estimated)\n", .{
                generated_all.items.len,
                code_budget / 1024,
                cumulative_size / 1024,
            });
        }
    } else if (effective_max == 0 and code_budget == 0 and profile_path != null) {
        // Auto-detect L2 cache size and use as default budget
        const l2_budget = detectL2CacheSize();
        if (l2_budget > 0) {
            var cumulative_size: u64 = 0;
            for (generated_all.items, 0..) |gf, i| {
                const est_size = @as(u64, gf.func.instructions.len) * BYTES_PER_INSTRUCTION;
                cumulative_size += est_size;
                if (cumulative_size > l2_budget) {
                    effective_max = @intCast(i);
                    std.debug.print("[freeze-pgo] Auto-sized to {d} functions for L2 cache ({d}KB, {d}KB estimated code)\n", .{
                        effective_max,
                        l2_budget / 1024,
                        (cumulative_size - est_size) / 1024,
                    });
                    break;
                }
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

            // Generate standalone WASM for int32 functions (no QuickJS runtime)
            // These can run directly in V8/workerd at near-native speed
            {
                var standalone_path_buf: [4096]u8 = undefined;
                const standalone_path = std.fmt.bufPrintZ(&standalone_path_buf, "{s}/standalone.o", .{cache_dir}) catch null;
                if (standalone_path) |sp| {
                    if (llvm_codegen.generateStandaloneWasm(allocator, llvm_funcs.items, sp)) |standalone_result| {
                        if (standalone_result.has_functions) {
                            std.debug.print("[freeze] Standalone WASM: {d} pure functions → {s}\n", .{ standalone_result.func_count, sp });
                        }
                    } else |err| {
                        std.debug.print("[freeze] Standalone WASM generation failed: {}\n", .{err});
                    }
                }
            }

            // NOTE: Int32 functions are NOT removed from generated_all.
            // They remain in Zig shards for cross-platform compatibility (embed, standalone/WASM).
            // The LLVM .o files re-register the same dispatch keys with LLVM-compiled versions.
            // Since native_dispatch.register() uses put() (overwrites), the LLVM versions win
            // because LLVM shard constructors run AFTER Zig shard inits.
        }

        // ===== Phase 2: Thin codegen for ALL remaining functions =====
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


            const cfg = cfg_builder.buildCFG(allocator, gf.func.instructions) catch |err| {
                std.debug.print("[freeze-debug] CFG build failed for '{s}'@{d} (gen_idx={d}): {}\n", .{ gf.func.name, gf.func.line_num, gi, err });
                continue;
            };
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

        std.debug.print("[freeze-debug] Thin loop: {d} generated_all, {d} skipped int32, {d} thin infos, {d} CFG fail\n", .{
            generated_all.items.len, thin_skipped_int32, thin_infos.items.len, generated_all.items.len - thin_skipped_int32 - thin_infos.items.len,
        });

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

    std.debug.print("[freeze] LLVM backend: {d} functions ({d} int32 + {d} thin) in {d} shard .o files\n", .{
        generated_all.items.len,
        int32_llvm_shard_count,
        llvm_shard_count - int32_llvm_shard_count,
        llvm_shard_count,
    });

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

    // Generate init function that registers all frozen functions
    try output.appendSlice(allocator,
        \\
        \\// Native registry functions (from native_shapes.zig)
        \\extern fn native_registry_init() void;
        \\extern fn native_registry_count() c_int;
        \\extern fn native_node_register(js_addr: u64, kind: i32, flags: i32, pos: i32, end: i32) ?*anyopaque;
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
        \\    // Use full 64-bit pointer address (matches jsvalueToAddr in frozen_helpers.zig)
        \\    const ptr_addr: u64 = @intFromPtr(obj.getPtr());
        \\    const node = native_node_register(ptr_addr, kind, flags, pos, end);
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

    // Register each generated function with native dispatch using parser index
    for (analysis.functions.items, 0..) |func, idx| {
        // Only register functions that were actually generated (skip partial freeze, etc.)
        if (!generated_indices.contains(idx)) continue;
        if (!func.can_freeze) continue;
        // Skip Phase 2 (byte-scanned) functions — no valid runtime index
        if (func.parser_index == 0xFFFFFFFF) continue;

        var reg_buf: [512]u8 = undefined;
        // Sanitize name for Zig identifier (replace colons, etc.)
        var sanitized_buf: [256]u8 = undefined;
        const sanitized_name = sanitizeName(func.name, &sanitized_buf);
        // Use parser_index for O(1) index-based dispatch
        const reg_line = std.fmt.bufPrint(&reg_buf,
            \\    // Register parser_index={d} ({s}) with native dispatch
            \\    native_dispatch.registerByIndex({d}, &__frozen_{d}_{s});
            \\    count += 1;
            \\
        , .{ func.parser_index, func.name, func.parser_index, idx, sanitized_name }) catch continue;
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
        if (handler.pattern == .unsupported) return false;
        // These patterns prove the function actually computes with integers
        switch (handler.pattern) {
            .binary_arith_i32, .binary_cmp_i32, .bitwise_binary_i32,
            .unary_i32, .inc_dec_i32, .lnot_i32, .push_const_i32,
            .push_bool_i32, .call_self_i32, .tail_call_self_i32,
            => has_int32_op = true,
            else => {},
        }
    }

    // Reject functions that are just pass-throughs (e.g., `function f(x) { return x; }`)
    // — they can be called with any type and int32 specialization would corrupt objects.
    if (!has_int32_op) return false;

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
