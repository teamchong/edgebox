//! Frozen Function Helpers
//!
//! Provides runtime support for frozen (AOT-compiled) JavaScript functions:
//! - Stack depth management for recursion limits
//! - Closure variable access
//! - Iterator protocol support
//! - Block fallback for partial freeze
//! - Native shape access (TypeScript AST nodes)
//! - Cached atoms for fast property access

const std = @import("std");
const js_value = @import("js_value.zig");
const types = @import("js_types.zig");

const JSContext = js_value.JSContext;
const JSValue = js_value.JSValue;
const JSVarRef = js_value.JSVarRef;
const CompressedValue = js_value.CompressedValue;
const quickjs = js_value.quickjs;
const is_wasm32 = js_value.is_wasm32;

// Import tag constants
const JS_TAG_CATCH_OFFSET = types.JS_TAG_CATCH_OFFSET;
const JS_TAG_OBJECT = types.JS_TAG_OBJECT;
const JS_TAG_STRING = types.JS_TAG_STRING;
const JS_TAG_SYMBOL = types.JS_TAG_SYMBOL;
const JS_TAG_BIG_INT = types.JS_TAG_BIG_INT;
const JS_TAG_FUNCTION_BYTECODE = types.JS_TAG_FUNCTION_BYTECODE;

// ============================================================================
// Stack Management
// ============================================================================

pub const FROZEN_MAX_CALL_DEPTH: usize = 10000;

/// Global call depth counter - tracks frozen function recursion
pub var frozen_call_depth: usize = 0;

/// Check and increment call depth, return true if stack overflow
pub inline fn checkStack() bool {
    if (frozen_call_depth >= FROZEN_MAX_CALL_DEPTH) {
        return true; // Stack overflow
    }
    frozen_call_depth += 1;
    return false;
}

/// Decrement call depth on function exit
pub inline fn exitStack() void {
    if (frozen_call_depth > 0) {
        frozen_call_depth -= 1;
    }
}

/// Reset call depth (call at start of each request)
pub fn frozen_reset_call_depth_zig() void {
    frozen_call_depth = 0;
}

// ============================================================================
// Closure Variable Access
// Closure variables are accessed through var_refs from the function's closure
// The closure_var_indices map bytecode indices to var_refs array positions
// ============================================================================

/// Get closure variable from var_refs array (safe version with bounds checking)
/// @param ctx - JSContext for memory management
/// @param var_refs - array of closure variable references from QuickJS
/// @param position - index into var_refs array
/// @param closure_var_count - size of var_refs array for bounds checking
pub inline fn getClosureVarSafe(ctx: *JSContext, var_refs: ?[*]*JSVarRef, position: u32, closure_var_count: c_int) JSValue {
    // Use C FFI for correct struct layout handling with bounds checking
    if (var_refs != null and closure_var_count > 0) {
        return quickjs.js_frozen_get_var_ref_safe(ctx, @ptrCast(var_refs), @intCast(position), closure_var_count);
    }
    return JSValue.UNDEFINED;
}

/// Get closure variable from var_refs array (legacy version - no bounds checking)
/// @param ctx - JSContext for memory management
/// @param var_refs - array of closure variable references from QuickJS
/// @param position - index into var_refs array
pub inline fn getClosureVar(ctx: *JSContext, var_refs: ?[*]*JSVarRef, position: u32) JSValue {
    // Use C FFI for correct struct layout handling
    // The JSVarRef struct layout in C doesn't match Zig's extern struct due to
    // GC header union alignment differences
    if (var_refs != null) {
        return quickjs.js_frozen_get_var_ref(ctx, @ptrCast(var_refs), @intCast(position));
    }
    return JSValue.UNDEFINED;
}

/// Set closure variable in var_refs array (safe version with bounds checking)
/// @param ctx - JSContext for memory management
/// @param var_refs - array of closure variable references from QuickJS
/// @param position - index into var_refs array
/// @param closure_var_count - size of var_refs array for bounds checking
/// @param val - value to set (ownership transferred)
pub inline fn setClosureVarSafe(ctx: *JSContext, var_refs: ?[*]*JSVarRef, position: u32, closure_var_count: c_int, val: JSValue) void {
    // Use C FFI for correct struct layout handling with bounds checking
    if (var_refs != null and closure_var_count > 0) {
        quickjs.js_frozen_set_var_ref_safe(ctx, @ptrCast(var_refs), @intCast(position), closure_var_count, val);
    }
}

/// Set closure variable with dup (for set_var_ref which keeps value on stack)
/// Unlike setClosureVarSafe, this duplicates the value before storing since
/// the original stays on the stack. Used for expressions like ++x where the
/// incremented value is both stored and used.
/// @param ctx - JSContext for memory management
/// @param var_refs - array of closure variable references from QuickJS
/// @param position - index into var_refs array
/// @param closure_var_count - size of var_refs array for bounds checking
/// @param val - value to store (will be duplicated, original ownership retained by caller)
pub inline fn setClosureVarDupSafe(ctx: *JSContext, var_refs: ?[*]*JSVarRef, position: u32, closure_var_count: c_int, val: JSValue) void {
    // Use C FFI for correct struct layout handling with bounds checking
    // Must dup because original stays on stack
    if (var_refs != null and closure_var_count > 0) {
        quickjs.js_frozen_set_var_ref_safe(ctx, @ptrCast(var_refs), @intCast(position), closure_var_count, JSValue.dup(ctx, val));
    }
}

/// Set closure variable in var_refs array (legacy version - no bounds checking)
/// @param ctx - JSContext for memory management
/// @param var_refs - array of closure variable references from QuickJS
/// @param position - index into var_refs array
/// @param val - value to set (ownership transferred)
pub inline fn setClosureVar(ctx: *JSContext, var_refs: ?[*]*JSVarRef, position: u32, val: JSValue) void {
    // Use C FFI for correct struct layout handling
    if (var_refs != null) {
        quickjs.js_frozen_set_var_ref(ctx, @ptrCast(var_refs), @intCast(position), val);
    }
}

/// Get closure variable with TDZ (Temporal Dead Zone) check (safe version)
/// Returns EXCEPTION if variable is uninitialized
pub inline fn getClosureVarCheckSafe(ctx: *JSContext, var_refs: ?[*]*JSVarRef, position: u32, closure_var_count: c_int) JSValue {
    const val = getClosureVarSafe(ctx, var_refs, position, closure_var_count);
    if (val.isUninitialized()) {
        return JSValue.throwReferenceError(ctx, "Cannot access '%s' before initialization");
    }
    return val;
}

/// Get closure variable with TDZ (Temporal Dead Zone) check (legacy version)
/// Returns EXCEPTION if variable is uninitialized
pub inline fn getClosureVarCheck(ctx: *JSContext, var_refs: ?[*]*JSVarRef, position: u32) JSValue {
    const val = getClosureVar(ctx, var_refs, position);
    if (val.isUninitialized()) {
        return JSValue.throwReferenceError(ctx, "Cannot access '%s' before initialization");
    }
    return val;
}

/// Set closure variable with TDZ check (safe version)
/// Returns true if variable was uninitialized (error condition)
pub inline fn setClosureVarCheckSafe(ctx: *JSContext, var_refs: ?[*]*JSVarRef, position: u32, closure_var_count: c_int, val: JSValue) bool {
    const existing = getClosureVarSafe(ctx, var_refs, position, closure_var_count);
    JSValue.free(ctx, existing);
    if (existing.isUninitialized()) {
        JSValue.free(ctx, val);
        _ = JSValue.throwReferenceError(ctx, "Cannot access '%s' before initialization");
        return true; // Error
    }
    setClosureVarSafe(ctx, var_refs, position, closure_var_count, val);
    return false;
}

/// Set closure variable with TDZ check (legacy version)
/// Returns true if variable was uninitialized (error condition)
pub inline fn setClosureVarCheck(ctx: *JSContext, var_refs: ?[*]*JSVarRef, position: u32, val: JSValue) bool {
    const existing = getClosureVar(ctx, var_refs, position);
    JSValue.free(ctx, existing);
    if (existing.isUninitialized()) {
        JSValue.free(ctx, val);
        _ = JSValue.throwReferenceError(ctx, "Cannot access '%s' before initialization");
        return true; // Error
    }
    setClosureVar(ctx, var_refs, position, val);
    return false;
}

// ============================================================================
// Iterator Protocol Helpers
// ============================================================================

/// Create a catch offset value (for try/catch/finally and iterator cleanup)
/// This is a pure Zig implementation matching JS_NewCatchOffset macro
pub inline fn newCatchOffset(val: i32) JSValue {
    if (is_wasm32) {
        // WASM32: NaN-boxed u64 (tag in upper 32 bits, payload in lower 32 bits)
        return JSValue{ .bits = (@as(u64, @intCast(@as(u32, @bitCast(@as(i32, JS_TAG_CATCH_OFFSET))))) << 32) | @as(u64, @intCast(@as(u32, @bitCast(val)))) };
    } else {
        // Native: 16-byte struct
        return JSValue{ .u = .{ .int32 = val }, .tag = JS_TAG_CATCH_OFFSET };
    }
}

/// Initialize for-of iterator
/// Stack before: [obj] at sp-1
/// Stack after: [iterator, next_method, catch_offset] (sp increases by 2)
/// Returns 0 on success, non-zero on error
pub inline fn forOfStart(ctx: *JSContext, stack: [*]JSValue, sp: *usize) c_int {
    // js_frozen_for_of_start expects pointer to current stack position
    // It reads obj from sp[-1], writes iterator to sp[-1], next_method to sp[0]
    const result = quickjs.js_frozen_for_of_start(ctx, stack + sp.*, 0);
    if (result != 0) return result;
    sp.* += 1; // next_method is now at sp
    // Add catch_offset at new sp position
    stack[sp.*] = newCatchOffset(0);
    sp.* += 1;
    return 0;
}

/// Get next value from for-of iterator
/// Stack before: [iterator, next_method, catch_offset, ...] with offset indicating iterator position
/// Stack after: [..., value, done] (sp increases by 2)
/// Returns 0 on success, non-zero on error
pub inline fn forOfNext(ctx: *JSContext, stack: [*]JSValue, sp: *usize, offset: i32) c_int {
    // js_frozen_for_of_next takes negative offset from current sp to iterator
    // offset 0 from bytecode means iterator is at sp-3 (offset -3)
    const result = quickjs.js_frozen_for_of_next(ctx, stack + sp.*, -(offset + 3));
    if (result != 0) return result;
    sp.* += 2; // value at sp-2, done at sp-1
    return 0;
}

/// Close iterator (cleanup after for-of loop)
/// Stack before: [iterator, next_method, catch_offset] at positions sp-3, sp-2, sp-1
/// Stack after: [] (sp decreases by 3)
/// Calls iterator.return() if it exists
pub fn iteratorClose(ctx: *JSContext, stack: [*]JSValue, sp: *usize) void {
    // Pop catch_offset (no need to free, it's a primitive)
    sp.* -= 1;

    // Pop next_method
    sp.* -= 1;
    const next_method = stack[sp.*];
    JSValue.free(ctx, next_method);

    // Pop iterator
    sp.* -= 1;
    const iter = stack[sp.*];

    // Call iterator.return() if it exists
    if (!iter.isUndefined()) {
        const ret_method = JSValue.getPropertyStr(ctx, iter, "return");
        if (!ret_method.isUndefined() and !ret_method.isNull()) {
            const ret = quickjs.JS_Call(ctx, ret_method, iter, 0, @ptrFromInt(0));
            JSValue.free(ctx, ret_method);
            // Don't propagate exception from return(), just clean up
            if (!ret.isException()) {
                JSValue.free(ctx, ret);
            }
        } else {
            JSValue.free(ctx, ret_method);
        }
        JSValue.free(ctx, iter);
    }
}

/// Check if iterator is done (top of stack is the done flag)
pub inline fn iteratorIsDone(stack: [*]JSValue, sp: usize) bool {
    const done = stack[sp - 1];
    // done is either true or false
    return done.isBool() and done.getBool();
}

/// Get iterator value (second from top after for_of_next)
pub inline fn iteratorGetValue(stack: [*]JSValue, sp: usize) JSValue {
    return stack[sp - 2];
}

// ============================================================================
// Object Operations
// ============================================================================

/// Copy data properties from source to destination (for spread operator)
/// excluded: JSValue object with keys to exclude (or UNDEFINED for no exclusion)
pub fn copyDataProperties(ctx: *JSContext, dst: JSValue, src: JSValue, excluded: JSValue) c_int {
    return quickjs.js_frozen_copy_data_properties(ctx, dst, src, excluded);
}

// ============================================================================
// Partial Freeze Block Fallback
// ============================================================================

/// FFI declaration for frozen_block_fallback (defined in frozen_runtime.c)
/// Called when execution enters a contaminated block (one with never_freeze opcodes)
/// Transfers control to the interpreter for that block, then returns control
pub extern fn frozen_block_fallback(
    ctx: *JSContext,
    func_name: [*:0]const u8,
    this_val: JSValue,
    argc: c_int,
    argv: [*]JSValue,
    locals: [*]JSValue,
    num_locals: c_int,
    stack: [*]JSValue,
    sp: *c_int,
    block_id: c_int,
    next_block_out: *c_int,
) JSValue;

/// High-level wrapper for block fallback
/// Executes a contaminated block in the interpreter and returns control to native code
/// Returns: result if function completed, or UNDEFINED to continue to next_block
pub fn blockFallback(
    ctx: *JSContext,
    func_name: [*:0]const u8,
    this_val: JSValue,
    argc: c_int,
    argv: [*]JSValue,
    locals: [*]JSValue,
    num_locals: c_int,
    stack: [*]JSValue,
    sp: *usize,
    block_id: usize,
    next_block: *usize,
) JSValue {
    var sp_int: c_int = @intCast(sp.*);
    var next_block_int: c_int = -1;

    const result = frozen_block_fallback(
        ctx,
        func_name,
        this_val,
        argc,
        argv,
        locals,
        num_locals,
        stack,
        &sp_int,
        @intCast(block_id),
        &next_block_int,
    );

    // Update sp from C value
    sp.* = @intCast(sp_int);

    // Update next_block (-1 means function completed, otherwise block id)
    if (next_block_int >= 0) {
        next_block.* = @intCast(next_block_int);
    }

    return result;
}

/// CV-aware block fallback for partial_freeze functions
/// Converts CV arrays to JSValue, calls fallback, converts back
pub fn blockFallbackCV(
    ctx: *JSContext,
    func_name: [*:0]const u8,
    this_val: JSValue,
    argc: c_int,
    argv: [*]JSValue,
    cv_locals: [*]CompressedValue,
    num_locals: c_int,
    cv_stack: [*]CompressedValue,
    sp: *usize,
    block_id: usize,
    next_block: *usize,
) JSValue {
    // Convert CV locals to JSValue (temporary arrays)
    var js_locals: [256]JSValue = .{JSValue.UNDEFINED} ** 256;
    var js_stack: [256]JSValue = .{JSValue.UNDEFINED} ** 256;

    const local_count: usize = @intCast(num_locals);
    for (0..local_count) |i| {
        js_locals[i] = cv_locals[i].toJSValueWithCtx(ctx);
    }

    const stack_count = sp.*;
    for (0..stack_count) |i| {
        js_stack[i] = cv_stack[i].toJSValueWithCtx(ctx);
    }

    // Call the JSValue-based fallback
    const result = blockFallback(
        ctx,
        func_name,
        this_val,
        argc,
        argv,
        &js_locals,
        num_locals,
        &js_stack,
        sp,
        block_id,
        next_block,
    );

    // Convert JSValue results back to CV
    for (0..local_count) |i| {
        cv_locals[i] = CompressedValue.fromJSValue(js_locals[i]);
    }

    const new_stack_count = sp.*;
    for (0..new_stack_count) |i| {
        cv_stack[i] = CompressedValue.fromJSValue(js_stack[i]);
    }

    return result;
}

// ============================================================================
// Native Shape Access
// Zero-overhead property access for known shapes (like TypeScript AST nodes)
// ============================================================================

/// Native AstNode structure - mirrors TypeScript AST node (defined in native_shapes.zig)
pub const NativeAstNode = extern struct {
    kind: i32,
    flags: i32,
    pos: i32,
    end: i32,
    parent: ?*NativeAstNode,
    js_value: u64,
    modifier_flags_cache: i32,
    transform_flags: i32,
};

/// Convert JSValue to address for registry lookup
/// Uses the raw representation as a unique identifier
pub inline fn jsvalueToAddr(val: JSValue) u64 {
    if (is_wasm32) {
        // WASM32: 64-bit NaN-boxed value, use bits directly
        return val.bits;
    } else {
        // Native 64-bit: Use both tag and value parts for unique address
        // This matches the C macro: union { JSValue v; uint64_t u; } u; u.u = val;
        // For 16-byte JSValue, we use tag as high bits, value.ptr as low bits
        const ptr_bits: u64 = @intFromPtr(val.u.ptr);
        const tag_bits: u64 = @bitCast(val.tag);
        return ptr_bits ^ (tag_bits << 32);
    }
}

/// Import native registry lookup from native_shapes.zig
/// The native_shapes module provides a SIMD-optimized columnar registry
extern fn native_shapes_lookup(js_addr: u64) ?*NativeAstNode;

/// Fast lookup for native node - delegates to native_shapes.zig implementation
/// Returns null if not registered
pub inline fn native_node_lookup(js_addr: u64) ?*NativeAstNode {
    if (js_addr == 0) return null;
    return native_shapes_lookup(js_addr);
}

/// Get node.kind with native fast path
/// Falls back to JSValue.getPropertyStr if not a native node
pub inline fn nativeGetKind(ctx: *JSContext, obj: JSValue) JSValue {
    const native = native_node_lookup(jsvalueToAddr(obj));
    if (native) |node| {
        return JSValue.newInt(node.kind);
    }
    return JSValue.getPropertyStr(ctx, obj, "kind");
}

/// Get node.flags with native fast path
pub inline fn nativeGetFlags(ctx: *JSContext, obj: JSValue) JSValue {
    const native = native_node_lookup(jsvalueToAddr(obj));
    if (native) |node| {
        return JSValue.newInt(node.flags);
    }
    return JSValue.getPropertyStr(ctx, obj, "flags");
}

/// Get node.pos with native fast path
pub inline fn nativeGetPos(ctx: *JSContext, obj: JSValue) JSValue {
    const native = native_node_lookup(jsvalueToAddr(obj));
    if (native) |node| {
        return JSValue.newInt(node.pos);
    }
    return JSValue.getPropertyStr(ctx, obj, "pos");
}

/// Get node.end with native fast path
pub inline fn nativeGetEnd(ctx: *JSContext, obj: JSValue) JSValue {
    const native = native_node_lookup(jsvalueToAddr(obj));
    if (native) |node| {
        return JSValue.newInt(node.end);
    }
    return JSValue.getPropertyStr(ctx, obj, "end");
}

/// Get node.parent with native fast path
/// Returns the original JSValue of the parent if found, otherwise falls back
pub inline fn nativeGetParent(ctx: *JSContext, obj: JSValue) JSValue {
    const native = native_node_lookup(jsvalueToAddr(obj));
    if (native) |node| {
        if (node.parent) |parent| {
            // Reconstruct JSValue from stored js_value bits
            // The parent's js_value contains the original JSValue bits
            const parent_val: JSValue = if (is_wasm32)
                // WASM32: 64-bit NaN-boxed, bits stored directly
                .{ .bits = parent.js_value }
            else
                // Native 64-bit: Separate tag and pointer
                .{
                    .u = .{ .ptr = @ptrFromInt(@as(usize, @truncate(parent.js_value))) },
                    .tag = @bitCast(@as(i64, @intCast(parent.js_value >> 32))),
                };
            return JSValue.dup(ctx, parent_val);
        }
    }
    return JSValue.getPropertyStr(ctx, obj, "parent");
}

/// Check if a property name is optimizable with native shapes
pub fn isNativeProperty(name: []const u8) bool {
    return std.mem.eql(u8, name, "kind") or
        std.mem.eql(u8, name, "flags") or
        std.mem.eql(u8, name, "pos") or
        std.mem.eql(u8, name, "end") or
        std.mem.eql(u8, name, "parent") or
        std.mem.eql(u8, name, "length");
}

/// Get array/string length with native fast path
/// Uses JS_GetLength for arrays (O(1)) instead of property lookup
pub inline fn nativeGetLength(ctx: *JSContext, obj: JSValue) JSValue {
    // JS_GetLength is O(1) for arrays and strings - much faster than getPropertyStr
    var len: i64 = 0;
    if (quickjs.JS_GetLength(ctx, obj, &len) < 0) {
        // Not an array/string, fall back to property access
        return JSValue.getPropertyStr(ctx, obj, "length");
    }
    return JSValue.newInt64(ctx, len);
}

/// Get array/string length returning CompressedValue
/// On WASM32, reads from global slots to avoid LLVM FastISel return corruption
pub inline fn nativeGetLengthCV(ctx: *JSContext, obj: JSValue) CompressedValue {
    const result = nativeGetLength(ctx, obj); // Result written to globals too
    if (is_wasm32) {
        // On WASM32, return value is corrupted by LLVM FastISel, read from globals
        return CompressedValue.fromJSValueFromGlobal();
    } else {
        // On native, the return value is not corrupted
        return CompressedValue.fromJSValue(result);
    }
}

// ============================================================================
// Cached Atoms for Fast Property Access
// ============================================================================

// Pre-computed atoms skip string hashing on every access (~10 cycles saved)

/// Cached atoms for hot TSC properties
pub var cached_atoms: CachedAtoms = .{};

pub const CachedAtoms = struct {
    initialized: bool = false,
    // TSC hot properties (beyond kind/flags/pos/end/parent which use native shapes)
    symbol: u32 = 0,
    escapedName: u32 = 0,
    declarations: u32 = 0,
    valueDeclaration: u32 = 0,
    members: u32 = 0,
    properties: u32 = 0,
    target: u32 = 0,
    constraint: u32 = 0,
    modifiers: u32 = 0,
    name: u32 = 0,
    text: u32 = 0,
    type_: u32 = 0, // "type" is reserved in Zig
    checker: u32 = 0,
    typeArguments: u32 = 0,
    arguments: u32 = 0,
};

/// Initialize cached atoms (call once at module init)
pub fn initCachedAtoms(ctx: *JSContext) void {
    if (cached_atoms.initialized) return;

    cached_atoms.symbol = quickjs.JS_NewAtom(ctx, "symbol");
    cached_atoms.escapedName = quickjs.JS_NewAtom(ctx, "escapedName");
    cached_atoms.declarations = quickjs.JS_NewAtom(ctx, "declarations");
    cached_atoms.valueDeclaration = quickjs.JS_NewAtom(ctx, "valueDeclaration");
    cached_atoms.members = quickjs.JS_NewAtom(ctx, "members");
    cached_atoms.properties = quickjs.JS_NewAtom(ctx, "properties");
    cached_atoms.target = quickjs.JS_NewAtom(ctx, "target");
    cached_atoms.constraint = quickjs.JS_NewAtom(ctx, "constraint");
    cached_atoms.modifiers = quickjs.JS_NewAtom(ctx, "modifiers");
    cached_atoms.name = quickjs.JS_NewAtom(ctx, "name");
    cached_atoms.text = quickjs.JS_NewAtom(ctx, "text");
    cached_atoms.type_ = quickjs.JS_NewAtom(ctx, "type");
    cached_atoms.checker = quickjs.JS_NewAtom(ctx, "checker");
    cached_atoms.typeArguments = quickjs.JS_NewAtom(ctx, "typeArguments");
    cached_atoms.arguments = quickjs.JS_NewAtom(ctx, "arguments");
    cached_atoms.initialized = true;
}

// Native getters using cached atoms (faster than getPropertyStr)
pub inline fn nativeGetSymbol(ctx: *JSContext, obj: JSValue) JSValue {
    if (!cached_atoms.initialized) initCachedAtoms(ctx);
    return quickjs.JS_GetProperty(ctx, obj, cached_atoms.symbol);
}

pub inline fn nativeGetEscapedName(ctx: *JSContext, obj: JSValue) JSValue {
    if (!cached_atoms.initialized) initCachedAtoms(ctx);
    return quickjs.JS_GetProperty(ctx, obj, cached_atoms.escapedName);
}

pub inline fn nativeGetDeclarations(ctx: *JSContext, obj: JSValue) JSValue {
    if (!cached_atoms.initialized) initCachedAtoms(ctx);
    return quickjs.JS_GetProperty(ctx, obj, cached_atoms.declarations);
}

pub inline fn nativeGetValueDeclaration(ctx: *JSContext, obj: JSValue) JSValue {
    if (!cached_atoms.initialized) initCachedAtoms(ctx);
    return quickjs.JS_GetProperty(ctx, obj, cached_atoms.valueDeclaration);
}

pub inline fn nativeGetMembers(ctx: *JSContext, obj: JSValue) JSValue {
    if (!cached_atoms.initialized) initCachedAtoms(ctx);
    return quickjs.JS_GetProperty(ctx, obj, cached_atoms.members);
}

pub inline fn nativeGetProperties(ctx: *JSContext, obj: JSValue) JSValue {
    if (!cached_atoms.initialized) initCachedAtoms(ctx);
    return quickjs.JS_GetProperty(ctx, obj, cached_atoms.properties);
}

pub inline fn nativeGetTarget(ctx: *JSContext, obj: JSValue) JSValue {
    if (!cached_atoms.initialized) initCachedAtoms(ctx);
    return quickjs.JS_GetProperty(ctx, obj, cached_atoms.target);
}

pub inline fn nativeGetConstraint(ctx: *JSContext, obj: JSValue) JSValue {
    if (!cached_atoms.initialized) initCachedAtoms(ctx);
    return quickjs.JS_GetProperty(ctx, obj, cached_atoms.constraint);
}

pub inline fn nativeGetModifiers(ctx: *JSContext, obj: JSValue) JSValue {
    if (!cached_atoms.initialized) initCachedAtoms(ctx);
    return quickjs.JS_GetProperty(ctx, obj, cached_atoms.modifiers);
}

pub inline fn nativeGetName(ctx: *JSContext, obj: JSValue) JSValue {
    if (!cached_atoms.initialized) initCachedAtoms(ctx);
    return quickjs.JS_GetProperty(ctx, obj, cached_atoms.name);
}

pub inline fn nativeGetText(ctx: *JSContext, obj: JSValue) JSValue {
    if (!cached_atoms.initialized) initCachedAtoms(ctx);
    return quickjs.JS_GetProperty(ctx, obj, cached_atoms.text);
}

pub inline fn nativeGetType(ctx: *JSContext, obj: JSValue) JSValue {
    if (!cached_atoms.initialized) initCachedAtoms(ctx);
    return quickjs.JS_GetProperty(ctx, obj, cached_atoms.type_);
}

pub inline fn nativeGetChecker(ctx: *JSContext, obj: JSValue) JSValue {
    if (!cached_atoms.initialized) initCachedAtoms(ctx);
    return quickjs.JS_GetProperty(ctx, obj, cached_atoms.checker);
}

pub inline fn nativeGetTypeArguments(ctx: *JSContext, obj: JSValue) JSValue {
    if (!cached_atoms.initialized) initCachedAtoms(ctx);
    return quickjs.JS_GetProperty(ctx, obj, cached_atoms.typeArguments);
}

pub inline fn nativeGetArguments(ctx: *JSContext, obj: JSValue) JSValue {
    if (!cached_atoms.initialized) initCachedAtoms(ctx);
    return quickjs.JS_GetProperty(ctx, obj, cached_atoms.arguments);
}
