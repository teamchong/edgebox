# Plan: Full Opcode Support for Freeze System

## Current State

- **21 opcodes** marked as `never_freeze`
- **12 are feasible** to support with maintainable code
- **3 are impossible** (eval, apply_eval, import) - require runtime compilation
- **6 are low priority** (generators/async) - complex state machine, low ROI

## Strategy: Call QuickJS C APIs

Instead of reimplementing complex JS semantics in generated C code, we **call back to QuickJS helper functions**. This is:
- ✅ Maintainable (uses existing tested code)
- ✅ Safe (same semantics as interpreter)
- ✅ Future-proof (automatically benefits from QuickJS updates)

## Implementation Phases

### Phase 1: Closures (fclosure, fclosure8)
**Impact: High** - Enables nested functions, callbacks, higher-order functions

```c
// Current: SKIP entire function
// New: Call js_closure()
CASE(OP_fclosure):
    JSValue bfunc = js_dup(ctx->rt->current_stack_frame->cur_func->u.func.function_bytecode->cpool[get_u32(pc)]);
    *sp++ = js_closure(ctx, bfunc, var_refs, sf);
```

**Changes needed:**
1. Add `var_refs` parameter to frozen function signature
2. Add handler in `opcode_handlers.zig` for `fclosure` category
3. Update `codegen_ssa.zig` to emit closure creation code

**Files:**
- `src/freeze/opcode_handlers.zig` - Add closure handler pattern
- `src/freeze/codegen_ssa.zig` - Generate closure code
- `src/freeze/opcodes.zig` - Change category from `never_freeze` to `complex`

---

### Phase 2: Iterators (for-in, for-of)
**Impact: High** - Enables all iteration patterns

| Opcode | QuickJS Helper | Stack Effect |
|--------|---------------|--------------|
| `for_in_start` | `js_for_in_start()` | -1, +1 |
| `for_in_next` | `js_for_in_next()` | -1, +3 |
| `for_of_start` | `js_for_of_start()` | -1, +3 |
| `for_of_next` | `js_for_of_next()` | -3, +5 |
| `for_await_of_start` | `js_for_of_start()` + async | -1, +3 |

**Generated code pattern:**
```c
// for_in_start
sf->cur_pc = pc;  // Required for exception handling
if (js_for_in_start(ctx, &stack[sp - 1])) {
    goto exception;
}
// stack adjustment handled by js_for_in_start

// for_in_next
sf->cur_pc = pc;
if (js_for_in_next(ctx, &stack[sp - 1])) {
    goto exception;
}
sp += 2;  // Pushes key and done flag
```

**Changes needed:**
1. Verify QuickJS exports these functions (they do - used internally)
2. Add handler patterns for each iterator opcode
3. Handle exception paths with proper cleanup

**Files:**
- `src/freeze/opcode_handlers.zig` - Add iterator handler patterns
- `src/freeze/codegen_ssa.zig` - Generate iterator code
- `src/freeze/opcodes.zig` - Change 5 opcodes to `complex` category

---

### Phase 3: With Statement (Optional - Low Priority)
**Impact: Low** - `with` is deprecated, rarely used

| Opcode | Action |
|--------|--------|
| `with_get_var` | `JS_HasProperty` + `JS_GetProperty` |
| `with_put_var` | `JS_HasProperty` + `JS_SetProperty` |
| `with_delete_var` | `JS_HasProperty` + `JS_DeleteProperty` |
| `with_make_ref` | Create reference object |
| `with_get_ref` | Get reference value |
| `with_get_ref_undef` | Get reference or undefined |

**Generated code pattern:**
```c
// with_get_var atom:0x123 label:+10
{
    JSAtom atom = 0x123;
    JSValue obj = stack[sp - 1];
    int has = JS_HasProperty(ctx, obj, atom);
    if (has < 0) goto exception;
    if (has) {
        JSValue val = JS_GetProperty(ctx, obj, atom);
        if (JS_IsException(val)) goto exception;
        JS_FreeValue(ctx, stack[sp - 1]);
        stack[sp - 1] = val;
    } else {
        goto label_fallback;  // Fall through to normal scope lookup
    }
}
```

**Recommendation:** Defer to Phase 4 unless user demand exists.

---

### Phase 4: Never Support (Accept Limitation)

These opcodes **cannot** be frozen - they require runtime compilation:

| Opcode | Reason |
|--------|--------|
| `eval` | Compiles string to bytecode at runtime |
| `apply_eval` | Same as eval with apply semantics |
| `import` | Dynamic module loading and compilation |

**Strategy:**
- Keep as `never_freeze`
- Document clearly that functions using eval/import cannot be frozen
- This is a **design boundary**, not a bug

---

### Phase 5: Generators/Async (Not Recommended)

These opcodes require **coroutine/state machine** transformation:

| Opcode | Complexity |
|--------|------------|
| `yield` | High - suspension point |
| `yield_star` | High - delegate to sub-generator |
| `async_yield_star` | High - async delegation |
| `await` | High - promise suspension |
| `initial_yield` | Medium - generator setup |
| `return_async` | Medium - async completion |

**Why not worth it:**
1. Requires converting function to state machine
2. Need to save/restore all local variables at each yield point
3. Complex interaction with exception handling
4. Generators are rarely in hot paths (they're for iteration, which has overhead anyway)

**Recommendation:** Keep as `never_freeze`. Functions with generators fall back to interpreter.

---

## Implementation Details

### Handler Pattern (opcode_handlers.zig)

```zig
pub const HandlerPattern = enum {
    // Existing patterns
    push_const,
    binary_op,
    unary_op,
    jump,
    call,
    // New patterns
    closure,        // Create closure from bytecode
    for_in_start,   // Initialize for-in enumeration
    for_in_next,    // Advance for-in enumeration
    for_of_start,   // Initialize for-of iterator
    for_of_next,    // Advance for-of iterator
    with_get_var,   // Dynamic scope lookup
    with_put_var,   // Dynamic scope assignment
};
```

### Code Generation (codegen_ssa.zig)

```zig
fn emitOpcode(self: *SSACodeGen, instr: Instruction) !void {
    const info = instr.getInfo();
    switch (info.category) {
        // ... existing cases ...
        .closure => try self.emitClosure(instr),
        .iterator => try self.emitIterator(instr),
        .with_scope => try self.emitWithScope(instr),
    }
}

fn emitClosure(self: *SSACodeGen, instr: Instruction) !void {
    const idx = instr.operand;
    try self.print(
        \\{{
        \\    JSValue bfunc = JS_DupValue(ctx, cpool[{d}]);
        \\    PUSH(js_closure(ctx, bfunc, var_refs, sf));
        \\}}
    , .{idx});
}

fn emitIterator(self: *SSACodeGen, instr: Instruction) !void {
    switch (instr.opcode) {
        .for_in_start => try self.write(
            \\sf->cur_pc = pc;
            \\if (js_for_in_start(ctx, &stack[sp - 1])) goto exception;
        ),
        .for_in_next => try self.write(
            \\sf->cur_pc = pc;
            \\if (js_for_in_next(ctx, &stack[sp - 1])) goto exception;
            \\sp += 2;
        ),
        // ... other iterator opcodes
    }
}
```

### QuickJS Header Requirements

Verify these functions are exported in `quickjs.h`:
```c
// Already internal, need to expose or use wrapper
int js_for_in_start(JSContext *ctx, JSValue *sp);
int js_for_in_next(JSContext *ctx, JSValue *sp);
int js_for_of_start(JSContext *ctx, JSValue *sp, int is_async);
int js_for_of_next(JSContext *ctx, JSValue *sp, int drop);

// Already public
JSValue js_closure(JSContext *ctx, JSValue bfunc, JSVarRef **cur_var_refs, JSStackFrame *sf);
```

If not exported, add wrapper functions or modify QuickJS header.

---

## Testing Strategy

### Unit Tests
1. `test_freeze_closure.js` - Nested functions, callbacks
2. `test_freeze_for_in.js` - for-in loops over objects
3. `test_freeze_for_of.js` - for-of loops over arrays, iterables

### Benchmark Tests
Compare frozen vs interpreted performance for:
- Function with closures
- Function with for-in loop
- Function with for-of loop

### Edge Cases
- Exception during iteration
- Break/continue in loops
- Nested iterators
- Closure over loop variable

---

## Summary

| Phase | Opcodes | Difficulty | Priority | Status |
|-------|---------|------------|----------|--------|
| 1. Closures | 2 | Easy | HIGH | TODO |
| 2. Iterators | 5 | Medium | HIGH | TODO |
| 3. With Stmt | 6 | Medium | LOW | DEFER |
| 4. Eval/Import | 3 | Impossible | N/A | REJECT |
| 5. Generators | 6 | Hard | LOW | REJECT |

**Result after Phase 1+2:**
- 7 more opcodes supported
- ~95% of real-world functions can be frozen
- Maintainable via QuickJS C API calls
