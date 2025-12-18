# Runtime Hooks Migration Plan

## Problem
QuickJS patches are fragile and cause maintenance issues:
- Duplicate code across patches
- Merge conflicts on QuickJS updates
- Build failures from patch application
- Hard to debug what's patched vs original

## Reality Check: Can We Avoid Patches Entirely?

**Answer: No** - Here's why:

QuickJS internals like `JSVarRef **` and `JSStackFrame *` are required for:
- `js_closure()` - needs access to closure variable refs
- Variable reference helpers - need `JSVarRef` struct layout
- Stack frame manipulation - need `JSStackFrame` internals

These types are **not in the public API** and their internal layout can change between QuickJS versions.

## Solution: Minimal Consolidated Patch

Since we must patch, let's do it **cleanly**:

1. **ONE patch file** (`quickjs-frozen-helpers.patch`)
   - Adds all frozen helpers to `quickjs.c`
   - Declares them in `quickjs.h`
   - Clear `/* EdgeBox: ... */` comments marking our code

2. **Fix duplication issue** - patch 001 and 002 have overlapping changes
   - Audit both patches
   - Merge into single patch
   - Remove duplicates

3. **Upstream contribution** - Submit PR to quickjs-ng to add extension points
   - Expose `JSVarRef` as opaque type
   - Add `JS_CreateClosure()` public API
   - Make frozen helpers unnecessary long-term

## Architecture

### Phase 1: Extract Patched Functions
Currently patched into quickjs.c (causing duplicate definition errors):
1. `js_closure` - Create closures with captured variables
2. `js_frozen_for_in_start/next` - For-in loop helpers
3. `js_frozen_for_of_start/next` - For-of loop helpers
4. `js_frozen_get_var_ref/set_var_ref` - Closure variable access
5. `JS_SetFrozenHomeObject/JS_GetFrozenHomeObject` - Super/home object tracking
6. `frozen_home_object_registry[]` - Global registry
7. `JS_GetImportMetaCurrent` - import.meta support
8. `JS_NewMappedArgumentsSimple` - Arguments object
9. `JS_NewPrivateSymbol` - Private field symbols
10. `JS_FrozenCheckBrand/JS_FrozenAddBrand` - Private field brands
11. `js_frozen_private_in` - Private field existence check
12. `JS_FrozenGetPrivateField/JS_FrozenSetPrivateField` - Private field access

### Phase 2: Create Helper Module
**File**: `src/freeze/edgebox_quickjs_helpers.c`

Contains all the above functions using **only QuickJS public API**.
Key constraint: No access to QuickJS internals.

### Phase 3: Runtime Registration
**File**: `src/runtime.zig`

```zig
pub fn initFrozenSupport(ctx: *JSContext) void {
    const global = c.JS_GetGlobalObject(ctx);
    defer c.JS_FreeValue(ctx, global);

    // Register helpers as global functions
    _ = c.JS_SetPropertyStr(ctx, global, "js_closure",
        c.JS_NewCFunction(ctx, js_closure, "js_closure", 3));

    // etc...
}
```

### Phase 4: Remove Patches
Delete patch files and patch application logic from build.

### Phase 5: Update Frozen Codegen
Update `src/freeze/codegen_ssa.zig` to call helpers via:
- Direct C calls (if linked)
- Or JS function calls (runtime registration)

## Benefits
✅ No patches - survives QuickJS updates seamlessly
✅ Clean separation - edgebox code separate from vendor code
✅ Easier debugging - clear boundary between QuickJS and our code
✅ Modular - frozen support is opt-in
✅ Testable - can unit test helpers independently

## Migration Steps
1. ✅ Design architecture (this document)
2. ⏳ Create `edgebox_quickjs_helpers.c` with all patched functions
3. ⏳ Add runtime registration in `src/runtime.zig`
4. ⏳ Remove patch files
5. ⏳ Test frozen functions work
6. ⏳ Commit and document

## Notes
- Some functions may need QuickJS internals (like `js_closure` needs `JSVarRef`)
- If internals needed, we can:
  - Option A: Use opaque handles and wrapper functions
  - Option B: Keep minimal inline helpers in frozen_runtime.h
  - Option C: Submit upstream PR to expose needed APIs
