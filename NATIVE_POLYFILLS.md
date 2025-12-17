# Native Polyfills - Zero Runtime Cost

## Problem
Currently polyfills are implemented as **4799 lines of JavaScript** that get parsed and executed on every daemon request, taking ~300ms.

## Solution
Implement polyfills as **native Zig functions** exposed via QuickJS C API. This eliminates all runtime initialization cost.

## Benefits
- **Zero initialization overhead**: Functions registered once at WASM load time
- **Faster execution**: Native code instead of interpreted JS
- **Smaller bundles**: No polyfill code in user bundles
- **Better AOT**: Native functions compile to machine code

## Architecture

### Current (Slow)
```javascript
// 4799 lines in node_polyfill.js
_modules.path = {
    join: function(...parts) {
        return parts.filter(p => p).join('/').replace(/\/+/g, '/');
    },
    dirname: function(p) {
        const idx = p.lastIndexOf('/');
        return idx === -1 ? '.' : p.slice(0, idx);
    },
    // ... hundreds more functions
};
```

### New (Fast)
```zig
// src/polyfills/path.zig
fn pathJoin(ctx: *qjs.JSContext, this: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    // Native implementation
    var result = std.ArrayList(u8).init(allocator);
    for (0..argc) |i| {
        const str = qjs.JS_ToCString(ctx, argv[i]);
        try result.appendSlice(str);
        qjs.JS_FreeCString(ctx, str);
    }
    return qjs.JS_NewString(ctx, result.items.ptr);
}

pub fn register(ctx: *qjs.JSContext) void {
    const path_obj = qjs.JS_NewObject(ctx);
    qjs.JS_SetPropertyStr(ctx, path_obj, "join",
        qjs.JS_NewCFunction(ctx, pathJoin, "join", -1));
    qjs.JS_SetPropertyStr(ctx, path_obj, "dirname",
        qjs.JS_NewCFunction(ctx, pathDirname, "dirname", 1));

    const global = qjs.JS_GetGlobalObject(ctx);
    qjs.JS_SetPropertyStr(ctx, global, "path", path_obj);
    qjs.JS_FreeValue(ctx, global);
}
```

## Implementation Priority

### Phase 1: Core Modules (Highest Impact)
These are used in almost every request:

1. **path** (~200 lines → ~50 lines Zig)
   - `join`, `resolve`, `dirname`, `basename`, `extname`, `normalize`
   - Pure string manipulation, easy to implement

2. **process** (~100 lines → ~30 lines Zig)
   - `env`, `argv`, `cwd`, `exit`, `platform`
   - Already have most of this in wasm_main_static.zig

3. **console** (~50 lines → ~10 lines Zig)
   - `log`, `error`, `warn`, `info`, `debug`
   - Just wrap print function

### Phase 2: I/O Modules (Medium Impact)
Used by file operations:

4. **fs** (~1500 lines → ~300 lines Zig)
   - `readFile`, `writeFile`, `readdir`, `stat`, `mkdir`
   - Already have async file operations in wasm_fetch.zig

5. **Buffer** (~800 lines → ~150 lines Zig)
   - `from`, `toString`, `slice`, `copy`
   - QuickJS has typed arrays, just need wrappers

### Phase 3: Network Modules (Low Priority)
Used by HTTP clients:

6. **http/https** (~500 lines → ~100 lines Zig)
   - Already have fetch, just need Node.js API wrapper

7. **net** (~400 lines → ~80 lines Zig)
   - Socket operations

8. **url** (~300 lines → ~60 lines Zig)
   - URL parsing (or use Zig standard library)

### Phase 4: Utility Modules (Low Priority)
Less frequently used:

9. **events** (~400 lines → ~80 lines Zig)
   - EventEmitter class

10. **stream** (~600 lines → ~120 lines Zig)
    - Readable/Writable streams

11. **crypto** (~200 lines → ~40 lines Zig)
    - Already have native crypto bindings

## Migration Strategy

### Step 1: Create Native Polyfill System
```zig
// src/polyfills/native.zig
const path = @import("polyfills/path.zig");
const process = @import("polyfills/process.zig");
const console = @import("polyfills/console.zig");

pub fn registerAll(ctx: *qjs.JSContext) void {
    path.register(ctx);
    process.register(ctx);
    console.register(ctx);
    // ... more modules
}
```

### Step 2: Hybrid Mode
Keep JS polyfills for complex modules, use native for simple ones:
```javascript
// Minimal JS polyfill - only for complex logic
if (!globalThis.path) {
    // Native path module not available, use JS fallback
    globalThis.path = { /* JS implementation */ };
}
```

### Step 3: Bundle Mode Flag
```bash
# For daemon: no polyfills (uses native)
edgeboxc build app/ --no-polyfills

# For standalone: bundle polyfills (for portability)
edgeboxc build app/ --bundle-polyfills
```

### Step 4: Wizer Integration
```
Build edgebox-base.wasm (with native polyfills)
  → wizer_init() registers native functions
  → Snapshot includes registered native bindings
  → User code runs with zero polyfill overhead
```

## Expected Performance

### Current
- **Polyfill init**: ~300ms (parse + execute 4799 lines)
- **Per request**: 300-360ms

### With Native Polyfills
- **Polyfill init**: ~1ms (register native functions)
- **Per request**: <10ms (just user code)

**Expected speedup: 30-50x**

## Implementation Example

Let's start with `path.join` as a proof of concept:

```zig
// src/polyfills/path.zig
const std = @import("std");
const qjs = @cImport(@cInclude("quickjs.h"));

var gpa = std.heap.GeneralPurposeAllocator(.{}){};
const allocator = gpa.allocator();

fn pathJoin(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    var parts = std.ArrayList([]const u8).init(allocator);
    defer parts.deinit();

    // Collect all string arguments
    for (0..@intCast(argc)) |i| {
        const str = qjs.JS_ToCString(ctx, argv[i]);
        if (str == null) continue;
        defer qjs.JS_FreeCString(ctx, str);

        const part = std.mem.span(str);
        if (part.len > 0) {
            parts.append(allocator.dupe(u8, part) catch continue) catch continue;
        }
    }

    // Join with '/' and normalize
    var result = std.ArrayList(u8).init(allocator);
    defer result.deinit();

    for (parts.items, 0..) |part, i| {
        if (i > 0) result.append('/') catch {};
        result.appendSlice(part) catch {};
    }

    // Create JS string
    return qjs.JS_NewStringLen(ctx, result.items.ptr, @intCast(result.items.len));
}

pub fn register(ctx: ?*qjs.JSContext) void {
    const path_obj = qjs.JS_NewObject(ctx);

    qjs.JS_SetPropertyStr(ctx, path_obj, "join",
        qjs.JS_NewCFunction(ctx, pathJoin, "join", -1));

    const global = qjs.JS_GetGlobalObject(ctx);
    qjs.JS_SetPropertyStr(ctx, global, "path", path_obj);
    qjs.JS_FreeValue(ctx, global);
}
```

## Next Steps

1. Implement `path` module natively (proof of concept)
2. Benchmark: Compare native vs JS polyfill performance
3. Add `--no-polyfills` flag to build system
4. Implement remaining core modules (process, console, fs)
5. Test daemon with native polyfills
6. Measure <10ms target

## Notes

- QuickJS already has some built-in modules (std, os) that we can leverage
- We already have async file/fetch operations in Zig
- Most polyfills are just thin wrappers around existing Zig code
- This aligns with Bun's approach: implement Node.js APIs natively for performance
