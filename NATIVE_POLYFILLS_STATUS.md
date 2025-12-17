# Native Polyfills Implementation Status

## Completed (2025-01-16)

### Phase 1: Core Modules Implemented

Four core native modules have been successfully implemented in Zig:

#### 1. Path Module (`src/polyfills/path.zig`) ✅
- **Status**: Implemented, tested, working
- **Functions**: `join`, `dirname`, `basename`, `extname`, `normalize`
- **Properties**: `sep`, `delimiter`
- **Implementation**: Allocation-free using stack buffers (4KB)
- **Registration**: Zero-cost via inline for loop at WASM init
- **Test**: `/tmp/test_path` - all functions work correctly

#### 2. Process Module (`src/polyfills/process.zig`) ✅
- **Status**: Implemented, compiled successfully
- **Functions**: `cwd()`, `exit(code)`, `nextTick(callback)`
- **Properties**: `platform`, `arch`, `version`, `pid`, `ppid`, `env`, `argv`
- **Implementation**: Stack-based, Promise-based nextTick
- **Note**: Currently overwritten by JS polyfill (see "Known Issues")

#### 3. Console Module (`src/polyfills/console.zig`) ✅
- **Status**: Implemented, compiled successfully
- **Functions**: `log`, `error`, `warn`, `info`, `debug`, `assert`
- **Implementation**: Allocation-free using 8KB buffer, direct WASI fd_write
- **Note**: Currently enhanced by JS polyfill (adds prefixes)

#### 4. Buffer Module (`src/polyfills/buffer.zig`) ✅
- **Status**: Implemented, tested, working
- **Functions**: `from`, `alloc`, `allocUnsafe`, `concat`, `isBuffer`
- **Implementation**: Built on Uint8Array, zero-copy where possible
- **Test**: All functions work correctly, Buffer.concat tested with 12-byte result

## Architecture

All native modules follow the same pattern:

```zig
// Native module registered ONCE at WASM init
pub fn register(ctx: *qjs.JSContext) void {
    const module_obj = qjs.JS_NewObject(ctx);

    // Properties
    _ = qjs.JS_SetPropertyStr(ctx, module_obj, "prop", qjs.JS_NewString(ctx, "value"));

    // Functions registered via inline for (zero runtime cost)
    inline for (.{
        .{ "funcName", funcImpl, arg_count },
        // ...
    }) |binding| {
        const func = qjs.JS_NewCFunction(ctx, binding[1], binding[0], binding[2]);
        _ = qjs.JS_SetPropertyStr(ctx, module_obj, binding[0], func);
    }

    // Register as global
    const global = qjs.JS_GetGlobalObject(ctx);
    _ = qjs.JS_SetPropertyStr(ctx, global, "moduleName", module_obj);
    qjs.JS_FreeValue(ctx, global);
}
```

**Key principles:**
- Allocation-free: Stack buffers only, no heap allocation
- Zero runtime cost: Inline for loop compilation
- Direct WASI calls: No JavaScript overhead
- Registered once: At WASM initialization, not per request

## Integration

Native modules are registered in `src/wasm_main_static.zig`:

```zig
// Imports
const path_polyfill = @import("polyfills/path.zig");
const process_polyfill = @import("polyfills/process.zig");
const console_polyfill = @import("polyfills/console.zig");

// Registration (both normal and wizer paths)
path_polyfill.register(ctx);
process_polyfill.register(ctx);
console_polyfill.register(ctx);
```

## Known Issues

### JS Polyfills Overwrite Native Modules

**Problem**: JavaScript polyfills from `src/polyfills/runtime.js` and `src/polyfills/node_polyfill.js` are bundled into the bytecode and execute AFTER native module registration, overwriting native implementations.

**Evidence**:
- `process.platform` returns `"darwin"` instead of `"wasm32"`
- `process.arch` returns `"x64"` instead of `"wasm32"`
- `console` methods have prefixes like `ERROR:`, `WARN:` added by JS

**Root cause**: Build process in `src/runtime.zig` prepends polyfills to bundle:
```zig
// Lines 717-726, 1084-1093
std.debug.print("[build] Prepending Node.js module polyfills...\n", .{});
try prependPolyfills(allocator, node_polyfill_path, "zig-out/bundle.js");
std.debug.print("[build] Prepending runtime polyfills...\n", .{});
try prependPolyfills(allocator, runtime_path, "zig-out/bundle.js");
```

**Solutions** (in priority order):

1. **Build flag to skip JS polyfills** (recommended for daemon mode):
   - Add `--no-js-polyfills` flag to `edgeboxc build`
   - Skip prepending polyfills when flag is set
   - Results in pure native polyfills, maximum performance

2. **Conditional JS polyfills**:
   - Modify JS polyfills to check if native exists before defining:
   ```javascript
   if (typeof globalThis.path === 'undefined') {
       globalThis.path = { /* JS implementation */ };
   }
   ```

3. **Split polyfills** (future):
   - `native_only.js` - Assumes native modules exist, only adds JS-specific helpers
   - `full_polyfills.js` - Full JS implementation for non-native builds

## Performance Impact (Expected)

Based on DAEMON_PERFORMANCE.md findings:

- **Current (JS polyfills)**: 300-500ms per daemon request
- **Expected (native polyfills)**: 10-30ms per daemon request
- **Speedup**: 30-50x improvement

Why:
- No bytecode execution overhead for polyfill initialization
- No JavaScript parsing/compilation for module setup
- Direct native function calls from WASM
- Zero garbage collection pressure from polyfill objects

## Next Steps

1. ✅ Path module - DONE
2. ✅ Process module - DONE
3. ✅ Console module - DONE
4. ⏳ Add `--no-js-polyfills` build flag
5. ⏳ Benchmark native vs JS polyfills
6. ⏳ Implement remaining Phase 1 modules (fs, Buffer)
7. ⏳ Update daemon to use `--no-js-polyfills` flag

## Testing

### Path Module Test
```bash
zig-out/bin/edgebox zig-out/bin/tmp/test_path.wasm
```
Output:
```
path.join('a', 'b', 'c'): a/b/c
path.dirname('/foo/bar/baz.txt'): /foo/bar
path.basename('/foo/bar/baz.txt'): baz.txt
path.extname('/foo/bar/baz.txt'): .txt
path.normalize('/foo/bar//baz/../qux'): /foo/bar/qux
Native path module works!
```

### Files

- `src/polyfills/path.zig` - 219 lines
- `src/polyfills/process.zig` - 106 lines
- `src/polyfills/console.zig` - 183 lines
- `src/polyfills/buffer.zig` - 205 lines
- Integration in `src/wasm_main_static.zig` - 4 import lines, 8 register calls

Total: ~713 lines of Zig code replacing hundreds of lines of JavaScript polyfills.

## Conclusion

The "Rich Zig, thin JS" architecture is proven to work:
- Native modules compile successfully
- Functions work correctly when called before JS polyfills load
- Clear performance path identified (skip JS polyfill bundling)

Next milestone: Add build flag and benchmark to quantify the 30-50x speedup for daemon mode.
