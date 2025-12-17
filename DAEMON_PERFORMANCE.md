# Daemon Performance Issue

## Problem

The EdgeBox daemon shows **300-500ms execution time** per request in benchmarks, even with pool hits. This is unacceptably slow compared to the <20ms target.

## Root Cause

Each daemon request executes the entire bundle including:
1. **6648 lines of polyfill code** (Node.js/browser compatibility layer)
2. **Frozen function registration** (4+ functions registered to global scope)
3. **Actual user code** (e.g., `console.log("Hello")`)

Even though WASM instances are pre-instantiated in the pool, each instance must:
- Re-initialize all polyfills (~280KB of runtime code)
- Register frozen functions to the global object
- Parse and execute the bytecode

**Total overhead: ~300ms per request**

## Solution (Not Yet Implemented)

Use **Wizer** to pre-initialize the WASM module:

```bash
# Current (slow):
wasm -> AOT -> embedded-daemon (300ms per request)

# Target (fast):
wasm -> wizer (pre-init) -> wizered.wasm -> AOT -> embedded-daemon (<10ms per request)
```

Wizer runs the `wizer_init()` function and snapshots the initialized state, so:
- Polyfills are already initialized
- Frozen functions are already registered
- Global scope is already set up

This would reduce daemon execution to just running the user code.

## Why Not Fixed Yet

1. **Build system complexity**: Need to integrate wizer into edgeboxc pipeline
2. **AOT compilation**: Wizer outputs WASM, need to AOT compile the wizered WASM
3. **Testing**: Need to verify wizered WASM works correctly with frozen functions

## Workaround

For now, the daemon is designed for **sustained load** not cold starts:
- First request: ~300ms (polyfill init)
- Subsequent requests: Should be faster (~50ms?) but still re-initializing

The benchmark accurately reflects this slow performance.

## Next Steps

1. Add wizer support to `src/wizer_wamr.zig`
2. Modify `src/edgeboxc.zig` to wizer WASM before AOT
3. Update build system to support `--wizer` flag
4. Re-run benchmarks with wizered daemon

**Target: <10ms daemon execution time**
