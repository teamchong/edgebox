# EdgeBox

## Overview

EdgeBox is a V8 pool runtime built in Zig. It uses WasmGC + V8 TurboFan inlining for data access and LLVM AOT for compute kernels. Recipe system accelerates any npm package.

## Key Features

- **WasmGC TurboFan Inlining**: Data access (struct.get/array.get) inlined at JS callsite — V8 compiles to single MOV instruction. Uses `--turbo-inline-js-wasm-calls`.
- **LLVM AOT Kernels**: Numeric JS compiled to WASM via LLVM — runs at native speed (WASM-tier TurboFan). NOT inlined into JS (standard WASM instructions). Call overhead ~50ns.
- **Auto SOA**: Detects object shapes, converts AoS to SoA via WasmGC columns. TurboFan inlines column access.
- **Recipe System**: Package-specific optimizations (TSC, webpack, etc.) via public API wrapping + WasmGC data model + source injection.
- **V8 Pool Daemon**: N V8 isolates on OS threads, Unix socket IPC, V8 snapshot, condvar dispatch.
- **Sandboxed Execution**: Secure isolation for untrusted code

## Build Requirements

- Zig 0.15+
- LLVM 18 (only system dependency)
- ARM64 Mac supported

## Build Commands

```bash
# Build
zig build

# Run tests
zig build test

# Build CLI tools
zig build cli
```

## Build Rules

- The TSC benchmark is at `benchmark/run.ts`, NOT in `bench/typescript/`
- **NEVER compile tsc_debug.js** - use `node_modules/typescript/lib/_tsc.js` instead
- **NEVER change compile flags** specified by the user - use exactly what they provide
- **ALWAYS provide a file argument when testing TSC** - running `tsc` without arguments hangs waiting for input
- CLI default is worker-only (`--wasm-only`). Use `--with-binary` to opt into native binary path.

## Vendor Submodules and Patches

### CRITICAL: Submodule Behavior

**⚠️ NEVER edit `vendor/` files directly - ALL changes are DESTROYED on every build!**

The build system enforces a strict workflow (see `build.zig` lines 48-66):
1. **Init**: If `vendor/quickjs-ng/quickjs.c` doesn't exist, runs `git submodule update --init --recursive`
2. **Reset**: Runs `git checkout .` in each vendor submodule to discard ALL local changes
4. **Build**: Compiles with the patched sources

**Any agent attempting to commit directly to vendor/ will have their changes discarded and destroyed.**

### Pinned Commits

All vendor submodules use **pinned commit IDs** (not origin/main):
- `vendor/quickjs-ng`: `fa9472db3607d9682755ab0e73690297fff8a811`
- `vendor/binaryen`: `b7dc66fb7036e689415a12caa871d48e2356322c`
- `vendor/metal0`: `83e85ddbe0a1d1f773bb7db8232a3b98b7c8a745`

The build system **NEVER fetches from origin or checks out main** - always uses the pinned commit.

### How to Make Vendor Changes

Use the helper scripts - do NOT manually run git commands in vendor/:

```bash
# Step 1: Reset vendor to pinned commit and apply existing patches
./scripts/apply-quickjs-patches.sh

# Step 2: Edit the C files directly in vendor/quickjs-ng/
# ... make your changes ...

# Step 3: Generate new patch (does NOT reset - you test first!)
./scripts/regenerate-quickjs-patch.sh

# Step 4: Build and test your changes
zig build cli
# ... test thoroughly ...

# Step 5: ONLY after user confirms it works, commit the patch
git add patches/quickjs/001-frozen-interpreter-all.patch
git commit -m "fix(quickjs): description of changes"

# Step 6: Reset vendor (next build will reapply patches automatically)
./scripts/apply-quickjs-patches.sh
```

**Scripts:**
- `scripts/apply-quickjs-patches.sh` - Reset to pinned commit + apply patches
- `scripts/regenerate-quickjs-patch.sh` - Generate patch from current changes (no reset)
- `scripts/pre-commit-vendor-check.sh` - Pre-commit hook to prevent accidental vendor commits

### Patch Locations

- `patches/quickjs/` - QuickJS-ng patches (frozen interpreter helpers, JS runtime modifications)

### Important Rules for Agents

**⚠️ CRITICAL: NEVER run `zig build` if you have uncommitted vendor edits! Run `./scripts/regenerate-quickjs-patch.sh` FIRST or all your changes will be DESTROYED!**

1. **NEVER** run `git add` or `git commit` in `vendor/` directories
2. **NEVER** assume vendor changes persist - they are reset on EVERY build
3. **NEVER** run `zig build` before saving vendor edits to patch file
4. **ALWAYS** run `./scripts/regenerate-quickjs-patch.sh` after editing vendor files (BEFORE building)
5. **ALWAYS** make changes via the patch files in `patches/`
6. **ALWAYS** test patches with `zig build` before committing
7. **ALWAYS** get user approval before committing patch changes

## Architecture

```
┌─────────────────────────────────────────────────────────────┐
│  Recipe System (per-package optimizations)                  │
│                                                             │
│  ┌─────────────────┐  ┌──────────────────────────────────┐  │
│  │ WasmGC Data      │  │ Source Injection                 │  │
│  │ (TurboFan inline)│  │ (patch TSC/webpack internals)    │  │
│  │                  │  │                                  │  │
│  │ type_flags_gc    │  │ createType → WasmGC setFlag      │  │
│  │ soa_gc (structs) │  │ isTypeRelatedTo → flag check     │  │
│  │ soa_gc (columns) │  │ createSourceFile → .d.ts cache   │  │
│  └────────┬─────────┘  └──────────────────────────────────┘  │
│           │                                                  │
│           ▼                                                  │
│  V8 TurboFan inlines struct.get/array.get at JS callsite    │
└─────────────────────────────────────────────────────────────┘

┌─────────────────────────────────────────────────────────────┐
│  Freeze (AOT Compute Kernels)                               │
│                                                             │
│  QuickJS Parser → Bytecode → LLVM IR → WASM                │
│  Runs at native speed (WASM-tier TurboFan)                  │
│  NOT inlined into JS — separate WASM execution context      │
│  Best for: batch processing, numeric kernels, fib, sort     │
└─────────────────────────────────────────────────────────────┘

┌─────────────────────────────────────────────────────────────┐
│  V8 Pool Daemon                                             │
│                                                             │
│  Zig binary → N V8 isolates → V8 snapshot (TypeScript)      │
│  Unix socket IPC → condvar dispatch → work-stealing         │
│  Adaptive: cold=parallel workers, warm=single worker        │
└─────────────────────────────────────────────────────────────┘
```

## Use Cases

- Serverless edge functions (like Cloudflare Workers)
- User plugin systems (run untrusted extensions)
- Multi-tenant platforms (isolate customers)
- Secure code execution environments

## Benchmarks

### TSC Type Checking (playwright, 2058 diagnostics)

| | Cold | vs Node.js |
|---|------|-----------|
| Node.js TSC | 3.2s | 1.0x |
| **EdgeBox** | **2.6s** | **1.23x faster** |
| tsgo (native Zig) | 0.75s | 4.3x faster |

EdgeBox advantage: WasmGC TurboFan inlining + monomorphic Node constructor + parallel workers + V8 snapshot + Zig IO cache.

### AOT Compute Kernels (freeze)

| Test | EdgeBox | Node.js | Speedup |
|------|---------|---------|---------|
| fib(45) | 2307ms | 4543ms | ~2x |
| struct checkBoth | 2ms | 7ms | 3.5x |
| struct_pool (overall) | 50ms | 239ms | 4.90x |
| Overall (40 benchmarks) | — | — | 2.56x |

Speedup from LLVM optimization + WASM-tier TurboFan (NOT JS-level inlining).

## Notes

- WasmGC is the ONLY WASM path — no linear memory WASM, no fallbacks
- V8 TurboFan inlines WasmGC struct.get/array.get ONLY (not i32 arithmetic)
- Freeze WASM runs at native speed but is NOT inlined into JS
- `--turbo-inline-js-wasm-calls` V8 flag required for WasmGC inlining
- Recipe system uses ts.setObjectAllocator (public API) for monomorphic constructors
- This is NOT for internal trusted code (use native Zig for that)
- Designed for running untrusted/user-submitted code safely


## Auto-Generated Index

> Last updated: 2026-02-08 10:45
> Run `continue` to refresh this index

### Helpers

### Patterns

### Modules
