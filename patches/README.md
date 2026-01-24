# EdgeBox Patches

Patches for vendor dependencies required by EdgeBox's freeze system.

## Directory Structure

```
patches/
├── quickjs/           # QuickJS-NG patches
│   └── 001-frozen-interpreter-all.patch
├── wamr/              # WAMR patches
│   └── *.patch
└── README.md
```

## How Patches Are Applied

Patches are applied automatically during `zig build`:

1. Build checks for `.patches-applied` marker in vendor submodule
2. If marker doesn't exist:
   - Resets submodule to clean state (`git checkout .`)
   - Applies all patches from the corresponding patches folder
   - Creates `.patches-applied` marker
3. If marker exists, skips patching (already applied)

## QuickJS Patches

### 001-frozen-interpreter-all.patch

Combined patch that exports internal QuickJS functions needed by the freeze system.

**Purpose:** Enable frozen C code generation by exposing:

- **Iterator helpers** - `js_frozen_for_in_start/next`, `js_frozen_for_of_start/next`
- **Special objects** - `JS_NewArguments`, `JS_GetImportMetaCurrent`
- **Private fields** - `JS_FrozenGetPrivateField`, `JS_FrozenSetPrivateField`, etc.
- **Closure support** - `js_closure`, `JS_GetFunctionConstantPool`
- **Dynamic import** - `js_dynamic_import`, `js_dynamic_import_job`

**Files modified:** `quickjs.c`, `quickjs.h`

## WAMR Patches

Located in `patches/wamr/`. These patches:
- Enable CoW memory for better performance
- Add interpreter hooks
- Fix macOS ARM64 x18 register issue (required for AOT)

## Updating Vendor Submodules

When updating QuickJS-NG or WAMR:

```bash
# 1. Update submodule
cd vendor/quickjs-ng
git fetch origin
git checkout <new-version>

# 2. Remove marker to force re-patch
rm .patches-applied

# 3. Test patches still apply
cd ../..
zig build

# 4. If patches fail, update them manually
```

## Why We Patch

EdgeBox generates native C code from JavaScript for 18x speedup. This requires access to QuickJS internals not exposed in the public API. These patches are minimal and well-documented.
