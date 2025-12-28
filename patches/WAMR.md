# Vendor Dependencies

## WAMR (WebAssembly Micro Runtime)

**Pinned Version:** `2a2dd19f` (main branch, 2025-12-28)

### Patches Applied

We maintain patches in `patches/wamr/`:
- `001-cow-linear-memory.patch` - Core CoW infrastructure (callbacks, is_cow_memory flag)
- `002-cow-interpreter.patch` - CoW integration for interpreter mode

### How to Upgrade WAMR

1. **Fetch and checkout the new version:**
   ```bash
   cd vendor/wamr
   git fetch origin
   git checkout <new-commit-or-tag>
   ```

2. **Check if patches apply cleanly:**
   ```bash
   cd ../..  # Back to edgebox root
   git -C vendor/wamr apply --check patches/wamr/*.patch
   ```

3. **If patches need updating:**
   - Review the failing patches
   - Manually update them based on upstream changes
   - Test the updated patches

4. **Apply patches:**
   ```bash
   git -C vendor/wamr apply patches/wamr/*.patch
   ```

5. **Rebuild WAMR:**
   ```bash
   ./scripts/build-wamr.sh
   ```

6. **Rebuild CLI and test:**
   ```bash
   zig build cli -Doptimize=ReleaseFast
   # Run stability tests
   ./bench/run_hyperfine.sh
   ```

7. **Update this file with the new pinned version.**

### Build Configuration

WAMR is built with these settings:
- SIMD: ON (required for AOT modules compiled with simd128)
- Fast JIT: OFF (eliminates warmup overhead)
- Instruction Metering: ON (for CPU limits)
- AOT: ON (for maximum performance)

See `scripts/build-wamr.sh` for the exact CMake configuration.

### Known Issues

- AOT mode has ~4% intermittent crash rate (SIGSEGV at offset 0x178)
- Issue is in WAMR's AOT instantiation code, not in our patches
- Interpreter mode is 100% stable but slower

## QuickJS-NG

**Submodule:** `vendor/quickjs-ng`

QuickJS-NG is used as-is without patches. To upgrade:

```bash
cd vendor/quickjs-ng
git fetch origin
git checkout <new-version>
```
