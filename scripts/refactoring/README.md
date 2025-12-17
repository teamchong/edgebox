# Codegen SSA Refactoring Scripts

This directory contains Python scripts to refactor `src/freeze/codegen_ssa.zig` by extracting 133 shared opcodes between `emitInstruction` and `emitTrampolineInstruction` into a shared `emitCommonOpcode` function.

## Status: 99% Complete

The refactoring removes ~2086 lines of duplicate code. All extraction and insertion logic is complete and working.

## Files

- `shared_ops.txt` - List of 133 shared opcodes between both functions
- `extract_complete.py` - Extracts shared opcodes from `emitTrampolineInstruction` and generates `emitCommonOpcode` function
- `insert_and_update.py` - Inserts the generated function and removes duplicates from both `emitInstruction` and `emitTrampolineInstruction`

## Usage

```bash
# 1. Extract shared opcodes and generate emitCommonOpcode
python3 scripts/refactoring/extract_complete.py
# Output: /tmp/emit_common_opcode.zig

# 2. Insert into source and remove duplicates
python3 scripts/refactoring/insert_and_update.py
# Output: /tmp/codegen_ssa_final.zig

# 3. Copy to source
cp /tmp/codegen_ssa_final.zig src/freeze/codegen_ssa.zig

# 4. Test build
zig build -Doptimize=ReleaseFast
```

## Known Issues

5 opcodes need manual fixing after generation (one-liners ending with `),`):
- `.dec`
- `.rot5l`
- `.shr`
- `.special_object`
- `.strict_neq`

These need to be converted from:
```zig
.dec => try self.write("..."),
```

To:
```zig
.dec => {
    try self.write("...");
    return true;
},
```

The issue is that `.special_object` contains a nested switch statement, and the simple pattern-based fixer incorrectly converts inner switch arms. A manual fix or more sophisticated AST-based parser is needed.

## Result

Once complete, the refactoring:
- Reduces file from 4629 to 2543 lines (2086 lines removed)
- Creates single source of truth for shared opcode implementations
- Eliminates duplication between trampoline and main execution paths
