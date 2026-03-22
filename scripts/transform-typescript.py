#!/usr/bin/env python3
"""Transform typescript.js — inject Zig zero-copy bridge into TSC.

Part of per-package recipe system. Applied at build time, not runtime.

Injections:
1. createType: register type flags in Zig on creation
2. isTypeRelatedTo: check Zig structural cache before V8 full check
"""
import sys

input_path = sys.argv[1] if len(sys.argv) > 1 else "node_modules/typescript/lib/typescript.js"
output_path = sys.argv[2] if len(sys.argv) > 2 else "src/workerd-tsc/typescript.js"

with open(input_path) as f:
    src = f.read()

count = 0

# 1. Inject type registration into createType (after result.id = typeCount)
old1 = 'result.id = typeCount;\n    (_a = tracing)'
new1 = 'result.id = typeCount;\n    if(typeof __edgebox_register_type==="function")__edgebox_register_type(typeCount,flags);\n    (_a = tracing)'
if old1 in src:
    src = src.replace(old1, new1, 1)
    count += 1
    print("[transform] 1/2: createType → __edgebox_register_type")
else:
    print("[transform] WARNING: createType pattern not found")

# 2. Inject structural check into isTypeRelatedTo (after source === target)
old2 = """if (source === target) {
      return true;
    }
    if (relation !== identityRelation)"""
new2 = """if (source === target) {
      return true;
    }
    if (typeof __edgebox_check_structural==="function"&&source.id&&target.id){var __r=__edgebox_check_structural(source.id,target.id);if(__r===1)return true;if(__r===0)return false;}
    if (relation !== identityRelation)"""
if old2 in src:
    src = src.replace(old2, new2, 1)
    count += 1
    print("[transform] 2/2: isTypeRelatedTo → __edgebox_check_structural")
else:
    print("[transform] WARNING: isTypeRelatedTo pattern not found")

with open(output_path, 'w') as f:
    f.write(src)
print(f"[transform] {count}/2 transforms applied — {len(src)} bytes → {output_path}")
