#!/usr/bin/env python3
"""Transform typescript.js — inject Zig structural check into isTypeRelatedTo."""
import sys, re

input_path = sys.argv[1] if len(sys.argv) > 1 else "node_modules/typescript/lib/typescript.js"
output_path = sys.argv[2] if len(sys.argv) > 2 else "src/workerd-tsc/typescript.js"

with open(input_path) as f:
    src = f.read()

# Inject after "if (source === target) { return true; }"
# but before the relation check
old = """if (source === target) {
      return true;
    }
    if (relation !== identityRelation)"""

new = """if (source === target) {
      return true;
    }
    if (typeof __edgebox_check_structural==="function"&&source.id&&target.id){var __r=__edgebox_check_structural(source.id,target.id);if(__r===1)return true;if(__r===0)return false;}
    if (relation !== identityRelation)"""

if old in src:
    src = src.replace(old, new, 1)  # Replace first occurrence only
    with open(output_path, 'w') as f:
        f.write(src)
    print(f"[transform] Injected structural check — {len(src)} bytes → {output_path}")
else:
    print("[transform] Pattern not found — copying original")
    with open(output_path, 'w') as f:
        f.write(src)

