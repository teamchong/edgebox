// Frozen checker functions — compiled to WASM via QuickJS → LLVM freeze pipeline.
//
// Usage: edgebox-compile src/tsc-recipe/frozen_checker.js
//   → frozen_checker-standalone.wasm (loaded by recipe at runtime)

// isSimpleTypeRelatedTo — pure flag-check version
// Takes source flags (i32) and target flags (i32), relation kind (i32)
// Returns: 1=related, 0=not-related, -1=unknown (fall through to full JS)
//
// Relation kinds: 0=assignable, 1=comparable, 2=subtype, 3=strictSubtype, 4=identity
function checkFlags(s, t, rel) {
  if ((t & 1) || (s & 131072)) return 1;
  if (t & 2) {
    if (rel === 3 && (s & 1)) return -1;
    return 1;
  }
  if (t & 131072) return 0;
  if ((s & 402653316) && (t & 4)) return 1;
  if ((s & 296) && (t & 8)) return 1;
  if ((s & 2112) && (t & 64)) return 1;
  if ((s & 528) && (t & 16)) return 1;
  if ((s & 12288) && (t & 4096)) return 1;
  if ((s & 32768) && (t & 49152)) return 1;
  if ((s & 65536) && (t & 65536)) return 1;
  if ((s & 524288) && (t & 67108864)) return 1;
  if (rel === 0 || rel === 1) {
    if (s & 1) return 1;
    if ((s & 8) && ((t & 32) || ((t & 256) && (t & 1024)))) return 1;
  }
  if ((s & 249860) && (t & 249860)) {
    if (((s & t) & 249860) === 0) return 0;
  }
  return -1;
}

function checkRelated(sFlags, tFlags, sId, tId, rel) {
  if (sId === tId) return 1;
  if ((sFlags & 524288) && (tFlags & 402784252)) {
    return checkFlags(sFlags, tFlags, rel);
  }
  if (rel === 4) {
    if (sFlags !== tFlags) return 0;
    if (sFlags & 67358815) return 1;
    return -1;
  }
  var r = checkFlags(sFlags, tFlags, rel);
  if (r !== -1) return r;
  return -1;
}

// Call both to prevent dead code elimination
var _r1 = checkFlags(1, 2, 0);
var _r2 = checkRelated(1, 2, 10, 20, 0);
