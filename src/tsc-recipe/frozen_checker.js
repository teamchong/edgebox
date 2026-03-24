// Frozen TSC checker — manual freeze through QuickJS → LLVM → WASM
//
// These functions access object properties (.flags, .id) directly.
// The freeze pipeline needs to handle get_field on known object shapes.
// We provide type annotations via parameter naming convention:
//   source = {flags: i32, id: i32}
//   target = {flags: i32, id: i32}

// Pure flag version (no object access — guaranteed freezable)
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
  return checkFlags(sFlags, tFlags, rel);
}

// Property name match using flat i32 array
function checkPropertyNames(srcCount, tgtCount, srcOff, tgtOff, mem) {
  if (tgtCount === 0) return -1;
  if (srcCount === 0) return -1;
  var i = 0;
  while (i < tgtCount) {
    var tgtHash = mem[tgtOff + i];
    var found = 0;
    var j = 0;
    while (j < srcCount) {
      if (mem[srcOff + j] === tgtHash) { found = 1; j = srcCount; }
      j = j + 1;
    }
    if (found === 0) return 0;
    i = i + 1;
  }
  return 1;
}

// === FLAT versions (take individual i32 fields, not objects) ===
// These work correctly with the freeze pipeline (no struct offset issues)

// getObjectFlags — takes flags and objectFlags as separate i32 params
function getObjectFlagsFlat(flags, objectFlags) {
  return flags & 3899393 ? objectFlags : 0;
}

// isUnitType — single flag check
function isUnitTypeFlat(flags) {
  return flags & 109472 ? 1 : 0;
}

// isObjectLiteralType
function isObjectLiteralTypeFlat(flags, objectFlags) {
  return (flags & 3899393) && (objectFlags & 128) ? 1 : 0;
}

// isFreshLiteralType
function isFreshLiteralTypeFlat(flags, objectFlags) {
  return (flags & 3899393) && (objectFlags & 8192) ? 1 : 0;
}

// isEmptyAnonymousObjectType check (used in isRelatedTo)
function isEmptyObjFlat(flags, objectFlags) {
  return (flags & 524288) && (objectFlags & 16) && !(objectFlags & 8192) ? 1 : 0;
}

// Prevent dead code elimination
var _r1 = checkFlags(1, 2, 0);
var _r2 = checkRelated(1, 2, 10, 20, 0);
var _arr = [0, 0, 0, 0, 0, 0, 0, 0];
var _r3 = checkPropertyNames(2, 2, 0, 4, _arr);
var _r4 = getObjectFlagsFlat(524288, 128);
var _r5 = isUnitTypeFlat(109472);
var _r6 = isObjectLiteralTypeFlat(524288, 128);
var _r7 = isFreshLiteralTypeFlat(524288, 8192);
var _r8 = isEmptyObjFlat(524288, 16);
