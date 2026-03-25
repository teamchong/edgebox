// Freeze-compiled TSC isSimpleTypeRelatedTo — TypeScript 6.0.2 flag values.
// WASM IS the function. Uses name hash for enum comparison (approximate but fast).
// Handles all flag checks + enum hash + value comparison in pure integers.

function enumRelated(srcSymH, tgtSymH) {
  if (srcSymH !== 0 && srcSymH === tgtSymH) return 1;
  return 0;
}

function isRelatedToFast(srcId, tgtId, sf, tf, rel, sn, srcSymH, tgtSymH, valEq, srcWild, tgtUnkLike) {
  if (srcId === tgtId) return 1;
  // Any target | Never source | wildcardType source
  if (tf & 1 || sf & 262144 || srcWild === 1) return 1;
  // Unknown target (not strictSubtype+Any)
  if (tf & 2 && !(rel === 3 && sf & 1)) return 1;
  // Never target
  if (tf & 262144) return 0;
  // StringLike → String
  if (sf & 12583968 && tf & 32) return 1;
  // StringLiteral+EnumLiteral → StringLiteral(no enum): value compare
  if (sf & 1024 && sf & 32768 && tf & 1024 && !(tf & 32768)) return valEq;
  // NumberLike → Number
  if (sf & 67648 && tf & 64) return 1;
  // NumberLiteral+EnumLiteral → NumberLiteral(no enum): value compare
  if (sf & 2048 && sf & 32768 && tf & 2048 && !(tf & 32768)) return valEq;
  // BigIntLike → BigInt
  if (sf & 4224 && tf & 128) return 1;
  // BooleanLike → Boolean
  if (sf & 8448 && tf & 256) return 1;
  // ESSymbolLike → ESSymbol
  if (sf & 16896 && tf & 512) return 1;
  // Enum → Enum: name hash approximates escapedName + isEnumTypeRelatedTo
  if (sf & 65536 && tf & 65536) return enumRelated(srcSymH, tgtSymH);
  // EnumLiteral → EnumLiteral
  if (sf & 32768 && tf & 32768) {
    if (sf & 134217728 && tf & 134217728) return enumRelated(srcSymH, tgtSymH);
    if (sf & 15360 && tf & 15360 && valEq === 1) return enumRelated(srcSymH, tgtSymH);
    return 0;
  }
  // Undefined
  if (sf & 4 && (sn === 0 && !(tf & 402653184) || tf & 20)) return 1;
  // Null
  if (sf & 8 && (sn === 0 && !(tf & 402653184) || tf & 8)) return 1;
  // Object → NonPrimitive (non-strictSubtype always true, strictSubtype → 0 for structural)
  if (sf & 1048576 && tf & 131072) {
    if (rel !== 3) return 1;
    return 0;
  }
  // Assignable/Comparable extras
  if (rel === 0 || rel === 1) {
    if (sf & 1) return 1;
    if (sf & 64 && (tf & 65536 || tf & 2048 && tf & 32768)) return 1;
    if (sf & 2048 && !(sf & 32768)) {
      if (tf & 65536) return 1;
      if (tf & 2048 && tf & 32768 && valEq === 1) return 1;
    }
    if (tgtUnkLike === 1) return 1;
  }
  return 0;
}

var _r = isRelatedToFast(1, 2, 4, 8, 0, 1, 0, 0, 0, 0, 0);

function getObjectFlagsF(typeFlags, objectFlags) {
  return typeFlags & 3899393 ? objectFlags : 0;
}

function bloomReject(srcBloom, tgtBloom) {
  if (srcBloom === 0 || tgtBloom === 0) return 0;
  return (tgtBloom & (~srcBloom)) !== 0 ? 1 : 0;
}

var _t2 = getObjectFlagsF(524288, 16);
var _t3 = bloomReject(7, 15);
