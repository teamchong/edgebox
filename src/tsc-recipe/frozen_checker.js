// Freeze-compiled TSC isSimpleTypeRelatedTo — complete, no -1 returns.
// Every case returns 1 (true) or 0 (false). No fallback needed.
// Helpers handle enum/value comparison using integer hashes.

function enumRelated(srcSymH, tgtSymH) {
  // Same symbol name hash → same enum → related
  // Different or 0 → not related via simple check (structural will handle)
  if (srcSymH !== 0 && srcSymH === tgtSymH) return 1;
  return 0;
}

function valueEqual(srcVal, tgtVal) {
  return srcVal === tgtVal ? 1 : 0;
}

function checkFlags(sf, tf, rel, sn, srcSymH, tgtSymH, srcVal, tgtVal) {
  if (tf & 1 || sf & 131072) return 1;           // Any target | Never source
  if (tf & 2 && !(rel === 3 && sf & 1)) return 1; // Unknown target
  if (tf & 131072) return 0;                       // Never target
  if (sf & 402653316 && tf & 4) return 1;          // StringLike → String
  // StringLiteral+EnumLiteral → StringLiteral: value compare
  if (sf & 128 && sf & 1024 && tf & 128 && !(tf & 1024)) {
    return valueEqual(srcVal, tgtVal);
  }
  if (sf & 296 && tf & 8) return 1;               // NumberLike → Number
  // NumberLiteral+EnumLiteral → NumberLiteral: value compare
  if (sf & 256 && sf & 1024 && tf & 256 && !(tf & 1024)) {
    return valueEqual(srcVal, tgtVal);
  }
  if (sf & 2112 && tf & 64) return 1;             // BigIntLike → BigInt
  if (sf & 528 && tf & 16) return 1;              // BooleanLike → Boolean
  if (sf & 12288 && tf & 4096) return 1;           // ESSymbolLike → ESSymbol
  // Enum → Enum: compare symbol name hash
  if (sf & 32 && tf & 32) {
    return enumRelated(srcSymH, tgtSymH);
  }
  // EnumLiteral → EnumLiteral: check union+union or literal+literal
  if (sf & 1024 && tf & 1024) {
    if (sf & 1048576 && tf & 1048576) return enumRelated(srcSymH, tgtSymH);
    if (sf & 2944 && tf & 2944) {
      if (valueEqual(srcVal, tgtVal) === 1) return enumRelated(srcSymH, tgtSymH);
    }
    return 0;
  }
  if (sf & 32768 && (sn === 0 || tf & 49152)) return 1; // Undefined
  if (sf & 65536 && (sn === 0 || tf & 65536)) return 1; // Null
  // Object → NonPrimitive: true for non-strictSubtype, structural for strictSubtype
  if (sf & 524288 && tf & 67108864) {
    if (rel !== 3) return 1; // not strictSubtype → always true
    return 0; // strictSubtype needs isEmpty check → structural handles
  }
  if (rel === 0 || rel === 1) {
    if (sf & 1) return 1;                          // Any source
    if (sf & 8 && (tf & 32 || tf & 256 && tf & 1024)) return 1; // Number → Enum
    // NumberLiteral(no enum) → Enum: needs isEnumTypeRelatedTo → 0
    if (sf & 256 && !(sf & 1024) && (tf & 32 || tf & 256 && tf & 1024)) return 0;
    // isUnknownLikeUnionType: target is union of unknown|other → true
    // Check: target has Union(1048576) + Unknown(2) flags
    if (tf & 1048576 && tf & 2) return 1;
  }
  return 0;
}

function isRelatedToFast(srcId, tgtId, sf, tf, rel, sn, srcSymH, tgtSymH, srcVal, tgtVal) {
  if (srcId === tgtId) return 1;
  if (rel === 4) {
    if (sf !== tf) return 0;
    if (sf & 67358815) return 1;
    return 0; // identity for complex types → structural
  }
  var chk = checkFlags(sf, tf, rel, sn, srcSymH, tgtSymH, srcVal, tgtVal);
  if (chk === 1) return 1;
  // Quick reject: two primitives with no overlap
  if ((sf & 249860) && (tf & 249860) && !(sf & tf & 249860)) return 0;
  return 0; // structural comparison will decide
}

var _r = isRelatedToFast(1, 2, 4, 8, 0, 1, 0, 0, 0, 0);

function getObjectFlagsF(typeFlags, objectFlags) {
  return typeFlags & 3899393 ? objectFlags : 0;
}

function bloomReject(srcBloom, tgtBloom) {
  if (srcBloom === 0 || tgtBloom === 0) return 0;
  return (tgtBloom & (~srcBloom)) !== 0 ? 1 : 0;
}

var _t2 = getObjectFlagsF(524288, 16);
var _t3 = bloomReject(7, 15);
