// Frozen TSC isSimpleTypeRelatedTo — compiled via QuickJS → LLVM → WASM
//
// This is the ACTUAL TSC function, refactored so closure dependencies
// become function parameters or WASM imports.
// The recipe provides the imports when instantiating the WASM module.

// Closure dependencies become function params/imports:
// - wildcardType → compared by ID, not object identity
// - relation objects → converted to i32 enum by caller
// - strictNullChecks → i32 param (1 or 0)
// - isEnumTypeRelatedTo → WASM import (calls back to JS)
// - getObjectFlags → WASM import
// - isEmptyAnonymousObjectType → WASM import
// - isUnknownLikeUnionType → WASM import
// - source.value === target.value → WASM import (valueEquals)

function makeIsSimpleTypeRelatedTo(
  isEnumTypeRelatedTo,
  getObjectFlags,
  isEmptyAnonymousObjectType,
  isUnknownLikeUnionType,
  valueEquals
) {
  // This inner function captures the imports as closure variables.
  // The freeze pipeline compiles it to WASM with the imports.
  function isSimpleTypeRelatedTo(s, t, rel, strictNullChecks, srcId, tgtId, wildcardId) {
    // target Any(1) or source Never(131072) or source === wildcardType
    if (t & 1 || s & 131072 || srcId === wildcardId) return 1;
    // target Unknown(2) — unless strictSubtype(3) && source Any
    if (t & 2 && !(rel === 3 && s & 1)) return 1;
    // target Never
    if (t & 131072) return 0;
    // StringLike → String
    if (s & 402653316 && t & 4) return 1;
    // StringLiteral+EnumLiteral → StringLiteral (need value check)
    if (s & 128 && s & 1024 && t & 128 && !(t & 1024)) {
      if (valueEquals(srcId, tgtId)) return 1;
    }
    // NumberLike → Number
    if (s & 296 && t & 8) return 1;
    // NumberLiteral+EnumLiteral → NumberLiteral (need value check)
    if (s & 256 && s & 1024 && t & 256 && !(t & 1024)) {
      if (valueEquals(srcId, tgtId)) return 1;
    }
    // BigIntLike → BigInt
    if (s & 2112 && t & 64) return 1;
    // BooleanLike → Boolean
    if (s & 528 && t & 16) return 1;
    // ESSymbolLike → ESSymbol
    if (s & 12288 && t & 4096) return 1;
    // Enum → Enum (need name + enum check via import)
    if (s & 32 && t & 32) {
      if (isEnumTypeRelatedTo(srcId, tgtId)) return 1;
    }
    // EnumLiteral → EnumLiteral
    if (s & 1024 && t & 1024) {
      if (s & 1048576 && t & 1048576) {
        if (isEnumTypeRelatedTo(srcId, tgtId)) return 1;
      }
      if (s & 2944 && t & 2944) {
        if (valueEquals(srcId, tgtId) && isEnumTypeRelatedTo(srcId, tgtId)) return 1;
      }
    }
    // Undefined — strictNullChecks aware
    if (s & 32768 && (strictNullChecks === 0 && !(t & 3145728) || t & 49152)) return 1;
    // Null — strictNullChecks aware
    if (s & 65536 && (strictNullChecks === 0 && !(t & 3145728) || t & 65536)) return 1;
    // Object → NonPrimitive (with strictSubtype exception)
    if (s & 524288 && t & 67108864) {
      if (!(rel === 3 && isEmptyAnonymousObjectType(srcId) && !(getObjectFlags(srcId) & 8192))) return 1;
    }
    // Assignable/comparable extras
    if (rel === 0 || rel === 1) {
      if (s & 1) return 1;
      if (s & 8 && (t & 32 || t & 256 && t & 1024)) return 1;
      if (s & 256 && !(s & 1024) && (t & 32 || t & 256 && t & 1024 && valueEquals(srcId, tgtId))) return 1;
      if (isUnknownLikeUnionType(tgtId)) return 1;
    }
    return -1;
  }
  return isSimpleTypeRelatedTo;
}

// Dummy imports for compilation
function dummyEnumRel(a, b) { return 0; }
function dummyGetOF(a) { return 0; }
function dummyIsEmpty(a) { return 0; }
function dummyIsUnknown(a) { return 0; }
function dummyValueEq(a, b) { return 0; }

var fn = makeIsSimpleTypeRelatedTo(dummyEnumRel, dummyGetOF, dummyIsEmpty, dummyIsUnknown, dummyValueEq);
var r = fn(1, 2, 0, 1, 10, 20, 999);
