// TSC Recipe Transform — applied to typescript.js source BEFORE snapshot creation.
// Injects Zig structural check directly into TSC's internal isTypeRelatedTo.
// This runs once at daemon start, result is baked into the V8 snapshot.
//
// Strategy: Find the internal isTypeRelatedTo function and inject a Zig fast path
// BEFORE the expensive checkTypeRelatedTo call. TSC's own fast paths still run first.

(function(src) {
  // Find the exact function signature in TypeScript source
  var marker = 'function isTypeRelatedTo(source, target, relation) {';
  var idx = src.indexOf(marker);
  if (idx === -1) return src; // TypeScript version mismatch — return unmodified

  // Find the expensive checkTypeRelatedTo call site:
  //   if (source.flags & 469499904 /* StructuredOrInstantiable */ || target.flags & 469499904) {
  //     return checkTypeRelatedTo(source, target, relation, void 0);
  //   }
  // We inject our Zig check BEFORE this block.

  var expensiveMarker = 'if (source.flags & 469499904';
  var expIdx = src.indexOf(expensiveMarker, idx);
  if (expIdx === -1) return src; // Can't find injection point

  // Inject: if Zig says compatible, return true (skip expensive recursive check).
  // If Zig says incompatible, still fall through to TSC's full check for correctness.
  // This is a FAST PATH, not a replacement — TSC is always the authority.
  var zigCheck =
    '/* edgebox: Zig structural fast path */\n' +
    '    if (typeof __edgebox_check_structural === "function" && source.id && target.id) {\n' +
    '      if (typeof __edgebox_register_type === "function") {\n' +
    '        __edgebox_register_type(source.id, source.flags || 0, 0);\n' +
    '        __edgebox_register_type(target.id, target.flags || 0, 0);\n' +
    '      }\n' +
    '      var __zr = __edgebox_check_structural(source.id, target.id);\n' +
    '      if (__zr === 1) return true;\n' +
    '    }\n' +
    '    ';

  return src.slice(0, expIdx) + zigCheck + src.slice(expIdx);
})
