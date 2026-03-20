// v8_tsc_shim.js — Zero-cost Map alias + SAB-backed SOA columns
// The getRelationKey and getFlowCacheKey integer packing eliminates
// string allocation in hot cache paths. No Map wrapping needed.
(function() {
  'use strict';
  globalThis.__FastRelationCache = Map;
  // SOA columns for Type objects — flat arrays indexed by type.id
  globalThis.__typesById = [];
  // __pc_typeFlags is now SAB-backed (set by v8_parallel_check.zig registerGlobals)
  // Fallback if parallel check not available:
  if (typeof globalThis.__pc_typeFlags === 'undefined') {
    globalThis.__pc_typeFlags = new Int32Array(262144);
  }
  if (typeof globalThis.__pc_objectFlags === 'undefined') {
    globalThis.__pc_objectFlags = new Int32Array(262144);
  }

  // 2-way associative relation cache — zero-copy flat array
  // Layout: [key0, keyHi0, result0, key1, keyHi1, result1] per bucket
  // 128K buckets × 6 entries × 4 bytes = 3MB (Int32Array)
  // Key match check prevents hash collision false positives.
  if (typeof globalThis.__pc_relKeys === 'undefined') {
    globalThis.__pc_relKeys = new Int32Array(131072 * 6); // 128K buckets × 6
  }

  // Source file cache for createSourceFile memoization
  // Populated by source transform that wraps createSourceFile
  globalThis.__sfCache = Object.create(null);
})();
