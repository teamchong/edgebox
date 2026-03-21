// v8_tsc_shim.js — Runtime shim for TSC transforms
// SAB-backed __pc_typeFlags and __pc_objectFlags are set by v8_runner.zig (from Zig mmap).
// If SAB is unavailable (non-TSC path), fall back to plain Int32Array.
(function() {
  'use strict';
  globalThis.__FastRelationCache = Map;
  globalThis.__typesById = [];
  // Fallback allocations only if SAB wasn't set by v8_runner
  if (typeof globalThis.__pc_typeFlags === 'undefined')
    globalThis.__pc_typeFlags = new Int32Array(262144);
  if (typeof globalThis.__pc_objectFlags === 'undefined')
    globalThis.__pc_objectFlags = new Int32Array(131072);
  // Source file cache for createSourceFile memoization (T9-T10)
  globalThis.__sfCache = Object.create(null);
})();
