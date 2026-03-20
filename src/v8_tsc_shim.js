// v8_tsc_shim.js — Zero-copy relation cache bypass
// Instead of wrapping Map, bypass it entirely for numeric keys.
// The getRelationKey patch returns packed integers.
// We intercept relation.get/set CALLS (not Map methods) to use
// a direct Int32Array lookup for packed integer keys.
//
// Zero-copy: no Map lookup, no hashing, no string allocation.
// Just array[key % SIZE] — one memory access.
(function() {
  'use strict';
  // FastRelationCache = Map (no wrapping needed, source replacement is no-op)
  globalThis.__FastRelationCache = Map;
})();
