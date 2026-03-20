// v8_tsc_shim.js — Zero-cost Map alias
// The getRelationKey and getFlowCacheKey integer packing eliminates
// string allocation in hot cache paths. No Map wrapping needed.
(function() {
  'use strict';
  globalThis.__FastRelationCache = Map;
  // Global type-by-ID array for O(1) type lookup (SOA for type objects)
  globalThis.__typesById = [];
})();
