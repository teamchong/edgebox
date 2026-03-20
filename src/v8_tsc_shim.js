// v8_tsc_shim.js — Zero-cost Map alias
// The getRelationKey and getFlowCacheKey integer packing eliminates
// string allocation in hot cache paths. No Map wrapping needed.
(function() {
  'use strict';
  globalThis.__FastRelationCache = Map;
  // SOA columns for Type objects — flat arrays indexed by type.id
  globalThis.__typesById = [];
  globalThis.__typeFlags = new Int32Array(262144); // 256K types, 1MB
})();
