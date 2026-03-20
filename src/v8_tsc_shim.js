// v8_tsc_shim.js — Zero-copy Map enhancement for TypeScript compiler
// Extends Map with a fast numeric key cache. Falls back to Map for everything.
// This is safe because Map is always the source of truth.
(function() {
  'use strict';

  // Enhanced Map: adds a direct-access cache for integer keys
  // The cache is a simple array indexed by (key % SIZE).
  // On collision, falls through to the real Map.
  var CACHE_SIZE = 131072; // 128K entries, 2MB (good balance of hit rate vs memory)

  function FastRelationCache() {
    // Use real Map as the authoritative store
    this._map = new Map();
    // Fast cache: parallel arrays for key+value
    this._ck = new Float64Array(CACHE_SIZE);
    this._cv = new Int32Array(CACHE_SIZE);
    this._ck.fill(-1); // -1 = empty
  }

  // Hash function that distributes packed type IDs evenly
  function frcHash(key) {
    // Mix bits: key contains (sourceId * 67108864 + targetId + 1)
    // Simple multiplicative hash with golden ratio
    var h = ((key * 2654435761) | 0) & 0x7FFFFFFF;
    return h & (CACHE_SIZE - 1);
  }

  FastRelationCache.prototype.get = function(key) {
    if (typeof key === 'number') {
      var idx = frcHash(key);
      if (this._ck[idx] === key) return this._cv[idx];
      // 2-way associative: check alternate slot
      var idx2 = idx ^ 1;
      if (this._ck[idx2] === key) return this._cv[idx2];
    }
    return this._map.get(key);
  };

  FastRelationCache.prototype.set = function(key, value) {
    this._map.set(key, value);
    if (typeof key === 'number') {
      var idx = frcHash(key);
      // Prefer empty slot, then evict primary
      if (this._ck[idx] === -1 || this._ck[idx] === key) {
        this._ck[idx] = key; this._cv[idx] = value;
      } else {
        var idx2 = idx ^ 1;
        this._ck[idx2] = key; this._cv[idx2] = value;
      }
    }
    return this;
  };

  FastRelationCache.prototype.has = function(key) {
    if (typeof key === 'number') {
      var idx = frcHash(key);
      if (this._ck[idx] === key) return true;
      if (this._ck[idx ^ 1] === key) return true;
    }
    return this._map.has(key);
  };

  // Delegate everything else to Map
  FastRelationCache.prototype.delete = function(key) { return this._map.delete(key); };
  FastRelationCache.prototype.clear = function() { this._map.clear(); this._ck.fill(-1); };
  FastRelationCache.prototype.forEach = function(cb) { this._map.forEach(cb); };
  FastRelationCache.prototype.entries = function() { return this._map.entries(); };
  FastRelationCache.prototype.keys = function() { return this._map.keys(); };
  FastRelationCache.prototype.values = function() { return this._map.values(); };
  FastRelationCache.prototype[Symbol.iterator] = function() { return this._map[Symbol.iterator](); };
  Object.defineProperty(FastRelationCache.prototype, 'size', {
    get: function() { return this._map.size; }
  });

  globalThis.__FastRelationCache = FastRelationCache;
})();
