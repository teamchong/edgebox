// v8_tsc_shim.js — Zero-copy optimizations for TypeScript compiler
//
// Replaces TSC's Map-based caches with flat Int32Array hash tables.
// Eliminates: string key allocation, Map hashing, GC pressure.
//
// Injected before TSC loads. Monkey-patches Map to intercept
// relation cache operations.

(function() {
  'use strict';

  // Fast integer hash map — replaces Map<string, number> for relation caches
  // Key: two type IDs (source.id, target.id)
  // Value: relation result (small integer: 1=Succeeded, 2=Failed, etc.)
  var CACHE_BITS = 20; // 1M slots = 8MB
  var CACHE_SIZE = 1 << CACHE_BITS;
  var CACHE_MASK = CACHE_SIZE - 1;

  function FastRelationCache() {
    // Parallel arrays: keys store packed (sourceId << 20 | targetId), values store result
    // Using two Int32Arrays for key+value instead of interleaved for better cache locality
    this._keys = new Int32Array(CACHE_SIZE);
    this._vals = new Int32Array(CACHE_SIZE);
    this._size = 0;
    this._fallback = new Map(); // for complex keys (intersection state, generic refs)
  }

  FastRelationCache.prototype.get = function(key) {
    // Fast path: simple "sourceId,targetId" key
    var comma = key.indexOf(',');
    if (comma > 0) {
      var colon = key.indexOf(':', comma);
      if (colon === -1) {
        // Simple key: "sourceId,targetId" → integer hash
        var srcId = +key.substring(0, comma);
        var tgtId = +key.substring(comma + 1);
        if (srcId === (srcId | 0) && tgtId === (tgtId | 0) && srcId < 0x100000 && tgtId < 0x100000) {
          var packed = (srcId << 20) | tgtId;
          var hash = ((srcId * 2654435761) ^ tgtId) & CACHE_MASK;
          // Linear probe (max 4 steps)
          for (var i = 0; i < 4; i++) {
            var slot = (hash + i) & CACHE_MASK;
            if (this._keys[slot] === packed) return this._vals[slot];
            if (this._keys[slot] === 0) return undefined;
          }
          return undefined;
        }
      }
    }
    // Slow path: complex key
    return this._fallback.get(key);
  };

  FastRelationCache.prototype.set = function(key, value) {
    var comma = key.indexOf(',');
    if (comma > 0) {
      var colon = key.indexOf(':', comma);
      if (colon === -1) {
        var srcId = +key.substring(0, comma);
        var tgtId = +key.substring(comma + 1);
        if (srcId === (srcId | 0) && tgtId === (tgtId | 0) && srcId < 0x100000 && tgtId < 0x100000) {
          var packed = (srcId << 20) | tgtId;
          var hash = ((srcId * 2654435761) ^ tgtId) & CACHE_MASK;
          for (var i = 0; i < 4; i++) {
            var slot = (hash + i) & CACHE_MASK;
            if (this._keys[slot] === packed || this._keys[slot] === 0) {
              this._keys[slot] = packed;
              this._vals[slot] = value;
              this._size++;
              return this;
            }
          }
          // All 4 probe slots full — evict first
          this._keys[hash] = packed;
          this._vals[hash] = value;
          return this;
        }
      }
    }
    this._fallback.set(key, value);
    return this;
  };

  FastRelationCache.prototype.has = function(key) {
    return this.get(key) !== undefined;
  };

  FastRelationCache.prototype.delete = function(key) {
    this._fallback.delete(key);
    return true;
  };

  FastRelationCache.prototype.clear = function() {
    this._keys.fill(0);
    this._vals.fill(0);
    this._size = 0;
    this._fallback.clear();
  };

  FastRelationCache.prototype.forEach = function(cb) {
    // Iterate non-zero entries
    for (var i = 0; i < CACHE_SIZE; i++) {
      if (this._keys[i] !== 0) {
        var srcId = this._keys[i] >>> 20;
        var tgtId = this._keys[i] & 0xFFFFF;
        cb(this._vals[i], srcId + ',' + tgtId, this);
      }
    }
    this._fallback.forEach(cb);
  };

  Object.defineProperty(FastRelationCache.prototype, 'size', {
    get: function() { return this._size + this._fallback.size; }
  });

  // Make it iterable
  FastRelationCache.prototype[Symbol.iterator] = function() {
    return this._fallback[Symbol.iterator]();
  };

  // Store for injection
  globalThis.__FastRelationCache = FastRelationCache;
})();
