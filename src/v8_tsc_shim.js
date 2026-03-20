// v8_tsc_shim.js — Zero-copy adaptive Map for TypeScript compiler
//
// Provides FastMap: a Map replacement that auto-promotes to Int32Array
// when it detects numeric "N,N" key patterns (type ID pairs).
// Falls back to regular Map for string/complex keys.
//
// Zero-copy principle: avoid string allocation for numeric lookups.
// The Int32Array is Zig-managed memory in the real implementation;
// here we use JS arrays as the proof-of-concept.

(function() {
  'use strict';

  var INITIAL_BITS = 14; // 16K slots initially (128KB)
  var MAX_BITS = 20;     // grow up to 1M slots (8MB)

  function FastMap() {
    this._map = new Map();
    this._keys = null; // lazy: Int32Array (allocated on first numeric hit)
    this._vals = null;
    this._bits = 0;
    this._mask = 0;
    this._numericCount = 0;
    this._promoted = false;
  }

  // Parse "N,N" or "N,N:S" key → packed integer, or -1 if not numeric
  function parseNumericKey(key) {
    var comma = key.indexOf(',');
    if (comma <= 0 || comma > 8) return -1;
    var colon = key.indexOf(':', comma);
    // Only handle simple "N,N" keys (no postfix) for the fast path
    if (colon !== -1) return -1;
    // Parse both IDs
    var a = 0, b = 0;
    for (var i = 0; i < comma; i++) {
      var c = key.charCodeAt(i) - 48;
      if (c < 0 || c > 9) return -1;
      a = a * 10 + c;
    }
    for (var j = comma + 1; j < key.length; j++) {
      var d = key.charCodeAt(j) - 48;
      if (d < 0 || d > 9) return -1;
      b = b * 10 + d;
    }
    if (a >= 0x100000 || b >= 0x100000) return -1;
    return (a << 20) | b;
  }

  function promote(self) {
    self._bits = INITIAL_BITS;
    self._mask = (1 << self._bits) - 1;
    self._keys = new Int32Array(1 << self._bits);
    self._vals = new Int32Array(1 << self._bits);
    self._promoted = true;
    // Migrate existing numeric entries from Map
    self._map.forEach(function(val, key) {
      var packed = parseNumericKey(key);
      if (packed >= 0) {
        fastSet(self, packed, val);
        self._map.delete(key);
      }
    });
  }

  function grow(self) {
    if (self._bits >= MAX_BITS) return;
    var oldKeys = self._keys;
    var oldVals = self._vals;
    var oldSize = 1 << self._bits;
    self._bits += 2;
    self._mask = (1 << self._bits) - 1;
    self._keys = new Int32Array(1 << self._bits);
    self._vals = new Int32Array(1 << self._bits);
    // Rehash
    for (var i = 0; i < oldSize; i++) {
      if (oldKeys[i] !== 0) {
        fastSet(self, oldKeys[i], oldVals[i]);
      }
    }
  }

  function fastSet(self, packed, value) {
    var hash = ((packed * 2654435761) >>> 0) & self._mask;
    for (var i = 0; i < 8; i++) {
      var slot = (hash + i) & self._mask;
      if (self._keys[slot] === packed || self._keys[slot] === 0) {
        self._keys[slot] = packed;
        self._vals[slot] = value;
        return;
      }
    }
    // All 8 probe slots full — grow and retry
    grow(self);
    fastSet(self, packed, value);
  }

  function fastGet(self, packed) {
    var hash = ((packed * 2654435761) >>> 0) & self._mask;
    for (var i = 0; i < 8; i++) {
      var slot = (hash + i) & self._mask;
      if (self._keys[slot] === packed) return self._vals[slot];
      if (self._keys[slot] === 0) return undefined;
    }
    return undefined;
  }

  FastMap.prototype.get = function(key) {
    if (this._promoted && typeof key === 'string') {
      var packed = parseNumericKey(key);
      if (packed >= 0) {
        var v = fastGet(this, packed);
        if (v !== undefined) return v;
        // Could be a hash collision miss — check Map fallback
      }
    }
    return this._map.get(key);
  };

  FastMap.prototype.set = function(key, value) {
    if (typeof key === 'string') {
      var packed = parseNumericKey(key);
      if (packed >= 0) {
        if (!this._promoted) {
          this._numericCount++;
          if (this._numericCount >= 16) {
            promote(this);
          }
        }
        if (this._promoted) {
          fastSet(this, packed, value);
          return this;
        }
      }
    }
    this._map.set(key, value);
    return this;
  };

  FastMap.prototype.has = function(key) {
    if (this._promoted && typeof key === 'string') {
      var packed = parseNumericKey(key);
      if (packed >= 0 && fastGet(this, packed) !== undefined) return true;
    }
    return this._map.has(key);
  };

  FastMap.prototype.delete = function(key) {
    return this._map.delete(key);
  };

  FastMap.prototype.clear = function() {
    this._map.clear();
    if (this._promoted) {
      this._keys.fill(0);
      this._vals.fill(0);
    }
  };

  FastMap.prototype.forEach = function(cb) {
    if (this._promoted) {
      var size = 1 << this._bits;
      for (var i = 0; i < size; i++) {
        if (this._keys[i] !== 0) {
          var a = this._keys[i] >>> 20;
          var b = this._keys[i] & 0xFFFFF;
          cb(this._vals[i], a + ',' + b, this);
        }
      }
    }
    this._map.forEach(cb);
  };

  Object.defineProperty(FastMap.prototype, 'size', {
    get: function() { return this._map.size + (this._promoted ? this._numericCount : 0); }
  });

  // entries/keys/values — delegate to Map (numeric entries reconstructed)
  FastMap.prototype.entries = function() {
    // Merge numeric entries back into Map for iteration
    if (this._promoted) {
      var size = 1 << this._bits;
      for (var i = 0; i < size; i++) {
        if (this._keys[i] !== 0) {
          var a = this._keys[i] >>> 20;
          var b = this._keys[i] & 0xFFFFF;
          this._map.set(a + ',' + b, this._vals[i]);
        }
      }
    }
    return this._map.entries();
  };

  FastMap.prototype.keys = function() {
    if (this._promoted) {
      var size = 1 << this._bits;
      for (var i = 0; i < size; i++) {
        if (this._keys[i] !== 0) {
          var a = this._keys[i] >>> 20;
          var b = this._keys[i] & 0xFFFFF;
          if (!this._map.has(a + ',' + b)) this._map.set(a + ',' + b, this._vals[i]);
        }
      }
    }
    return this._map.keys();
  };

  FastMap.prototype.values = function() {
    if (this._promoted) {
      var size = 1 << this._bits;
      for (var i = 0; i < size; i++) {
        if (this._keys[i] !== 0) {
          var a = this._keys[i] >>> 20;
          var b = this._keys[i] & 0xFFFFF;
          if (!this._map.has(a + ',' + b)) this._map.set(a + ',' + b, this._vals[i]);
        }
      }
    }
    return this._map.values();
  };

  FastMap.prototype[Symbol.iterator] = function() {
    return this.entries();
  };

  globalThis.__FastRelationCache = FastMap;

  // SOA shadow column for Type.flags — flat Int32Array indexed by type.id
  // createType() writes here; hot paths can read __type_flags[id] instead of type.flags
  globalThis.__type_flags = new Int32Array(262144); // 256K types, 1MB
  globalThis.__type_col_size = 262144;
})();
