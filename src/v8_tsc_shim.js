// v8_tsc_shim.js — Inline relation cache acceleration
// Adds _ck/_cv (Float64Array/Array cache) to relation Map objects.
// The source transform inlines cache lookups at relation.get() call sites
// instead of wrapping Map — zero function dispatch overhead.
(function() {
  'use strict';

  // FastRelationCache: Map subclass that auto-creates cache on first numeric set
  function FastRelationCache() {
    var m = new Map();
    m._ck = null;
    m._cv = null;
    var origSet = m.set.bind(m);
    m.set = function(key, value) {
      origSet(key, value);
      if (typeof key === 'number') {
        if (!this._ck) {
          this._ck = new Float64Array(131072);
          this._cv = new Array(131072);
          this._ck.fill(-1);
        }
        var h = (((key * 2654435761) | 0) & 0x7FFFFFFF) & 131071;
        this._ck[h] = key;
        this._cv[h] = value;
      }
      return this;
    };
    return m;
  }

  globalThis.__FastRelationCache = FastRelationCache;
})();
