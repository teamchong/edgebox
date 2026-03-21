// v8_tsc_shim.js — Minimal runtime shim for TSC transforms
(function() {
  'use strict';
  globalThis.__FastRelationCache = Map;
  globalThis.__typesById = [];
  // Source file cache for createSourceFile memoization (T9-T10)
  globalThis.__sfCache = Object.create(null);
})();
