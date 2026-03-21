// v8_tsc_shim.js — Minimal runtime shim for TSC transforms
(function() {
  'use strict';
  globalThis.__FastRelationCache = Map;
  globalThis.__typesById = [];
  // objectFlags column for propertiesIdenticalTo fast-reject (T-SOA1/T-SOA2)
  if (typeof globalThis.__pc_objectFlags === 'undefined')
    globalThis.__pc_objectFlags = new Int32Array(131072);
  // Source file cache for createSourceFile memoization (T9-T10)
  globalThis.__sfCache = Object.create(null);
})();
