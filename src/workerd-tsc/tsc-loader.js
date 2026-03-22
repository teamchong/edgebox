// tsc-loader.js — Load TypeScript compiler into workerd
// Shared by main-worker and checker-workers.
// Uses __edgebox_io_sync (Zig IO bridge) for filesystem access.
'use strict';

var ts = null;

function loadTsc() {
  if (ts) return ts;

  // Read typescript.js from Zig IO cache
  var resp = JSON.parse(__edgebox_io_sync(JSON.stringify({
    op: 'readFile',
    path: require('path').resolve(process.cwd(), 'node_modules/typescript/lib/typescript.js')
  })));

  if (!resp.ok) {
    throw new Error('Failed to load typescript.js: ' + JSON.stringify(resp));
  }

  // Evaluate TypeScript source (creates global `ts` namespace)
  var mod = { exports: {} };
  var fn = new Function('module', 'exports', 'require', '__filename', '__dirname', resp.data);
  fn(mod, mod.exports, require, 'typescript.js', '.');
  ts = mod.exports;

  return ts;
}

module.exports = { loadTsc };
