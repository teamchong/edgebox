// v8_parallel_init.js — Runtime initialization for edgebox parallel APIs
// Runs AFTER __edgebox_parallel C callback is registered.
(function() {
  var e = globalThis.edgebox || {};

  // edgebox.parallel — dispatch functions to separate V8 isolates
  e.parallel = function(fns) {
    if (!Array.isArray(fns) || fns.length === 0) return [];
    var codes = fns.map(function(fn) {
      if (typeof fn === 'function') return 'return (' + fn.toString() + ')()';
      if (typeof fn === 'string') return fn;
      return 'return null';
    });
    var json = __edgebox_parallel(JSON.stringify(codes));
    return json ? JSON.parse(json) : [];
  };

  // edgebox.parallelAsync — Promise wrapper
  e.parallelAsync = function(fns) {
    return Promise.resolve(e.parallel(fns));
  };

  // edgebox.map — parallel map over arrays
  e.map = function(arr, fn, opts) {
    if (!Array.isArray(arr) || arr.length === 0) return [];
    var cpus = (opts && opts.workers) || 4;
    var size = (opts && opts.chunkSize) || Math.ceil(arr.length / cpus);
    if (arr.length <= size) return arr.map(fn); // too small to parallelize
    var chunks = [];
    for (var i = 0; i < arr.length; i += size) chunks.push(arr.slice(i, i + size));
    var fnStr = fn.toString();
    var codes = chunks.map(function(chunk) {
      return 'var fn = ' + fnStr + '; return ' + JSON.stringify(chunk) + '.map(fn)';
    });
    var json = __edgebox_parallel(JSON.stringify(codes));
    if (!json) return arr.map(fn);
    var results = JSON.parse(json);
    var flat = [];
    for (var j = 0; j < results.length; j++) {
      if (Array.isArray(results[j])) {
        for (var k = 0; k < results[j].length; k++) flat.push(results[j][k]);
      }
    }
    return flat;
  };

  // edgebox.reduce — parallel reduce with auto-combine
  e.reduce = function(arr, fn, initial, combiner) {
    if (!Array.isArray(arr) || arr.length === 0) return initial;
    var cpus = 4;
    var size = Math.ceil(arr.length / cpus);
    if (arr.length <= size) return arr.reduce(fn, initial);
    var chunks = [];
    for (var i = 0; i < arr.length; i += size) chunks.push(arr.slice(i, i + size));
    var fnStr = fn.toString();
    var initStr = JSON.stringify(initial);
    var codes = chunks.map(function(chunk) {
      return 'var fn = ' + fnStr + '; return ' + JSON.stringify(chunk) + '.reduce(fn, ' + initStr + ')';
    });
    var json = __edgebox_parallel(JSON.stringify(codes));
    if (!json) return arr.reduce(fn, initial);
    var results = JSON.parse(json);
    var comb = combiner || fn;
    var result = results[0];
    for (var j = 1; j < results.length; j++) result = comb(result, results[j]);
    return result;
  };

  globalThis.edgebox = e;
})();
