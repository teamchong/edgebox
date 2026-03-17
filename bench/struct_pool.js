// Struct Pool Benchmark
// Demonstrates tsgo-style flat struct allocation in typed arrays vs JS objects.
// Simulates a type-checker workload: millions of Type records, filter/match/traverse.
//
// Struct layout (16 bytes per Type, 4 i32 fields):
//   offset +0: kind      (i32) — type kind enum (0-15)
//   offset +1: flags     (i32) — bitfield
//   offset +2: symbolId  (i32) — reference to symbol table
//   offset +3: parentId  (i32) — reference to parent type (-1 = none)
//
// Usage:
//   node bench/struct_pool.js
//   node zig-out/bin/bench/struct_pool.js/struct_pool-worker.mjs

const N = 500000; // 500K types (typical large TS project)
const FIELDS = 4; // fields per struct

// === Flat struct functions (EdgeBox compiles to WASM) ===

// Count types matching a specific kind (stride iteration — no multiply)
function filterByKind(pool, count, kind) {
  var matches = 0;
  var end = (count << 2) | 0;
  for (var base = 0; base < end; base = base + 4 | 0) {
    if (pool[base] === kind) {
      matches = matches + 1 | 0;
    }
  }
  return matches;
}

// Count types with specific flag bits set
function filterByFlags(pool, count, mask) {
  var matches = 0;
  var end = (count << 2) | 0;
  for (var base = 0; base < end; base = base + 4 | 0) {
    if ((pool[base + 1 | 0] & mask) !== 0) {
      matches = matches + 1 | 0;
    }
  }
  return matches;
}

// Follow parent chain, return depth (simulates type resolution)
// parentId field is at offset +3; each struct is 4 i32s wide
function resolveDepth(pool, handle) {
  var depth = 0;
  var cur = (handle << 2) | 0;
  while (cur >= 0) {
    depth = depth + 1 | 0;
    cur = pool[cur + 3 | 0] | 0;
    if (cur >= 0) cur = (cur << 2) | 0;
    if (depth > 100) break; // cycle guard
  }
  return depth;
}

// Sum resolve depths for all types (exercises parent chain traversal)
function sumResolveDepths(pool, count) {
  var total = 0;
  for (var i = 0; i < count; i = i + 1 | 0) {
    total = (total + resolveDepth(pool, i)) | 0;
  }
  return total;
}

// Structural comparison: count types that match kind+flags of a reference type
function matchStructural(pool, count, refKind, refFlags) {
  var matches = 0;
  var end = (count << 2) | 0;
  for (var base = 0; base < end; base = base + 4 | 0) {
    if (pool[base] === refKind && pool[base + 1 | 0] === refFlags) {
      matches = matches + 1 | 0;
    }
  }
  return matches;
}

// Compute aggregate stats: sum of all symbolIds for types matching a kind
function aggregateSymbols(pool, count, kind) {
  var sum = 0;
  var end = (count << 2) | 0;
  for (var base = 0; base < end; base = base + 4 | 0) {
    if (pool[base] === kind) {
      sum = (sum + pool[base + 2 | 0]) | 0;
    }
  }
  return sum;
}

// === Build test data ===

// Flat pool: Int32Array, 4 fields per type
var pool = new Int32Array(N * FIELDS);
// Equivalent JS objects for comparison
var objects = new Array(N);

// Simple LCG PRNG for deterministic data
var seed = 12345;
function rng() {
  seed = (seed * 1103515245 + 12345) | 0;
  return (seed >>> 16) & 0x7fff;
}

for (var i = 0; i < N; i++) {
  var kind = rng() % 16;
  var flags = rng() & 0xff;
  var symbolId = rng();
  // 70% of types have a parent (chain depth ~3-5 on average)
  var parentId = (rng() % 10 < 7 && i > 0) ? (rng() % i) : -1;

  var base = i * FIELDS;
  pool[base] = kind;
  pool[base + 1] = flags;
  pool[base + 2] = symbolId;
  pool[base + 3] = parentId;

  objects[i] = { kind: kind, flags: flags, symbolId: symbolId, parentId: parentId };
}

// === JS object equivalents (for comparison) ===

function filterByKindJS(objs, count, kind) {
  var matches = 0;
  for (var i = 0; i < count; i++) {
    if (objs[i].kind === kind) matches++;
  }
  return matches;
}

function filterByFlagsJS(objs, count, mask) {
  var matches = 0;
  for (var i = 0; i < count; i++) {
    if ((objs[i].flags & mask) !== 0) matches++;
  }
  return matches;
}

function resolveDepthJS(objs, handle) {
  var depth = 0;
  var cur = handle;
  while (cur >= 0) {
    depth++;
    cur = objs[cur].parentId;
    if (depth > 100) break;
  }
  return depth;
}

function sumResolveDepthsJS(objs, count) {
  var total = 0;
  for (var i = 0; i < count; i++) {
    total += resolveDepthJS(objs, i);
  }
  return total;
}

function matchStructuralJS(objs, count, refKind, refFlags) {
  var matches = 0;
  for (var i = 0; i < count; i++) {
    if (objs[i].kind === refKind && objs[i].flags === refFlags) matches++;
  }
  return matches;
}

function aggregateSymbolsJS(objs, count, kind) {
  var sum = 0;
  for (var i = 0; i < count; i++) {
    if (objs[i].kind === kind) sum = (sum + objs[i].symbolId) | 0;
  }
  return sum;
}

// === Benchmark ===

var WARMUP = 5;
var RUNS = 20;

function bench(name, fn) {
  for (var w = 0; w < WARMUP; w++) fn();
  var start = Date.now();
  var result;
  for (var r = 0; r < RUNS; r++) result = fn();
  var elapsed = Date.now() - start;
  console.log(name + ": " + elapsed + "ms (result=" + result + ")");
  return elapsed;
}

console.log("=== Struct Pool Benchmark ===");
console.log("Types: " + N + " (" + (N * FIELDS * 4 / 1024 / 1024).toFixed(1) + "MB flat pool)");
console.log("");

console.log("--- filterByKind (scan + compare) ---");
var t1 = bench("  flat_pool ", function() { return filterByKind(pool, N, 3); });
var t2 = bench("  js_objects", function() { return filterByKindJS(objects, N, 3); });
console.log("  speedup: " + (t2/t1).toFixed(2) + "x");
console.log("");

console.log("--- filterByFlags (scan + bitwise) ---");
var t3 = bench("  flat_pool ", function() { return filterByFlags(pool, N, 0x20); });
var t4 = bench("  js_objects", function() { return filterByFlagsJS(objects, N, 0x20); });
console.log("  speedup: " + (t4/t3).toFixed(2) + "x");
console.log("");

console.log("--- sumResolveDepths (chain traversal) ---");
var t5 = bench("  flat_pool ", function() { return sumResolveDepths(pool, N); });
var t6 = bench("  js_objects", function() { return sumResolveDepthsJS(objects, N); });
console.log("  speedup: " + (t6/t5).toFixed(2) + "x");
console.log("");

console.log("--- matchStructural (multi-field compare) ---");
var t7 = bench("  flat_pool ", function() { return matchStructural(pool, N, 3, 0x20); });
var t8 = bench("  js_objects", function() { return matchStructuralJS(objects, N, 3, 0x20); });
console.log("  speedup: " + (t8/t7).toFixed(2) + "x");
console.log("");

console.log("--- aggregateSymbols (filter + accumulate) ---");
var t9 = bench("  flat_pool ", function() { return aggregateSymbols(pool, N, 5); });
var t10 = bench("  js_objects", function() { return aggregateSymbolsJS(objects, N, 5); });
console.log("  speedup: " + (t10/t9).toFixed(2) + "x");
console.log("");

var totalFlat = t1 + t3 + t5 + t7 + t9;
var totalJS = t2 + t4 + t6 + t8 + t10;
console.log("=== Total: flat_pool " + totalFlat + "ms vs js_objects " + totalJS + "ms ===");
console.log("=== Overall: " + (totalJS/totalFlat).toFixed(2) + "x ===");
