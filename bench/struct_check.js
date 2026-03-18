// Minimal struct test — checking if EdgeBox can detect and convert object property access
// to flat struct access in WASM memory.

function checkKind(t, kind) {
  return t.kind === kind ? 1 : 0;
}

function checkFlags(t, mask) {
  return (t.flags & mask) !== 0 ? 1 : 0;
}

function checkBoth(t, kind, mask) {
  return t.kind === kind && (t.flags & mask) !== 0 ? 1 : 0;
}

// Multi-struct-arg: exercises the codegen fix for compare(nodeA, nodeB)
function sameKind(a, b) {
  return a.kind === b.kind ? 1 : 0;
}

function sumKinds(arr, count) {
  var sum = 0;
  for (var i = 0; i < count; i = i + 1 | 0) {
    sum = (sum + arr[i].kind) | 0;
  }
  return sum;
}

// Test
var types = [];
for (var i = 0; i < 100000; i++) {
  types.push({ kind: (i % 16) | 0, flags: (i * 7) & 0xff, symbolId: i });
}

var WARMUP = 5;
var RUNS = 20;

for (var w = 0; w < WARMUP; w++) {
  checkKind(types[0], 3);
  sumKinds(types, types.length);
}

var t0 = Date.now();
var r1 = 0;
for (var r = 0; r < RUNS; r++) {
  for (var j = 0; j < types.length; j++) {
    r1 = (r1 + checkBoth(types[j], 3, 0x20)) | 0;
  }
}
var t1 = Date.now();
console.log("checkBoth: " + (t1 - t0) + "ms (matches=" + r1 + ")");

t0 = Date.now();
var r2 = 0;
for (var r = 0; r < RUNS; r++) {
  r2 = sumKinds(types, types.length);
}
t1 = Date.now();
console.log("sumKinds: " + (t1 - t0) + "ms (sum=" + r2 + ")");

// Multi-struct-arg correctness check: compare types[k] vs types[k+16]
// (same kind since kind = k % 16, so k and k+16 always match)
var r3 = 0;
for (var k = 0; k < 1000; k++) {
  r3 = (r3 + sameKind(types[k], types[k + 16])) | 0;
}
console.log("sameKind: matches=" + r3 + " (expected 1000)");
