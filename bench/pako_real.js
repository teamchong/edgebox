// Real pako (zlib) benchmark — uses the actual npm package
var pako = require('pako');

// Create test data — repetitive but not trivial
var data = new Uint8Array(10000);
for (var i = 0; i < 10000; i++) data[i] = (i * 31 + 17) & 0xff;

// Correctness test: compress → decompress → verify
var compressed = pako.deflate(data);
var decompressed = pako.inflate(compressed);
var match = true;
for (var i = 0; i < data.length; i++) {
  if (data[i] !== decompressed[i]) { match = false; break; }
}
console.log("roundtrip match: " + match + " (compressed " + data.length + " → " + compressed.length + " bytes)");

// Benchmark: deflate
var start = Date.now();
for (var i = 0; i < 1000; i++) {
  pako.deflate(data);
}
var t1 = Date.now() - start;
console.log("deflate 1K (10KB): " + t1 + "ms");

// Benchmark: inflate
start = Date.now();
for (var i = 0; i < 1000; i++) {
  pako.inflate(compressed);
}
var t2 = Date.now() - start;
console.log("inflate 1K (10KB): " + t2 + "ms");

// Benchmark: deflateRaw (no header)
start = Date.now();
for (var i = 0; i < 1000; i++) {
  pako.deflateRaw(data);
}
var t3 = Date.now() - start;
console.log("deflateRaw 1K (10KB): " + t3 + "ms");
