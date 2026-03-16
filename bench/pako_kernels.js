// Extracted pako core numeric kernels with named function declarations
// These compile to standalone WASM via the AOT+JIT pipeline

// Adler32 checksum — from pako/lib/zlib/adler32.js
function adler32(adler, buf, len, pos) {
  var s1 = (adler & 0xffff) | 0;
  var s2 = ((adler >>> 16) & 0xffff) | 0;
  var n = 0;

  while (len !== 0) {
    n = len > 2000 ? 2000 : len;
    len -= n;
    do {
      s1 = (s1 + buf[pos++]) | 0;
      s2 = (s2 + s1) | 0;
    } while (--n);
    s1 %= 65521;
    s2 %= 65521;
  }
  return (s1 | (s2 << 16)) | 0;
}

// CRC32 table generator (uses XOR, shifts, bitwise — pure i32)
function makeCrcTable() {
  var table = new Array(256);
  for (var n = 0; n < 256; n++) {
    var c = n;
    for (var k = 0; k < 8; k++) {
      c = c & 1 ? 0xEDB88320 ^ (c >>> 1) : c >>> 1;
    }
    table[n] = c;
  }
  return table;
}

// Zero-fill an array
function zero(buf) {
  var len = buf.length;
  while (--len >= 0) {
    buf[len] = 0;
  }
}

// Simple array fill (seed pattern)
function fillData(arr, seed) {
  for (var i = 0; i < arr.length; i++) {
    arr[i] = (i * seed + 17) & 0xff;
  }
}

// Test and benchmark
var data = new Array(10000);
fillData(data, 31);

// Warmup
for (var i = 0; i < 3; i++) {
  adler32(1, data, data.length, 0);
}

// Benchmark adler32
var start = Date.now();
var result;
for (var i = 0; i < 10000; i++) {
  result = adler32(1, data, 10000, 0);
}
var t1 = Date.now() - start;
console.log("adler32 10K (10KB): " + t1 + "ms (checksum=0x" + (result >>> 0).toString(16) + ")");

// Test zero
var zbuf = new Array(1000);
fillData(zbuf, 42);
zero(zbuf);
var allZero = true;
for (var i = 0; i < 1000; i++) {
  if (zbuf[i] !== 0) { allZero = false; break; }
}
console.log("zero: " + (allZero ? "PASS" : "FAIL"));

// Benchmark zero
start = Date.now();
for (var i = 0; i < 100000; i++) {
  zero(zbuf);
}
var t2 = Date.now() - start;
console.log("zero 100K (1000): " + t2 + "ms");

// Benchmark fillData
start = Date.now();
for (var i = 0; i < 100000; i++) {
  fillData(data, 31);
}
var t3 = Date.now() - start;
console.log("fillData 100K (10KB): " + t3 + "ms");

// Test CRC table
var crcTable = makeCrcTable();
console.log("crc_table[0]=" + crcTable[0] + " crc_table[1]=" + crcTable[1] + " crc_table[255]=" + crcTable[255]);
