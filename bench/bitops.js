// Bitwise Operations Benchmark
// Tests pure i32 bitwise compute — ideal for standalone WASM

// 1. Right-rotate 32-bit
function rotr(x, n) {
  return (x >>> n) | (x << (32 - n));
}

// 2. Left-rotate 32-bit
function rotl(x, n) {
  return (x << n) | (x >>> (32 - n));
}

// 3. Bit population count (Hamming weight)
function popcount(x) {
  x = x - ((x >>> 1) & 0x55555555);
  x = (x & 0x33333333) + ((x >>> 2) & 0x33333333);
  x = (x + (x >>> 4)) & 0x0F0F0F0F;
  return (x * 0x01010101) >>> 24;
}

// 4. Reverse bits
function reverseBits(n) {
  n = ((n >>> 1) & 0x55555555) | ((n & 0x55555555) << 1);
  n = ((n >>> 2) & 0x33333333) | ((n & 0x33333333) << 2);
  n = ((n >>> 4) & 0x0F0F0F0F) | ((n & 0x0F0F0F0F) << 4);
  n = ((n >>> 8) & 0x00FF00FF) | ((n & 0x00FF00FF) << 8);
  n = (n >>> 16) | (n << 16);
  return n;
}

// 5. CRC32 step (one byte)
function crc32_step(crc, byte) {
  crc = crc ^ byte;
  var i = 0;
  while (i < 8) {
    if (crc & 1) {
      crc = (crc >>> 1) ^ 0xEDB88320;
    } else {
      crc = crc >>> 1;
    }
    i = i + 1;
  }
  return crc;
}

// 6. Jenkins hash (one-at-a-time)
function jenkinsHash(key) {
  var hash = 0;
  var i = 0;
  while (i < 32) {
    hash = hash + ((key >>> i) & 0xFF);
    hash = hash + (hash << 10);
    hash = hash ^ (hash >>> 6);
    i = i + 8;
  }
  hash = hash + (hash << 3);
  hash = hash ^ (hash >>> 11);
  hash = hash + (hash << 15);
  return hash;
}

// 7. MurmurHash3 finalizer (32-bit)
function murmur3_fmix(h) {
  h = h ^ (h >>> 16);
  h = (h * 0x85ebca6b) | 0;
  h = h ^ (h >>> 13);
  h = (h * 0xc2b2ae35) | 0;
  h = h ^ (h >>> 16);
  return h;
}

// 8. xorshift32 PRNG
function xorshift32(state) {
  state = state ^ (state << 13);
  state = state ^ (state >>> 17);
  state = state ^ (state << 5);
  return state;
}

// === Run benchmarks ===
function bench(name, fn, iterations) {
  for (var i = 0; i < 3; i++) fn();
  var start = Date.now();
  var result;
  for (var i = 0; i < iterations; i++) {
    result = fn();
  }
  var elapsed = Date.now() - start;
  console.log(name + ": " + elapsed + "ms (" + iterations + " runs, result=" + result + ")");
  return elapsed;
}

console.log("=== Bitwise Operations Benchmarks ===");

bench("rotr 100M", function() {
  var x = 0x12345678;
  for (var i = 0; i < 100000000; i++) x = rotr(x, 7);
  return x;
}, 1);

bench("popcount 10M", function() {
  var total = 0;
  for (var i = 0; i < 10000000; i++) total = total + popcount(i);
  return total;
}, 1);

bench("reverseBits 10M", function() {
  var x = 0;
  for (var i = 0; i < 10000000; i++) x = x ^ reverseBits(i);
  return x;
}, 1);

bench("crc32 1M", function() {
  var crc = 0xFFFFFFFF;
  for (var i = 0; i < 1000000; i++) crc = crc32_step(crc, i & 0xFF);
  return crc;
}, 1);

bench("jenkinsHash 10M", function() {
  var h = 0;
  for (var i = 0; i < 10000000; i++) h = h ^ jenkinsHash(i);
  return h;
}, 1);

bench("murmur3 10M", function() {
  var h = 0;
  for (var i = 0; i < 10000000; i++) h = h ^ murmur3_fmix(i);
  return h;
}, 1);

bench("xorshift32 100M", function() {
  var state = 1;
  for (var i = 0; i < 100000000; i++) state = xorshift32(state);
  return state;
}, 1);
