// SHA-256 implementation (pure compute, no dependencies)
// Based on the FIPS 180-4 specification

var K = [
  0x428a2f98, 0x71374491, 0xb5c0fbcf, 0xe9b5dba5,
  0x3956c25b, 0x59f111f1, 0x923f82a4, 0xab1c5ed5,
  0xd807aa98, 0x12835b01, 0x243185be, 0x550c7dc3,
  0x72be5d74, 0x80deb1fe, 0x9bdc06a7, 0xc19bf174,
  0xe49b69c1, 0xefbe4786, 0x0fc19dc6, 0x240ca1cc,
  0x2de92c6f, 0x4a7484aa, 0x5cb0a9dc, 0x76f988da,
  0x983e5152, 0xa831c66d, 0xb00327c8, 0xbf597fc7,
  0xc6e00bf3, 0xd5a79147, 0x06ca6351, 0x14292967,
  0x27b70a85, 0x2e1b2138, 0x4d2c6dfc, 0x53380d13,
  0x650a7354, 0x766a0abb, 0x81c2c92e, 0x92722c85,
  0xa2bfe8a1, 0xa81a664b, 0xc24b8b70, 0xc76c51a3,
  0xd192e819, 0xd6990624, 0xf40e3585, 0x106aa070,
  0x19a4c116, 0x1e376c08, 0x2748774c, 0x34b0bcb5,
  0x391c0cb3, 0x4ed8aa4a, 0x5b9cca4f, 0x682e6ff3,
  0x748f82ee, 0x78a5636f, 0x84c87814, 0x8cc70208,
  0x90befffa, 0xa4506ceb, 0xbef9a3f7, 0xc67178f2,
];

// Right-rotate 32-bit integer
function rotr(x, n) {
  return (x >>> n) | (x << (32 - n));
}

// SHA-256 compression function (single block)
// Takes 8 state values and 16 message words, returns new state
function sha256_compress(
  h0, h1, h2, h3, h4, h5, h6, h7,
  w0, w1, w2, w3, w4, w5, w6, w7,
  w8, w9, w10, w11, w12, w13, w14, w15
) {
  // Message schedule expansion (W[16..63])
  var W = [
    w0, w1, w2, w3, w4, w5, w6, w7,
    w8, w9, w10, w11, w12, w13, w14, w15,
    0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0,
  ];

  var i = 16;
  while (i < 64) {
    var s0 = rotr(W[i-15], 7) ^ rotr(W[i-15], 18) ^ (W[i-15] >>> 3);
    var s1 = rotr(W[i-2], 17) ^ rotr(W[i-2], 19) ^ (W[i-2] >>> 10);
    W[i] = (W[i-16] + s0 + W[i-7] + s1) | 0;
    i = i + 1;
  }

  // Compression
  var a = h0, b = h1, c = h2, d = h3;
  var e = h4, f = h5, g = h6, h = h7;

  i = 0;
  while (i < 64) {
    var S1 = rotr(e, 6) ^ rotr(e, 11) ^ rotr(e, 25);
    var ch = (e & f) ^ (~e & g);
    var temp1 = (h + S1 + ch + K[i] + W[i]) | 0;
    var S0 = rotr(a, 2) ^ rotr(a, 13) ^ rotr(a, 22);
    var maj = (a & b) ^ (a & c) ^ (b & c);
    var temp2 = (S0 + maj) | 0;

    h = g;
    g = f;
    f = e;
    e = (d + temp1) | 0;
    d = c;
    c = b;
    b = a;
    a = (temp1 + temp2) | 0;
    i = i + 1;
  }

  return ((h0 + a) | 0);
}

// Benchmark: repeated compression on dummy data
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

console.log("=== SHA-256 Benchmarks ===");

bench("sha256 compress 1M", function() {
  var h = 0x6a09e667;
  var i = 0;
  while (i < 1000000) {
    h = sha256_compress(
      h, 0xbb67ae85, 0x3c6ef372, 0xa54ff53a,
      0x510e527f, 0x9b05688c, 0x1f83d9ab, 0x5be0cd19,
      i, i+1, i+2, i+3, i+4, i+5, i+6, i+7,
      i+8, i+9, i+10, i+11, i+12, i+13, i+14, i+15
    );
    i = i + 1;
  }
  return h;
}, 1);

bench("rotr 100M", function() {
  var x = 0x12345678;
  var i = 0;
  while (i < 100000000) {
    x = rotr(x, 7);
    i = i + 1;
  }
  return x;
}, 1);
