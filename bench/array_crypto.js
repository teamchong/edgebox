// Array-based crypto benchmarks — tests standalone WASM with linear memory
// These are simplified versions of real crypto primitives (tweetnacl, sha256)

// Salsa20 quarter-round (core crypto primitive, pure i32 + array access)
function salsa20_qr(state) {
  var x0 = state[0], x1 = state[1], x2 = state[2], x3 = state[3];
  var x4 = state[4], x5 = state[5], x6 = state[6], x7 = state[7];
  var x8 = state[8], x9 = state[9], x10 = state[10], x11 = state[11];
  var x12 = state[12], x13 = state[13], x14 = state[14], x15 = state[15];
  var u;

  for (var i = 0; i < 20; i = i + 2) {
    u = x0 + x12 | 0;
    x4 = x4 ^ (u << 7 | u >>> 25);
    u = x4 + x0 | 0;
    x8 = x8 ^ (u << 9 | u >>> 23);
    u = x8 + x4 | 0;
    x12 = x12 ^ (u << 13 | u >>> 19);
    u = x12 + x8 | 0;
    x0 = x0 ^ (u << 18 | u >>> 14);

    u = x5 + x1 | 0;
    x9 = x9 ^ (u << 7 | u >>> 25);
    u = x9 + x5 | 0;
    x13 = x13 ^ (u << 9 | u >>> 23);
    u = x13 + x9 | 0;
    x1 = x1 ^ (u << 13 | u >>> 19);
    u = x1 + x13 | 0;
    x5 = x5 ^ (u << 18 | u >>> 14);

    u = x10 + x6 | 0;
    x14 = x14 ^ (u << 7 | u >>> 25);
    u = x14 + x10 | 0;
    x2 = x2 ^ (u << 9 | u >>> 23);
    u = x2 + x14 | 0;
    x6 = x6 ^ (u << 13 | u >>> 19);
    u = x6 + x2 | 0;
    x10 = x10 ^ (u << 18 | u >>> 14);

    u = x15 + x11 | 0;
    x3 = x3 ^ (u << 7 | u >>> 25);
    u = x3 + x15 | 0;
    x7 = x7 ^ (u << 9 | u >>> 23);
    u = x7 + x3 | 0;
    x11 = x11 ^ (u << 13 | u >>> 19);
    u = x11 + x7 | 0;
    x15 = x15 ^ (u << 18 | u >>> 14);

    u = x0 + x3 | 0;
    x1 = x1 ^ (u << 7 | u >>> 25);
    u = x1 + x0 | 0;
    x2 = x2 ^ (u << 9 | u >>> 23);
    u = x2 + x1 | 0;
    x3 = x3 ^ (u << 13 | u >>> 19);
    u = x3 + x2 | 0;
    x0 = x0 ^ (u << 18 | u >>> 14);

    u = x5 + x4 | 0;
    x6 = x6 ^ (u << 7 | u >>> 25);
    u = x6 + x5 | 0;
    x7 = x7 ^ (u << 9 | u >>> 23);
    u = x7 + x6 | 0;
    x4 = x4 ^ (u << 13 | u >>> 19);
    u = x4 + x7 | 0;
    x5 = x5 ^ (u << 18 | u >>> 14);

    u = x10 + x9 | 0;
    x11 = x11 ^ (u << 7 | u >>> 25);
    u = x11 + x10 | 0;
    x8 = x8 ^ (u << 9 | u >>> 23);
    u = x8 + x11 | 0;
    x9 = x9 ^ (u << 13 | u >>> 19);
    u = x9 + x8 | 0;
    x10 = x10 ^ (u << 18 | u >>> 14);

    u = x15 + x14 | 0;
    x12 = x12 ^ (u << 7 | u >>> 25);
    u = x12 + x15 | 0;
    x13 = x13 ^ (u << 9 | u >>> 23);
    u = x13 + x12 | 0;
    x14 = x14 ^ (u << 13 | u >>> 19);
    u = x14 + x13 | 0;
    x15 = x15 ^ (u << 18 | u >>> 14);
  }

  state[0] = x0; state[1] = x1; state[2] = x2; state[3] = x3;
  state[4] = x4; state[5] = x5; state[6] = x6; state[7] = x7;
  state[8] = x8; state[9] = x9; state[10] = x10; state[11] = x11;
  state[12] = x12; state[13] = x13; state[14] = x14; state[15] = x15;
  return 0;
}

// Vector add: o[i] = a[i] + b[i] for 16 elements
function vec_add16(o, a, b) {
  o[0] = a[0] + b[0]; o[1] = a[1] + b[1]; o[2] = a[2] + b[2]; o[3] = a[3] + b[3];
  o[4] = a[4] + b[4]; o[5] = a[5] + b[5]; o[6] = a[6] + b[6]; o[7] = a[7] + b[7];
  o[8] = a[8] + b[8]; o[9] = a[9] + b[9]; o[10] = a[10] + b[10]; o[11] = a[11] + b[11];
  o[12] = a[12] + b[12]; o[13] = a[13] + b[13]; o[14] = a[14] + b[14]; o[15] = a[15] + b[15];
  return 0;
}

// Dot product of two 16-element vectors
function dot16(a, b) {
  return a[0]*b[0] + a[1]*b[1] + a[2]*b[2] + a[3]*b[3]
       + a[4]*b[4] + a[5]*b[5] + a[6]*b[6] + a[7]*b[7]
       + a[8]*b[8] + a[9]*b[9] + a[10]*b[10] + a[11]*b[11]
       + a[12]*b[12] + a[13]*b[13] + a[14]*b[14] + a[15]*b[15];
}

// === Benchmark runner ===
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

console.log("=== Array Crypto Benchmarks ===");

bench("salsa20_qr 1M", function() {
  var state = [1634760805, 857760878, 2036477234, 1797285236,
               1634760805, 857760878, 2036477234, 1797285236,
               0, 0, 0, 0, 0, 0, 0, 0];
  for (var i = 0; i < 1000000; i++) {
    salsa20_qr(state);
  }
  return state[0];
}, 1);

bench("vec_add16 10M", function() {
  var o = [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0];
  var a = [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16];
  var b = [16,15,14,13,12,11,10,9,8,7,6,5,4,3,2,1];
  for (var i = 0; i < 10000000; i++) {
    vec_add16(o, a, b);
  }
  return o[0];
}, 1);

bench("dot16 10M", function() {
  var a = [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16];
  var b = [16,15,14,13,12,11,10,9,8,7,6,5,4,3,2,1];
  var sum = 0;
  for (var i = 0; i < 10000000; i++) {
    sum = sum + dot16(a, b) | 0;
  }
  return sum;
}, 1);
