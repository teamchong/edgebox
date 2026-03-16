// Extracted numeric kernels from tweetnacl-js (nacl-fast.js)
// Top-level functions for AOT+JIT WASM compilation

function core_salsa20(o, p, k, c) {
  var j0  = c[ 0] & 0xff | (c[ 1] & 0xff)<<8 | (c[ 2] & 0xff)<<16 | (c[ 3] & 0xff)<<24,
      j1  = k[ 0] & 0xff | (k[ 1] & 0xff)<<8 | (k[ 2] & 0xff)<<16 | (k[ 3] & 0xff)<<24,
      j2  = k[ 4] & 0xff | (k[ 5] & 0xff)<<8 | (k[ 6] & 0xff)<<16 | (k[ 7] & 0xff)<<24,
      j3  = k[ 8] & 0xff | (k[ 9] & 0xff)<<8 | (k[10] & 0xff)<<16 | (k[11] & 0xff)<<24,
      j4  = k[12] & 0xff | (k[13] & 0xff)<<8 | (k[14] & 0xff)<<16 | (k[15] & 0xff)<<24,
      j5  = c[ 4] & 0xff | (c[ 5] & 0xff)<<8 | (c[ 6] & 0xff)<<16 | (c[ 7] & 0xff)<<24,
      j6  = p[ 0] & 0xff | (p[ 1] & 0xff)<<8 | (p[ 2] & 0xff)<<16 | (p[ 3] & 0xff)<<24,
      j7  = p[ 4] & 0xff | (p[ 5] & 0xff)<<8 | (p[ 6] & 0xff)<<16 | (p[ 7] & 0xff)<<24,
      j8  = p[ 8] & 0xff | (p[ 9] & 0xff)<<8 | (p[10] & 0xff)<<16 | (p[11] & 0xff)<<24,
      j9  = p[12] & 0xff | (p[13] & 0xff)<<8 | (p[14] & 0xff)<<16 | (p[15] & 0xff)<<24,
      j10 = c[ 8] & 0xff | (c[ 9] & 0xff)<<8 | (c[10] & 0xff)<<16 | (c[11] & 0xff)<<24,
      j11 = k[16] & 0xff | (k[17] & 0xff)<<8 | (k[18] & 0xff)<<16 | (k[19] & 0xff)<<24,
      j12 = k[20] & 0xff | (k[21] & 0xff)<<8 | (k[22] & 0xff)<<16 | (k[23] & 0xff)<<24,
      j13 = k[24] & 0xff | (k[25] & 0xff)<<8 | (k[26] & 0xff)<<16 | (k[27] & 0xff)<<24,
      j14 = k[28] & 0xff | (k[29] & 0xff)<<8 | (k[30] & 0xff)<<16 | (k[31] & 0xff)<<24,
      j15 = c[12] & 0xff | (c[13] & 0xff)<<8 | (c[14] & 0xff)<<16 | (c[15] & 0xff)<<24;

  var x0 = j0, x1 = j1, x2 = j2, x3 = j3, x4 = j4, x5 = j5, x6 = j6, x7 = j7,
      x8 = j8, x9 = j9, x10 = j10, x11 = j11, x12 = j12, x13 = j13, x14 = j14,
      x15 = j15, u;

  for (var i = 0; i < 20; i += 2) {
    u = x0 + x12 | 0; x4 ^= u<<7 | u>>>(32-7);
    u = x4 + x0 | 0;  x8 ^= u<<9 | u>>>(32-9);
    u = x8 + x4 | 0;  x12 ^= u<<13 | u>>>(32-13);
    u = x12 + x8 | 0;  x0 ^= u<<18 | u>>>(32-18);
    u = x5 + x1 | 0;  x9 ^= u<<7 | u>>>(32-7);
    u = x9 + x5 | 0;  x13 ^= u<<9 | u>>>(32-9);
    u = x13 + x9 | 0;  x1 ^= u<<13 | u>>>(32-13);
    u = x1 + x13 | 0;  x5 ^= u<<18 | u>>>(32-18);
    u = x10 + x6 | 0;  x14 ^= u<<7 | u>>>(32-7);
    u = x14 + x10 | 0; x2 ^= u<<9 | u>>>(32-9);
    u = x2 + x14 | 0;  x6 ^= u<<13 | u>>>(32-13);
    u = x6 + x2 | 0;   x10 ^= u<<18 | u>>>(32-18);
    u = x15 + x11 | 0; x3 ^= u<<7 | u>>>(32-7);
    u = x3 + x15 | 0;  x7 ^= u<<9 | u>>>(32-9);
    u = x7 + x3 | 0;   x11 ^= u<<13 | u>>>(32-13);
    u = x11 + x7 | 0;  x15 ^= u<<18 | u>>>(32-18);
    u = x0 + x3 | 0;   x1 ^= u<<7 | u>>>(32-7);
    u = x1 + x0 | 0;   x2 ^= u<<9 | u>>>(32-9);
    u = x2 + x1 | 0;   x3 ^= u<<13 | u>>>(32-13);
    u = x3 + x2 | 0;   x0 ^= u<<18 | u>>>(32-18);
    u = x5 + x4 | 0;   x6 ^= u<<7 | u>>>(32-7);
    u = x6 + x5 | 0;   x7 ^= u<<9 | u>>>(32-9);
    u = x7 + x6 | 0;   x4 ^= u<<13 | u>>>(32-13);
    u = x4 + x7 | 0;   x5 ^= u<<18 | u>>>(32-18);
    u = x10 + x9 | 0;  x11 ^= u<<7 | u>>>(32-7);
    u = x11 + x10 | 0; x8 ^= u<<9 | u>>>(32-9);
    u = x8 + x11 | 0;  x9 ^= u<<13 | u>>>(32-13);
    u = x9 + x8 | 0;   x10 ^= u<<18 | u>>>(32-18);
    u = x15 + x14 | 0; x12 ^= u<<7 | u>>>(32-7);
    u = x12 + x15 | 0; x13 ^= u<<9 | u>>>(32-9);
    u = x13 + x12 | 0; x14 ^= u<<13 | u>>>(32-13);
    u = x14 + x13 | 0; x15 ^= u<<18 | u>>>(32-18);
  }
  x0 =  x0 +  j0 | 0; x1 =  x1 +  j1 | 0; x2 =  x2 +  j2 | 0; x3 =  x3 +  j3 | 0;
  x4 =  x4 +  j4 | 0; x5 =  x5 +  j5 | 0; x6 =  x6 +  j6 | 0; x7 =  x7 +  j7 | 0;
  x8 =  x8 +  j8 | 0; x9 =  x9 +  j9 | 0; x10 = x10 + j10 | 0; x11 = x11 + j11 | 0;
  x12 = x12 + j12 | 0; x13 = x13 + j13 | 0; x14 = x14 + j14 | 0; x15 = x15 + j15 | 0;

  o[ 0] = x0 >>>  0 & 0xff; o[ 1] = x0 >>>  8 & 0xff; o[ 2] = x0 >>> 16 & 0xff; o[ 3] = x0 >>> 24 & 0xff;
  o[ 4] = x1 >>>  0 & 0xff; o[ 5] = x1 >>>  8 & 0xff; o[ 6] = x1 >>> 16 & 0xff; o[ 7] = x1 >>> 24 & 0xff;
  o[ 8] = x2 >>>  0 & 0xff; o[ 9] = x2 >>>  8 & 0xff; o[10] = x2 >>> 16 & 0xff; o[11] = x2 >>> 24 & 0xff;
  o[12] = x3 >>>  0 & 0xff; o[13] = x3 >>>  8 & 0xff; o[14] = x3 >>> 16 & 0xff; o[15] = x3 >>> 24 & 0xff;
  o[16] = x4 >>>  0 & 0xff; o[17] = x4 >>>  8 & 0xff; o[18] = x4 >>> 16 & 0xff; o[19] = x4 >>> 24 & 0xff;
  o[20] = x5 >>>  0 & 0xff; o[21] = x5 >>>  8 & 0xff; o[22] = x5 >>> 16 & 0xff; o[23] = x5 >>> 24 & 0xff;
  o[24] = x6 >>>  0 & 0xff; o[25] = x6 >>>  8 & 0xff; o[26] = x6 >>> 16 & 0xff; o[27] = x6 >>> 24 & 0xff;
  o[28] = x7 >>>  0 & 0xff; o[29] = x7 >>>  8 & 0xff; o[30] = x7 >>> 16 & 0xff; o[31] = x7 >>> 24 & 0xff;
  o[32] = x8 >>>  0 & 0xff; o[33] = x8 >>>  8 & 0xff; o[34] = x8 >>> 16 & 0xff; o[35] = x8 >>> 24 & 0xff;
  o[36] = x9 >>>  0 & 0xff; o[37] = x9 >>>  8 & 0xff; o[38] = x9 >>> 16 & 0xff; o[39] = x9 >>> 24 & 0xff;
  o[40] = x10 >>>  0 & 0xff; o[41] = x10 >>>  8 & 0xff; o[42] = x10 >>> 16 & 0xff; o[43] = x10 >>> 24 & 0xff;
  o[44] = x11 >>>  0 & 0xff; o[45] = x11 >>>  8 & 0xff; o[46] = x11 >>> 16 & 0xff; o[47] = x11 >>> 24 & 0xff;
  o[48] = x12 >>>  0 & 0xff; o[49] = x12 >>>  8 & 0xff; o[50] = x12 >>> 16 & 0xff; o[51] = x12 >>> 24 & 0xff;
  o[52] = x13 >>>  0 & 0xff; o[53] = x13 >>>  8 & 0xff; o[54] = x13 >>> 16 & 0xff; o[55] = x13 >>> 24 & 0xff;
  o[56] = x14 >>>  0 & 0xff; o[57] = x14 >>>  8 & 0xff; o[58] = x14 >>> 16 & 0xff; o[59] = x14 >>> 24 & 0xff;
  o[60] = x15 >>>  0 & 0xff; o[61] = x15 >>>  8 & 0xff; o[62] = x15 >>> 16 & 0xff; o[63] = x15 >>> 24 & 0xff;
}

function core_hsalsa20(o, p, k, c) {
  var j0  = c[ 0] & 0xff | (c[ 1] & 0xff)<<8 | (c[ 2] & 0xff)<<16 | (c[ 3] & 0xff)<<24,
      j1  = k[ 0] & 0xff | (k[ 1] & 0xff)<<8 | (k[ 2] & 0xff)<<16 | (k[ 3] & 0xff)<<24,
      j2  = k[ 4] & 0xff | (k[ 5] & 0xff)<<8 | (k[ 6] & 0xff)<<16 | (k[ 7] & 0xff)<<24,
      j3  = k[ 8] & 0xff | (k[ 9] & 0xff)<<8 | (k[10] & 0xff)<<16 | (k[11] & 0xff)<<24,
      j4  = k[12] & 0xff | (k[13] & 0xff)<<8 | (k[14] & 0xff)<<16 | (k[15] & 0xff)<<24,
      j5  = c[ 4] & 0xff | (c[ 5] & 0xff)<<8 | (c[ 6] & 0xff)<<16 | (c[ 7] & 0xff)<<24,
      j6  = p[ 0] & 0xff | (p[ 1] & 0xff)<<8 | (p[ 2] & 0xff)<<16 | (p[ 3] & 0xff)<<24,
      j7  = p[ 4] & 0xff | (p[ 5] & 0xff)<<8 | (p[ 6] & 0xff)<<16 | (p[ 7] & 0xff)<<24,
      j8  = p[ 8] & 0xff | (p[ 9] & 0xff)<<8 | (p[10] & 0xff)<<16 | (p[11] & 0xff)<<24,
      j9  = p[12] & 0xff | (p[13] & 0xff)<<8 | (p[14] & 0xff)<<16 | (p[15] & 0xff)<<24,
      j10 = c[ 8] & 0xff | (c[ 9] & 0xff)<<8 | (c[10] & 0xff)<<16 | (c[11] & 0xff)<<24,
      j11 = k[16] & 0xff | (k[17] & 0xff)<<8 | (k[18] & 0xff)<<16 | (k[19] & 0xff)<<24,
      j12 = k[20] & 0xff | (k[21] & 0xff)<<8 | (k[22] & 0xff)<<16 | (k[23] & 0xff)<<24,
      j13 = k[24] & 0xff | (k[25] & 0xff)<<8 | (k[26] & 0xff)<<16 | (k[27] & 0xff)<<24,
      j14 = k[28] & 0xff | (k[29] & 0xff)<<8 | (k[30] & 0xff)<<16 | (k[31] & 0xff)<<24,
      j15 = c[12] & 0xff | (c[13] & 0xff)<<8 | (c[14] & 0xff)<<16 | (c[15] & 0xff)<<24;

  var x0 = j0, x1 = j1, x2 = j2, x3 = j3, x4 = j4, x5 = j5, x6 = j6, x7 = j7,
      x8 = j8, x9 = j9, x10 = j10, x11 = j11, x12 = j12, x13 = j13, x14 = j14,
      x15 = j15, u;

  for (var i = 0; i < 20; i += 2) {
    u = x0 + x12 | 0; x4 ^= u<<7 | u>>>(32-7);
    u = x4 + x0 | 0;  x8 ^= u<<9 | u>>>(32-9);
    u = x8 + x4 | 0;  x12 ^= u<<13 | u>>>(32-13);
    u = x12 + x8 | 0;  x0 ^= u<<18 | u>>>(32-18);
    u = x5 + x1 | 0;  x9 ^= u<<7 | u>>>(32-7);
    u = x9 + x5 | 0;  x13 ^= u<<9 | u>>>(32-9);
    u = x13 + x9 | 0;  x1 ^= u<<13 | u>>>(32-13);
    u = x1 + x13 | 0;  x5 ^= u<<18 | u>>>(32-18);
    u = x10 + x6 | 0;  x14 ^= u<<7 | u>>>(32-7);
    u = x14 + x10 | 0; x2 ^= u<<9 | u>>>(32-9);
    u = x2 + x14 | 0;  x6 ^= u<<13 | u>>>(32-13);
    u = x6 + x2 | 0;   x10 ^= u<<18 | u>>>(32-18);
    u = x15 + x11 | 0; x3 ^= u<<7 | u>>>(32-7);
    u = x3 + x15 | 0;  x7 ^= u<<9 | u>>>(32-9);
    u = x7 + x3 | 0;   x11 ^= u<<13 | u>>>(32-13);
    u = x11 + x7 | 0;  x15 ^= u<<18 | u>>>(32-18);
    u = x0 + x3 | 0;   x1 ^= u<<7 | u>>>(32-7);
    u = x1 + x0 | 0;   x2 ^= u<<9 | u>>>(32-9);
    u = x2 + x1 | 0;   x3 ^= u<<13 | u>>>(32-13);
    u = x3 + x2 | 0;   x0 ^= u<<18 | u>>>(32-18);
    u = x5 + x4 | 0;   x6 ^= u<<7 | u>>>(32-7);
    u = x6 + x5 | 0;   x7 ^= u<<9 | u>>>(32-9);
    u = x7 + x6 | 0;   x4 ^= u<<13 | u>>>(32-13);
    u = x4 + x7 | 0;   x5 ^= u<<18 | u>>>(32-18);
    u = x10 + x9 | 0;  x11 ^= u<<7 | u>>>(32-7);
    u = x11 + x10 | 0; x8 ^= u<<9 | u>>>(32-9);
    u = x8 + x11 | 0;  x9 ^= u<<13 | u>>>(32-13);
    u = x9 + x8 | 0;   x10 ^= u<<18 | u>>>(32-18);
    u = x15 + x14 | 0; x12 ^= u<<7 | u>>>(32-7);
    u = x12 + x15 | 0; x13 ^= u<<9 | u>>>(32-9);
    u = x13 + x12 | 0; x14 ^= u<<13 | u>>>(32-13);
    u = x14 + x13 | 0; x15 ^= u<<18 | u>>>(32-18);
  }

  o[ 0] = x0 >>>  0 & 0xff; o[ 1] = x0 >>>  8 & 0xff; o[ 2] = x0 >>> 16 & 0xff; o[ 3] = x0 >>> 24 & 0xff;
  o[ 4] = x5 >>>  0 & 0xff; o[ 5] = x5 >>>  8 & 0xff; o[ 6] = x5 >>> 16 & 0xff; o[ 7] = x5 >>> 24 & 0xff;
  o[ 8] = x10 >>>  0 & 0xff; o[ 9] = x10 >>>  8 & 0xff; o[10] = x10 >>> 16 & 0xff; o[11] = x10 >>> 24 & 0xff;
  o[12] = x15 >>>  0 & 0xff; o[13] = x15 >>>  8 & 0xff; o[14] = x15 >>> 16 & 0xff; o[15] = x15 >>> 24 & 0xff;
  o[16] = x6 >>>  0 & 0xff; o[17] = x6 >>>  8 & 0xff; o[18] = x6 >>> 16 & 0xff; o[19] = x6 >>> 24 & 0xff;
  o[20] = x7 >>>  0 & 0xff; o[21] = x7 >>>  8 & 0xff; o[22] = x7 >>> 16 & 0xff; o[23] = x7 >>> 24 & 0xff;
  o[24] = x8 >>>  0 & 0xff; o[25] = x8 >>>  8 & 0xff; o[26] = x8 >>> 16 & 0xff; o[27] = x8 >>> 24 & 0xff;
  o[28] = x9 >>>  0 & 0xff; o[29] = x9 >>>  8 & 0xff; o[30] = x9 >>> 16 & 0xff; o[31] = x9 >>> 24 & 0xff;
}

function vn(x, xi, y, yi, n) {
  var d = 0;
  for (var i = 0; i < n; i++) d |= x[xi+i]^y[yi+i];
  return (1 & ((d - 1) >>> 8)) - 1;
}

function crypto_verify_16(x, xi, y, yi) {
  return vn(x,xi,y,yi,16);
}

function crypto_verify_32(x, xi, y, yi) {
  return vn(x,xi,y,yi,32);
}

function ts64(x, i, h, l) {
  x[i]   = (h >> 24) & 0xff;
  x[i+1] = (h >> 16) & 0xff;
  x[i+2] = (h >>  8) & 0xff;
  x[i+3] = h & 0xff;
  x[i+4] = (l >> 24) & 0xff;
  x[i+5] = (l >> 16) & 0xff;
  x[i+6] = (l >>  8) & 0xff;
  x[i+7] = l & 0xff;
}

function set25519(r, a) {
  for (var i = 0; i < 16; i++) r[i] = a[i] | 0;
}

function A(o, a, b) {
  for (var i = 0; i < 16; i++) o[i] = a[i] + b[i];
}

function Z(o, a, b) {
  for (var i = 0; i < 16; i++) o[i] = a[i] - b[i];
}

function neq25519(a, b) {
  var c = new Array(32), d = new Array(32);
  pack25519(c, a);
  pack25519(d, b);
  return crypto_verify_32(c, 0, d, 0);
}

function car25519(o) {
  var i, v, c = 1;
  for (i = 0; i < 16; i++) {
    v = o[i] + c + 65535;
    c = v >>> 16;  // Changed Math.floor(v/65536) to unsigned shift
    o[i] = v - c * 65536;
  }
  o[0] += c-1 + 37 * (c-1);
}

function pack25519(o, n) {
  var i, m = new Array(16), t = new Array(16);
  for (i = 0; i < 16; i++) t[i] = n[i];
  car25519(t);
  car25519(t);
  car25519(t);
  for (var j = 0; j < 2; j++) {
    m[0] = t[0] - 0xffed;
    for (i = 1; i < 15; i++) {
      m[i] = t[i] - 0xffff - ((m[i-1]>>16) & 1);
      m[i-1] &= 0xffff;
    }
    m[15] = t[15] - 0x7fff - ((m[14]>>16) & 1);
    var b = (m[15]>>16) & 1;
    m[14] &= 0xffff;
    sel25519(t, m, 1-b);
  }
  for (i = 0; i < 16; i++) {
    o[2*i] = t[i] & 0xff;
    o[2*i+1] = t[i] >> 8;
  }
}

function sel25519(p, q, b) {
  var t, c = ~(b-1);
  for (var i = 0; i < 16; i++) {
    t = c & (p[i] ^ q[i]);
    p[i] ^= t;
    q[i] ^= t;
  }
}

function S(o, a) {
  M(o, a, a);
}

// Simplified M for i32 tier (no floating point concerns)
function M(o, a, b) {
  var v, c,
     t0 = 0,  t1 = 0,  t2 = 0,  t3 = 0,  t4 = 0,  t5 = 0,  t6 = 0,  t7 = 0,
     t8 = 0,  t9 = 0, t10 = 0, t11 = 0, t12 = 0, t13 = 0, t14 = 0, t15 = 0,
    t16 = 0, t17 = 0, t18 = 0, t19 = 0, t20 = 0, t21 = 0, t22 = 0, t23 = 0,
    t24 = 0, t25 = 0, t26 = 0, t27 = 0, t28 = 0, t29 = 0, t30 = 0,
    b0 = b[0], b1 = b[1], b2 = b[2], b3 = b[3], b4 = b[4], b5 = b[5],
    b6 = b[6], b7 = b[7], b8 = b[8], b9 = b[9], b10 = b[10], b11 = b[11],
    b12 = b[12], b13 = b[13], b14 = b[14], b15 = b[15];

  v = a[0];
  t0 += v * b0; t1 += v * b1; t2 += v * b2; t3 += v * b3;
  t4 += v * b4; t5 += v * b5; t6 += v * b6; t7 += v * b7;
  t8 += v * b8; t9 += v * b9; t10 += v * b10; t11 += v * b11;
  t12 += v * b12; t13 += v * b13; t14 += v * b14; t15 += v * b15;

  v = a[1];
  t1 += v * b0; t2 += v * b1; t3 += v * b2; t4 += v * b3;
  t5 += v * b4; t6 += v * b5; t7 += v * b6; t8 += v * b7;
  t9 += v * b8; t10 += v * b9; t11 += v * b10; t12 += v * b11;
  t13 += v * b12; t14 += v * b13; t15 += v * b14; t16 += v * b15;

  v = a[2];
  t2 += v * b0; t3 += v * b1; t4 += v * b2; t5 += v * b3;
  t6 += v * b4; t7 += v * b5; t8 += v * b6; t9 += v * b7;
  t10 += v * b8; t11 += v * b9; t12 += v * b10; t13 += v * b11;
  t14 += v * b12; t15 += v * b13; t16 += v * b14; t17 += v * b15;

  // ... (remaining a[3]-a[15] follow same pattern)
  // Reduction step
  c = t0; t0 &= 0xffff; c = (c - t0) / 65536; t1 += c;
  c = t1; t1 &= 0xffff; c = (c - t1) / 65536; t2 += c;

  return o[0] | 0; // simplified return for testing
}

// === Test Harness ===
var sigma = [101, 120, 112, 97, 110, 100, 32, 51, 50, 45, 98, 121, 116, 101, 32, 107];
var key = new Array(32);
var nonce = new Array(16);
var out = new Array(64);
for (var i = 0; i < 32; i++) key[i] = i;
for (var i = 0; i < 16; i++) nonce[i] = i * 3;

// Test core_salsa20
for (var i = 0; i < 3; i++) core_salsa20(out, nonce, key, sigma);
var start = Date.now();
for (var i = 0; i < 1000000; i++) core_salsa20(out, nonce, key, sigma);
var t1 = Date.now() - start;
var sum = 0;
for (var i = 0; i < 64; i++) sum = sum + out[i] | 0;
console.log("core_salsa20 1M: " + t1 + "ms (checksum=" + sum + ")");

// Test core_hsalsa20
var hout = new Array(32);
for (var i = 0; i < 3; i++) core_hsalsa20(hout, nonce, key, sigma);
start = Date.now();
for (var i = 0; i < 1000000; i++) core_hsalsa20(hout, nonce, key, sigma);
var t2 = Date.now() - start;
sum = 0;
for (var i = 0; i < 32; i++) sum = sum + hout[i] | 0;
console.log("core_hsalsa20 1M: " + t2 + "ms (checksum=" + sum + ")");

// Test A/Z (field add/sub)
var fa = [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16];
var fb = [16,15,14,13,12,11,10,9,8,7,6,5,4,3,2,1];
var fo = new Array(16);
for (var i = 0; i < 3; i++) A(fo, fa, fb);
start = Date.now();
for (var i = 0; i < 10000000; i++) A(fo, fa, fb);
var t3 = Date.now() - start;
console.log("A (field add) 10M: " + t3 + "ms (result=" + fo[0] + ")");

// Test vn (constant-time compare)
var va = new Array(32);
var vb = new Array(32);
for (var i = 0; i < 32; i++) { va[i] = i; vb[i] = i; }
for (var i = 0; i < 3; i++) vn(va, 0, vb, 0, 32);
start = Date.now();
for (var i = 0; i < 10000000; i++) vn(va, 0, vb, 0, 32);
var t4 = Date.now() - start;
console.log("vn (verify) 10M: " + t4 + "ms (result=" + vn(va,0,vb,0,32) + ")");
