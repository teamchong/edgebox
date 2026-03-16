// Minimal test: vn + crypto_verify_16 + crypto_verify_32

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

// Test
var a = new Array(32);
var b = new Array(32);
for (var i = 0; i < 32; i++) { a[i] = i; b[i] = i; }
console.log("verify_32 equal: " + crypto_verify_32(a, 0, b, 0));
b[15] = 99;
console.log("verify_32 diff: " + crypto_verify_32(a, 0, b, 0));
console.log("verify_16 equal: " + crypto_verify_16(a, 0, b, 0));

// Benchmark
b[15] = 15; // restore
var start = Date.now();
for (var i = 0; i < 10000000; i++) vn(a, 0, b, 0, 32);
var t = Date.now() - start;
console.log("vn 10M: " + t + "ms (result=" + vn(a,0,b,0,32) + ")");
