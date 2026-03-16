// Real tweetnacl benchmark — uses the actual npm package
var nacl = require('tweetnacl');

// crypto_hash (SHA-512)
var msg = new Uint8Array(64);
for (var i = 0; i < 64; i++) msg[i] = i;
var hash = nacl.hash(msg);
console.log("hash[0..3]: " + hash[0] + "," + hash[1] + "," + hash[2] + "," + hash[3]);

// crypto_verify
var a = new Uint8Array(32);
var b = new Uint8Array(32);
for (var i = 0; i < 32; i++) { a[i] = i; b[i] = i; }
console.log("verify equal: " + nacl.verify(a, b));
b[15] = 99;
console.log("verify diff: " + nacl.verify(a, b));

// Benchmark: crypto_hash (SHA-512)
var start = Date.now();
for (var i = 0; i < 100000; i++) {
  nacl.hash(msg);
}
var t = Date.now() - start;
console.log("hash 100K: " + t + "ms");

// Benchmark: secretbox (encrypt + decrypt)
var key = new Uint8Array(32);
var nonce = new Uint8Array(24);
for (var i = 0; i < 32; i++) key[i] = i;
for (var i = 0; i < 24; i++) nonce[i] = i * 2;
var plaintext = new Uint8Array(128);
for (var i = 0; i < 128; i++) plaintext[i] = i & 0xff;

start = Date.now();
for (var i = 0; i < 100000; i++) {
  var box = nacl.secretbox(plaintext, nonce, key);
}
t = Date.now() - start;
console.log("secretbox 100K: " + t + "ms (len=" + box.length + ")");
