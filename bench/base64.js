// Base64 Encode/Decode Benchmark
// Real use case: JWT tokens, data URIs, binary-to-text in REST APIs, email MIME
// Node.js Buffer.from(str, 'base64') uses native C++, but:
//   - Cloudflare Workers don't have Buffer
//   - atob/btoa work on strings (slow for binary data)
//   - Many libraries implement their own for typed array support
//   - js-base64 (48M weekly downloads) is pure JS
//
// Why this is an EdgeBox win:
//   - Lookup table with integer indexing
//   - Bitwise shift/mask operations (6-bit to 8-bit conversion)
//   - Tight loops, no string allocation in core loop

// Decode table: base64 char code → 6-bit value
// Indexed by ASCII code (0-127), -1 for invalid
var DECODE_TABLE = new Int32Array(128);
(function() {
  for (var i = 0; i < 128; i++) DECODE_TABLE[i] = -1;
  var chars = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/';
  for (var i = 0; i < 64; i++) DECODE_TABLE[chars.charCodeAt(i)] = i;
})();

// Encode table: 6-bit value → base64 char code
var ENCODE_TABLE = new Int32Array(64);
(function() {
  var chars = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/';
  for (var i = 0; i < 64; i++) ENCODE_TABLE[i] = chars.charCodeAt(i);
})();

// Decode base64 from Int32Array of char codes to Int32Array of bytes
// Returns number of decoded bytes written to output
function base64Decode(input, inputLen, output) {
  var outPos = 0;
  var buf = 0;
  var bits = 0;

  for (var i = 0; i < inputLen; i = i + 1 | 0) {
    var c = input[i] | 0;
    if (c === 61) break; // '=' padding
    if (c < 0 || c > 127) continue;
    var val = DECODE_TABLE[c] | 0;
    if (val < 0) continue;

    buf = (buf << 6) | val;
    bits = bits + 6 | 0;

    if (bits >= 8) {
      bits = bits - 8 | 0;
      output[outPos] = (buf >> bits) & 255;
      outPos = outPos + 1 | 0;
    }
  }
  return outPos;
}

// Encode bytes (Int32Array) to base64 char codes (Int32Array)
// Returns number of output chars written
function base64Encode(input, inputLen, output) {
  var outPos = 0;
  var i = 0;

  // Process 3 bytes at a time → 4 base64 chars
  while (i + 2 < inputLen) {
    var b0 = input[i] | 0;
    var b1 = input[i + 1 | 0] | 0;
    var b2 = input[i + 2 | 0] | 0;
    output[outPos] = ENCODE_TABLE[(b0 >> 2) & 63] | 0;
    output[outPos + 1 | 0] = ENCODE_TABLE[((b0 & 3) << 4) | ((b1 >> 4) & 15)] | 0;
    output[outPos + 2 | 0] = ENCODE_TABLE[((b1 & 15) << 2) | ((b2 >> 6) & 3)] | 0;
    output[outPos + 3 | 0] = ENCODE_TABLE[b2 & 63] | 0;
    outPos = outPos + 4 | 0;
    i = i + 3 | 0;
  }

  // Handle remaining 1-2 bytes
  if (i < inputLen) {
    var b0 = input[i] | 0;
    output[outPos] = ENCODE_TABLE[(b0 >> 2) & 63] | 0;
    if (i + 1 < inputLen) {
      var b1 = input[i + 1 | 0] | 0;
      output[outPos + 1 | 0] = ENCODE_TABLE[((b0 & 3) << 4) | ((b1 >> 4) & 15)] | 0;
      output[outPos + 2 | 0] = ENCODE_TABLE[(b1 & 15) << 2] | 0;
    } else {
      output[outPos + 1 | 0] = ENCODE_TABLE[(b0 & 3) << 4] | 0;
      output[outPos + 2 | 0] = 61; // '='
    }
    output[outPos + 3 | 0] = 61; // '='
    outPos = outPos + 4 | 0;
  }

  return outPos;
}

// === Generate test data ===
var DATA_SIZE = 10000; // 10KB binary payload (realistic JWT-like size)
var N_PAYLOADS = 100;
var payloads = [];
var encoded = [];
var seed = 42;

for (var p = 0; p < N_PAYLOADS; p++) {
  var data = new Int32Array(DATA_SIZE);
  for (var j = 0; j < DATA_SIZE; j++) {
    seed = (seed * 1103515245 + 12345) & 0x7FFFFFFF;
    data[j] = seed & 255;
  }
  payloads.push(data);

  // Pre-encode for decode benchmark
  var encBuf = new Int32Array(((DATA_SIZE + 2) / 3 | 0) * 4 + 4);
  var encLen = base64Encode(data, DATA_SIZE, encBuf);
  encoded.push({ buf: encBuf, len: encLen });
}

var totalKB = N_PAYLOADS * DATA_SIZE / 1024 | 0;
console.log('Base64: ' + N_PAYLOADS + ' payloads, ' + DATA_SIZE + ' bytes each, total ' + totalKB + 'KB');

// Warmup
var tmpOut = new Int32Array(DATA_SIZE * 2);
for (var w = 0; w < 50; w++) {
  base64Decode(encoded[w % N_PAYLOADS].buf, encoded[w % N_PAYLOADS].len, tmpOut);
  base64Encode(payloads[w % N_PAYLOADS], DATA_SIZE, tmpOut);
}

// === Benchmark: decode ===
var RUNS = 100;
var t0 = Date.now();
var checksum = 0;
for (var r = 0; r < RUNS; r++) {
  for (var p = 0; p < N_PAYLOADS; p++) {
    var dec = base64Decode(encoded[p].buf, encoded[p].len, tmpOut);
    checksum = checksum + dec | 0;
  }
}
var decMs = Date.now() - t0;
console.log('base64_decode ' + (RUNS * N_PAYLOADS) + 'x (10KB each): ' + decMs + 'ms (checksum=' + checksum + ')');

// === Benchmark: encode ===
var encOut = new Int32Array(((DATA_SIZE + 2) / 3 | 0) * 4 + 4);
t0 = Date.now();
checksum = 0;
for (var r = 0; r < RUNS; r++) {
  for (var p = 0; p < N_PAYLOADS; p++) {
    var enc = base64Encode(payloads[p], DATA_SIZE, encOut);
    checksum = checksum + enc | 0;
  }
}
var encMs = Date.now() - t0;
console.log('base64_encode ' + (RUNS * N_PAYLOADS) + 'x (10KB each): ' + encMs + 'ms (checksum=' + checksum + ')');
