// Protobuf Varint Encode/Decode Benchmark
// Real use case: gRPC, Protocol Buffers, MessagePack, binary protocols
// Varint encoding is the foundation of protobuf wire format — every field
// header and integer value uses it. Large gRPC services decode millions/sec.
//
// Why this is an EdgeBox win:
//   - Pure integer bit manipulation (shift, AND, OR)
//   - Tight loops with conditional branches
//   - Integer overflow patterns (values > 2^31)
//   - No string/object allocation

// Encode a 32-bit integer as varint into buffer at position
// Returns new position after encoding
function varintEncode(buf, pos, value) {
  value = value | 0;
  // Handle negative values (zigzag encode)
  value = (value << 1) ^ (value >> 31);
  while (value >= 128) {
    buf[pos] = (value & 127) | 128;
    pos = pos + 1 | 0;
    value = value >>> 7;
  }
  buf[pos] = value & 127;
  return pos + 1 | 0;
}

// Decode a varint from buffer at position
// Returns decoded value (zigzag decoded)
function varintDecode(buf, pos, limit) {
  var result = 0;
  var shift = 0;
  while (pos < limit) {
    var b = buf[pos] | 0;
    pos = pos + 1 | 0;
    result = result | ((b & 127) << shift);
    if ((b & 128) === 0) {
      // Zigzag decode
      return (result >>> 1) ^ (-(result & 1));
    }
    shift = shift + 7 | 0;
  }
  return result;
}

// Decode all varints in a buffer, return sum (checksum)
function varintDecodeAll(buf, len) {
  var pos = 0;
  var sum = 0;
  while (pos < len) {
    var result = 0;
    var shift = 0;
    while (pos < len) {
      var b = buf[pos] | 0;
      pos = pos + 1 | 0;
      result = result | ((b & 127) << shift);
      if ((b & 128) === 0) {
        break;
      }
      shift = shift + 7 | 0;
    }
    // Zigzag decode
    var decoded = (result >>> 1) ^ (-(result & 1));
    sum = sum + decoded | 0;
  }
  return sum;
}

// Simulate a protobuf message: field headers + values
// Field header = (field_number << 3) | wire_type
// We encode a stream of tag+value pairs like a real protobuf message
function encodeMessage(buf, fields, nFields) {
  var pos = 0;
  for (var i = 0; i < nFields; i = i + 1 | 0) {
    var fieldNum = fields[i * 2] | 0;
    var value = fields[i * 2 + 1 | 0] | 0;
    // Encode field header (field_number << 3 | wire_type_0_varint)
    var header = (fieldNum << 3) | 0;
    pos = varintEncode(buf, pos, header);
    // Encode value
    pos = varintEncode(buf, pos, value);
  }
  return pos;
}

// Decode a protobuf message, return checksum of all field values
function decodeMessage(buf, len) {
  var pos = 0;
  var checksum = 0;
  while (pos < len) {
    // Decode field header
    var header = 0;
    var shift = 0;
    while (pos < len) {
      var b = buf[pos] | 0;
      pos = pos + 1 | 0;
      header = header | ((b & 127) << shift);
      if ((b & 128) === 0) break;
      shift = shift + 7 | 0;
    }
    var fieldNum = header >> 3;
    // Decode value (varint)
    var result = 0;
    shift = 0;
    while (pos < len) {
      var b2 = buf[pos] | 0;
      pos = pos + 1 | 0;
      result = result | ((b2 & 127) << shift);
      if ((b2 & 128) === 0) break;
      shift = shift + 7 | 0;
    }
    var value = (result >>> 1) ^ (-(result & 1));
    checksum = checksum + (fieldNum * value) | 0;
  }
  return checksum;
}

// === Generate test data ===
var N_MESSAGES = 1000;
var FIELDS_PER_MSG = 50;
var messages = [];
var seed = 42;

for (var m = 0; m < N_MESSAGES; m++) {
  var fields = new Int32Array(FIELDS_PER_MSG * 2);
  for (var f = 0; f < FIELDS_PER_MSG; f++) {
    seed = (seed * 1103515245 + 12345) & 0x7FFFFFFF;
    fields[f * 2] = 1 + (seed & 15); // field number 1-16
    seed = (seed * 1103515245 + 12345) & 0x7FFFFFFF;
    fields[f * 2 + 1] = (seed >> 4) - 0x2000000; // value range ~+-33M
  }
  // Encode to buffer
  var buf = new Int32Array(FIELDS_PER_MSG * 12); // max 6 bytes per varint * 2
  var len = encodeMessage(buf, fields, FIELDS_PER_MSG);
  messages.push({ buf: buf, len: len });
}

var totalBytes = 0;
for (var m = 0; m < messages.length; m++) totalBytes += messages[m].len;
console.log('Protobuf: ' + N_MESSAGES + ' messages, ' + FIELDS_PER_MSG + ' fields each, ' + (totalBytes / 1024 | 0) + 'KB encoded');

// Warmup
for (var w = 0; w < 100; w++) {
  var msg = messages[w % N_MESSAGES];
  decodeMessage(msg.buf, msg.len);
}

// === Benchmark: decode all messages ===
var RUNS = 100;
var t0 = Date.now();
var checksum = 0;
for (var r = 0; r < RUNS; r++) {
  for (var m = 0; m < N_MESSAGES; m++) {
    var msg = messages[m];
    checksum = checksum + decodeMessage(msg.buf, msg.len) | 0;
  }
}
var ms = Date.now() - t0;
console.log('protobuf_decode ' + (RUNS * N_MESSAGES) + 'x: ' + ms + 'ms (checksum=' + checksum + ')');

// === Benchmark: raw varint decode (no field structure) ===
var rawBuf = new Int32Array(500000);
var rawLen = 0;
seed = 42;
for (var i = 0; i < 100000; i++) {
  seed = (seed * 1103515245 + 12345) & 0x7FFFFFFF;
  rawLen = varintEncode(rawBuf, rawLen, (seed >> 4) - 0x2000000);
}
console.log('Raw varint buffer: ' + rawLen + ' ints (' + (rawLen * 4 / 1024 | 0) + 'KB)');

t0 = Date.now();
checksum = 0;
for (var r = 0; r < RUNS * 10; r++) {
  checksum = checksum + varintDecodeAll(rawBuf, rawLen) | 0;
}
ms = Date.now() - t0;
console.log('varint_decode_all ' + (RUNS * 10) + 'x (100K varints each): ' + ms + 'ms (checksum=' + checksum + ')');
