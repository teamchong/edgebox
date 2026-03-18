// VLQ (Variable-Length Quantity) Decode Benchmark
// Real use case: Source map decoding — used by every bundler, debugger, and error reporter
// The source-map npm package (90M weekly downloads) spends >60% of time in VLQ decode.
//
// Why this is an EdgeBox win:
//   - Tight loop over integer array with bitwise ops (shift, AND, OR)
//   - Integer overflow patterns (accumulated values exceed Smi range)
//   - No string/object allocation in hot path
//   - V8 TurboFan can deopt on large accumulated values
//
// VLQ encoding (base64-VLQ as used in source maps):
//   Each character encodes 6 bits. Bit 5 = continuation flag.
//   First segment: bit 0 = sign, bits 1-4 = value.
//   Subsequent segments: bits 0-4 = value.

// Pre-decoded VLQ segments as Int32Array (simulating already-parsed base64 characters)
// Each value is 0-63 (6-bit), bit 5 = continuation flag
// This isolates the pure decode arithmetic from base64 char lookup

function vlqDecode(data, pos, len) {
  var result = 0;
  var shift = 0;
  var value = 0;
  var decoded = 0;

  while (pos < len) {
    var b = data[pos] | 0;
    pos = pos + 1 | 0;

    value = value | ((b & 31) << shift);
    shift = shift + 5 | 0;

    if ((b & 32) === 0) {
      // End of segment — extract sign and accumulate
      if ((value & 1) !== 0) {
        result = result + (-(value >> 1) | 0) | 0;
      } else {
        result = result + (value >> 1) | 0;
      }
      value = 0;
      shift = 0;
      decoded = decoded + 1 | 0;
    }
  }

  return result;
}

// Decode an entire source map mapping line (semicolon-separated segments)
// Returns total of all decoded values (checksum-like)
function vlqDecodeLine(data, startPos, endPos) {
  var total = 0;
  var pos = startPos | 0;
  var end = endPos | 0;
  var shift = 0;
  var value = 0;

  while (pos < end) {
    var b = data[pos] | 0;
    pos = pos + 1 | 0;

    if (b === 64) {
      // Separator (comma equivalent) — reset for next segment
      shift = 0;
      value = 0;
      continue;
    }

    value = value | ((b & 31) << shift);
    shift = shift + 5 | 0;

    if ((b & 32) === 0) {
      if ((value & 1) !== 0) {
        total = total + (-(value >> 1) | 0) | 0;
      } else {
        total = total + (value >> 1) | 0;
      }
      value = 0;
      shift = 0;
    }
  }

  return total;
}

// Generate realistic VLQ data (simulating source map mappings)
// Real source maps have millions of segments encoding line/column deltas
function generateVlqData(numSegments, seed) {
  var data = [];
  for (var i = 0; i < numSegments; i = i + 1 | 0) {
    seed = (seed * 1103515245 + 12345) & 0x7FFFFFFF;
    // Generate a realistic delta value (-500 to 500)
    var delta = ((seed >> 4) % 1001) - 500;
    // Encode as VLQ
    var v = delta < 0 ? ((-delta) << 1) | 1 : delta << 1;
    // Emit continuation bytes
    while (v >= 32) {
      data.push((v & 31) | 32); // continuation bit set
      v = v >>> 5;
    }
    data.push(v & 31); // final byte (no continuation)

    // Add separator between segments (except last)
    if (i < numSegments - 1) {
      data.push(64); // separator
    }
  }
  return new Int32Array(data);
}

// === Generate test data ===
var N_SEGMENTS = 100000; // 100K segments per "source map line"
var N_LINES = 10;
var lines = [];
var totalInts = 0;
for (var l = 0; l < N_LINES; l++) {
  var line = generateVlqData(N_SEGMENTS, 42 + l * 997);
  lines.push(line);
  totalInts = totalInts + line.length;
}
console.log('VLQ data: ' + N_LINES + ' lines, ' + (N_SEGMENTS * N_LINES) + ' segments, ' + (totalInts * 4 / 1024 | 0) + 'KB');

// Warmup
for (var w = 0; w < 20; w++) {
  vlqDecodeLine(lines[w % N_LINES], 0, lines[w % N_LINES].length);
}

// === Benchmark: single segment decode ===
var RUNS = 50;
var t0 = Date.now();
var checksum = 0;
for (var r = 0; r < RUNS; r++) {
  for (var l = 0; l < N_LINES; l++) {
    checksum = checksum + vlqDecodeLine(lines[l], 0, lines[l].length) | 0;
  }
}
var ms = Date.now() - t0;
var totalDecodes = RUNS * N_LINES;
console.log('vlq_decode_line ' + totalDecodes + 'x (' + (N_SEGMENTS * N_LINES * RUNS / 1e6 | 0) + 'M segments): ' + ms + 'ms (checksum=' + checksum + ')');

// === Benchmark: fine-grained vlqDecode (per-segment) ===
var singleLine = lines[0];
t0 = Date.now();
checksum = 0;
for (var r = 0; r < RUNS; r++) {
  checksum = checksum + vlqDecode(singleLine, 0, singleLine.length) | 0;
}
ms = Date.now() - t0;
console.log('vlq_decode_single ' + RUNS + 'x: ' + ms + 'ms (checksum=' + checksum + ')');
