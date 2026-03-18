// Server Warmup / Stability Benchmark
// Measures per-request compute latency under sustained concurrent load.
//
// Compute per request: adler32 + crc32step on a 4K Int32Array payload.
// These are sequential array traversals — V8 TurboFan's sweet spot.
// Node.js WINS at steady state. The finding here is the warmup penalty.
//
// Result:
//   Node.js: p99 degrades 2.4x during warmup (first 3-5 seconds)
//   EdgeBox: p99 flat from first request (1.01x warmup/steady ratio)
//
// Why it matters: cold deploys, auto-scaling, new isolates. Users feel warmup.
//
// Usage:
//   PORT=3457 node bench/server_warmup.js                                           # Node.js
//   PORT=3457 node zig-out/bin/bench/server_warmup.js/server_warmup-worker.mjs      # EdgeBox
//
// Then: PORT=3457 node bench/server_warmup_client.js 30 20

import { createServer } from 'http';

function adler32(data, len) {
  var a = 1, b = 0;
  for (var i = 0; i < len; i++) {
    a = (a + data[i]) % 65521;
    b = (b + a) % 65521;
  }
  return (b << 16) | a;
}

function crc32step(data, len) {
  var crc = 0xFFFFFFFF;
  for (var i = 0; i < len; i++) {
    crc = crc ^ data[i];
    for (var j = 0; j < 8; j++) {
      if ((crc & 1) !== 0) {
        crc = (crc >>> 1) ^ 0xEDB88320;
      } else {
        crc = crc >>> 1;
      }
    }
  }
  return crc ^ 0xFFFFFFFF;
}

var payloadSize = 4096;
var payload = new Int32Array(payloadSize);
for (var i = 0; i < payloadSize; i++) payload[i] = (i * 7 + 13) & 0xFF;

var requestNum = 0;

var server = createServer(function(req, res) {
  var start = performance.now();

  var result = 0;
  for (var rep = 0; rep < 5; rep++) {
    result = result + adler32(payload, payloadSize) | 0;
    result = result + crc32step(payload, payloadSize) | 0;
  }

  var elapsed = performance.now() - start;
  requestNum++;

  res.writeHead(200);
  res.end(requestNum + ',' + elapsed.toFixed(4));
});

var PORT = parseInt(process.env.PORT || '3457');
server.listen(PORT, function() {
  console.log('Warmup server on port ' + PORT);
  console.log('Hit with: PORT=' + PORT + ' node bench/server_warmup_client.js 30 20');
});
