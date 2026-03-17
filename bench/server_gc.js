// Server GC Flatness Benchmark
// Measures p99 latency under sustained load with compute-heavy handlers.
// Tests the hypothesis: EdgeBox WASM arena eliminates GC pauses.
//
// Run two ways:
//   PORT=3456 node bench/server_gc.js                     # Stock Node.js
//   PORT=3456 node zig-out/bin/bench/server_gc.js/server_gc-worker.mjs  # EdgeBox
//
// Then hit with: PORT=3456 node bench/server_gc_client.js

import { createServer } from 'http';

// === Compute kernels (pure numeric — EdgeBox compiles to WASM) ===
function adler32(data, len) {
  var a = 1, b = 0;
  for (var i = 0; i < len; i++) {
    a = (a + data[i]) % 65521;
    b = (b + a) % 65521;
  }
  return (b << 16) | a;
}

function arrayStats(data) {
  var sum = 0, min = data[0], max = data[0];
  for (var i = 0; i < data.length; i++) {
    sum = sum + data[i] | 0;
    if (data[i] < min) min = data[i];
    if (data[i] > max) max = data[i];
  }
  return sum + min + max | 0;
}

// === Stats tracking ===
var windowStart = Date.now();
var WINDOW_MS = 1000;
var windowLatencies = [];
var totalRequests = 0;

// Heavier payload — 10K elements, enough that adler32 + arrayStats takes real CPU
var payloadSize = 10000;
var payload = new Int32Array(payloadSize);
for (var i = 0; i < payloadSize; i++) payload[i] = (i * 7 + 13) & 0xFF;

var server = createServer(function(req, res) {
  var start = performance.now();

  // Compute: run adler32 + arrayStats 10x per request (simulates real work)
  var checksum = 0;
  for (var rep = 0; rep < 10; rep++) {
    checksum = checksum + adler32(payload, payloadSize) | 0;
    checksum = checksum + arrayStats(payload) | 0;
  }

  var elapsed = performance.now() - start;
  windowLatencies.push(elapsed);
  totalRequests++;

  // Report every second
  if (Date.now() - windowStart >= WINDOW_MS) {
    windowLatencies.sort(function(a, b) { return a - b; });
    var p50 = windowLatencies[Math.floor(windowLatencies.length * 0.5)] || 0;
    var p99 = windowLatencies[Math.floor(windowLatencies.length * 0.99)] || 0;
    var max = windowLatencies[windowLatencies.length - 1] || 0;
    console.log('rps=' + windowLatencies.length +
      ' p50=' + p50.toFixed(2) + 'ms p99=' + p99.toFixed(2) + 'ms max=' + max.toFixed(2) + 'ms' +
      ' reqs=' + totalRequests);
    windowLatencies = [];
    windowStart = Date.now();
  }

  res.writeHead(200);
  res.end(String(checksum));
});

var PORT = parseInt(process.env.PORT || '3456');
server.listen(PORT, function() {
  console.log('Server listening on port ' + PORT);
  console.log('Hit with: PORT=' + PORT + ' node bench/server_gc_client.js');
});
