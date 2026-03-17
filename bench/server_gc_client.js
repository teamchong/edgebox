// Load test client for server_gc.js
// Sends sustained HTTP requests and measures end-to-end latency.
//
// Usage: node bench/server_gc_client.js [duration_seconds] [concurrency]
//
// Measures latency distribution over time windows to detect GC pauses.

import http from 'http';

var DURATION = parseInt(process.argv[2] || '30') * 1000;
var CONCURRENCY = parseInt(process.argv[3] || '50');
var PORT = parseInt(process.env.PORT || '3456');

var latencies = [];
var windowLatencies = [];
var windowStart = Date.now();
var WINDOW_MS = 2000;
var totalSent = 0;
var totalErrors = 0;
var done = false;
var windows = [];

function sendRequest() {
  if (done) return;
  var start = Date.now();
  var req = http.request({ hostname: '127.0.0.1', port: PORT, method: 'GET', path: '/' }, function(res) {
    var body = '';
    res.on('data', function(chunk) { body += chunk; });
    res.on('end', function() {
      var elapsed = Date.now() - start;
      latencies.push(elapsed);
      windowLatencies.push(elapsed);
      totalSent++;

      // Report every window
      if (Date.now() - windowStart >= WINDOW_MS) {
        windowLatencies.sort(function(a, b) { return a - b; });
        var p50 = windowLatencies[Math.floor(windowLatencies.length * 0.5)] || 0;
        var p99 = windowLatencies[Math.floor(windowLatencies.length * 0.99)] || 0;
        var max = windowLatencies[windowLatencies.length - 1] || 0;
        var rps = windowLatencies.length / (WINDOW_MS / 1000);
        windows.push({ rps: rps, p50: p50, p99: p99, max: max, n: windowLatencies.length });
        console.log('t=' + Math.floor((Date.now() - globalStart) / 1000) + 's' +
          ' rps=' + Math.round(rps) +
          ' p50=' + p50 + 'ms p99=' + p99 + 'ms max=' + max + 'ms');
        windowLatencies = [];
        windowStart = Date.now();
      }

      if (!done) sendRequest();
    });
  });
  req.on('error', function() {
    totalErrors++;
    if (!done) sendRequest();
  });
  req.end();
}

var globalStart = Date.now();
console.log('Load test: ' + (DURATION/1000) + 's, ' + CONCURRENCY + ' concurrent, port ' + PORT);

// Launch concurrent workers
for (var i = 0; i < CONCURRENCY; i++) sendRequest();

setTimeout(function() {
  done = true;
  latencies.sort(function(a, b) { return a - b; });

  console.log('\n=== Summary ===');
  console.log('Total requests: ' + totalSent + ' (' + totalErrors + ' errors)');
  console.log('Duration: ' + (DURATION/1000) + 's');
  console.log('Avg RPS: ' + Math.round(totalSent / (DURATION/1000)));
  if (latencies.length > 0) {
    console.log('p50: ' + latencies[Math.floor(latencies.length * 0.50)] + 'ms');
    console.log('p90: ' + latencies[Math.floor(latencies.length * 0.90)] + 'ms');
    console.log('p99: ' + latencies[Math.floor(latencies.length * 0.99)] + 'ms');
    console.log('max: ' + latencies[latencies.length - 1] + 'ms');
  }

  // Check for p99 spikes (GC pauses)
  var spikeCount = 0;
  var avgP99 = 0;
  for (var i = 0; i < windows.length; i++) avgP99 += windows[i].p99;
  avgP99 = avgP99 / (windows.length || 1);
  for (var i = 0; i < windows.length; i++) {
    if (windows[i].p99 > avgP99 * 3) spikeCount++;
  }
  console.log('p99 spikes (>3x avg): ' + spikeCount + '/' + windows.length + ' windows');
  console.log('Avg p99: ' + avgP99.toFixed(1) + 'ms');

  process.exit(0);
}, DURATION);
