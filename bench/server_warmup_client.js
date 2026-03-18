// Client for server_warmup.js — sustained concurrent load with per-second percentile tracking.
//
// Sends concurrent requests for DURATION seconds, reports per-second p50/p95/p99.
// Shows warmup curve AND steady-state stability — the two things that matter.
//
// Usage:
//   PORT=3457 node bench/server_warmup_client.js [duration_secs] [concurrency]
//   PORT=3457 node bench/server_warmup_client.js 30 20

import http from 'http';

var DURATION = parseInt(process.argv[2] || '30');
var CONCURRENCY = parseInt(process.argv[3] || '20');
var PORT = parseInt(process.env.PORT || '3457');

// Per-second buckets: collect server compute times
var buckets = []; // buckets[second] = [compute_ms, ...]
var startTime = Date.now();
var done = false;
var inflight = 0;
var totalRequests = 0;
var errors = 0;

function currentSecond() {
  return Math.floor((Date.now() - startTime) / 1000);
}

function percentile(sorted, p) {
  if (sorted.length === 0) return 0;
  var idx = Math.ceil(sorted.length * p / 100) - 1;
  if (idx < 0) idx = 0;
  return sorted[idx];
}

function sendRequest() {
  if (done) return;
  inflight++;

  var req = http.request({ hostname: '127.0.0.1', port: PORT, method: 'GET', path: '/' }, function(res) {
    var body = '';
    res.on('data', function(chunk) { body += chunk; });
    res.on('end', function() {
      inflight--;
      totalRequests++;
      var sec = currentSecond();
      var parts = body.split(',');
      var compute = parseFloat(parts[1]) || 0;

      if (!buckets[sec]) buckets[sec] = [];
      buckets[sec].push(compute);

      // Keep firing if still within duration
      if (!done) sendRequest();
    });
  });

  req.on('error', function() {
    inflight--;
    errors++;
    if (!done) sendRequest();
  });

  req.end();
}

function report() {
  console.log('sec,rps,p50_ms,p95_ms,p99_ms,max_ms');

  var totalP99s = [];

  for (var s = 0; s < buckets.length; s++) {
    var b = buckets[s];
    if (!b || b.length === 0) continue;
    b.sort(function(a, c) { return a - c; });
    var p50 = percentile(b, 50);
    var p95 = percentile(b, 95);
    var p99 = percentile(b, 99);
    var max = b[b.length - 1];
    totalP99s.push(p99);
    console.log(s + ',' + b.length + ',' + p50.toFixed(3) + ',' + p95.toFixed(3) + ',' + p99.toFixed(3) + ',' + max.toFixed(3));
  }

  // Summary stats
  var allCompute = [];
  for (var s = 0; s < buckets.length; s++) {
    if (buckets[s]) allCompute = allCompute.concat(buckets[s]);
  }
  allCompute.sort(function(a, c) { return a - c; });

  // Warmup phase: first 3 seconds
  var warmupCompute = [];
  for (var s = 0; s < Math.min(3, buckets.length); s++) {
    if (buckets[s]) warmupCompute = warmupCompute.concat(buckets[s]);
  }
  warmupCompute.sort(function(a, c) { return a - c; });

  // Steady state: last 10 seconds
  var steadyCompute = [];
  for (var s = Math.max(0, buckets.length - 10); s < buckets.length; s++) {
    if (buckets[s]) steadyCompute = steadyCompute.concat(buckets[s]);
  }
  steadyCompute.sort(function(a, c) { return a - c; });

  console.log('');
  console.log('total_requests: ' + totalRequests);
  console.log('errors: ' + errors);
  console.log('duration: ' + DURATION + 's');
  console.log('concurrency: ' + CONCURRENCY);
  console.log('');
  console.log('overall_p50: ' + percentile(allCompute, 50).toFixed(3) + 'ms');
  console.log('overall_p95: ' + percentile(allCompute, 95).toFixed(3) + 'ms');
  console.log('overall_p99: ' + percentile(allCompute, 99).toFixed(3) + 'ms');
  console.log('');
  console.log('warmup_p99 (first 3s): ' + percentile(warmupCompute, 99).toFixed(3) + 'ms');
  console.log('steady_p99 (last 10s): ' + percentile(steadyCompute, 99).toFixed(3) + 'ms');
  console.log('p99_ratio (warmup/steady): ' + (percentile(warmupCompute, 99) / percentile(steadyCompute, 99)).toFixed(2) + 'x');

  process.exit(0);
}

console.log('Load test: ' + DURATION + 's, ' + CONCURRENCY + ' concurrent to port ' + PORT);

// Launch concurrent workers
for (var c = 0; c < CONCURRENCY; c++) {
  sendRequest();
}

// Stop after duration
setTimeout(function() {
  done = true;
  // Wait for inflight to drain (max 2s)
  var waitStart = Date.now();
  var interval = setInterval(function() {
    if (inflight === 0 || Date.now() - waitStart > 2000) {
      clearInterval(interval);
      report();
    }
  }, 50);
}, DURATION * 1000);
