// HTTP Server Benchmark - Blocking Mode (high performance)
// Run with: wrk -t4 -c100 -d10s http://localhost:8888/

var http = require('http');

print('[HTTP] Starting blocking HTTP server benchmark');
print('[HTTP] Run: wrk -t4 -c100 -d10s http://localhost:8888/');

var requestCount = 0;

http.createBlockingServer(8888, function(req, res) {
    requestCount++;
    // Include counter in response to prevent static response caching
    // This ensures JS handler runs for every request (realistic benchmark)
    var body = 'Hello, World! #' + requestCount;
    res.writeHead(200, {
        'Content-Type': 'text/plain'
    });
    res.end(body);
});
