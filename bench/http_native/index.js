// HTTP Server Benchmark - Native Mode (kqueue/epoll event loop, 1 WASM crossing)
// Run with: wrk -t4 -c100 -d10s http://localhost:8889/

var http = require('http');

print('[HTTP] Starting native HTTP server benchmark');
print('[HTTP] Run: wrk -t4 -c100 -d10s http://localhost:8889/');

var requestCount = 0;

http.createNativeServer(8889, function(req, res) {
    requestCount++;
    // Include counter in response to prevent static response caching
    // This ensures JS handler runs for every request (realistic benchmark)
    var body = 'Hello, World! #' + requestCount;
    res.writeHead(200, {
        'Content-Type': 'text/plain'
    });
    res.end(body);
});
