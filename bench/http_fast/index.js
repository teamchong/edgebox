// HTTP Server Benchmark - Fast Mode (batched operations, 2 crossings instead of 4)
// Run with: wrk -t4 -c100 -d10s http://localhost:8888/

var http = require('http');

print('[HTTP] Starting fast HTTP server benchmark');
print('[HTTP] Run: wrk -t4 -c100 -d10s http://localhost:8888/');

http.createFastServer(8888, function(req, res) {
    res.writeHead(200, {
        'Content-Type': 'text/plain'
    });
    res.end('Hello, World!');
});
