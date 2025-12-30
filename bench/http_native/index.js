// HTTP Server Benchmark - Native Mode (kqueue/epoll event loop, 1 WASM crossing)
// Run with: wrk -t4 -c100 -d10s http://localhost:8889/

var http = require('http');

print('[HTTP] Starting native HTTP server benchmark');
print('[HTTP] Run: wrk -t4 -c100 -d10s http://localhost:8889/');

http.createNativeServer(8889, function(req, res) {
    res.writeHead(200, {
        'Content-Type': 'text/plain'
    });
    res.end('Hello, World!');
});
