// HTTP Server Benchmark
// Run with: wrk -t4 -c100 -d10s http://localhost:8888/

var http = require('http');

var requestCount = 0;

var server = http.createServer(function(req, res) {
    requestCount++;
    res.writeHead(200, {
        'Content-Type': 'text/plain',
        'Content-Length': '13'
    });
    res.end('Hello, World!');
});

server.listen(8888, function() {
    print('[HTTP] Server listening on http://localhost:8888');
    print('[HTTP] Run: wrk -t4 -c100 -d10s http://localhost:8888/');
});

// Print stats every 5 seconds
setInterval(function() {
    print('[HTTP] Requests served: ' + requestCount);
}, 5000);
