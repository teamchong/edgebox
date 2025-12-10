// HTTP server test - exit immediately after test

const http = require('http');
const net = require('net');

const server = http.createServer(function(req, res) {
    console.log('Request:', req.method, req.url);
    res.writeHead(200, { 'Content-Type': 'text/plain' });
    res.end('Hello World!');
});

server.listen(8080);

// Use a timer to wait for listen, then make request
setTimeout(function() {
    console.log('Server should be up, making request...');

    const client = net.connect({ port: 8080 }, function() {
        console.log('Connected, sending HTTP request');
        client.write('GET /test HTTP/1.1\r\nHost: localhost\r\n\r\n');
    });

    client.on('data', function(data) {
        console.log('Got response:');
        console.log(data.toString().split('\r\n').slice(0, 5).join('\n'));
        // Exit immediately
        std.exit(0);
    });

    client.on('error', function(err) {
        console.log('Error:', err);
        std.exit(1);
    });
}, 100);
