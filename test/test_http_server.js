// Test http.createServer

const http = require('http');

console.log('Creating HTTP server...');

const server = http.createServer(function(req, res) {
    console.log('Request:', req.method, req.url);
    res.writeHead(200, { 'Content-Type': 'text/plain' });
    res.end('Hello from EdgeBox HTTP Server!');
});

server.on('listening', function() {
    console.log('HTTP Server listening on port', server.address().port);

    // Make a test request using net.connect
    const net = require('net');
    console.log('Making test request...');

    const client = net.connect({ port: 8080 }, function() {
        console.log('Client connected, sending request...');
        client.write('GET /test HTTP/1.1\r\nHost: localhost\r\n\r\n');
    });

    client.on('data', function(data) {
        console.log('Response received:');
        console.log(data.toString());
        client.end();
    });

    client.on('end', function() {
        console.log('Client disconnected');
        server.close();
    });

    client.on('error', function(err) {
        console.error('Client error:', err);
    });
});

server.on('close', function() {
    console.log('Server closed');
    console.log('Test complete!');
    if (typeof std !== 'undefined') std.exit(0);
});

server.on('error', function(err) {
    console.error('Server error:', err);
});

console.log('Starting server on port 8080...');
server.listen(8080);
