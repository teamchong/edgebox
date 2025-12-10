// HTTP server test with quick exit

const http = require('http');
const net = require('net');

console.log('Creating HTTP server...');

const server = http.createServer(function(req, res) {
    console.log('Request:', req.method, req.url);
    res.writeHead(200, { 'Content-Type': 'text/plain' });
    res.end('Hello World!');
});

server.on('listening', function() {
    console.log('Server listening');

    // Make a quick test request
    const client = net.connect({ port: 8080 }, function() {
        client.write('GET / HTTP/1.1\r\nHost: localhost\r\n\r\n');
    });

    var response = '';
    client.on('data', function(data) {
        response += data.toString();
    });

    client.on('end', function() {
        console.log('Response:');
        console.log(response);
        server.close();
        std.exit(0);  // Exit immediately to avoid panic
    });

    client.on('error', function(err) {
        console.error('Error:', err);
        std.exit(1);
    });
});

server.on('error', function(err) {
    console.error('Server error:', err);
    std.exit(1);
});

server.listen(8080);
