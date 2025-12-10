// Test net.Server

const net = require('net');

console.log('Creating server...');
const server = net.createServer(function(socket) {
    console.log('Client connected');
    socket.on('data', function(data) {
        console.log('Received:', data.toString());
        socket.write('Echo: ' + data.toString());
    });
});

server.on('listening', function() {
    console.log('Server listening on port', server.address().port);

    // Connect a client
    console.log('Connecting client...');
    const client = net.connect({ port: 3000 }, function() {
        console.log('Client connected');
        client.write('Hello!');
    });

    client.on('data', function(data) {
        console.log('Client received:', data.toString());
        client.end();
    });

    client.on('end', function() {
        console.log('Client disconnected');
        server.close();
    });
});

server.on('close', function() {
    console.log('Server closed');
    console.log('Test complete!');
    // Exit explicitly to avoid the panic
    if (typeof std !== 'undefined') std.exit(0);
});

server.on('error', function(err) {
    console.error('Server error:', err);
});

console.log('Starting server on port 3000...');
server.listen(3000);
