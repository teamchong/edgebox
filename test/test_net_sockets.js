// Test net.Socket and net.Server implementation

const net = require('net');

// Test 1: Create server
console.log('Test 1: Creating server...');
const server = net.createServer((socket) => {
    console.log('Server: Client connected');

    socket.on('data', (data) => {
        console.log('Server: Received:', data.toString());
        socket.write('Hello from server!');
    });

    socket.on('end', () => {
        console.log('Server: Client disconnected');
    });
});

server.on('listening', () => {
    console.log('Server: Listening on port', server.address().port);

    // Test 2: Create client and connect
    console.log('Test 2: Creating client...');
    const client = net.connect({ port: 3000 }, () => {
        console.log('Client: Connected to server');
        client.write('Hello from client!');
    });

    client.on('data', (data) => {
        console.log('Client: Received:', data.toString());
        client.end();
    });

    client.on('end', () => {
        console.log('Client: Disconnected');
        server.close();
    });
});

server.on('close', () => {
    console.log('Server: Closed');
    console.log('All tests passed!');
});

server.on('error', (err) => {
    console.error('Server error:', err);
});

server.listen(3000);
