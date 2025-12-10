// Basic net module test

const net = require('net');

console.log('net module:', typeof net);
console.log('net.Socket:', typeof net.Socket);
console.log('net.Server:', typeof net.Server);
console.log('net.createServer:', typeof net.createServer);
console.log('net.connect:', typeof net.connect);

// Test Server
console.log('\nCreating server...');
const server = net.createServer((socket) => {
    console.log('Client connected');
});
console.log('Server created:', server);
console.log('Server.listen:', typeof server.listen);
