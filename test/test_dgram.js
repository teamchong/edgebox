// Test dgram module

const dgram = require('dgram');

console.log('dgram.createSocket:', typeof dgram.createSocket);
console.log('dgram.Socket:', typeof dgram.Socket);

const socket = dgram.createSocket('udp4');
console.log('Created socket:', typeof socket);
console.log('socket.bind:', typeof socket.bind);
console.log('socket.send:', typeof socket.send);
console.log('socket.close:', typeof socket.close);

socket.on('listening', function() {
    console.log('Socket listening on', socket.address());
});

socket.on('error', function(err) {
    console.log('Socket error:', err.message);
});

socket.bind(41234, function() {
    console.log('Bound successfully');
    socket.close();
    console.log('Test passed!');
});
