// Simplest HTTP server test

console.log('Step 1: Requiring http...');
const http = require('http');
console.log('Step 2: http.createServer type:', typeof http.createServer);

console.log('Step 3: Creating server...');
const server = http.createServer(function(req, res) {
    console.log('Got request');
    res.end('OK');
});
console.log('Step 4: Server created');

console.log('Step 5: Adding listening handler...');
server.on('listening', function() {
    console.log('Step 6: Listening!');
});

console.log('Step 7: Calling listen...');
server.listen(8080);
console.log('Step 8: Listen called');

// Exit after a short delay
setTimeout(function() {
    console.log('Step 9: Timeout, closing server...');
    server.close();
}, 500);

console.log('Step 10: setTimeout set');
