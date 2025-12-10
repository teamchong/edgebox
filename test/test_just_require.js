// Just require without using

console.log('About to require http...');
const http = require('http');
console.log('http required, type:', typeof http);
console.log('http.createServer type:', typeof http.createServer);
console.log('Done, exiting normally');
