// Simple HTTP/2 test

const http2 = require('http2');

console.log('http2.connect:', typeof http2.connect);
console.log('http2.createServer:', typeof http2.createServer);
console.log('http2.getDefaultSettings:', typeof http2.getDefaultSettings);
console.log('http2.Http2Session:', typeof http2.Http2Session);

// Test constants
console.log('HTTP2_HEADER_METHOD:', http2.constants.HTTP2_HEADER_METHOD);
console.log('NGHTTP2_NO_ERROR:', http2.constants.NGHTTP2_NO_ERROR);

console.log('Test passed!');
