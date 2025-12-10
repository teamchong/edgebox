// Test HTTP/2 module

const http2 = require('http2');

console.log('http2.connect:', typeof http2.connect);
console.log('http2.createServer:', typeof http2.createServer);
console.log('http2.getDefaultSettings:', typeof http2.getDefaultSettings);

// Test getDefaultSettings
const settings = http2.getDefaultSettings();
console.log('Default settings:', JSON.stringify(settings));

// Test getPackedSettings / getUnpackedSettings
const packed = http2.getPackedSettings({ maxConcurrentStreams: 200 });
console.log('Packed settings length:', packed.length);
const unpacked = http2.getUnpackedSettings(packed);
console.log('Unpacked maxConcurrentStreams:', unpacked.maxConcurrentStreams);

// Test constants
console.log('HTTP2_HEADER_METHOD:', http2.constants.HTTP2_HEADER_METHOD);
console.log('NGHTTP2_NO_ERROR:', http2.constants.NGHTTP2_NO_ERROR);

console.log('Test passed!');
