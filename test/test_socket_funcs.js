// Check socket functions exist

console.log('Checking socket native functions...');
console.log('__edgebox_socket_create:', typeof globalThis.__edgebox_socket_create);
console.log('__edgebox_socket_bind:', typeof globalThis.__edgebox_socket_bind);
console.log('__edgebox_socket_listen:', typeof globalThis.__edgebox_socket_listen);
console.log('__edgebox_socket_connect:', typeof globalThis.__edgebox_socket_connect);
console.log('__edgebox_socket_write:', typeof globalThis.__edgebox_socket_write);
console.log('__edgebox_socket_read:', typeof globalThis.__edgebox_socket_read);
console.log('__edgebox_socket_close:', typeof globalThis.__edgebox_socket_close);
console.log('__edgebox_socket_state:', typeof globalThis.__edgebox_socket_state);

// Before loading polyfills
console.log('\n_polyfillsLoaded:', globalThis._polyfillsLoaded);

// Load polyfills
const path = require('path');

// After
console.log('After require, _polyfillsLoaded:', globalThis._polyfillsLoaded);
console.log('__edgebox_socket_create:', typeof globalThis.__edgebox_socket_create);
