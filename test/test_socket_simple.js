// Simple socket test

console.log('Testing socket bindings...');

// Check if socket functions exist
console.log('__edgebox_socket_create:', typeof __edgebox_socket_create);
console.log('__edgebox_socket_bind:', typeof __edgebox_socket_bind);
console.log('__edgebox_socket_listen:', typeof __edgebox_socket_listen);
console.log('__edgebox_socket_accept:', typeof __edgebox_socket_accept);
console.log('__edgebox_socket_connect:', typeof __edgebox_socket_connect);
console.log('__edgebox_socket_write:', typeof __edgebox_socket_write);
console.log('__edgebox_socket_read:', typeof __edgebox_socket_read);
console.log('__edgebox_socket_close:', typeof __edgebox_socket_close);
console.log('__edgebox_socket_state:', typeof __edgebox_socket_state);

if (typeof __edgebox_socket_create === 'function') {
    console.log('Creating socket...');
    const id = __edgebox_socket_create();
    console.log('Socket ID:', id);
    console.log('State:', __edgebox_socket_state(id));
    __edgebox_socket_close(id);
    console.log('Socket closed');
} else {
    console.log('Socket functions not available');
}
