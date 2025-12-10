// Check for polyfill errors

// Try to manually run the net module code
console.log('Testing Socket class manually...');

try {
    class EventEmitter {
        constructor() { this._events = {}; }
        on(event, listener) { (this._events[event] = this._events[event] || []).push(listener); return this; }
        emit(event, ...args) { (this._events[event] || []).forEach(fn => fn(...args)); return true; }
    }

    const SOCKET_STATE = { CREATED: 0, BOUND: 1, LISTENING: 2, CONNECTED: 3, CLOSED: 4 };

    class Socket extends EventEmitter {
        constructor(options = {}) {
            super();
            this._socketId = null;
            console.log('Socket constructor called');
        }
    }

    console.log('Socket class created:', typeof Socket);
    const s = new Socket();
    console.log('Socket instance created:', s);
} catch (e) {
    console.log('Error creating Socket:', e.message);
    console.log(e.stack);
}

// Check if __edgebox_socket functions are available in polyfill scope
console.log('\nChecking globalThis functions:');
console.log('__edgebox_socket_create:', typeof globalThis.__edgebox_socket_create);
