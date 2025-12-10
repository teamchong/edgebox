// Test net module directly after triggering polyfill load

// Trigger polyfill loading
const fs = require('fs');

// Now check net module in detail
console.log('Direct _modules.net inspection:');
const net = globalThis._modules.net;
console.log('net:', net);
console.log('typeof net:', typeof net);

if (net) {
    const keys = Object.keys(net);
    console.log('net keys:', keys);
    for (const key of keys) {
        console.log('  ' + key + ':', typeof net[key]);
    }
}

// Check if Socket class exists anywhere
console.log('\nDoes Socket exist in net?');
console.log('net.Socket:', net?.Socket);
console.log('net.Server:', net?.Server);
console.log('net.connect:', net?.connect);
console.log('net.createServer:', net?.createServer);
