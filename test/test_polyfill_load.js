// Force load polyfills and check for errors

console.log('Checking polyfill load...');

// Use any Node.js API to trigger polyfill loading
try {
    const os = require('os');
    console.log('OS module loaded:', typeof os);
    console.log('os.platform:', os.platform());
} catch (e) {
    console.log('Error loading os:', e.message);
}

// Now check net
console.log('\nChecking net after polyfill load...');
try {
    const net = require('net');
    console.log('net module:', typeof net);
    console.log('net.Socket:', typeof net.Socket);
    console.log('net.createServer:', typeof net.createServer);
    console.log('net keys:', Object.keys(net));
} catch (e) {
    console.log('Error:', e.message);
    console.log(e.stack);
}

// Check internal _modules
console.log('\n_modules.net:', typeof globalThis._modules?.net);
if (globalThis._modules?.net) {
    console.log('_modules.net keys:', Object.keys(globalThis._modules.net));
}
