// Debug net module

console.log('Checking _modules...');

// In the polyfill, _modules is global
if (typeof globalThis._modules !== 'undefined') {
    console.log('_modules exists');
    console.log('_modules.net:', JSON.stringify(Object.keys(globalThis._modules.net || {})));
} else {
    console.log('_modules does not exist');
}

// Check require
console.log('require:', typeof require);
const net = require('net');
console.log('net module keys:', Object.keys(net));
console.log('Full net:', JSON.stringify(net, null, 2));
