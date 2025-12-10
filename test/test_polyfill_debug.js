// Debug polyfill loading

console.log('=== Polyfill Debug ===');
console.log('_modules exists:', typeof globalThis._modules !== 'undefined');
console.log('_polyfillsLoaded:', globalThis._polyfillsLoaded);
console.log('_wizerInitialized:', globalThis._wizerInitialized);

if (globalThis._modules) {
    console.log('Modules registered:', Object.keys(globalThis._modules).length);
    console.log('Module names:', Object.keys(globalThis._modules).slice(0, 20).join(', '));
}

// Try requiring something to trigger polyfill load
console.log('\nTriggering require...');
try {
    const path = require('path');
    console.log('path module loaded:', typeof path);
    console.log('path.join:', typeof path.join);
} catch (e) {
    console.log('Error:', e.message);
}

console.log('\nAfter require:');
console.log('_polyfillsLoaded:', globalThis._polyfillsLoaded);
if (globalThis._modules) {
    console.log('Modules registered:', Object.keys(globalThis._modules).length);
}

// Check net specifically
console.log('\nChecking net module...');
console.log('_modules.net:', typeof globalThis._modules?.net);
if (globalThis._modules?.net) {
    console.log('net keys:', Object.keys(globalThis._modules.net));
}
