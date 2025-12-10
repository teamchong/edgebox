// List all registered modules
const path = require('path');  // Trigger polyfill load

const modules = Object.keys(globalThis._modules).sort();
console.log('Registered modules:', modules.length);
console.log(modules.join(', '));

// Check specific modules
console.log('\ncluster:', typeof globalThis._modules.cluster);
console.log('worker_threads:', typeof globalThis._modules.worker_threads);
