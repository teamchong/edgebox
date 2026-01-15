console.log('[DEBUG] process.argv:', process.argv);
console.log('[DEBUG] Starting tsc...');

// Global error handlers
process.on('uncaughtException', (err) => {
    console.log('[ERROR] Uncaught exception:', err.message);
    console.log(err.stack);
});

process.on('unhandledRejection', (reason, promise) => {
    console.log('[ERROR] Unhandled rejection:', reason);
});

// Make tsc more verbose
process.argv.push('--listEmittedFiles');

// Trace console.error to catch tsc errors
const originalError = console.error;
console.error = function(...args) {
    console.log('[TSC ERROR]', ...args);
    originalError.apply(this, args);
};

require('typescript/lib/tsc.js');

console.log('[DEBUG] After require tsc');
