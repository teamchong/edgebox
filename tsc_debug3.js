// Trace every fs method
const _fs = require('fs');
const _path = require('path');

Object.keys(_fs).forEach(method => {
    const original = _fs[method];
    if (typeof original === 'function') {
        _fs[method] = function(...args) {
            // Only log write-related methods
            if (method.match(/open|write|close|create/i)) {
                console.log('[FS.' + method + ']', args[0] || '');
            }
            return original.apply(this, args);
        };
    }
});

console.log('[DEBUG] process.argv:', process.argv);

// Replace process.exit to see the exit code
const originalExit = process.exit;
let exitCalled = false;
process.exit = function(code) {
    exitCalled = true;
    console.log('[EXIT] code=' + code);
    // Let it exit
    originalExit.call(process, code);
};

require('typescript/lib/tsc.js');
console.log('[DEBUG] After tsc (exitCalled=' + exitCalled + ')');
