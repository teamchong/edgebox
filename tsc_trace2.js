// Comprehensive fs tracing
const _fs = require('fs');

// Trace all fs methods
['writeFileSync', 'writeFile', 'createWriteStream', 'openSync', 'open', 
 'writeSync', 'write', 'closeSync', 'close', 'appendFileSync', 'appendFile'].forEach(method => {
    const original = _fs[method];
    if (original) {
        _fs[method] = function(...args) {
            console.log('[TRACE fs.' + method + ']', args[0]);
            return original.apply(this, args);
        };
    }
});

// Trace fs/promises
if (_fs.promises) {
    ['writeFile', 'appendFile', 'open'].forEach(method => {
        const original = _fs.promises[method];
        if (original) {
            _fs.promises[method] = async function(...args) {
                console.log('[TRACE fs.promises.' + method + ']', args[0]);
                return original.apply(this, args);
            };
        }
    });
}

// Check what TypeScript's sys.writeFile does
console.log('[TRACE] Starting tsc...');

// Now load tsc
require('typescript/lib/tsc.js');
