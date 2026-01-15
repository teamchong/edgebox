console.log('[DEBUG] process.argv:', process.argv);
console.log('[DEBUG] process.cwd():', process.cwd());
console.log('[DEBUG] Starting tsc...');

// Monkey-patch process.exit to see when tsc exits
const originalExit = process.exit;
process.exit = function(code) {
    console.log('[DEBUG] process.exit called with code:', code);
    // Don't actually exit so we can see the trace
    if (code !== 0) {
        console.log('[DEBUG] Would have exited with error code:', code);
    }
    originalExit.call(process, code);
};

require('typescript/lib/tsc.js');
