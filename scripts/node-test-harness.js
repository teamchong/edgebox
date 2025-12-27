// Node.js Core Test Harness for EdgeBox
// This module stays warm in the daemon and executes test code from files
// Usage: edgebox <harness.aot> --test-file /tmp/test.js

const fs = require('fs');
const path = require('path');

// Find test file argument
let testFile = null;
for (let i = 0; i < process.argv.length; i++) {
    if (process.argv[i] === '--test-file' && process.argv[i + 1]) {
        testFile = process.argv[i + 1];
        break;
    }
}

// Also check environment variable
if (!testFile) {
    testFile = process.env.EDGEBOX_TEST_FILE;
}

if (!testFile) {
    print('Usage: edgebox harness.aot --test-file /path/to/test.js');
    print('   or: EDGEBOX_TEST_FILE=/path/to/test.js edgebox harness.aot');
    // If no test file specified, just confirm harness is ready
    print('HARNESS_READY');
} else {
    try {
        // Read and execute test code
        const testCode = fs.readFileSync(testFile, 'utf8');

        // Create isolated scope for test
        const testFn = new Function('require', 'assert', 'print', 'process', 'Buffer', 'console', 'setTimeout', 'setInterval', 'clearTimeout', 'clearInterval', testCode);

        // Run test with globals
        testFn(
            require,
            require('assert'),
            print,
            process,
            require('buffer').Buffer,
            console,
            setTimeout,
            setInterval,
            clearTimeout,
            clearInterval
        );
    } catch (e) {
        print('FAIL: ' + (e.message || e));
        if (e.stack) {
            print(e.stack.split('\n').slice(0, 3).join('\n'));
        }
    }
}
