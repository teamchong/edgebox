// Phase 12 Test Suite

console.log('=== Phase 12 Tests ===\n');

// Test 1: process.memoryUsage on macOS
console.log('Test 1: process.memoryUsage');
try {
    const mem = process.memoryUsage();
    console.log('  rss:', mem.rss);
    console.log('  heapTotal:', mem.heapTotal);
    console.log('  heapUsed:', mem.heapUsed);
    if (mem.rss > 0) {
        console.log('  [PASS] memoryUsage returns non-zero values');
    } else {
        console.log('  [WARN] memoryUsage returns 0 (may be expected on some platforms)');
    }
} catch (e) {
    console.log('  [FAIL] Error:', e.message);
}

// Test 2: util.isDeepStrictEqual with Map
console.log('\nTest 2: util.isDeepStrictEqual with Map');
try {
    const util = require('util');
    const map1 = new Map([['a', 1], ['b', 2]]);
    const map2 = new Map([['a', 1], ['b', 2]]);
    const map3 = new Map([['a', 1], ['b', 3]]);

    if (util.isDeepStrictEqual(map1, map2)) {
        console.log('  [PASS] Equal maps are equal');
    } else {
        console.log('  [FAIL] Equal maps should be equal');
    }

    if (!util.isDeepStrictEqual(map1, map3)) {
        console.log('  [PASS] Different maps are not equal');
    } else {
        console.log('  [FAIL] Different maps should not be equal');
    }
} catch (e) {
    console.log('  [FAIL] Error:', e.message);
}

// Test 3: util.isDeepStrictEqual with Set
console.log('\nTest 3: util.isDeepStrictEqual with Set');
try {
    const util = require('util');
    const set1 = new Set([1, 2, 3]);
    const set2 = new Set([1, 2, 3]);
    const set3 = new Set([1, 2, 4]);

    if (util.isDeepStrictEqual(set1, set2)) {
        console.log('  [PASS] Equal sets are equal');
    } else {
        console.log('  [FAIL] Equal sets should be equal');
    }

    if (!util.isDeepStrictEqual(set1, set3)) {
        console.log('  [PASS] Different sets are not equal');
    } else {
        console.log('  [FAIL] Different sets should not be equal');
    }
} catch (e) {
    console.log('  [FAIL] Error:', e.message);
}

// Test 4: util.callbackify
console.log('\nTest 4: util.callbackify');
try {
    const util = require('util');

    async function asyncFunc(a, b) {
        return a + b;
    }

    const callbackified = util.callbackify(asyncFunc);

    callbackified(1, 2, function(err, result) {
        if (err) {
            console.log('  [FAIL] Error:', err.message);
        } else if (result === 3) {
            console.log('  [PASS] callbackify works correctly, result:', result);
        } else {
            console.log('  [FAIL] Expected 3, got:', result);
        }
    });
} catch (e) {
    console.log('  [FAIL] Error:', e.message);
}

// Test 5: crypto.createSecretKey
console.log('\nTest 5: crypto.createSecretKey');
try {
    const crypto = require('crypto');
    const key = crypto.createSecretKey(Buffer.from('my-secret-key'));

    if (key.type === 'secret') {
        console.log('  [PASS] createSecretKey returns secret key object');
    } else {
        console.log('  [FAIL] Expected type "secret", got:', key.type);
    }

    if (key.symmetricKeySize === 13) {
        console.log('  [PASS] symmetricKeySize is correct');
    } else {
        console.log('  [FAIL] Expected size 13, got:', key.symmetricKeySize);
    }
} catch (e) {
    console.log('  [FAIL] Error:', e.message);
}

// Test 6: HTTP addTrailers (structural test only)
console.log('\nTest 6: http.ServerResponse.addTrailers');
try {
    const http = require('http');

    // Create a mock ServerResponse to test addTrailers exists
    const res = new http.ServerResponse();
    if (typeof res.addTrailers === 'function') {
        res.addTrailers({ 'Content-MD5': 'abc123' });
        if (res._trailers && res._trailers['content-md5'] === 'abc123') {
            console.log('  [PASS] addTrailers method works');
        } else {
            console.log('  [FAIL] Trailers not stored correctly');
        }
    } else {
        console.log('  [FAIL] addTrailers method not found');
    }
} catch (e) {
    console.log('  [FAIL] Error:', e.message);
}

// Test 7: assert.deepStrictEqual with Map
console.log('\nTest 7: assert.deepStrictEqual with Map');
try {
    const assert = require('assert');
    const map1 = new Map([['key', { nested: 'value' }]]);
    const map2 = new Map([['key', { nested: 'value' }]]);

    assert.deepStrictEqual(map1, map2);
    console.log('  [PASS] deepStrictEqual works with Maps containing objects');
} catch (e) {
    console.log('  [FAIL] Error:', e.message);
}

// Test 8: IPC native functions available
console.log('\nTest 8: IPC native functions');
try {
    if (typeof __edgebox_socketpair === 'function') {
        console.log('  [PASS] __edgebox_socketpair is available');
    } else {
        console.log('  [WARN] __edgebox_socketpair not available (expected in WASI)');
    }

    if (typeof __edgebox_ipc_write === 'function') {
        console.log('  [PASS] __edgebox_ipc_write is available');
    } else {
        console.log('  [WARN] __edgebox_ipc_write not available');
    }

    if (typeof __edgebox_ipc_read === 'function') {
        console.log('  [PASS] __edgebox_ipc_read is available');
    } else {
        console.log('  [WARN] __edgebox_ipc_read not available');
    }
} catch (e) {
    console.log('  [FAIL] Error:', e.message);
}

console.log('\n=== Phase 12 Tests Complete ===');
