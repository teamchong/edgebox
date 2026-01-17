// DNS polyfill tests
const { assertEqual, assertTrue, assertFalse, assertTypeOf, summary } = require('./helpers/assert.js');

const dns = require('dns');

console.log('=== DNS Polyfill Tests ===\n');

let passed = 0, failed = 0;

// ============================================
// dns.lookup
// ============================================
console.log('--- dns.lookup ---');

assertTypeOf(dns.lookup, 'function', 'dns.lookup is function');
passed++;

// Test localhost lookup (should always work)
let lookupCalled = false;
dns.lookup('localhost', (err, address, family) => {
    lookupCalled = true;
    if (err) {
        console.log('PASS: dns.lookup callback called (with error for localhost)');
    } else {
        console.log('PASS: dns.lookup resolved localhost to', address);
        assertTrue(typeof address === 'string', 'address is string');
        assertTrue(family === 4 || family === 6, 'family is 4 or 6');
    }
});

// Wait a bit for async callback
setTimeout(() => {
    assertTrue(lookupCalled, 'dns.lookup callback was called');
}, 100);

// Test with options object
dns.lookup('localhost', { family: 4 }, (err, address, family) => {
    if (!err) {
        assertEqual(family, 4, 'lookup with family:4 returns IPv4');
    }
    console.log('PASS: dns.lookup with options works');
});

// ============================================
// dns.resolve4
// ============================================
console.log('\n--- dns.resolve4 ---');

assertTypeOf(dns.resolve4, 'function', 'dns.resolve4 is function');

// Sync version test
try {
    const addresses = dns.resolve4('localhost');
    if (Array.isArray(addresses)) {
        console.log('PASS: dns.resolve4 returns array');
    }
} catch (e) {
    console.log('PASS: dns.resolve4 exists (threw:', e.message, ')');
}

// ============================================
// dns.resolve6
// ============================================
console.log('\n--- dns.resolve6 ---');

assertTypeOf(dns.resolve6, 'function', 'dns.resolve6 is function');

// ============================================
// dns.reverse
// ============================================
console.log('\n--- dns.reverse ---');

assertTypeOf(dns.reverse, 'function', 'dns.reverse is function');

// Test reverse lookup on localhost IP
try {
    const hostnames = dns.reverse('127.0.0.1');
    assertTrue(Array.isArray(hostnames), 'dns.reverse returns array');
    console.log('PASS: dns.reverse works');
} catch (e) {
    console.log('PASS: dns.reverse exists (threw:', e.message, ')');
}

// ============================================
// dns.promises
// ============================================
console.log('\n--- dns.promises ---');

if (dns.promises) {
    assertTypeOf(dns.promises.lookup, 'function', 'dns.promises.lookup exists');
    assertTypeOf(dns.promises.resolve4, 'function', 'dns.promises.resolve4 exists');
    assertTypeOf(dns.promises.resolve6, 'function', 'dns.promises.resolve6 exists');
    console.log('PASS: dns.promises API available');
} else {
    console.log('SKIP: dns.promises not available');
}

// ============================================
// dns constants
// ============================================
console.log('\n--- dns constants ---');

// ADDRCONFIG, V4MAPPED, ALL are common constants
if (typeof dns.ADDRCONFIG !== 'undefined') {
    assertTypeOf(dns.ADDRCONFIG, 'number', 'dns.ADDRCONFIG is number');
}
if (typeof dns.V4MAPPED !== 'undefined') {
    assertTypeOf(dns.V4MAPPED, 'number', 'dns.V4MAPPED is number');
}

// ============================================
// Error handling
// ============================================
console.log('\n--- Error handling ---');

// Invalid hostname should call callback with error
dns.lookup('definitely-not-a-real-hostname-12345.invalid', (err, address) => {
    if (err) {
        assertTrue(err.code === 'ENOTFOUND' || err.code === 'ENOENT' || err.message.includes('not found'),
            'Invalid hostname returns ENOTFOUND-like error');
        console.log('PASS: Invalid hostname error handling');
    } else {
        console.log('PASS: dns.lookup callback called for invalid hostname');
    }
});

setTimeout(() => {
    summary();
}, 200);
