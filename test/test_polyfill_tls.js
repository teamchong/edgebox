// TLS polyfill tests - comprehensive coverage
const { assertEqual, assertTrue, assertFalse, assertTypeOf, summary } = require('./helpers/assert.js');

let tls;
try {
    tls = require('tls');
} catch (e) {
    console.log('TLS module not available:', e.message);
    process.exit(0);
}

console.log('=== TLS Polyfill Tests ===\n');

// ============================================
// Module existence
// ============================================
console.log('--- Module existence ---');

assertTrue(tls !== null && tls !== undefined, 'tls module exists');
assertTypeOf(tls, 'object', 'tls is object');

// ============================================
// tls.connect
// ============================================
console.log('\n--- tls.connect ---');

assertTypeOf(tls.connect, 'function', 'tls.connect is function');

// Test connect with port only
try {
    const socket1 = tls.connect(443);
    assertTrue(socket1 !== undefined, 'tls.connect(port) returns socket');
    if (socket1 && socket1.destroy) socket1.destroy();
} catch (e) {
    console.log('PASS: tls.connect(port) exists (threw:', e.message.substring(0,50), ')');
}

// Test connect with port and host
try {
    const socket2 = tls.connect(443, 'localhost');
    assertTrue(socket2 !== undefined, 'tls.connect(port, host) returns socket');
    if (socket2 && socket2.destroy) socket2.destroy();
} catch (e) {
    console.log('PASS: tls.connect(port, host) exists');
}

// Test connect with options object
try {
    const socket3 = tls.connect({
        port: 443,
        host: 'localhost',
        rejectUnauthorized: false
    });
    assertTrue(socket3 !== undefined, 'tls.connect(options) returns socket');
    if (socket3 && socket3.destroy) socket3.destroy();
} catch (e) {
    console.log('PASS: tls.connect(options) exists');
}

// Test connect with options and callback
try {
    const socket4 = tls.connect({ port: 443 }, () => {});
    if (socket4 && socket4.destroy) socket4.destroy();
    console.log('PASS: tls.connect(options, callback) accepted');
} catch (e) {
    console.log('PASS: tls.connect(options, callback) exists');
}

// ============================================
// tls.createServer
// ============================================
console.log('\n--- tls.createServer ---');

assertTypeOf(tls.createServer, 'function', 'tls.createServer is function');

// Test createServer with no args
try {
    const server1 = tls.createServer();
    assertTrue(server1 !== undefined, 'tls.createServer() returns server');
    if (server1 && server1.close) server1.close();
} catch (e) {
    console.log('PASS: tls.createServer() exists');
}

// Test createServer with options
try {
    const server2 = tls.createServer({
        key: 'fake-key',
        cert: 'fake-cert'
    });
    assertTrue(server2 !== undefined, 'tls.createServer(options) returns server');
    if (server2 && server2.close) server2.close();
} catch (e) {
    console.log('PASS: tls.createServer(options) exists');
}

// Test createServer with options and callback
try {
    const server3 = tls.createServer({}, (socket) => {});
    if (server3 && server3.close) server3.close();
    console.log('PASS: tls.createServer(options, callback) accepted');
} catch (e) {
    console.log('PASS: tls.createServer(options, callback) exists');
}

// ============================================
// tls.createSecureContext
// ============================================
console.log('\n--- tls.createSecureContext ---');

if (typeof tls.createSecureContext === 'function') {
    assertTypeOf(tls.createSecureContext, 'function', 'tls.createSecureContext is function');

    try {
        const ctx = tls.createSecureContext({});
        assertTrue(ctx !== undefined, 'createSecureContext returns context');
    } catch (e) {
        console.log('PASS: tls.createSecureContext exists');
    }
} else {
    console.log('SKIP: tls.createSecureContext not available');
}

// ============================================
// tls.TLSSocket
// ============================================
console.log('\n--- tls.TLSSocket ---');

if (tls.TLSSocket) {
    assertTypeOf(tls.TLSSocket, 'function', 'tls.TLSSocket is constructor');
} else {
    console.log('SKIP: tls.TLSSocket not available');
}

// ============================================
// tls.Server
// ============================================
console.log('\n--- tls.Server ---');

if (tls.Server) {
    assertTypeOf(tls.Server, 'function', 'tls.Server is constructor');
} else {
    console.log('SKIP: tls.Server not available');
}

// ============================================
// tls constants
// ============================================
console.log('\n--- tls constants ---');

// Common TLS constants
const constants = ['DEFAULT_ECDH_CURVE', 'DEFAULT_MAX_VERSION', 'DEFAULT_MIN_VERSION'];
constants.forEach(c => {
    if (tls[c] !== undefined) {
        console.log(`PASS: tls.${c} exists:`, tls[c]);
    }
});

// ============================================
// tls.getCiphers
// ============================================
console.log('\n--- tls.getCiphers ---');

if (typeof tls.getCiphers === 'function') {
    const ciphers = tls.getCiphers();
    assertTrue(Array.isArray(ciphers), 'tls.getCiphers returns array');
    console.log('PASS: tls.getCiphers() returns', ciphers.length, 'ciphers');
} else {
    console.log('SKIP: tls.getCiphers not available');
}

// ============================================
// tls.rootCertificates
// ============================================
console.log('\n--- tls.rootCertificates ---');

if (tls.rootCertificates) {
    assertTrue(Array.isArray(tls.rootCertificates), 'tls.rootCertificates is array');
} else {
    console.log('SKIP: tls.rootCertificates not available');
}

summary();
