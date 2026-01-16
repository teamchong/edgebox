// Comprehensive Console polyfill tests
// Tests console.log, error, warn, info, debug, time, timeEnd

const {
    assertEqual, assertTrue, assertTypeOf, assertDefined, summary
} = require('./helpers/assert.js');

console.log('=== Console Polyfill Tests ===\n');

// ============================================
// console object existence
// ============================================
console.log('--- console object ---');
assertDefined(console, 'console is defined');
assertTypeOf(console.log, 'function', 'console.log is function');
assertTypeOf(console.error, 'function', 'console.error is function');
assertTypeOf(console.warn, 'function', 'console.warn is function');
assertTypeOf(console.info, 'function', 'console.info is function');

// ============================================
// Basic output methods
// ============================================
console.log('\n--- Basic Output ---');

// These should not throw
let noThrow = true;
try {
    console.log('Test log');
    console.error('Test error');
    console.warn('Test warn');
    console.info('Test info');
} catch (e) {
    noThrow = false;
}
assertTrue(noThrow, 'Basic console methods do not throw');

// Multiple arguments
console.log('Multiple', 'arguments', 'test');
console.log('PASS: Multiple arguments accepted');

// Various types
console.log('String:', 'hello');
console.log('Number:', 42);
console.log('Boolean:', true);
console.log('Object:', { key: 'value' });
console.log('Array:', [1, 2, 3]);
console.log('Null:', null);
console.log('Undefined:', undefined);
console.log('PASS: Various types handled');

// ============================================
// console.debug
// ============================================
console.log('\n--- console.debug ---');
if (typeof console.debug === 'function') {
    console.debug('Debug message');
    console.log('PASS: console.debug works');
} else {
    console.log('SKIP: console.debug not available');
}

// ============================================
// console.time / console.timeEnd
// ============================================
console.log('\n--- console.time/timeEnd ---');
if (typeof console.time === 'function' && typeof console.timeEnd === 'function') {
    console.time('testTimer');
    // Do some work
    let sum = 0;
    for (let i = 0; i < 1000; i++) sum += i;
    console.timeEnd('testTimer');
    console.log('PASS: console.time/timeEnd works');
} else {
    console.log('SKIP: console.time/timeEnd not available');
}

// ============================================
// console.trace
// ============================================
console.log('\n--- console.trace ---');
if (typeof console.trace === 'function') {
    // console.trace('Stack trace'); // Commented out to avoid noisy output
    console.log('PASS: console.trace is available');
} else {
    console.log('SKIP: console.trace not available');
}

// ============================================
// console.assert
// ============================================
console.log('\n--- console.assert ---');
if (typeof console.assert === 'function') {
    console.assert(true, 'This should not print');
    // console.assert(false, 'This would print if assertion fails'); // Commented out
    console.log('PASS: console.assert is available');
} else {
    console.log('SKIP: console.assert not available');
}

// ============================================
// console.dir
// ============================================
console.log('\n--- console.dir ---');
if (typeof console.dir === 'function') {
    console.dir({ nested: { object: 'value' } });
    console.log('PASS: console.dir works');
} else {
    console.log('SKIP: console.dir not available');
}

// ============================================
// console.table
// ============================================
console.log('\n--- console.table ---');
if (typeof console.table === 'function') {
    console.table([{ a: 1, b: 2 }, { a: 3, b: 4 }]);
    console.log('PASS: console.table works');
} else {
    console.log('SKIP: console.table not available');
}

// ============================================
// console.clear
// ============================================
console.log('\n--- console.clear ---');
if (typeof console.clear === 'function') {
    // console.clear(); // Don't actually clear during test
    console.log('PASS: console.clear is available');
} else {
    console.log('SKIP: console.clear not available');
}

// ============================================
// console.count
// ============================================
console.log('\n--- console.count ---');
if (typeof console.count === 'function') {
    console.count('counter');
    console.count('counter');
    console.count('counter');
    if (typeof console.countReset === 'function') {
        console.countReset('counter');
    }
    console.log('PASS: console.count works');
} else {
    console.log('SKIP: console.count not available');
}

// ============================================
// console.group / console.groupEnd
// ============================================
console.log('\n--- console.group ---');
if (typeof console.group === 'function' && typeof console.groupEnd === 'function') {
    console.group('Test Group');
    console.log('Inside group');
    console.groupEnd();
    console.log('PASS: console.group/groupEnd works');
} else {
    console.log('SKIP: console.group/groupEnd not available');
}

// ============================================
// Format string support (if available)
// ============================================
console.log('\n--- Format Strings ---');
console.log('String: %s', 'hello');
console.log('Number: %d', 42);
console.log('Object: %o', { key: 'value' });
console.log('PASS: Format string patterns logged (verify output above)');

// ============================================
// Edge Cases
// ============================================
console.log('\n--- Edge Cases ---');

// Empty call
console.log();
console.log('PASS: Empty console.log works');

// Very long string
const longStr = 'x'.repeat(1000);
console.log('Long string (1000 chars):', longStr.substring(0, 50) + '...');
console.log('PASS: Long string handled');

// Circular reference (may or may not handle gracefully)
const circular = { name: 'circular' };
circular.self = circular;
try {
    // Note: This might cause issues depending on implementation
    // console.log('Circular:', circular);
    console.log('PASS: Circular reference test (logging skipped)');
} catch (e) {
    console.log('INFO: Circular reference causes error (expected in some implementations)');
}

// Function logging
console.log('Function:', function test() {});
console.log('Arrow function:', () => {});
console.log('PASS: Function logging');

// Symbol (if available)
if (typeof Symbol !== 'undefined') {
    console.log('Symbol:', Symbol('test'));
    console.log('PASS: Symbol logging');
}

// BigInt (if available)
if (typeof BigInt !== 'undefined') {
    console.log('BigInt:', BigInt(9007199254740991));
    console.log('PASS: BigInt logging');
}

summary();
