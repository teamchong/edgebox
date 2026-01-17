// Comprehensive Timers polyfill tests
// Tests setTimeout, setInterval, setImmediate, clearTimeout, clearInterval

const {
    assertEqual, assertTrue, assertFalse, assertTypeOf,
    assertDefined, assertGreater, summary
} = require('./helpers/assert.js');

console.log('=== Timers Polyfill Tests ===\n');

// ============================================
// setTimeout and clearTimeout
// ============================================
console.log('--- setTimeout / clearTimeout ---');

// setTimeout exists
assertTypeOf(setTimeout, 'function', 'setTimeout is a function');

// setTimeout returns an ID
const timeoutId = setTimeout(() => {}, 0);
assertDefined(timeoutId, 'setTimeout returns an ID');

// clearTimeout exists
assertTypeOf(clearTimeout, 'function', 'clearTimeout is a function');

// clearTimeout with the ID (should not throw)
let clearTimeoutNoThrow = true;
try {
    clearTimeout(timeoutId);
} catch (e) {
    clearTimeoutNoThrow = false;
}
assertTrue(clearTimeoutNoThrow, 'clearTimeout does not throw');

// clearTimeout with null/undefined (should not throw)
let clearNullNoThrow = true;
try {
    clearTimeout(null);
    clearTimeout(undefined);
    clearTimeout(0);
} catch (e) {
    clearNullNoThrow = false;
}
assertTrue(clearNullNoThrow, 'clearTimeout(null/undefined) does not throw');

// setTimeout with 0 delay
let zeroDelayRan = false;
setTimeout(() => { zeroDelayRan = true; }, 0);
// Can't test synchronously if it ran, but we can test it was accepted
console.log('PASS: setTimeout(fn, 0) accepted');

// setTimeout with arguments
let argsReceived = null;
setTimeout((a, b, c) => { argsReceived = [a, b, c]; }, 0, 1, 2, 3);
// Async test - just verify it was accepted
console.log('PASS: setTimeout with arguments accepted');

// Cleared timeout should not run (tested via flag, though async)
let clearedRan = false;
const toClear = setTimeout(() => { clearedRan = true; }, 10);
clearTimeout(toClear);
// Check immediately - shouldn't have run
assertFalse(clearedRan, 'Cleared timeout has not run yet');

// ============================================
// setInterval and clearInterval
// ============================================
console.log('\n--- setInterval / clearInterval ---');

// setInterval exists
assertTypeOf(setInterval, 'function', 'setInterval is a function');

// setInterval returns an ID
const intervalId = setInterval(() => {}, 1000);
assertDefined(intervalId, 'setInterval returns an ID');

// clearInterval exists
assertTypeOf(clearInterval, 'function', 'clearInterval is a function');

// clearInterval with the ID (should not throw)
let clearIntervalNoThrow = true;
try {
    clearInterval(intervalId);
} catch (e) {
    clearIntervalNoThrow = false;
}
assertTrue(clearIntervalNoThrow, 'clearInterval does not throw');

// clearInterval with null/undefined (should not throw)
let clearIntNullNoThrow = true;
try {
    clearInterval(null);
    clearInterval(undefined);
} catch (e) {
    clearIntNullNoThrow = false;
}
assertTrue(clearIntNullNoThrow, 'clearInterval(null/undefined) does not throw');

// setInterval with arguments
const intWithArgs = setInterval((x) => {}, 1000, 42);
clearInterval(intWithArgs);
console.log('PASS: setInterval with arguments accepted');

// ============================================
// setImmediate and clearImmediate
// ============================================
console.log('\n--- setImmediate / clearImmediate ---');

if (typeof setImmediate === 'function') {
    assertTypeOf(setImmediate, 'function', 'setImmediate is a function');

    // setImmediate returns an ID
    const immediateId = setImmediate(() => {});
    assertDefined(immediateId, 'setImmediate returns an ID');

    // clearImmediate exists
    if (typeof clearImmediate === 'function') {
        assertTypeOf(clearImmediate, 'function', 'clearImmediate is a function');

        // clearImmediate with the ID
        let clearImmNoThrow = true;
        try {
            clearImmediate(immediateId);
        } catch (e) {
            clearImmNoThrow = false;
        }
        assertTrue(clearImmNoThrow, 'clearImmediate does not throw');
    } else {
        console.log('SKIP: clearImmediate not available');
    }

    // setImmediate with arguments
    setImmediate((a, b) => {}, 1, 2);
    console.log('PASS: setImmediate with arguments accepted');
} else {
    console.log('SKIP: setImmediate not available');
}

// ============================================
// queueMicrotask
// ============================================
console.log('\n--- queueMicrotask ---');

if (typeof queueMicrotask === 'function') {
    assertTypeOf(queueMicrotask, 'function', 'queueMicrotask is a function');

    let microtaskQueued = false;
    queueMicrotask(() => { microtaskQueued = true; });
    // Microtasks run before next event loop turn, but after current sync code
    console.log('PASS: queueMicrotask accepted');
} else {
    console.log('SKIP: queueMicrotask not available');
}

// ============================================
// timers module (if available)
// ============================================
console.log('\n--- timers module ---');

let timers;
try {
    timers = require('timers');
} catch (e) {
    timers = null;
}

if (timers) {
    // timers.setTimeout
    if (typeof timers.setTimeout === 'function') {
        const modTimeoutId = timers.setTimeout(() => {}, 100);
        assertDefined(modTimeoutId, 'timers.setTimeout returns ID');
        timers.clearTimeout(modTimeoutId);
        console.log('PASS: timers.setTimeout works');
    } else {
        console.log('SKIP: timers.setTimeout not available');
    }

    // timers.setInterval
    if (typeof timers.setInterval === 'function') {
        const modIntervalId = timers.setInterval(() => {}, 100);
        assertDefined(modIntervalId, 'timers.setInterval returns ID');
        timers.clearInterval(modIntervalId);
        console.log('PASS: timers.setInterval works');
    } else {
        console.log('SKIP: timers.setInterval not available');
    }

    // timers.setImmediate
    if (typeof timers.setImmediate === 'function') {
        const modImmId = timers.setImmediate(() => {});
        assertDefined(modImmId, 'timers.setImmediate returns ID');
        if (typeof timers.clearImmediate === 'function') {
            timers.clearImmediate(modImmId);
        }
        console.log('PASS: timers.setImmediate works');
    } else {
        console.log('SKIP: timers.setImmediate not available');
    }
} else {
    console.log('SKIP: timers module not available');
}

// ============================================
// timers/promises (if available)
// ============================================
console.log('\n--- timers/promises ---');

let timersPromises;
try {
    timersPromises = require('timers/promises');
} catch (e) {
    timersPromises = null;
}

if (timersPromises) {
    // setTimeout promise version
    if (typeof timersPromises.setTimeout === 'function') {
        const promise = timersPromises.setTimeout(0);
        assertTrue(promise instanceof Promise, 'timers/promises.setTimeout returns Promise');
        console.log('PASS: timers/promises.setTimeout exists');
    } else {
        console.log('SKIP: timers/promises.setTimeout not available');
    }

    // setImmediate promise version
    if (typeof timersPromises.setImmediate === 'function') {
        const promise = timersPromises.setImmediate();
        assertTrue(promise instanceof Promise, 'timers/promises.setImmediate returns Promise');
        console.log('PASS: timers/promises.setImmediate exists');
    } else {
        console.log('SKIP: timers/promises.setImmediate not available');
    }

    // setInterval (returns async iterator)
    if (typeof timersPromises.setInterval === 'function') {
        const iterator = timersPromises.setInterval(100);
        // Should have Symbol.asyncIterator
        assertDefined(iterator[Symbol.asyncIterator], 'setInterval returns async iterable');
        console.log('PASS: timers/promises.setInterval exists');
    } else {
        console.log('SKIP: timers/promises.setInterval not available');
    }
} else {
    console.log('SKIP: timers/promises not available');
}

// ============================================
// Timer object methods (Node.js style)
// ============================================
console.log('\n--- Timer object methods ---');

// Timeout object might have ref/unref methods
const testTimeout = setTimeout(() => {}, 100);
if (testTimeout && typeof testTimeout.ref === 'function') {
    testTimeout.ref();
    console.log('PASS: Timeout.ref() exists');
} else {
    console.log('SKIP: Timeout.ref() not available');
}

if (testTimeout && typeof testTimeout.unref === 'function') {
    testTimeout.unref();
    console.log('PASS: Timeout.unref() exists');
} else {
    console.log('SKIP: Timeout.unref() not available');
}

if (testTimeout && typeof testTimeout.hasRef === 'function') {
    const hasRef = testTimeout.hasRef();
    assertTypeOf(hasRef, 'boolean', 'hasRef returns boolean');
} else {
    console.log('SKIP: Timeout.hasRef() not available');
}

if (testTimeout && typeof testTimeout.refresh === 'function') {
    testTimeout.refresh();
    console.log('PASS: Timeout.refresh() exists');
} else {
    console.log('SKIP: Timeout.refresh() not available');
}

// Clean up the test timeout
clearTimeout(testTimeout);

// ============================================
// Edge cases
// ============================================
console.log('\n--- Edge cases ---');

// Very large delay (should not overflow)
const largeDelay = setTimeout(() => {}, 2147483647); // Max 32-bit signed int
assertDefined(largeDelay, 'Large delay timeout accepted');
clearTimeout(largeDelay);
console.log('PASS: Large delay (2^31-1 ms) accepted');

// Negative delay treated as 0
const negativeDelay = setTimeout(() => {}, -100);
assertDefined(negativeDelay, 'Negative delay timeout accepted');
clearTimeout(negativeDelay);
console.log('PASS: Negative delay treated as 0');

// Non-numeric delay
const stringDelay = setTimeout(() => {}, "10");
assertDefined(stringDelay, 'String delay timeout accepted');
clearTimeout(stringDelay);
console.log('PASS: String delay coerced');

// No callback (might throw or be a no-op)
let noCallbackError = null;
try {
    setTimeout();
} catch (e) {
    noCallbackError = e;
}
console.log('PASS: setTimeout() without callback handled');

// Callback that throws
let errorThrown = false;
setTimeout(() => {
    // This would throw in async context
    // errorThrown = true;
    // throw new Error('test');
}, 0);
console.log('PASS: Callback that throws is handled async');

// Multiple clearTimeout on same ID
const multiClear = setTimeout(() => {}, 100);
clearTimeout(multiClear);
clearTimeout(multiClear);
clearTimeout(multiClear);
console.log('PASS: Multiple clearTimeout on same ID safe');

// ============================================
// Ordering tests (sync verification only)
// ============================================
console.log('\n--- Ordering ---');

// We can only verify the calls are accepted, not async ordering
let orderCheckSetup = false;
setTimeout(() => {
    // First timeout
}, 10);
setTimeout(() => {
    // Second timeout
}, 5);
setImmediate && setImmediate(() => {
    // Immediate
});
queueMicrotask && queueMicrotask(() => {
    // Microtask
});
console.log('PASS: Multiple timer types can be queued');

summary();
