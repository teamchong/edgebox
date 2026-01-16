// Reusable test assertions for polyfill tests
// Usage: const { assertEqual, assertApprox, assertThrows, assertArrayEqual, summary } = require('./helpers/assert.js');

let passed = 0;
let failed = 0;

function assertEqual(actual, expected, msg) {
    if (actual === expected) {
        console.log(`PASS: ${msg}`);
        passed++;
        return true;
    }
    console.error(`FAIL: ${msg} - expected ${expected}, got ${actual}`);
    failed++;
    return false;
}

function assertNotEqual(actual, expected, msg) {
    if (actual !== expected) {
        console.log(`PASS: ${msg}`);
        passed++;
        return true;
    }
    console.error(`FAIL: ${msg} - expected different from ${expected}, got ${actual}`);
    failed++;
    return false;
}

function assertApprox(actual, expected, epsilon, msg) {
    if (Math.abs(actual - expected) < epsilon) {
        console.log(`PASS: ${msg}`);
        passed++;
        return true;
    }
    console.error(`FAIL: ${msg} - expected ~${expected}, got ${actual} (diff: ${Math.abs(actual - expected)})`);
    failed++;
    return false;
}

function assertTrue(value, msg) {
    if (value === true) {
        console.log(`PASS: ${msg}`);
        passed++;
        return true;
    }
    console.error(`FAIL: ${msg} - expected true, got ${value}`);
    failed++;
    return false;
}

function assertFalse(value, msg) {
    if (value === false) {
        console.log(`PASS: ${msg}`);
        passed++;
        return true;
    }
    console.error(`FAIL: ${msg} - expected false, got ${value}`);
    failed++;
    return false;
}

function assertThrows(fn, msg) {
    try {
        fn();
        console.error(`FAIL: ${msg} - expected exception, none thrown`);
        failed++;
        return false;
    } catch (e) {
        console.log(`PASS: ${msg}`);
        passed++;
        return true;
    }
}

function assertDoesNotThrow(fn, msg) {
    try {
        fn();
        console.log(`PASS: ${msg}`);
        passed++;
        return true;
    } catch (e) {
        console.error(`FAIL: ${msg} - unexpected exception: ${e}`);
        failed++;
        return false;
    }
}

function assertArrayEqual(actual, expected, msg) {
    if (!Array.isArray(actual) && !(actual instanceof Uint8Array)) {
        console.error(`FAIL: ${msg} - actual is not an array`);
        failed++;
        return false;
    }
    if (actual.length !== expected.length) {
        console.error(`FAIL: ${msg} - length mismatch: ${actual.length} vs ${expected.length}`);
        failed++;
        return false;
    }
    for (let i = 0; i < actual.length; i++) {
        if (actual[i] !== expected[i]) {
            console.error(`FAIL: ${msg} - mismatch at index ${i}: ${actual[i]} vs ${expected[i]}`);
            failed++;
            return false;
        }
    }
    console.log(`PASS: ${msg}`);
    passed++;
    return true;
}

function assertDeepEqual(actual, expected, msg) {
    const actualJson = JSON.stringify(actual);
    const expectedJson = JSON.stringify(expected);
    if (actualJson === expectedJson) {
        console.log(`PASS: ${msg}`);
        passed++;
        return true;
    }
    console.error(`FAIL: ${msg} - expected ${expectedJson}, got ${actualJson}`);
    failed++;
    return false;
}

function assertTypeOf(value, type, msg) {
    if (typeof value === type) {
        console.log(`PASS: ${msg}`);
        passed++;
        return true;
    }
    console.error(`FAIL: ${msg} - expected type ${type}, got ${typeof value}`);
    failed++;
    return false;
}

function assertInstanceOf(value, constructor, msg) {
    if (value instanceof constructor) {
        console.log(`PASS: ${msg}`);
        passed++;
        return true;
    }
    console.error(`FAIL: ${msg} - not an instance of ${constructor.name}`);
    failed++;
    return false;
}

function assertNull(value, msg) {
    if (value === null) {
        console.log(`PASS: ${msg}`);
        passed++;
        return true;
    }
    console.error(`FAIL: ${msg} - expected null, got ${value}`);
    failed++;
    return false;
}

function assertUndefined(value, msg) {
    if (value === undefined) {
        console.log(`PASS: ${msg}`);
        passed++;
        return true;
    }
    console.error(`FAIL: ${msg} - expected undefined, got ${value}`);
    failed++;
    return false;
}

function assertDefined(value, msg) {
    if (value !== undefined && value !== null) {
        console.log(`PASS: ${msg}`);
        passed++;
        return true;
    }
    console.error(`FAIL: ${msg} - expected defined value, got ${value}`);
    failed++;
    return false;
}

function assertNaN(value, msg) {
    if (Number.isNaN(value)) {
        console.log(`PASS: ${msg}`);
        passed++;
        return true;
    }
    console.error(`FAIL: ${msg} - expected NaN, got ${value}`);
    failed++;
    return false;
}

function assertInfinity(value, msg) {
    if (value === Infinity) {
        console.log(`PASS: ${msg}`);
        passed++;
        return true;
    }
    console.error(`FAIL: ${msg} - expected Infinity, got ${value}`);
    failed++;
    return false;
}

function assertNegInfinity(value, msg) {
    if (value === -Infinity) {
        console.log(`PASS: ${msg}`);
        passed++;
        return true;
    }
    console.error(`FAIL: ${msg} - expected -Infinity, got ${value}`);
    failed++;
    return false;
}

function assertContains(str, substr, msg) {
    if (typeof str === 'string' && str.includes(substr)) {
        console.log(`PASS: ${msg}`);
        passed++;
        return true;
    }
    console.error(`FAIL: ${msg} - "${str}" does not contain "${substr}"`);
    failed++;
    return false;
}

function assertStartsWith(str, prefix, msg) {
    if (typeof str === 'string' && str.startsWith(prefix)) {
        console.log(`PASS: ${msg}`);
        passed++;
        return true;
    }
    console.error(`FAIL: ${msg} - "${str}" does not start with "${prefix}"`);
    failed++;
    return false;
}

function assertEndsWith(str, suffix, msg) {
    if (typeof str === 'string' && str.endsWith(suffix)) {
        console.log(`PASS: ${msg}`);
        passed++;
        return true;
    }
    console.error(`FAIL: ${msg} - "${str}" does not end with "${suffix}"`);
    failed++;
    return false;
}

function assertGreater(actual, expected, msg) {
    if (actual > expected) {
        console.log(`PASS: ${msg}`);
        passed++;
        return true;
    }
    console.error(`FAIL: ${msg} - ${actual} is not greater than ${expected}`);
    failed++;
    return false;
}

function assertLess(actual, expected, msg) {
    if (actual < expected) {
        console.log(`PASS: ${msg}`);
        passed++;
        return true;
    }
    console.error(`FAIL: ${msg} - ${actual} is not less than ${expected}`);
    failed++;
    return false;
}

function assertInRange(value, min, max, msg) {
    if (value >= min && value <= max) {
        console.log(`PASS: ${msg}`);
        passed++;
        return true;
    }
    console.error(`FAIL: ${msg} - ${value} is not in range [${min}, ${max}]`);
    failed++;
    return false;
}

function summary() {
    console.log(`\n=== SUMMARY: ${passed} passed, ${failed} failed ===`);
    if (failed > 0) {
        if (typeof process !== 'undefined' && process.exit) {
            process.exit(1);
        }
    }
    return { passed, failed };
}

function reset() {
    passed = 0;
    failed = 0;
}

// Export for module systems
if (typeof module !== 'undefined' && module.exports) {
    module.exports = {
        assertEqual,
        assertNotEqual,
        assertApprox,
        assertTrue,
        assertFalse,
        assertThrows,
        assertDoesNotThrow,
        assertArrayEqual,
        assertDeepEqual,
        assertTypeOf,
        assertInstanceOf,
        assertNull,
        assertUndefined,
        assertDefined,
        assertNaN,
        assertInfinity,
        assertNegInfinity,
        assertContains,
        assertStartsWith,
        assertEndsWith,
        assertGreater,
        assertLess,
        assertInRange,
        summary,
        reset
    };
}
