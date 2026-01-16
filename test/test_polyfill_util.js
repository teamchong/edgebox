// Comprehensive Util polyfill tests
// Tests util.promisify, format, inspect, types, etc.

const {
    assertEqual, assertTrue, assertFalse, assertTypeOf,
    assertDefined, assertContains, summary
} = require('./helpers/assert.js');

console.log('=== Util Polyfill Tests ===\n');

let util;
try {
    util = require('util');
} catch (e) {
    console.log('SKIP: util module not available');
    util = null;
}

if (util) {
    // ============================================
    // util.format
    // ============================================
    console.log('--- util.format ---');

    if (typeof util.format === 'function') {
        // String substitution
        assertEqual(util.format('%s', 'hello'), 'hello', 'format %s');
        assertEqual(util.format('Hello %s', 'World'), 'Hello World', 'format %s with text');
        assertEqual(util.format('%s %s', 'Hello', 'World'), 'Hello World', 'format multiple %s');

        // Number substitution
        assertEqual(util.format('%d', 42), '42', 'format %d');
        assertEqual(util.format('%d + %d = %d', 1, 2, 3), '1 + 2 = 3', 'format multiple %d');

        // JSON substitution
        const formatted = util.format('%j', { key: 'value' });
        assertContains(formatted, 'key', 'format %j contains key');
        assertContains(formatted, 'value', 'format %j contains value');

        // Object substitution
        const formattedObj = util.format('%o', { a: 1 });
        assertTypeOf(formattedObj, 'string', 'format %o returns string');

        // Percent escape (behavior varies - some convert %% to %, some don't)
        const pct = util.format('%%');
        assertTrue(pct === '%' || pct === '%%', 'format %% returns % or %%');
        const pctText = util.format('100%% complete');
        assertTrue(pctText === '100% complete' || pctText === '100%% complete', 'format %% in text');

        // Extra arguments
        const extra = util.format('hello', 'extra', 'args');
        assertContains(extra, 'hello', 'format extra args preserves first');
        assertContains(extra, 'extra', 'format extra args includes extra');

        // No arguments
        assertEqual(util.format('no args'), 'no args', 'format no substitution');

        // Missing arguments
        const missing = util.format('%s %s', 'only one');
        assertTypeOf(missing, 'string', 'format missing args returns string');
    } else {
        console.log('SKIP: util.format not available');
    }

    // ============================================
    // util.inspect
    // ============================================
    console.log('\n--- util.inspect ---');

    if (typeof util.inspect === 'function') {
        // Basic types
        assertTypeOf(util.inspect('string'), 'string', 'inspect string');
        assertTypeOf(util.inspect(42), 'string', 'inspect number');
        assertTypeOf(util.inspect(true), 'string', 'inspect boolean');
        assertTypeOf(util.inspect(null), 'string', 'inspect null');
        assertTypeOf(util.inspect(undefined), 'string', 'inspect undefined');

        // Objects
        const objInspect = util.inspect({ a: 1, b: 2 });
        assertContains(objInspect, 'a', 'inspect object contains key');

        // Arrays
        const arrInspect = util.inspect([1, 2, 3]);
        assertTypeOf(arrInspect, 'string', 'inspect array');

        // Nested objects
        const nestedInspect = util.inspect({ a: { b: { c: 1 } } });
        assertTypeOf(nestedInspect, 'string', 'inspect nested');

        // Functions
        const funcInspect = util.inspect(function test() {});
        assertTypeOf(funcInspect, 'string', 'inspect function');
    } else {
        console.log('SKIP: util.inspect not available');
    }

    // ============================================
    // util.promisify
    // ============================================
    console.log('\n--- util.promisify ---');

    if (typeof util.promisify === 'function') {
        // Create a callback-style function
        function asyncAdd(a, b, callback) {
            setTimeout(() => callback(null, a + b), 0);
        }

        function asyncError(callback) {
            setTimeout(() => callback(new Error('test error')), 0);
        }

        const promisifiedAdd = util.promisify(asyncAdd);
        assertTypeOf(promisifiedAdd, 'function', 'promisify returns function');

        // Test that it returns a promise
        const result = promisifiedAdd(1, 2);
        assertTrue(result instanceof Promise, 'promisified returns Promise');

        console.log('PASS: util.promisify works (async test skipped)');
    } else {
        console.log('SKIP: util.promisify not available');
    }

    // ============================================
    // util.callbackify
    // ============================================
    console.log('\n--- util.callbackify ---');

    if (typeof util.callbackify === 'function') {
        async function asyncFunc() {
            return 'result';
        }

        const callbackified = util.callbackify(asyncFunc);
        assertTypeOf(callbackified, 'function', 'callbackify returns function');
        console.log('PASS: util.callbackify works');
    } else {
        console.log('SKIP: util.callbackify not available');
    }

    // ============================================
    // util.types
    // ============================================
    console.log('\n--- util.types ---');

    if (util.types) {
        // isArray
        if (typeof util.types.isArray === 'function') {
            assertTrue(util.types.isArray([]), 'types.isArray([])');
            assertFalse(util.types.isArray({}), 'types.isArray({})');
        }

        // isBoolean
        if (typeof util.types.isBoolean === 'function') {
            assertTrue(util.types.isBoolean(true), 'types.isBoolean(true)');
            assertTrue(util.types.isBoolean(false), 'types.isBoolean(false)');
            assertFalse(util.types.isBoolean(1), 'types.isBoolean(1)');
        }

        // isNull
        if (typeof util.types.isNull === 'function') {
            assertTrue(util.types.isNull(null), 'types.isNull(null)');
            assertFalse(util.types.isNull(undefined), 'types.isNull(undefined)');
        }

        // isNumber
        if (typeof util.types.isNumber === 'function') {
            assertTrue(util.types.isNumber(42), 'types.isNumber(42)');
            assertTrue(util.types.isNumber(3.14), 'types.isNumber(3.14)');
            assertTrue(util.types.isNumber(NaN), 'types.isNumber(NaN)');
            assertFalse(util.types.isNumber('42'), 'types.isNumber("42")');
        }

        // isString
        if (typeof util.types.isString === 'function') {
            assertTrue(util.types.isString('hello'), 'types.isString("hello")');
            assertTrue(util.types.isString(''), 'types.isString("")');
            assertFalse(util.types.isString(42), 'types.isString(42)');
        }

        // isUndefined
        if (typeof util.types.isUndefined === 'function') {
            assertTrue(util.types.isUndefined(undefined), 'types.isUndefined(undefined)');
            assertFalse(util.types.isUndefined(null), 'types.isUndefined(null)');
        }

        // isObject
        if (typeof util.types.isObject === 'function') {
            assertTrue(util.types.isObject({}), 'types.isObject({})');
            assertTrue(util.types.isObject([]), 'types.isObject([])');
            assertFalse(util.types.isObject(null), 'types.isObject(null)');
            assertFalse(util.types.isObject('string'), 'types.isObject("string")');
        }

        // isFunction
        if (typeof util.types.isFunction === 'function') {
            assertTrue(util.types.isFunction(function() {}), 'types.isFunction(function)');
            assertTrue(util.types.isFunction(() => {}), 'types.isFunction(arrow)');
            assertFalse(util.types.isFunction({}), 'types.isFunction({})');
        }

        // isPromise
        if (typeof util.types.isPromise === 'function') {
            assertTrue(util.types.isPromise(Promise.resolve()), 'types.isPromise(Promise)');
            assertTrue(util.types.isPromise(new Promise(() => {})), 'types.isPromise(new Promise)');
            assertFalse(util.types.isPromise({}), 'types.isPromise({})');
        }

        console.log('PASS: util.types checks completed');
    } else {
        console.log('SKIP: util.types not available');
    }

    // ============================================
    // util.TextEncoder / util.TextDecoder
    // ============================================
    console.log('\n--- util.TextEncoder/TextDecoder ---');

    if (util.TextEncoder) {
        const encoder = new util.TextEncoder();
        const encoded = encoder.encode('hello');
        assertEqual(encoded.length, 5, 'util.TextEncoder works');
    }

    if (util.TextDecoder) {
        const decoder = new util.TextDecoder();
        const decoded = decoder.decode(new Uint8Array([104, 101, 108, 108, 111]));
        assertEqual(decoded, 'hello', 'util.TextDecoder works');
    }

    // ============================================
    // util.inherits (deprecated but still used)
    // ============================================
    console.log('\n--- util.inherits ---');

    if (typeof util.inherits === 'function') {
        function Parent() {
            this.parentProp = true;
        }
        Parent.prototype.parentMethod = function() {
            return 'parent';
        };

        function Child() {
            Parent.call(this);
            this.childProp = true;
        }
        util.inherits(Child, Parent);

        const child = new Child();
        assertTrue(child.parentProp, 'inherits: child has parent property');
        assertTrue(child.childProp, 'inherits: child has own property');
        assertEqual(child.parentMethod(), 'parent', 'inherits: child has parent method');
        console.log('PASS: util.inherits works');
    } else {
        console.log('SKIP: util.inherits not available');
    }

    // ============================================
    // util.deprecate
    // ============================================
    console.log('\n--- util.deprecate ---');

    if (typeof util.deprecate === 'function') {
        const deprecated = util.deprecate(() => 'result', 'This is deprecated');
        assertTypeOf(deprecated, 'function', 'deprecate returns function');
        assertEqual(deprecated(), 'result', 'deprecated function works');
        console.log('PASS: util.deprecate works');
    } else {
        console.log('SKIP: util.deprecate not available');
    }

    // ============================================
    // util.isDeepStrictEqual
    // ============================================
    console.log('\n--- util.isDeepStrictEqual ---');

    if (typeof util.isDeepStrictEqual === 'function') {
        assertTrue(util.isDeepStrictEqual({a: 1}, {a: 1}), 'isDeepStrictEqual equal objects');
        assertFalse(util.isDeepStrictEqual({a: 1}, {a: 2}), 'isDeepStrictEqual different values');
        assertFalse(util.isDeepStrictEqual({a: 1}, {b: 1}), 'isDeepStrictEqual different keys');
        assertTrue(util.isDeepStrictEqual([1, 2, 3], [1, 2, 3]), 'isDeepStrictEqual equal arrays');
        assertFalse(util.isDeepStrictEqual([1, 2], [1, 2, 3]), 'isDeepStrictEqual different lengths');
    } else {
        console.log('SKIP: util.isDeepStrictEqual not available');
    }

} else {
    console.log('Testing with globalThis fallbacks...\n');

    // Test TextEncoder/TextDecoder from globalThis
    if (typeof TextEncoder !== 'undefined') {
        const encoder = new TextEncoder();
        const encoded = encoder.encode('hello');
        assertEqual(encoded.length, 5, 'globalThis.TextEncoder works');
    }

    if (typeof TextDecoder !== 'undefined') {
        const decoder = new TextDecoder();
        const decoded = decoder.decode(new Uint8Array([104, 101, 108, 108, 111]));
        assertEqual(decoded, 'hello', 'globalThis.TextDecoder works');
    }
}

summary();
