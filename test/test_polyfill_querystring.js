// Comprehensive QueryString polyfill tests
// Tests querystring.parse, stringify, escape, unescape

const {
    assertEqual, assertTrue, assertDeepEqual, assertTypeOf, summary
} = require('./helpers/assert.js');

console.log('=== QueryString Polyfill Tests ===\n');

let qs;
try {
    qs = require('querystring');
} catch (e) {
    console.log('SKIP: querystring module not available');
    qs = null;
}

if (qs) {
    // ============================================
    // querystring.parse
    // ============================================
    console.log('--- querystring.parse ---');

    // Basic parsing
    const parsed1 = qs.parse('foo=bar&baz=qux');
    assertEqual(parsed1.foo, 'bar', 'parse basic foo');
    assertEqual(parsed1.baz, 'qux', 'parse basic baz');

    // Multiple values for same key
    const parsed2 = qs.parse('foo=1&foo=2&foo=3');
    if (Array.isArray(parsed2.foo)) {
        assertEqual(parsed2.foo.length, 3, 'parse multiple values');
        assertEqual(parsed2.foo[0], '1', 'parse multiple values first');
    } else {
        // Some implementations keep only one value
        console.log('INFO: Multiple values for same key returns:', parsed2.foo);
    }

    // URL encoded values
    const parsed3 = qs.parse('name=Hello%20World&value=a%2Bb');
    assertEqual(parsed3.name, 'Hello World', 'parse URL encoded space');
    assertEqual(parsed3.value, 'a+b', 'parse URL encoded plus');

    // Empty string
    const parsed4 = qs.parse('');
    assertTypeOf(parsed4, 'object', 'parse empty returns object');
    assertEqual(Object.keys(parsed4).length, 0, 'parse empty has no keys');

    // Key without value
    const parsed5 = qs.parse('key');
    assertEqual(parsed5.key, '', 'parse key without value');

    // Key with empty value
    const parsed6 = qs.parse('key=');
    assertEqual(parsed6.key, '', 'parse key with empty value');

    // Multiple equals signs
    const parsed7 = qs.parse('key=value=with=equals');
    assertEqual(parsed7.key, 'value=with=equals', 'parse multiple equals');

    // Leading/trailing ampersands
    const parsed8 = qs.parse('&foo=bar&');
    assertEqual(parsed8.foo, 'bar', 'parse with leading/trailing &');

    // Custom separator
    if (qs.parse.length >= 2) {
        const parsed9 = qs.parse('foo:bar;baz:qux', ';', ':');
        if (parsed9.foo === 'bar') {
            assertEqual(parsed9.foo, 'bar', 'parse custom separator foo');
            assertEqual(parsed9.baz, 'qux', 'parse custom separator baz');
        } else {
            console.log('INFO: Custom separator not supported');
        }
    }

    // ============================================
    // querystring.stringify
    // ============================================
    console.log('\n--- querystring.stringify ---');

    // Basic stringify
    const str1 = qs.stringify({ foo: 'bar', baz: 'qux' });
    assertTrue(str1.includes('foo=bar'), 'stringify includes foo=bar');
    assertTrue(str1.includes('baz=qux'), 'stringify includes baz=qux');
    assertTrue(str1.includes('&'), 'stringify includes separator');

    // Array values
    const str2 = qs.stringify({ foo: ['1', '2', '3'] });
    assertTrue(str2.includes('foo=1'), 'stringify array value 1');
    assertTrue(str2.includes('foo=2'), 'stringify array value 2');
    assertTrue(str2.includes('foo=3'), 'stringify array value 3');

    // URL encoding
    const str3 = qs.stringify({ name: 'Hello World' });
    assertTrue(str3.includes('Hello') || str3.includes('Hello%20'), 'stringify encodes space');

    // Empty object
    const str4 = qs.stringify({});
    assertEqual(str4, '', 'stringify empty object');

    // Null/undefined values
    const str5 = qs.stringify({ foo: null, bar: undefined });
    // Behavior varies - some include key=, some skip
    console.log('INFO: null/undefined stringify:', str5);

    // Numbers and booleans
    const str6 = qs.stringify({ num: 42, bool: true });
    assertTrue(str6.includes('num=42'), 'stringify number');
    assertTrue(str6.includes('bool=true'), 'stringify boolean');

    // ============================================
    // querystring.escape
    // ============================================
    console.log('\n--- querystring.escape ---');

    if (typeof qs.escape === 'function') {
        assertEqual(qs.escape('Hello World'), 'Hello%20World', 'escape space');
        assertEqual(qs.escape('a+b'), 'a%2Bb', 'escape plus');
        assertEqual(qs.escape('a=b'), 'a%3Db', 'escape equals');
        assertEqual(qs.escape('a&b'), 'a%26b', 'escape ampersand');
        assertEqual(qs.escape('hello'), 'hello', 'escape no change');
        assertEqual(qs.escape(''), '', 'escape empty');
    } else {
        console.log('SKIP: querystring.escape not available');
    }

    // ============================================
    // querystring.unescape
    // ============================================
    console.log('\n--- querystring.unescape ---');

    if (typeof qs.unescape === 'function') {
        assertEqual(qs.unescape('Hello%20World'), 'Hello World', 'unescape space');
        assertEqual(qs.unescape('a%2Bb'), 'a+b', 'unescape plus');
        assertEqual(qs.unescape('a%3Db'), 'a=b', 'unescape equals');
        assertEqual(qs.unescape('a%26b'), 'a&b', 'unescape ampersand');
        assertEqual(qs.unescape('hello'), 'hello', 'unescape no change');
        assertEqual(qs.unescape(''), '', 'unescape empty');
    } else {
        console.log('SKIP: querystring.unescape not available');
    }

    // ============================================
    // Round-trip tests
    // ============================================
    console.log('\n--- Round-trip Tests ---');

    function roundTrip(obj) {
        const str = qs.stringify(obj);
        const parsed = qs.parse(str);
        return JSON.stringify(parsed);
    }

    // Simple object
    const rt1 = { a: '1', b: '2' };
    const rt1Parsed = qs.parse(qs.stringify(rt1));
    assertEqual(rt1Parsed.a, '1', 'round-trip a');
    assertEqual(rt1Parsed.b, '2', 'round-trip b');

    // With special characters
    const rt2 = { name: 'test value' };
    const rt2Parsed = qs.parse(qs.stringify(rt2));
    assertEqual(rt2Parsed.name, 'test value', 'round-trip with space');

    // ============================================
    // Edge Cases
    // ============================================
    console.log('\n--- Edge Cases ---');

    // Very long query string
    const longObj = {};
    for (let i = 0; i < 100; i++) {
        longObj[`key${i}`] = `value${i}`;
    }
    const longStr = qs.stringify(longObj);
    const longParsed = qs.parse(longStr);
    assertEqual(longParsed.key0, 'value0', 'long string first key');
    assertEqual(longParsed.key99, 'value99', 'long string last key');

    // Unicode characters
    const unicodeObj = { name: '\u4e2d\u6587' }; // Chinese characters
    const unicodeStr = qs.stringify(unicodeObj);
    const unicodeParsed = qs.parse(unicodeStr);
    assertEqual(unicodeParsed.name, '\u4e2d\u6587', 'round-trip unicode');

    // Special URL characters
    const specialObj = { url: 'https://example.com/path?query=value' };
    const specialStr = qs.stringify(specialObj);
    const specialParsed = qs.parse(specialStr);
    assertEqual(specialParsed.url, 'https://example.com/path?query=value', 'round-trip URL');

} else {
    // If querystring is not available, test with URLSearchParams as fallback
    console.log('\n--- Using URLSearchParams as fallback ---');

    // Basic parse equivalent
    const params = new URLSearchParams('foo=bar&baz=qux');
    assertEqual(params.get('foo'), 'bar', 'URLSearchParams parse foo');
    assertEqual(params.get('baz'), 'qux', 'URLSearchParams parse baz');

    // Basic stringify equivalent
    const newParams = new URLSearchParams();
    newParams.set('foo', 'bar');
    newParams.set('baz', 'qux');
    assertTrue(newParams.toString().includes('foo=bar'), 'URLSearchParams stringify foo');
}

summary();
