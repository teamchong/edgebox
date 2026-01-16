// Comprehensive URL polyfill tests
// Tests URL class and url module (parse, format, resolve)

const {
    assertEqual, assertTrue, assertFalse, assertTypeOf,
    assertDefined, assertThrows, summary
} = require('./helpers/assert.js');

console.log('=== URL Polyfill Tests ===\n');

// ============================================
// URL class - Basic parsing
// ============================================
console.log('--- URL class basic ---');

const url1 = new URL('https://example.com:8080/path/to/page?query=value#hash');
assertEqual(url1.protocol, 'https:', 'URL protocol');
assertEqual(url1.hostname, 'example.com', 'URL hostname');
assertEqual(url1.port, '8080', 'URL port');
assertEqual(url1.pathname, '/path/to/page', 'URL pathname');
assertEqual(url1.search, '?query=value', 'URL search');
assertEqual(url1.hash, '#hash', 'URL hash');
assertEqual(url1.host, 'example.com:8080', 'URL host');
assertEqual(url1.origin, 'https://example.com:8080', 'URL origin');

// Without port
const url2 = new URL('https://example.com/path');
assertEqual(url2.port, '', 'URL default port is empty');
assertEqual(url2.host, 'example.com', 'URL host without port');

// With username/password
const url3 = new URL('https://user:pass@example.com/');
assertEqual(url3.username, 'user', 'URL username');
assertEqual(url3.password, 'pass', 'URL password');

// ============================================
// URL class - Relative URLs
// ============================================
console.log('\n--- URL relative ---');

const base = 'https://example.com/a/b/c';
const rel1 = new URL('/path', base);
assertEqual(rel1.href, 'https://example.com/path', 'Relative to root');

const rel2 = new URL('path', base);
assertEqual(rel2.href, 'https://example.com/a/b/path', 'Relative to current');

const rel3 = new URL('../path', base);
assertEqual(rel3.href, 'https://example.com/a/path', 'Relative with ..');

const rel4 = new URL('?query=new', base);
assertEqual(rel4.href, 'https://example.com/a/b/c?query=new', 'Relative query');

const rel5 = new URL('#newhash', base);
assertEqual(rel5.href, 'https://example.com/a/b/c#newhash', 'Relative hash');

// ============================================
// URL class - toString and href
// ============================================
console.log('\n--- URL toString/href ---');

const url4 = new URL('https://example.com/path?query=value#hash');
assertEqual(url4.toString(), url4.href, 'toString equals href');
assertEqual(url4.toJSON(), url4.href, 'toJSON equals href');

// ============================================
// URL class - Modifying properties
// ============================================
console.log('\n--- URL modification ---');

const url5 = new URL('https://example.com/old');
url5.pathname = '/new';
assertEqual(url5.pathname, '/new', 'Modified pathname');
assertTrue(url5.href.includes('/new'), 'href includes new pathname');

url5.search = '?foo=bar';
assertEqual(url5.search, '?foo=bar', 'Modified search');

url5.hash = '#section';
assertEqual(url5.hash, '#section', 'Modified hash');

// ============================================
// URLSearchParams
// ============================================
console.log('\n--- URLSearchParams ---');

const params = new URLSearchParams('foo=1&bar=2&foo=3');

// get
assertEqual(params.get('foo'), '1', 'get first value');
assertEqual(params.get('bar'), '2', 'get single value');
assertEqual(params.get('baz'), null, 'get non-existent');

// getAll
const allFoo = params.getAll('foo');
assertTrue(Array.isArray(allFoo), 'getAll returns array');
assertEqual(allFoo.length, 2, 'getAll returns all values');
assertEqual(allFoo[0], '1', 'getAll first value');
assertEqual(allFoo[1], '3', 'getAll second value');

// has
assertTrue(params.has('foo'), 'has existing key');
assertFalse(params.has('baz'), 'has non-existent key');

// set
params.set('bar', 'new');
assertEqual(params.get('bar'), 'new', 'set overwrites value');

// append
params.append('new', 'value');
assertTrue(params.has('new'), 'append adds key');
assertEqual(params.get('new'), 'value', 'append adds value');

// delete
params.delete('new');
assertFalse(params.has('new'), 'delete removes key');

// toString
const paramsStr = params.toString();
assertTypeOf(paramsStr, 'string', 'toString returns string');
assertTrue(paramsStr.includes('foo=1'), 'toString includes foo=1');
assertTrue(paramsStr.includes('bar=new'), 'toString includes bar=new');

// ============================================
// URLSearchParams from URL
// ============================================
console.log('\n--- URLSearchParams from URL ---');

const url6 = new URL('https://example.com?a=1&b=2');
assertEqual(url6.searchParams.get('a'), '1', 'searchParams.get from URL');
assertEqual(url6.searchParams.get('b'), '2', 'searchParams.get from URL');

// Modifying searchParams affects URL
url6.searchParams.set('a', 'new');
assertTrue(url6.href.includes('a=new'), 'URL href reflects searchParams change');

// ============================================
// URLSearchParams iteration
// ============================================
console.log('\n--- URLSearchParams iteration ---');

const iterParams = new URLSearchParams('x=1&y=2&z=3');

// entries
let entryCount = 0;
for (const [key, value] of iterParams.entries()) {
    entryCount++;
    assertTypeOf(key, 'string', 'Entry key is string');
    assertTypeOf(value, 'string', 'Entry value is string');
}
assertEqual(entryCount, 3, 'entries iteration count');

// keys
let keyCount = 0;
for (const key of iterParams.keys()) {
    keyCount++;
    assertTypeOf(key, 'string', 'Key is string');
}
assertEqual(keyCount, 3, 'keys iteration count');

// values
let valueCount = 0;
for (const value of iterParams.values()) {
    valueCount++;
    assertTypeOf(value, 'string', 'Value is string');
}
assertEqual(valueCount, 3, 'values iteration count');

// forEach
let forEachCount = 0;
iterParams.forEach((value, key) => {
    forEachCount++;
});
assertEqual(forEachCount, 3, 'forEach count');

// ============================================
// url module (require('url'))
// ============================================
console.log('\n--- url module ---');

let url;
try {
    url = require('url');
} catch (e) {
    console.log('SKIP: url module not available via require');
    url = null;
}

if (url) {
    // url.parse
    if (typeof url.parse === 'function') {
        const parsed = url.parse('https://example.com:8080/path?query=value#hash');
        assertEqual(parsed.protocol, 'https:', 'url.parse protocol');
        assertEqual(parsed.hostname, 'example.com', 'url.parse hostname');
        assertEqual(parsed.port, '8080', 'url.parse port');
        assertEqual(parsed.pathname, '/path', 'url.parse pathname');
        assertEqual(parsed.search, '?query=value', 'url.parse search');
        assertEqual(parsed.hash, '#hash', 'url.parse hash');
    }

    // url.format
    if (typeof url.format === 'function') {
        const formatted = url.format({
            protocol: 'https:',
            hostname: 'example.com',
            port: '8080',
            pathname: '/path',
            search: '?query=value'
        });
        assertTrue(formatted.includes('https://'), 'url.format includes protocol');
        assertTrue(formatted.includes('example.com'), 'url.format includes hostname');
        assertTrue(formatted.includes('/path'), 'url.format includes pathname');
    }

    // url.resolve
    if (typeof url.resolve === 'function') {
        assertEqual(url.resolve('https://example.com/a/b', '/c'), 'https://example.com/c', 'url.resolve absolute');
        assertEqual(url.resolve('https://example.com/a/b', 'c'), 'https://example.com/a/c', 'url.resolve relative');
    }
}

// ============================================
// Edge cases
// ============================================
console.log('\n--- Edge Cases ---');

// URL with special characters
const specialUrl = new URL('https://example.com/path%20with%20spaces?q=hello%20world');
assertTrue(specialUrl.pathname.includes('path'), 'URL with encoded spaces');

// URL with IPv6
try {
    const ipv6Url = new URL('https://[::1]:8080/path');
    assertEqual(ipv6Url.hostname, '[::1]', 'IPv6 hostname');
    assertEqual(ipv6Url.port, '8080', 'IPv6 port');
} catch (e) {
    console.log('INFO: IPv6 URL parsing not supported');
}

// URL without path
const noPath = new URL('https://example.com');
assertEqual(noPath.pathname, '/', 'Default pathname is /');

// URL with only query
const queryOnly = new URL('https://example.com?foo=bar');
assertEqual(queryOnly.pathname, '/', 'pathname with query only');
assertEqual(queryOnly.search, '?foo=bar', 'search with query only');

// Invalid URL should throw
try {
    new URL('not-a-url');
    console.log('FAIL: Invalid URL should throw');
} catch (e) {
    console.log('PASS: Invalid URL throws error');
}

// Empty query params
const emptyParams = new URLSearchParams('');
assertEqual(emptyParams.toString(), '', 'Empty params toString');

// Query param with no value
const noValueParams = new URLSearchParams('key');
assertEqual(noValueParams.get('key'), '', 'Key with no value');

// Query param with empty value
const emptyValueParams = new URLSearchParams('key=');
assertEqual(emptyValueParams.get('key'), '', 'Key with empty value');

// Multiple query params construction
const multiParams = new URLSearchParams([['a', '1'], ['b', '2']]);
assertEqual(multiParams.get('a'), '1', 'Array construction a');
assertEqual(multiParams.get('b'), '2', 'Array construction b');

// Object construction
const objParams = new URLSearchParams({x: '10', y: '20'});
assertEqual(objParams.get('x'), '10', 'Object construction x');
assertEqual(objParams.get('y'), '20', 'Object construction y');

summary();
