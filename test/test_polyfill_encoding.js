// Comprehensive Encoding polyfill tests
// Tests TextEncoder, TextDecoder, atob, btoa

const {
    assertEqual, assertArrayEqual, assertTrue, assertInstanceOf,
    assertTypeOf, summary
} = require('./helpers/assert.js');

console.log('=== Encoding Polyfill Tests ===\n');

// ============================================
// TextEncoder
// ============================================
console.log('--- TextEncoder ---');

const encoder = new TextEncoder();
assertTypeOf(encoder.encoding, 'string', 'TextEncoder has encoding property');
assertEqual(encoder.encoding, 'utf-8', 'TextEncoder encoding is utf-8');

// Basic ASCII
const asciiResult = encoder.encode('hello');
assertInstanceOf(asciiResult, Uint8Array, 'encode returns Uint8Array');
assertEqual(asciiResult.length, 5, 'ASCII string length');
assertArrayEqual(Array.from(asciiResult), [104, 101, 108, 108, 111], 'ASCII encoding');

// Empty string
const emptyResult = encoder.encode('');
assertEqual(emptyResult.length, 0, 'Empty string length');

// Single character
const singleResult = encoder.encode('A');
assertArrayEqual(Array.from(singleResult), [65], 'Single character encoding');

// Numbers as string
const numbersResult = encoder.encode('123');
assertArrayEqual(Array.from(numbersResult), [49, 50, 51], 'Number string encoding');

// Special ASCII characters
const specialResult = encoder.encode('!@#$%');
assertArrayEqual(Array.from(specialResult), [33, 64, 35, 36, 37], 'Special characters encoding');

// Whitespace
const whitespaceResult = encoder.encode(' \t\n');
assertArrayEqual(Array.from(whitespaceResult), [32, 9, 10], 'Whitespace encoding');

// 2-byte UTF-8 characters (Latin Extended)
const latin2Byte = encoder.encode('\u00e9'); // e with acute accent
assertEqual(latin2Byte.length, 2, 'Latin extended is 2 bytes');
assertArrayEqual(Array.from(latin2Byte), [0xc3, 0xa9], 'Latin extended encoding');

// More 2-byte characters
const euro = encoder.encode('\u00a9'); // Copyright symbol
assertEqual(euro.length, 2, 'Copyright symbol is 2 bytes');

// 3-byte UTF-8 characters (CJK)
const chinese = encoder.encode('\u4e2d'); // Chinese character for "middle"
assertEqual(chinese.length, 3, 'Chinese character is 3 bytes');
assertArrayEqual(Array.from(chinese), [0xe4, 0xb8, 0xad], 'Chinese character encoding');

// Mixed content
const mixedResult = encoder.encode('Hello \u4e2d');
assertEqual(mixedResult.length, 9, 'Mixed content length'); // 6 ASCII + 3 Chinese

// Long string
const longString = 'a'.repeat(10000);
const longResult = encoder.encode(longString);
assertEqual(longResult.length, 10000, 'Long string encoding');

// ============================================
// TextEncoder.encodeInto
// ============================================
console.log('\n--- TextEncoder.encodeInto ---');

if (typeof encoder.encodeInto === 'function') {
    const destArray = new Uint8Array(10);
    const encodeIntoResult = encoder.encodeInto('hello', destArray);
    assertTypeOf(encodeIntoResult.read, 'number', 'encodeInto returns read count');
    assertTypeOf(encodeIntoResult.written, 'number', 'encodeInto returns written count');
    assertEqual(encodeIntoResult.written, 5, 'encodeInto written bytes');
    assertEqual(destArray[0], 104, 'encodeInto first byte');

    // Truncation test
    const smallDest = new Uint8Array(3);
    const truncResult = encoder.encodeInto('hello', smallDest);
    assertEqual(truncResult.written, 3, 'encodeInto truncates');
    assertArrayEqual(Array.from(smallDest), [104, 101, 108], 'encodeInto truncated content');
} else {
    console.log('SKIP: encodeInto not available');
}

// ============================================
// TextDecoder
// ============================================
console.log('\n--- TextDecoder ---');

const decoder = new TextDecoder();
assertTypeOf(decoder.encoding, 'string', 'TextDecoder has encoding property');
assertEqual(decoder.encoding, 'utf-8', 'TextDecoder encoding is utf-8');

// Basic ASCII
const asciiDecoded = decoder.decode(new Uint8Array([104, 101, 108, 108, 111]));
assertEqual(asciiDecoded, 'hello', 'ASCII decoding');

// Empty array
const emptyDecoded = decoder.decode(new Uint8Array(0));
assertEqual(emptyDecoded, '', 'Empty array decoding');

// Single byte
const singleDecoded = decoder.decode(new Uint8Array([65]));
assertEqual(singleDecoded, 'A', 'Single byte decoding');

// 2-byte UTF-8
const latin2Decoded = decoder.decode(new Uint8Array([0xc3, 0xa9]));
assertEqual(latin2Decoded, '\u00e9', 'Latin extended decoding');

// 3-byte UTF-8
const chineseDecoded = decoder.decode(new Uint8Array([0xe4, 0xb8, 0xad]));
assertEqual(chineseDecoded, '\u4e2d', 'Chinese character decoding');

// Mixed content
const mixedDecoded = decoder.decode(new Uint8Array([72, 105, 0xe4, 0xb8, 0xad]));
assertEqual(mixedDecoded, 'Hi\u4e2d', 'Mixed content decoding');

// ArrayBuffer input
const arrayBuffer = new Uint8Array([104, 101, 108, 108, 111]).buffer;
const abDecoded = decoder.decode(arrayBuffer);
assertEqual(abDecoded, 'hello', 'ArrayBuffer decoding');

// Long input
const longBytes = new Uint8Array(10000);
for (let i = 0; i < 10000; i++) longBytes[i] = 97; // 'a'
const longDecoded = decoder.decode(longBytes);
assertEqual(longDecoded.length, 10000, 'Long input decoding length');
assertEqual(longDecoded[0], 'a', 'Long input first char');
assertEqual(longDecoded[9999], 'a', 'Long input last char');

// ============================================
// Round-trip tests
// ============================================
console.log('\n--- Round-trip Tests ---');

function roundTrip(str) {
    const encoded = encoder.encode(str);
    const decoded = decoder.decode(encoded);
    return decoded === str;
}

assertTrue(roundTrip('hello'), 'Round-trip: hello');
assertTrue(roundTrip(''), 'Round-trip: empty');
assertTrue(roundTrip('Hello World 123!'), 'Round-trip: mixed ASCII');
assertTrue(roundTrip('\u00e9'), 'Round-trip: 2-byte UTF-8');
assertTrue(roundTrip('\u4e2d'), 'Round-trip: 3-byte UTF-8');
assertTrue(roundTrip('Hello \u4e2d World'), 'Round-trip: mixed');
assertTrue(roundTrip('a'.repeat(1000)), 'Round-trip: long string');

// ============================================
// atob (Base64 decode)
// ============================================
console.log('\n--- atob (Base64 decode) ---');

// Basic decoding
assertEqual(atob('SGVsbG8='), 'Hello', 'atob("SGVsbG8=")');
assertEqual(atob('SGVsbG8gV29ybGQ='), 'Hello World', 'atob("SGVsbG8gV29ybGQ=")');
assertEqual(atob('YWJj'), 'abc', 'atob("YWJj")');

// No padding
assertEqual(atob('YQ'), 'a', 'atob no padding (1 char)');
assertEqual(atob('YWI'), 'ab', 'atob no padding (2 chars)');

// Empty string
assertEqual(atob(''), '', 'atob("")');

// Special characters in output
assertEqual(atob('ISIj'), '!"#', 'atob special chars');

// Longer content
assertEqual(atob('VGhlIHF1aWNrIGJyb3duIGZveCBqdW1wcyBvdmVyIHRoZSBsYXp5IGRvZw=='),
    'The quick brown fox jumps over the lazy dog', 'atob longer content');

// Numbers
assertEqual(atob('MTIzNDU2Nzg5MA=='), '1234567890', 'atob numbers');

// ============================================
// btoa (Base64 encode)
// ============================================
console.log('\n--- btoa (Base64 encode) ---');

// Basic encoding
assertEqual(btoa('Hello'), 'SGVsbG8=', 'btoa("Hello")');
assertEqual(btoa('Hello World'), 'SGVsbG8gV29ybGQ=', 'btoa("Hello World")');
assertEqual(btoa('abc'), 'YWJj', 'btoa("abc")');

// Different padding
assertEqual(btoa('a'), 'YQ==', 'btoa("a") - 2 padding');
assertEqual(btoa('ab'), 'YWI=', 'btoa("ab") - 1 padding');
assertEqual(btoa('abc'), 'YWJj', 'btoa("abc") - no padding');

// Empty string
assertEqual(btoa(''), '', 'btoa("")');

// Special characters
assertEqual(btoa('!"#'), 'ISIj', 'btoa special chars');

// Numbers
assertEqual(btoa('1234567890'), 'MTIzNDU2Nzg5MA==', 'btoa numbers');

// Long content
assertEqual(btoa('The quick brown fox jumps over the lazy dog'),
    'VGhlIHF1aWNrIGJyb3duIGZveCBqdW1wcyBvdmVyIHRoZSBsYXp5IGRvZw==', 'btoa longer content');

// Binary content (0-255)
const binaryStr = String.fromCharCode(0, 127, 128, 255);
const binaryBase64 = btoa(binaryStr);
assertEqual(atob(binaryBase64), binaryStr, 'btoa/atob binary round-trip');

// ============================================
// atob/btoa Round-trip Tests
// ============================================
console.log('\n--- atob/btoa Round-trip ---');

function base64RoundTrip(str) {
    return atob(btoa(str)) === str;
}

assertTrue(base64RoundTrip('Hello'), 'Base64 round-trip: Hello');
assertTrue(base64RoundTrip(''), 'Base64 round-trip: empty');
assertTrue(base64RoundTrip('a'), 'Base64 round-trip: a');
assertTrue(base64RoundTrip('ab'), 'Base64 round-trip: ab');
assertTrue(base64RoundTrip('abc'), 'Base64 round-trip: abc');
assertTrue(base64RoundTrip('abcd'), 'Base64 round-trip: abcd');
assertTrue(base64RoundTrip('The quick brown fox'), 'Base64 round-trip: sentence');
assertTrue(base64RoundTrip('12345'), 'Base64 round-trip: numbers');
assertTrue(base64RoundTrip('!@#$%^&*()'), 'Base64 round-trip: special');

// All printable ASCII
let allPrintable = '';
for (let i = 32; i < 127; i++) {
    allPrintable += String.fromCharCode(i);
}
assertTrue(base64RoundTrip(allPrintable), 'Base64 round-trip: all printable ASCII');

// ============================================
// Edge Cases
// ============================================
console.log('\n--- Edge Cases ---');

// Very long base64
const longStr = 'a'.repeat(10000);
const longBase64 = btoa(longStr);
assertEqual(atob(longBase64), longStr, 'Long base64 round-trip');

// Base64 with newlines (some implementations support this)
// Note: Standard atob doesn't require handling newlines
const base64WithoutNewlines = 'SGVsbG8gV29ybGQ=';
assertEqual(atob(base64WithoutNewlines), 'Hello World', 'Base64 standard format');

// Whitespace handling in atob
// Note: Standard atob ignores whitespace in some implementations
// This test verifies core functionality
assertEqual(atob('YWJj'), 'abc', 'Base64 no whitespace');

summary();
