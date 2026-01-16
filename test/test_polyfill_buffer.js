// Comprehensive Buffer polyfill tests
// Tests all 26+ Buffer functions with edge cases

const {
    assertEqual, assertArrayEqual, assertTrue, assertFalse,
    assertInstanceOf, assertTypeOf, assertGreater, summary
} = require('./helpers/assert.js');

console.log('=== Buffer Polyfill Tests ===\n');

// ============================================
// Buffer.from - Create buffer from various sources
// ============================================
console.log('--- Buffer.from ---');

// From string (UTF-8)
const buf1 = Buffer.from('hello');
assertEqual(buf1.length, 5, 'Buffer.from("hello") length');
assertEqual(buf1[0], 104, 'Buffer.from("hello")[0] = "h"');
assertEqual(buf1[1], 101, 'Buffer.from("hello")[1] = "e"');
assertEqual(buf1[4], 111, 'Buffer.from("hello")[4] = "o"');

// From string with Unicode
const bufUnicode = Buffer.from('hello world');
assertEqual(bufUnicode.length, 11, 'Buffer.from("hello world") length');

// From array
const buf2 = Buffer.from([65, 66, 67]);
assertEqual(buf2.length, 3, 'Buffer.from([65, 66, 67]) length');
assertEqual(buf2[0], 65, 'Buffer.from([65, 66, 67])[0]');
assertEqual(buf2.toString(), 'ABC', 'Buffer.from([65, 66, 67]).toString()');

// From Uint8Array
const u8 = new Uint8Array([1, 2, 3, 4, 5]);
const buf3 = Buffer.from(u8);
assertEqual(buf3.length, 5, 'Buffer.from(Uint8Array) length');
assertArrayEqual(Array.from(buf3), [1, 2, 3, 4, 5], 'Buffer.from(Uint8Array) content');

// From ArrayBuffer
const ab = new ArrayBuffer(4);
const view = new Uint8Array(ab);
view[0] = 10; view[1] = 20; view[2] = 30; view[3] = 40;
const buf4 = Buffer.from(ab);
assertEqual(buf4.length, 4, 'Buffer.from(ArrayBuffer) length');
assertEqual(buf4[0], 10, 'Buffer.from(ArrayBuffer)[0]');

// From Base64
const bufBase64 = Buffer.from('SGVsbG8gV29ybGQ=', 'base64');
assertEqual(bufBase64.toString(), 'Hello World', 'Buffer.from base64 decode');

// From Base64URL
const bufBase64Url = Buffer.from('SGVsbG8tV29ybGQ', 'base64url');
assertEqual(bufBase64Url.toString(), 'Hello-World', 'Buffer.from base64url decode');

// Empty buffer
const bufEmpty = Buffer.from('');
assertEqual(bufEmpty.length, 0, 'Buffer.from("") length');

// ============================================
// Buffer.alloc - Allocate zeroed buffer
// ============================================
console.log('\n--- Buffer.alloc ---');

const allocBuf = Buffer.alloc(10);
assertEqual(allocBuf.length, 10, 'Buffer.alloc(10) length');
assertEqual(allocBuf[0], 0, 'Buffer.alloc(10)[0] is zero');
assertEqual(allocBuf[9], 0, 'Buffer.alloc(10)[9] is zero');

const allocBufFill = Buffer.alloc(5, 0x61);  // 'a'
assertEqual(allocBufFill.length, 5, 'Buffer.alloc(5, 0x61) length');
assertEqual(allocBufFill[0], 0x61, 'Buffer.alloc filled with 0x61');
assertEqual(allocBufFill.toString(), 'aaaaa', 'Buffer.alloc filled toString');

// Zero length
const allocZero = Buffer.alloc(0);
assertEqual(allocZero.length, 0, 'Buffer.alloc(0) length');

// ============================================
// Buffer.allocUnsafe - Allocate uninitialized buffer
// ============================================
console.log('\n--- Buffer.allocUnsafe ---');

const unsafeBuf = Buffer.allocUnsafe(100);
assertEqual(unsafeBuf.length, 100, 'Buffer.allocUnsafe(100) length');
assertInstanceOf(unsafeBuf, Buffer, 'Buffer.allocUnsafe returns Buffer');

// ============================================
// Buffer.concat - Concatenate buffers
// ============================================
console.log('\n--- Buffer.concat ---');

const concatBuf1 = Buffer.from('Hello');
const concatBuf2 = Buffer.from(' ');
const concatBuf3 = Buffer.from('World');
const concatResult = Buffer.concat([concatBuf1, concatBuf2, concatBuf3]);
assertEqual(concatResult.toString(), 'Hello World', 'Buffer.concat result');
assertEqual(concatResult.length, 11, 'Buffer.concat length');

// Concat with total length
const concatWithLen = Buffer.concat([concatBuf1, concatBuf2], 6);
assertEqual(concatWithLen.length, 6, 'Buffer.concat with totalLength');
assertEqual(concatWithLen.toString(), 'Hello ', 'Buffer.concat truncated');

// Concat empty array
const concatEmpty = Buffer.concat([]);
assertEqual(concatEmpty.length, 0, 'Buffer.concat([]) length');

// ============================================
// Buffer.isBuffer - Type checking
// ============================================
console.log('\n--- Buffer.isBuffer ---');

assertTrue(Buffer.isBuffer(Buffer.from('test')), 'Buffer.isBuffer(Buffer)');
assertTrue(Buffer.isBuffer(Buffer.alloc(10)), 'Buffer.isBuffer(Buffer.alloc)');
// Note: In our polyfill, Uint8Array might also return true
assertFalse(Buffer.isBuffer('string'), 'Buffer.isBuffer(string)');
assertFalse(Buffer.isBuffer([1, 2, 3]), 'Buffer.isBuffer(array)');
assertFalse(Buffer.isBuffer(null), 'Buffer.isBuffer(null)');
assertFalse(Buffer.isBuffer(undefined), 'Buffer.isBuffer(undefined)');

// ============================================
// Buffer.byteLength - Calculate byte length
// ============================================
console.log('\n--- Buffer.byteLength ---');

assertEqual(Buffer.byteLength('hello'), 5, 'Buffer.byteLength("hello")');
assertEqual(Buffer.byteLength(''), 0, 'Buffer.byteLength("")');
// Unicode characters take multiple bytes in UTF-8
assertGreater(Buffer.byteLength('\u00e9'), 1, 'Buffer.byteLength(unicode) > 1'); // e with accent

// ============================================
// buffer.toString - Convert to string
// ============================================
console.log('\n--- buffer.toString ---');

const strBuf = Buffer.from('Hello World');
assertEqual(strBuf.toString(), 'Hello World', 'toString() default utf-8');
assertEqual(strBuf.toString('utf-8'), 'Hello World', 'toString("utf-8")');
assertEqual(strBuf.toString('utf8'), 'Hello World', 'toString("utf8")');

// toString with slice
assertEqual(strBuf.toString('utf-8', 0, 5), 'Hello', 'toString with start/end');
assertEqual(strBuf.toString('utf-8', 6), 'World', 'toString with start only');

// Base64 encoding
assertEqual(strBuf.toString('base64'), 'SGVsbG8gV29ybGQ=', 'toString("base64")');

// Base64URL encoding
const base64UrlBuf = Buffer.from('Hello-World');
const base64UrlStr = base64UrlBuf.toString('base64url');
assertFalse(base64UrlStr.includes('+'), 'base64url has no +');
assertFalse(base64UrlStr.includes('/'), 'base64url has no /');
assertFalse(base64UrlStr.includes('='), 'base64url has no =');

// ============================================
// buffer.slice - Create view
// ============================================
console.log('\n--- buffer.slice ---');

const sliceBuf = Buffer.from('Hello World');
const sliced = sliceBuf.slice(0, 5);
assertEqual(sliced.toString(), 'Hello', 'slice(0, 5)');
assertEqual(sliced.length, 5, 'slice length');

const slicedEnd = sliceBuf.slice(6);
assertEqual(slicedEnd.toString(), 'World', 'slice(6)');

const slicedNeg = sliceBuf.slice(-5);
assertEqual(slicedNeg.toString(), 'World', 'slice(-5)');

// ============================================
// buffer.copy - Copy data between buffers
// ============================================
console.log('\n--- buffer.copy ---');

const srcBuf = Buffer.from('Hello');
const destBuf = Buffer.alloc(10);
const bytesCopied = srcBuf.copy(destBuf);
assertEqual(bytesCopied, 5, 'copy() returns bytes copied');
assertEqual(destBuf.slice(0, 5).toString(), 'Hello', 'copy() content');

// Copy with offset
const destBuf2 = Buffer.alloc(10);
srcBuf.copy(destBuf2, 3);
assertEqual(destBuf2.slice(3, 8).toString(), 'Hello', 'copy with targetStart');

// Copy partial
const destBuf3 = Buffer.alloc(10);
srcBuf.copy(destBuf3, 0, 0, 3);
assertEqual(destBuf3.slice(0, 3).toString(), 'Hel', 'copy partial');

// ============================================
// buffer.equals - Compare buffers
// ============================================
console.log('\n--- buffer.equals ---');

const eqBuf1 = Buffer.from('hello');
const eqBuf2 = Buffer.from('hello');
const eqBuf3 = Buffer.from('world');
const eqBuf4 = Buffer.from('hell');

assertTrue(eqBuf1.equals(eqBuf2), 'equals same content');
assertFalse(eqBuf1.equals(eqBuf3), 'equals different content');
assertFalse(eqBuf1.equals(eqBuf4), 'equals different length');

// ============================================
// buffer.compare - Compare buffers
// ============================================
console.log('\n--- buffer.compare ---');

const cmpBuf1 = Buffer.from('abc');
const cmpBuf2 = Buffer.from('abc');
const cmpBuf3 = Buffer.from('abd');
const cmpBuf4 = Buffer.from('abb');
const cmpBuf5 = Buffer.from('abcd');

assertEqual(cmpBuf1.compare(cmpBuf2), 0, 'compare equal');
assertEqual(cmpBuf1.compare(cmpBuf3) < 0, true, 'compare less');
assertEqual(cmpBuf1.compare(cmpBuf4) > 0, true, 'compare greater');
assertEqual(cmpBuf1.compare(cmpBuf5) < 0, true, 'compare shorter');

// ============================================
// buffer.write - Write string to buffer
// ============================================
console.log('\n--- buffer.write ---');

const writeBuf = Buffer.alloc(20);
const written = writeBuf.write('Hello');
assertEqual(written, 5, 'write() returns bytes written');
assertEqual(writeBuf.slice(0, 5).toString(), 'Hello', 'write() content');

// Write with offset
const writeBuf2 = Buffer.alloc(20);
writeBuf2.write('Hello', 5);
assertEqual(writeBuf2.slice(5, 10).toString(), 'Hello', 'write with offset');

// Write with length limit
const writeBuf3 = Buffer.alloc(10);
const written3 = writeBuf3.write('Hello World', 0, 5);
assertEqual(written3, 5, 'write with length limit');
assertEqual(writeBuf3.slice(0, 5).toString(), 'Hello', 'write limited content');

// ============================================
// buffer[index] - Direct byte access
// ============================================
console.log('\n--- buffer[index] access ---');

const indexBuf = Buffer.from([0, 127, 128, 255]);
assertEqual(indexBuf[0], 0, 'buffer[0]');
assertEqual(indexBuf[1], 127, 'buffer[1]');
assertEqual(indexBuf[2], 128, 'buffer[2]');
assertEqual(indexBuf[3], 255, 'buffer[3]');

// Modify bytes
indexBuf[0] = 42;
assertEqual(indexBuf[0], 42, 'buffer[0] = 42');

// ============================================
// buffer.set - Set bytes (Uint8Array method)
// ============================================
console.log('\n--- buffer.set ---');

const setBuf = Buffer.alloc(10);
setBuf.set([1, 2, 3, 4, 5]);
assertArrayEqual(Array.from(setBuf.slice(0, 5)), [1, 2, 3, 4, 5], 'set() array');

setBuf.set([9, 8, 7], 5);
assertArrayEqual(Array.from(setBuf.slice(5, 8)), [9, 8, 7], 'set() with offset');

// ============================================
// buffer.fill - Fill buffer
// ============================================
console.log('\n--- buffer.fill ---');

const fillBuf = Buffer.alloc(5);
fillBuf.fill(0x42);
assertEqual(fillBuf[0], 0x42, 'fill byte 0');
assertEqual(fillBuf[4], 0x42, 'fill byte 4');

// ============================================
// Iteration
// ============================================
console.log('\n--- Iteration ---');

const iterBuf = Buffer.from([1, 2, 3, 4, 5]);
let iterSum = 0;
for (const byte of iterBuf) {
    iterSum += byte;
}
assertEqual(iterSum, 15, 'for...of iteration sum');

// ============================================
// UTF-8 Multi-byte characters
// ============================================
console.log('\n--- UTF-8 Multi-byte ---');

// 2-byte UTF-8 (e.g., e with accent \u00e9)
const utf8_2byte = Buffer.from('\u00e9');
assertEqual(utf8_2byte.length, 2, 'UTF-8 2-byte character length');

// 3-byte UTF-8 (e.g., Chinese character)
const utf8_3byte = Buffer.from('\u4e2d');
assertEqual(utf8_3byte.length, 3, 'UTF-8 3-byte character length');

// Round-trip
assertEqual(utf8_2byte.toString(), '\u00e9', 'UTF-8 2-byte round-trip');
assertEqual(utf8_3byte.toString(), '\u4e2d', 'UTF-8 3-byte round-trip');

// ============================================
// Edge Cases
// ============================================
console.log('\n--- Edge Cases ---');

// Very large buffer
const largeBuf = Buffer.alloc(1000000);
assertEqual(largeBuf.length, 1000000, 'Large buffer allocation');
largeBuf[999999] = 42;
assertEqual(largeBuf[999999], 42, 'Large buffer access');

// Buffer with all byte values
const allBytesBuf = Buffer.alloc(256);
for (let i = 0; i < 256; i++) {
    allBytesBuf[i] = i;
}
assertEqual(allBytesBuf[0], 0, 'All bytes: 0');
assertEqual(allBytesBuf[255], 255, 'All bytes: 255');
assertEqual(allBytesBuf[128], 128, 'All bytes: 128');

// Subarray (alias for slice in some implementations)
if (typeof Buffer.prototype.subarray === 'function') {
    const subBuf = Buffer.from('Hello World');
    const sub = subBuf.subarray(0, 5);
    assertEqual(sub.toString(), 'Hello', 'subarray()');
}

// ============================================
// UTF-16LE encoding (used by tsc)
// ============================================
console.log('\n--- UTF-16LE ---');

const utf16Buf = Buffer.from([0x48, 0x00, 0x69, 0x00]); // "Hi" in UTF-16LE
assertEqual(utf16Buf.toString('utf16le'), 'Hi', 'UTF-16LE decode');

summary();
