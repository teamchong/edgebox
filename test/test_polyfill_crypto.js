// Comprehensive Crypto polyfill tests
// Tests hash, hmac, randomBytes, randomUUID with known test vectors

const {
    assertEqual, assertTrue, assertFalse, assertTypeOf,
    assertStartsWith, assertContains, summary
} = require('./helpers/assert.js');

const crypto = require('crypto');

console.log('=== Crypto Polyfill Tests ===\n');

// ============================================
// crypto.getHashes - Available algorithms
// ============================================
console.log('--- crypto.getHashes ---');
const hashes = crypto.getHashes();
assertTrue(Array.isArray(hashes), 'getHashes() returns array');
assertContains(hashes.join(','), 'sha256', 'sha256 available');
assertContains(hashes.join(','), 'sha512', 'sha512 available');
assertContains(hashes.join(','), 'sha1', 'sha1 available');
assertContains(hashes.join(','), 'md5', 'md5 available');

// ============================================
// SHA-256 Test Vectors
// ============================================
console.log('\n--- SHA-256 ---');

// Empty string
const sha256Empty = crypto.createHash('sha256').update('').digest('hex');
assertEqual(sha256Empty, 'e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855', 'SHA-256("")');

// "hello"
const sha256Hello = crypto.createHash('sha256').update('hello').digest('hex');
assertEqual(sha256Hello, '2cf24dba5fb0a30e26e83b2ac5b9e29e1b161e5c1fa7425e73043362938b9824', 'SHA-256("hello")');

// "hello world"
const sha256HelloWorld = crypto.createHash('sha256').update('hello world').digest('hex');
assertEqual(sha256HelloWorld, 'b94d27b9934d3e08a52e52d7da7dabfac484efe37a5380ee9088f7ace2efcde9', 'SHA-256("hello world")');

// "abc"
const sha256Abc = crypto.createHash('sha256').update('abc').digest('hex');
assertEqual(sha256Abc, 'ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad', 'SHA-256("abc")');

// Longer message
const sha256Long = crypto.createHash('sha256').update('The quick brown fox jumps over the lazy dog').digest('hex');
assertEqual(sha256Long, 'd7a8fbb307d7809469ca9abcb0082e4f8d5651e46d3cdb762d02d0bf37c9e592', 'SHA-256(quick brown fox)');

// Multiple updates (chained)
const sha256Chain = crypto.createHash('sha256')
    .update('hello')
    .update(' ')
    .update('world')
    .digest('hex');
assertEqual(sha256Chain, 'b94d27b9934d3e08a52e52d7da7dabfac484efe37a5380ee9088f7ace2efcde9', 'SHA-256 chained updates');

// ============================================
// SHA-512 Test Vectors
// ============================================
console.log('\n--- SHA-512 ---');

// Empty string
const sha512Empty = crypto.createHash('sha512').update('').digest('hex');
assertEqual(sha512Empty, 'cf83e1357eefb8bdf1542850d66d8007d620e4050b5715dc83f4a921d36ce9ce47d0d13c5d85f2b0ff8318d2877eec2f63b931bd47417a81a538327af927da3e', 'SHA-512("")');

// "hello"
const sha512Hello = crypto.createHash('sha512').update('hello').digest('hex');
assertEqual(sha512Hello, '9b71d224bd62f3785d96d46ad3ea3d73319bfbc2890caadae2dff72519673ca72323c3d99ba5c11d7c7acc6e14b8c5da0c4663475c2e5c3adef46f73bcdec043', 'SHA-512("hello")');

// "abc"
const sha512Abc = crypto.createHash('sha512').update('abc').digest('hex');
assertEqual(sha512Abc, 'ddaf35a193617abacc417349ae20413112e6fa4e89a97ea20a9eeee64b55d39a2192992a274fc1a836ba3c23a3feebbd454d4423643ce80e2a9ac94fa54ca49f', 'SHA-512("abc")');

// ============================================
// SHA-1 Test Vectors
// ============================================
console.log('\n--- SHA-1 ---');

// Empty string
const sha1Empty = crypto.createHash('sha1').update('').digest('hex');
assertEqual(sha1Empty, 'da39a3ee5e6b4b0d3255bfef95601890afd80709', 'SHA-1("")');

// "hello"
const sha1Hello = crypto.createHash('sha1').update('hello').digest('hex');
assertEqual(sha1Hello, 'aaf4c61ddcc5e8a2dabede0f3b482cd9aea9434d', 'SHA-1("hello")');

// "abc"
const sha1Abc = crypto.createHash('sha1').update('abc').digest('hex');
assertEqual(sha1Abc, 'a9993e364706816aba3e25717850c26c9cd0d89d', 'SHA-1("abc")');

// ============================================
// MD5 Test Vectors
// ============================================
console.log('\n--- MD5 ---');

// Empty string
const md5Empty = crypto.createHash('md5').update('').digest('hex');
assertEqual(md5Empty, 'd41d8cd98f00b204e9800998ecf8427e', 'MD5("")');

// "hello"
const md5Hello = crypto.createHash('md5').update('hello').digest('hex');
assertEqual(md5Hello, '5d41402abc4b2a76b9719d911017c592', 'MD5("hello")');

// "abc"
const md5Abc = crypto.createHash('md5').update('abc').digest('hex');
assertEqual(md5Abc, '900150983cd24fb0d6963f7d28e17f72', 'MD5("abc")');

// ============================================
// Base64 Output
// ============================================
console.log('\n--- Base64 Output ---');

const sha256Base64 = crypto.createHash('sha256').update('hello').digest('base64');
assertEqual(sha256Base64, 'LPJNul+wow4m6DsqxbninhsWHlwfp0JecwQzYpOLmCQ=', 'SHA-256 base64 output');

// ============================================
// HMAC-SHA256 Test Vectors
// ============================================
console.log('\n--- HMAC-SHA256 ---');

// Standard test vector
const hmac1 = crypto.createHmac('sha256', 'key').update('message').digest('hex');
assertEqual(hmac1, '6e9ef29b75fffc5b7abae527d58fdadb2fe42e7219011976917343065f58ed4a', 'HMAC-SHA256(key, message)');

// Empty key
const hmac2 = crypto.createHmac('sha256', '').update('hello').digest('hex');
assertTypeOf(hmac2, 'string', 'HMAC-SHA256 with empty key returns string');
assertEqual(hmac2.length, 64, 'HMAC-SHA256 output length');

// Empty message
const hmac3 = crypto.createHmac('sha256', 'secret').update('').digest('hex');
assertEqual(hmac3, 'f9e66e179b6747ae54108f82f8ade8b3c25d76fd30afde6c395822c530196169', 'HMAC-SHA256(secret, "")');

// Chained updates
const hmac4 = crypto.createHmac('sha256', 'secret')
    .update('hello')
    .update(' ')
    .update('world')
    .digest('hex');
assertTypeOf(hmac4, 'string', 'HMAC-SHA256 chained returns string');
assertEqual(hmac4.length, 64, 'HMAC-SHA256 chained output length');

// ============================================
// HMAC-SHA512 Test Vectors
// ============================================
console.log('\n--- HMAC-SHA512 ---');

const hmacSha512 = crypto.createHmac('sha512', 'key').update('message').digest('hex');
assertTypeOf(hmacSha512, 'string', 'HMAC-SHA512 returns string');
assertEqual(hmacSha512.length, 128, 'HMAC-SHA512 output length');

// ============================================
// HMAC-SHA1 Test Vectors
// ============================================
console.log('\n--- HMAC-SHA1 ---');

const hmacSha1 = crypto.createHmac('sha1', 'key').update('message').digest('hex');
assertTypeOf(hmacSha1, 'string', 'HMAC-SHA1 returns string');
assertEqual(hmacSha1.length, 40, 'HMAC-SHA1 output length');

// ============================================
// crypto.randomBytes
// ============================================
console.log('\n--- crypto.randomBytes ---');

const rand1 = crypto.randomBytes(16);
assertEqual(rand1.length, 16, 'randomBytes(16) length');
assertTrue(rand1 instanceof Uint8Array || rand1 instanceof Buffer, 'randomBytes returns buffer');

const rand2 = crypto.randomBytes(32);
assertEqual(rand2.length, 32, 'randomBytes(32) length');

// Check randomness - at least one non-zero byte in large sample
const rand3 = crypto.randomBytes(100);
let hasNonZero = false;
for (let i = 0; i < rand3.length; i++) {
    if (rand3[i] !== 0) {
        hasNonZero = true;
        break;
    }
}
assertTrue(hasNonZero, 'randomBytes has non-zero values');

// Check bytes are different (statistical test)
const rand4 = crypto.randomBytes(100);
let differentBytes = 0;
for (let i = 0; i < rand3.length; i++) {
    if (rand3[i] !== rand4[i]) differentBytes++;
}
assertTrue(differentBytes > 50, 'randomBytes produces different results');

// ============================================
// crypto.randomUUID
// ============================================
console.log('\n--- crypto.randomUUID ---');

const uuid1 = crypto.randomUUID();
assertTypeOf(uuid1, 'string', 'randomUUID returns string');
assertEqual(uuid1.length, 36, 'UUID length is 36');

// UUID format: xxxxxxxx-xxxx-4xxx-yxxx-xxxxxxxxxxxx
const uuidParts = uuid1.split('-');
assertEqual(uuidParts.length, 5, 'UUID has 5 parts');
assertEqual(uuidParts[0].length, 8, 'UUID part 1 length');
assertEqual(uuidParts[1].length, 4, 'UUID part 2 length');
assertEqual(uuidParts[2].length, 4, 'UUID part 3 length');
assertEqual(uuidParts[3].length, 4, 'UUID part 4 length');
assertEqual(uuidParts[4].length, 12, 'UUID part 5 length');

// Version should be 4
assertEqual(uuidParts[2][0], '4', 'UUID version is 4');

// Variant should be 8, 9, a, or b
const variant = uuidParts[3][0].toLowerCase();
assertTrue(['8', '9', 'a', 'b'].includes(variant), 'UUID variant is valid');

// Generate multiple UUIDs - they should be different
const uuid2 = crypto.randomUUID();
const uuid3 = crypto.randomUUID();
assertFalse(uuid1 === uuid2, 'UUIDs are unique (1 vs 2)');
assertFalse(uuid2 === uuid3, 'UUIDs are unique (2 vs 3)');
assertFalse(uuid1 === uuid3, 'UUIDs are unique (1 vs 3)');

// ============================================
// Web Crypto API (crypto.subtle)
// ============================================
console.log('\n--- Web Crypto API ---');

// Check if subtle exists
if (typeof crypto.subtle !== 'undefined' || typeof globalThis.crypto?.subtle !== 'undefined') {
    const subtle = crypto.subtle || globalThis.crypto.subtle;
    assertTrue(typeof subtle.digest === 'function', 'subtle.digest exists');
    assertTrue(typeof subtle.sign === 'function', 'subtle.sign exists');
    assertTrue(typeof subtle.generateKey === 'function', 'subtle.generateKey exists');
    assertTrue(typeof subtle.importKey === 'function', 'subtle.importKey exists');
    console.log('PASS: Web Crypto API subtle interface available');
} else {
    console.log('SKIP: Web Crypto API not available in this environment');
}

// Check globalThis.crypto
if (typeof globalThis.crypto !== 'undefined') {
    assertTrue(typeof globalThis.crypto.getRandomValues === 'function', 'getRandomValues exists');
    assertTrue(typeof globalThis.crypto.randomUUID === 'function', 'randomUUID exists');

    // Test getRandomValues
    const arr = new Uint8Array(10);
    globalThis.crypto.getRandomValues(arr);
    let hasValue = false;
    for (let i = 0; i < arr.length; i++) {
        if (arr[i] !== 0) hasValue = true;
    }
    assertTrue(hasValue, 'getRandomValues fills array');
}

// ============================================
// Edge Cases and Error Handling
// ============================================
console.log('\n--- Edge Cases ---');

// Binary data in hash
const binaryData = String.fromCharCode(0, 1, 2, 255, 254, 253);
const binaryHash = crypto.createHash('sha256').update(binaryData).digest('hex');
assertEqual(binaryHash.length, 64, 'Binary data hash has correct length');

// Long input
const longInput = 'a'.repeat(10000);
const longHash = crypto.createHash('sha256').update(longInput).digest('hex');
assertEqual(longHash.length, 64, 'Long input hash has correct length');

// Unicode in hash
const unicodeHash = crypto.createHash('sha256').update('\u00e9\u4e2d').digest('hex');
assertEqual(unicodeHash.length, 64, 'Unicode hash has correct length');

summary();
