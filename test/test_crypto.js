// Test crypto module
const crypto = require('crypto');

// Test SHA-256
const sha256 = crypto.createHash('sha256').update('hello').digest('hex');
console.log('SHA-256 of "hello":', sha256);
console.log('Expected:          2cf24dba5fb0a30e26e83b2ac5b9e29e1b161e5c1fa7425e73043362938b9824');
console.log('Match:', sha256 === '2cf24dba5fb0a30e26e83b2ac5b9e29e1b161e5c1fa7425e73043362938b9824');

// Test SHA-512
const sha512 = crypto.createHash('sha512').update('hello').digest('hex');
console.log('\nSHA-512 of "hello":', sha512.substring(0, 32) + '...');
console.log('Expected starts:   9b71d224bd62f3785d96d46ad3ea3d73...');

// Test MD5
const md5 = crypto.createHash('md5').update('hello').digest('hex');
console.log('\nMD5 of "hello":', md5);
console.log('Expected:         5d41402abc4b2a76b9719d911017c592');
console.log('Match:', md5 === '5d41402abc4b2a76b9719d911017c592');

// Test HMAC-SHA256
const hmac = crypto.createHmac('sha256', 'secret').update('message').digest('hex');
console.log('\nHMAC-SHA256("secret", "message"):', hmac);

// Test getHashes
console.log('\nAvailable hashes:', crypto.getHashes());

console.log('\nAll tests completed!');
