console.log('=== Round 11 Tests ===\n');

let passed = 0, failed = 0;
function test(name, condition) {
    if (condition) { console.log('PASS:', name); passed++; }
    else { console.log('FAIL:', name); failed++; }
}

// ========== Ed25519 SIGNATURES ==========
console.log('\n--- crypto Ed25519 ---');
const crypto = require('crypto');

// Test generateKeyPairSync
try {
    const { publicKey, privateKey } = crypto.generateKeyPairSync('ed25519');
    test('generateKeyPairSync ed25519 returns keys', publicKey && privateKey);
    test('Ed25519 publicKey is 32 bytes', publicKey.length === 32);
    test('Ed25519 privateKey is 32 bytes', privateKey.length === 32);
} catch (e) {
    console.log('generateKeyPairSync error:', e.message);
    test('generateKeyPairSync ed25519', false);
}

// Test sign and verify
try {
    const { publicKey, privateKey } = crypto.generateKeyPairSync('ed25519');

    // Sign
    const sign = crypto.createSign('ed25519');
    sign.update('hello world');
    const signature = sign.sign(privateKey);
    test('Ed25519 sign returns 64 bytes', signature && signature.length === 64);

    // Verify valid signature
    const verify = crypto.createVerify('ed25519');
    verify.update('hello world');
    const valid = verify.verify(publicKey, signature);
    test('Ed25519 verify valid signature', valid === true);

    // Verify wrong message fails
    const verify2 = crypto.createVerify('ed25519');
    verify2.update('wrong message');
    const invalid = verify2.verify(publicKey, signature);
    test('Ed25519 verify invalid message', invalid === false);
} catch (e) {
    console.log('Ed25519 sign/verify error:', e.message);
    test('Ed25519 sign/verify works', false);
}

// ========== X25519 ECDH ==========
console.log('\n--- crypto X25519 ECDH ---');

try {
    const alice = crypto.createECDH('x25519');
    alice.generateKeys();
    test('X25519 alice generateKeys', alice.getPublicKey().length === 32);

    const bob = crypto.createECDH('x25519');
    bob.generateKeys();
    test('X25519 bob generateKeys', bob.getPublicKey().length === 32);

    const aliceSecret = alice.computeSecret(bob.getPublicKey());
    const bobSecret = bob.computeSecret(alice.getPublicKey());
    test('X25519 shared secret length', aliceSecret.length === 32);
    test('X25519 shared secrets match', aliceSecret.equals(bobSecret));
} catch (e) {
    console.log('X25519 error:', e.message);
    test('X25519 ECDH works', false);
}

// Test x25519 key pair generation
try {
    const { publicKey, privateKey } = crypto.generateKeyPairSync('x25519');
    test('generateKeyPairSync x25519 returns keys', publicKey && privateKey);
    test('X25519 publicKey is 32 bytes', publicKey.length === 32);
    test('X25519 privateKey is 32 bytes', privateKey.length === 32);
} catch (e) {
    console.log('X25519 generateKeyPairSync error:', e.message);
    test('generateKeyPairSync x25519', false);
}

// ========== FS FILE DESCRIPTORS ==========
console.log('\n--- fs file descriptors ---');
const fs = require('fs');

// Test openSync, writeSync, closeSync
try {
    const fd = fs.openSync('/tmp/test_fd_round11.txt', 'w');
    test('fs.openSync returns fd', typeof fd === 'number' && fd > 0);

    const written = fs.writeSync(fd, Buffer.from('hello fd'));
    test('fs.writeSync returns bytes', written === 8);

    fs.closeSync(fd);
    test('fs.closeSync works', true);
} catch (e) {
    console.log('fs open/write/close error:', e.message);
    test('fs open/write/close works', false);
}

// Test readSync
try {
    const fd = fs.openSync('/tmp/test_fd_round11.txt', 'r');
    const buf = Buffer.alloc(10);
    const bytesRead = fs.readSync(fd, buf, 0, 10, 0);
    test('fs.readSync returns bytes', bytesRead === 8);
    test('fs.readSync content', buf.slice(0, 8).toString() === 'hello fd');
    fs.closeSync(fd);
} catch (e) {
    console.log('fs.readSync error:', e.message);
    test('fs.readSync works', false);
}

// Test fstatSync
try {
    const fd = fs.openSync('/tmp/test_fd_round11.txt', 'r');
    const stats = fs.fstatSync(fd);
    test('fs.fstatSync returns stats', stats && typeof stats.size === 'number');
    test('fs.fstatSync size correct', stats.size === 8);
    test('fs.fstatSync has isFile', typeof stats.isFile === 'function');
    test('fs.fstatSync isFile() works', stats.isFile() === true);
    test('fs.fstatSync isDirectory() works', stats.isDirectory() === false);
    fs.closeSync(fd);
} catch (e) {
    console.log('fs.fstatSync error:', e.message);
    test('fs.fstatSync works', false);
}

// Test fsyncSync
try {
    const fd = fs.openSync('/tmp/test_fd_round11.txt', 'r+');
    fs.fsyncSync(fd);
    test('fs.fsyncSync works', true);
    fs.closeSync(fd);
} catch (e) {
    console.log('fs.fsyncSync error:', e.message);
    test('fs.fsyncSync works', false);
}

// Test ftruncateSync - basic existence check
test('fs.ftruncateSync exists', typeof fs.ftruncateSync === 'function');

// Test position parameter in readSync (using existing file)
try {
    // First create a file with known content using writeFileSync
    fs.writeFileSync('/tmp/test_fd_pos.txt', 'ABCDEFGHIJ');

    const fd = fs.openSync('/tmp/test_fd_pos.txt', 'r');

    // Read from position 5
    const buf = Buffer.alloc(5);
    const bytesRead = fs.readSync(fd, buf, 0, 5, 5);
    test('fs.readSync with position', bytesRead === 5 && buf.toString() === 'FGHIJ');

    fs.closeSync(fd);
} catch (e) {
    console.log('fs.readSync position error:', e.message);
    test('fs.readSync with position', false);
}

// ========== SUMMARY ==========
console.log('\n=================================');
console.log('Results: ' + passed + ' passed, ' + failed + ' failed');
console.log('=================================');
