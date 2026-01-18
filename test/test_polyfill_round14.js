console.log('=== Round 14 Tests ===\n');

let passed = 0, failed = 0;
function test(name, condition) {
    if (condition) { console.log('PASS:', name); passed++; }
    else { console.log('FAIL:', name); failed++; }
}

const fs = require('fs');
const os = require('os');
const crypto = require('crypto');

// ========== FS FDATASYNC ==========
console.log('\n--- fs fdatasync ---');

try {
    fs.writeFileSync('/tmp/test_fdatasync.txt', 'test');
    const fd = fs.openSync('/tmp/test_fdatasync.txt', 'r+');
    fs.fdatasyncSync(fd);
    fs.closeSync(fd);
    test('fs.fdatasyncSync works', true);
} catch (e) {
    console.log('fdatasync error:', e.message);
    test('fs.fdatasyncSync', false);
}

// ========== FS FUTIMES ==========
console.log('\n--- fs futimes ---');

try {
    fs.writeFileSync('/tmp/test_futimes.txt', 'test');
    const fd = fs.openSync('/tmp/test_futimes.txt', 'r+');
    const newTime = new Date('2020-06-15T12:00:00Z');
    fs.futimesSync(fd, newTime, newTime);
    fs.closeSync(fd);
    const stats = fs.statSync('/tmp/test_futimes.txt');
    test('fs.futimesSync works', stats.mtime.getUTCFullYear() === 2020);
} catch (e) {
    console.log('futimes error:', e.message);
    test('fs.futimesSync', false);
}

// ========== FS LUTIMES ==========
console.log('\n--- fs lutimes ---');

try {
    fs.writeFileSync('/tmp/test_lutimes_target.txt', 'target');
    try { fs.unlinkSync('/tmp/test_lutimes_link.txt'); } catch {}
    fs.symlinkSync('/tmp/test_lutimes_target.txt', '/tmp/test_lutimes_link.txt');
    const newTime = new Date('2019-01-15T12:00:00Z');
    fs.lutimesSync('/tmp/test_lutimes_link.txt', newTime, newTime);
    const stats = fs.lstatSync('/tmp/test_lutimes_link.txt');
    test('fs.lutimesSync works', stats.mtime.getUTCFullYear() === 2019);
} catch (e) {
    console.log('lutimes error:', e.message);
    test('fs.lutimesSync', false);
}

// ========== FS STATFS ==========
console.log('\n--- fs statfs ---');

try {
    const stats = fs.statfsSync('/');
    test('fs.statfsSync returns object', typeof stats === 'object');
    test('fs.statfsSync has bsize', typeof stats.bsize === 'number');
    test('fs.statfsSync has blocks', typeof stats.blocks === 'number');
    test('fs.statfsSync has bfree', typeof stats.bfree === 'number');
    test('fs.statfsSync has bavail', typeof stats.bavail === 'number');
    console.log('  bsize:', stats.bsize);
    console.log('  blocks:', stats.blocks);
    console.log('  bfree:', stats.bfree);
    console.log('  bavail:', stats.bavail);
} catch (e) {
    console.log('statfs error:', e.message);
    test('fs.statfsSync', false);
}

// ========== OS CONSTANTS ==========
console.log('\n--- os.constants ---');

test('os.constants.signals exists', typeof os.constants.signals === 'object');
test('os.constants.signals.SIGTERM', os.constants.signals.SIGTERM === 15);
test('os.constants.signals.SIGKILL', os.constants.signals.SIGKILL === 9);
test('os.constants.signals.SIGINT', os.constants.signals.SIGINT === 2);
test('os.constants.signals.SIGHUP', os.constants.signals.SIGHUP === 1);
test('os.constants.signals.SIGPIPE', os.constants.signals.SIGPIPE === 13);

test('os.constants.errno exists', typeof os.constants.errno === 'object');
test('os.constants.errno.ENOENT', os.constants.errno.ENOENT === 2);
test('os.constants.errno.EACCES', os.constants.errno.EACCES === 13);
test('os.constants.errno.EEXIST', os.constants.errno.EEXIST === 17);
test('os.constants.errno.EINVAL', os.constants.errno.EINVAL === 22);
test('os.constants.errno.EPERM', os.constants.errno.EPERM === 1);

console.log('  SIGTERM:', os.constants.signals.SIGTERM);
console.log('  SIGKILL:', os.constants.signals.SIGKILL);
console.log('  ENOENT:', os.constants.errno.ENOENT);
console.log('  EACCES:', os.constants.errno.EACCES);

// ========== CRYPTO KEY OBJECTS ==========
console.log('\n--- crypto key objects ---');

test('crypto.createSecretKey exists', typeof crypto.createSecretKey === 'function');

try {
    const key = crypto.createSecretKey(Buffer.alloc(32));
    test('createSecretKey returns object', typeof key === 'object');
    test('createSecretKey type is secret', key.type === 'secret');
    test('createSecretKey symmetricKeySize', key.symmetricKeySize === 32);
    test('createSecretKey export works', key.export().length === 32);

    // Test JWK export
    const jwk = key.export({ format: 'jwk' });
    test('createSecretKey JWK export', jwk.kty === 'oct' && typeof jwk.k === 'string');
} catch (e) {
    console.log('createSecretKey error:', e.message);
    test('crypto.createSecretKey', false);
}

test('crypto.generateKeySync exists', typeof crypto.generateKeySync === 'function');

try {
    const key = crypto.generateKeySync('aes', { length: 256 });
    test('generateKeySync returns KeyObject', key.type === 'secret');
    test('generateKeySync correct size', key.symmetricKeySize === 32);

    const hmacKey = crypto.generateKeySync('hmac', { length: 512 });
    test('generateKeySync hmac key', hmacKey.symmetricKeySize === 64);
} catch (e) {
    console.log('generateKeySync error:', e.message);
    test('crypto.generateKeySync', false);
}

// ========== SUMMARY ==========
console.log('\n=================================');
console.log('Results: ' + passed + ' passed, ' + failed + ' failed');
console.log('=================================');
