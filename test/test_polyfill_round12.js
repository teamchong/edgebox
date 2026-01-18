console.log('=== Round 12 Tests ===\n');

let passed = 0, failed = 0;
function test(name, condition) {
    if (condition) { console.log('PASS:', name); passed++; }
    else { console.log('FAIL:', name); failed++; }
}

const fs = require('fs');
const os = require('os');

// ========== FS CHMOD ==========
console.log('\n--- fs chmod ---');

try {
    fs.writeFileSync('/tmp/test_chmod.txt', 'test');
    fs.chmodSync('/tmp/test_chmod.txt', 0o644);
    const stats = fs.statSync('/tmp/test_chmod.txt');
    test('fs.chmodSync works', (stats.mode & 0o777) === 0o644);

    // Test 755
    fs.chmodSync('/tmp/test_chmod.txt', 0o755);
    const stats2 = fs.statSync('/tmp/test_chmod.txt');
    test('fs.chmodSync 755 works', (stats2.mode & 0o777) === 0o755);
} catch (e) {
    console.log('chmod error:', e.message);
    test('fs.chmodSync', false);
}

// ========== FS CHOWN ==========
console.log('\n--- fs chown ---');

try {
    fs.writeFileSync('/tmp/test_chown.txt', 'test');
    const uid = process.getuid();
    const gid = process.getgid();
    // chown to current user (should succeed without root)
    fs.chownSync('/tmp/test_chown.txt', uid, gid);
    test('fs.chownSync works (self)', true);
} catch (e) {
    console.log('chown error:', e.message);
    test('fs.chownSync', false);
}

// ========== FS UTIMES ==========
console.log('\n--- fs utimes ---');

try {
    fs.writeFileSync('/tmp/test_utimes.txt', 'test');
    const newTime = new Date('2020-06-15T12:00:00Z'); // Use mid-year date to avoid timezone edge cases
    fs.utimesSync('/tmp/test_utimes.txt', newTime, newTime);
    const stats = fs.statSync('/tmp/test_utimes.txt');
    // Use getUTCFullYear to avoid timezone issues
    test('fs.utimesSync works', stats.mtime.getUTCFullYear() === 2020);
} catch (e) {
    console.log('utimes error:', e.message);
    test('fs.utimesSync', false);
}

// ========== FS SYMLINK/READLINK ==========
console.log('\n--- fs symlink/readlink ---');

try {
    fs.writeFileSync('/tmp/test_link_target.txt', 'target');
    try { fs.unlinkSync('/tmp/test_symlink.txt'); } catch {}

    fs.symlinkSync('/tmp/test_link_target.txt', '/tmp/test_symlink.txt');
    test('fs.symlinkSync creates link', fs.existsSync('/tmp/test_symlink.txt'));

    const target = fs.readlinkSync('/tmp/test_symlink.txt');
    test('fs.readlinkSync reads target', target === '/tmp/test_link_target.txt');

    const lstat = fs.lstatSync('/tmp/test_symlink.txt');
    test('fs.lstatSync isSymbolicLink', lstat.isSymbolicLink());
} catch (e) {
    console.log('symlink error:', e.message);
    test('fs.symlinkSync/readlinkSync', false);
}

// ========== FS LINK (hard link) ==========
console.log('\n--- fs link ---');

try {
    fs.writeFileSync('/tmp/test_hardlink_src.txt', 'source');
    try { fs.unlinkSync('/tmp/test_hardlink.txt'); } catch {}

    fs.linkSync('/tmp/test_hardlink_src.txt', '/tmp/test_hardlink.txt');
    test('fs.linkSync creates hard link', fs.existsSync('/tmp/test_hardlink.txt'));

    const content = fs.readFileSync('/tmp/test_hardlink.txt', 'utf8');
    test('fs.linkSync content matches', content === 'source');
} catch (e) {
    console.log('link error:', e.message);
    test('fs.linkSync', false);
}

// ========== FS MKDTEMP ==========
console.log('\n--- fs mkdtemp ---');

try {
    const tmpDir = fs.mkdtempSync('/tmp/edgebox-test-');
    test('fs.mkdtempSync returns path', tmpDir.startsWith('/tmp/edgebox-test-'));
    test('fs.mkdtempSync creates dir', fs.statSync(tmpDir).isDirectory());
    fs.rmdirSync(tmpDir);
} catch (e) {
    console.log('mkdtemp error:', e.message);
    test('fs.mkdtempSync', false);
}

// ========== FS COPYFILE ==========
console.log('\n--- fs copyFile ---');

try {
    fs.writeFileSync('/tmp/test_copy_src.txt', 'copy me');
    try { fs.unlinkSync('/tmp/test_copy_dest.txt'); } catch {}

    fs.copyFileSync('/tmp/test_copy_src.txt', '/tmp/test_copy_dest.txt');
    test('fs.copyFileSync creates dest', fs.existsSync('/tmp/test_copy_dest.txt'));

    const content = fs.readFileSync('/tmp/test_copy_dest.txt', 'utf8');
    test('fs.copyFileSync content matches', content === 'copy me');
} catch (e) {
    console.log('copyFile error:', e.message);
    test('fs.copyFileSync', false);
}

// ========== PROCESS SETUID/SETGID ==========
console.log('\n--- process setuid/setgid ---');

test('process.setuid exists', typeof process.setuid === 'function');
test('process.setgid exists', typeof process.setgid === 'function');
test('process.seteuid exists', typeof process.seteuid === 'function');
test('process.setegid exists', typeof process.setegid === 'function');

// Can't actually test setuid without root - just verify functions exist

// ========== OS CPUS ==========
console.log('\n--- os cpus ---');

try {
    const cpus = os.cpus();
    test('os.cpus returns array', Array.isArray(cpus));
    test('os.cpus has entries', cpus.length > 0);
    test('os.cpus has model', cpus[0] && typeof cpus[0].model === 'string');
    test('os.cpus has speed', cpus[0] && typeof cpus[0].speed === 'number');
    console.log('  CPU count:', cpus.length);
    console.log('  CPU model:', cpus[0]?.model);
    console.log('  CPU speed:', cpus[0]?.speed, 'MHz');
} catch (e) {
    console.log('cpus error:', e.message);
    test('os.cpus', false);
}

// ========== SUMMARY ==========
console.log('\n=================================');
console.log('Results: ' + passed + ' passed, ' + failed + ' failed');
console.log('=================================');
