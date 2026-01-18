console.log('=== Round 13 Tests ===\n');

let passed = 0, failed = 0;
function test(name, condition) {
    if (condition) { console.log('PASS:', name); passed++; }
    else { console.log('FAIL:', name); failed++; }
}

const fs = require('fs');
const os = require('os');

// ========== OS MODULE ==========
console.log('\n--- os module ---');

test('os.devNull exists', os.devNull === '/dev/null');
test('os.machine returns string', typeof os.machine() === 'string' && os.machine().length > 0);
test('os.version returns string', typeof os.version() === 'string' && os.version().length > 0);
test('os.availableParallelism returns number', typeof os.availableParallelism() === 'number' && os.availableParallelism() > 0);

console.log('  machine:', os.machine());
console.log('  version:', os.version());
console.log('  parallelism:', os.availableParallelism());

try {
    const prio = os.getPriority();
    test('os.getPriority works', typeof prio === 'number');
    console.log('  priority:', prio);
} catch (e) {
    console.log('getPriority error:', e.message);
    test('os.getPriority', false);
}

// ========== PROCESS MODULE ==========
console.log('\n--- process module ---');

test('process.argv0 exists', typeof process.argv0 === 'string');
test('process.initgroups exists', typeof process.initgroups === 'function');
test('process.setgroups exists', typeof process.setgroups === 'function');

try {
    const usage = process.resourceUsage();
    test('process.resourceUsage returns object', typeof usage === 'object');
    test('process.resourceUsage has userCPUTime', typeof usage.userCPUTime === 'number');
    test('process.resourceUsage has systemCPUTime', typeof usage.systemCPUTime === 'number');
    test('process.resourceUsage has maxRSS', typeof usage.maxRSS === 'number');
    console.log('  userCPUTime:', usage.userCPUTime, 'μs');
    console.log('  systemCPUTime:', usage.systemCPUTime, 'μs');
    console.log('  maxRSS:', usage.maxRSS, 'bytes');
} catch (e) {
    console.log('resourceUsage error:', e.message);
    test('process.resourceUsage', false);
}

// ========== FS MODULE ==========
console.log('\n--- fs module ---');

test('fs.fchmodSync exists', typeof fs.fchmodSync === 'function');
test('fs.fchownSync exists', typeof fs.fchownSync === 'function');

try {
    fs.writeFileSync('/tmp/test_fchmod.txt', 'test');
    const fd = fs.openSync('/tmp/test_fchmod.txt', 'r+');
    fs.fchmodSync(fd, 0o644);
    fs.closeSync(fd);
    const stats = fs.statSync('/tmp/test_fchmod.txt');
    test('fs.fchmodSync works', (stats.mode & 0o777) === 0o644);

    // Test 755
    const fd2 = fs.openSync('/tmp/test_fchmod.txt', 'r+');
    fs.fchmodSync(fd2, 0o755);
    fs.closeSync(fd2);
    const stats2 = fs.statSync('/tmp/test_fchmod.txt');
    test('fs.fchmodSync 755 works', (stats2.mode & 0o777) === 0o755);
} catch (e) {
    console.log('fchmod error:', e.message);
    test('fs.fchmodSync', false);
}

try {
    fs.writeFileSync('/tmp/test_fchown.txt', 'test');
    const fd = fs.openSync('/tmp/test_fchown.txt', 'r+');
    const uid = process.getuid();
    const gid = process.getgid();
    // fchown to current user (should succeed without root)
    fs.fchownSync(fd, uid, gid);
    fs.closeSync(fd);
    test('fs.fchownSync works (self)', true);
} catch (e) {
    console.log('fchown error:', e.message);
    test('fs.fchownSync', false);
}

// ========== SUMMARY ==========
console.log('\n=================================');
console.log('Results: ' + passed + ' passed, ' + failed + ' failed');
console.log('=================================');
