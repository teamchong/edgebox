// Comprehensive Process polyfill tests
// Tests process.env, argv, cwd, platform, version, etc.

const {
    assertEqual, assertTrue, assertFalse, assertTypeOf,
    assertDefined, assertContains, summary
} = require('./helpers/assert.js');

console.log('=== Process Polyfill Tests ===\n');

// ============================================
// process object existence
// ============================================
console.log('--- process object ---');
assertDefined(process, 'process is defined');
assertDefined(globalThis.process, 'globalThis.process is defined');

// ============================================
// process.env
// ============================================
console.log('\n--- process.env ---');
assertDefined(process.env, 'process.env is defined');
assertTypeOf(process.env, 'object', 'process.env is object');

// Standard environment variables (may or may not exist)
console.log('PATH exists:', process.env.PATH !== undefined);
console.log('HOME exists:', process.env.HOME !== undefined);
console.log('USER exists:', process.env.USER !== undefined);

// Can read env vars
if (process.env.PATH) {
    assertTrue(process.env.PATH.length > 0, 'PATH has content');
}

// ============================================
// process.argv
// ============================================
console.log('\n--- process.argv ---');
assertDefined(process.argv, 'process.argv is defined');
assertTrue(Array.isArray(process.argv), 'process.argv is array');

// Should have at least the interpreter and script
console.log('argv length:', process.argv.length);
console.log('argv[0]:', process.argv[0] || '(empty)');
if (process.argv.length > 1) {
    console.log('argv[1]:', process.argv[1]);
}

// ============================================
// process.cwd()
// ============================================
console.log('\n--- process.cwd() ---');
if (typeof process.cwd === 'function') {
    const cwd = process.cwd();
    assertTypeOf(cwd, 'string', 'cwd() returns string');
    assertTrue(cwd.length > 0, 'cwd() is not empty');
    assertTrue(cwd.startsWith('/'), 'cwd() is absolute path');
    console.log('Current directory:', cwd);
} else {
    console.log('SKIP: process.cwd not available');
}

// ============================================
// process.platform
// ============================================
console.log('\n--- process.platform ---');
assertDefined(process.platform, 'process.platform is defined');
assertTypeOf(process.platform, 'string', 'process.platform is string');
console.log('Platform:', process.platform);

// Should be one of the known platforms
const knownPlatforms = ['darwin', 'linux', 'win32', 'freebsd', 'openbsd', 'sunos', 'aix', 'wasm'];
// Note: EdgeBox might report 'wasm' or custom platform
console.log('Platform is known or custom:', knownPlatforms.includes(process.platform) || process.platform.length > 0);

// ============================================
// process.arch
// ============================================
console.log('\n--- process.arch ---');
if (process.arch !== undefined) {
    assertTypeOf(process.arch, 'string', 'process.arch is string');
    console.log('Architecture:', process.arch);
} else {
    console.log('SKIP: process.arch not available');
}

// ============================================
// process.version
// ============================================
console.log('\n--- process.version ---');
if (process.version !== undefined) {
    assertTypeOf(process.version, 'string', 'process.version is string');
    console.log('Version:', process.version);
} else {
    console.log('SKIP: process.version not available');
}

// ============================================
// process.versions
// ============================================
console.log('\n--- process.versions ---');
if (process.versions !== undefined) {
    assertTypeOf(process.versions, 'object', 'process.versions is object');
    console.log('Versions:', JSON.stringify(process.versions));
} else {
    console.log('SKIP: process.versions not available');
}

// ============================================
// process.pid
// ============================================
console.log('\n--- process.pid ---');
if (process.pid !== undefined) {
    assertTypeOf(process.pid, 'number', 'process.pid is number');
    assertTrue(process.pid > 0, 'process.pid is positive');
    console.log('PID:', process.pid);
} else {
    console.log('SKIP: process.pid not available');
}

// ============================================
// process.ppid
// ============================================
console.log('\n--- process.ppid ---');
if (process.ppid !== undefined) {
    assertTypeOf(process.ppid, 'number', 'process.ppid is number');
    console.log('Parent PID:', process.ppid);
} else {
    console.log('SKIP: process.ppid not available');
}

// ============================================
// process.exit
// ============================================
console.log('\n--- process.exit ---');
assertTypeOf(process.exit, 'function', 'process.exit is function');
// Don't actually call process.exit(0) - it would end the test!

// ============================================
// process.stdout / process.stderr
// ============================================
console.log('\n--- process.stdout/stderr ---');
if (process.stdout !== undefined) {
    assertDefined(process.stdout, 'process.stdout is defined');
    if (typeof process.stdout.write === 'function') {
        process.stdout.write('stdout test\n');
        console.log('PASS: process.stdout.write works');
    }
} else {
    console.log('SKIP: process.stdout not available');
}

if (process.stderr !== undefined) {
    assertDefined(process.stderr, 'process.stderr is defined');
    if (typeof process.stderr.write === 'function') {
        // process.stderr.write('stderr test\n'); // Commented to avoid noise
        console.log('PASS: process.stderr.write is available');
    }
} else {
    console.log('SKIP: process.stderr not available');
}

// ============================================
// process.stdin
// ============================================
console.log('\n--- process.stdin ---');
if (process.stdin !== undefined) {
    assertDefined(process.stdin, 'process.stdin is defined');
    console.log('PASS: process.stdin is available');
} else {
    console.log('SKIP: process.stdin not available');
}

// ============================================
// process.nextTick
// ============================================
console.log('\n--- process.nextTick ---');
if (typeof process.nextTick === 'function') {
    let nextTickCalled = false;
    process.nextTick(() => {
        nextTickCalled = true;
    });
    // Note: nextTick is async, so we can't test synchronously
    console.log('PASS: process.nextTick is available');
} else {
    console.log('SKIP: process.nextTick not available');
}

// ============================================
// process.hrtime
// ============================================
console.log('\n--- process.hrtime ---');
if (typeof process.hrtime === 'function') {
    const start = process.hrtime();
    // Do some work
    let sum = 0;
    for (let i = 0; i < 10000; i++) sum += i;
    const diff = process.hrtime(start);

    assertTrue(Array.isArray(diff), 'hrtime returns array');
    assertEqual(diff.length, 2, 'hrtime array has 2 elements');
    assertTypeOf(diff[0], 'number', 'hrtime[0] is number (seconds)');
    assertTypeOf(diff[1], 'number', 'hrtime[1] is number (nanoseconds)');
    console.log('hrtime diff:', diff[0], 'seconds,', diff[1], 'nanoseconds');
} else {
    console.log('SKIP: process.hrtime not available');
}

// process.hrtime.bigint
if (typeof process.hrtime === 'function' && typeof process.hrtime.bigint === 'function') {
    const time = process.hrtime.bigint();
    assertTypeOf(time, 'bigint', 'hrtime.bigint returns bigint');
    console.log('PASS: process.hrtime.bigint works');
} else {
    console.log('SKIP: process.hrtime.bigint not available');
}

// ============================================
// process.memoryUsage
// ============================================
console.log('\n--- process.memoryUsage ---');
if (typeof process.memoryUsage === 'function') {
    const mem = process.memoryUsage();
    assertTypeOf(mem, 'object', 'memoryUsage returns object');
    if (mem.heapUsed !== undefined) {
        assertTypeOf(mem.heapUsed, 'number', 'heapUsed is number');
    }
    if (mem.heapTotal !== undefined) {
        assertTypeOf(mem.heapTotal, 'number', 'heapTotal is number');
    }
    console.log('Memory usage:', JSON.stringify(mem));
} else {
    console.log('SKIP: process.memoryUsage not available');
}

// ============================================
// process.uptime
// ============================================
console.log('\n--- process.uptime ---');
if (typeof process.uptime === 'function') {
    const uptime = process.uptime();
    assertTypeOf(uptime, 'number', 'uptime returns number');
    assertTrue(uptime >= 0, 'uptime is non-negative');
    console.log('Uptime:', uptime, 'seconds');
} else {
    console.log('SKIP: process.uptime not available');
}

// ============================================
// process.on / process.once (event handling)
// ============================================
console.log('\n--- process.on/once ---');
if (typeof process.on === 'function') {
    console.log('PASS: process.on is available');

    // Register exit handler (won't be called in this test)
    // process.on('exit', () => console.log('Exit handler'));
} else {
    console.log('SKIP: process.on not available');
}

// ============================================
// process.chdir (change directory)
// ============================================
console.log('\n--- process.chdir ---');
if (typeof process.chdir === 'function' && typeof process.cwd === 'function') {
    const originalDir = process.cwd();
    // Don't actually change directory in tests
    console.log('PASS: process.chdir is available');
} else {
    console.log('SKIP: process.chdir not available');
}

// ============================================
// process.execPath
// ============================================
console.log('\n--- process.execPath ---');
if (process.execPath !== undefined) {
    assertTypeOf(process.execPath, 'string', 'execPath is string');
    console.log('Exec path:', process.execPath);
} else {
    console.log('SKIP: process.execPath not available');
}

// ============================================
// process.title
// ============================================
console.log('\n--- process.title ---');
if (process.title !== undefined) {
    assertTypeOf(process.title, 'string', 'title is string');
    console.log('Process title:', process.title);
} else {
    console.log('SKIP: process.title not available');
}

summary();
