// OS polyfill tests - comprehensive coverage
const { assertEqual, assertTrue, assertFalse, assertTypeOf, summary } = require('./helpers/assert.js');

const os = require('os');

console.log('=== OS Polyfill Tests ===\n');

// ============================================
// os.platform
// ============================================
console.log('--- os.platform ---');

assertTypeOf(os.platform, 'function', 'os.platform is function');
const platform = os.platform();
assertTypeOf(platform, 'string', 'os.platform() returns string');
assertTrue(['aix', 'darwin', 'freebsd', 'linux', 'openbsd', 'sunos', 'win32', 'android'].includes(platform) || platform.length > 0,
    'os.platform() returns valid platform: ' + platform);

// ============================================
// os.arch
// ============================================
console.log('\n--- os.arch ---');

assertTypeOf(os.arch, 'function', 'os.arch is function');
const arch = os.arch();
assertTypeOf(arch, 'string', 'os.arch() returns string');
assertTrue(['arm', 'arm64', 'ia32', 'mips', 'mipsel', 'ppc', 'ppc64', 's390', 's390x', 'x32', 'x64', 'aarch64', 'wasm32'].includes(arch) || arch.length > 0,
    'os.arch() returns valid arch: ' + arch);

// ============================================
// os.type
// ============================================
console.log('\n--- os.type ---');

assertTypeOf(os.type, 'function', 'os.type is function');
const osType = os.type();
assertTypeOf(osType, 'string', 'os.type() returns string');
assertTrue(osType.length > 0, 'os.type() returns non-empty string: ' + osType);

// ============================================
// os.release
// ============================================
console.log('\n--- os.release ---');

assertTypeOf(os.release, 'function', 'os.release is function');
const release = os.release();
assertTypeOf(release, 'string', 'os.release() returns string');

// ============================================
// os.version
// ============================================
console.log('\n--- os.version ---');

if (typeof os.version === 'function') {
    const version = os.version();
    assertTypeOf(version, 'string', 'os.version() returns string');
} else {
    console.log('SKIP: os.version not available');
}

// ============================================
// os.hostname
// ============================================
console.log('\n--- os.hostname ---');

assertTypeOf(os.hostname, 'function', 'os.hostname is function');
const hostname = os.hostname();
assertTypeOf(hostname, 'string', 'os.hostname() returns string');

// ============================================
// os.homedir
// ============================================
console.log('\n--- os.homedir ---');

assertTypeOf(os.homedir, 'function', 'os.homedir is function');
const homedir = os.homedir();
assertTypeOf(homedir, 'string', 'os.homedir() returns string');
assertTrue(homedir.length > 0, 'os.homedir() returns non-empty path');

// ============================================
// os.tmpdir
// ============================================
console.log('\n--- os.tmpdir ---');

assertTypeOf(os.tmpdir, 'function', 'os.tmpdir is function');
const tmpdir = os.tmpdir();
assertTypeOf(tmpdir, 'string', 'os.tmpdir() returns string');
assertTrue(tmpdir.length > 0, 'os.tmpdir() returns non-empty path');

// ============================================
// os.cpus
// ============================================
console.log('\n--- os.cpus ---');

assertTypeOf(os.cpus, 'function', 'os.cpus is function');
const cpus = os.cpus();
assertTrue(Array.isArray(cpus), 'os.cpus() returns array');
if (cpus.length > 0) {
    const cpu = cpus[0];
    assertTypeOf(cpu.model, 'string', 'cpu.model is string');
    assertTypeOf(cpu.speed, 'number', 'cpu.speed is number');
    assertTrue(cpu.times !== undefined, 'cpu.times exists');
}

// ============================================
// os.totalmem
// ============================================
console.log('\n--- os.totalmem ---');

assertTypeOf(os.totalmem, 'function', 'os.totalmem is function');
const totalmem = os.totalmem();
assertTypeOf(totalmem, 'number', 'os.totalmem() returns number');
assertTrue(totalmem > 0, 'os.totalmem() returns positive value');

// ============================================
// os.freemem
// ============================================
console.log('\n--- os.freemem ---');

assertTypeOf(os.freemem, 'function', 'os.freemem is function');
const freemem = os.freemem();
assertTypeOf(freemem, 'number', 'os.freemem() returns number');
assertTrue(freemem >= 0, 'os.freemem() returns non-negative value');

// ============================================
// os.uptime
// ============================================
console.log('\n--- os.uptime ---');

assertTypeOf(os.uptime, 'function', 'os.uptime is function');
const uptime = os.uptime();
assertTypeOf(uptime, 'number', 'os.uptime() returns number');
assertTrue(uptime >= 0, 'os.uptime() returns non-negative value');

// ============================================
// os.loadavg
// ============================================
console.log('\n--- os.loadavg ---');

assertTypeOf(os.loadavg, 'function', 'os.loadavg is function');
const loadavg = os.loadavg();
assertTrue(Array.isArray(loadavg), 'os.loadavg() returns array');
assertEqual(loadavg.length, 3, 'os.loadavg() returns 3 values');

// ============================================
// os.networkInterfaces
// ============================================
console.log('\n--- os.networkInterfaces ---');

assertTypeOf(os.networkInterfaces, 'function', 'os.networkInterfaces is function');
const interfaces = os.networkInterfaces();
assertTypeOf(interfaces, 'object', 'os.networkInterfaces() returns object');

// ============================================
// os.userInfo
// ============================================
console.log('\n--- os.userInfo ---');

if (typeof os.userInfo === 'function') {
    const userInfo = os.userInfo();
    assertTypeOf(userInfo, 'object', 'os.userInfo() returns object');
    assertTypeOf(userInfo.username, 'string', 'userInfo.username is string');
    assertTypeOf(userInfo.homedir, 'string', 'userInfo.homedir is string');
} else {
    console.log('SKIP: os.userInfo not available');
}

// ============================================
// os.endianness
// ============================================
console.log('\n--- os.endianness ---');

assertTypeOf(os.endianness, 'function', 'os.endianness is function');
const endianness = os.endianness();
assertTrue(endianness === 'BE' || endianness === 'LE', 'os.endianness() returns BE or LE');

// ============================================
// os.EOL
// ============================================
console.log('\n--- os.EOL ---');

assertTrue(os.EOL !== undefined, 'os.EOL exists');
assertTypeOf(os.EOL, 'string', 'os.EOL is string');
assertTrue(os.EOL === '\n' || os.EOL === '\r\n', 'os.EOL is valid line ending');

// ============================================
// os.constants
// ============================================
console.log('\n--- os.constants ---');

if (os.constants) {
    assertTypeOf(os.constants, 'object', 'os.constants is object');
    if (os.constants.signals) {
        assertTypeOf(os.constants.signals, 'object', 'os.constants.signals is object');
    }
    if (os.constants.errno) {
        assertTypeOf(os.constants.errno, 'object', 'os.constants.errno is object');
    }
} else {
    console.log('SKIP: os.constants not available');
}

// ============================================
// os.devNull
// ============================================
console.log('\n--- os.devNull ---');

if (os.devNull !== undefined) {
    assertTypeOf(os.devNull, 'string', 'os.devNull is string');
    assertTrue(os.devNull === '/dev/null' || os.devNull === '\\\\.\\nul', 'os.devNull is valid');
} else {
    console.log('SKIP: os.devNull not available');
}

// ============================================
// os.getPriority / os.setPriority
// ============================================
console.log('\n--- os.getPriority / os.setPriority ---');

if (typeof os.getPriority === 'function') {
    const priority = os.getPriority();
    assertTypeOf(priority, 'number', 'os.getPriority() returns number');
}

if (typeof os.setPriority === 'function') {
    assertTypeOf(os.setPriority, 'function', 'os.setPriority is function');
}

// ============================================
// os.machine
// ============================================
console.log('\n--- os.machine ---');

if (typeof os.machine === 'function') {
    const machine = os.machine();
    assertTypeOf(machine, 'string', 'os.machine() returns string');
} else {
    console.log('SKIP: os.machine not available');
}

summary();
