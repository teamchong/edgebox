// Test child_process.spawnSync and execSync
const { spawnSync, execSync } = require('child_process');

console.log('=== Testing child_process ===');

// Test 1: spawnSync with echo
console.log('\n1. Testing spawnSync with echo:');
try {
    const result = spawnSync('echo', ['Hello', 'from', 'child_process']);
    console.log('  stdout:', result.stdout);
    console.log('  status:', result.status);
    console.log('  ✅ spawnSync works!');
} catch (e) {
    console.log('  ❌ Error:', e.message);
}

// Test 2: execSync
console.log('\n2. Testing execSync:');
try {
    const output = execSync('echo "Hello from execSync"');
    console.log('  output:', output);
    console.log('  ✅ execSync works!');
} catch (e) {
    console.log('  ❌ Error:', e.message);
}

// Test 3: git status
console.log('\n3. Testing git status:');
try {
    const result = spawnSync('git', ['status', '--short']);
    console.log('  stdout:', result.stdout.substring(0, 100) + '...');
    console.log('  status:', result.status);
    console.log('  ✅ git works!');
} catch (e) {
    console.log('  ❌ Error:', e.message);
}

// Test 4: ls
console.log('\n4. Testing ls:');
try {
    const result = spawnSync('ls', ['-la']);
    console.log('  stdout (first 200 chars):', result.stdout.substring(0, 200) + '...');
    console.log('  ✅ ls works!');
} catch (e) {
    console.log('  ❌ Error:', e.message);
}

console.log('\n=== All tests completed ===');
