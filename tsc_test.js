// TypeScript Compiler Test for EdgeBox
// Uses TypeScript programmatic API to compile .ts files

const fs = require('fs');
const ts = require('typescript');

console.log('TypeScript version:', ts.version);

// Create test file
const testDir = '/tmp/tsc_edgebox_test';
try { fs.mkdirSync(testDir, { recursive: true }); } catch(e) {}
try { fs.mkdirSync(testDir + '/out', { recursive: true }); } catch(e) {}

const inputFile = testDir + '/test.ts';
fs.writeFileSync(inputFile, `
interface User {
    name: string;
    age: number;
}

function greet(user: User): string {
    return \`Hello, \${user.name}! You are \${user.age} years old.\`;
}

const alice: User = { name: "Alice", age: 30 };
console.log(greet(alice));
`);

console.log('Input:', inputFile);

// Use TypeScript API
const program = ts.createProgram([inputFile], {
    outDir: testDir + '/out',
    module: ts.ModuleKind.CommonJS,
    target: ts.ScriptTarget.ES2020
});

console.log('Source files:', program.getSourceFiles().length);

const emitResult = program.emit();

console.log('Emit result:');
console.log('  emitSkipped:', emitResult.emitSkipped);
console.log('  diagnostics:', emitResult.diagnostics.length);

// Show output
const files = fs.readdirSync(testDir + '/out');
console.log('Output files:', files);
files.forEach(f => {
    console.log('--- ' + f + ' ---');
    console.log(fs.readFileSync(testDir + '/out/' + f, 'utf8'));
});

console.log('Done!');
