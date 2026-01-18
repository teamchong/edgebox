const fs = require('fs');
const path = require('path');

// Create test input file
const inputDir = '/tmp/ts_emit_test';
const outputDir = '/tmp/ts_emit_out';

// Ensure dirs exist
try { fs.mkdirSync(inputDir, { recursive: true }); } catch(e) {}
try { fs.mkdirSync(outputDir, { recursive: true }); } catch(e) {}

// Write test file
fs.writeFileSync(inputDir + '/test.ts', 'const x: number = 42; export default x;');
console.log('Created test file');

// Log all fs.writeFileSync calls
const origWrite = fs.writeFileSync;
let writeCount = 0;
fs.writeFileSync = function(p, data, options) {
    writeCount++;
    console.log('[INTERCEPT #' + writeCount + '] fs.writeFileSync:', p);
    return origWrite.call(this, p, data, options);
};

// Use TypeScript program API
const ts = require('typescript');
console.log('TypeScript version:', ts.version);

const configPath = ts.findConfigFile(inputDir, ts.sys.fileExists, 'tsconfig.json');
console.log('Config path:', configPath);

// Create program without config file
const program = ts.createProgram([inputDir + '/test.ts'], {
    outDir: outputDir,
    module: ts.ModuleKind.ESNext,
    target: ts.ScriptTarget.ES2020,
    declaration: true
});

console.log('Program created, source files:', program.getSourceFiles().length);

const emitResult = program.emit();
console.log('Emit result:');
console.log('  - emitSkipped:', emitResult.emitSkipped);
console.log('  - diagnostics:', emitResult.diagnostics.length);
emitResult.diagnostics.forEach(d => {
    console.log('    DIAG:', ts.flattenDiagnosticMessageText(d.messageText, '\n'));
});

console.log('Write calls intercepted:', writeCount);

// Check output
try {
    const files = fs.readdirSync(outputDir);
    console.log('Output files:', files);
} catch(e) {
    console.log('Error reading output dir:', e.message);
}
