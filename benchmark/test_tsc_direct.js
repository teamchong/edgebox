// Direct test of how tsc uses fs
const fs = require('fs');

// Log all calls to writeFileSync
const origWrite = fs.writeFileSync;
fs.writeFileSync = function(path, data, options) {
    console.log('[INTERCEPT] fs.writeFileSync:', path, 'len=' + (data ? data.length : 0));
    return origWrite.call(this, path, data, options);
};

// Compile a simple file
const ts = require('typescript');
console.log('TypeScript version:', ts.version);

const result = ts.transpileModule('const x: number = 42; console.log(x);', {
    compilerOptions: { module: ts.ModuleKind.CommonJS }
});
console.log('Transpiled output:', result.outputText.substring(0, 100));
