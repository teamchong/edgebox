#!/usr/bin/env node
// EdgeBox TSC - TypeScript Compiler
// Entry point for compiling TypeScript files

const ts = require('../../bench/typescript/lib/typescript.js');

// Get command line arguments (skip node and script path)
const args = process.argv.slice(2);

// If no args, show version and help
if (args.length === 0) {
    console.log(`TypeScript ${ts.version}`);
    console.log('Usage: tsc [options] [file...]');
    console.log('');
    console.log('Options:');
    console.log('  --version, -v    Print the compiler version');
    console.log('  --help, -h       Print this help message');
    console.log('  --noEmit         Do not emit outputs');
    console.log('  --outDir <dir>   Redirect output to directory');
    process.exit(0);
}

// Handle --version
if (args.includes('--version') || args.includes('-v')) {
    console.log(`Version ${ts.version}`);
    process.exit(0);
}

// Handle --help
if (args.includes('--help') || args.includes('-h')) {
    console.log(`TypeScript ${ts.version}`);
    console.log('Usage: tsc [options] [file...]');
    console.log('');
    console.log('Options:');
    console.log('  --version, -v    Print the compiler version');
    console.log('  --help, -h       Print this help message');
    console.log('  --noEmit         Do not emit outputs');
    console.log('  --outDir <dir>   Redirect output to directory');
    process.exit(0);
}

// Parse options and files
let outDir = null;
let noEmit = false;
const files = [];

for (let i = 0; i < args.length; i++) {
    const arg = args[i];
    if (arg === '--outDir' && i + 1 < args.length) {
        outDir = args[++i];
    } else if (arg === '--noEmit') {
        noEmit = true;
    } else if (!arg.startsWith('-')) {
        files.push(arg);
    }
}

// Compile each file
const fs = require('fs');
const path = require('path');

for (const file of files) {
    try {
        const sourceCode = fs.readFileSync(file, 'utf8');

        const result = ts.transpileModule(sourceCode, {
            compilerOptions: {
                module: ts.ModuleKind.CommonJS,
                target: ts.ScriptTarget.ES2020,
                strict: true,
            },
            fileName: file,
        });

        if (!noEmit && result.outputText) {
            const baseName = path.basename(file, '.ts');
            const outPath = outDir
                ? path.join(outDir, baseName + '.js')
                : file.replace(/\.ts$/, '.js');

            // Create output directory if needed
            if (outDir && !fs.existsSync(outDir)) {
                fs.mkdirSync(outDir, { recursive: true });
            }

            fs.writeFileSync(outPath, result.outputText);
            console.log(`Compiled: ${file} -> ${outPath}`);
        }

        if (result.diagnostics && result.diagnostics.length > 0) {
            for (const diag of result.diagnostics) {
                console.error(ts.flattenDiagnosticMessageText(diag.messageText, '\n'));
            }
        }
    } catch (err) {
        console.error(`Error compiling ${file}: ${err.message}`);
        process.exit(1);
    }
}
