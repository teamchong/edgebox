#!/usr/bin/env node
// Manifest Generator for EdgeBox Frozen Functions
// NO JS MODIFICATIONS - just generates manifest for build system
// Native dispatch handled entirely in Zig via quickjs.c patch

const fs = require('fs');

const inputFile = process.argv[2];
const outputFile = process.argv[3] || inputFile;
const manifestFile = process.argv[4];

if (!inputFile) {
    console.error('Usage: inject_hooks.js <input.js> [output.js] [manifest.json]');
    process.exit(1);
}

const code = fs.readFileSync(inputFile, 'utf8');

// Just copy input to output - NO modifications
fs.writeFileSync(outputFile, code);

// Generate manifest if requested
if (manifestFile) {
    const identified = [];
    const funcNameRegex = /function\s+(\w+)\s*\(/g;
    let match;

    while ((match = funcNameRegex.exec(code)) !== null) {
        identified.push({ name: match[1], argCount: 0 });
    }

    fs.writeFileSync(manifestFile, JSON.stringify({ functions: identified }, null, 2));
    console.log(`[manifest] Identified ${identified.length} functions`);
}
