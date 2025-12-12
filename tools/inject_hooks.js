#!/usr/bin/env node
// Interceptor Hook Injector for EdgeBox Frozen Functions
// Injects globalThis.__frozen_NAME checks at function entry points
// This enables zero-config frozen function optimization for ALL compatible functions

const fs = require('fs');

const inputFile = process.argv[2];
const outputFile = process.argv[3] || inputFile;
const manifestFile = process.argv[4]; // Optional: output manifest

if (!inputFile) {
    console.error('Usage: inject_hooks.js <input.js> [output.js] [manifest.json]');
    process.exit(1);
}

const code = fs.readFileSync(inputFile, 'utf8');

// Track injected functions with metadata
const injected = [];

// Find all named functions
const funcRegex = /function\s+(\w+)\s*\(([^)]*)\)\s*\{/g;
let match;
const functions = [];

while ((match = funcRegex.exec(code)) !== null) {
    const name = match[1];
    const args = match[2];
    const startIdx = match.index + match[0].length;

    // Find matching closing brace to get function body
    let braceCount = 1;
    let i = startIdx;
    let inString = null;
    while (i < code.length && braceCount > 0) {
        const c = code[i];
        if (inString) {
            if (c === inString && code[i-1] !== '\\') inString = null;
        } else {
            if (c === '"' || c === "'" || c === '`') inString = c;
            else if (c === '{') braceCount++;
            else if (c === '}') braceCount--;
        }
        i++;
    }

    const bodyLength = i - startIdx - 1;

    // Check for self-recursion (for metadata)
    const body = code.slice(startIdx, i - 1);
    const selfCallRegex = new RegExp(`\\b${name}\\s*\\(`);
    const isSelfRecursive = selfCallRegex.test(body);

    functions.push({
        name,
        args,
        matchStart: match.index,
        matchEnd: match.index + match[0].length,
        bodyLength,
        isSelfRecursive
    });
}

// Second pass: inject hooks for ALL named functions
// The freeze tool will decide which ones can actually be frozen
let offset = 0;
let patched = code;

for (const func of functions) {
    // Skip common runtime/module patterns
    const skipNames = ['require', 'exports', 'module', 'define', '__webpack', '__esModule'];
    if (func.name.length < 1 || skipNames.some(s => func.name.startsWith(s))) {
        continue;
    }

    // Skip very small functions (< 20 chars body) - not worth the hook overhead
    if (func.bodyLength < 20) {
        continue;
    }

    const argCount = func.args.trim() ? func.args.split(',').length : 0;
    injected.push({ name: func.name, argCount, isSelfRecursive: func.isSelfRecursive });

    // Generate hook: check for frozen version, delegate if exists
    const hook = `if(globalThis.__frozen_${func.name})return globalThis.__frozen_${func.name}(${func.args});`;

    // Insert hook at function body start
    const insertPos = func.matchEnd + offset;
    patched = patched.slice(0, insertPos) + hook + patched.slice(insertPos);
    offset += hook.length;
}

fs.writeFileSync(outputFile, patched);

// Write manifest if requested
if (manifestFile) {
    fs.writeFileSync(manifestFile, JSON.stringify({ functions: injected }, null, 2));
}

if (injected.length > 0) {
    console.log(`[inject_hooks] Injected ${injected.length} function hooks (general purpose)`);
}
