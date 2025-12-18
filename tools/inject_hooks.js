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

// Find all named functions - must handle nested parens in default params
// e.g., function foo(a, b = function() {}) { ... }
const funcNameRegex = /function\s+(\w+)\s*\(/g;
let match;
const functions = [];

while ((match = funcNameRegex.exec(code)) !== null) {
    const name = match[1];
    const argsStart = match.index + match[0].length;

    // Find matching closing paren (handling nested parens in default params)
    let parenCount = 1;
    let i = argsStart;
    let inString = null;
    while (i < code.length && parenCount > 0) {
        const c = code[i];
        if (inString) {
            if (c === inString && code[i-1] !== '\\') inString = null;
        } else {
            if (c === '"' || c === "'" || c === '`') inString = c;
            else if (c === '(') parenCount++;
            else if (c === ')') parenCount--;
        }
        i++;
    }
    const argsEnd = i - 1;
    const args = code.slice(argsStart, argsEnd);

    // Skip to opening brace
    let j = i;
    while (j < code.length && code[j] !== '{') j++;
    if (j >= code.length) continue;

    const bodyStart = j + 1; // Position after opening brace

    // Find matching closing brace to get function body
    let braceCount = 1;
    let k = bodyStart;
    let inString2 = null;
    while (k < code.length && braceCount > 0) {
        const c = code[k];
        if (inString2) {
            if (c === inString2 && code[k-1] !== '\\') inString2 = null;
        } else {
            if (c === '"' || c === "'" || c === '`') inString2 = c;
            else if (c === '{') braceCount++;
            else if (c === '}') braceCount--;
        }
        k++;
    }

    const bodyLength = k - bodyStart - 1;

    // Check for self-recursion (for metadata)
    const body = code.slice(bodyStart, k - 1);
    const selfCallRegex = new RegExp(`\\b${name}\\s*\\(`);
    const isSelfRecursive = selfCallRegex.test(body);

    functions.push({
        name,
        args,
        matchStart: match.index,
        matchEnd: bodyStart, // Position after the opening brace
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

    // Skip functions with destructuring params - they can't use simple frozen hooks
    if (func.args.includes('{') || func.args.includes('[')) {
        continue;
    }

    // Extract argument names properly handling strings and nested parens in default values
    const argNames = [];
    if (func.args.trim()) {
        let depth = 0;
        let inString = null;
        let current = '';
        for (let ci = 0; ci < func.args.length; ci++) {
            const ch = func.args[ci];
            if (inString) {
                if (ch === inString && func.args[ci-1] !== '\\') inString = null;
                current += ch;
            } else {
                if (ch === '"' || ch === "'" || ch === '`') {
                    inString = ch;
                    current += ch;
                } else if (ch === '(' || ch === '{' || ch === '[') {
                    depth++;
                    current += ch;
                } else if (ch === ')' || ch === '}' || ch === ']') {
                    depth--;
                    current += ch;
                } else if (ch === ',' && depth === 0) {
                    // Argument separator - extract name
                    const trimmed = current.trim();
                    const eqIdx = trimmed.indexOf('=');
                    let namePart = eqIdx >= 0 ? trimmed.slice(0, eqIdx).trim() : trimmed;
                    if (namePart.startsWith('...')) namePart = namePart.slice(3);
                    if (namePart) argNames.push(namePart);
                    current = '';
                } else {
                    current += ch;
                }
            }
        }
        // Last argument
        if (current.trim()) {
            const trimmed = current.trim();
            const eqIdx = trimmed.indexOf('=');
            let namePart = eqIdx >= 0 ? trimmed.slice(0, eqIdx).trim() : trimmed;
            if (namePart.startsWith('...')) namePart = namePart.slice(3);
            if (namePart) argNames.push(namePart);
        }
    }

    const argCount = argNames.length;
    injected.push({ name: func.name, argCount, isSelfRecursive: func.isSelfRecursive });

    // Generate hook: check for frozen version, delegate if exists
    // Also check __frozen_fallback_active to prevent infinite loop in partial freeze fallback
    // Save original function to globalThis for partial freeze fallback (lazy, on first call)
    const hook = `if(globalThis.__frozen_${func.name}){if(!globalThis.__frozen_fallback_active){if(!globalThis.__original_${func.name})globalThis.__original_${func.name}=${func.name};return globalThis.__frozen_${func.name}(${argNames.join(',')});}}`;

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
