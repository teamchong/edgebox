#!/usr/bin/env node
// Patch Closure Hooks - Updates frozen function hooks with closure variables
// This runs AFTER the freeze tool to patch hooks with closure var arrays
//
// Usage: patch_closure_hooks.js <hooked_bundle.js> <frozen_functions.c>
//
// The frozen_functions.c contains a CLOSURE_MANIFEST comment block at the end:
// /* CLOSURE_MANIFEST_BEGIN
// {"functions":[{"name":"foo","closureVars":[{"n":"counter","c":false},{"n":"data","c":true}]}]}
// CLOSURE_MANIFEST_END */
//
// Each closure var has: n=name, c=isConst (const vars skip write-back)

const fs = require('fs');

const hookedBundle = process.argv[2];
const frozenFunctionsC = process.argv[3];

if (!hookedBundle || !frozenFunctionsC) {
    console.error('Usage: patch_closure_hooks.js <hooked_bundle.js> <frozen_functions.c>');
    process.exit(1);
}

// Read the frozen functions C file
let frozenContent;
try {
    frozenContent = fs.readFileSync(frozenFunctionsC, 'utf8');
} catch (e) {
    console.error(`Could not read frozen functions: ${e.message}`);
    process.exit(1);
}

// Extract closure manifest from comment block
const manifestMatch = frozenContent.match(/CLOSURE_MANIFEST_BEGIN\n([\s\S]*?)\nCLOSURE_MANIFEST_END/);
if (!manifestMatch) {
    // No closure manifest - nothing to patch
    console.log('[patch_closure_hooks] No closure manifest found, skipping');
    process.exit(0);
}

let manifest;
try {
    manifest = JSON.parse(manifestMatch[1]);
} catch (e) {
    console.error(`Could not parse closure manifest: ${e.message}`);
    process.exit(1);
}

if (!manifest.functions || manifest.functions.length === 0) {
    console.log('[patch_closure_hooks] No functions with closures, skipping');
    process.exit(0);
}

// Read the hooked bundle
let bundleContent;
try {
    bundleContent = fs.readFileSync(hookedBundle, 'utf8');
} catch (e) {
    console.error(`Could not read hooked bundle: ${e.message}`);
    process.exit(1);
}

// For each function with closure vars, patch its hook to:
// 1. Create a closure array before the call
// 2. Call frozen function with the array
// 3. Write back closure values from array after the call (EXCEPT const vars)
let patchCount = 0;
for (const func of manifest.functions) {
    const { name, closureVars } = func;
    if (!closureVars || closureVars.length === 0) continue;

    // Support both old format (array of strings) and new format (array of {n, c} objects)
    const vars = closureVars.map(cv => typeof cv === 'string' ? { n: cv, c: false } : cv);

    // Current hook pattern (from inject_hooks.js):
    // if(globalThis.__frozen_NAME){if(!globalThis.__frozen_fallback_active){if(!globalThis.__original_NAME)globalThis.__original_NAME=NAME;return globalThis.__frozen_NAME(args);}}
    //
    // We need to change this to:
    // if(globalThis.__frozen_NAME){if(!globalThis.__frozen_fallback_active){if(!globalThis.__original_NAME)globalThis.__original_NAME=NAME;var __cv=[var1,var2];var __r=globalThis.__frozen_NAME(args,__cv);var1=__cv[0];var2=__cv[1];return __r;}}

    // Pattern to match: return globalThis.__frozen_NAME(args);}}
    const hookPattern = new RegExp(
        `return globalThis\\.__frozen_${name}\\(([^;]*?)\\);\\}\\}`,
        'g'
    );

    // Generate the replacement code
    // Include all vars in the array (for reading), but only write back mutable ones
    const closureArrayInit = `var __cv=[${vars.map(v => v.n).join(',')}];`;
    // Only generate write-back for non-const vars
    const writeBackCode = vars
        .map((v, i) => v.c ? null : `${v.n}=__cv[${i}]`)
        .filter(x => x !== null)
        .join(';');

    bundleContent = bundleContent.replace(hookPattern, (match, args) => {
        const argsStr = args.trim();
        // Build the call with closure array added
        let callArgs;
        if (argsStr === '') {
            callArgs = '__cv';
        } else {
            callArgs = `${argsStr},__cv`;
        }
        // Only add write-back if there are mutable vars
        if (writeBackCode) {
            return `${closureArrayInit}var __r=globalThis.__frozen_${name}(${callArgs});${writeBackCode};return __r;}}`;
        } else {
            // All vars are const - no write-back needed
            return `${closureArrayInit}return globalThis.__frozen_${name}(${callArgs});}}`;
        }
    });

    patchCount++;
}

// Write the patched bundle
try {
    fs.writeFileSync(hookedBundle, bundleContent);
} catch (e) {
    console.error(`Could not write patched bundle: ${e.message}`);
    process.exit(1);
}

if (patchCount > 0) {
    console.log(`[patch_closure_hooks] Patched ${patchCount} functions with closure vars`);
}
