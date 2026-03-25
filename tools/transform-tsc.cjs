#!/usr/bin/env node
// Build-time TSC transformation: convert checker closures to globals.
// Run once during `zig build` to prepare typescript.js for freeze compilation.
//
// What it does:
// 1. Finds `isSimpleTypeRelatedTo` inside createTypeChecker
// 2. Replaces it with a call to a global function `__eb_isSimpleTypeRelatedTo`
// 3. Adds the global function OUTSIDE createTypeChecker (accessible to freeze)
// 4. The global reads captured variables from a context object set by the recipe
//
// The transformed typescript.js is deterministic — same output every run.
// V8 feedback vectors work because the source is always the same.

const fs = require('fs');
const path = require('path');

const tsPath = path.join(__dirname, '..', 'node_modules', 'typescript', 'lib', 'typescript.js');
let src = fs.readFileSync(tsPath, 'utf8');

// Check if already transformed
if (src.includes('__eb_isSimpleTypeRelatedTo')) {
  console.log('[transform-tsc] already transformed');
  process.exit(0);
}

// Find isSimpleTypeRelatedTo inside createTypeChecker
const funcStart = src.indexOf('function isSimpleTypeRelatedTo(source, target, relation, errorReporter) {');
if (funcStart < 0) {
  console.error('[transform-tsc] FATAL: isSimpleTypeRelatedTo not found');
  process.exit(1);
}

// Find the end of the function — match braces
let depth = 0;
let funcEnd = -1;
for (let i = funcStart; i < src.length; i++) {
  if (src[i] === '{') depth++;
  if (src[i] === '}') {
    depth--;
    if (depth === 0) { funcEnd = i + 1; break; }
  }
}
if (funcEnd < 0) {
  console.error('[transform-tsc] FATAL: could not find end of isSimpleTypeRelatedTo');
  process.exit(1);
}

const originalFunc = src.substring(funcStart, funcEnd);
console.log('[transform-tsc] isSimpleTypeRelatedTo: ' + originalFunc.length + ' chars');

// Replace the function with a thin wrapper that calls the global
const wrapper = `function isSimpleTypeRelatedTo(source, target, relation, errorReporter) {
    // Transformed: delegates to global __eb_isSimpleTypeRelatedTo (freeze-compilable)
    if (globalThis.__eb_isSimpleTypeRelatedTo) {
      return globalThis.__eb_isSimpleTypeRelatedTo(source, target, relation, errorReporter,
        strictNullChecks, wildcardType, assignableRelation, comparableRelation, strictSubtypeRelation);
    }
    // Fallback: original implementation inlined
    ${originalFunc.replace('function isSimpleTypeRelatedTo(source, target, relation, errorReporter) {', '{')}
  }`;

// Also create the global function (placed before createTypeChecker)
const ctcStart = src.indexOf('function createTypeChecker(host) {');
if (ctcStart < 0) {
  console.error('[transform-tsc] FATAL: createTypeChecker not found');
  process.exit(1);
}

// The global function takes captured variables as extra params
const globalFunc = `
// [EdgeBox] Global version of isSimpleTypeRelatedTo for freeze compilation.
// Captured variables passed as parameters instead of closure references.
globalThis.__eb_isSimpleTypeRelatedTo = null; // Set by recipe after freeze-compile
function __eb_isSimpleTypeRelatedTo_impl(source, target, relation, errorReporter,
    strictNullChecks, wildcardType, assignableRelation, comparableRelation, strictSubtypeRelation) {
${originalFunc.replace('function isSimpleTypeRelatedTo(source, target, relation, errorReporter) {', '{')}
}
`;

// Insert global function BEFORE createTypeChecker
src = src.substring(0, ctcStart) + globalFunc + '\n' + src.substring(ctcStart);

// Now replace the closure function (its position shifted by globalFunc.length)
const newFuncStart = src.indexOf('function isSimpleTypeRelatedTo(source, target, relation, errorReporter) {',
  ctcStart + globalFunc.length);
if (newFuncStart < 0) {
  console.error('[transform-tsc] FATAL: could not find isSimpleTypeRelatedTo after insertion');
  process.exit(1);
}

// Find end again
depth = 0;
funcEnd = -1;
for (let i = newFuncStart; i < src.length; i++) {
  if (src[i] === '{') depth++;
  if (src[i] === '}') {
    depth--;
    if (depth === 0) { funcEnd = i + 1; break; }
  }
}

src = src.substring(0, newFuncStart) + wrapper + src.substring(funcEnd);

fs.writeFileSync(tsPath, src);
console.log('[transform-tsc] transformed typescript.js (' + src.length + ' bytes)');
console.log('[transform-tsc] global __eb_isSimpleTypeRelatedTo_impl added');
console.log('[transform-tsc] closure isSimpleTypeRelatedTo delegates to global');
