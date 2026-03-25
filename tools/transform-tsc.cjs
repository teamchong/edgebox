#!/usr/bin/env node
// Build-time TSC transformation: convert checker closures to globals.
// Deterministic — same output every run. V8 feedback vectors work.

const fs = require('fs');
const path = require('path');

const tsPath = path.join(__dirname, '..', 'node_modules', 'typescript', 'lib', 'typescript.js');
let src = fs.readFileSync(tsPath, 'utf8');

if (src.includes('__eb_isSimpleTypeRelatedTo')) {
  console.log('[transform-tsc] already transformed');
  process.exit(0);
}

// Functions to extract from createTypeChecker and their captured variables
const transforms = [
  {
    name: 'isSimpleTypeRelatedTo',
    params: 'source, target, relation, errorReporter',
    captures: ['strictNullChecks', 'wildcardType', 'assignableRelation', 'comparableRelation', 'strictSubtypeRelation', 'identityRelation'],
  },
  {
    name: 'isTypeRelatedTo',
    params: 'source, target, relation',
    captures: ['comparableRelation', 'identityRelation'],
  },
];

function findFuncBounds(source, signature) {
  const start = source.indexOf(signature);
  if (start < 0) return null;
  let depth = 0, end = -1;
  for (let i = start; i < source.length; i++) {
    if (source[i] === '{') depth++;
    if (source[i] === '}') { depth--; if (depth === 0) { end = i + 1; break; } }
  }
  return end > 0 ? { start, end } : null;
}

const ctcStart = src.indexOf('function createTypeChecker(host) {');
if (ctcStart < 0) { console.error('FATAL: createTypeChecker not found'); process.exit(1); }

let globalFuncs = '';

for (const t of transforms) {
  const sig = `function ${t.name}(${t.params}) {`;
  const bounds = findFuncBounds(src, sig);
  if (!bounds) { console.error(`FATAL: ${t.name} not found`); process.exit(1); }

  const originalBody = src.substring(bounds.start, bounds.end);
  const ebName = `__eb_${t.name}`;
  const extraParams = t.captures.join(', ');

  // Create global function
  globalFuncs += `\n// [EdgeBox] Global ${t.name} for freeze compilation\n`;
  globalFuncs += `function ${ebName}_impl(${t.params}, ${extraParams}) `;
  globalFuncs += originalBody.substring(sig.length - 1) + '\n'; // starts from {

  // Create wrapper with WasmGC fast-path + inline fallback
  const paramNames = t.params.split(',').map(s => s.trim());
  let wasmFastPath = '';
  if (t.name === 'isSimpleTypeRelatedTo') {
    // WasmGC isRelatedToFast: built at build time, runs in snapshot.
    // Handles flag checks as pure integer ops — no closure refs needed.
    // Falls through to inline fallback for string/value/object cases.
    wasmFastPath = `
    if (globalThis.__gcCheckRel) {
      var _rel = relation === assignableRelation ? 0 : relation === comparableRelation ? 1 :
                 relation === strictSubtypeRelation ? 3 : relation === identityRelation ? 4 : 2;
      // TODO: wire to freeze-compiled WasmGC version of isSimpleTypeRelatedTo
      // (not hand-written checkRel which has correctness bugs)
    }`;
  }
  const wrapper = `function ${t.name}(${t.params}) {${wasmFastPath}
    ${originalBody.replace(sig, '{')}
  }`;

  // Replace in source
  src = src.substring(0, bounds.start) + wrapper + src.substring(bounds.end);
  console.log(`[transform-tsc] ${t.name}: ${t.captures.length} captures, ${originalBody.length} chars`);
}

// Insert all global functions BEFORE createTypeChecker
const newCtcStart = src.indexOf('function createTypeChecker(host) {');
src = src.substring(0, newCtcStart) + globalFuncs + '\n' + src.substring(newCtcStart);

fs.writeFileSync(tsPath, src);
console.log(`[transform-tsc] done: ${src.length} bytes, ${transforms.length} functions transformed`);
