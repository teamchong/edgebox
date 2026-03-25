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
  {
    name: 'checkBinaryLikeExpression',
    params: 'left, operatorToken, right, checkMode, errorNode',
    captures: [],
  },
  {
    name: 'resolveStructuredTypeMembers',
    params: 'type',
    captures: [],
  },
  {
    name: 'checkVariableDeclaration',
    params: 'node',
    captures: [],
  },
  {
    name: 'instantiateType',
    params: 'type, mapper',
    captures: [],
  },
  {
    name: 'checkExpression',
    params: 'node, checkMode, forceTuple',
    captures: ['currentNode', 'instantiationCount'],
  },
  {
    name: 'checkSourceElementWorker',
    params: 'node',
    captures: ['cancellationToken', 'compilerOptions'],
  },
  {
    name: 'getTypeOfExpression',
    params: 'node',
    captures: ['flowInvocationCount', 'flowTypeCache'],
  },
  {
    name: 'getTypeArguments',
    params: 'type',
    captures: ['currentNode', 'errorType'],
  },
  {
    name: 'getSignaturesOfType',
    params: 'type, kind',
    captures: ['globalArrayType', 'globalReadonlyArrayType'],
  },
  {
    name: 'createType',
    params: 'flags',
    captures: ['Type7', 'checker', 'typeCount'],
  },
  {
    name: 'checkReturnStatement',
    params: 'node',
    captures: ['compilerOptions', 'strictNullChecks', 'undefinedType'],
  },
  {
    name: 'checkCallExpression',
    params: 'node, checkMode',
    captures: ['anyType', 'noImplicitAny', 'resolvingSignature', 'silentNeverType', 'voidType'],
  },
  {
    name: 'getResolvedSignature',
    params: 'node, candidatesOutArray, checkMode',
    captures: ['flowLoopCount', 'flowLoopStart', 'resolutionStart', 'resolutionTargets', 'resolvingSignature'],
  },
  {
    name: 'getPropertyOfType',
    params: 'type, name, skipObjectFunctionPropertyAugment, includeTypeOnlyMembers',
    captures: ['anyFunctionType', 'globalCallableFunctionType', 'globalFunctionType', 'globalNewableFunctionType', 'globalObjectType'],
  },
  {
    name: 'chooseOverload',
    params: 'candidates2, relation, isSingleNonGenericCandidate2, signatureHelpTrailingComma2 = false',
    captures: [],
  },
  {
    name: 'getApparentType',
    params: 'type',
    captures: ['emptyObjectType', 'globalBooleanType', 'globalNumberType', 'globalStringType', 'strictNullChecks', 'stringNumberSymbolType', 'unknownType'],
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
  if (false && t.name === "isSimpleTypeRelatedTo") {
    // WasmGC isRelatedToFast: built at build time, runs in snapshot.
    // Handles flag checks as pure integer ops — no closure refs needed.
    // Falls through to inline fallback for string/value/object cases.
    // __gcCheckRel = freeze-compiled isRelatedToFast from frozen_checker.js.
    // Same logic as this function, compiled to WASM at build time.
    // Returns 1=true, 0=false, -1=unknown (fall through to inline JS).
    // WASM fast-path: always call freeze-compiled isRelatedToFast.
    // No conditionals — WASM is always loaded at this point.
    // TurboFan inlines the WASM call at the JS callsite.
    // WASM fast-path — freeze-compiled at build time, native speed.
    // WASM fast-path with enum hash + value comparison (10 params).
    // MonoSymbol stores name hash in __gcSymbolHashArr.
    // source.value is integer for number literals.
    // Freeze-compiled WASM (correct, from actual TSC logic).
    // Minimal extraction: .id + .flags (V8 inline cache, fast).
    // WASM IS the function — no pre-check, no fallback.
    // WASM IS the function — lightweight wrapper with name hash for enums.
    // No JS function calls (isEnumTypeRelatedTo etc.) — only property access.
    wasmFastPath = `
    var _srcSH = source.symbol ? source.symbol._nameHash | 0 : 0;
    var _tgtSH = target.symbol ? target.symbol._nameHash | 0 : 0;
    var _rel = relation === assignableRelation ? 0 : relation === comparableRelation ? 1 :
      relation === strictSubtypeRelation ? 3 : relation === identityRelation ? 4 : 2;
    var _tgtUL = (_rel <= 1 && target.objectFlags & 67108864) ? 1 : 0;
    var _r = globalThis.__frozenIsRelated(source.id | 0, target.id | 0,
      source.flags | 0, target.flags | 0, _rel, strictNullChecks ? 1 : 0,
      _srcSH, _tgtSH, source.value === target.value ? 1 : 0,
      source === wildcardType ? 1 : 0, _tgtUL);
    return _r === 1;`;
  }

  // For functions with WASM replacement: WASM IS the function, no fallback.
  if (wasmFastPath) {
    const wrapper = `function ${t.name}(${t.params}) {${wasmFastPath}}`;
    src = src.substring(0, bounds.start) + wrapper + src.substring(bounds.end);
    console.log(`[transform-tsc] ${t.name}: ${t.captures.length} captures, ${originalBody.length} chars (WASM replaces)`);
    continue;
  }
  let postCall = '';
  if (false && t.name === 'createType') {
    // After createType runs, type.id is set. Write flags to WASM array.
    // No prediction — use the actual type.id that TSC assigned.
    postCall = `
    var _ct_result = (function(${t.params}) ${originalBody.substring(sig.length - 1)})(${paramNames.join(', ')});
    if (_ct_result && _ct_result.id > 0 && _ct_result.id < 131072 && globalThis.__gcFlags) {
      globalThis.__gcFlags.setFlag(globalThis.__gcFlagsArr, _ct_result.id | 0, flags | 0);
    }
    return _ct_result;`;
  }
  const wrapper = postCall
    ? `function ${t.name}(${t.params}) {${wasmFastPath}${postCall}}`
    : `function ${t.name}(${t.params}) {${wasmFastPath}
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
