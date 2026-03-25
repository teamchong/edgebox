#!/usr/bin/env node
// Closure-to-Global Transformer
// Extracts inner functions from createTypeChecker and converts them to
// standalone global functions with captured variables as a context parameter.
//
// Usage: node tools/closure-to-global.js <functionName> [functionName2 ...]
// Output: prints transformed function to stdout

const ts = require('../node_modules/typescript');
const fs = require('fs');

const src = fs.readFileSync('node_modules/typescript/lib/_tsc.js', 'utf8');
const sf = ts.createSourceFile('_tsc.js', src, ts.ScriptTarget.Latest, true);

// Find createTypeChecker
function findFunction(node, name) {
  if (ts.isFunctionDeclaration(node) && node.name && node.name.text === name) return node;
  let found = null;
  node.forEachChild(c => { if (!found) found = findFunction(c, name); });
  return found;
}

const ctc = findFunction(sf, 'createTypeChecker');
if (!ctc) { console.error('createTypeChecker not found'); process.exit(1); }

// Collect ALL scope variables in createTypeChecker
const scopeVars = new Set();
ctc.parameters.forEach(p => scopeVars.add(p.name.text));
ctc.body.statements.forEach(stmt => {
  if (ts.isVariableStatement(stmt)) {
    stmt.declarationList.declarations.forEach(d => {
      if (ts.isIdentifier(d.name)) scopeVars.add(d.name.text);
    });
  }
});

// Find target function(s)
const targetNames = process.argv.slice(2);
if (targetNames.length === 0) {
  console.error('Usage: node closure-to-global.js <functionName> [functionName2 ...]');
  process.exit(1);
}

function findInner(node, name) {
  if (ts.isFunctionDeclaration(node) && node.name && node.name.text === name) return node;
  let found = null;
  node.forEachChild(c => { if (!found) found = findInner(c, name); });
  return found;
}

for (const name of targetNames) {
  const fn = findInner(ctc, name);
  if (!fn) { console.error(`Function ${name} not found in createTypeChecker`); continue; }

  const body = src.substring(fn.pos, fn.end);

  // Find ALL identifiers used in the function body
  const usedIds = new Set();
  function walk(node) {
    if (ts.isIdentifier(node)) usedIds.add(node.text);
    node.forEachChild(walk);
  }
  walk(fn);

  // Find captured variables: used in body AND in scopeVars
  const params = new Set();
  fn.parameters.forEach(p => {
    if (ts.isIdentifier(p.name)) params.add(p.name.text);
  });

  // Local declarations
  const locals = new Set();
  function findLocals(node) {
    if (ts.isVariableDeclaration(node) && ts.isIdentifier(node.name)) {
      locals.add(node.name.text);
    }
    if (ts.isFunctionDeclaration(node) && node.name) locals.add(node.name.text);
    node.forEachChild(findLocals);
  }
  fn.body.forEachChild(findLocals);

  const captured = [...usedIds].filter(id =>
    scopeVars.has(id) && !params.has(id) && !locals.has(id) && id !== name
  ).sort();

  console.log(`// ${name}: ${captured.length} captured variables`);
  console.log(`// Captured: ${captured.join(', ')}`);
  console.log(`// Original params: ${[...params].join(', ')}`);
  console.log(`// Transform: add __ctx as first param, replace captures with __ctx.varName`);
  console.log(`//`);

  // Output transformed function
  const paramList = [...params].join(', ');
  const capturedComment = captured.map(v => `//   __ctx.${v}`).join('\n');
  console.log(`// Context fields needed:`);
  console.log(capturedComment);
  console.log(`function ${name}(__ctx, ${paramList}) {`);

  // For each captured variable, add a local alias at the top
  for (const v of captured) {
    console.log(`  var ${v} = __ctx.${v};`);
  }

  // Output the original function body (skip the function declaration line)
  const bodyStart = fn.body.pos;
  const bodyEnd = fn.body.end;
  const bodyText = src.substring(bodyStart, bodyEnd);
  // Remove the outer { } and print
  const inner = bodyText.trim().replace(/^\{/, '').replace(/\}$/, '');
  console.log(inner);
  console.log('}');
  console.log('');
}
