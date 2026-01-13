#!/usr/bin/env node
/**
 * test262 Bundle Generator
 *
 * Bundles all test262 tests into a single JS file for fast compilation.
 * Instead of compiling 50,000+ tests individually (~3s each = 40+ hours),
 * we compile once and run all tests (~10-15 minutes total).
 *
 * Usage:
 *   node scripts/bundle-test262.js [options]
 *
 * Options:
 *   --output, -o    Output file (default: zig-out/test262-bundle.js)
 *   --limit, -l     Limit number of tests (for testing)
 *   --filter, -f    Filter tests by path pattern (e.g., "language/expressions")
 *   --list          Just list tests, don't bundle
 */

const fs = require('fs');
const path = require('path');

const TEST262_DIR = 'vendor/quickjs-ng/test262';
const HARNESS_DIR = path.join(TEST262_DIR, 'harness');
const TEST_DIR = path.join(TEST262_DIR, 'test');

// Parse arguments
const args = process.argv.slice(2);
let outputFile = 'zig-out/test262-bundle.js';
let limit = 0;
let filter = '';
let listOnly = false;
let shard = 0;      // Current shard (1-based)
let totalShards = 0; // Total number of shards

for (let i = 0; i < args.length; i++) {
  if (args[i] === '--output' || args[i] === '-o') {
    outputFile = args[++i];
  } else if (args[i] === '--limit' || args[i] === '-l') {
    limit = parseInt(args[++i], 10);
  } else if (args[i] === '--filter' || args[i] === '-f') {
    filter = args[++i];
  } else if (args[i] === '--list') {
    listOnly = true;
  } else if (args[i] === '--shard') {
    // Format: --shard 1/10 (shard 1 of 10)
    const parts = args[++i].split('/');
    shard = parseInt(parts[0], 10);
    totalShards = parseInt(parts[1], 10);
  } else if (args[i] === '--count') {
    // Just count tests and exit
    const allTests = findTests(TEST_DIR);
    console.log(allTests.length);
    process.exit(0);
  }
}

// Find all test files
function findTests(dir, tests = []) {
  const entries = fs.readdirSync(dir, { withFileTypes: true });
  for (const entry of entries) {
    const fullPath = path.join(dir, entry.name);
    if (entry.isDirectory()) {
      // Skip certain directories
      if (entry.name === 'annexB' || entry.name === 'intl402') continue;
      findTests(fullPath, tests);
    } else if (entry.name.endsWith('.js') && !entry.name.endsWith('_FIXTURE.js')) {
      const relPath = path.relative(TEST_DIR, fullPath);
      if (!filter || relPath.includes(filter)) {
        tests.push({ path: fullPath, name: relPath });
      }
    }
  }
  return tests;
}

// Parse test metadata (YAML front matter)
function parseTestMeta(content) {
  const match = content.match(/\/\*---([\s\S]*?)---\*\//);
  if (!match) return {};

  const meta = {};
  const yaml = match[1];

  // Simple YAML parsing for common fields
  if (yaml.includes('async')) meta.async = true;
  if (yaml.includes('module')) meta.module = true;
  if (yaml.includes('raw')) meta.raw = true;
  if (yaml.includes('negative:')) meta.negative = true;

  // Extract features
  const featuresMatch = yaml.match(/features:\s*\[(.*?)\]/s);
  if (featuresMatch) {
    meta.features = featuresMatch[1].split(',').map(f => f.trim());
  }

  // Extract includes
  const includesMatch = yaml.match(/includes:\s*\[(.*?)\]/s);
  if (includesMatch) {
    meta.includes = includesMatch[1].split(',').map(f => f.trim().replace(/['"]/g, ''));
  }

  return meta;
}

// Load harness files
function loadHarness() {
  const harness = {};
  // Load ALL harness files for full compatibility
  const harnessFiles = fs.readdirSync(HARNESS_DIR).filter(f => f.endsWith('.js'));

  for (const file of harnessFiles) {
    const filePath = path.join(HARNESS_DIR, file);
    try {
      harness[file] = fs.readFileSync(filePath, 'utf-8');
    } catch (e) {
      console.log(`[bundle] Warning: Could not load ${file}`);
    }
  }
  console.log(`[bundle] Loaded ${Object.keys(harness).length} harness files`);
  return harness;
}

// Escape JS string for embedding (use JSON.stringify for safety)
function escapeJS(str) {
  // JSON.stringify handles all escaping correctly, then we strip the outer quotes
  return JSON.stringify(str).slice(1, -1);
}

// Main
console.log('[bundle] Finding test262 tests...');
let tests = findTests(TEST_DIR);
console.log(`[bundle] Found ${tests.length} tests`);

if (filter) {
  console.log(`[bundle] Filter: "${filter}"`);
}

if (limit > 0) {
  tests = tests.slice(0, limit);
  console.log(`[bundle] Limited to ${tests.length} tests`);
}

// Apply sharding if specified
if (shard > 0 && totalShards > 0) {
  const testsPerShard = Math.ceil(tests.length / totalShards);
  const startIdx = (shard - 1) * testsPerShard;
  const endIdx = Math.min(startIdx + testsPerShard, tests.length);
  tests = tests.slice(startIdx, endIdx);
  console.log(`[bundle] Shard ${shard}/${totalShards}: tests ${startIdx + 1}-${endIdx} (${tests.length} tests)`);
}

if (listOnly) {
  for (const test of tests) {
    console.log(test.name);
  }
  process.exit(0);
}

// Load harness
console.log('[bundle] Loading harness files...');
const harness = loadHarness();

// Filter out async/module tests (not supported in simple eval)
console.log('[bundle] Filtering compatible tests...');
const compatibleTests = [];
const skippedTests = { async: 0, module: 0, raw: 0, other: 0 };

for (const test of tests) {
  const content = fs.readFileSync(test.path, 'utf-8');
  const meta = parseTestMeta(content);

  if (meta.async) { skippedTests.async++; continue; }
  if (meta.module) { skippedTests.module++; continue; }
  if (meta.raw) { skippedTests.raw++; continue; }

  compatibleTests.push({ ...test, content, meta });
}

console.log(`[bundle] Compatible: ${compatibleTests.length}, Skipped: async=${skippedTests.async}, module=${skippedTests.module}, raw=${skippedTests.raw}`);

// Generate bundle
console.log('[bundle] Generating bundle...');

let bundle = `// Auto-generated test262 bundle
// Generated: ${new Date().toISOString()}
// Tests: ${compatibleTests.length}

// Results output
var __results = { passed: 0, failed: 0, errors: [] };

// test262 error class
function Test262Error(message) {
  this.message = message;
  this.name = 'Test262Error';
}
Test262Error.prototype = Object.create(Error.prototype);
Test262Error.prototype.constructor = Test262Error;

// Stub $262 host object (test262-specific)
var $262 = {
  createRealm: function() { return { global: globalThis }; },
  detachArrayBuffer: function(ab) { /* stub */ },
  evalScript: function(code) { return eval(code); },
  gc: function() { /* stub */ },
  global: globalThis,
  agent: {
    start: function() {},
    broadcast: function() {},
    getReport: function() { return null; },
    sleep: function() {},
    monotonicNow: function() { return Date.now(); }
  }
};

// Stub print if not defined
if (typeof print === 'undefined') {
  var print = console.log.bind(console);
}

// Load all harness files
${Object.entries(harness).map(([name, code]) => `// Harness: ${name}\n${code}`).join('\n\n')}

// Test runner with robust error handling
function runTest(name, code) {
  try {
    // Use indirect eval with IIFE for global scope execution
    (1, eval)("(function() { 'use strict'; " + code + "\\n})()");
    __results.passed++;
    return true;
  } catch (e) {
    __results.failed++;
    var errMsg = "unknown error";
    try {
      if (e && e.message) {
        errMsg = (e.name || "Error") + ": " + String(e.message).slice(0, 150);
      } else {
        errMsg = String(e).slice(0, 200);
      }
    } catch (e2) {
      errMsg = "error in error handler";
    }
    __results.errors.push({ test: name, error: errMsg });
    return false;
  }
}

// Run all tests
print("[test262] Running " + ${compatibleTests.length} + " tests...");
var __startTime = Date.now();

`;

// Add each test using JSON.stringify for proper escaping
for (let i = 0; i < compatibleTests.length; i++) {
  const test = compatibleTests[i];
  // Use JSON.stringify for both name and code - guaranteed proper escaping
  bundle += `runTest(${JSON.stringify(test.name)}, ${JSON.stringify(test.content)});\n`;

  // Progress marker every 1000 tests
  if ((i + 1) % 1000 === 0) {
    bundle += `print("[test262] Progress: " + ${i + 1} + "/${compatibleTests.length}");\n`;
  }
}

// Add results output
bundle += `
var __endTime = Date.now();
var __duration = (__endTime - __startTime) / 1000;

print("");
print("========================================");
print("test262 Results");
print("========================================");
print("Passed: " + __results.passed);
print("Failed: " + __results.failed);
print("Total:  " + (__results.passed + __results.failed));
print("Time:   " + __duration.toFixed(2) + "s");
print("========================================");

// Output errors (first 50)
if (__results.errors.length > 0) {
  print("");
  print("Failed tests (first 50):");
  for (var i = 0; i < Math.min(50, __results.errors.length); i++) {
    print("  FAIL: " + __results.errors[i].test);
    print("        " + __results.errors[i].error);
  }
}

// Write JSON results to stdout marker for CI parsing
print("");
print("__TEST262_JSON_START__");
var __errorsToReport = [];
for (var __ei = 0; __ei < Math.min(100, __results.errors.length); __ei++) {
  __errorsToReport.push(__results.errors[__ei]);
}
print(JSON.stringify({
  passed: __results.passed,
  failed: __results.failed,
  duration: __duration,
  errors: __errorsToReport
}));
print("__TEST262_JSON_END__");
`;

// Write bundle
fs.mkdirSync(path.dirname(outputFile), { recursive: true });
fs.writeFileSync(outputFile, bundle);

const stats = fs.statSync(outputFile);
console.log(`[bundle] Written: ${outputFile} (${(stats.size / 1024 / 1024).toFixed(1)}MB)`);
console.log('[bundle] Done!');
console.log('');
console.log('Next steps:');
console.log(`  1. Compile: ./zig-out/bin/edgeboxc --minimal ${outputFile}`);
console.log(`  2. Run:     ./zig-out/bin/${path.basename(outputFile, '.js')}/${path.basename(outputFile, '.js')}`);
