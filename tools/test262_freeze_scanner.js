#!/usr/bin/env node
/**
 * Test262 Freeze Scanner
 *
 * Scans test262 test suite to find opcodes that prevent functions from being frozen.
 * Compiles each test with edgeboxc and analyzes the freeze log output.
 */

const fs = require('fs');
const path = require('path');
const { execSync, spawnSync } = require('child_process');

const TEST262_DIR = path.join(__dirname, '../test262/test');
const EDGEBOXC = path.join(__dirname, '../zig-out/bin/edgeboxc');
const TMP_DIR = '/tmp/test262_freeze_scan';

// Track opcode statistics
const opcodeStats = new Map();
const failedTests = [];
const successTests = [];

function ensureTmpDir() {
    if (!fs.existsSync(TMP_DIR)) {
        fs.mkdirSync(TMP_DIR, { recursive: true });
    }
}

function scanTest(testFile) {
    const testName = path.relative(TEST262_DIR, testFile);
    const tmpTestDir = path.join(TMP_DIR, 'test_' + Date.now());

    try {
        // Create temp directory with test as index.js
        fs.mkdirSync(tmpTestDir, { recursive: true });
        fs.copyFileSync(testFile, path.join(tmpTestDir, 'index.js'));

        // Build with edgeboxc and capture output
        const result = spawnSync(EDGEBOXC, ['build', tmpTestDir], {
            encoding: 'utf8',
            timeout: 10000,
        });

        const output = result.stdout + result.stderr;

        // Parse freeze output for opcode blocks
        const blockedMatches = output.matchAll(/BLOCKED: '([^']+)' reason=([^\s]+)/g);
        for (const match of blockedMatches) {
            const funcName = match[1];
            const reason = match[2];

            if (!opcodeStats.has(reason)) {
                opcodeStats.set(reason, { count: 0, tests: new Set() });
            }
            opcodeStats.get(reason).count++;
            opcodeStats.get(reason).tests.add(testName);
        }

        // Check if any functions were frozen
        const frozenMatch = output.match(/FROZEN[^:]*: '([^']+)'/);
        if (frozenMatch) {
            successTests.push(testName);
        } else if (Array.from(blockedMatches).length > 0) {
            failedTests.push(testName);
        }

        return { success: !!frozenMatch, output };

    } catch (error) {
        return { success: false, error: error.message };
    } finally {
        // Cleanup
        try {
            if (fs.existsSync(tmpTestDir)) {
                fs.rmSync(tmpTestDir, { recursive: true, force: true });
            }
        } catch (e) {}
    }
}

function findJSTests(dir, maxTests = 100) {
    const tests = [];

    function walk(currentDir) {
        if (tests.length >= maxTests) return;

        const entries = fs.readdirSync(currentDir, { withFileTypes: true });
        for (const entry of entries) {
            if (tests.length >= maxTests) break;

            const fullPath = path.join(currentDir, entry.name);
            if (entry.isDirectory()) {
                walk(fullPath);
            } else if (entry.isFile() && entry.name.endsWith('.js')) {
                // Skip helper files
                if (entry.name.startsWith('_') || entry.name.includes('FIXTURE')) {
                    continue;
                }
                tests.push(fullPath);
            }
        }
    }

    walk(dir);
    return tests;
}

function main() {
    console.log('Test262 Freeze Scanner');
    console.log('======================\n');

    // Check edgeboxc exists
    if (!fs.existsSync(EDGEBOXC)) {
        console.error('Error: edgeboxc not found. Run: zig build cli -Doptimize=ReleaseFast');
        process.exit(1);
    }

    ensureTmpDir();

    // Target ES2024 features specifically
    const targetDirs = [
        'language/expressions/function',
        'language/expressions/arrow-function',
        'language/expressions/async-function',
        'language/expressions/class',
        'language/statements/function',
        'language/statements/class',
    ];

    let allTests = [];
    for (const targetDir of targetDirs) {
        const fullPath = path.join(TEST262_DIR, targetDir);
        if (fs.existsSync(fullPath)) {
            const tests = findJSTests(fullPath, 20); // 20 tests per category
            allTests = allTests.concat(tests);
        }
    }

    console.log(`Found ${allTests.length} tests to scan\n`);

    let processed = 0;
    for (const testFile of allTests) {
        processed++;
        const testName = path.relative(TEST262_DIR, testFile);
        process.stdout.write(`\r[${processed}/${allTests.length}] Scanning: ${testName.substring(0, 60).padEnd(60)}`);
        scanTest(testFile);
    }

    console.log('\n\n=== Results ===\n');
    console.log(`Tests processed: ${allTests.length}`);
    console.log(`Tests with frozen functions: ${successTests.length}`);
    console.log(`Tests with blocked functions: ${failedTests.length}\n`);

    console.log('=== Opcode Block Reasons (sorted by frequency) ===\n');
    const sortedOpcodes = Array.from(opcodeStats.entries())
        .sort((a, b) => b[1].count - a[1].count);

    for (const [opcode, stats] of sortedOpcodes) {
        console.log(`${opcode.padEnd(30)} ${stats.count} occurrences in ${stats.tests.size} tests`);
    }

    // Detailed breakdown
    console.log('\n=== Detailed Breakdown ===\n');
    for (const [opcode, stats] of sortedOpcodes.slice(0, 5)) {
        console.log(`\n${opcode}:`);
        const testList = Array.from(stats.tests).slice(0, 5);
        for (const test of testList) {
            console.log(`  - ${test}`);
        }
        if (stats.tests.size > 5) {
            console.log(`  ... and ${stats.tests.size - 5} more`);
        }
    }

    // Cleanup
    try {
        fs.rmSync(TMP_DIR, { recursive: true, force: true });
    } catch (e) {}
}

main();
