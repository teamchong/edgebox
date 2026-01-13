#!/usr/bin/env bun
/**
 * EdgeBox Runtime Benchmark
 *
 * Based on Microsoft's official typescript-go benchmark methodology.
 * Uses TypeScript's own source code as test input.
 *
 * Compares:
 * - EdgeBox-compiled tsc (AOT native binary)
 * - Node.js tsc (baseline)
 * - tsgo (Microsoft's Go version, if available)
 *
 * Reference: https://github.com/microsoft/typescript-go/_packages/api/test/api.bench.ts
 */

import { execSync, spawnSync } from "child_process";
import { existsSync, mkdirSync, readdirSync, readFileSync, writeFileSync, rmSync, statSync } from "fs";
import { join, dirname } from "path";
import { Bench } from "tinybench";

const BENCHMARK_DIR = import.meta.dirname;
const PROJECT_ROOT = join(BENCHMARK_DIR, "..");
const FIXTURES_DIR = join(BENCHMARK_DIR, "fixtures");
const RESULTS_FILE = join(BENCHMARK_DIR, "results.json");

// TypeScript source files used by Microsoft's benchmark
const TS_SUBMODULE = join(FIXTURES_DIR, "TypeScript");
const TS_COMPILER_DIR = join(TS_SUBMODULE, "src/compiler");

interface BenchmarkResult {
  tool: string;
  benchmark: string;
  mean: number;
  stdDev: number;
  samples: number;
}

// ============================================================================
// Setup - Download TypeScript source (same as Microsoft's benchmark)
// ============================================================================

async function ensureTypeScriptSource(): Promise<boolean> {
  if (existsSync(join(TS_COMPILER_DIR, "checker.ts"))) {
    console.log("TypeScript source already available");
    return true;
  }

  console.log("Downloading TypeScript source (used by Microsoft's benchmark)...");
  mkdirSync(FIXTURES_DIR, { recursive: true });

  try {
    // Clone TypeScript repo (shallow clone for speed)
    execSync(
      `git clone --depth 1 --filter=blob:none --sparse https://github.com/microsoft/TypeScript.git ${TS_SUBMODULE}`,
      { stdio: "inherit", timeout: 120000 }
    );
    // Sparse checkout only src/compiler
    execSync(
      `git -C ${TS_SUBMODULE} sparse-checkout set src/compiler src/lib`,
      { stdio: "inherit", timeout: 60000 }
    );
    return true;
  } catch (e) {
    console.error("Failed to download TypeScript source:", e);
    return false;
  }
}

function buildEdgeboxc(): string | null {
  const edgeboxcPath = join(PROJECT_ROOT, "zig-out/bin/edgeboxc");

  if (!existsSync(edgeboxcPath)) {
    console.log("Building edgeboxc...");
    try {
      execSync("zig build cli", { cwd: PROJECT_ROOT, stdio: "inherit" });
    } catch (e) {
      console.error("Failed to build edgeboxc:", e);
      return null;
    }
  }

  return existsSync(edgeboxcPath) ? edgeboxcPath : null;
}

function compileTypescriptWithEdgebox(edgeboxcPath: string): string | null {
  const tscSource = join(BENCHMARK_DIR, "node_modules/typescript/lib/tsc.js");
  const outputDir = join(PROJECT_ROOT, "zig-out/bin/tsc.js");
  const compiledTsc = join(outputDir, "tsc");

  if (!existsSync(tscSource)) {
    console.log("TypeScript npm package not installed");
    return null;
  }

  if (!existsSync(compiledTsc)) {
    console.log("Compiling TypeScript compiler with EdgeBox AOT...");
    try {
      execSync(`${edgeboxcPath} --binary-only "${tscSource}"`, {
        cwd: PROJECT_ROOT,
        stdio: "inherit",
        timeout: 600000, // 10 min timeout for AOT compilation
      });
    } catch (e) {
      console.error("Failed to compile tsc with EdgeBox:", e);
      return null;
    }
  }

  return existsSync(compiledTsc) ? compiledTsc : null;
}

function getTsgoPath(): string | null {
  // Check if tsgo is installed
  try {
    execSync("which tsgo", { stdio: "pipe" });
    return "tsgo";
  } catch {
    // Try to find in typescript-go repo
    const tsgoPath = "/tmp/typescript-go/built/local/tsgo";
    if (existsSync(tsgoPath)) {
      return tsgoPath;
    }
    return null;
  }
}

// ============================================================================
// Benchmark functions
// ============================================================================

function getTestFiles(): string[] {
  // Use the same files Microsoft uses in their benchmark
  const files = [
    join(TS_COMPILER_DIR, "checker.ts"),      // Large file ~50k lines
    join(TS_COMPILER_DIR, "program.ts"),      // Medium file
    join(TS_COMPILER_DIR, "parser.ts"),       // Parser
    join(TS_COMPILER_DIR, "scanner.ts"),      // Scanner
    join(TS_COMPILER_DIR, "binder.ts"),       // Binder
    join(TS_COMPILER_DIR, "utilities.ts"),    // Utilities
  ];
  return files.filter(f => existsSync(f));
}

function countLines(file: string): number {
  const content = readFileSync(file, "utf-8");
  return content.split("\n").length;
}

function runTscWithNode(files: string[], outDir: string): number {
  const start = performance.now();
  try {
    spawnSync("node", [
      "node_modules/typescript/lib/tsc.js",
      "--outDir", outDir,
      "--target", "ES2020",
      "--module", "ESNext",
      "--skipLibCheck",
      "--noEmit", "false",
      ...files
    ], {
      cwd: BENCHMARK_DIR,
      stdio: "pipe",
      timeout: 300000
    });
  } catch {}
  return performance.now() - start;
}

function runTscWithEdgebox(compiledTsc: string, files: string[], outDir: string): number {
  const start = performance.now();
  try {
    spawnSync(compiledTsc, [
      "--outDir", outDir,
      "--target", "ES2020",
      "--module", "ESNext",
      "--skipLibCheck",
      "--noEmit", "false",
      ...files
    ], {
      stdio: "pipe",
      timeout: 300000
    });
  } catch {}
  return performance.now() - start;
}

function runTsgo(tsgoPath: string, files: string[], outDir: string): number {
  const start = performance.now();
  try {
    spawnSync(tsgoPath, [
      "--outDir", outDir,
      "--target", "ES2020",
      "--module", "ESNext",
      "--skipLibCheck",
      "--noEmit", "false",
      ...files
    ], {
      stdio: "pipe",
      timeout: 300000
    });
  } catch {}
  return performance.now() - start;
}

// ============================================================================
// Main benchmark
// ============================================================================

async function main(): Promise<void> {
  console.log("EdgeBox Runtime Benchmark");
  console.log("=========================");
  console.log("Based on Microsoft's typescript-go benchmark methodology\n");

  // Install dependencies
  if (!existsSync(join(BENCHMARK_DIR, "node_modules/typescript"))) {
    console.log("Installing dependencies...");
    execSync("npm install typescript tinybench", { cwd: BENCHMARK_DIR, stdio: "inherit" });
  }

  // Ensure TypeScript source is available
  const hasSource = await ensureTypeScriptSource();
  if (!hasSource) {
    console.error("Cannot proceed without TypeScript source");
    process.exit(1);
  }

  // Build tools
  const edgeboxcPath = buildEdgeboxc();
  const compiledTsc = edgeboxcPath ? compileTypescriptWithEdgebox(edgeboxcPath) : null;
  const tsgoPath = getTsgoPath();

  console.log("\nTools available:");
  console.log(`  Node.js tsc: ✓`);
  console.log(`  EdgeBox tsc: ${compiledTsc ? "✓" : "✗"}`);
  console.log(`  tsgo (Go):   ${tsgoPath ? "✓" : "✗"}`);

  // Get test files
  const testFiles = getTestFiles();
  if (testFiles.length === 0) {
    console.error("No test files found");
    process.exit(1);
  }

  const totalLines = testFiles.reduce((sum, f) => sum + countLines(f), 0);
  console.log(`\nTest files: ${testFiles.length}`);
  console.log(`Total lines: ${totalLines.toLocaleString()}`);
  testFiles.forEach(f => {
    const lines = countLines(f);
    console.log(`  - ${f.split("/").pop()}: ${lines.toLocaleString()} lines`);
  });

  // Setup output dirs
  const outBase = join(BENCHMARK_DIR, "out");
  mkdirSync(outBase, { recursive: true });

  // Run benchmark with tinybench
  const bench = new Bench({
    iterations: 5,
    warmup: true,
    warmupIterations: 2,
  });

  const nodeOutDir = join(outBase, "node");
  const edgeboxOutDir = join(outBase, "edgebox");
  const tsgoOutDir = join(outBase, "tsgo");

  // Add Node.js tsc benchmark
  bench.add("Node.js tsc", () => {
    mkdirSync(nodeOutDir, { recursive: true });
    runTscWithNode(testFiles, nodeOutDir);
    rmSync(nodeOutDir, { recursive: true, force: true });
  });

  // Add EdgeBox tsc benchmark if available
  if (compiledTsc) {
    bench.add("EdgeBox tsc", () => {
      mkdirSync(edgeboxOutDir, { recursive: true });
      runTscWithEdgebox(compiledTsc, testFiles, edgeboxOutDir);
      rmSync(edgeboxOutDir, { recursive: true, force: true });
    });
  }

  // Add tsgo benchmark if available
  if (tsgoPath) {
    bench.add("tsgo (Go)", () => {
      mkdirSync(tsgoOutDir, { recursive: true });
      runTsgo(tsgoPath, testFiles, tsgoOutDir);
      rmSync(tsgoOutDir, { recursive: true, force: true });
    });
  }

  console.log("\nRunning benchmarks...\n");
  await bench.run();

  // Print results
  console.log("\n=== Results ===\n");
  console.table(bench.table());

  // Save results as JSON
  const results: BenchmarkResult[] = bench.tasks.map(task => ({
    tool: task.name,
    benchmark: "compile TypeScript compiler source",
    mean: task.result?.mean ?? 0,
    stdDev: task.result?.sd ?? 0,
    samples: task.result?.samples?.length ?? 0,
  }));

  writeFileSync(RESULTS_FILE, JSON.stringify(results, null, 2));
  console.log(`\nResults saved to ${RESULTS_FILE}`);

  // Calculate speedups
  const nodeResult = bench.tasks.find(t => t.name === "Node.js tsc");
  if (nodeResult?.result?.mean) {
    console.log("\n=== Speedup vs Node.js tsc ===\n");
    for (const task of bench.tasks) {
      if (task.name !== "Node.js tsc" && task.result?.mean) {
        const speedup = nodeResult.result.mean / task.result.mean;
        console.log(`  ${task.name}: ${speedup.toFixed(2)}x faster`);
      }
    }
  }

  // Cleanup
  rmSync(outBase, { recursive: true, force: true });
}

main().catch(console.error);
