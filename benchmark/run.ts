#!/usr/bin/env npx tsx
/**
 * TSC Benchmark
 *
 * Based on Microsoft's typescript-go performance comparison.
 * Uses TypeScript compiler source as test input (same as Microsoft).
 *
 * Compares:
 * - EdgeBox-compiled tsc
 * - Node.js tsc
 * - tsgo (Microsoft's Go version)
 */

import { execSync, spawnSync } from "node:child_process";
import { existsSync, mkdirSync, rmSync, readdirSync, readFileSync, writeFileSync } from "node:fs";
import { join } from "node:path";
import { performance } from "node:perf_hooks";

const BENCHMARK_DIR = import.meta.dirname;
const PROJECT_ROOT = join(BENCHMARK_DIR, "..");
const FIXTURES_DIR = join(BENCHMARK_DIR, "fixtures");
const TS_SUBMODULE = join(FIXTURES_DIR, "TypeScript");
const TS_COMPILER_DIR = join(TS_SUBMODULE, "src/compiler");
const RESULTS_FILE = join(BENCHMARK_DIR, "results.json");

// ============================================================================
// Setup
// ============================================================================

function ensureTypeScriptSource(): boolean {
  if (existsSync(join(TS_COMPILER_DIR, "checker.ts"))) {
    return true;
  }

  console.log("Downloading TypeScript source...");
  mkdirSync(FIXTURES_DIR, { recursive: true });

  try {
    execSync(
      `git clone --depth 1 --filter=blob:none --sparse https://github.com/microsoft/TypeScript.git "${TS_SUBMODULE}"`,
      { stdio: "inherit", timeout: 120000 }
    );
    execSync(
      `git -C "${TS_SUBMODULE}" sparse-checkout set src/compiler`,
      { stdio: "inherit", timeout: 60000 }
    );
    return true;
  } catch (e) {
    console.error("Failed to download TypeScript source:", e);
    return false;
  }
}

function buildEdgeboxc(): string | null {
  const path = join(PROJECT_ROOT, "zig-out/bin/edgeboxc");
  if (!existsSync(path)) {
    console.log("Building edgeboxc...");
    try {
      execSync("zig build cli", { cwd: PROJECT_ROOT, stdio: "inherit" });
    } catch {
      return null;
    }
  }
  return existsSync(path) ? path : null;
}

function compileWithEdgebox(edgeboxcPath: string): string | null {
  const tscSource = join(BENCHMARK_DIR, "node_modules/typescript/lib/tsc.js");
  const outputDir = join(PROJECT_ROOT, "zig-out/bin/tsc.js");
  const compiled = join(outputDir, "tsc");

  if (!existsSync(tscSource)) return null;
  if (existsSync(compiled)) return compiled;

  console.log("Compiling tsc with EdgeBox AOT...");
  try {
    execSync(`"${edgeboxcPath}" --binary-only "${tscSource}"`, {
      cwd: PROJECT_ROOT,
      stdio: "inherit",
      timeout: 600000,
    });
  } catch {
    return null;
  }
  return existsSync(compiled) ? compiled : null;
}

function getTsgoPath(): string | null {
  const path = "/tmp/typescript-go/built/local/tsgo";
  return existsSync(path) ? path : null;
}

// ============================================================================
// Benchmark
// ============================================================================

function getTestFiles(): string[] {
  if (!existsSync(TS_COMPILER_DIR)) return [];
  return readdirSync(TS_COMPILER_DIR)
    .filter(f => f.endsWith(".ts"))
    .map(f => join(TS_COMPILER_DIR, f));
}

function countLines(files: string[]): number {
  return files.reduce((sum, f) => {
    return sum + readFileSync(f, "utf-8").split("\n").length;
  }, 0);
}

function runTsc(cmd: string, args: string[], outDir: string, keepOutput = false): { time: number; outputFiles: number; outputSize: number; outputs: Map<string, string> } {
  mkdirSync(outDir, { recursive: true });

  // Replace empty outDir placeholder with actual path
  const finalArgs = args.map(a => a === "" ? outDir : a);

  const start = performance.now();
  spawnSync(cmd, finalArgs, { stdio: "pipe", timeout: 300000 });
  const elapsed = performance.now() - start;

  // Collect output
  let outputFiles = 0;
  let outputSize = 0;
  const outputs = new Map<string, string>();

  if (existsSync(outDir)) {
    const files = readdirSync(outDir).filter(f => f.endsWith(".js"));
    outputFiles = files.length;
    for (const f of files) {
      const content = readFileSync(join(outDir, f), "utf-8");
      outputSize += content.length;
      outputs.set(f, content);
    }
  }

  if (!keepOutput) {
    rmSync(outDir, { recursive: true, force: true });
  }
  return { time: elapsed, outputFiles, outputSize, outputs };
}

function compareOutputs(baseline: Map<string, string>, test: Map<string, string>, baselineName: string, testName: string): boolean {
  if (baseline.size !== test.size) {
    console.error(`  ERROR: ${testName} produced ${test.size} files, ${baselineName} produced ${baseline.size}`);
    return false;
  }

  let identical = true;
  for (const [file, content] of baseline) {
    const testContent = test.get(file);
    if (!testContent) {
      console.error(`  ERROR: ${testName} missing file: ${file}`);
      identical = false;
    } else if (testContent !== content) {
      console.error(`  ERROR: ${testName} output differs for: ${file}`);
      identical = false;
    }
  }

  if (identical) {
    console.log(`  ✓ ${testName} output matches ${baselineName}`);
  }
  return identical;
}

function benchmark(name: string, cmd: string, args: string[], files: string[], runs: number): { mean: number; min: number; max: number; outputFiles: number; outputSize: number; valid: boolean; outputs: Map<string, string> } {
  const outDir = join(BENCHMARK_DIR, "out", name.replace(/\s+/g, "_"));
  const times: number[] = [];
  let outputFiles = 0;
  let outputSize = 0;
  let outputs = new Map<string, string>();

  // Warmup and capture output for validation
  const warmup = runTsc(cmd, [...args, ...files], outDir);
  outputFiles = warmup.outputFiles;
  outputSize = warmup.outputSize;
  outputs = warmup.outputs;

  // Actual runs
  for (let i = 0; i < runs; i++) {
    const result = runTsc(cmd, [...args, ...files], outDir);
    times.push(result.time);

    // Validate consistency
    if (result.outputFiles !== outputFiles || Math.abs(result.outputSize - outputSize) > 100) {
      console.warn(`  Warning: Inconsistent output on run ${i + 1}`);
    }
  }

  const valid = outputFiles > 0 && outputSize > 0;
  if (!valid) {
    console.warn(`  Warning: ${name} produced no output (${outputFiles} files, ${outputSize} bytes)`);
  }

  return {
    mean: times.reduce((a, b) => a + b, 0) / times.length,
    min: Math.min(...times),
    max: Math.max(...times),
    outputFiles,
    outputSize,
    valid,
    outputs,
  };
}

// ============================================================================
// Main
// ============================================================================

async function main() {
  console.log("TSC Benchmark");
  console.log("=============\n");

  // Setup
  if (!existsSync(join(BENCHMARK_DIR, "node_modules/typescript"))) {
    console.log("Installing TypeScript...");
    execSync("npm install typescript", { cwd: BENCHMARK_DIR, stdio: "inherit" });
  }

  if (!ensureTypeScriptSource()) {
    console.error("Cannot proceed without TypeScript source");
    process.exit(1);
  }

  const edgeboxcPath = buildEdgeboxc();
  const edgeboxTsc = edgeboxcPath ? compileWithEdgebox(edgeboxcPath) : null;
  const tsgoPath = getTsgoPath();

  const files = getTestFiles();
  const lines = countLines(files);

  console.log(`Test input: TypeScript compiler source`);
  console.log(`Files: ${files.length}`);
  console.log(`Lines: ${lines.toLocaleString()}\n`);

  console.log("Tools:");
  console.log(`  Node.js tsc: ✓`);
  console.log(`  EdgeBox tsc: ${edgeboxTsc ? "✓" : "✗"}`);
  console.log(`  tsgo (Go):   ${tsgoPath ? "✓" : "✗"}\n`);

  const tscArgs = ["--outDir", "", "--target", "ES2020", "--module", "ESNext", "--skipLibCheck", "--noEmit", "false"];
  const results: any[] = [];
  const RUNS = 5;

  // Node.js tsc
  console.log("Running Node.js tsc...");
  const nodeResult = benchmark(
    "node_tsc",
    "node",
    [join(BENCHMARK_DIR, "node_modules/typescript/lib/tsc.js"), ...tscArgs],
    files,
    RUNS
  );
  results.push({ tool: "Node.js tsc", ...nodeResult });
  console.log(`  Mean: ${nodeResult.mean.toFixed(0)}ms\n`);

  // EdgeBox tsc
  if (edgeboxTsc) {
    console.log("Running EdgeBox tsc...");
    const edgeboxResult = benchmark("edgebox_tsc", edgeboxTsc, tscArgs, files, RUNS);
    results.push({ tool: "EdgeBox tsc", ...edgeboxResult });
    console.log(`  Mean: ${edgeboxResult.mean.toFixed(0)}ms\n`);
  }

  // tsgo
  if (tsgoPath) {
    console.log("Running tsgo...");
    const tsgoResult = benchmark("tsgo", tsgoPath, tscArgs, files, RUNS);
    results.push({ tool: "tsgo (Go)", ...tsgoResult });
    console.log(`  Mean: ${tsgoResult.mean.toFixed(0)}ms\n`);
  }

  // Validate EdgeBox output matches Node.js (same tsc.js should produce same output)
  console.log("\n=== Output Validation ===\n");
  const nodeOutputs = results.find(r => r.tool === "Node.js tsc")?.outputs;
  if (nodeOutputs) {
    for (const r of results) {
      if (r.tool === "EdgeBox tsc" && r.outputs.size > 0) {
        compareOutputs(nodeOutputs, r.outputs, "Node.js tsc", "EdgeBox tsc");
      }
    }
  }

  // Results
  console.log("\n=== Results ===\n");
  console.log("| Tool | Mean (ms) | Min | Max | Output Files | Output Size | Valid |");
  console.log("|------|-----------|-----|-----|--------------|-------------|-------|");
  for (const r of results) {
    console.log(`| ${r.tool} | ${r.mean.toFixed(0)} | ${r.min.toFixed(0)} | ${r.max.toFixed(0)} | ${r.outputFiles} | ${(r.outputSize / 1024).toFixed(0)}KB | ${r.valid ? "✓" : "✗"} |`);
  }

  // Speedup
  const baseline = results.find(r => r.tool === "Node.js tsc");
  if (baseline) {
    console.log("\n=== Speedup vs Node.js ===\n");
    for (const r of results) {
      if (r.tool !== "Node.js tsc") {
        const speedup = baseline.mean / r.mean;
        console.log(`  ${r.tool}: ${speedup.toFixed(2)}x`);
      }
    }
  }

  // Save results (without outputs map for JSON)
  const jsonResults = results.map(r => ({
    tool: r.tool,
    mean: r.mean,
    min: r.min,
    max: r.max,
    outputFiles: r.outputFiles,
    outputSize: r.outputSize,
    valid: r.valid,
  }));
  writeFileSync(RESULTS_FILE, JSON.stringify(jsonResults, null, 2));
  console.log(`\nResults saved to ${RESULTS_FILE}`);
}

main().catch(console.error);
