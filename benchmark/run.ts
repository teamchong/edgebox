#!/usr/bin/env bun
/**
 * EdgeBox Runtime Benchmark
 *
 * Compares EdgeBox-compiled tsc against Node.js tsc and esbuild.
 * This tests EdgeBox as a JavaScript runtime, not as a transpiler.
 *
 * Flow:
 * 1. Build edgeboxc (EdgeBox AOT compiler)
 * 2. Compile npm typescript package with edgeboxc
 * 3. Compare: EdgeBox-tsc vs Node.js-tsc vs esbuild
 */

import { execSync, spawnSync } from "child_process";
import { existsSync, mkdirSync, readdirSync, readFileSync, writeFileSync, rmSync } from "fs";
import { join } from "path";

const BENCHMARK_DIR = import.meta.dirname;
const PROJECT_ROOT = join(BENCHMARK_DIR, "..");
const FIXTURES_DIR = join(BENCHMARK_DIR, "fixtures");
const RESULTS_FILE = join(BENCHMARK_DIR, "results.json");

interface BenchmarkResult {
  tool: string;
  codebase: string;
  files: number;
  lines: number;
  timeMs: number;
  linesPerSecond: number;
}

// ============================================================================
// Setup
// ============================================================================

function buildEdgeboxc(): string {
  const edgeboxcPath = join(PROJECT_ROOT, "zig-out/bin/edgeboxc");

  if (!existsSync(edgeboxcPath)) {
    console.log("Building edgeboxc...");
    execSync("zig build cli", { cwd: PROJECT_ROOT, stdio: "inherit" });
  }

  return edgeboxcPath;
}

function compileTypescriptWithEdgebox(edgeboxcPath: string): string | null {
  const tscSource = join(BENCHMARK_DIR, "node_modules/typescript/lib/tsc.js");
  const outputDir = join(PROJECT_ROOT, "zig-out/bin/tsc.js");
  const compiledTsc = join(outputDir, "tsc");

  if (!existsSync(tscSource)) {
    console.log("TypeScript not installed, skipping EdgeBox benchmark");
    return null;
  }

  if (!existsSync(compiledTsc)) {
    console.log("Compiling TypeScript compiler with EdgeBox...");
    try {
      execSync(`${edgeboxcPath} --binary-only ${tscSource}`, {
        cwd: PROJECT_ROOT,
        stdio: "inherit",
        timeout: 300000, // 5 min timeout
      });
    } catch (e) {
      console.log("Failed to compile tsc with EdgeBox:", e);
      return null;
    }
  }

  return existsSync(compiledTsc) ? compiledTsc : null;
}

// ============================================================================
// Fixture Management
// ============================================================================

function ensureFixtures(): void {
  if (!existsSync(FIXTURES_DIR)) {
    mkdirSync(FIXTURES_DIR, { recursive: true });
  }

  // Generate synthetic TypeScript files
  const syntheticDir = join(FIXTURES_DIR, "synthetic");
  if (!existsSync(syntheticDir)) {
    console.log("Generating synthetic TypeScript files (50k lines)...");
    mkdirSync(syntheticDir, { recursive: true });
    generateSyntheticFiles(syntheticDir, 100, 500);
  }

  const largeDir = join(FIXTURES_DIR, "large");
  if (!existsSync(largeDir)) {
    console.log("Generating large synthetic TypeScript files (200k lines)...");
    mkdirSync(largeDir, { recursive: true });
    generateSyntheticFiles(largeDir, 200, 1000);
  }
}

function generateSyntheticFiles(dir: string, fileCount: number, linesPerFile: number): void {
  const template = `
interface Config {
  host: string;
  port: number;
  debug: boolean;
}

type Handler<T> = (data: T) => Promise<void>;

export class Server<T> {
  private config: Config;
  private handlers: Map<string, Handler<T>>;

  constructor(config: Config) {
    this.config = config;
    this.handlers = new Map();
  }

  public register(name: string, handler: Handler<T>): void {
    this.handlers.set(name, handler);
  }

  public async handle(name: string, data: T): Promise<void> {
    const handler = this.handlers.get(name);
    if (handler) {
      await handler(data);
    }
  }
}

export function createServer<T>(config: Config): Server<T> {
  return new Server<T>(config);
}

const defaultConfig: Config = {
  host: 'localhost',
  port: 3000,
  debug: true,
};

export default createServer;
`.trim();

  const templateLines = template.split("\n").length;
  const reps = Math.ceil(linesPerFile / templateLines);

  for (let i = 0; i < fileCount; i++) {
    const content = Array(reps).fill(template).join("\n\n");
    writeFileSync(join(dir, `module_${i}.ts`), content);
  }
}

// ============================================================================
// File Collection
// ============================================================================

function collectTsFiles(dir: string): string[] {
  const files: string[] = [];

  function walk(currentDir: string): void {
    for (const entry of readdirSync(currentDir, { withFileTypes: true })) {
      const fullPath = join(currentDir, entry.name);
      if (entry.isDirectory() && !entry.name.startsWith(".") && entry.name !== "node_modules") {
        walk(fullPath);
      } else if (entry.isFile() && (entry.name.endsWith(".ts") || entry.name.endsWith(".tsx"))) {
        if (!entry.name.endsWith(".d.ts")) {
          files.push(fullPath);
        }
      }
    }
  }

  walk(dir);
  return files;
}

function countLines(files: string[]): number {
  return files.reduce((total, file) => {
    const content = readFileSync(file, "utf-8");
    return total + content.split("\n").length;
  }, 0);
}

// ============================================================================
// Transpiler Runners
// ============================================================================

function runNodeTsc(files: string[], outDir: string): number {
  const start = performance.now();
  try {
    execSync(
      `node node_modules/typescript/lib/tsc.js --outDir ${outDir} --target ES2020 --module ESNext --skipLibCheck --noEmit false ${files.map((f) => `"${f}"`).join(" ")}`,
      { cwd: BENCHMARK_DIR, stdio: "pipe", maxBuffer: 100 * 1024 * 1024 }
    );
  } catch {
    // tsc may fail on type errors, but we still measure time
  }
  return performance.now() - start;
}

function runEdgeboxTsc(compiledTsc: string, files: string[], outDir: string): number {
  const start = performance.now();
  try {
    execSync(
      `${compiledTsc} --outDir ${outDir} --target ES2020 --module ESNext --skipLibCheck --noEmit false ${files.map((f) => `"${f}"`).join(" ")}`,
      { stdio: "pipe", maxBuffer: 100 * 1024 * 1024 }
    );
  } catch {
    // tsc may fail on type errors, but we still measure time
  }
  return performance.now() - start;
}

function runEsbuild(files: string[], outDir: string): number {
  const start = performance.now();
  try {
    execSync(
      `npx esbuild ${files.map((f) => `"${f}"`).join(" ")} --outdir=${outDir} --format=esm --target=es2020`,
      { cwd: BENCHMARK_DIR, stdio: "pipe", maxBuffer: 100 * 1024 * 1024 }
    );
  } catch {
    // Ignore errors
  }
  return performance.now() - start;
}

function runSwc(files: string[], outDir: string): number {
  const start = performance.now();
  try {
    execSync(
      `npx swc ${files.map((f) => `"${f}"`).join(" ")} --out-dir ${outDir} -C jsc.target=es2020`,
      { cwd: BENCHMARK_DIR, stdio: "pipe", maxBuffer: 100 * 1024 * 1024 }
    );
  } catch {
    // Ignore errors
  }
  return performance.now() - start;
}

// ============================================================================
// Benchmark Runner
// ============================================================================

async function runBenchmark(
  name: string,
  dir: string,
  compiledTsc: string | null
): Promise<BenchmarkResult[]> {
  const files = collectTsFiles(dir);
  const lines = countLines(files);

  console.log(`\n=== ${name} ===`);
  console.log(`Files: ${files.length}, Lines: ${lines.toLocaleString()}`);

  const results: BenchmarkResult[] = [];
  const outBase = join(BENCHMARK_DIR, "out");

  // Warmup
  console.log("Warming up...");
  const warmupDir = join(outBase, "warmup");
  mkdirSync(warmupDir, { recursive: true });
  runEsbuild(files.slice(0, 10), warmupDir);
  rmSync(warmupDir, { recursive: true, force: true });

  // Tools to benchmark
  type RunFn = (files: string[], outDir: string) => number;
  const tools: { name: string; run: RunFn }[] = [
    { name: "node-tsc", run: runNodeTsc },
    { name: "esbuild", run: runEsbuild },
    { name: "swc", run: runSwc },
  ];

  // Add EdgeBox-compiled tsc if available
  if (compiledTsc) {
    tools.unshift({
      name: "edgebox-tsc",
      run: (files, outDir) => runEdgeboxTsc(compiledTsc, files, outDir),
    });
  }

  // Run each tool 3 times, take best
  for (const tool of tools) {
    const times: number[] = [];

    for (let i = 0; i < 3; i++) {
      const outDir = join(outBase, `${tool.name}_${i}`);
      mkdirSync(outDir, { recursive: true });

      const time = tool.run(files, outDir);
      times.push(time);

      rmSync(outDir, { recursive: true, force: true });
    }

    const bestTime = Math.min(...times);
    const lps = lines / (bestTime / 1000);

    results.push({
      tool: tool.name,
      codebase: name,
      files: files.length,
      lines,
      timeMs: bestTime,
      linesPerSecond: lps,
    });

    console.log(`  ${tool.name}: ${bestTime.toFixed(0)}ms (${(lps / 1000).toFixed(1)}k lines/sec)`);
  }

  return results;
}

// ============================================================================
// Main
// ============================================================================

async function main(): Promise<void> {
  console.log("EdgeBox Runtime Benchmark");
  console.log("=========================");
  console.log("Comparing EdgeBox-compiled tsc vs Node.js tsc vs esbuild (Go) vs swc (Rust)\n");

  // Install dependencies
  try {
    execSync("npx tsc --version", { cwd: BENCHMARK_DIR, stdio: "pipe" });
  } catch {
    console.log("Installing TypeScript...");
    execSync("npm install -D typescript esbuild @swc/cli @swc/core", { cwd: BENCHMARK_DIR, stdio: "inherit" });
  }

  // Build edgeboxc and compile tsc
  const edgeboxcPath = buildEdgeboxc();
  const compiledTsc = compileTypescriptWithEdgebox(edgeboxcPath);

  if (compiledTsc) {
    console.log(`EdgeBox-compiled tsc: ${compiledTsc}`);
  } else {
    console.log("Warning: EdgeBox-compiled tsc not available, will compare Node.js vs esbuild only");
  }

  ensureFixtures();

  const allResults: BenchmarkResult[] = [];

  // Run benchmarks
  const syntheticResults = await runBenchmark("Synthetic (50k lines)", join(FIXTURES_DIR, "synthetic"), compiledTsc);
  allResults.push(...syntheticResults);

  const largeResults = await runBenchmark("Large (200k lines)", join(FIXTURES_DIR, "large"), compiledTsc);
  allResults.push(...largeResults);

  // Save results
  writeFileSync(RESULTS_FILE, JSON.stringify(allResults, null, 2));

  // Print summary
  console.log("\n\n=== Summary ===");
  console.log("| Codebase | Tool | Files | Lines | Time (ms) | Lines/sec |");
  console.log("|----------|------|-------|-------|-----------|-----------|");
  for (const r of allResults) {
    console.log(
      `| ${r.codebase.padEnd(20)} | ${r.tool.padEnd(12)} | ${r.files.toString().padStart(5)} | ${r.lines.toLocaleString().padStart(7)} | ${r.timeMs.toFixed(0).padStart(9)} | ${(r.linesPerSecond / 1000).toFixed(1).padStart(7)}k |`
    );
  }

  // Calculate speedups vs Node.js tsc
  console.log("\n=== Speedup vs Node.js tsc ===");
  const nodeTscResults = allResults.filter((r) => r.tool === "node-tsc");
  for (const nodeTsc of nodeTscResults) {
    const others = allResults.filter((r) => r.codebase === nodeTsc.codebase && r.tool !== "node-tsc");
    console.log(`\n${nodeTsc.codebase}:`);
    for (const other of others) {
      const speedup = nodeTsc.timeMs / other.timeMs;
      console.log(`  ${other.tool}: ${speedup.toFixed(1)}x faster`);
    }
  }
}

main().catch(console.error);
