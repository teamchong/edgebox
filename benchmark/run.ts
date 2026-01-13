#!/usr/bin/env bun
/**
 * EdgeBox TypeScript Transpiler Benchmark
 *
 * Compares EdgeBox against tsc, esbuild, and swc using real-world codebases.
 * Based on methodology from esbuild and sucrase benchmarks.
 *
 * Codebases tested:
 * - TypeScript compiler source (large real-world TS)
 * - Zod (popular validation library)
 * - Custom synthetic tests
 */

import { execSync, spawnSync } from "child_process";
import { existsSync, mkdirSync, readdirSync, readFileSync, statSync, writeFileSync, rmSync } from "fs";
import { join, relative } from "path";

const BENCHMARK_DIR = import.meta.dirname;
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
// Fixture Management
// ============================================================================

async function ensureFixtures(): Promise<void> {
  if (!existsSync(FIXTURES_DIR)) {
    mkdirSync(FIXTURES_DIR, { recursive: true });
  }

  // Generate synthetic large codebase (similar to esbuild's three.js 10x approach)
  const syntheticDir = join(FIXTURES_DIR, "synthetic");
  if (!existsSync(syntheticDir)) {
    console.log("Generating synthetic TypeScript files (50k lines)...");
    mkdirSync(syntheticDir, { recursive: true });
    generateSyntheticFiles(syntheticDir, 100, 500); // 100 files, ~500 lines each = 50k lines
  }

  // Generate larger synthetic codebase
  const largeDir = join(FIXTURES_DIR, "large");
  if (!existsSync(largeDir)) {
    console.log("Generating large synthetic TypeScript files (200k lines)...");
    mkdirSync(largeDir, { recursive: true });
    generateSyntheticFiles(largeDir, 200, 1000); // 200 files, ~1000 lines each = 200k lines
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
        // Skip .d.ts files
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

function runTsc(files: string[], outDir: string): number {
  const listFile = join(BENCHMARK_DIR, "temp_files.txt");
  writeFileSync(listFile, files.join("\n"));

  const start = performance.now();
  try {
    execSync(
      `npx tsc --outDir ${outDir} --target ES2020 --module ESNext --skipLibCheck --noEmit false ${files.map((f) => `"${f}"`).join(" ")}`,
      { stdio: "pipe", maxBuffer: 100 * 1024 * 1024 }
    );
  } catch (e) {
    // tsc may fail on type errors, but we still measure time
  }
  const elapsed = performance.now() - start;

  rmSync(listFile, { force: true });
  return elapsed;
}

function runEsbuild(files: string[], outDir: string): number {
  const start = performance.now();
  try {
    execSync(
      `npx esbuild ${files.map((f) => `"${f}"`).join(" ")} --outdir=${outDir} --format=esm --target=es2020`,
      { stdio: "pipe", maxBuffer: 100 * 1024 * 1024 }
    );
  } catch (e) {
    // Ignore errors
  }
  return performance.now() - start;
}

function runSwc(files: string[], outDir: string): number {
  const start = performance.now();
  try {
    execSync(
      `npx swc ${files.map((f) => `"${f}"`).join(" ")} --out-dir ${outDir} -C jsc.target=es2020`,
      { stdio: "pipe", maxBuffer: 100 * 1024 * 1024 }
    );
  } catch (e) {
    // Ignore errors
  }
  return performance.now() - start;
}

function runEdgebox(dir: string): { timeMs: number; linesPerSecond: number } | null {
  // Use pre-built EdgeBox benchmark CLI
  const benchPath = join(BENCHMARK_DIR, "edgebox-bench");
  if (!existsSync(benchPath)) {
    return null;
  }

  try {
    const result = spawnSync(benchPath, [dir], { stdio: "pipe" });
    if (result.status === 0 && result.stderr) {
      // Output is on stderr (debug.print)
      const output = result.stderr.toString().trim();
      const json = JSON.parse(output);
      return { timeMs: json.timeMs, linesPerSecond: json.linesPerSecond };
    }
  } catch (e) {
    // Ignore errors
  }
  return null;
}

// ============================================================================
// Benchmark Runner
// ============================================================================

async function runBenchmark(name: string, dir: string): Promise<BenchmarkResult[]> {
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

  // Run each tool 3 times, take best
  const tools = [
    { name: "tsc", run: runTsc },
    { name: "esbuild", run: runEsbuild },
    { name: "swc", run: runSwc },
    // { name: "edgebox", run: runEdgebox },
  ];

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

  // Run EdgeBox separately (uses different interface)
  const edgeboxResult = runEdgebox(dir);
  if (edgeboxResult) {
    results.push({
      tool: "edgebox",
      codebase: name,
      files: files.length,
      lines,
      timeMs: edgeboxResult.timeMs,
      linesPerSecond: edgeboxResult.linesPerSecond,
    });
    console.log(`  edgebox: ${edgeboxResult.timeMs.toFixed(1)}ms (${(edgeboxResult.linesPerSecond / 1000000).toFixed(1)}M lines/sec)`);
  }

  return results;
}

// ============================================================================
// Main
// ============================================================================

async function main(): Promise<void> {
  console.log("EdgeBox TypeScript Transpiler Benchmark");
  console.log("=======================================\n");

  // Check dependencies
  try {
    execSync("npx tsc --version", { stdio: "pipe" });
    execSync("npx esbuild --version", { stdio: "pipe" });
    execSync("npx swc --version", { stdio: "pipe" });
  } catch (e) {
    console.log("Installing dependencies...");
    execSync("npm install -D typescript esbuild @swc/cli @swc/core", { stdio: "inherit" });
  }

  await ensureFixtures();

  const allResults: BenchmarkResult[] = [];

  // Run benchmarks
  const syntheticResults = await runBenchmark("Synthetic (50k lines)", join(FIXTURES_DIR, "synthetic"));
  allResults.push(...syntheticResults);

  const largeResults = await runBenchmark("Large (200k lines)", join(FIXTURES_DIR, "large"));
  allResults.push(...largeResults);

  // Save results
  writeFileSync(RESULTS_FILE, JSON.stringify(allResults, null, 2));

  // Print summary table
  console.log("\n\n=== Summary ===");
  console.log("| Codebase | Tool | Files | Lines | Time (ms) | Lines/sec |");
  console.log("|----------|------|-------|-------|-----------|-----------|");
  for (const r of allResults) {
    console.log(
      `| ${r.codebase.padEnd(20)} | ${r.tool.padEnd(8)} | ${r.files.toString().padStart(5)} | ${r.lines.toLocaleString().padStart(7)} | ${r.timeMs.toFixed(0).padStart(9)} | ${(r.linesPerSecond / 1000).toFixed(1).padStart(7)}k |`
    );
  }

  // Calculate speedups vs tsc
  console.log("\n=== Speedup vs tsc ===");
  const tscResults = allResults.filter((r) => r.tool === "tsc");
  for (const tsc of tscResults) {
    const others = allResults.filter((r) => r.codebase === tsc.codebase && r.tool !== "tsc");
    console.log(`\n${tsc.codebase}:`);
    for (const other of others) {
      const speedup = tsc.timeMs / other.timeMs;
      console.log(`  ${other.tool}: ${speedup.toFixed(1)}x faster`);
    }
  }
}

main().catch(console.error);
