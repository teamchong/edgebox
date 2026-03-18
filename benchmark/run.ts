#!/usr/bin/env npx tsx
/**
 * TSC Benchmark - Microsoft tsgo Test Suite
 *
 * Uses the same popular open-source projects as Microsoft's typescript-go benchmark.
 * Compares EdgeBox-compiled tsc vs Node.js tsc and validates output correctness.
 *
 * Usage:
 *   npx tsx run.ts              # Run all projects
 *   npx tsx run.ts --quick      # Run only rxjs + trpc (fast feedback)
 *   npx tsx run.ts --no-vscode  # Skip VSCode (saves time)
 *   npx tsx run.ts --runs 5     # Set number of runs per project
 *
 * Projects (from Microsoft's tsgo benchmark):
 * - rxjs (2.1K LOC)
 * - tRPC (18K LOC)
 * - date-fns (104K LOC)
 * - TypeORM (270K LOC)
 * - Playwright (356K LOC)
 * - VSCode (1.5M LOC)
 */

import { execSync, spawnSync } from "node:child_process";
import { existsSync, mkdirSync, rmSync, readdirSync, readFileSync, writeFileSync, statSync } from "node:fs";
import { join, relative } from "node:path";
import { performance } from "node:perf_hooks";

// ============================================================================
// Configuration
// ============================================================================

const BENCHMARK_DIR = import.meta.dirname;
const PROJECT_ROOT = join(BENCHMARK_DIR, "..");
const FIXTURES_DIR = join(BENCHMARK_DIR, "fixtures");
const OUT_DIR = join(BENCHMARK_DIR, "out");
const RESULTS_FILE = join(BENCHMARK_DIR, "results.json");

interface Project {
  name: string;
  repo: string;
  path: string;
  quick?: boolean; // Include in --quick mode
}

// Microsoft's tsgo benchmark projects
const ALL_PROJECTS: Project[] = [
  { name: "rxjs", repo: "ReactiveX/rxjs", path: "packages/rxjs/src", quick: true },
  { name: "trpc", repo: "trpc/trpc", path: "packages/server/src", quick: true },
  { name: "date-fns", repo: "date-fns/date-fns", path: "src" },
  { name: "typeorm", repo: "typeorm/typeorm", path: "src" },
  { name: "playwright", repo: "microsoft/playwright", path: "packages/playwright-core/src" },
  { name: "vscode", repo: "microsoft/vscode", path: "src" },
];

// ============================================================================
// CLI Argument Parsing
// ============================================================================

interface Options {
  quick: boolean;
  noVscode: boolean;
  runs: number;
  help: boolean;
  debug: boolean;
  includeTsgo: boolean;
  includeBun: boolean;
}

function parseArgs(): Options {
  const args = process.argv.slice(2);
  const options: Options = {
    quick: false,
    noVscode: false,
    runs: 3,
    help: false,
    debug: false,
    includeTsgo: false,
    includeBun: false,
  };

  for (let i = 0; i < args.length; i++) {
    const arg = args[i];
    switch (arg) {
      case "--quick":
      case "-q":
        options.quick = true;
        break;
      case "--no-vscode":
        options.noVscode = true;
        break;
      case "--runs":
      case "-r":
        options.runs = parseInt(args[++i] || "3", 10);
        break;
      case "--help":
      case "-h":
        options.help = true;
        break;
      case "--debug":
      case "-d":
        options.debug = true;
        break;
      case "--include-tsgo":
        options.includeTsgo = true;
        break;
      case "--include-bun":
        options.includeBun = true;
        break;
    }
  }

  return options;
}

function printHelp() {
  console.log(`
TSC Benchmark - Microsoft tsgo Test Suite

Usage:
  npx tsx run.ts [options]

Options:
  --quick, -q       Run only rxjs + trpc (fast feedback)
  --no-vscode       Skip VSCode project (saves time)
  --runs, -r N      Number of runs per project (default: 3)
  --debug, -d       Enable debug output (stdout/stderr from runs)
  --include-tsgo    Include tsgo in comparison (requires tsgo installed)
  --include-bun     Include Bun in comparison (requires bun installed)
  --help, -h        Show this help message

Examples:
  npx tsx run.ts               # Run all projects
  npx tsx run.ts --quick       # Quick test with 2 small projects
  npx tsx run.ts --runs 5      # More runs for stable measurements
  npx tsx run.ts --include-tsgo  # Include tsgo comparison
  npx tsx run.ts --include-bun   # Include Bun comparison
`);
}

// ============================================================================
// Setup Functions
// ============================================================================

function downloadProject(project: Project): string | null {
  const dir = join(FIXTURES_DIR, project.name);
  const srcPath = join(dir, project.path);

  if (existsSync(srcPath)) {
    console.log(`  ${project.name}: Using cached`);
    return dir;
  }

  console.log(`  ${project.name}: Downloading...`);
  mkdirSync(FIXTURES_DIR, { recursive: true });

  try {
    // Remove existing directory if present
    if (existsSync(dir)) {
      rmSync(dir, { recursive: true, force: true });
    }

    // Sparse checkout - only get the src folder
    execSync(
      `git clone --depth 1 --filter=blob:none --sparse https://github.com/${project.repo}.git "${dir}"`,
      { stdio: "pipe", timeout: 120000 }
    );
    execSync(
      `git -C "${dir}" sparse-checkout set ${project.path}`,
      { stdio: "pipe", timeout: 60000 }
    );

    if (!existsSync(srcPath)) {
      console.error(`  ${project.name}: Failed - src path not found`);
      return null;
    }

    return dir;
  } catch (e) {
    console.error(`  ${project.name}: Failed to download -`, (e as Error).message);
    return null;
  }
}

function buildEdgeboxTsc(): string | null {
  const edgeboxPath = join(PROJECT_ROOT, "zig-out/bin/edgebox");
  const tscSourceAbs = join(BENCHMARK_DIR, "node_modules/typescript/lib/_tsc.js");
  // Use relative path from PROJECT_ROOT to avoid path.join issues with absolute paths
  const tscSourceRel = relative(PROJECT_ROOT, tscSourceAbs);
  const outputDir = join(PROJECT_ROOT, "zig-out/bin", tscSourceRel);
  const workerMjs = join(outputDir, "_tsc-worker.mjs");

  // Check if already compiled
  if (existsSync(workerMjs)) {
    return workerMjs;
  }

  // Build edgebox if needed
  if (!existsSync(edgeboxPath)) {
    console.log("Building edgebox...");
    try {
      execSync("zig build cli -Doptimize=ReleaseFast", { cwd: PROJECT_ROOT, stdio: "inherit" });
    } catch {
      console.error("Failed to build edgebox");
      return null;
    }
  }

  // Compile tsc with EdgeBox (worker path: .mjs + .wasm)
  console.log("Compiling tsc with EdgeBox AOT (worker path)...");
  try {
    execSync(`"${edgeboxPath}" "${tscSourceRel}"`, {
      cwd: PROJECT_ROOT,
      stdio: "pipe",  // Suppress build output
      timeout: 600000,
    });
  } catch {
    console.error("Failed to compile tsc with EdgeBox");
    return null;
  }

  return existsSync(workerMjs) ? workerMjs : null;
}

// ============================================================================
// File Utilities
// ============================================================================

function findTsFiles(dir: string): string[] {
  const files: string[] = [];

  function walk(currentDir: string) {
    if (!existsSync(currentDir)) return;

    for (const entry of readdirSync(currentDir, { withFileTypes: true })) {
      const fullPath = join(currentDir, entry.name);

      if (entry.isDirectory()) {
        // Skip node_modules and hidden directories
        if (entry.name !== "node_modules" && !entry.name.startsWith(".")) {
          walk(fullPath);
        }
      } else if (entry.name.endsWith(".ts") && !entry.name.endsWith(".d.ts")) {
        files.push(fullPath);
      }
    }
  }

  walk(dir);
  return files;
}

function countLines(files: string[]): number {
  return files.reduce((sum, f) => {
    try {
      return sum + readFileSync(f, "utf-8").split("\n").length;
    } catch {
      return sum;
    }
  }, 0);
}

function findJsFiles(dir: string): string[] {
  if (!existsSync(dir)) return [];

  const files: string[] = [];

  function walk(currentDir: string) {
    for (const entry of readdirSync(currentDir, { withFileTypes: true })) {
      const fullPath = join(currentDir, entry.name);
      if (entry.isDirectory()) {
        walk(fullPath);
      } else if (entry.name.endsWith(".js")) {
        files.push(relative(dir, fullPath));
      }
    }
  }

  walk(dir);
  return files.sort();
}

// ============================================================================
// Benchmark Functions
// ============================================================================

interface RunResult {
  time: number;
  success: boolean;
  outputFiles: number;
  outputSize: number;
  diagnostics: number; // count of "error TS" lines in stdout
  error?: string;
}

function runTsc(cmd: string, args: string[], outDir: string, debug: boolean = false): RunResult {
  // Clean output directory
  if (existsSync(outDir)) {
    rmSync(outDir, { recursive: true, force: true });
  }
  mkdirSync(outDir, { recursive: true });

  const start = performance.now();
  const result = spawnSync(cmd, args, {
    stdio: "pipe",
    timeout: 300000,
    env: { ...process.env, NO_COLOR: "1" },
  });
  const elapsed = performance.now() - start;

  // Count type errors from stdout (tsc prints "error TS" diagnostics to stdout)
  const stdout = result.stdout?.toString() || "";
  const diagnostics = (stdout.match(/error TS\d+/g) || []).length;

  // Check for errors
  if (result.status !== 0 && result.status !== 2) {
    // status 2 is type errors, which is OK
    const stderr = result.stderr?.toString() || "";
    if (stderr.includes("error") && !stderr.includes("error TS")) {
      return {
        time: elapsed,
        success: false,
        outputFiles: 0,
        outputSize: 0,
        diagnostics: 0,
        error: stderr.slice(0, 200),
      };
    }
  }

  // Debug output (only when --debug flag is set)
  if (debug) {
    const stderr = result.stderr?.toString() || "";
    if (stderr.length > 0 || stdout.length > 0) {
      console.log(`    DEBUG stdout (${stdout.length}): ${stdout.slice(0, 500)}`);
      console.log(`    DEBUG stderr (${stderr.length}): ${stderr.slice(0, 500)}`);
      console.log(`    DEBUG status: ${result.status}, diagnostics: ${diagnostics}`);
    }
  }

  // Count output files
  const jsFiles = findJsFiles(outDir);
  let outputSize = 0;
  for (const f of jsFiles) {
    try {
      outputSize += statSync(join(outDir, f)).size;
    } catch {}
  }

  return {
    time: elapsed,
    success: true,
    outputFiles: jsFiles.length,
    outputSize,
    diagnostics,
  };
}


interface BenchmarkResult {
  project: string;
  lines: number;
  files: number;
  nodeMean: number;
  nodeMin: number;
  nodeMax: number;
  edgeboxMean: number;
  edgeboxMin: number;
  edgeboxMax: number;
  speedup: number;
  nodeDiagnostics: number;
  edgeboxDiagnostics: number;
  outputMatch: boolean;
  outputDiff?: string;
  // tsgo data (optional)
  tsgoMean?: number;
  tsgoMin?: number;
  tsgoMax?: number;
  tsgoSpeedup?: number;
  // bun data (optional)
  bunMean?: number;
  bunMin?: number;
  bunMax?: number;
  bunSpeedup?: number;
}

interface BenchmarkOptions {
  runs: number;
  debug: boolean;
  includeTsgo: boolean;
  tsgoPath?: string;
  includeBun: boolean;
  bunPath?: string;
}

function benchmarkProject(
  project: Project,
  projectDir: string,
  nodeTsc: string,
  edgeboxTsc: string,
  options: BenchmarkOptions
): BenchmarkResult | null {
  const { runs, debug, includeTsgo, tsgoPath, includeBun, bunPath } = options;
  const srcDir = join(projectDir, project.path);
  const tsFiles = findTsFiles(srcDir);

  if (tsFiles.length === 0) {
    console.log(`  ${project.name}: No TypeScript files found`);
    return null;
  }

  const lines = countLines(tsFiles);
  console.log(`  ${project.name}: ${tsFiles.length} files, ${lines.toLocaleString()} lines`);

  const nodeOutDir = join(OUT_DIR, `${project.name}_node`);
  const edgeboxOutDir = join(OUT_DIR, `${project.name}_edgebox`);
  const tsgoOutDir = join(OUT_DIR, `${project.name}_tsgo`);

  const tscArgs = [
    "--noEmit",
    "--target", "ES2020",
    "--module", "ESNext",
    "--moduleResolution", "node",
    "--skipLibCheck",
    ...tsFiles,
  ];

  // Benchmark Node.js tsc
  const nodeTimes: number[] = [];
  let nodeResult: RunResult = { time: 0, success: false, outputFiles: 0, outputSize: 0, diagnostics: 0 };

  for (let i = 0; i < runs; i++) {
    nodeResult = runTsc("node", [nodeTsc, ...tscArgs], nodeOutDir, debug);
    if (!nodeResult.success) {
      console.log(`    Node.js: Failed - ${nodeResult.error}`);
      return null;
    }
    nodeTimes.push(nodeResult.time);
  }

  // Benchmark EdgeBox tsc (worker .mjs via Node.js — V8 inlines WASM)
  const edgeboxTimes: number[] = [];
  let edgeboxResult: RunResult = { time: 0, success: false, outputFiles: 0, outputSize: 0, diagnostics: 0 };

  for (let i = 0; i < runs; i++) {
    edgeboxResult = runTsc("node", [edgeboxTsc, ...tscArgs], edgeboxOutDir, debug);
    if (!edgeboxResult.success) {
      console.log(`    EdgeBox: Failed - ${edgeboxResult.error}`);
      return null;
    }
    edgeboxTimes.push(edgeboxResult.time);
  }

  // Benchmark Bun tsc (optional)
  const bunOutDir = join(OUT_DIR, `${project.name}_bun`);
  let bunTimes: number[] = [];

  if (includeBun && bunPath) {
    for (let i = 0; i < runs; i++) {
      const bunResult = runTsc(bunPath, [nodeTsc, ...tscArgs], bunOutDir, debug);
      if (!bunResult.success) {
        console.log(`    Bun: Failed - ${bunResult.error}`);
        bunTimes = [];
        break;
      }
      bunTimes.push(bunResult.time);
    }
  }

  // Benchmark tsgo (optional)
  let tsgoTimes: number[] = [];
  let tsgoResult: RunResult | undefined;

  if (includeTsgo && tsgoPath) {
    for (let i = 0; i < runs; i++) {
      tsgoResult = runTsc(tsgoPath, tscArgs, tsgoOutDir, debug);
      if (!tsgoResult.success) {
        console.log(`    tsgo: Failed - ${tsgoResult.error}`);
        tsgoTimes = []; // Skip tsgo stats
        break;
      }
      tsgoTimes.push(tsgoResult.time);
    }
  }

  // Compare diagnostic counts (with --noEmit, both should report the same type errors)
  const diagMatch = nodeResult.diagnostics === edgeboxResult.diagnostics;
  const comparison = {
    match: diagMatch,
    diff: diagMatch ? undefined : `Diagnostics: Node=${nodeResult.diagnostics} EdgeBox=${edgeboxResult.diagnostics}`,
  };

  // Calculate stats
  const nodeMean = nodeTimes.reduce((a, b) => a + b, 0) / nodeTimes.length;
  const nodeMin = Math.min(...nodeTimes);
  const nodeMax = Math.max(...nodeTimes);

  const edgeboxMean = edgeboxTimes.reduce((a, b) => a + b, 0) / edgeboxTimes.length;
  const edgeboxMin = Math.min(...edgeboxTimes);
  const edgeboxMax = Math.max(...edgeboxTimes);

  const speedup = nodeMean / edgeboxMean;

  // Bun stats (optional)
  let bunMean: number | undefined;
  let bunMin: number | undefined;
  let bunMax: number | undefined;
  let bunSpeedup: number | undefined;

  if (bunTimes.length > 0) {
    bunMean = bunTimes.reduce((a, b) => a + b, 0) / bunTimes.length;
    bunMin = Math.min(...bunTimes);
    bunMax = Math.max(...bunTimes);
    bunSpeedup = bunMean / edgeboxMean;
  }

  // tsgo stats (optional)
  let tsgoMean: number | undefined;
  let tsgoMin: number | undefined;
  let tsgoMax: number | undefined;
  let tsgoSpeedup: number | undefined;

  if (tsgoTimes.length > 0) {
    tsgoMean = tsgoTimes.reduce((a, b) => a + b, 0) / tsgoTimes.length;
    tsgoMin = Math.min(...tsgoTimes);
    tsgoMax = Math.max(...tsgoTimes);
    tsgoSpeedup = nodeMean / tsgoMean;
  }

  console.log(`    Node.js:  ${nodeMean.toFixed(0)}ms (${nodeResult.diagnostics} errors)`);
  if (bunMean !== undefined) {
    console.log(`    Bun:      ${bunMean.toFixed(0)}ms - ${bunSpeedup!.toFixed(2)}x vs EdgeBox`);
  }
  console.log(`    EdgeBox:  ${edgeboxMean.toFixed(0)}ms (${edgeboxResult.diagnostics} errors) - ${speedup.toFixed(2)}x vs Node`);
  if (tsgoMean !== undefined) {
    console.log(`    tsgo:     ${tsgoMean.toFixed(0)}ms - ${tsgoSpeedup!.toFixed(2)}x vs Node`);
  }
  console.log(`    Match:    ${comparison.match ? "✓ Same diagnostics" : `✗ ${comparison.diff}`}`);

  // Cleanup
  rmSync(nodeOutDir, { recursive: true, force: true });
  rmSync(edgeboxOutDir, { recursive: true, force: true });
  if (existsSync(bunOutDir)) {
    rmSync(bunOutDir, { recursive: true, force: true });
  }
  if (existsSync(tsgoOutDir)) {
    rmSync(tsgoOutDir, { recursive: true, force: true });
  }

  return {
    project: project.name,
    lines,
    files: tsFiles.length,
    nodeMean,
    nodeMin,
    nodeMax,
    edgeboxMean,
    edgeboxMin,
    edgeboxMax,
    speedup,
    nodeDiagnostics: nodeResult.diagnostics,
    edgeboxDiagnostics: edgeboxResult.diagnostics,
    outputMatch: comparison.match,
    outputDiff: comparison.diff,
    bunMean,
    bunMin,
    bunMax,
    bunSpeedup,
    tsgoMean,
    tsgoMin,
    tsgoMax,
    tsgoSpeedup,
  };
}

// ============================================================================
// Main
// ============================================================================

async function main() {
  const options = parseArgs();

  if (options.help) {
    printHelp();
    process.exit(0);
  }

  // Filter projects based on options
  let projects = [...ALL_PROJECTS];
  if (options.quick) {
    projects = projects.filter(p => p.quick);
  }
  if (options.noVscode) {
    projects = projects.filter(p => p.name !== "vscode");
  }

  console.log("╔════════════════════════════════════════════════════════════════╗");
  console.log("║     TSC Benchmark - Microsoft tsgo Test Suite                  ║");
  console.log("╚════════════════════════════════════════════════════════════════╝\n");

  if (options.quick) {
    console.log("Mode: Quick (rxjs + trpc only)\n");
  }

  // Ensure TypeScript is installed
  if (!existsSync(join(BENCHMARK_DIR, "node_modules/typescript"))) {
    console.log("Installing TypeScript...\n");
    execSync("npm install typescript", { cwd: BENCHMARK_DIR, stdio: "inherit" });
  }

  const nodeTsc = join(BENCHMARK_DIR, "node_modules/typescript/lib/tsc.js");
  if (!existsSync(nodeTsc)) {
    console.error("TypeScript not found. Run: npm install typescript");
    process.exit(1);
  }

  // Build EdgeBox tsc
  console.log("=== Setup ===\n");
  const edgeboxTsc = buildEdgeboxTsc();
  if (!edgeboxTsc) {
    console.error("Failed to build EdgeBox tsc");
    process.exit(1);
  }
  console.log(`EdgeBox tsc: ${edgeboxTsc}`);

  // Find Bun if requested
  let bunPath: string | undefined;
  if (options.includeBun) {
    try {
      bunPath = execSync("which bun", { encoding: "utf-8" }).trim();
      console.log(`Bun: ${bunPath}`);
    } catch {
      console.log("Bun: Not found (skipping Bun comparison)");
      bunPath = undefined;
    }
  }

  // Find tsgo if requested
  let tsgoPath: string | undefined;
  if (options.includeTsgo) {
    try {
      tsgoPath = execSync("which tsgo", { encoding: "utf-8" }).trim();
      console.log(`tsgo: ${tsgoPath}`);
    } catch {
      console.log("tsgo: Not found (skipping tsgo comparison)");
      tsgoPath = undefined;
    }
  }
  console.log();

  // Download projects
  console.log("=== Downloading Projects ===\n");
  const projectDirs: Map<string, string> = new Map();

  for (const project of projects) {
    const dir = downloadProject(project);
    if (dir) {
      projectDirs.set(project.name, dir);
    }
  }

  console.log(`\nDownloaded ${projectDirs.size}/${projects.length} projects\n`);

  // Run benchmarks
  console.log(`=== Running Benchmarks (${options.runs} runs each) ===\n`);
  mkdirSync(OUT_DIR, { recursive: true });

  const benchOptions: BenchmarkOptions = {
    runs: options.runs,
    debug: options.debug,
    includeTsgo: options.includeTsgo,
    tsgoPath,
    includeBun: options.includeBun,
    bunPath,
  };

  const results: BenchmarkResult[] = [];

  for (const project of projects) {
    const dir = projectDirs.get(project.name);
    if (!dir) {
      console.log(`  ${project.name}: Skipped (not downloaded)`);
      continue;
    }

    const result = benchmarkProject(project, dir, nodeTsc, edgeboxTsc, benchOptions);
    if (result) {
      results.push(result);
    }
    console.log();
  }

  // Check if any results have optional data
  const hasBun = results.some(r => r.bunMean !== undefined);
  const hasTsgo = results.some(r => r.tsgoMean !== undefined);

  // Print results table
  console.log("╔════════════════════════════════════════════════════════════════════════════════════════════════════╗");
  console.log("║                                          RESULTS                                                   ║");
  console.log("╚════════════════════════════════════════════════════════════════════════════════════════════════════╝\n");

  // Build header dynamically
  let header = "| Project    | Lines     | Node.js (ms) |";
  let separator = "|------------|-----------|--------------|";
  if (hasBun) { header += " Bun (ms)     |"; separator += "--------------|"; }
  header += " EdgeBox (ms) | vs Node |";
  separator += "--------------|---------|";
  if (hasBun) { header += " vs Bun  |"; separator += "---------|"; }
  if (hasTsgo) { header += " tsgo (ms) | tsgo Speedup |"; separator += "-----------|--------------|"; }
  header += " Match |";
  separator += "-------|";

  console.log(header);
  console.log(separator);

  for (const r of results) {
    const lines = r.lines.toLocaleString().padStart(9);
    const node = r.nodeMean.toFixed(0).padStart(12);
    const edgebox = r.edgeboxMean.toFixed(0).padStart(12);
    const speedup = `${r.speedup.toFixed(2)}x`.padStart(7);
    const match = r.outputMatch ? "  ✓  " : "  ✗  ";

    let row = `| ${r.project.padEnd(10)} | ${lines} | ${node} |`;
    if (hasBun) {
      const bun = r.bunMean !== undefined ? r.bunMean.toFixed(0).padStart(12) : "-".padStart(12);
      row += ` ${bun} |`;
    }
    row += ` ${edgebox} | ${speedup} |`;
    if (hasBun) {
      const bunSpd = r.bunSpeedup !== undefined ? `${r.bunSpeedup.toFixed(2)}x`.padStart(7) : "-".padStart(7);
      row += ` ${bunSpd} |`;
    }
    if (hasTsgo) {
      const tsgo = r.tsgoMean !== undefined ? r.tsgoMean.toFixed(0).padStart(9) : "-".padStart(9);
      const tsgoSpd = r.tsgoSpeedup !== undefined ? `${r.tsgoSpeedup.toFixed(2)}x`.padStart(12) : "-".padStart(12);
      row += ` ${tsgo} | ${tsgoSpd} |`;
    }
    row += ` ${match} |`;
    console.log(row);
  }

  // Summary
  if (results.length > 0) {
    const totalLines = results.reduce((sum, r) => sum + r.lines, 0);
    const avgSpeedup = results.reduce((sum, r) => sum + r.speedup, 0) / results.length;
    const allMatch = results.every(r => r.outputMatch);

    const bunResults = results.filter(r => r.bunSpeedup !== undefined);
    const avgBunSpeedup = bunResults.length > 0
      ? bunResults.reduce((sum, r) => sum + r.bunSpeedup!, 0) / bunResults.length
      : 0;

    const tsgoResults = results.filter(r => r.tsgoSpeedup !== undefined);
    const avgTsgoSpeedup = tsgoResults.length > 0
      ? tsgoResults.reduce((sum, r) => sum + r.tsgoSpeedup!, 0) / tsgoResults.length
      : 0;

    console.log(separator);
    let summary = `| TOTAL      | ${totalLines.toLocaleString().padStart(9)} |              |`;
    if (hasBun) summary += `              |`;
    summary += `              | ${avgSpeedup.toFixed(2)}x avg |`;
    if (hasBun) summary += ` ${avgBunSpeedup > 0 ? avgBunSpeedup.toFixed(2) + 'x avg' : '-'.padStart(7)} |`;
    if (hasTsgo) summary += `           | ${avgTsgoSpeedup > 0 ? avgTsgoSpeedup.toFixed(2) + 'x avg' : '-'.padStart(12)} |`;
    summary += `${allMatch ? "  ✓  " : "  ✗  "} |`;
    console.log(summary);
  }

  // Save results
  writeFileSync(RESULTS_FILE, JSON.stringify(results, null, 2));
  console.log(`\nResults saved to ${RESULTS_FILE}`);

  // Output validation summary
  const mismatches = results.filter(r => !r.outputMatch);
  if (mismatches.length > 0) {
    console.log("\nDiagnostic count mismatches:");
    for (const r of mismatches) {
      console.log(`   ${r.project}: ${r.outputDiff}`);
    }
    process.exit(1);
  } else if (results.length > 0) {
    console.log("\n✓ All diagnostics match Node.js tsc");
  }
}

main().catch(console.error);
