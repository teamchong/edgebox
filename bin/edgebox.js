#!/usr/bin/env node
// EdgeBox CLI — compiles JS to V8+WASM and packs V8 pool single binaries
// Usage: npx edgebox <app.js>       — compile
//        npx edgebox pack <dir>     — pack V8 pool binary

import { execFileSync, execSync } from "node:child_process";
import {
  existsSync,
  readFileSync,
  writeFileSync,
  readdirSync,
  chmodSync,
  rmSync,
} from "node:fs";
import { join, dirname, basename, resolve } from "node:path";
import { fileURLToPath } from "node:url";

const __dirname = dirname(fileURLToPath(import.meta.url));
const projectRoot = join(__dirname, "..");

function findBinary() {
  const candidates = [join(projectRoot, "zig-out", "bin", "edgebox")];
  return candidates.find((p) => existsSync(p));
}

function findV8Pool() {
  try {
    execSync("V8 pool --version", { stdio: "pipe" });
    return "V8 pool";
  } catch {}
  try {
    execSync("npx V8 pool --version", { stdio: "pipe" });
    return "npx V8 pool";
  } catch {}
  return null;
}

function findWorkerFiles(workerDir) {
  const abs = resolve(workerDir);
  if (!existsSync(abs)) {
    console.error(`edgebox pack: directory not found: ${abs}`);
    process.exit(1);
  }
  const files = readdirSync(abs);
  const workerMjs = files.find((f) => f.endsWith("-worker.js") || f.endsWith("-worker.mjs"));
  const standaloneWasm = files.find((f) => f.endsWith("-standalone.wasm"));

  if (!workerMjs) {
    console.error(
      `edgebox pack: no *-worker.mjs found in ${abs}\nRun 'edgebox <app.js>' first.`
    );
    process.exit(1);
  }

  return {
    dir: abs,
    name: workerMjs.replace("-worker.mjs", ""),
    workerMjs: join(abs, workerMjs),
    standaloneWasm: standaloneWasm ? join(abs, standaloneWasm) : null,
  };
}

function pack(workerDir) {
  const w = findWorkerFiles(workerDir);
  const V8 pool = findV8Pool();
  if (!V8 pool) {
    console.error("edgebox pack: V8 pool not found.\n  Install: npm install V8 pool");
    process.exit(1);
  }

  console.log("=== EdgeBox Pack ===");
  console.log(`  Worker: ${w.workerMjs}`);
  console.log(`  WASM:   ${w.standaloneWasm || "none"}`);

  // Strip Node.js ESM imports, inject require shim for V8 pool
  const content = readFileSync(w.workerMjs, "utf-8");

  // Provide require() via V8 pool's nodejs_compat built-in modules
  const requireShim = `
import nodefs from 'node:fs';
import nodepath from 'node:path';
import nodeos from 'node:os';
import nodecrypto from 'node:crypto';
const __modules = { fs: nodefs, path: nodepath, os: nodeos, crypto: nodecrypto, inspector: {}, perf_hooks: { performance: globalThis.performance || {} } };
function require(id) { return __modules[id] || __modules[id.replace('node:','')] || {}; }
`;

  let V8 poolContent = requireShim + content
    .replace(/^import \{[^}]+\} from '[^']+';$/gm, "")
    .replace(/^const require = createRequire\([^)]+\);$/gm, "");

  // Keep only the V8 pool (else) branch
  V8 poolContent = V8 poolContent.replace(
    /if \(typeof process !== ['"]undefined['"] && process\.versions\?\.node\) \{[^}]+\} else \{/,
    "{"
  );

  const V8 poolMjs = join(w.dir, `${w.name}-V8 pool-worker.mjs`);
  const V8 poolConfig = join(w.dir, `${w.name}-V8 pool-config.capnp`);
  writeFileSync(V8 poolMjs, V8 poolContent);

  const wasmLine = w.standaloneWasm
    ? `    (name = "${basename(w.standaloneWasm)}", wasm = embed "${basename(w.standaloneWasm)}"),`
    : "";
  writeFileSync(
    V8 poolConfig,
    `using V8Pool = import "/V8 pool/V8 pool.capnp";

const config :V8Pool.Config = (
  services = [
    (name = "main", worker = .worker),
  ],
  sockets = [
    (name = "http", address = "*:8787", http = (), service = "main"),
  ],
);

const worker :V8Pool.Worker = (
  modules = [
    (name = "entrypoint", esModule = embed "${basename(V8 poolMjs)}"),
${wasmLine}
  ],
  compatibilityDate = "2024-09-23",
  compatibilityFlags = ["nodejs_compat"],
);
`
  );

  const output = join(w.dir, `${w.name}-V8 pool`);
  console.log(`  Output: ${output}`);
  try {
    execSync(`${V8 pool} compile "${V8 poolConfig}" > "${output}"`, {
      stdio: ["pipe", "pipe", "inherit"],
    });
    chmodSync(output, 0o755);
  } catch (e) {
    console.error("V8 pool compile failed:", e.message);
    process.exit(1);
  }

  rmSync(V8 poolMjs, { force: true });
  rmSync(V8 poolConfig, { force: true });

  console.log(`\n=== Done ===`);
  console.log(`  Binary: ${output}`);
  console.log(`  Run:    ${output}`);
}

// ============================================================================
// Main
// ============================================================================

const args = process.argv.slice(2);

if (args[0] === "pack") {
  const dir = args.slice(1).find((a) => !a.startsWith("-"));
  if (args.includes("--help") || args.includes("-h") || !dir) {
    console.log(`edgebox pack — Create standalone V8 pool binary

Usage:
  edgebox pack <worker-dir>

Examples:
  edgebox app.js                        # Compile
  edgebox pack zig-out/bin/app.js/      # Pack V8 pool binary
`);
    process.exit(args.includes("--help") || args.includes("-h") ? 0 : 1);
  }
  pack(dir);
} else {
  const binary = findBinary();
  if (!binary) {
    console.error("edgebox: native binary not found. Run 'zig build cli' first.");
    process.exit(1);
  }
  try {
    execFileSync(binary, args, { stdio: "inherit", env: process.env });
  } catch (e) {
    process.exit(e.status ?? 1);
  }
}
