using Workerd = import "/workerd/workerd.capnp";

# EdgeBox Parallel TSC — workerd configuration
#
# Architecture:
#   main-worker: loads TSC, runs createProgram, dispatches file shards to checker workers
#   checker-N: receives file indices via RPC, checks only assigned files, returns diagnostics
#
# All workers share Zig IO (readFile/stat/readdir via __edgebox_io_sync global).
# File cache is shared across all workers (Zig mmap, zero-copy reads after first access).

const config :Workerd.Config = (
  services = [
    (name = "main", worker = .mainWorker),
    (name = "checker-0", worker = .checkerWorker),
    (name = "checker-1", worker = .checkerWorker),
    (name = "checker-2", worker = .checkerWorker),
    (name = "checker-3", worker = .checkerWorker),
  ],
  sockets = [
    # CLI mode: bind to random port (never used, workerd requires at least one socket)
    (name = "http", address = "127.0.0.1:0", http = (), service = "main"),
  ],
);

const mainWorker :Workerd.Worker = (
  modules = [
    (name = "entrypoint", esModule = embed "main-worker.js"),
    (name = "./tsc-loader.js", commonJsModule = embed "tsc-loader.js"),
  ],
  bindings = [
    # Service bindings for RPC to checker workers (zero-copy structured clone)
    (name = "CHECKER_0", service = "checker-0"),
    (name = "CHECKER_1", service = "checker-1"),
    (name = "CHECKER_2", service = "checker-2"),
    (name = "CHECKER_3", service = "checker-3"),
  ],
  compatibilityDate = "2024-09-23",
  compatibilityFlags = ["nodejs_compat"],
);

const checkerWorker :Workerd.Worker = (
  modules = [
    (name = "entrypoint", esModule = embed "checker-worker.js"),
    (name = "./tsc-loader.js", commonJsModule = embed "tsc-loader.js"),
  ],
  compatibilityDate = "2024-09-23",
  compatibilityFlags = ["nodejs_compat"],
);
