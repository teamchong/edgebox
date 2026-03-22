using Workerd = import "/workerd/workerd.capnp";
const config :Workerd.Config = (
  services = [
    (name = "main", worker = .mainWorker),
    (name = "checker-0", worker = .checkerWorker),
    (name = "checker-1", worker = .checkerWorker),
    (name = "checker-2", worker = .checkerWorker),
    (name = "checker-3", worker = .checkerWorker),
  ],
  sockets = [(name = "http", address = "127.0.0.1:9999", http = (), service = "main")],
);

const mainWorker :Workerd.Worker = (
  modules = [
    (name = "entrypoint", esModule = embed "main-parallel.js"),
    (name = "./bootstrap.js", commonJsModule = embed "bootstrap.js"),
    (name = "fs", commonJsModule = embed "fs.js"),
    (name = "path", commonJsModule = embed "path.js"),
    (name = "os", commonJsModule = embed "os.js"),
    (name = "./typescript.js", commonJsModule = embed "typescript.js"),
  ],
  bindings = [
    (name = "CHECKER_0", service = "checker-0"),
    (name = "CHECKER_1", service = "checker-1"),
    (name = "CHECKER_2", service = "checker-2"),
    (name = "CHECKER_3", service = "checker-3"),
  ],
  compatibilityDate = "2024-09-23",
);

const checkerWorker :Workerd.Worker = (
  modules = [
    (name = "entrypoint", esModule = embed "checker-parallel.js"),
    (name = "./bootstrap.js", commonJsModule = embed "bootstrap.js"),
    (name = "fs", commonJsModule = embed "fs.js"),
    (name = "path", commonJsModule = embed "path.js"),
    (name = "os", commonJsModule = embed "os.js"),
    (name = "./typescript.js", commonJsModule = embed "typescript.js"),
  ],
  compatibilityDate = "2024-09-23",
);
