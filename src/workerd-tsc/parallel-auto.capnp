using Workerd = import "/workerd/workerd.capnp";
const config :Workerd.Config = (
  services = [
    (name = "checker-0", worker = .checkerWorker),
    (name = "checker-1", worker = .checkerWorker),
    (name = "checker-2", worker = .checkerWorker),
    (name = "checker-3", worker = .checkerWorker),
  ],
  sockets = [
    (name = "s0", address = "127.0.0.1:9900", http = (), service = "checker-0"),
    (name = "s1", address = "127.0.0.1:9901", http = (), service = "checker-1"),
    (name = "s2", address = "127.0.0.1:9902", http = (), service = "checker-2"),
    (name = "s3", address = "127.0.0.1:9903", http = (), service = "checker-3"),
  ],
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
