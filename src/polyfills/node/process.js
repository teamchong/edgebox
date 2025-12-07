// shim for using process in browser
var queue = [];
var draining = false;
var currentQueue;
var queueIndex = -1;

function cleanUpNextTick() {
  if (!draining || !currentQueue) {
    return;
  }
  draining = false;
  if (currentQueue.length) {
    queue = currentQueue.concat(queue);
  } else {
    queueIndex = -1;
  }
  if (queue.length) {
    drainQueue();
  }
}

function drainQueue() {
  if (draining) {
    return;
  }
  var timeout = setTimeout(cleanUpNextTick, 0);
  draining = true;
  var len = queue.length;
  while (len) {
    currentQueue = queue;
    queue = [];
    while (++queueIndex < len) {
      if (currentQueue) {
        var item = currentQueue[queueIndex];
        item.fun.apply(null, item.array);
      }
    }
    queueIndex = -1;
    len = queue.length;
  }
  currentQueue = null;
  draining = false;
  clearTimeout(timeout, 0);
}

function nextTick(fun) {
  var args = new Array(arguments.length - 1);
  if (arguments.length > 1) {
    for (var i = 1; i < arguments.length; i++) {
      args[i - 1] = arguments[i];
    }
  }
  queue.push({ fun, args });
  if (queue.length === 1 && !draining) {
    setTimeout(drainQueue, 0);
  }
}

const title = "browser";
const browser = true;
const env = {};
const argv = [];
const version = ""; // empty string to avoid regexp issues
const versions = {};

function noop() {}

const on = noop;
const addListener = noop;
const once = noop;
const off = noop;
const removeListener = noop;
const removeAllListeners = noop;
const emit = noop;
const prependListener = noop;
const prependOnceListener = noop;

const listeners = function (name) {
  return [];
};

const binding = function (name) {
  throw new Error("process.binding is not supported in browser polyfill");
};

const cwd = function () {
  return "/";
};

const chdir = function (dir) {
  throw new Error("process.chdir is not supported in browser polyfill");
};

const umask = function () {
  return 0;
};

const hrtime = function (previousTimestamp) {
  const baseNow = Math.floor((Date.now() - performance.now()) * 1e-3);
  const clocktime = performance.now() * 1e-3;
  let seconds = Math.floor(clocktime) + baseNow;
  let nanoseconds = Math.floor((clocktime % 1) * 1e9);
  if (previousTimestamp) {
    seconds = seconds - previousTimestamp[0];
    nanoseconds = nanoseconds - previousTimestamp[1];
    if (nanoseconds < 0) {
      seconds--;
      nanoseconds += 1e9;
    }
  }
  return [seconds, nanoseconds];
};

hrtime.bigint = function () {
  const [seconds, nanoseconds] = hrtime();
  return BigInt(seconds) * BigInt(1e9) + BigInt(nanoseconds);
};

const exit = function (code) {
  throw new Error('process.exit(' + (code || 0) + ') called');
};

const kill = function (pid, signal) {
  throw new Error('process.kill is not supported');
};

const pid = 1;
const ppid = 0;

const memoryUsage = function () {
  return {
    rss: 0,
    heapTotal: 0,
    heapUsed: 0,
    external: 0,
    arrayBuffers: 0
  };
};

const uptime = function () {
  return performance.now() / 1000;
};

const features = {
  inspector: false,
  debug: false,
  uv: false,
  ipv6: true,
  tls_alpn: false,
  tls_sni: false,
  tls_ocsp: false,
  tls: false
};

const stderr = {
  write: function (data) { console.error(data); },
  fd: 2,
  isTTY: false
};

const stdout = {
  write: function (data) { console.log(data); },
  fd: 1,
  isTTY: false
};

const stdin = {
  fd: 0,
  isTTY: false,
  read: function () { return null; }
};

const connected = false;
const allowedNodeEnvironmentFlags = new Set();

module.exports = {
  nextTick,
  title,
  browser,
  env,
  argv,
  version,
  versions,
  on,
  addListener,
  once,
  off,
  removeListener,
  removeAllListeners,
  emit,
  prependListener,
  prependOnceListener,
  listeners,
  binding,
  cwd,
  chdir,
  umask,
  hrtime,
  exit,
  kill,
  pid,
  ppid,
  memoryUsage,
  uptime,
  features,
  stderr,
  stdout,
  stdin,
  connected,
  allowedNodeEnvironmentFlags
};
