const endianness = function () {
  return "LE";
};

const hostname = function () {
  if (typeof location !== "undefined") {
    return location.hostname;
  } else return "";
};

const loadavg = function () {
  return [];
};

const uptime = function () {
  return 0;
};

const freemem = function () {
  return Number.MAX_VALUE;
};

const totalmem = function () {
  return Number.MAX_VALUE;
};

const cpus = function () {
  return [];
};

const type = function () {
  return "Browser";
};

const release = function () {
  if (typeof navigator !== "undefined") {
    return navigator.appVersion;
  }
  return "";
};

const getNetworkInterfaces = function () {
  return {};
};
const networkInterfaces = getNetworkInterfaces;

const arch = function () {
  return "javascript";
};

const platform = function () {
  return "browser";
};

const tmpdir = function () {
  return "/tmp";
};
const tmpDir = tmpdir;

const EOL = "\n";

const homedir = function () {
  return "/";
};

const userInfo = function () {
  return {
    uid: -1,
    gid: -1,
    username: 'user',
    homedir: '/',
    shell: '/bin/sh'
  };
};

const constants = {
  signals: {
    SIGHUP: 1, SIGINT: 2, SIGQUIT: 3, SIGILL: 4, SIGTRAP: 5, SIGABRT: 6,
    SIGIOT: 6, SIGBUS: 7, SIGFPE: 8, SIGKILL: 9, SIGUSR1: 10, SIGSEGV: 11,
    SIGUSR2: 12, SIGPIPE: 13, SIGALRM: 14, SIGTERM: 15, SIGSTKFLT: 16,
    SIGCHLD: 17, SIGCONT: 18, SIGSTOP: 19, SIGTSTP: 20, SIGTTIN: 21,
    SIGTTOU: 22, SIGURG: 23, SIGXCPU: 24, SIGXFSZ: 25, SIGVTALRM: 26,
    SIGPROF: 27, SIGWINCH: 28, SIGIO: 29, SIGPOLL: 29, SIGPWR: 30, SIGSYS: 31
  },
  errno: {},
  priority: {
    PRIORITY_LOW: 19,
    PRIORITY_BELOW_NORMAL: 10,
    PRIORITY_NORMAL: 0,
    PRIORITY_ABOVE_NORMAL: -7,
    PRIORITY_HIGH: -14,
    PRIORITY_HIGHEST: -20
  }
};

module.exports = {
  endianness,
  hostname,
  loadavg,
  uptime,
  freemem,
  totalmem,
  cpus,
  type,
  release,
  getNetworkInterfaces,
  networkInterfaces,
  arch,
  platform,
  tmpdir,
  tmpDir,
  EOL,
  homedir,
  userInfo,
  constants
};
