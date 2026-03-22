// EdgeBox Bootstrap — sets up Node.js globals for workerd
// ALL polyfills use direct Zig typed methods (zero JSON)

// ── process ──
globalThis.process = {
  argv: ['edgebox'],
  env: {},
  platform: 'linux',
  arch: 'x64',
  version: 'v20.0.0',
  versions: { node: '20.0.0' },
  cwd: function() { return __edgebox_cwd(); },
  exit: function(code) { __edgebox_exit(code || 0); },
  stdout: { write: function(s) { __edgebox_write_stdout(String(s)); return true; }, isTTY: false, columns: 80 },
  stderr: { write: function(s) { __edgebox_write_stderr(String(s)); return true; }, isTTY: false },
  nextTick: function(cb) { queueMicrotask(cb); },
  hrtime: Object.assign(function() { var t = Date.now(); return [Math.floor(t/1000), (t%1000)*1e6]; }, {
    bigint: function() { return BigInt(Date.now()) * 1000000n; }
  }),
  memoryUsage: function() { return { rss: 0, heapTotal: 0, heapUsed: 0, external: 0 }; },
  cpuUsage: function() { return { user: 0, system: 0 }; },
  uptime: function() { return 0; },
  on: function() { return process; },
  once: function() { return process; },
  removeListener: function() { return process; },
  emit: function() { return false; },
  binding: function() { return {}; },
};

// ── Buffer ──
if (typeof Buffer === 'undefined') {
  globalThis.Buffer = {
    from: function(s, enc) {
      if (typeof s === 'string') {
        var arr = new Uint8Array(s.length);
        for (var i = 0; i < s.length; i++) arr[i] = s.charCodeAt(i);
        return arr;
      }
      return new Uint8Array(s);
    },
    isBuffer: function(obj) { return obj instanceof Uint8Array; },
    alloc: function(n) { return new Uint8Array(n); },
    concat: function(bufs) {
      var total = 0;
      for (var i = 0; i < bufs.length; i++) total += bufs[i].length;
      var result = new Uint8Array(total);
      var offset = 0;
      for (var j = 0; j < bufs.length; j++) { result.set(bufs[j], offset); offset += bufs[j].length; }
      return result;
    },
    byteLength: function(s) { return typeof s === 'string' ? s.length : s.byteLength || 0; },
    isEncoding: function() { return true; },
  };
}

// ── module/require shims ──
if (typeof module === 'undefined') globalThis.module = { exports: {} };
if (typeof __filename === 'undefined') { globalThis.__filename = '/edgebox/worker.js'; globalThis.__dirname = '/edgebox'; }

// ── fs (zero JSON — all via Zig typed methods) ──
var _fs = {
  readFileSync: function(p, enc) {
    var content = __edgebox_read_file(String(p));
    if (!content && content !== '') return null;
    return content;
  },
  writeFileSync: function(p, data) { __edgebox_write_file(String(p), String(data)); },
  existsSync: function(p) { return __edgebox_file_exists(String(p)) === 1 || __edgebox_dir_exists(String(p)) === 1; },
  statSync: function(p) {
    var json = __edgebox_stat(String(p));
    if (!json) { var e = new Error('ENOENT: ' + p); e.code = 'ENOENT'; throw e; }
    var s = JSON.parse(json);
    return { isFile: function(){return s.isFile;}, isDirectory: function(){return s.isDirectory;}, isSymbolicLink: function(){return false;}, size: s.size, mtime: new Date(), dev: 0, ino: 0 };
  },
  lstatSync: function(p) { return _fs.statSync(p); },
  readdirSync: function(p, opts) {
    var json = __edgebox_readdir(String(p));
    var entries = JSON.parse(json);
    if (opts && opts.withFileTypes) {
      return entries.map(function(name) {
        var full = String(p) + '/' + name;
        var isF = __edgebox_file_exists(full) === 1;
        var isD = __edgebox_dir_exists(full) === 1;
        return { name: name, isFile: function(){return isF;}, isDirectory: function(){return isD;}, isSymbolicLink: function(){return false;}, isBlockDevice: function(){return false;}, isCharacterDevice: function(){return false;}, isFIFO: function(){return false;}, isSocket: function(){return false;} };
      });
    }
    return entries;
  },
  realpathSync: Object.assign(function(p) { return __edgebox_realpath(String(p)); }, { native: function(p) { return __edgebox_realpath(String(p)); } }),
  watchFile: function() {}, unwatchFile: function() {},
  watch: function() { return { close: function(){} }; },
};

// ── path ──
var _path = {
  join: function() { return Array.prototype.slice.call(arguments).join('/').replace(/\/+/g,'/'); },
  dirname: function(p) { var i=String(p).lastIndexOf('/'); return i>=0?String(p).slice(0,i):'.'; },
  basename: function(p,e) { p=String(p); var b=p.slice(p.lastIndexOf('/')+1); if(e&&b.endsWith(e))b=b.slice(0,-e.length); return b; },
  resolve: function() { var a=Array.prototype.slice.call(arguments),r=''; for(var i=a.length-1;i>=0;i--){r=String(a[i])+(r?'/'+r:'');if(String(a[i]).charAt(0)==='/')break;} if(r.charAt(0)!=='/'){r='/'+r;} return r.replace(/\/+/g,'/'); },
  normalize: function(p) { return String(p).replace(/\/+/g,'/'); },
  relative: function(f,t) { return String(t); },
  isAbsolute: function(p) { return String(p).charAt(0)==='/'; },
  extname: function(p) { p=String(p); var i=p.lastIndexOf('.'); return i>=0?p.slice(i):''; },
  sep: '/', delimiter: ':',
};
_path.posix = _path;

// ── os ──
var _os = {
  EOL: '\n', platform: function(){return'linux';}, tmpdir: function(){return'/tmp';}, homedir: function(){return'/tmp';},
  cpus: function(){return[{model:'edgebox',speed:3000,times:{user:0,nice:0,sys:0,idle:0,irq:0}}];},
  arch: function(){return'x64';}, type: function(){return'Linux';}, release: function(){return'6.0.0';},
  networkInterfaces: function(){return{};}, hostname: function(){return'edgebox';},
  totalmem: function(){return 8*1024*1024*1024;}, freemem: function(){return 4*1024*1024*1024;},
  endianness: function(){return'LE';}, userInfo: function(){return{username:'edgebox',uid:1000,gid:1000,shell:'/bin/sh',homedir:'/tmp'};},
};

// ── crypto ──
var _crypto = {
  createHash: function(algo) {
    var _data = '';
    return {
      update: function(d) { _data += String(d); return this; },
      digest: function(enc) { return __edgebox_hash(algo, _data); },
    };
  },
  randomBytes: function(size) {
    var hex = __edgebox_random_bytes(size);
    return Buffer.from(hex, 'hex');
  },
  randomUUID: function() {
    var hex = __edgebox_random_bytes(16);
    return hex.slice(0,8)+'-'+hex.slice(8,12)+'-4'+hex.slice(13,16)+'-'+((parseInt(hex[16],16)&3|8).toString(16))+hex.slice(17,20)+'-'+hex.slice(20,32);
  },
};

// ── perf_hooks ──
var _perf_hooks = { performance: { now: function(){ return Date.now(); }, mark: function(){}, measure: function(){} } };

// ── events ──
var _events = function() { this._e = {}; };
_events.prototype.on = function(n,f) { (this._e[n]=this._e[n]||[]).push(f); return this; };
_events.prototype.once = function(n,f) { var self=this; function w(){self.removeListener(n,w);f.apply(this,arguments);} this.on(n,w); return this; };
_events.prototype.emit = function(n) { var a=[].slice.call(arguments,1); (this._e[n]||[]).forEach(function(f){f.apply(null,a);}); return true; };
_events.prototype.removeListener = function(n,f) { var l=this._e[n]; if(l){var i=l.indexOf(f);if(i>=0)l.splice(i,1);} return this; };
_events.prototype.removeAllListeners = function(n) { if(n)delete this._e[n]; else this._e={}; return this; };
_events.prototype.listeners = function(n) { return (this._e[n]||[]).slice(); };
_events.prototype.listenerCount = function(n) { return (this._e[n]||[]).length; };
_events.EventEmitter = _events;
_events.default = _events;

// ── require ──
globalThis.require = function(name) {
  name = String(name).replace(/^node:/, '');
  if (name === 'fs') return _fs;
  if (name === 'path') return _path;
  if (name === 'os') return _os;
  if (name === 'crypto') return _crypto;
  if (name === 'perf_hooks') return _perf_hooks;
  if (name === 'events') return _events;
  if (name === 'util') return { inherits: function(c,s){c.prototype=Object.create(s.prototype);c.prototype.constructor=c;}, deprecate: function(f){return f;}, inspect: function(o){return JSON.stringify(o);}, types: { isDate: function(v){return v instanceof Date;} } };
  if (name === 'stream') return { Readable: _events, Writable: _events, Transform: _events, PassThrough: _events, Duplex: _events, pipeline: function(){}, finished: function(){} };
  if (name === 'assert') return function(v,m){if(!v)throw new Error(m||'Assertion failed');};
  if (name === 'url') return { parse: function(u){try{var o=new URL(u);return{href:o.href,protocol:o.protocol,host:o.host,hostname:o.hostname,port:o.port,pathname:o.pathname,search:o.search,hash:o.hash};}catch(e){return{href:u};}}, format: function(o){return o.href||'';}, resolve: function(f,t){return new URL(t,f).href;}, URL: URL };
  if (name === 'querystring') return { parse: function(s){var r={};(s||'').split('&').forEach(function(p){var kv=p.split('=');if(kv[0])r[decodeURIComponent(kv[0])]=decodeURIComponent(kv[1]||'');});return r;}, stringify: function(o){return Object.keys(o).map(function(k){return encodeURIComponent(k)+'='+encodeURIComponent(o[k]);}).join('&');} };
  if (name === 'buffer') return { Buffer: Buffer };
  if (name === 'string_decoder') return { StringDecoder: function(){this.write=function(b){return typeof b==='string'?b:String.fromCharCode.apply(null,b);};this.end=function(){return'';};} };
  if (name === 'child_process' || name === 'net' || name === 'tls' || name === 'http' || name === 'https' || name === 'http2' || name === 'dgram' || name === 'dns' || name === 'cluster' || name === 'readline' || name === 'tty' || name === 'zlib' || name === 'vm' || name === 'timers' || name === 'worker_threads' || name === 'inspector') return {};
  return {};
};

// ── Tell TSC where lib.d.ts files live ──
globalThis.__edgebox_ts_lib = __edgebox_cwd() + '/node_modules/typescript/lib';
