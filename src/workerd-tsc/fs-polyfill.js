function io(op, params) {
  return JSON.parse(__edgebox_io_sync(JSON.stringify(Object.assign({op}, params || {}))));
}
function isFile(p) { var r = io('stat', {path: p}); return r.ok && r.data.isFile; }
function isDir(p) { var r = io('stat', {path: p}); return r.ok && r.data.isDirectory; }

module.exports = {
  readFileSync: function(p, enc) {
    p = String(p);
    if (p.startsWith('/lib.') && p.endsWith('.d.ts') && globalThis.__edgebox_ts_lib) {
      p = globalThis.__edgebox_ts_lib + p;
    }
    var r = io('readFile', {path: p});
    return r.ok ? r.data : null;
  },
  writeFileSync: function() {},
  existsSync: function(p) {
    p = String(p);
    if (p.startsWith('/lib.') && p.endsWith('.d.ts') && globalThis.__edgebox_ts_lib) {
      p = globalThis.__edgebox_ts_lib + p;
    }
    var r = io('fileExists', {path: p});
    if (r.ok && r.data) return true;
    var r2 = io('dirExists', {path: p});
    return r2.ok && r2.data;
  },
  statSync: function(p) {
    p = String(p);
    if (p.startsWith('/lib.') && p.endsWith('.d.ts') && globalThis.__edgebox_ts_lib) {
      p = globalThis.__edgebox_ts_lib + p;
    }
    var r = io('stat', {path: p});
    if(!r.ok){var e=new Error('ENOENT: '+p);e.code='ENOENT';throw e;}
    return{isFile:function(){return r.data.isFile;},isDirectory:function(){return r.data.isDirectory;},isSymbolicLink:function(){return false;},size:r.data.size,mtime:new Date(),dev:0,ino:0};
  },
  lstatSync: function(p) { return module.exports.statSync(p); },
  readdirSync: function(p, opts) {
    p = String(p);
    var r = io('readdir', {path: p});
    if (!r.ok) return [];
    if (opts && opts.withFileTypes) {
      return r.data.map(function(name) {
        var full = p + '/' + name;
        var _isFile = isFile(full);
        var _isDir = isDir(full);
        return {
          name: name,
          isFile: function() { return _isFile; },
          isDirectory: function() { return _isDir; },
          isSymbolicLink: function() { return false; },
          isBlockDevice: function() { return false; },
          isCharacterDevice: function() { return false; },
          isFIFO: function() { return false; },
          isSocket: function() { return false; },
        };
      });
    }
    return r.data;
  },
  realpathSync: Object.assign(function(p){return String(p);}, {native: function(p){return String(p);}}),
  watchFile: function() {},
  unwatchFile: function() {},
  watch: function() { return{close:function(){}}; },
};
