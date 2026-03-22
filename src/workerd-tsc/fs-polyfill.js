function io(op, params) {
  return JSON.parse(__edgebox_io_sync(JSON.stringify(Object.assign({op}, params || {}))));
}
module.exports = {
  readFileSync: function(p, enc) {
    p = String(p);
    // Resolve lib.d.ts paths — TSC looks for them relative to getExecutingFilePath
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
    return r.ok && r.data;
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
  readdirSync: function(p, o) { var r = io('readdir', {path: String(p)}); if(!r.ok)return[]; if(o&&o.withFileTypes){return r.data.map(function(n){return{name:n,isFile:function(){return true;},isDirectory:function(){return false;},isSymbolicLink:function(){return false;}};});} return r.data; },
  realpathSync: Object.assign(function(p){return String(p);}, {native: function(p){return String(p);}}),
  watchFile: function() {},
  unwatchFile: function() {},
  watch: function() { return{close:function(){}}; },
};
