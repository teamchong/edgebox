var path = module.exports = {
  join: function() { return Array.prototype.slice.call(arguments).join('/').replace(/\/+/g,'/'); },
  dirname: function(p) { var i=String(p).lastIndexOf('/'); return i>=0?String(p).slice(0,i):'.'; },
  basename: function(p,e) { p=String(p); var b=p.slice(p.lastIndexOf('/')+1); if(e&&b.endsWith(e))b=b.slice(0,-e.length); return b; },
  resolve: function() { var a=Array.prototype.slice.call(arguments),r=''; for(var i=a.length-1;i>=0;i--){r=String(a[i])+(r?'/'+r:'');if(String(a[i]).charAt(0)==='/')break;} if(r.charAt(0)!=='/'){r='/'+r;} return r.replace(/\/+/g,'/'); },
  normalize: function(p) { return String(p).replace(/\/+/g,'/'); },
  relative: function(f,t) { return String(t); },
  isAbsolute: function(p) { return String(p).charAt(0)==='/'; },
  extname: function(p) { p=String(p); var i=p.lastIndexOf('.'); return i>=0?p.slice(i):''; },
  sep: '/', delimiter: ':', posix: null,
};
path.posix = path;
