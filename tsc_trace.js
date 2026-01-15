// Override fs.writeFileSync to trace calls
const _fs = require('fs');
const _originalWriteFileSync = _fs.writeFileSync;
_fs.writeFileSync = function(path, data, opts) {
    console.log('[TRACE] writeFileSync called:', path, 'data length:', data ? data.length : 0);
    return _originalWriteFileSync.call(this, path, data, opts);
};

// Also trace writeFile (async)
const _originalWriteFile = _fs.writeFile;
_fs.writeFile = function(path, data, opts, cb) {
    console.log('[TRACE] writeFile called:', path, 'data length:', data ? data.length : 0);
    return _originalWriteFile.call(this, path, data, opts, cb);
};

// Trace createWriteStream
const _originalCreateWriteStream = _fs.createWriteStream;
_fs.createWriteStream = function(path, opts) {
    console.log('[TRACE] createWriteStream called:', path);
    return _originalCreateWriteStream.call(this, path, opts);
};

// Now load tsc
require('typescript/lib/tsc.js');
