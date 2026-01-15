// Comprehensive fs tracing - focus on openSync, writeSync, closeSync
const _fs = require('fs');

const originalOpenSync = _fs.openSync;
_fs.openSync = function(path, flags, mode) {
    console.log('[TRACE fs.openSync]', path, flags);
    return originalOpenSync.call(this, path, flags, mode);
};

const originalWriteSync = _fs.writeSync;
_fs.writeSync = function(fd, data, ...rest) {
    console.log('[TRACE fs.writeSync] fd=' + fd, 'data len=' + (data ? data.length : 0));
    return originalWriteSync.call(this, fd, data, ...rest);
};

const originalCloseSync = _fs.closeSync;
_fs.closeSync = function(fd) {
    console.log('[TRACE fs.closeSync] fd=' + fd);
    return originalCloseSync.call(this, fd);
};

console.log('[TRACE] Starting tsc...');
require('typescript/lib/tsc.js');
