    // ===== FS MODULE =====
    // fs module uses either native __edgebox_fs_* functions or std/os fallback
    // Checks happen at call time, not at load time
    let _fileReadCount = 0;
    _modules.fs = {
        readFileSync: function(path, options) {
            path = _remapPath(path); // Mount remapping
            _fileReadCount++;
            if (_fileReadCount <= 30) _log('[FS] readFileSync #' + _fileReadCount + ': ' + path);
            const encoding = typeof options === 'string' ? options : (options && options.encoding);
            if (typeof globalThis.__edgebox_fs_read === 'function') {
                const data = globalThis.__edgebox_fs_read(path);
                const result = (encoding === 'utf8' || encoding === 'utf-8') ? data : Buffer.from(data);
                if (_fileReadCount <= 30) {
                    _log('[FS] readFileSync #' + _fileReadCount + ' result: type=' + (typeof result) + ' len=' + (result ? result.length : 0) + ' isBuffer=' + Buffer.isBuffer(result));
                }
                return result;
            } else if (typeof std !== 'undefined' && std.loadFile) {
                const data = std.loadFile(path);
                if (data === null) { const err = new Error('ENOENT: ' + path); err.code = 'ENOENT'; throw err; }
                return (encoding === 'utf8' || encoding === 'utf-8') ? data : Buffer.from(data);
            }
            throw new Error('fs.readFileSync not implemented - __edgebox_fs_read=' + typeof globalThis.__edgebox_fs_read);
        },
        writeFileSync: function(pathOrFd, data, options) {
            if (typeof pathOrFd === 'string') pathOrFd = _remapPath(pathOrFd); // Mount remapping
            const strBuf = typeof data === 'string' ? data : String(data);
            // Handle file descriptor (number) vs path (string)
            if (typeof pathOrFd === 'number') {
                const fd = pathOrFd;
                const _osModule = globalThis._os || (typeof os !== 'undefined' ? os : null);
                // Real fd (< 100) - try os.write
                if (_osModule && typeof _osModule.write === 'function' && fd < 100) {
                    // os.write expects ArrayBuffer, convert string
                    const encoder = new TextEncoder();
                    const arrayBuf = encoder.encode(strBuf).buffer;
                    const written = _osModule.write(fd, arrayBuf, 0, strBuf.length);
                    return written;
                }
                // Pseudo-fd (>= 100) - buffer the data and write on close
                // Or if path is tracked, write directly to the path
                const trackedPath = globalThis._fdPaths ? globalThis._fdPaths[fd] : null;
                if (trackedPath && typeof globalThis.__edgebox_fs_write === 'function') {
                    return globalThis.__edgebox_fs_write(trackedPath, strBuf);
                }
                return;
            }
            if (typeof globalThis.__edgebox_fs_write === 'function') return globalThis.__edgebox_fs_write(pathOrFd, strBuf);
            if (typeof std !== 'undefined' && std.open) {
                const f = std.open(path, 'w'); if (!f) throw new Error('ENOENT: ' + path);
                f.puts(String(data)); f.close();
                return;
            }
            throw new Error('fs.writeFileSync not implemented');
        },
        appendFileSync: function(path, data, options) {
            if (typeof path === 'string') path = _remapPath(path); // Mount remapping
            const content = typeof data === 'string' ? data : String(data);

            // Handle file descriptor (number) - Node.js supports both path and fd
            if (typeof path === 'number') {
                return this.writeSync(path, content);
            }

            // Use native append if available (more efficient)
            if (typeof globalThis.__edgebox_fs_append === 'function') {
                return globalThis.__edgebox_fs_append(path, content);
            }
            // Fallback: Read existing content, append new data, write back
            let existing = '';
            try {
                existing = this.readFileSync(path, { encoding: 'utf8' });
            } catch(e) {
                // File doesn't exist, start fresh
            }
            const newContent = existing + content;
            return this.writeFileSync(path, newContent, options);
        },
        appendFile: function(path, data, options) {
            return Promise.resolve(this.appendFileSync(path, data, options));
        },
        existsSync: function(path) {
            path = _remapPath(path); // Mount remapping
            if (typeof globalThis.__edgebox_fs_exists === 'function') return globalThis.__edgebox_fs_exists(path);
            if (typeof _os !== 'undefined' && _os.stat) { try { return _os.stat(path)[1] === 0; } catch(e) { return false; } }
            return false;
        },
        mkdirSync: function(path, options) {
            path = _remapPath(path); // Mount remapping
            const recursive = (options && options.recursive) || false;
            if (typeof globalThis.__edgebox_fs_mkdir === 'function') return globalThis.__edgebox_fs_mkdir(path, recursive);
            if (typeof _os !== 'undefined' && _os.mkdir) { try { _os.mkdir(path); } catch(e) { if (!recursive) throw e; } return; }
            throw new Error('fs.mkdirSync not implemented');
        },
        readdirSync: function(path, options) {
            path = _remapPath(path); // Mount remapping
            _log('[FS] readdirSync: ' + path + ' withFileTypes=' + (options && options.withFileTypes));
            let entries;
            if (typeof globalThis.__edgebox_fs_readdir === 'function') {
                entries = globalThis.__edgebox_fs_readdir(path);
            } else if (typeof _os !== 'undefined' && _os.readdir) {
                const r = _os.readdir(path);
                if (r[1] !== 0) { const err = new Error('ENOENT: ' + path); err.code = 'ENOENT'; throw err; }
                entries = r[0].filter(x => x !== '.' && x !== '..');
            } else {
                throw new Error('fs.readdirSync not implemented');
            }
            if (options && options.withFileTypes) {
                return entries.map(name => ({
                    name,
                    isFile: () => this.statSync(path + '/' + name).isFile(),
                    isDirectory: () => this.statSync(path + '/' + name).isDirectory(),
                    isSymbolicLink: () => false
                }));
            }
            return entries;
        },
        statSync: function(path, options) {
            path = _remapPath(path); // Mount remapping
            const throwIfNoEntry = options && options.throwIfNoEntry === false ? false : true;
            _log('[FS] statSync: ' + path + ' throwIfNoEntry=' + throwIfNoEntry);
            try {
                if (typeof globalThis.__edgebox_fs_stat === 'function') {
                    const result = globalThis.__edgebox_fs_stat(path);
                    _log('[FS] statSync result: isFile=' + (result && result.isFile ? result.isFile() : 'undefined'));
                    return result;
                }
                if (typeof _os !== 'undefined' && _os.stat) {
                    const r = _os.stat(path);
                    if (r[1] !== 0) {
                        if (!throwIfNoEntry) return undefined;
                        const err = new Error('ENOENT: ' + path); err.code = 'ENOENT'; throw err;
                    }
                    const s = r[0];
                    const mtimeMs = s.mtime * 1000;
                    const atimeMs = s.atime ? s.atime * 1000 : mtimeMs;
                    const ctimeMs = s.ctime ? s.ctime * 1000 : mtimeMs;
                    return {
                        isFile: () => (s.mode & 0o170000) === 0o100000,
                        isDirectory: () => (s.mode & 0o170000) === 0o040000,
                        isSymbolicLink: () => (s.mode & 0o170000) === 0o120000,
                        size: s.size, mode: s.mode,
                        mtime: new Date(mtimeMs), mtimeMs: mtimeMs,
                        atime: new Date(atimeMs), atimeMs: atimeMs,
                        ctime: new Date(ctimeMs), ctimeMs: ctimeMs
                    };
                }
                throw new Error('fs.statSync not implemented');
            } catch (e) {
                if (!throwIfNoEntry && e.code === 'ENOENT') return undefined;
                throw e;
            }
        },
        lstatSync: function(path, options) { return this.statSync(path, options); }, // statSync already does remap
        unlinkSync: function(path) {
            path = _remapPath(path); // Mount remapping
            if (typeof globalThis.__edgebox_fs_unlink === 'function') return globalThis.__edgebox_fs_unlink(path);
            if (typeof _os !== 'undefined' && _os.remove) { try { _os.remove(path); } catch(e) { const err = new Error('ENOENT: ' + path); err.code = 'ENOENT'; throw err; } return; }
            throw new Error('fs.unlinkSync not implemented');
        },
        rmdirSync: function(path, options) {
            path = _remapPath(path); // Mount remapping
            const recursive = (options && options.recursive) || false;
            if (typeof globalThis.__edgebox_fs_rmdir === 'function') return globalThis.__edgebox_fs_rmdir(path, recursive);
            if (typeof _os !== 'undefined' && _os.remove) { try { _os.remove(path); } catch(e) { throw new Error('ENOENT: ' + path); } return; }
            throw new Error('fs.rmdirSync not implemented');
        },
        rmSync: function(path, options) { return this.rmdirSync(path, options); }, // rmdirSync already does remap
        renameSync: function(oldPath, newPath) {
            oldPath = _remapPath(oldPath); // Mount remapping
            newPath = _remapPath(newPath);
            if (typeof globalThis.__edgebox_fs_rename === 'function') return globalThis.__edgebox_fs_rename(oldPath, newPath);
            if (typeof _os !== 'undefined' && _os.rename) { try { _os.rename(oldPath, newPath); } catch(e) { throw new Error('ENOENT: ' + oldPath); } return; }
            throw new Error('fs.renameSync not implemented');
        },
        copyFileSync: function(src, dest) {
            src = _remapPath(src); // Mount remapping
            dest = _remapPath(dest);
            if (typeof globalThis.__edgebox_fs_copy === 'function') return globalThis.__edgebox_fs_copy(src, dest);
            // Fallback: read and write (readFileSync/writeFileSync already remap, but we did it above too - harmless)
            this.writeFileSync(dest, this.readFileSync(src));
        },
        // realpathSync - returns the path as-is (no symlink resolution in WASI)
        realpathSync: Object.assign(function(path) { return path; }, { native: function(path) { return path; } }),
        realpath: Object.assign(function(path, opts, cb) {
            if (typeof opts === 'function') { cb = opts; opts = {}; }
            if (cb) cb(null, path);
            return Promise.resolve(path);
        }, { native: function(path, opts, cb) { if (typeof opts === 'function') { cb = opts; } if (cb) cb(null, path); } }),
        // accessSync - check if path exists, throw if not
        accessSync: function(path, mode) {
            path = _remapPath(path); // Mount remapping
            if (typeof globalThis.__edgebox_fs_exists === 'function') {
                if (!globalThis.__edgebox_fs_exists(path)) {
                    const err = new Error('ENOENT: no such file or directory, access \'' + path + '\'');
                    err.code = 'ENOENT';
                    throw err;
                }
            } else if (typeof _os !== 'undefined' && _os.stat) {
                try { if (_os.stat(path)[1] !== 0) throw new Error('ENOENT'); }
                catch(e) { const err = new Error('ENOENT: ' + path); err.code = 'ENOENT'; throw err; }
            }
        },
        // File descriptor operations using QuickJS _os module
        openSync: function(path, flags, mode) {
            path = _remapPath(path); // Mount remapping
            // Ensure parent directory exists for write flags
            if (flags && (flags === 'w' || flags === 'wx' || flags === 'xw' || flags === 'w+' || flags === 'a' || flags === 'a+')) {
                const lastSlash = path.lastIndexOf('/');
                if (lastSlash > 0) {
                    const parent = path.substring(0, lastSlash);
                    if (typeof globalThis.__edgebox_fs_mkdir === 'function') {
                        try { globalThis.__edgebox_fs_mkdir(parent, true); } catch(e) { /* ignore */ }
                    }
                }
            }
            // Try to get QuickJS _os module
            const _osModule = globalThis._os || (typeof os !== 'undefined' ? os : null);
            // Map Node.js flags to POSIX flags
            let osFlags = 0;
            if (_osModule && typeof _osModule.open === 'function') {
                if (flags === 'r' || flags === 'rs' || !flags) osFlags = _osModule.O_RDONLY;
                else if (flags === 'r+') osFlags = _osModule.O_RDWR;
                else if (flags === 'w') osFlags = _osModule.O_WRONLY | _osModule.O_CREAT | _osModule.O_TRUNC;
                else if (flags === 'wx' || flags === 'xw') osFlags = _osModule.O_WRONLY | _osModule.O_CREAT | _osModule.O_EXCL;
                else if (flags === 'w+') osFlags = _osModule.O_RDWR | _osModule.O_CREAT | _osModule.O_TRUNC;
                else if (flags === 'a') osFlags = _osModule.O_WRONLY | _osModule.O_CREAT | _osModule.O_APPEND;
                else if (flags === 'a+') osFlags = _osModule.O_RDWR | _osModule.O_CREAT | _osModule.O_APPEND;
                else osFlags = _osModule.O_RDWR | _osModule.O_CREAT;

                const fd = _osModule.open(path, osFlags, mode || 0o666);
                if (fd < 0) {
                    const err = new Error('ENOENT: no such file or directory, open \'' + path + '\'');
                    err.code = 'ENOENT';
                    throw err;
                }
                // Store path for fstatSync lookup
                if (!globalThis._fdPaths) globalThis._fdPaths = {};
                globalThis._fdPaths[fd] = path;
                return fd;
            }
            // Fallback: use native fs functions (limited - no fd tracking)
            // Create a pseudo-fd that stores the path for later writeFileSync calls
            _log('[fs.openSync] Using pseudo-fd fallback');
            const pseudoFd = ++globalThis._nextPseudoFd || (globalThis._nextPseudoFd = 100);
            globalThis._nextPseudoFd = pseudoFd;
            if (!globalThis._fdPaths) globalThis._fdPaths = {};
            globalThis._fdPaths[pseudoFd] = path;
            globalThis._fdFlags = globalThis._fdFlags || {};
            globalThis._fdFlags[pseudoFd] = flags;
            return pseudoFd;
        },
        closeSync: function(fd) {
            const _osModule = globalThis._os || (typeof os !== 'undefined' ? os : null);
            if (_osModule && typeof _osModule.close === 'function' && fd < 100) {
                // Real fd - close via os module
                _osModule.close(fd);
            }
            // Clean up tracking
            if (globalThis._fdPaths) delete globalThis._fdPaths[fd];
            if (globalThis._fdFlags) delete globalThis._fdFlags[fd];
            if (globalThis._fdBuffers) delete globalThis._fdBuffers[fd];
        },
        readSync: function(fd, buffer, offset, length, position) {
            if (typeof _os !== 'undefined' && _os.read) {
                const result = _os.read(fd, buffer.buffer || buffer, offset, length);
                return result;
            }
            return 0;
        },
        writeSync: function(fd, buffer, offsetOrPosition, lengthOrEncoding, position) {
            // Get QuickJS os module - same pattern as openSync
            const _osModule = globalThis._os || (typeof os !== 'undefined' ? os : null);
            if (_osModule && typeof _osModule.write === 'function') {
                // Node.js fs.writeSync has two signatures:
                // 1. writeSync(fd, buffer, offset, length, position) - Buffer signature
                // 2. writeSync(fd, string, position, encoding) - String signature
                //
                // Detection: if buffer is string and lengthOrEncoding is string (encoding), use string signature
                let arrayBuffer;
                let writeOffset = 0;
                let writeLength;

                if (typeof buffer === 'string') {
                    // String signature: writeSync(fd, string, position, encoding)
                    // lengthOrEncoding is the encoding (e.g., "utf8"), ignore it
                    const encoder = new TextEncoder();
                    arrayBuffer = encoder.encode(buffer).buffer;
                    writeLength = arrayBuffer.byteLength;
                    // offsetOrPosition is the file position, but QuickJS os.write doesn't support position
                    // We write at current position anyway
                } else if (buffer instanceof ArrayBuffer) {
                    arrayBuffer = buffer;
                    writeOffset = offsetOrPosition || 0;
                    writeLength = lengthOrEncoding || arrayBuffer.byteLength;
                } else if (buffer && buffer.buffer instanceof ArrayBuffer) {
                    // TypedArray or Buffer
                    arrayBuffer = buffer.buffer;
                    writeOffset = offsetOrPosition || 0;
                    writeLength = lengthOrEncoding || arrayBuffer.byteLength;
                } else {
                    // Fallback: convert to string then to ArrayBuffer
                    const encoder = new TextEncoder();
                    arrayBuffer = encoder.encode(String(buffer)).buffer;
                    writeLength = arrayBuffer.byteLength;
                }

                const result = _osModule.write(fd, arrayBuffer, writeOffset, writeLength);
                return result;
            }

            // Fallback: use native fs_write with tracked path
            const trackedPath = globalThis._fdPaths ? globalThis._fdPaths[fd] : null;
            if (trackedPath && typeof globalThis.__edgebox_fs_write === 'function') {
                // Convert buffer to string for __edgebox_fs_write
                let dataStr;
                if (typeof buffer === 'string') {
                    dataStr = buffer;
                } else if (buffer instanceof Uint8Array || buffer instanceof ArrayBuffer) {
                    dataStr = new TextDecoder().decode(buffer);
                } else if (buffer && buffer.buffer instanceof ArrayBuffer) {
                    dataStr = new TextDecoder().decode(buffer);
                } else {
                    dataStr = String(buffer);
                }

                // Append to buffer for this fd (will be flushed on close)
                if (!globalThis._fdBuffers) globalThis._fdBuffers = {};
                if (!globalThis._fdBuffers[fd]) globalThis._fdBuffers[fd] = '';
                globalThis._fdBuffers[fd] += dataStr;

                // Write immediately to file (TypeScript expects write to persist)
                globalThis.__edgebox_fs_write(trackedPath, globalThis._fdBuffers[fd]);
                return dataStr.length;
            }

            return buffer ? buffer.length : 0;
        },
        fstatSync: function(fd) {
            const path = globalThis._fdPaths ? globalThis._fdPaths[fd] : null;
            if (path && typeof globalThis.__edgebox_fs_stat === 'function') {
                return globalThis.__edgebox_fs_stat(path);
            }
            return { isFile: () => true, isDirectory: () => false, size: 0 };
        },
        fsyncSync: function(fd) {
            // fsync is a no-op in WASI - data is flushed on close
        },
        // Stream factories
        createReadStream: function(path) {
            const content = globalThis.__edgebox_fs_read(path);
            const stream = new EventEmitter();
            stream.pipe = (dest) => { dest.write(content); dest.end(); return dest; };
            setTimeout(() => { stream.emit('data', content); stream.emit('end'); }, 0);
            return stream;
        },
        createWriteStream: function(path) {
            const chunks = [];
            return {
                write: (chunk) => { chunks.push(chunk); return true; },
                end: (chunk) => { if (chunk) chunks.push(chunk); globalThis.__edgebox_fs_write(path, chunks.join('')); },
                on: () => {}
            };
        },
        // Constants
        constants: { F_OK: 0, R_OK: 4, W_OK: 2, X_OK: 1, COPYFILE_EXCL: 1 },
        // Async versions - use true async API if available
        readFile: function(path, options) {
            path = _remapPath(path); // Mount remapping
            const self = this;
            const encoding = typeof options === 'string' ? options : (options && options.encoding);

            // Check for async file API
            if (typeof globalThis.__edgebox_file_read_start === 'function') {
                return new Promise((resolve, reject) => {
                    const requestId = globalThis.__edgebox_file_read_start(path);
                    if (requestId < 0) {
                        const err = new Error('ENOENT: no such file or directory, open \'' + path + '\'');
                        err.code = 'ENOENT';
                        reject(err);
                        return;
                    }

                    const pollForResult = () => {
                        const status = globalThis.__edgebox_file_poll(requestId);
                        if (status === 1) {
                            // Complete - get result
                            try {
                                const data = globalThis.__edgebox_file_result(requestId);
                                resolve((encoding === 'utf8' || encoding === 'utf-8') ? data : Buffer.from(data));
                            } catch (e) {
                                reject(e);
                            }
                        } else if (status < 0) {
                            const err = new Error('ENOENT: no such file or directory');
                            err.code = 'ENOENT';
                            reject(err);
                        } else {
                            // Still pending - poll again
                            setTimeout(pollForResult, 1);
                        }
                    };
                    setTimeout(pollForResult, 0);
                });
            }
            // Fallback to sync
            return Promise.resolve(self.readFileSync(path, options));
        },
        writeFile: function(path, data, options) {
            path = _remapPath(path); // Mount remapping
            const self = this;
            const buf = typeof data === 'string' ? data : String(data);

            // Check for async file API
            if (typeof globalThis.__edgebox_file_write_start === 'function') {
                return new Promise((resolve, reject) => {
                    const requestId = globalThis.__edgebox_file_write_start(path, buf);
                    if (requestId < 0) {
                        const err = new Error('EACCES: permission denied, open \'' + path + '\'');
                        err.code = 'EACCES';
                        reject(err);
                        return;
                    }

                    const pollForResult = () => {
                        const status = globalThis.__edgebox_file_poll(requestId);
                        if (status === 1) {
                            // Complete
                            try {
                                globalThis.__edgebox_file_result(requestId);
                                resolve();
                            } catch (e) {
                                reject(e);
                            }
                        } else if (status < 0) {
                            const err = new Error('EACCES: permission denied');
                            err.code = 'EACCES';
                            reject(err);
                        } else {
                            // Still pending - poll again
                            setTimeout(pollForResult, 1);
                        }
                    };
                    setTimeout(pollForResult, 0);
                });
            }
            // Fallback to sync
            return Promise.resolve(self.writeFileSync(path, data, options));
        },
        exists: function(path) { return Promise.resolve(this.existsSync(path)); },
        mkdir: function(path, options) { return Promise.resolve(this.mkdirSync(path, options)); },
        readdir: function(path, options) { return Promise.resolve(this.readdirSync(path, options)); },
        stat: function(path) { return Promise.resolve(this.statSync(path)); },
        lstat: function(path) { return Promise.resolve(this.lstatSync(path)); },
        unlink: function(path) { return Promise.resolve(this.unlinkSync(path)); },
        rmdir: function(path, options) { return Promise.resolve(this.rmdirSync(path, options)); },
        rm: function(path, options) { return Promise.resolve(this.rmSync(path, options)); },
        rename: function(oldPath, newPath) { return Promise.resolve(this.renameSync(oldPath, newPath)); },
        copyFile: function(src, dest) { return Promise.resolve(this.copyFileSync(src, dest)); },
        access: function(path, mode) { return Promise.resolve(this.accessSync(path, mode)); },
        // File watching stubs - not supported in WASI but needs to return valid watcher objects
        watch: function(path, options, listener) {
            if (typeof options === 'function') { listener = options; options = {}; }
            // Return a fake FSWatcher that does nothing
            const watcher = new EventEmitter();
            watcher.close = function() {};
            watcher.ref = function() { return this; };
            watcher.unref = function() { return this; };
            return watcher;
        },
        watchFile: function(path, options, listener) {
            if (typeof options === 'function') { listener = options; options = {}; }
            // No-op - file watching not supported
        },
        unwatchFile: function(path, listener) {
            // No-op
        },
        promises: null
    };
    _modules.fs.promises = {
        readFile: _modules.fs.readFile.bind(_modules.fs),
        writeFile: _modules.fs.writeFile.bind(_modules.fs),
        mkdir: _modules.fs.mkdir.bind(_modules.fs),
        readdir: _modules.fs.readdir.bind(_modules.fs),
        stat: _modules.fs.stat.bind(_modules.fs),
        lstat: _modules.fs.lstat.bind(_modules.fs),
        unlink: _modules.fs.unlink.bind(_modules.fs),
        rmdir: _modules.fs.rmdir.bind(_modules.fs),
        rename: _modules.fs.rename.bind(_modules.fs),
        copyFile: _modules.fs.copyFile.bind(_modules.fs),
        access: path => Promise.resolve(_modules.fs.existsSync(path)),
        realpath: path => Promise.resolve(_modules.fs.realpathSync(path))
    };
    _modules['fs/promises'] = _modules.fs.promises;

