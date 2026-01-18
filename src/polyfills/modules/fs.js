    // ===== FS MODULE =====
    // fs module uses either native __edgebox_fs_* functions or std/os fallback
    // Checks happen at call time, not at load time
    let _fileReadCount = 0;

    // Helper: wrap Promise-returning function to support Node.js callback style
    function _wrapWithCallback(promiseFn) {
        return function(...args) {
            const lastArg = args[args.length - 1];
            if (typeof lastArg === 'function') {
                const callback = args.pop();
                promiseFn.apply(this, args)
                    .then(result => callback(null, result))
                    .catch(err => callback(err));
                return;
            }
            return promiseFn.apply(this, args);
        };
    }
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
                    isSymbolicLink: () => {
                        try {
                            const lstats = this.lstatSync(path + '/' + name);
                            return lstats.isSymbolicLink ? lstats.isSymbolicLink() : false;
                        } catch(e) { return false; }
                    }
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
        lstatSync: function(path, options) {
            path = _remapPath(path);
            // Use native lstat if available (for symlink detection)
            if (typeof globalThis.__edgebox_fs_lstat === 'function') {
                return globalThis.__edgebox_fs_lstat(path);
            }
            return this.statSync(path, options);
        },
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
        // Permission operations - use native if available, no-op in WASI
        chmodSync: function(path, mode) {
            path = _remapPath(path);
            if (typeof globalThis.__edgebox_fs_chmod === 'function') {
                return globalThis.__edgebox_fs_chmod(path, mode);
            }
            // no-op in WASI (no syscall support)
        },
        chownSync: function(path, uid, gid) {
            path = _remapPath(path);
            if (typeof globalThis.__edgebox_fs_chown === 'function') {
                return globalThis.__edgebox_fs_chown(path, uid, gid);
            }
            // no-op in WASI
        },
        lchmodSync: function(path, mode) { /* no-op - lchmod not available on all platforms */ },
        lchownSync: function(path, uid, gid) { /* no-op - lchown not available on all platforms */ },
        utimesSync: function(path, atime, mtime) {
            path = _remapPath(path);
            // Convert Date objects to seconds since epoch
            const atimeSec = atime instanceof Date ? atime.getTime() / 1000 : atime;
            const mtimeSec = mtime instanceof Date ? mtime.getTime() / 1000 : mtime;
            if (typeof globalThis.__edgebox_fs_utimes === 'function') {
                return globalThis.__edgebox_fs_utimes(path, atimeSec, mtimeSec);
            }
            // no-op in WASI
        },
        lutimesSync: function(path, atime, mtime) { /* no-op */ },
        futimesSync: function(fd, atime, mtime) { /* no-op */ },
        fchmodSync: function(fd, mode) { /* no-op */ },
        fchownSync: function(fd, uid, gid) { /* no-op */ },
        // Link operations - use native if available, no-op in WASI
        linkSync: function(existingPath, newPath) {
            existingPath = _remapPath(existingPath);
            newPath = _remapPath(newPath);
            if (typeof globalThis.__edgebox_fs_link === 'function') {
                return globalThis.__edgebox_fs_link(existingPath, newPath);
            }
            // no-op in WASI (no link support)
        },
        symlinkSync: function(target, path, type) {
            path = _remapPath(path);
            if (typeof globalThis.__edgebox_fs_symlink === 'function') {
                return globalThis.__edgebox_fs_symlink(target, path);
            }
            // no-op in WASI (no symlink support)
        },
        readlinkSync: function(path, options) {
            path = _remapPath(path);
            if (typeof globalThis.__edgebox_fs_readlink === 'function') {
                return globalThis.__edgebox_fs_readlink(path);
            }
            return path; // WASI fallback: return path as-is
        },
        // Truncate operations
        truncateSync: function(path, len) {
            path = _remapPath(path);
            if (typeof globalThis.__edgebox_fs_truncate === 'function') {
                return globalThis.__edgebox_fs_truncate(path, len || 0);
            }
            // Fallback: read, truncate, write
            const content = this.readFileSync(path);
            this.writeFileSync(path, content.slice(0, len || 0));
        },
        ftruncateSync: function(fd, len) { /* no-op - would need fd-based write */ },
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
        // Stream factories - proper Readable/Writable streams
        createReadStream: function(filePath, options) {
            filePath = _remapPath(filePath);
            options = options || {};
            const { encoding = null, start = 0, end = Infinity, highWaterMark = 64 * 1024, autoClose = true } = options;

            const stream = new (_modules.stream?.Readable || EventEmitter)();
            stream.path = filePath;
            stream.bytesRead = 0;
            stream.pending = true;

            let content = null;
            let position = start;
            let destroyed = false;

            stream._read = function(size) {
                if (destroyed) return;

                // Lazy load content on first read
                if (content === null) {
                    try {
                        const raw = globalThis.__edgebox_fs_read(filePath);
                        content = typeof raw === 'string' ? Buffer.from(raw) : (raw || Buffer.alloc(0));
                        stream.pending = false;
                    } catch (e) {
                        stream.destroy(e);
                        return;
                    }
                }

                if (position >= Math.min(end, content.length)) {
                    this.push(null);
                    return;
                }

                const chunkEnd = Math.min(position + (size || highWaterMark), end, content.length);
                const chunk = content.slice(position, chunkEnd);
                position = chunkEnd;
                stream.bytesRead += chunk.length;

                this.push(encoding ? chunk.toString(encoding) : chunk);
            };

            stream.destroy = function(err) {
                if (destroyed) return this;
                destroyed = true;
                if (err) this.emit('error', err);
                this.emit('close');
                return this;
            };

            stream.close = function(callback) {
                stream.destroy();
                if (callback) callback();
            };

            // Start reading asynchronously
            setTimeout(() => {
                if (!destroyed) stream._read(highWaterMark);
            }, 0);

            return stream;
        },
        createWriteStream: function(filePath, options) {
            filePath = _remapPath(filePath);
            options = options || {};
            const { encoding = 'utf8', flags = 'w', autoClose = true } = options;

            const stream = new (_modules.stream?.Writable || EventEmitter)();
            stream.path = filePath;
            stream.bytesWritten = 0;
            stream.pending = true;

            const chunks = [];
            let destroyed = false;
            let finished = false;

            stream._write = function(chunk, enc, callback) {
                if (destroyed) {
                    if (callback) callback(new Error('Stream destroyed'));
                    return;
                }
                stream.pending = false;
                const data = Buffer.isBuffer(chunk) ? chunk : Buffer.from(String(chunk), encoding);
                chunks.push(data);
                stream.bytesWritten += data.length;
                if (callback) callback();
            };

            stream.write = function(chunk, enc, callback) {
                if (typeof enc === 'function') { callback = enc; enc = encoding; }
                stream._write(chunk, enc, callback);
                return true;
            };

            stream._final = function(callback) {
                if (finished) { if (callback) callback(); return; }
                finished = true;
                try {
                    const combined = Buffer.concat(chunks);
                    globalThis.__edgebox_fs_write(filePath, combined.toString());
                    if (callback) callback();
                } catch (e) {
                    if (callback) callback(e);
                }
            };

            stream.end = function(chunk, enc, callback) {
                if (typeof chunk === 'function') { callback = chunk; chunk = null; }
                if (typeof enc === 'function') { callback = enc; enc = null; }
                if (chunk) stream.write(chunk, enc);
                stream._final((err) => {
                    if (err) stream.emit('error', err);
                    else stream.emit('finish');
                    if (autoClose) stream.emit('close');
                    if (callback) callback(err);
                });
                return this;
            };

            stream.destroy = function(err) {
                if (destroyed) return this;
                destroyed = true;
                if (err) stream.emit('error', err);
                stream.emit('close');
                return this;
            };

            stream.close = function(callback) {
                stream.end(callback);
            };

            // EventEmitter methods if not inherited
            if (!stream.on) {
                const listeners = {};
                stream.on = function(event, fn) { (listeners[event] = listeners[event] || []).push(fn); return this; };
                stream.emit = function(event, ...args) { (listeners[event] || []).forEach(fn => fn(...args)); return true; };
                stream.once = function(event, fn) { const wrapped = (...args) => { stream.off(event, wrapped); fn(...args); }; return stream.on(event, wrapped); };
                stream.off = stream.removeListener = function(event, fn) { listeners[event] = (listeners[event] || []).filter(f => f !== fn); return this; };
            }

            return stream;
        },
        // Constants
        constants: { F_OK: 0, R_OK: 4, W_OK: 2, X_OK: 1, COPYFILE_EXCL: 1 },
        // Async versions - use true async API if available
        // Both support Promise style and callback style: readFile(path, [options], [callback])
        readFile: function(path, options, callback) {
            // Handle optional arguments - Node.js supports: readFile(path[, options], callback)
            if (typeof options === 'function') { callback = options; options = {}; }
            path = _remapPath(path);
            const self = this;
            const encoding = typeof options === 'string' ? options : (options && options.encoding);

            const doRead = () => {
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
                                try {
                                    const data = globalThis.__edgebox_file_result(requestId);
                                    resolve((encoding === 'utf8' || encoding === 'utf-8') ? data : Buffer.from(data));
                                } catch (e) { reject(e); }
                            } else if (status < 0) {
                                const err = new Error('ENOENT: no such file or directory');
                                err.code = 'ENOENT';
                                reject(err);
                            } else {
                                setTimeout(pollForResult, 1);
                            }
                        };
                        setTimeout(pollForResult, 0);
                    });
                }
                // Fallback to sync
                return Promise.resolve(self.readFileSync(path, options));
            };

            const promise = doRead();
            if (callback) {
                promise.then(result => callback(null, result)).catch(err => callback(err));
                return;
            }
            return promise;
        },
        writeFile: function(path, data, options, callback) {
            // Handle optional arguments - Node.js supports: writeFile(path, data[, options], callback)
            if (typeof options === 'function') { callback = options; options = {}; }
            path = _remapPath(path);
            const self = this;
            const buf = typeof data === 'string' ? data : String(data);

            const doWrite = () => {
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
                                try {
                                    globalThis.__edgebox_file_result(requestId);
                                    resolve();
                                } catch (e) { reject(e); }
                            } else if (status < 0) {
                                const err = new Error('EACCES: permission denied');
                                err.code = 'EACCES';
                                reject(err);
                            } else {
                                setTimeout(pollForResult, 1);
                            }
                        };
                        setTimeout(pollForResult, 0);
                    });
                }
                // Fallback to sync
                return Promise.resolve(self.writeFileSync(path, data, options));
            };

            const promise = doWrite();
            if (callback) {
                promise.then(result => callback(null, result)).catch(err => callback(err));
                return;
            }
            return promise;
        },
        appendFile: function(path, data, options, callback) {
            if (typeof options === 'function') { callback = options; options = {}; }
            const promise = Promise.resolve(this.appendFileSync(path, data, options));
            if (callback) {
                promise.then(() => callback(null)).catch(err => callback(err));
                return;
            }
            return promise;
        },
        exists: _wrapWithCallback(function(path) { return Promise.resolve(this.existsSync(path)); }),
        mkdir: _wrapWithCallback(function(path, options) { return Promise.resolve(this.mkdirSync(path, options)); }),
        readdir: _wrapWithCallback(function(path, options) { return Promise.resolve(this.readdirSync(path, options)); }),
        stat: _wrapWithCallback(function(path, options) { return Promise.resolve(this.statSync(path, options)); }),
        lstat: _wrapWithCallback(function(path, options) { return Promise.resolve(this.lstatSync(path, options)); }),
        unlink: _wrapWithCallback(function(path) { return Promise.resolve(this.unlinkSync(path)); }),
        rmdir: _wrapWithCallback(function(path, options) { return Promise.resolve(this.rmdirSync(path, options)); }),
        rm: _wrapWithCallback(function(path, options) { return Promise.resolve(this.rmSync(path, options)); }),
        rename: _wrapWithCallback(function(oldPath, newPath) { return Promise.resolve(this.renameSync(oldPath, newPath)); }),
        copyFile: _wrapWithCallback(function(src, dest, mode) { return Promise.resolve(this.copyFileSync(src, dest)); }),
        access: _wrapWithCallback(function(path, mode) { return Promise.resolve(this.accessSync(path, mode)); }),
        open: _wrapWithCallback(function(path, flags, mode) { return Promise.resolve(this.openSync(path, flags, mode)); }),
        close: _wrapWithCallback(function(fd) { return Promise.resolve(this.closeSync(fd)); }),
        read: _wrapWithCallback(function(fd, buffer, offset, length, position) { return Promise.resolve({ bytesRead: this.readSync(fd, buffer, offset, length, position), buffer }); }),
        write: _wrapWithCallback(function(fd, buffer, offset, length, position) { return Promise.resolve({ bytesWritten: this.writeSync(fd, buffer, offset, length, position), buffer }); }),
        fstat: _wrapWithCallback(function(fd, options) { return Promise.resolve(this.fstatSync(fd, options)); }),
        fsync: _wrapWithCallback(function(fd) { return Promise.resolve(this.fsyncSync(fd)); }),
        // Permission async - delegates to sync (which uses native if available)
        chmod: _wrapWithCallback(function(path, mode) { return Promise.resolve(this.chmodSync(path, mode)); }),
        chown: _wrapWithCallback(function(path, uid, gid) { return Promise.resolve(this.chownSync(path, uid, gid)); }),
        lchmod: _wrapWithCallback(function(path, mode) { return Promise.resolve(this.lchmodSync(path, mode)); }),
        lchown: _wrapWithCallback(function(path, uid, gid) { return Promise.resolve(this.lchownSync(path, uid, gid)); }),
        fchmod: _wrapWithCallback(function(fd, mode) { return Promise.resolve(this.fchmodSync(fd, mode)); }),
        fchown: _wrapWithCallback(function(fd, uid, gid) { return Promise.resolve(this.fchownSync(fd, uid, gid)); }),
        utimes: _wrapWithCallback(function(path, atime, mtime) { return Promise.resolve(this.utimesSync(path, atime, mtime)); }),
        futimes: _wrapWithCallback(function(fd, atime, mtime) { return Promise.resolve(this.futimesSync(fd, atime, mtime)); }),
        // Link async - delegates to sync (which uses native if available)
        link: _wrapWithCallback(function(existingPath, newPath) { return Promise.resolve(this.linkSync(existingPath, newPath)); }),
        symlink: _wrapWithCallback(function(target, path, type) { return Promise.resolve(this.symlinkSync(target, path, type)); }),
        readlink: _wrapWithCallback(function(path) { return Promise.resolve(this.readlinkSync(path)); }),
        // Truncate async
        truncate: _wrapWithCallback(function(path, len) { return Promise.resolve(this.truncateSync(path, len)); }),
        ftruncate: _wrapWithCallback(function(fd, len) { return Promise.resolve(this.ftruncateSync(fd, len)); }),
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
        // Basic file operations
        readFile: _modules.fs.readFile.bind(_modules.fs),
        writeFile: _modules.fs.writeFile.bind(_modules.fs),
        appendFile: function(path, data, options) {
            return Promise.resolve(_modules.fs.appendFileSync(_remapPath(path), data, options));
        },

        // Directory operations
        mkdir: _modules.fs.mkdir.bind(_modules.fs),
        readdir: _modules.fs.readdir.bind(_modules.fs),
        rmdir: _modules.fs.rmdir.bind(_modules.fs),

        // File operations
        stat: _modules.fs.stat.bind(_modules.fs),
        lstat: _modules.fs.lstat.bind(_modules.fs),
        unlink: _modules.fs.unlink.bind(_modules.fs),
        rename: _modules.fs.rename.bind(_modules.fs),
        copyFile: _modules.fs.copyFile.bind(_modules.fs),

        // rm - recursive removal
        rm: function(path, options) {
            options = options || {};
            return Promise.resolve(_modules.fs.rmSync(_remapPath(path), options));
        },

        // access - check file accessibility
        access: function(path, mode) {
            const p = _remapPath(path);
            return new Promise((resolve, reject) => {
                try {
                    if (_modules.fs.existsSync(p)) resolve();
                    else {
                        const err = new Error(`ENOENT: no such file or directory, access '${p}'`);
                        err.code = 'ENOENT';
                        reject(err);
                    }
                } catch (e) { reject(e); }
            });
        },

        // realpath - resolve symlinks
        realpath: function(path, options) {
            return Promise.resolve(_modules.fs.realpathSync(_remapPath(path), options));
        },

        // truncate - truncate file to specified length
        truncate: function(path, len) {
            len = len || 0;
            return _modules.fs.readFile(_remapPath(path)).then(content => {
                const truncated = content.slice(0, len);
                return _modules.fs.writeFile(_remapPath(path), truncated);
            });
        },

        // chmod/chown - delegates to sync (uses native if available)
        chmod: function(path, mode) { return Promise.resolve(_modules.fs.chmodSync(_remapPath(path), mode)); },
        chown: function(path, uid, gid) { return Promise.resolve(_modules.fs.chownSync(_remapPath(path), uid, gid)); },
        lchmod: function(path, mode) { return Promise.resolve(_modules.fs.lchmodSync(_remapPath(path), mode)); },
        lchown: function(path, uid, gid) { return Promise.resolve(_modules.fs.lchownSync(_remapPath(path), uid, gid)); },

        // utimes - delegates to sync (uses native if available)
        utimes: function(path, atime, mtime) { return Promise.resolve(_modules.fs.utimesSync(_remapPath(path), atime, mtime)); },
        lutimes: function(path, atime, mtime) { return Promise.resolve(_modules.fs.lutimesSync(_remapPath(path), atime, mtime)); },

        // link/symlink - delegates to sync (uses native if available)
        link: function(existingPath, newPath) { return Promise.resolve(_modules.fs.linkSync(_remapPath(existingPath), _remapPath(newPath))); },
        symlink: function(target, path, type) { return Promise.resolve(_modules.fs.symlinkSync(target, _remapPath(path), type)); },
        readlink: function(path, options) { return Promise.resolve(_modules.fs.readlinkSync(_remapPath(path), options)); },

        // mkdtemp - create temp directory with random suffix
        mkdtemp: function(prefix, options) {
            const suffix = Math.random().toString(36).substring(2, 8);
            const tempPath = prefix + suffix;
            return _modules.fs.mkdir(tempPath).then(() => tempPath);
        },

        // open - return FileHandle-like object
        open: function(path, flags, mode) {
            const fd = _modules.fs.openSync(_remapPath(path), flags || 'r', mode);
            return Promise.resolve({
                fd: fd,
                read: function(buffer, offset, length, position) {
                    const bytesRead = _modules.fs.readSync(fd, buffer, offset, length, position);
                    return Promise.resolve({ bytesRead, buffer });
                },
                write: function(buffer, offset, length, position) {
                    const bytesWritten = _modules.fs.writeSync(fd, buffer, offset, length, position);
                    return Promise.resolve({ bytesWritten, buffer });
                },
                close: function() {
                    _modules.fs.closeSync(fd);
                    return Promise.resolve();
                },
                stat: function(options) {
                    return Promise.resolve(_modules.fs.fstatSync(fd));
                },
                truncate: function(len) {
                    // Read, truncate, write back
                    return Promise.resolve();
                },
                sync: function() {
                    _modules.fs.fsyncSync(fd);
                    return Promise.resolve();
                },
                datasync: function() {
                    _modules.fs.fsyncSync(fd);
                    return Promise.resolve();
                }
            });
        },

        // watch - async iterator (stubbed - not supported in WASI)
        watch: function(path, options) {
            return {
                [Symbol.asyncIterator]: async function*() {
                    // No events in WASI
                },
                close: function() {}
            };
        },

        // cp - copy file or directory
        cp: function(src, dest, options) {
            return _modules.fs.copyFile(_remapPath(src), _remapPath(dest));
        },

        // constants
        constants: _modules.fs.constants
    };
    _modules['fs/promises'] = _modules.fs.promises;
    _modules['node:fs/promises'] = _modules.fs.promises;

