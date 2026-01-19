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
            const withFileTypes = options && options.withFileTypes;
            const recursive = options && options.recursive;
            _log('[FS] readdirSync: ' + path + ' withFileTypes=' + withFileTypes + ' recursive=' + recursive);

            const self = this;

            // Helper function to read a single directory
            function readDir(dirPath) {
                let entries;
                if (typeof globalThis.__edgebox_fs_readdir === 'function') {
                    entries = globalThis.__edgebox_fs_readdir(dirPath);
                } else if (typeof _os !== 'undefined' && _os.readdir) {
                    const r = _os.readdir(dirPath);
                    if (r[1] !== 0) { const err = new Error('ENOENT: ' + dirPath); err.code = 'ENOENT'; throw err; }
                    entries = r[0].filter(x => x !== '.' && x !== '..');
                } else {
                    throw new Error('fs.readdirSync not implemented');
                }
                return entries;
            }

            // Helper function to create Dirent-like object
            function createDirent(name, basePath, parentPath) {
                const fullPath = basePath + '/' + name;
                // For recursive, parentPath is the relative path from original dir
                const entryName = parentPath ? parentPath + '/' + name : name;
                return {
                    name: entryName,
                    parentPath: parentPath || '',
                    isFile: function() { return self.statSync(fullPath).isFile(); },
                    isDirectory: function() { return self.statSync(fullPath).isDirectory(); },
                    isSymbolicLink: function() {
                        try {
                            const lstats = self.lstatSync(fullPath);
                            return lstats.isSymbolicLink ? lstats.isSymbolicLink() : false;
                        } catch(e) { return false; }
                    }
                };
            }

            let entries = readDir(path);

            // Non-recursive mode
            if (!recursive) {
                if (withFileTypes) {
                    return entries.map(name => createDirent(name, path, ''));
                }
                return entries;
            }

            // Recursive mode - traverse all subdirectories
            const results = [];
            const stack = entries.map(name => ({ name, basePath: path, relativePath: '' }));

            while (stack.length > 0) {
                const { name, basePath, relativePath } = stack.shift();
                const fullPath = basePath + '/' + name;
                const entryRelativePath = relativePath ? relativePath + '/' + name : name;

                if (withFileTypes) {
                    results.push(createDirent(name, basePath, relativePath));
                } else {
                    results.push(entryRelativePath);
                }

                // Check if it's a directory and recurse
                try {
                    const stats = self.statSync(fullPath);
                    if (stats.isDirectory()) {
                        const subEntries = readDir(fullPath);
                        // Add subdirectory entries to the stack
                        for (const subName of subEntries) {
                            stack.push({
                                name: subName,
                                basePath: fullPath,
                                relativePath: entryRelativePath
                            });
                        }
                    }
                } catch (e) {
                    // Skip entries we can't stat (e.g., broken symlinks)
                }
            }

            return results;
        },
        statSync: function(path, options) {
            path = _remapPath(path); // Mount remapping
            const throwIfNoEntry = options && options.throwIfNoEntry === false ? false : true;
            const useBigInt = options && options.bigint === true;
            _log('[FS] statSync: ' + path + ' throwIfNoEntry=' + throwIfNoEntry);
            try {
                if (typeof globalThis.__edgebox_fs_stat === 'function') {
                    const result = globalThis.__edgebox_fs_stat(path);
                    _log('[FS] statSync result: isFile=' + (result && result.isFile ? result.isFile() : 'undefined'));
                    // Convert to BigInt if requested
                    if (useBigInt && result) {
                        return {
                            isFile: result.isFile,
                            isDirectory: result.isDirectory,
                            isSymbolicLink: result.isSymbolicLink || (() => false),
                            isBlockDevice: result.isBlockDevice || (() => false),
                            isCharacterDevice: result.isCharacterDevice || (() => false),
                            isFIFO: result.isFIFO || (() => false),
                            isSocket: result.isSocket || (() => false),
                            dev: BigInt(result.dev || 0),
                            ino: BigInt(result.ino || 0),
                            mode: BigInt(result.mode || 0),
                            nlink: BigInt(result.nlink || 1),
                            uid: BigInt(result.uid || 0),
                            gid: BigInt(result.gid || 0),
                            rdev: BigInt(result.rdev || 0),
                            size: BigInt(result.size || 0),
                            blksize: BigInt(result.blksize || 4096),
                            blocks: BigInt(result.blocks || 0),
                            atimeMs: BigInt(Math.floor(result.atimeMs || 0)),
                            mtimeMs: BigInt(Math.floor(result.mtimeMs || 0)),
                            ctimeMs: BigInt(Math.floor(result.ctimeMs || 0)),
                            birthtimeMs: BigInt(Math.floor(result.birthtimeMs || result.ctimeMs || 0)),
                            atimeNs: BigInt(Math.floor((result.atimeMs || 0) * 1000000)),
                            mtimeNs: BigInt(Math.floor((result.mtimeMs || 0) * 1000000)),
                            ctimeNs: BigInt(Math.floor((result.ctimeMs || 0) * 1000000)),
                            birthtimeNs: BigInt(Math.floor((result.birthtimeMs || result.ctimeMs || 0) * 1000000)),
                            atime: result.atime || new Date(result.atimeMs || 0),
                            mtime: result.mtime || new Date(result.mtimeMs || 0),
                            ctime: result.ctime || new Date(result.ctimeMs || 0),
                            birthtime: result.birthtime || new Date(result.birthtimeMs || result.ctimeMs || 0)
                        };
                    }
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

                    if (useBigInt) {
                        return {
                            isFile: () => (s.mode & 0o170000) === 0o100000,
                            isDirectory: () => (s.mode & 0o170000) === 0o040000,
                            isSymbolicLink: () => (s.mode & 0o170000) === 0o120000,
                            isBlockDevice: () => false,
                            isCharacterDevice: () => false,
                            isFIFO: () => false,
                            isSocket: () => false,
                            dev: BigInt(s.dev || 0),
                            ino: BigInt(s.ino || 0),
                            mode: BigInt(s.mode),
                            nlink: BigInt(s.nlink || 1),
                            uid: BigInt(s.uid || 0),
                            gid: BigInt(s.gid || 0),
                            rdev: BigInt(s.rdev || 0),
                            size: BigInt(s.size),
                            blksize: BigInt(s.blksize || 4096),
                            blocks: BigInt(s.blocks || 0),
                            atimeMs: BigInt(Math.floor(atimeMs)),
                            mtimeMs: BigInt(Math.floor(mtimeMs)),
                            ctimeMs: BigInt(Math.floor(ctimeMs)),
                            birthtimeMs: BigInt(Math.floor(ctimeMs)),
                            atimeNs: BigInt(Math.floor(atimeMs * 1000000)),
                            mtimeNs: BigInt(Math.floor(mtimeMs * 1000000)),
                            ctimeNs: BigInt(Math.floor(ctimeMs * 1000000)),
                            birthtimeNs: BigInt(Math.floor(ctimeMs * 1000000)),
                            atime: new Date(atimeMs),
                            mtime: new Date(mtimeMs),
                            ctime: new Date(ctimeMs),
                            birthtime: new Date(ctimeMs)
                        };
                    }
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
        lutimesSync: function(path, atime, mtime) {
            // Use native implementation if available
            const nativeLutimesSync = _modules._nativeFs && _modules._nativeFs.lutimesSync;
            if (nativeLutimesSync) {
                return nativeLutimesSync(_remapPath(path), atime, mtime);
            }
            // no-op in WASI
        },
        futimesSync: function(fd, atime, mtime) {
            // Use native implementation if available
            const nativeFutimesSync = _modules._nativeFs && _modules._nativeFs.futimesSync;
            if (nativeFutimesSync) {
                return nativeFutimesSync(fd, atime, mtime);
            }
            // no-op in WASI
        },
        fdatasyncSync: function(fd) {
            // Use native implementation if available
            const nativeFdatasyncSync = _modules._nativeFs && _modules._nativeFs.fdatasyncSync;
            if (nativeFdatasyncSync) {
                return nativeFdatasyncSync(fd);
            }
            // Fallback to fsync semantics
            const nativeFsyncSync = _modules._nativeFs && _modules._nativeFs.fsyncSync;
            if (nativeFsyncSync) {
                return nativeFsyncSync(fd);
            }
        },
        statfsSync: function(path) {
            // Use native implementation if available
            const nativeStatfsSync = _modules._nativeFs && _modules._nativeFs.statfsSync;
            if (nativeStatfsSync) {
                return nativeStatfsSync(_remapPath(path));
            }
            throw new Error('fs.statfsSync not supported');
        },
        fchmodSync: function(fd, mode) {
            // Use native implementation if available
            const nativeFchmodSync = _modules._nativeFs && _modules._nativeFs.fchmodSync;
            if (nativeFchmodSync) {
                return nativeFchmodSync(fd, mode);
            }
            // no-op in WASI
        },
        fchownSync: function(fd, uid, gid) {
            // Use native implementation if available
            const nativeFchownSync = _modules._nativeFs && _modules._nativeFs.fchownSync;
            if (nativeFchownSync) {
                return nativeFchownSync(fd, uid, gid);
            }
            // no-op in WASI
        },
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
        // mkdtempSync - create temp directory with random suffix
        mkdtempSync: function(prefix, options) {
            // Use native mkdtemp if available
            if (typeof _modules !== 'undefined' && _modules._nativeFs && _modules._nativeFs.mkdtempSync) {
                return _modules._nativeFs.mkdtempSync(prefix);
            }
            // Fallback: use random suffix
            const suffix = Math.random().toString(36).substring(2, 8);
            const tempPath = prefix + suffix;
            this.mkdirSync(tempPath);
            return tempPath;
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
            // Check for native implementation (set by fs.zig) that supports position
            const nativeReadSync = _modules._nativeFs && _modules._nativeFs.readSync;
            if (nativeReadSync) {
                return nativeReadSync(fd, buffer.buffer || buffer, offset, length, position);
            }
            // Fallback to QuickJS os.read (doesn't support position)
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
        constants: {
            // Access mode flags
            F_OK: 0, R_OK: 4, W_OK: 2, X_OK: 1,
            // Copy file flags
            COPYFILE_EXCL: 1, COPYFILE_FICLONE: 2, COPYFILE_FICLONE_FORCE: 4,
            // Open flags
            O_RDONLY: 0, O_WRONLY: 1, O_RDWR: 2,
            O_CREAT: 0o100, O_EXCL: 0o200, O_NOCTTY: 0o400,
            O_TRUNC: 0o1000, O_APPEND: 0o2000, O_NONBLOCK: 0o4000,
            O_SYNC: 0o4010000, O_DSYNC: 0o10000, O_DIRECTORY: 0o200000,
            O_NOFOLLOW: 0o400000,
            // File type flags (for stat mode)
            S_IFMT: 0o170000, S_IFREG: 0o100000, S_IFDIR: 0o040000,
            S_IFCHR: 0o020000, S_IFBLK: 0o060000, S_IFIFO: 0o010000,
            S_IFLNK: 0o120000, S_IFSOCK: 0o140000,
            // Permission flags
            S_IRWXU: 0o700, S_IRUSR: 0o400, S_IWUSR: 0o200, S_IXUSR: 0o100,
            S_IRWXG: 0o070, S_IRGRP: 0o040, S_IWGRP: 0o020, S_IXGRP: 0o010,
            S_IRWXO: 0o007, S_IROTH: 0o004, S_IWOTH: 0o002, S_IXOTH: 0o001,
            // UV filesystem flags
            UV_FS_SYMLINK_DIR: 1, UV_FS_SYMLINK_JUNCTION: 2, UV_FS_COPYFILE_EXCL: 1,
            UV_FS_COPYFILE_FICLONE: 2, UV_FS_COPYFILE_FICLONE_FORCE: 4
        },

        // Dir class for opendir
        Dir: class Dir {
            constructor(path, entries) {
                this.path = path;
                this._entries = entries || [];
                this._index = 0;
                this._closed = false;
            }
            read(callback) {
                if (this._closed) {
                    const err = new Error('Directory handle was closed');
                    err.code = 'ERR_DIR_CLOSED';
                    if (callback) return callback(err);
                    return Promise.reject(err);
                }
                const entry = this._entries[this._index++] || null;
                if (callback) return callback(null, entry);
                return Promise.resolve(entry);
            }
            readSync() {
                if (this._closed) {
                    const err = new Error('Directory handle was closed');
                    err.code = 'ERR_DIR_CLOSED';
                    throw err;
                }
                return this._entries[this._index++] || null;
            }
            close(callback) {
                this._closed = true;
                if (callback) return callback(null);
                return Promise.resolve();
            }
            closeSync() {
                this._closed = true;
            }
            [Symbol.asyncIterator]() {
                const self = this;
                return {
                    async next() {
                        const entry = await self.read();
                        if (entry === null) {
                            return { done: true, value: undefined };
                        }
                        return { done: false, value: entry };
                    }
                };
            }
        },

        // Dirent class for directory entries
        Dirent: class Dirent {
            constructor(name, type) {
                this.name = name;
                this._type = type; // 'file', 'directory', 'symlink', etc.
            }
            isFile() { return this._type === 'file'; }
            isDirectory() { return this._type === 'directory'; }
            isSymbolicLink() { return this._type === 'symlink'; }
            isBlockDevice() { return this._type === 'block'; }
            isCharacterDevice() { return this._type === 'character'; }
            isFIFO() { return this._type === 'fifo'; }
            isSocket() { return this._type === 'socket'; }
        },

        // opendir - open directory for iteration
        opendir: function(path, options, callback) {
            if (typeof options === 'function') { callback = options; options = {}; }
            path = _remapPath(path);
            const fs = _modules.fs;

            const doOpendir = () => {
                try {
                    const names = fs.readdirSync(path);
                    const entries = names.map(name => {
                        try {
                            const stat = fs.statSync(path + '/' + name);
                            const type = stat.isDirectory() ? 'directory' :
                                        stat.isSymbolicLink && stat.isSymbolicLink() ? 'symlink' : 'file';
                            return new fs.Dirent(name, type);
                        } catch (e) {
                            return new fs.Dirent(name, 'file');
                        }
                    });
                    return new fs.Dir(path, entries);
                } catch (e) {
                    const err = new Error('ENOENT: no such directory: ' + path);
                    err.code = 'ENOENT';
                    throw err;
                }
            };

            if (callback) {
                try {
                    const dir = doOpendir();
                    callback(null, dir);
                } catch (e) {
                    callback(e);
                }
                return;
            }
            return Promise.resolve().then(doOpendir);
        },

        // opendirSync - synchronous version
        opendirSync: function(path, options) {
            path = _remapPath(path);
            const fs = _modules.fs;
            const names = fs.readdirSync(path);
            const entries = names.map(name => {
                try {
                    const stat = fs.statSync(path + '/' + name);
                    const type = stat.isDirectory() ? 'directory' :
                                stat.isSymbolicLink && stat.isSymbolicLink() ? 'symlink' : 'file';
                    return new fs.Dirent(name, type);
                } catch (e) {
                    return new fs.Dirent(name, 'file');
                }
            });
            return new fs.Dir(path, entries);
        },
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

        // read - standalone read (like fs.read but promise-based)
        read: function(fd, buffer, offset, length, position) {
            return new Promise((resolve, reject) => {
                try {
                    // Handle FileHandle object or raw fd
                    const actualFd = typeof fd === 'object' && fd.fd !== undefined ? fd.fd : fd;
                    const bytesRead = _modules.fs.readSync(actualFd, buffer, offset, length, position);
                    resolve({ bytesRead, buffer });
                } catch (e) {
                    reject(e);
                }
            });
        },

        // write - standalone write (like fs.write but promise-based)
        write: function(fd, buffer, offset, length, position) {
            return new Promise((resolve, reject) => {
                try {
                    // Handle FileHandle object or raw fd
                    const actualFd = typeof fd === 'object' && fd.fd !== undefined ? fd.fd : fd;
                    // Handle string or Buffer
                    if (typeof buffer === 'string') {
                        const strBuf = Buffer.from(buffer, offset); // offset is encoding for strings
                        const bytesWritten = _modules.fs.writeSync(actualFd, strBuf, 0, strBuf.length, length); // length is position for strings
                        resolve({ bytesWritten, buffer: strBuf });
                    } else {
                        const bytesWritten = _modules.fs.writeSync(actualFd, buffer, offset, length, position);
                        resolve({ bytesWritten, buffer });
                    }
                } catch (e) {
                    reject(e);
                }
            });
        },

        // readv - scatter read into multiple buffers
        readv: function(fd, buffers, position) {
            return new Promise((resolve, reject) => {
                try {
                    const actualFd = typeof fd === 'object' && fd.fd !== undefined ? fd.fd : fd;
                    let totalBytesRead = 0;
                    for (const buffer of buffers) {
                        const bytesRead = _modules.fs.readSync(actualFd, buffer, 0, buffer.length, position !== undefined ? position + totalBytesRead : null);
                        totalBytesRead += bytesRead;
                        if (bytesRead < buffer.length) break; // EOF
                    }
                    resolve({ bytesRead: totalBytesRead, buffers });
                } catch (e) {
                    reject(e);
                }
            });
        },

        // writev - gather write from multiple buffers
        writev: function(fd, buffers, position) {
            return new Promise((resolve, reject) => {
                try {
                    const actualFd = typeof fd === 'object' && fd.fd !== undefined ? fd.fd : fd;
                    let totalBytesWritten = 0;
                    for (const buffer of buffers) {
                        const bytesWritten = _modules.fs.writeSync(actualFd, buffer, 0, buffer.length, position !== undefined ? position + totalBytesWritten : null);
                        totalBytesWritten += bytesWritten;
                    }
                    resolve({ bytesWritten: totalBytesWritten, buffers });
                } catch (e) {
                    reject(e);
                }
            });
        },

        // open - return FileHandle-like object
        open: function(path, flags, mode) {
            const fd = _modules.fs.openSync(_remapPath(path), flags || 'r', mode);
            const fileHandle = {
                fd: fd,
                read: function(bufferOrOptions, offset, length, position) {
                    // Handle both signatures:
                    // read(buffer, offset, length, position)
                    // read({ buffer, offset, length, position })
                    let buffer, opts;
                    if (bufferOrOptions && typeof bufferOrOptions === 'object' && !(bufferOrOptions instanceof Buffer) && !(bufferOrOptions instanceof Uint8Array)) {
                        opts = bufferOrOptions;
                        buffer = opts.buffer || Buffer.alloc(16384);
                        offset = opts.offset || 0;
                        length = opts.length || buffer.length - offset;
                        position = opts.position;
                    } else {
                        buffer = bufferOrOptions || Buffer.alloc(16384);
                    }
                    const bytesRead = _modules.fs.readSync(fd, buffer, offset || 0, length || buffer.length, position);
                    return Promise.resolve({ bytesRead, buffer });
                },
                readFile: function(options) {
                    // Read entire file from current position
                    const stat = _modules.fs.fstatSync(fd);
                    const buffer = Buffer.alloc(stat.size);
                    _modules.fs.readSync(fd, buffer, 0, stat.size, 0);
                    if (options && options.encoding) {
                        return Promise.resolve(buffer.toString(options.encoding));
                    }
                    return Promise.resolve(buffer);
                },
                write: function(bufferOrString, offsetOrOptions, length, position) {
                    if (typeof bufferOrString === 'string') {
                        const encoding = typeof offsetOrOptions === 'string' ? offsetOrOptions : 'utf8';
                        const strBuf = Buffer.from(bufferOrString, encoding);
                        const bytesWritten = _modules.fs.writeSync(fd, strBuf, 0, strBuf.length, null);
                        return Promise.resolve({ bytesWritten, buffer: strBuf });
                    }
                    const bytesWritten = _modules.fs.writeSync(fd, bufferOrString, offsetOrOptions || 0, length || bufferOrString.length, position);
                    return Promise.resolve({ bytesWritten, buffer: bufferOrString });
                },
                writeFile: function(data, options) {
                    const buffer = Buffer.isBuffer(data) ? data : Buffer.from(data, options?.encoding || 'utf8');
                    _modules.fs.writeSync(fd, buffer, 0, buffer.length, 0);
                    return Promise.resolve();
                },
                appendFile: function(data, options) {
                    const buffer = Buffer.isBuffer(data) ? data : Buffer.from(data, options?.encoding || 'utf8');
                    const stat = _modules.fs.fstatSync(fd);
                    _modules.fs.writeSync(fd, buffer, 0, buffer.length, stat.size);
                    return Promise.resolve();
                },
                close: function() {
                    _modules.fs.closeSync(fd);
                    return Promise.resolve();
                },
                stat: function(options) {
                    return Promise.resolve(_modules.fs.fstatSync(fd));
                },
                truncate: function(len) {
                    len = len || 0;
                    // Read current content, truncate, write back
                    const stat = _modules.fs.fstatSync(fd);
                    if (len >= stat.size) {
                        return Promise.resolve();
                    }
                    const buffer = Buffer.alloc(len);
                    _modules.fs.readSync(fd, buffer, 0, len, 0);
                    // Rewrite the file (WASI limitation - no ftruncate)
                    _modules.fs.writeSync(fd, buffer, 0, len, 0);
                    return Promise.resolve();
                },
                sync: function() {
                    _modules.fs.fsyncSync(fd);
                    return Promise.resolve();
                },
                datasync: function() {
                    _modules.fs.fsyncSync(fd);
                    return Promise.resolve();
                },
                chmod: function(mode) {
                    // Not directly supported via fd in WASI, would need path
                    return Promise.resolve();
                },
                chown: function(uid, gid) {
                    // Not directly supported via fd in WASI
                    return Promise.resolve();
                },
                readv: function(buffers, position) {
                    let totalBytesRead = 0;
                    for (const buffer of buffers) {
                        const bytesRead = _modules.fs.readSync(fd, buffer, 0, buffer.length, position !== undefined ? position + totalBytesRead : null);
                        totalBytesRead += bytesRead;
                        if (bytesRead < buffer.length) break;
                    }
                    return Promise.resolve({ bytesRead: totalBytesRead, buffers });
                },
                writev: function(buffers, position) {
                    let totalBytesWritten = 0;
                    for (const buffer of buffers) {
                        const bytesWritten = _modules.fs.writeSync(fd, buffer, 0, buffer.length, position !== undefined ? position + totalBytesWritten : null);
                        totalBytesWritten += bytesWritten;
                    }
                    return Promise.resolve({ bytesWritten: totalBytesWritten, buffers });
                },
                createReadStream: function(options) {
                    // Create a Readable stream from this FileHandle
                    const readable = new Readable();
                    const chunkSize = (options && options.highWaterMark) || 16384;
                    let pos = (options && options.start) || 0;
                    const end = (options && options.end) || Infinity;
                    readable._read = function() {
                        if (pos >= end) {
                            readable.push(null);
                            return;
                        }
                        const buffer = Buffer.alloc(Math.min(chunkSize, end - pos));
                        try {
                            const bytesRead = _modules.fs.readSync(fd, buffer, 0, buffer.length, pos);
                            if (bytesRead === 0) {
                                readable.push(null);
                            } else {
                                pos += bytesRead;
                                readable.push(buffer.slice(0, bytesRead));
                            }
                        } catch (e) {
                            readable.destroy(e);
                        }
                    };
                    return readable;
                },
                createWriteStream: function(options) {
                    // Create a Writable stream to this FileHandle
                    const writable = new Writable();
                    let pos = (options && options.start) || 0;
                    writable._write = function(chunk, encoding, callback) {
                        try {
                            const buffer = Buffer.isBuffer(chunk) ? chunk : Buffer.from(chunk, encoding);
                            const bytesWritten = _modules.fs.writeSync(fd, buffer, 0, buffer.length, pos);
                            pos += bytesWritten;
                            callback();
                        } catch (e) {
                            callback(e);
                        }
                    };
                    return writable;
                },
                // Symbol for async disposal (Node.js 20+)
                [Symbol.asyncDispose]: async function() {
                    await fileHandle.close();
                }
            };
            return Promise.resolve(fileHandle);
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

