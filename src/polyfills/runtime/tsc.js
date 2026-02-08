// EdgeBox Runtime: TypeScript Compiler Integration

/**
 * TSC Factory Interception for Native Shape Registration
 * Enables ~1.7x faster property access by avoiding QuickJS hash table lookups
 */
globalThis.__edgebox_intercept_tsc_factory = function() {
    if (typeof globalThis.ts === 'undefined') {
        console.log('[tsc.js] globalThis.ts not defined yet');
        return false;
    }

    if (typeof __edgebox_register_node !== 'function') {
        return false;
    }

    if (globalThis.__edgebox_tsc_intercepted) {
        return true;
    }

    globalThis.__edgebox_tsc_intercepted = true;
    return true;
};

globalThis.__edgebox_intercept_tsc_factory();

/**
 * TSC sys object hook - called with the real sys object from inside the TSC bundle closure.
 * This replaces the broken setTimeout-based polling that could never find the local sys variable.
 */
globalThis.__edgebox_hook_tsc_sys = function(sys) {
    if (!sys) return;

    // Hook sys.write to use process.stdout.write (should already work via default implementation,
    // but ensure it's properly bound)
    var origWrite = sys.write;
    sys.write = function(s) {
        if (typeof process !== 'undefined' && process.stdout && typeof process.stdout.write === 'function') {
            process.stdout.write(s);
        } else if (origWrite) {
            origWrite(s);
        }
    };

    // Hook sys.writeOutputIsTTY to return false (pipe mode)
    sys.writeOutputIsTTY = function() { return false; };

    // Hook sys.writeFile for output emission
    sys.writeFile = function(path, data, writeByteOrderMark) {
        if (writeByteOrderMark) {
            data = '\uFEFF' + data;
        }
        if (typeof globalThis.__edgebox_fs_write === 'function') {
            globalThis.__edgebox_fs_write(path, data);
        } else if (typeof require === 'function') {
            try { require('fs').writeFileSync(path, data); } catch(e) {}
        }
    };

    // Hook sys.readFile for file reading
    var origReadFile = sys.readFile;
    sys.readFile = function(path, encoding) {
        if (typeof globalThis.__edgebox_fs_read === 'function') {
            try {
                return globalThis.__edgebox_fs_read(path);
            } catch(e) {
                return undefined;
            }
        }
        if (origReadFile) {
            return origReadFile.call(sys, path, encoding);
        }
        return undefined;
    };

    // Hook sys.fileExists
    var origFileExists = sys.fileExists;
    sys.fileExists = function(path) {
        if (typeof globalThis.__edgebox_fs_stat === 'function') {
            try {
                var stat = globalThis.__edgebox_fs_stat(path);
                return stat && !stat.isDirectory;
            } catch(e) {
                return false;
            }
        }
        if (origFileExists) {
            return origFileExists.call(sys, path);
        }
        return false;
    };

    // Hook sys.directoryExists
    var origDirectoryExists = sys.directoryExists;
    sys.directoryExists = function(path) {
        if (typeof globalThis.__edgebox_fs_stat === 'function') {
            try {
                var stat = globalThis.__edgebox_fs_stat(path);
                return stat && stat.isDirectory;
            } catch(e) {
                return false;
            }
        }
        if (origDirectoryExists) {
            return origDirectoryExists.call(sys, path);
        }
        return false;
    };

    // Hook sys.createDirectory
    sys.createDirectory = function(path) {
        if (typeof globalThis.__edgebox_fs_mkdir === 'function') {
            try {
                globalThis.__edgebox_fs_mkdir(path, true);
            } catch(e) {
                // Ignore errors - directory may already exist
            }
        }
    };

    // Hook sys.getDirectories
    if (sys.getDirectories) {
        var origGetDirectories = sys.getDirectories;
        sys.getDirectories = function(path) {
            if (typeof globalThis.__edgebox_fs_readdir === 'function') {
                try {
                    var entries = globalThis.__edgebox_fs_readdir(path);
                    return entries.filter(function(e) { return e.isDirectory; }).map(function(e) { return e.name; });
                } catch(e) {
                    return [];
                }
            }
            if (origGetDirectories) {
                return origGetDirectories.call(sys, path);
            }
            return [];
        };
    }

    // Hook sys.readDirectory
    if (sys.readDirectory) {
        var origReadDirectory = sys.readDirectory;
        sys.readDirectory = function(path, extensions, excludes, includes, depth) {
            if (typeof globalThis.__edgebox_fs_readdir === 'function') {
                try {
                    var entries = globalThis.__edgebox_fs_readdir(path);
                    var files = entries.filter(function(e) { return !e.isDirectory; }).map(function(e) { return e.name; });
                    if (extensions && extensions.length > 0) {
                        files = files.filter(function(f) {
                            return extensions.some(function(ext) { return f.endsWith(ext); });
                        });
                    }
                    return files;
                } catch(e) {
                    return origReadDirectory ? origReadDirectory.call(sys, path, extensions, excludes, includes, depth) : [];
                }
            }
            return origReadDirectory ? origReadDirectory.call(sys, path, extensions, excludes, includes, depth) : [];
        };
    }
};
