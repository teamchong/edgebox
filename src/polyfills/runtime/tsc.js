// EdgeBox Runtime: TypeScript Compiler Integration

/**
 * TSC Factory Interception for Native Shape Registration
 * Enables ~1.7x faster property access by avoiding QuickJS hash table lookups
 */
globalThis.__edgebox_intercept_tsc_factory = function() {
    if (typeof globalThis.ts === 'undefined') {
        return false;
    }

    if (typeof __edgebox_register_node !== 'function') {
        return false;
    }

    if (globalThis.__edgebox_tsc_intercepted) {
        return true;
    }

    const ts = globalThis.ts;

    const originalCreateSourceFile = ts.createSourceFile;
    if (typeof originalCreateSourceFile === 'function') {
        ts.createSourceFile = function(fileName, sourceText, languageVersion, setParentNodes, scriptKind) {
            const result = originalCreateSourceFile.apply(this, arguments);

            function registerNode(node) {
                if (node && typeof node.kind === 'number') {
                    __edgebox_register_node(
                        node,
                        node.kind,
                        node.flags || 0,
                        node.pos || 0,
                        node.end || 0
                    );
                    ts.forEachChild(node, registerNode);
                }
            }
            registerNode(result);

            return result;
        };
    }

    if (ts.factory && typeof ts.factory.createNode === 'function') {
        const originalCreateNode = ts.factory.createNode;
        ts.factory.createNode = function(kind, pos, end) {
            const node = originalCreateNode.apply(this, arguments);
            if (node && typeof __edgebox_register_node === 'function') {
                __edgebox_register_node(node, kind, node.flags || 0, pos || 0, end || 0);
            }
            return node;
        };
    }

    globalThis.__edgebox_tsc_intercepted = true;
    return true;
};

globalThis.__edgebox_intercept_tsc_factory();

// TypeScript sys interface integration
(function() {
    'use strict';
    var checkCount = 0;
    var hooked = false;

    function hookTypescriptSys() {
        checkCount++;
        if (typeof sys === 'undefined' || !sys) {
            if (checkCount < 10) {
                setTimeout(hookTypescriptSys, 50);
            }
            return false;
        }

        if (hooked) return true;
        hooked = true;

        sys.writeFile = function(path, data, writeByteOrderMark) {
            if (writeByteOrderMark) {
                data = '\uFEFF' + data;
            }
            if (typeof globalThis.__edgebox_fs_write === 'function') {
                globalThis.__edgebox_fs_write(path, data);
            } else {
                throw new Error('sys.writeFile: native binding not available');
            }
        };

        if (sys.readFile) {
            sys.readFile = function(path, encoding) {
                if (typeof globalThis.__edgebox_fs_read === 'function') {
                    try {
                        return globalThis.__edgebox_fs_read(path);
                    } catch(e) {
                        return undefined;
                    }
                }
                return undefined;
            };
        }

        sys.fileExists = function(path) {
            if (typeof globalThis.__edgebox_fs_stat === 'function') {
                try {
                    var stat = globalThis.__edgebox_fs_stat(path);
                    return stat && !stat.isDirectory;
                } catch(e) {
                    return false;
                }
            }
            return false;
        };

        sys.directoryExists = function(path) {
            if (typeof globalThis.__edgebox_fs_stat === 'function') {
                try {
                    var stat = globalThis.__edgebox_fs_stat(path);
                    return stat && stat.isDirectory;
                } catch(e) {
                    return false;
                }
            }
            return false;
        };

        sys.createDirectory = function(path) {
            if (typeof globalThis.__edgebox_fs_mkdir === 'function') {
                try {
                    globalThis.__edgebox_fs_mkdir(path, true);
                } catch(e) {
                    // Ignore errors - directory may already exist
                }
            }
        };

        if (sys.getDirectories) {
            sys.getDirectories = function(path) {
                if (typeof globalThis.__edgebox_fs_readdir === 'function') {
                    try {
                        var entries = globalThis.__edgebox_fs_readdir(path);
                        return entries.filter(function(e) { return e.isDirectory; }).map(function(e) { return e.name; });
                    } catch(e) {
                        return [];
                    }
                }
                return [];
            };
        }

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
                        return origReadDirectory ? origReadDirectory.call(this, path, extensions, excludes, includes, depth) : [];
                    }
                }
                return origReadDirectory ? origReadDirectory.call(this, path, extensions, excludes, includes, depth) : [];
            };
        }

        return true;
    }

    hookTypescriptSys();
    if (!hooked) {
        setTimeout(hookTypescriptSys, 10);
    }
})();
