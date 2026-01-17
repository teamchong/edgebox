    // ===== PATH MODULE =====
    // ONLY create JS polyfill if native Zig path module doesn't exist
    // Native path module is registered in src/polyfills/path.zig with zero-allocation implementations
    if (!_modules.path) {
        _modules.path = {
            sep: '/',
            delimiter: ':',
            join: function(...parts) {
                const joined = parts.filter(p => p).join('/').replace(/\/+/g, '/').replace(/\/$/, '') || '.';
                return this.normalize(joined);
            },
            resolve: function(...parts) {
                let resolved = '';
                for (let i = parts.length - 1; i >= 0; i--) {
                    const part = parts[i];
                    if (!part) continue;
                    resolved = part + '/' + resolved;
                    if (part.startsWith('/')) break;
                }
                return this.normalize(resolved || '/');
            },
            normalize: function(p) {
                const parts = p.split('/').filter(x => x && x !== '.');
                const result = [];
                for (const part of parts) {
                    if (part === '..') {
                        if (result.length && result[result.length-1] !== '..') result.pop();
                        else if (!p.startsWith('/')) result.push('..');
                    } else result.push(part);
                }
                return (p.startsWith('/') ? '/' : '') + result.join('/') || '.';
            },
            dirname: function(p) {
                const idx = p.lastIndexOf('/');
                if (idx === -1) return '.';
                if (idx === 0) return '/';
                return p.slice(0, idx);
            },
            basename: function(p, ext) {
                let base = p.slice(p.lastIndexOf('/') + 1);
                if (ext && base.endsWith(ext)) base = base.slice(0, -ext.length);
                return base;
            },
            extname: function(p) {
                const base = this.basename(p);
                const idx = base.lastIndexOf('.');
                return idx > 0 ? base.slice(idx) : '';
            },
            isAbsolute: function(p) { return p.startsWith('/'); },
            relative: function(from, to) {
                from = this.resolve(from).split('/');
                to = this.resolve(to).split('/');
                while (from.length && to.length && from[0] === to[0]) { from.shift(); to.shift(); }
                return [...from.map(() => '..'), ...to].join('/') || '.';
            },
            parse: function(p) {
                return { root: p.startsWith('/') ? '/' : '', dir: this.dirname(p), base: this.basename(p), ext: this.extname(p), name: this.basename(p, this.extname(p)) };
            },
            format: function(obj) {
                const dir = obj.dir || obj.root || '';
                const base = obj.base || (obj.name || '') + (obj.ext || '');
                return dir ? this.join(dir, base) : base;
            }
        };
    }
    // Add posix and win32 aliases (WASM always uses posix-style paths)
    _modules.path.posix = _modules.path;
    _modules.path.win32 = Object.assign({}, _modules.path, { sep: '\\', delimiter: ';' });
    _modules['node:path'] = _modules.path;

