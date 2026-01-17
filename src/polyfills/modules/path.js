    // ===== PATH MODULE =====
    // 100% Zig native: src/polyfills/path.zig
    // JS: only adds posix/win32/node: aliases
    if (_modules.path) {
        _modules.path.posix = _modules.path;
        _modules.path.win32 = Object.assign({}, _modules.path, { sep: '\\', delimiter: ';' });
    }
    _modules['node:path'] = _modules.path;

