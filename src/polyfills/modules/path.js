    // ===== PATH MODULE =====
    // 100% Zig native: src/polyfills/path.zig
    // JS: only adds posix/win32/node: aliases
    // Note: path.posix used to be a self-reference causing GC issues
    // Now we create a shallow copy to avoid circular reference
    if (_modules.path) {
        // Create shallow copies to avoid circular reference that prevents GC
        _modules.path.posix = Object.assign({}, _modules.path);
        _modules.path.win32 = Object.assign({}, _modules.path, { sep: '\\', delimiter: ';' });
    }
    _modules['node:path'] = _modules.path;

