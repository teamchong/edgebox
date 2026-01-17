(function() {
    'use strict';
    // Debug flag - disabled for performance
    const _debug = false; // globalThis._polyfillDebug || ...
    const _log = _debug ? print : function() {};

    _log('[node_polyfill] START - globalThis._os?.setTimeout: ' + (typeof globalThis._os?.setTimeout));

    // GUARD: Skip polyfill initialization if already done (Wizer pre-initialized)
    if (globalThis._polyfillsInitialized) {
        return;
    }

    // Try to get QuickJS os module - needed for file descriptor operations
    let _os = null;
    try {
        if (typeof globalThis._os !== 'undefined') {
            _os = globalThis._os;
        } else if (typeof os !== 'undefined') {
            _os = os;
            globalThis._os = os;
        }
    } catch(e) {}

    // Module registry - use globalThis._modules for compatibility with require()
    globalThis._modules = globalThis._modules || {};
    const _modules = globalThis._modules;

    // ===== LAZY MODULE HELPER =====
    // Defers module creation until first access to reduce startup time
    function _lazyModule(name, factory) {
        Object.defineProperty(_modules, name, {
            configurable: true,
            enumerable: true,
            get: function() {
                _log('[node_polyfill] Lazy-loading module: ' + name);
                const mod = factory();
                // Replace getter with value for subsequent accesses
                Object.defineProperty(_modules, name, {
                    value: mod,
                    writable: true,
                    configurable: true,
                    enumerable: true
                });
                // Also set node: alias
                _modules['node:' + name] = mod;
                return mod;
            }
        });
    }

    // ===== MOUNT PATH REMAPPING (Docker-style volumes) =====
    // Parse __EDGEBOX_MOUNTS env var: [{"host":"/tmp/edgebox-home","guest":"/Users/name"}]
    let _edgeboxMounts = [];
    try {
        if (typeof std !== 'undefined' && std.getenv) {
            const mountsJson = std.getenv('__EDGEBOX_MOUNTS');
            if (mountsJson) {
                _edgeboxMounts = JSON.parse(mountsJson);
                // Sort by guest path length (longest first for specificity)
                _edgeboxMounts.sort((a, b) => b.guest.length - a.guest.length);
                _log('[node_polyfill] Mounts loaded: ' + JSON.stringify(_edgeboxMounts));
            }
        }
    } catch (e) {
        _log('[node_polyfill] Failed to parse mounts: ' + e);
    }

    // Remap guest path to host path (e.g., $HOME/.claude -> /tmp/edgebox-home/.claude)
    function _remapPath(path) {
        if (!path || typeof path !== 'string') return path;
        for (const mount of _edgeboxMounts) {
            // Exact match or path starts with mount.guest/
            if (path === mount.guest) {
                _log('[node_polyfill] Remapped ' + path + ' -> ' + mount.host);
                return mount.host;
            }
            if (path.startsWith(mount.guest + '/')) {
                const remapped = mount.host + path.slice(mount.guest.length);
                _log('[node_polyfill] Remapped ' + path + ' -> ' + remapped);
                return remapped;
            }
        }
        return path;
    }

