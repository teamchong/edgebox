// EdgeBox Runtime Core - Always included
// Contains: print capture, error handling, console, timers

// Capture native print function (QuickJS's js_std_add_helpers provides it)
const __native_print = typeof print === 'function' ? print : () => {};

// GUARD: Skip if already initialized (Wizer pre-initialized case)
if (globalThis._runtimePolyfillsInitialized) {
    // Runtime polyfills already loaded by Wizer, skip to user code
} else {

// Global error handler - catch any unhandled errors
globalThis._edgebox_debug = typeof scriptArgs !== 'undefined' && scriptArgs.includes('--debug');

// Unhandled promise rejection handler
if (typeof Promise !== 'undefined' && typeof Promise.prototype.then === 'function') {
    const originalThen = Promise.prototype.then;
    Promise.prototype.then = function(onFulfilled, onRejected) {
        return originalThen.call(this, onFulfilled, function(err) {
            if (onRejected) {
                return onRejected(err);
            }
            // Log unhandled rejection
            if (typeof print === 'function') {
                print('[PROMISE] Unhandled rejection: ' + (err && err.message ? err.message : String(err)));
            }
            throw err;
        });
    };
}

// Intercept Emscripten-style abort() calls from WASM modules that fail to load
globalThis.abort = function(msg) {
    if (typeof print === 'function') {
        print('WARN: Aborted(' + msg + ')');
        try { throw new Error('abort trace'); } catch(e) { print('[ABORT STACK] ' + e.stack); }
    }
};

// DEBUG: Keepalive timer (disabled by default)
globalThis._edgeboxKeepalive = 0;
globalThis._edgeboxStartKeepalive = function() {
    if (globalThis._edgeboxKeepalive) return;
    globalThis._edgeboxKeepalive = 1;
    var count = 0;
    function tick() {
        count++;
        if (count <= 20) {
            print('[KEEPALIVE] tick ' + count);
            setTimeout(tick, 500);
        } else {
            print('[KEEPALIVE] stopping after 20 ticks');
        }
    }
    setTimeout(tick, 100);
    print('[KEEPALIVE] started');
};

// CommonJS module shim for Node.js bundles that use module.exports
// IMPORTANT: Use 'var' to hoist to global scope so bare 'module' references work
if (typeof module === 'undefined') {
    var moduleExports = {};
    var module = { exports: moduleExports };
    var exports = moduleExports;
    // Also set on globalThis for explicit access
    globalThis.module = module;
    globalThis.exports = exports;
}

// Debug: Trace key CLI initialization milestones
globalThis.__cliTrace = function(msg) {
    print('[CLI TRACE] ' + msg);
};

// Ensure console has all methods (QuickJS console is incomplete)
(function() {
    if (typeof console !== 'undefined' && typeof print === 'function') {
        if (!console.error) console.error = function() { print.apply(null, ['[ERROR]'].concat(Array.prototype.slice.call(arguments))); };
        if (!console.warn) console.warn = function() { print.apply(null, ['[WARN]'].concat(Array.prototype.slice.call(arguments))); };
        if (!console.info) console.info = console.log;
        if (!console.debug) console.debug = console.log;
        if (!console.trace) console.trace = function() { print.apply(null, ['[TRACE]'].concat(Array.prototype.slice.call(arguments))); try { throw new Error(); } catch(e) { print(e.stack); } };
        if (!console.dir) console.dir = function(obj) { print(JSON.stringify(obj, null, 2)); };
        if (!console.time) console.time = function() {};
        if (!console.timeEnd) console.timeEnd = function() {};
        if (!console.assert) console.assert = function(cond, msg) { if (!cond) throw new Error(msg || 'Assertion failed'); };
        if (!console.clear) console.clear = function() {};
        if (!console.count) console.count = function() {};
        if (!console.countReset) console.countReset = function() {};
        if (!console.group) console.group = console.log;
        if (!console.groupCollapsed) console.groupCollapsed = console.log;
        if (!console.groupEnd) console.groupEnd = function() {};
        if (!console.table) console.table = console.dir;
    }
})();

// Install global error handler first, before anything else
if (globalThis._edgebox_debug) {
    if (globalThis._edgebox_debug) print('[EDGEBOX JS] Runtime polyfills loading...');
}

// Unhandled rejection and error tracking
globalThis._edgebox_errors = [];
globalThis._edgebox_reportError = function(type, error) {
    const msg = error && error.message ? error.message : String(error);
    const stack = error && error.stack ? error.stack : '';
    if (globalThis._edgebox_debug) {
        print('[EDGEBOX ' + type + '] ' + msg);
        if (stack) print(stack);
    }
    globalThis._edgebox_errors.push({ type, error: msg, stack });
};

// Global unhandled rejection handler for browsers/Node-like environments
globalThis.onunhandledrejection = function(event) {
    if (event && event.preventDefault) {
        event.preventDefault();
    }
    if (globalThis._edgebox_debug) {
        globalThis._edgebox_reportError('UNHANDLED_REJECTION', event && event.reason);
    }
};

// Console polyfill - preserve native methods, only add fallbacks
{
    const _existingConsole = globalThis.console || {};
    const consoleImpl = {
        log: _existingConsole.log || ((...args) => __native_print(...args)),
        error: _existingConsole.error || ((...args) => __native_print('ERROR:', ...args)),
        warn: _existingConsole.warn || ((...args) => __native_print('WARN:', ...args)),
        info: _existingConsole.info || ((...args) => __native_print('INFO:', ...args)),
        debug: _existingConsole.debug || ((...args) => __native_print('DEBUG:', ...args)),
        trace: _existingConsole.trace || ((...args) => __native_print('TRACE:', ...args)),
        dir: _existingConsole.dir || ((obj) => __native_print(JSON.stringify(obj, null, 2))),
        table: _existingConsole.table || ((data) => __native_print(JSON.stringify(data, null, 2))),
        assert: _existingConsole.assert || ((cond, ...args) => { if (!cond) __native_print('ASSERTION FAILED:', ...args); }),
        time: _existingConsole.time || (() => {}),
        timeEnd: _existingConsole.timeEnd || (() => {}),
        timeLog: _existingConsole.timeLog || (() => {}),
        count: _existingConsole.count || (() => {}),
        countReset: _existingConsole.countReset || (() => {}),
        group: _existingConsole.group || (() => {}),
        groupEnd: _existingConsole.groupEnd || (() => {}),
        groupCollapsed: _existingConsole.groupCollapsed || (() => {}),
        clear: _existingConsole.clear || (() => {}),
    };

    Object.defineProperty(globalThis, 'console', {
        value: consoleImpl,
        writable: false,
        configurable: true,
        enumerable: true
    });
}

// Timer polyfills using QuickJS os.setTimeout for proper event loop integration
(function() {
    const _os = (typeof globalThis._os !== 'undefined' && globalThis._os)
              || (typeof os !== 'undefined' ? os : null);
    let _timerId = 1;
    const _timers = new Map();

    class Timeout {
        constructor(id, callback, delay, args, isInterval = false) {
            this._id = id;
            this._callback = callback;
            this._delay = delay;
            this._args = args;
            this._isInterval = isInterval;
            this._refed = true;
        }
        ref() { this._refed = true; return this; }
        unref() { this._refed = false; return this; }
        hasRef() { return this._refed; }
        refresh() { return this; }
        [Symbol.toPrimitive]() { return this._id; }
    }

    if (_os && typeof _os.setTimeout === 'function') {
        let _setTimeoutCount = 0;
        globalThis.setTimeout = function(callback, delay = 0, ...args) {
            const id = _timerId++;
            const handle = _os.setTimeout(() => {
                _timers.delete(id);
                callback(...args);
            }, delay);
            _timers.set(id, handle);
            return new Timeout(id, callback, delay, args, false);
        };

        globalThis.clearTimeout = function(timer) {
            if (timer == null) return;
            const id = typeof timer === 'object' ? timer._id : timer;
            const handle = _timers.get(id);
            if (handle !== undefined) {
                _os.clearTimeout(handle);
                _timers.delete(id);
            }
        };

        globalThis.setInterval = function(callback, delay = 0, ...args) {
            const id = _timerId++;
            const tick = () => {
                callback(...args);
                if (_timers.has(id)) {
                    const handle = _os.setTimeout(tick, delay);
                    _timers.set(id, handle);
                }
            };
            const handle = _os.setTimeout(tick, delay);
            _timers.set(id, handle);
            return new Timeout(id, callback, delay, args, true);
        };

        globalThis.clearInterval = function(timer) {
            if (timer == null) return;
            const id = typeof timer === 'object' ? timer._id : timer;
            const handle = _timers.get(id);
            if (handle !== undefined) {
                _os.clearTimeout(handle);
                _timers.delete(id);
            }
        };
    } else {
        globalThis.setTimeout = function(callback, delay = 0, ...args) {
            const id = _timerId++;
            _timers.set(id, { callback, args, delay });
            if (delay === 0) {
                queueMicrotask(() => {
                    if (_timers.has(id)) {
                        _timers.delete(id);
                        callback(...args);
                    }
                });
            }
            return new Timeout(id, callback, delay, args, false);
        };

        globalThis.clearTimeout = function(timer) {
            if (timer == null) return;
            const id = typeof timer === 'object' ? timer._id : timer;
            _timers.delete(id);
        };
        globalThis.setInterval = function(callback, delay = 0, ...args) {
            const id = _timerId++;
            return new Timeout(id, callback, delay, args, true);
        };
        globalThis.clearInterval = function(timer) {
            if (timer == null) return;
            const id = typeof timer === 'object' ? timer._id : timer;
            _timers.delete(id);
        };
    }

    globalThis.setImmediate = function(callback, ...args) { return setTimeout(callback, 0, ...args); };
    globalThis.clearImmediate = function(id) { clearTimeout(id); };
})();

// Performance polyfill
globalThis.performance = globalThis.performance || { now: () => Date.now(), mark: () => {}, measure: () => {}, getEntriesByType: () => [] };

// Store original console.log to ensure it works
const _originalConsoleLog = console.log.bind(console);

// Mark core as loaded (end block will set _runtimePolyfillsInitialized)
globalThis._runtimeCoreLoaded = true;
