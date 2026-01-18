// EdgeBox Runtime Polyfills
// These are bundled with user code at build time for bytecode caching

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
// Instead of crashing, we warn and continue so the SDK can use API fallbacks
globalThis.abort = function(msg) {
    if (typeof print === 'function') {
        print('WARN: Aborted(' + msg + ')');
        // Print stack trace for debugging
        try { throw new Error('abort trace'); } catch(e) { print('[ABORT STACK] ' + e.stack); }
    }
    // Don't actually throw - let execution continue for graceful degradation
    // The SDK should detect tokenizer failure and use API-based token counting
};

// DEBUG: Add a keepalive timer to prevent event loop from exiting too early
// This will be removed once we identify why the event loop exits
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

// Start keepalive is done later after setTimeout is properly defined

// process is fully set up by native Zig (process.zig) before this runs

// CommonJS module shim for Node.js bundles that use module.exports
if (typeof module === 'undefined') {
    globalThis.module = { exports: {} };
    globalThis.exports = globalThis.module.exports;
}


// EARLY v9 definition for SDK AbortController creation
// Must be defined BEFORE bundle code runs
if (typeof globalThis.v9 === 'undefined') {
    globalThis.v9 = function() {
        // Will be replaced with full implementation after node_polyfill loads
        // For now just return a basic AbortController
        if (typeof AbortController !== 'undefined') {
            return new AbortController();
        }
        // Fallback if AbortController isn't available yet
        return { signal: { aborted: false }, abort: function() {} };
    };
}

// Debug: Trace key CLI initialization milestones
globalThis.__cliTrace = function(msg) {
    print('[CLI TRACE] ' + msg);
};

// Mock localStorage for localforage and other storage-dependent libs
if (typeof globalThis.localStorage === 'undefined') {
    const _storage = {};
    globalThis.localStorage = {
        getItem: (key) => _storage[key] !== undefined ? _storage[key] : null,
        setItem: (key, value) => { _storage[key] = String(value); },
        removeItem: (key) => { delete _storage[key]; },
        clear: () => { for (const k in _storage) delete _storage[k]; },
        key: (i) => Object.keys(_storage)[i] || null,
        get length() { return Object.keys(_storage).length; }
    };
}

// atob/btoa polyfills for base64 encoding/decoding (QuickJS doesn't have these natively)
if (typeof globalThis.atob === 'undefined') {
    const base64chars = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/';
    const base64lookup = new Uint8Array(256);
    for (let i = 0; i < base64chars.length; i++) {
        base64lookup[base64chars.charCodeAt(i)] = i;
    }

    globalThis.atob = function(str) {
        // Decode base64 string to binary string
        str = String(str).replace(/[\t\n\f\r ]/g, '');
        if (str.length % 4 === 1) throw new DOMException('Invalid base64 string', 'InvalidCharacterError');

        let output = '';
        let buffer = 0;
        let bits = 0;

        for (let i = 0; i < str.length; i++) {
            const c = str.charCodeAt(i);
            if (c === 61) break; // '=' padding
            const val = base64lookup[c];
            if (val === undefined) continue;
            buffer = (buffer << 6) | val;
            bits += 6;
            if (bits >= 8) {
                bits -= 8;
                output += String.fromCharCode((buffer >> bits) & 0xFF);
            }
        }
        return output;
    };

    globalThis.btoa = function(str) {
        // Encode binary string to base64
        str = String(str);
        let output = '';
        for (let i = 0; i < str.length; i += 3) {
            const a = str.charCodeAt(i);
            const b = i + 1 < str.length ? str.charCodeAt(i + 1) : 0;
            const c = i + 2 < str.length ? str.charCodeAt(i + 2) : 0;

            output += base64chars[a >> 2];
            output += base64chars[((a & 3) << 4) | (b >> 4)];
            output += i + 1 < str.length ? base64chars[((b & 15) << 2) | (c >> 6)] : '=';
            output += i + 2 < str.length ? base64chars[c & 63] : '=';
        }
        return output;
    };

    // Also add DOMException if it doesn't exist
    if (typeof globalThis.DOMException === 'undefined') {
        globalThis.DOMException = class DOMException extends Error {
            constructor(message, name) {
                super(message);
                this.name = name || 'DOMException';
            }
        };
    }
}

// WebAssembly stub - provide mock that appears to work but returns placeholder values
// This allows tiktoken to "initialize" without crashing, and the SDK can use API-based
// token counting as a fallback when it detects the tokenizer isn't accurate.
if (typeof globalThis.WebAssembly === 'undefined') {
    if (globalThis._edgebox_debug && typeof print === 'function') {
        print('[WASM] Providing mock WebAssembly stub');
    }

    // Memory/Table/Global constructors that work
    const MockMemory = function(desc) {
        this.buffer = new ArrayBuffer(desc && desc.initial ? desc.initial * 65536 : 65536);
        this.grow = function(pages) { return 0; };
    };
    const MockTable = function(desc) {
        this.length = desc && desc.initial ? desc.initial : 0;
        this.get = function(idx) { return null; };
        this.set = function(idx, val) {};
        this.grow = function(delta) { return this.length; };
    };
    const MockGlobal = function(desc, val) {
        this.value = val !== undefined ? val : 0;
    };

    // Mock Module that returns empty exports structure
    function MockModule(bytes) {
        this._bytes = bytes;
    }
    MockModule.imports = function(mod) { return []; };
    MockModule.exports = function(mod) { return []; };
    MockModule.customSections = function(mod, name) { return []; };

    // Mock Instance with proxy exports that return placeholder values
    function MockInstance(module, imports) {
        this.module = module;
        // For tree-sitter: track if initialization is complete to return appropriate values
        let _initialized = false;
        // Create proxy that returns stub functions for any accessed export
        this.exports = new Proxy({}, {
            get: function(target, prop) {
                try {
                    // Don't spam logs for common properties
                    if (prop !== 'then' && prop !== 'memory' && typeof print === 'function') {
                        print('[WASM] export accessed: ' + String(prop));
                    }
                    if (prop === 'memory' || prop === 'E') {
                        // Return a memory object with a proper buffer
                        // E is often the memory export in Emscripten
                        if (!target.memory) {
                            target.memory = new MockMemory({initial: 256});
                            print('[WASM] created memory for ' + prop + ', buffer size: ' + target.memory.buffer.byteLength);
                        }
                        return target.memory;
                    }
                    if (prop === '__wasm_call_ctors' || prop === '__initialize' || prop === '_initialize' || prop === 'F') {
                        // Initialization function - mark as initialized and return
                        return function() {
                            if (typeof print === 'function') print('[WASM] init function called: ' + String(prop));
                            _initialized = true;
                            return 0;
                        };
                    }
                    // For tiktoken/tree-sitter specific exports, return sensible values
                    if (prop === 'malloc' || prop === '_malloc') {
                        return function(size) {
                            if (typeof print === 'function') print('[WASM] malloc(' + size + ')');
                            // Return a fake pointer (just a number)
                            return 1024;  // Return non-zero to indicate success
                        };
                    }
                    if (prop === 'free' || prop === '_free' || prop === 'F') {
                        return function() { return 0; };  // free is void
                    }
                    // Return a function that returns 0 for any other export
                    return function() {
                        if (typeof print === 'function') print('[WASM] export function called: ' + String(prop));
                        return 0;
                    };
                } catch(e) {
                    print('[WASM PROXY ERROR] ' + String(prop) + ': ' + e.message);
                    print('[WASM PROXY ERROR STACK] ' + e.stack);
                    throw e;
                }
            }
        });
    }

    // Error class for WebAssembly errors
    const WasmError = class extends Error {
        constructor(msg) { super(msg); this.name = 'WebAssemblyNotSupported'; }
    };

    globalThis.WebAssembly = {
        validate: function(bytes) { return true; },  // Return true to let code continue
        compile: function(bytes) {
            if (typeof print === 'function') print('[WASM] compile called - returning mock module');
            return Promise.resolve(new MockModule(bytes));
        },
        instantiate: function(bytes, imports) {
            if (typeof print === 'function') print('[WASM] instantiate called, bytes type: ' + (bytes ? bytes.constructor.name : 'null') + ', size: ' + (bytes && bytes.byteLength ? bytes.byteLength : 'N/A'));
            // Return mock module and instance that "work" but return placeholder values
            const module = new MockModule(bytes);
            const instance = new MockInstance(module, imports);
            // instantiate returns either {module, instance} or just instance depending on input
            if (bytes instanceof MockModule) {
                return Promise.resolve(instance);
            }
            return Promise.resolve({ module: module, instance: instance });
        },
        compileStreaming: function(source) {
            if (typeof print === 'function') print('[WASM] compileStreaming called - returning mock module');
            return Promise.resolve(new MockModule(null));
        },
        instantiateStreaming: function(source, imports) {
            if (typeof print === 'function') print('[WASM] instantiateStreaming called - returning mock');
            const module = new MockModule(null);
            const instance = new MockInstance(module, imports);
            return Promise.resolve({ module: module, instance: instance });
        },
        Module: MockModule,
        Instance: MockInstance,
        Memory: MockMemory,
        Table: MockTable,
        Global: MockGlobal,
        // Error classes
        CompileError: class extends Error { constructor(m) { super(m); this.name = 'CompileError'; } },
        LinkError: class extends Error { constructor(m) { super(m); this.name = 'LinkError'; } },
        RuntimeError: class extends Error { constructor(m) { super(m); this.name = 'RuntimeError'; } }
    };
}

// Ensure console has all methods (QuickJS console is incomplete)
(function() {
    if (typeof console !== 'undefined' && typeof print === 'function') {
        // Add missing console methods
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

// CRITICAL: Hook into QuickJS's promise rejection tracking
// QuickJS uses std.setPromiseRejectionCallback to handle rejections
// Without this, js_std_loop will print "Possibly unhandled promise rejection" and exit(1) in a loop
(function() {
    // Track handled rejections to prevent QuickJS from reporting them
    const handledRejections = new WeakSet();

    // Override Promise.prototype.catch to mark promises as handled
    const origCatch = Promise.prototype.catch;
    Promise.prototype.catch = function(onRejected) {
        handledRejections.add(this);
        return origCatch.call(this, onRejected);
    };

    // Override Promise.prototype.then to mark promises with rejection handlers as handled
    const origThen = Promise.prototype.then;
    Promise.prototype.then = function(onFulfilled, onRejected) {
        if (onRejected) {
            handledRejections.add(this);
        }
        return origThen.call(this, onFulfilled, onRejected);
    };

    // Global unhandled rejection handler for browsers/Node-like environments
    globalThis.onunhandledrejection = function(event) {
        if (event && event.preventDefault) {
            event.preventDefault();
        }
        // Silently track but don't spam console
        if (globalThis._edgebox_debug) {
            globalThis._edgebox_reportError('UNHANDLED_REJECTION', event && event.reason);
        }
    };
})();

// Node.js global aliases
globalThis.global = globalThis;
globalThis.self = globalThis;

// Console polyfill - preserve native methods, only add fallbacks
// Native console (from console.zig) provides: log, error, warn, info, debug, assert, time, timeEnd, timeLog, count, countReset
// We only add fallbacks for methods that don't exist
{
    // Use __native_print captured at file start to avoid any shadowing issues
    const _existingConsole = globalThis.console || {};
    const consoleImpl = {
        // Preserve native methods if they exist, otherwise use fallbacks
        log: _existingConsole.log || ((...args) => __native_print(...args)),
        error: _existingConsole.error || ((...args) => __native_print('ERROR:', ...args)),
        warn: _existingConsole.warn || ((...args) => __native_print('WARN:', ...args)),
        info: _existingConsole.info || ((...args) => __native_print('INFO:', ...args)),
        debug: _existingConsole.debug || ((...args) => __native_print('DEBUG:', ...args)),
        trace: _existingConsole.trace || ((...args) => __native_print('TRACE:', ...args)),
        dir: _existingConsole.dir || ((obj) => __native_print(JSON.stringify(obj, null, 2))),
        table: _existingConsole.table || ((data) => __native_print(JSON.stringify(data, null, 2))),
        assert: _existingConsole.assert || ((cond, ...args) => { if (!cond) __native_print('ASSERTION FAILED:', ...args); }),
        // Native timing/counting from console.zig
        time: _existingConsole.time || (() => {}),
        timeEnd: _existingConsole.timeEnd || (() => {}),
        timeLog: _existingConsole.timeLog || (() => {}),
        count: _existingConsole.count || (() => {}),
        countReset: _existingConsole.countReset || (() => {}),
        // Grouping (no native implementation yet)
        group: _existingConsole.group || (() => {}),
        groupEnd: _existingConsole.groupEnd || (() => {}),
        groupCollapsed: _existingConsole.groupCollapsed || (() => {}),
        clear: _existingConsole.clear || (() => {}),
    };

    // Make console non-writable and non-configurable to prevent overrides
    Object.defineProperty(globalThis, 'console', {
        value: consoleImpl,
        writable: false,
        configurable: false,
        enumerable: true
    });
}

// Timer polyfills using QuickJS os.setTimeout for proper event loop integration
(function() {
    // Check if QuickJS's os module is available (provides real timers)
    // Note: In static builds, os is available as globalThis._os (via importWizerStdModules)
    // In dynamic builds, it may be just `os` in global scope
    const _os = (typeof globalThis._os !== 'undefined' && globalThis._os)
              || (typeof os !== 'undefined' ? os : null);
    let _timerId = 1;
    const _timers = new Map();

    // Create a Timeout object like Node.js returns
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
        // Use QuickJS native timers - integrates with js_std_loop
        // print('[TIMER] Using _os.setTimeout for proper event loop integration');
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
            if (timer == null) return; // Handle null/undefined gracefully
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
            if (timer == null) return; // Handle null/undefined gracefully
            const id = typeof timer === 'object' ? timer._id : timer;
            const handle = _timers.get(id);
            if (handle !== undefined) {
                _os.clearTimeout(handle);
                _timers.delete(id);
            }
        };
    } else {
        // Fallback for environments without os.setTimeout
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
            if (timer == null) return; // Handle null/undefined gracefully
            const id = typeof timer === 'object' ? timer._id : timer;
            _timers.delete(id);
        };
        globalThis.setInterval = function(callback, delay = 0, ...args) {
            const id = _timerId++;
            return new Timeout(id, callback, delay, args, true);
        };
        globalThis.clearInterval = function(timer) {
            if (timer == null) return; // Handle null/undefined gracefully
            const id = typeof timer === 'object' ? timer._id : timer;
            _timers.delete(id);
        };
    }

    globalThis.setImmediate = function(callback, ...args) { return setTimeout(callback, 0, ...args); };
    globalThis.clearImmediate = function(id) { clearTimeout(id); };

    // DEBUG: Keepalive disabled - was keeping event loop alive forever
    // if (typeof globalThis._edgeboxStartKeepalive === 'function') {
    //     setTimeout(function() {
    //         globalThis._edgeboxStartKeepalive();
    //     }, 0);
    // }
})();

// TextEncoder/TextDecoder polyfills
// Check both existence AND that it's actually constructible (native binding may be broken)
var _needTextEncoderPolyfill = typeof TextEncoder === 'undefined';
if (!_needTextEncoderPolyfill) {
    try { new TextEncoder(); } catch(e) { _needTextEncoderPolyfill = true; }
}
if (_needTextEncoderPolyfill) {
    // Use native UTF-8 encoding when available (2400x faster)
    const _nativeFromUtf8 = typeof _modules !== 'undefined' && _modules._nativeBuffer && _modules._nativeBuffer.fromUtf8String;
    globalThis.TextEncoder = class TextEncoder {
        constructor(encoding = 'utf-8') { this.encoding = encoding; }
        encode(str) {
            // Fast path: use native Zig implementation
            if (_nativeFromUtf8) return _nativeFromUtf8(str);
            // Fallback: JS implementation
            const bytes = [];
            for (let i = 0; i < str.length; i++) {
                let code = str.charCodeAt(i);
                if (code < 0x80) bytes.push(code);
                else if (code < 0x800) { bytes.push(0xC0 | (code >> 6)); bytes.push(0x80 | (code & 0x3F)); }
                else if (code >= 0xD800 && code <= 0xDBFF && i + 1 < str.length) {
                    const next = str.charCodeAt(++i);
                    if (next >= 0xDC00 && next <= 0xDFFF) {
                        code = ((code - 0xD800) << 10) + (next - 0xDC00) + 0x10000;
                        bytes.push(0xF0 | (code >> 18)); bytes.push(0x80 | ((code >> 12) & 0x3F));
                        bytes.push(0x80 | ((code >> 6) & 0x3F)); bytes.push(0x80 | (code & 0x3F));
                    }
                } else { bytes.push(0xE0 | (code >> 12)); bytes.push(0x80 | ((code >> 6) & 0x3F)); bytes.push(0x80 | (code & 0x3F)); }
            }
            return new Uint8Array(bytes);
        }
    };
}

var _needTextDecoderPolyfill = typeof TextDecoder === 'undefined';
if (!_needTextDecoderPolyfill) {
    try { new TextDecoder(); } catch(e) { _needTextDecoderPolyfill = true; }
}
if (_needTextDecoderPolyfill) {
    // Use native UTF-8 decoding when available (2400x faster)
    const _nativeToUtf8 = typeof _modules !== 'undefined' && _modules._nativeBuffer && _modules._nativeBuffer.toUtf8String;
    globalThis.TextDecoder = class TextDecoder {
        constructor(encoding = 'utf-8') { this.encoding = encoding; }
        decode(input) {
            const bytes = input instanceof Uint8Array ? input : new Uint8Array(input);
            // Fast path: use native Zig implementation
            if (_nativeToUtf8) return _nativeToUtf8(bytes);
            // Fallback: JS implementation
            let str = '';
            for (let i = 0; i < bytes.length; ) {
                const b = bytes[i++];
                if (b < 0x80) str += String.fromCharCode(b);
                else if (b < 0xE0) str += String.fromCharCode(((b & 0x1F) << 6) | (bytes[i++] & 0x3F));
                else if (b < 0xF0) str += String.fromCharCode(((b & 0x0F) << 12) | ((bytes[i++] & 0x3F) << 6) | (bytes[i++] & 0x3F));
                else {
                    const code = ((b & 0x07) << 18) | ((bytes[i++] & 0x3F) << 12) | ((bytes[i++] & 0x3F) << 6) | (bytes[i++] & 0x3F);
                    if (code > 0xFFFF) { const offset = code - 0x10000; str += String.fromCharCode(0xD800 + (offset >> 10), 0xDC00 + (offset & 0x3FF)); }
                    else str += String.fromCharCode(code);
                }
            }
            return str;
        }
    };
}

// DOMException polyfill
if (typeof DOMException === 'undefined') {
    globalThis.DOMException = class DOMException extends Error {
        constructor(message, name) { super(message); this.name = name || 'Error'; }
    };
}

// ===== WEB STREAMS API POLYFILL =====
// Enhanced ReadableStream/WritableStream/TransformStream implementation
if (typeof ReadableStream === 'undefined') {
    // ReadableStreamDefaultReader with proper state management
    class ReadableStreamDefaultReader {
        constructor(stream) {
            if (stream._locked) {
                throw new TypeError('ReadableStream is locked');
            }
            this._stream = stream;
            this._stream._locked = true;
            this._stream._reader = this;
            this._closedPromise = new Promise((resolve, reject) => {
                this._closedResolve = resolve;
                this._closedReject = reject;
            });
        }
        read() {
            if (!this._stream) {
                return Promise.reject(new TypeError('Reader has been released'));
            }
            return this._stream._read();
        }
        releaseLock() {
            if (this._stream) {
                this._stream._locked = false;
                this._stream._reader = null;
                this._stream = null;
            }
        }
        get closed() {
            return this._closedPromise;
        }
        cancel(reason) {
            if (!this._stream) {
                return Promise.reject(new TypeError('Reader has been released'));
            }
            return this._stream.cancel(reason);
        }
    }

    globalThis.ReadableStream = class ReadableStream {
        constructor(underlyingSource = {}, strategy = {}) {
            this._source = underlyingSource;
            this._queue = [];
            this._closed = false;
            this._locked = false;
            this._reader = null;
            this._error = null;
            this._pullPending = false;
            this._started = false;

            const self = this;
            this._controller = {
                desiredSize: strategy.highWaterMark || 1,
                enqueue: (chunk) => {
                    if (self._closed) return;
                    self._queue.push(chunk);
                    self._resolveWaitingReaders();
                },
                close: () => {
                    self._closed = true;
                    self._resolveWaitingReaders();
                    if (self._reader && self._reader._closedResolve) {
                        self._reader._closedResolve();
                    }
                },
                error: (e) => {
                    self._error = e;
                    self._closed = true;
                    self._resolveWaitingReaders();
                    if (self._reader && self._reader._closedReject) {
                        self._reader._closedReject(e);
                    }
                }
            };

            this._waitingReaders = [];

            // Call start asynchronously via setTimeout (not microtask)
            if (underlyingSource.start) {
                setTimeout(() => {
                    try {
                        const result = underlyingSource.start(this._controller);
                        if (result && typeof result.then === 'function') {
                            result.then(() => { this._started = true; }).catch(e => this._controller.error(e));
                        } else {
                            this._started = true;
                        }
                    } catch (e) {
                        this._controller.error(e);
                    }
                });
            } else {
                this._started = true;
            }
        }

        _resolveWaitingReaders() {
            while (this._waitingReaders.length > 0 && (this._queue.length > 0 || this._closed || this._error)) {
                const { resolve, reject } = this._waitingReaders.shift();
                if (this._error) {
                    reject(this._error);
                } else if (this._queue.length > 0) {
                    resolve({ value: this._queue.shift(), done: false });
                } else {
                    resolve({ value: undefined, done: true });
                }
            }
        }

        getReader(options) {
            if (options && options.mode === 'byob') {
                throw new TypeError('BYOB readers not supported');
            }
            return new ReadableStreamDefaultReader(this);
        }

        _read() {
            if (this._error) return Promise.reject(this._error);
            if (this._queue.length > 0) {
                return Promise.resolve({ value: this._queue.shift(), done: false });
            }
            if (this._closed) {
                return Promise.resolve({ value: undefined, done: true });
            }

            // If source has pull, call it
            if (this._source.pull && !this._pullPending) {
                this._pullPending = true;
                const pullPromise = new Promise((resolve, reject) => {
                    this._waitingReaders.push({ resolve, reject });
                });

                setTimeout(() => {
                    try {
                        const result = this._source.pull(this._controller);
                        this._pullPending = false;
                        if (result && typeof result.then === 'function') {
                            result.then(() => {
                                this._resolveWaitingReaders();
                            }).catch(e => {
                                this._controller.error(e);
                            });
                        } else {
                            this._resolveWaitingReaders();
                        }
                    } catch (e) {
                        this._pullPending = false;
                        this._controller.error(e);
                    }
                });

                return pullPromise;
            }

            // Wait for data
            return new Promise((resolve, reject) => {
                this._waitingReaders.push({ resolve, reject });
                // Set a timeout to prevent hanging forever
                setTimeout(() => {
                    const idx = this._waitingReaders.findIndex(r => r.resolve === resolve);
                    if (idx >= 0) {
                        this._waitingReaders.splice(idx, 1);
                        resolve({ value: undefined, done: true });
                    }
                }, 30000);
            });
        }

        pipeThrough(transform, options) {
            const reader = this.getReader();
            const writer = transform.writable.getWriter();

            (async () => {
                try {
                    while (true) {
                        const { done, value } = await reader.read();
                        if (done) {
                            await writer.close();
                            break;
                        }
                        await writer.write(value);
                    }
                } catch (e) {
                    await writer.abort(e);
                } finally {
                    reader.releaseLock();
                    writer.releaseLock();
                }
            })();

            return transform.readable;
        }

        async pipeTo(dest, options = {}) {
            const reader = this.getReader();
            const writer = dest.getWriter();

            try {
                while (true) {
                    const { done, value } = await reader.read();
                    if (done) {
                        if (!options.preventClose) await writer.close();
                        break;
                    }
                    await writer.write(value);
                }
            } catch (e) {
                if (!options.preventAbort) await writer.abort(e);
                throw e;
            } finally {
                reader.releaseLock();
                writer.releaseLock();
            }
        }

        tee() {
            const reader = this.getReader();
            const queue1 = [];
            const queue2 = [];
            let closed = false;

            const pullFromSource = async () => {
                try {
                    const { done, value } = await reader.read();
                    if (done) {
                        closed = true;
                        return;
                    }
                    queue1.push(value);
                    queue2.push(value);
                } catch (e) {
                    closed = true;
                    throw e;
                }
            };

            const stream1 = new ReadableStream({
                async pull(controller) {
                    if (queue1.length > 0) {
                        controller.enqueue(queue1.shift());
                    } else if (closed) {
                        controller.close();
                    } else {
                        await pullFromSource();
                        if (queue1.length > 0) {
                            controller.enqueue(queue1.shift());
                        } else {
                            controller.close();
                        }
                    }
                }
            });

            const stream2 = new ReadableStream({
                async pull(controller) {
                    if (queue2.length > 0) {
                        controller.enqueue(queue2.shift());
                    } else if (closed) {
                        controller.close();
                    } else {
                        await pullFromSource();
                        if (queue2.length > 0) {
                            controller.enqueue(queue2.shift());
                        } else {
                            controller.close();
                        }
                    }
                }
            });

            return [stream1, stream2];
        }

        cancel(reason) {
            this._closed = true;
            if (this._source.cancel) {
                return Promise.resolve(this._source.cancel(reason));
            }
            return Promise.resolve();
        }

        get locked() {
            return this._locked;
        }

        [Symbol.asyncIterator]() {
            const reader = this.getReader();
            return {
                async next() {
                    return reader.read();
                },
                async return() {
                    reader.releaseLock();
                    return { done: true, value: undefined };
                }
            };
        }
    };
}

if (typeof WritableStream === 'undefined') {
    class WritableStreamDefaultWriter {
        constructor(stream) {
            this._stream = stream;
            this._closed = false;
        }
        write(chunk) {
            return this._stream._write(chunk);
        }
        close() {
            this._closed = true;
            return Promise.resolve();
        }
        abort(reason) {
            return Promise.resolve();
        }
        releaseLock() {}
        get closed() {
            return Promise.resolve(this._closed);
        }
        get ready() {
            return Promise.resolve();
        }
        get desiredSize() {
            return 1;
        }
    }

    globalThis.WritableStream = class WritableStream {
        constructor(underlyingSink = {}, strategy = {}) {
            this._sink = underlyingSink;
            this._closed = false;
            if (underlyingSink.start) {
                try { underlyingSink.start({ error: () => {} }); }
                catch (e) {}
            }
        }
        getWriter() {
            return new WritableStreamDefaultWriter(this);
        }
        _write(chunk) {
            if (this._sink.write) {
                return Promise.resolve(this._sink.write(chunk, { error: () => {} }));
            }
            return Promise.resolve();
        }
        close() {
            this._closed = true;
            if (this._sink.close) {
                return Promise.resolve(this._sink.close());
            }
            return Promise.resolve();
        }
        abort(reason) {
            return Promise.resolve();
        }
        get locked() {
            return false;
        }
    };
}

if (typeof TransformStream === 'undefined') {
    globalThis.TransformStream = class TransformStream {
        constructor(transformer = {}, writableStrategy = {}, readableStrategy = {}) {
            this._transformer = transformer;
            const queue = [];
            let closed = false;

            this.readable = new ReadableStream({
                pull(controller) {
                    while (queue.length > 0) {
                        controller.enqueue(queue.shift());
                    }
                    if (closed) controller.close();
                }
            });

            this.writable = new WritableStream({
                write(chunk) {
                    if (transformer.transform) {
                        transformer.transform(chunk, {
                            enqueue: (c) => queue.push(c)
                        });
                    } else {
                        queue.push(chunk);
                    }
                },
                close() {
                    closed = true;
                    if (transformer.flush) {
                        transformer.flush({ enqueue: (c) => queue.push(c) });
                    }
                }
            });
        }
    };
}

// ByteLengthQueuingStrategy and CountQueuingStrategy
if (typeof ByteLengthQueuingStrategy === 'undefined') {
    globalThis.ByteLengthQueuingStrategy = class ByteLengthQueuingStrategy {
        constructor({ highWaterMark }) {
            this.highWaterMark = highWaterMark;
        }
        size(chunk) {
            return chunk.byteLength;
        }
    };
}

if (typeof CountQueuingStrategy === 'undefined') {
    globalThis.CountQueuingStrategy = class CountQueuingStrategy {
        constructor({ highWaterMark }) {
            this.highWaterMark = highWaterMark;
        }
        size() {
            return 1;
        }
    };
}

// Event polyfills
if (typeof Event === 'undefined') {
    globalThis.Event = class Event {
        constructor(type, options = {}) {
            this.type = type;
            this.bubbles = options.bubbles || false;
            this.cancelable = options.cancelable || false;
            this.composed = options.composed || false;
            this.defaultPrevented = false;
            this.target = null;
            this.currentTarget = null;
            this.timeStamp = Date.now();
        }
        preventDefault() { this.defaultPrevented = true; }
        stopPropagation() {}
        stopImmediatePropagation() {}
    };
}
if (typeof CustomEvent === 'undefined') {
    globalThis.CustomEvent = class CustomEvent extends Event {
        constructor(type, options = {}) { super(type, options); this.detail = options.detail || null; }
    };
}
if (typeof EventTarget === 'undefined') {
    globalThis.EventTarget = class EventTarget {
        constructor() { this._listeners = {}; }
        addEventListener(type, listener) { (this._listeners[type] = this._listeners[type] || []).push(listener); }
        removeEventListener(type, listener) { if (this._listeners[type]) this._listeners[type] = this._listeners[type].filter(l => l !== listener); }
        dispatchEvent(event) { event.target = this; (this._listeners[event.type] || []).forEach(l => l(event)); return !event.defaultPrevented; }
    };
}

// Crypto polyfill
globalThis.crypto = globalThis.crypto || {
    getRandomValues(array) {
        for (let i = 0; i < array.length; i++) {
            if (array instanceof Uint8Array) array[i] = Math.floor(Math.random() * 256);
            else if (array instanceof Uint16Array) array[i] = Math.floor(Math.random() * 65536);
            else if (array instanceof Uint32Array) array[i] = Math.floor(Math.random() * 4294967296);
            else array[i] = Math.floor(Math.random() * 256);
        }
        return array;
    },
    randomUUID() {
        const bytes = new Uint8Array(16);
        crypto.getRandomValues(bytes);
        bytes[6] = (bytes[6] & 0x0f) | 0x40;
        bytes[8] = (bytes[8] & 0x3f) | 0x80;
        const hex = Array.from(bytes, b => b.toString(16).padStart(2, '0')).join('');
        return hex.slice(0,8) + '-' + hex.slice(8,12) + '-' + hex.slice(12,16) + '-' + hex.slice(16,20) + '-' + hex.slice(20);
    },
};

// Performance polyfill
globalThis.performance = globalThis.performance || { now: () => Date.now(), mark: () => {}, measure: () => {}, getEntriesByType: () => [] };

// Intl polyfill (minimal)
if (typeof Intl === 'undefined') {
    globalThis.Intl = {
        DateTimeFormat: class DateTimeFormat { constructor(locale, opts) { this.locale = locale; this.opts = opts; } format(date) { return date.toISOString(); } formatToParts(date) { return [{ type: 'literal', value: date.toISOString() }]; } resolvedOptions() { return this.opts || {}; } },
        NumberFormat: class NumberFormat { constructor(locale, opts) { this.locale = locale; this.opts = opts; } format(num) { return String(num); } formatToParts(num) { return [{ type: 'integer', value: String(num) }]; } resolvedOptions() { return this.opts || {}; } },
        Collator: class Collator { constructor(locale, opts) { this.locale = locale; } compare(a, b) { return a < b ? -1 : a > b ? 1 : 0; } resolvedOptions() { return {}; } },
        PluralRules: class PluralRules { constructor(locale, opts) { this.locale = locale; } select(n) { return n === 1 ? 'one' : 'other'; } resolvedOptions() { return {}; } },
        RelativeTimeFormat: class RelativeTimeFormat { constructor(locale, opts) { this.locale = locale; } format(value, unit) { return value + ' ' + unit + (Math.abs(value) !== 1 ? 's' : '') + (value < 0 ? ' ago' : ''); } formatToParts(value, unit) { return [{ type: 'literal', value: this.format(value, unit) }]; } resolvedOptions() { return {}; } },
        ListFormat: class ListFormat { constructor(locale, opts) { this.locale = locale; } format(list) { return list.join(', '); } formatToParts(list) { return list.map(v => ({ type: 'element', value: v })); } resolvedOptions() { return {}; } },
        Segmenter: class Segmenter { constructor(locale, opts) { this.locale = locale; this.granularity = opts?.granularity || 'grapheme'; } segment(str) { return { [Symbol.iterator]: function*() { for (let i = 0; i < str.length; i++) yield { segment: str[i], index: i }; } }; } resolvedOptions() { return { granularity: this.granularity }; } },
        getCanonicalLocales: (locales) => Array.isArray(locales) ? locales : [locales],
        supportedValuesOf: (key) => []
    };
}

// URLSearchParams polyfill
if (typeof URLSearchParams === 'undefined') {
    globalThis.URLSearchParams = class URLSearchParams {
        constructor(init) {
            this._params = [];
            if (typeof init === 'string') {
                const query = init.startsWith('?') ? init.slice(1) : init;
                query.split('&').forEach(pair => {
                    const [k, v] = pair.split('=').map(decodeURIComponent);
                    if (k) this._params.push([k, v || '']);
                });
            } else if (init && typeof init === 'object') {
                Object.entries(init).forEach(([k, v]) => this._params.push([k, String(v)]));
            }
        }
        get(name) { const p = this._params.find(([k]) => k === name); return p ? p[1] : null; }
        getAll(name) { return this._params.filter(([k]) => k === name).map(([, v]) => v); }
        has(name) { return this._params.some(([k]) => k === name); }
        set(name, value) { this.delete(name); this._params.push([name, String(value)]); }
        append(name, value) { this._params.push([name, String(value)]); }
        delete(name) { this._params = this._params.filter(([k]) => k !== name); }
        keys() { return this._params.map(([k]) => k)[Symbol.iterator](); }
        values() { return this._params.map(([, v]) => v)[Symbol.iterator](); }
        entries() { return this._params[Symbol.iterator](); }
        forEach(fn) { this._params.forEach(([k, v]) => fn(v, k, this)); }
        toString() { return this._params.map(([k, v]) => encodeURIComponent(k) + '=' + encodeURIComponent(v)).join('&'); }
        [Symbol.iterator]() { return this.entries(); }
    };
}

// URL polyfill
if (typeof URL === 'undefined') {
    globalThis.URL = class URL {
        constructor(url, base) {
            if (base) {
                const baseUrl = new URL(base);
                if (url.startsWith('/')) url = baseUrl.origin + url;
                else if (!url.includes('://')) url = baseUrl.href.replace(/[^/]*$/, '') + url;
            }
            const match = url.match(/^([^:]+):\/\/([^/:]+)(?::(\d+))?(\/[^?#]*)?(\?[^#]*)?(#.*)?$/);
            if (!match) throw new TypeError('Invalid URL');
            this.protocol = match[1] + ':';
            this.hostname = match[2];
            this.port = match[3] || '';
            this.pathname = match[4] || '/';
            this.search = match[5] || '';
            this.hash = match[6] || '';
            this.searchParams = new URLSearchParams(this.search);
        }
        get host() { return this.hostname + (this.port ? ':' + this.port : ''); }
        get origin() { return this.protocol + '//' + this.host; }
        get href() { return this.origin + this.pathname + this.search + this.hash; }
        toString() { return this.href; }
        toJSON() { return this.href; }
    };
}

// AbortController polyfill
if (typeof AbortController === 'undefined') {
    globalThis.AbortSignal = class AbortSignal extends EventTarget {
        constructor() { super(); this.aborted = false; this.reason = undefined; }
        throwIfAborted() { if (this.aborted) throw this.reason; }
        static abort(reason) { const s = new AbortSignal(); s.aborted = true; s.reason = reason || new DOMException('Aborted', 'AbortError'); return s; }
        static timeout(ms) { const s = new AbortSignal(); setTimeout(() => { s.aborted = true; s.reason = new DOMException('Timeout', 'TimeoutError'); s.dispatchEvent(new Event('abort')); }, ms); return s; }
    };
    globalThis.AbortController = class AbortController {
        constructor() { this.signal = new AbortSignal(); }
        abort(reason) { this.signal.aborted = true; this.signal.reason = reason || new DOMException('Aborted', 'AbortError'); this.signal.dispatchEvent(new Event('abort')); }
    };
}

// FormData polyfill (for API requests that use multipart/form-data)
if (typeof FormData === 'undefined') {
    globalThis.FormData = class FormData {
        constructor() { this._entries = []; }
        append(name, value, filename) { this._entries.push([String(name), value, filename]); }
        delete(name) { this._entries = this._entries.filter(([n]) => n !== name); }
        get(name) { const e = this._entries.find(([n]) => n === name); return e ? e[1] : null; }
        getAll(name) { return this._entries.filter(([n]) => n === name).map(e => e[1]); }
        has(name) { return this._entries.some(([n]) => n === name); }
        set(name, value, filename) { this.delete(name); this.append(name, value, filename); }
        keys() { return this._entries.map(e => e[0])[Symbol.iterator](); }
        values() { return this._entries.map(e => e[1])[Symbol.iterator](); }
        entries() { return this._entries.map(e => [e[0], e[1]])[Symbol.iterator](); }
        forEach(cb, thisArg) { this._entries.forEach(([n, v]) => cb.call(thisArg, v, n, this)); }
        [Symbol.iterator]() { return this.entries(); }
    };
}

// Headers class for fetch API
if (typeof Headers === 'undefined') {
    globalThis.Headers = class Headers {
        constructor(init) {
            this._headers = {};
            if (init instanceof Headers) {
                init.forEach((value, key) => this.set(key, value));
            } else if (typeof init === 'object' && init !== null) {
                Object.entries(init).forEach(([key, value]) => this.set(key, value));
            }
        }
        get(name) { return this._headers[name.toLowerCase()] || null; }
        set(name, value) { this._headers[name.toLowerCase()] = String(value); }
        has(name) { return name.toLowerCase() in this._headers; }
        delete(name) { delete this._headers[name.toLowerCase()]; }
        append(name, value) {
            const key = name.toLowerCase();
            if (this._headers[key]) this._headers[key] += ', ' + value;
            else this._headers[key] = String(value);
        }
        forEach(callback) { Object.entries(this._headers).forEach(([k, v]) => callback(v, k, this)); }
        entries() { return Object.entries(this._headers)[Symbol.iterator](); }
        keys() { return Object.keys(this._headers)[Symbol.iterator](); }
        values() { return Object.values(this._headers)[Symbol.iterator](); }
        [Symbol.iterator]() { return this.entries(); }
        normalize() { return this; }
        toJSON() { return { ...this._headers }; }
    };
}

// Request class for fetch API (required by Axios)
if (typeof Request === 'undefined') {
    globalThis.Request = class Request {
        constructor(input, init = {}) {
            if (input instanceof Request) {
                this.url = input.url;
                this.method = init.method || input.method || 'GET';
                this.headers = new Headers(init.headers || input.headers);
                this.body = init.body !== undefined ? init.body : input.body;
            } else {
                this.url = String(input);
                this.method = init.method || 'GET';
                this.headers = new Headers(init.headers);
                this.body = init.body || null;
            }
            this.signal = init.signal || null;
            this.credentials = init.credentials || 'same-origin';
            this.mode = init.mode || 'cors';
            this.cache = init.cache || 'default';
            this.redirect = init.redirect || 'follow';
            this.referrer = init.referrer || '';
            this.duplex = init.duplex || 'half';
        }
        clone() { return new Request(this); }
    };
}

// Response class for fetch API (required by Axios)
if (typeof Response === 'undefined') {
    globalThis.Response = class Response {
        constructor(body, init = {}) {
            this._body = body;
            this._bodyUsed = false;
            this.status = init.status || 200;
            this.statusText = init.statusText || 'OK';
            this.ok = this.status >= 200 && this.status < 300;
            this.headers = init.headers instanceof Headers ? init.headers : new Headers(init.headers);
            this.type = 'basic';
            this.url = init.url || '';
            this.redirected = false;
        }
        get bodyUsed() { return this._bodyUsed; }
        get body() {
            const self = this;
            // Check if this is an SSE (Server-Sent Events) response
            // Try content-type header first, then check body content format
            const contentType = this.headers && typeof this.headers.get === 'function'
                ? this.headers.get('content-type') : '';
            const headerIsSSE = contentType && contentType.includes('text/event-stream');
            // SSE responses start with "event:" or "data:" lines separated by double newlines
            const bodyLooksLikeSSE = typeof self._body === 'string' &&
                (self._body.startsWith('event:') || self._body.startsWith('data:') ||
                 self._body.includes('\n\ndata:') || self._body.includes('\n\nevent:'));
            const isSSE = headerIsSSE || bodyLooksLikeSSE;

            // For SSE responses, parse and yield individual events
            if (isSSE && typeof self._body === 'string') {
                return {
                    getReader() {
                        // Split response by SSE event delimiter (double newline)
                        const events = self._body.split('\n\n').filter(e => e.trim());
                        let index = 0;
                        return {
                            async read() {
                                if (index >= events.length) {
                                    return { done: true, value: undefined };
                                }
                                self._bodyUsed = true;
                                const eventStr = events[index++];
                                // Parse SSE format: "event: type\ndata: {...}"
                                const dataMatch = eventStr.match(/^data:\s*(.*)$/m);
                                if (dataMatch) {
                                    // Return just the data line content as a chunk
                                    const chunk = new TextEncoder().encode(dataMatch[1] + '\n');
                                    return { done: false, value: chunk };
                                }
                                // Skip non-data events (comments, empty), try next
                                return this.read();
                            },
                            releaseLock() {}
                        };
                    },
                    [Symbol.asyncIterator]() {
                        const reader = this.getReader();
                        return {
                            async next() { return reader.read(); }
                        };
                    }
                };
            }

            // For non-SSE responses, return body as single chunk (existing behavior)
            return {
                getReader() {
                    let done = false;
                    return {
                        async read() {
                            if (done) return { done: true, value: undefined };
                            done = true;
                            self._bodyUsed = true;
                            const data = typeof self._body === 'string'
                                ? new TextEncoder().encode(self._body)
                                : self._body;
                            return { done: false, value: data };
                        },
                        releaseLock() {}
                    };
                },
                [Symbol.asyncIterator]() {
                    const reader = this.getReader();
                    return {
                        async next() { return reader.read(); }
                    };
                }
            };
        }
        async text() { this._bodyUsed = true; return typeof this._body === 'string' ? this._body : new TextDecoder().decode(this._body); }
        async json() { return JSON.parse(await this.text()); }
        async arrayBuffer() {
            this._bodyUsed = true;
            if (this._body instanceof ArrayBuffer) return this._body;
            if (typeof this._body === 'string') return new TextEncoder().encode(this._body).buffer;
            return this._body.buffer || this._body;
        }
        async blob() { return new Blob([await this.arrayBuffer()]); }
        clone() { return new Response(this._body, { status: this.status, statusText: this.statusText, headers: this.headers }); }
    };
}

// TextEncoder/TextDecoder should already exist, but ensure they're available
if (typeof TextEncoder === 'undefined') {
    globalThis.TextEncoder = class TextEncoder {
        encode(str) { return new Uint8Array([...str].map(c => c.charCodeAt(0))); }
    };
}

// Fetch polyfill using native binding - ALWAYS override to ensure we control HTTP
// Bundled node-fetch won't work in WASM, so we force our implementation
{
    const hasSyncApi = typeof globalThis.__edgebox_fetch === 'function';
    if (globalThis._edgebox_debug) print('[FETCH POLYFILL] hasSyncApi=' + hasSyncApi);

    if (hasSyncApi) {
        const _edgebox_fetch = async function(input, options = {}) {
            // Handle Request object as first argument
            let url, method, headers, body;
            if (input instanceof Request) {
                url = input.url;
                method = options.method || input.method || 'GET';
                headers = options.headers || input.headers || {};
                body = options.body !== undefined ? options.body : input.body;
            } else {
                url = typeof input === 'string' ? input : String(input);
                method = options.method || 'GET';
                headers = options.headers || {};
                body = options.body || null;
            }

            // Convert Headers object to plain object
            let headersObj = headers;
            if (headers instanceof Headers) {
                headersObj = {};
                headers.forEach((value, key) => { headersObj[key] = value; });
            } else if (headers && typeof headers.toJSON === 'function') {
                headersObj = headers.toJSON();
            }

            // Ensure body is a string
            let bodyStr = null;
            if (body !== null && body !== undefined) {
                if (typeof body === 'string') {
                    bodyStr = body;
                } else if (body instanceof ArrayBuffer) {
                    bodyStr = new TextDecoder().decode(body);
                } else if (ArrayBuffer.isView(body)) {
                    bodyStr = new TextDecoder().decode(body);
                } else if (typeof body === 'object') {
                    bodyStr = JSON.stringify(body);
                } else {
                    bodyStr = String(body);
                }
            }

            // Handle data: URLs in JavaScript (native fetch only handles http/https)
            if (url.startsWith('data:')) {
                try {
                    // Parse data URL: data:[<mediatype>][;base64],<data>
                    const commaIndex = url.indexOf(',');
                    if (commaIndex === -1) throw new Error('Invalid data URL');
                    const meta = url.slice(5, commaIndex); // skip 'data:'
                    const dataStr = url.slice(commaIndex + 1);
                    const isBase64 = meta.includes(';base64');
                    const mimeType = meta.replace(';base64', '') || 'text/plain';

                    let bodyBytes;
                    if (isBase64) {
                        // Decode base64
                        const binaryStr = atob(dataStr);
                        bodyBytes = new Uint8Array(binaryStr.length);
                        for (let i = 0; i < binaryStr.length; i++) {
                            bodyBytes[i] = binaryStr.charCodeAt(i);
                        }
                    } else {
                        // URL-decode the data
                        bodyBytes = new TextEncoder().encode(decodeURIComponent(dataStr));
                    }

                    const responseHeaders = new Headers({ 'content-type': mimeType });
                    return new Response(bodyBytes, {
                        status: 200,
                        statusText: 'OK',
                        headers: responseHeaders,
                        url: url
                    });
                } catch (e) {
                    throw e;
                }
            }

            // Use synchronous fetch - async polling with setTimeout causes deadlock in WASM
            // The sync API blocks until the HTTP request completes, which is fine for CLI tools
            const result = globalThis.__edgebox_fetch(url, method, JSON.stringify(headersObj), bodyStr);

            const responseHeaders = new Headers(result.headers || {});
            return new Response(result.body, {
                status: result.status,
                statusText: result.ok ? 'OK' : 'Error',
                headers: responseHeaders,
                url: url
            });
        };

        // Set on globalThis AND as a global variable (some bundlers check `typeof fetch`)
        globalThis.fetch = _edgebox_fetch;
        // Also try to make it available without globalThis prefix
        try {
            if (typeof global !== 'undefined') global.fetch = _edgebox_fetch;
        } catch(e) {}
        if (globalThis._edgebox_debug) print('[FETCH POLYFILL] fetch installed on globalThis');
    }
}

// Buffer polyfill (minimal)
if (typeof Buffer === 'undefined') {
    globalThis.Buffer = class Buffer extends Uint8Array {
        constructor(arg, encodingOrOffset, length) {
            if (typeof arg === 'number') { super(arg); }
            else if (typeof arg === 'string') { super(new TextEncoder().encode(arg)); }
            else if (arg instanceof ArrayBuffer) { super(arg, encodingOrOffset, length); }
            else if (ArrayBuffer.isView(arg)) { super(arg.buffer, arg.byteOffset, arg.byteLength); }
            else if (Array.isArray(arg)) { super(arg); }
            else { super(0); }
        }
        static from(value, encodingOrOffset, length) {
            if (typeof value === 'string') return new Buffer(new TextEncoder().encode(value));
            if (value instanceof ArrayBuffer) return new Buffer(value, encodingOrOffset, length);
            if (ArrayBuffer.isView(value)) return new Buffer(value.buffer, value.byteOffset, value.byteLength);
            if (Array.isArray(value)) return new Buffer(value);
            return new Buffer(value);
        }
        static alloc(size, fill) { const buf = new Buffer(size); if (fill !== undefined) buf.fill(fill); return buf; }
        static allocUnsafe(size) { return new Buffer(size); }
        static allocUnsafeSlow(size) { return new Buffer(size); }
        static isBuffer(obj) { return obj instanceof Buffer || obj instanceof Uint8Array; }
        static concat(list, totalLength) {
            if (list.length === 0) return Buffer.alloc(0);
            totalLength = totalLength ?? list.reduce((acc, buf) => acc + buf.length, 0);
            const result = Buffer.alloc(totalLength);
            let offset = 0;
            for (const buf of list) { result.set(buf, offset); offset += buf.length; }
            return result;
        }
        toString(encoding = 'utf8') { return new TextDecoder(encoding === 'utf8' ? 'utf-8' : encoding).decode(this); }
        slice(start, end) { return new Buffer(super.slice(start, end)); }
        write(string, offset = 0, length, encoding = 'utf8') {
            const bytes = new TextEncoder().encode(string);
            const len = Math.min(bytes.length, length ?? this.length - offset);
            this.set(bytes.subarray(0, len), offset);
            return len;
        }
        copy(target, targetStart = 0, sourceStart = 0, sourceEnd = this.length) {
            const slice = this.subarray(sourceStart, sourceEnd);
            target.set(slice, targetStart);
            return slice.length;
        }
        equals(other) {
            if (this.length !== other.length) return false;
            for (let i = 0; i < this.length; i++) if (this[i] !== other[i]) return false;
            return true;
        }
        compare(other) {
            const len = Math.min(this.length, other.length);
            for (let i = 0; i < len; i++) {
                if (this[i] < other[i]) return -1;
                if (this[i] > other[i]) return 1;
            }
            return this.length - other.length;
        }
        indexOf(value, byteOffset = 0) {
            if (typeof value === 'string') value = new TextEncoder().encode(value);
            if (typeof value === 'number') {
                for (let i = byteOffset; i < this.length; i++) if (this[i] === value) return i;
                return -1;
            }
            outer: for (let i = byteOffset; i <= this.length - value.length; i++) {
                for (let j = 0; j < value.length; j++) if (this[i + j] !== value[j]) continue outer;
                return i;
            }
            return -1;
        }
        includes(value, byteOffset = 0) { return this.indexOf(value, byteOffset) !== -1; }
        static byteLength(str, encoding = 'utf8') { return new TextEncoder().encode(str).length; }
        toJSON() { return { type: 'Buffer', data: Array.from(this) }; }
        readInt32LE(offset = 0) { return new DataView(this.buffer, this.byteOffset, this.byteLength).getInt32(offset, true); }
        readInt32BE(offset = 0) { return new DataView(this.buffer, this.byteOffset, this.byteLength).getInt32(offset, false); }
        writeInt32LE(value, offset = 0) { new DataView(this.buffer, this.byteOffset, this.byteLength).setInt32(offset, value, true); return offset + 4; }
        writeInt32BE(value, offset = 0) { new DataView(this.buffer, this.byteOffset, this.byteLength).setInt32(offset, value, false); return offset + 4; }
        readUInt32LE(offset = 0) { return new DataView(this.buffer, this.byteOffset, this.byteLength).getUint32(offset, true); }
        readUInt32BE(offset = 0) { return new DataView(this.buffer, this.byteOffset, this.byteLength).getUint32(offset, false); }
        writeUInt32LE(value, offset = 0) { new DataView(this.buffer, this.byteOffset, this.byteLength).setUint32(offset, value, true); return offset + 4; }
        writeUInt32BE(value, offset = 0) { new DataView(this.buffer, this.byteOffset, this.byteLength).setUint32(offset, value, false); return offset + 4; }
        readUInt16BE(offset = 0) { return new DataView(this.buffer, this.byteOffset, this.byteLength).getUint16(offset, false); }
        writeUInt16BE(value, offset = 0) { new DataView(this.buffer, this.byteOffset, this.byteLength).setUint16(offset, value, false); return offset + 2; }
        readFloatLE(offset = 0) { return new DataView(this.buffer, this.byteOffset, this.byteLength).getFloat32(offset, true); }
        writeFloatLE(value, offset = 0) { new DataView(this.buffer, this.byteOffset, this.byteLength).setFloat32(offset, value, true); return offset + 4; }
        readDoubleLE(offset = 0) { return new DataView(this.buffer, this.byteOffset, this.byteLength).getFloat64(offset, true); }
        writeDoubleLE(value, offset = 0) { new DataView(this.buffer, this.byteOffset, this.byteLength).setFloat64(offset, value, true); return offset + 8; }
    };
}

// Process is fully implemented in native Zig (process.zig)
// No JS polyfill needed - native Zig provides all process functionality

// WebAssembly polyfill stub
// Some Node.js code checks for WebAssembly existence
if (typeof WebAssembly === 'undefined') {
    globalThis.WebAssembly = {
        // Indicate that WebAssembly is not actually available
        // but provide stub methods to prevent "undefined" errors
        validate: function(bytes) { return false; },
        compile: function(bytes) { return Promise.reject(new Error('WebAssembly not supported in this environment')); },
        instantiate: function(bytes, imports) { return Promise.reject(new Error('WebAssembly not supported in this environment')); },
        compileStreaming: function(source) { return Promise.reject(new Error('WebAssembly not supported')); },
        instantiateStreaming: function(source, imports) { return Promise.reject(new Error('WebAssembly not supported')); },
        Module: function(bytes) { throw new Error('WebAssembly.Module not supported'); },
        Instance: function(module, imports) { throw new Error('WebAssembly.Instance not supported'); },
        Memory: function(descriptor) { throw new Error('WebAssembly.Memory not supported'); },
        Table: function(descriptor) { throw new Error('WebAssembly.Table not supported'); },
        Global: function(descriptor, value) { throw new Error('WebAssembly.Global not supported'); },
        CompileError: class CompileError extends Error { constructor(msg) { super(msg); this.name = 'CompileError'; } },
        LinkError: class LinkError extends Error { constructor(msg) { super(msg); this.name = 'LinkError'; } },
        RuntimeError: class RuntimeError extends Error { constructor(msg) { super(msg); this.name = 'RuntimeError'; } },
    };
}

// Store original console.log to ensure it works
const _originalConsoleLog = console.log.bind(console);

// ===== HOST STDLIB CLASSES (HostArray, HostMap) =====
// Native i32 data structures backed by Zig host functions
// ~10x faster than JS arrays/maps for numeric sort operations
// Limitation: i32 values only (no strings, objects, floats)
// IMPORTANT: Must call free() when done to release host resources

if (typeof __edgebox_array_new === 'function') {
    /**
     * HostArray - Native i32 array backed by Zig host functions
     *
     * Usage:
     *   const arr = new HostArray();
     *   arr.push(42).push(17).push(99);
     *   arr.sort();
     *   print(arr.get(0)); // 17 (sorted)
     *   arr.free(); // IMPORTANT: must free when done
     */
    globalThis.HostArray = class HostArray {
        constructor() {
            this._handle = __edgebox_array_new();
            if (this._handle < 0) throw new Error('Failed to create HostArray');
        }

        push(value) {
            __edgebox_array_push(this._handle, value | 0);
            return this; // Chainable
        }

        pop() {
            return __edgebox_array_pop(this._handle);
        }

        get(index) {
            return __edgebox_array_get(this._handle, index | 0);
        }

        set(index, value) {
            __edgebox_array_set(this._handle, index | 0, value | 0);
            return this;
        }

        get length() {
            return __edgebox_array_len(this._handle);
        }

        sort() {
            __edgebox_array_sort(this._handle);
            return this;
        }

        sortDesc() {
            __edgebox_array_sort_desc(this._handle);
            return this;
        }

        reverse() {
            __edgebox_array_reverse(this._handle);
            return this;
        }

        clear() {
            __edgebox_array_clear(this._handle);
            return this;
        }

        indexOf(value) {
            return __edgebox_array_index_of(this._handle, value | 0);
        }

        free() {
            if (this._handle >= 0) {
                __edgebox_array_free(this._handle);
                this._handle = -1;
            }
        }

        // Convert to JS array (for interop)
        toArray() {
            const len = this.length;
            const arr = new Array(len);
            for (let i = 0; i < len; i++) {
                arr[i] = this.get(i);
            }
            return arr;
        }

        // Create from JS array
        static from(jsArray) {
            const arr = new HostArray();
            for (const v of jsArray) {
                arr.push(v | 0);
            }
            return arr;
        }
    };
}

if (typeof __edgebox_map_new === 'function') {
    /**
     * HostMap - Native string->i32 map backed by Zig host functions
     *
     * Usage:
     *   const map = new HostMap();
     *   map.set("count", 42);
     *   print(map.get("count")); // 42
     *   print(map.has("count")); // true
     *   map.free(); // IMPORTANT: must free when done
     */
    globalThis.HostMap = class HostMap {
        constructor() {
            this._handle = __edgebox_map_new();
            if (this._handle < 0) throw new Error('Failed to create HostMap');
        }

        set(key, value) {
            if (typeof key !== 'string') throw new TypeError('key must be a string');
            const result = __edgebox_map_set(this._handle, key, value | 0);
            if (result < 0) throw new Error('map_set failed');
            return this; // Chainable
        }

        get(key) {
            if (typeof key !== 'string') return undefined;
            const result = __edgebox_map_get(this._handle, key);
            return result === -1 ? undefined : result;
        }

        has(key) {
            if (typeof key !== 'string') return false;
            return __edgebox_map_has(this._handle, key);
        }

        delete(key) {
            if (typeof key !== 'string') return false;
            return __edgebox_map_delete(this._handle, key);
        }

        get size() {
            return __edgebox_map_len(this._handle);
        }

        clear() {
            __edgebox_map_clear(this._handle);
            return this;
        }

        free() {
            if (this._handle >= 0) {
                __edgebox_map_free(this._handle);
                this._handle = -1;
            }
        }
    };
}

    /**
     * TSC Factory Interception for Native Shape Registration
     *
     * When TypeScript compiler creates AST nodes, we intercept the factory
     * and register nodes in the native registry. This enables ~1.7x faster
     * property access (kind, flags, pos, end) by avoiding QuickJS hash table
     * lookups.
     *
     * The interception is lazy - it waits for the `ts` global to be defined.
     */
    globalThis.__edgebox_intercept_tsc_factory = function() {
        // Check if ts is available
        if (typeof globalThis.ts === 'undefined') {
            return false;
        }

        // Check if registration function is available
        if (typeof __edgebox_register_node !== 'function') {
            return false;
        }

        // Already intercepted?
        if (globalThis.__edgebox_tsc_intercepted) {
            return true;
        }

        const ts = globalThis.ts;

        // Find the factory - it could be ts.factory or created dynamically
        // The main entry point is createSourceFile which uses a private factory

        // Intercept createSourceFile to hook into node creation
        const originalCreateSourceFile = ts.createSourceFile;
        if (typeof originalCreateSourceFile === 'function') {
            ts.createSourceFile = function(fileName, sourceText, languageVersion, setParentNodes, scriptKind) {
                const result = originalCreateSourceFile.apply(this, arguments);

                // Walk the AST and register all nodes
                function registerNode(node) {
                    if (node && typeof node.kind === 'number') {
                        __edgebox_register_node(
                            node,
                            node.kind,
                            node.flags || 0,
                            node.pos || 0,
                            node.end || 0
                        );
                        // Register children recursively
                        ts.forEachChild(node, registerNode);
                    }
                }
                registerNode(result);

                return result;
            };
        }

        // Also try to intercept the factory's createNode if available
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

    // Try to intercept immediately, and also set up a lazy check
    globalThis.__edgebox_intercept_tsc_factory();

// TypeScript sys interface integration
// Replace TypeScript's sys methods with our native bindings for proper file I/O
(function() {
    'use strict';
    var checkCount = 0;
    var hooked = false;

    function hookTypescriptSys() {
        checkCount++;
        // Look for global 'sys' object (TypeScript's system interface)
        if (typeof sys === 'undefined' || !sys) {
            if (checkCount < 10) {
                setTimeout(hookTypescriptSys, 50);
            }
            return false;
        }

        if (hooked) return true;
        hooked = true;

        // Replace sys.writeFile with native implementation
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

        // Replace sys.readFile with native implementation
        if (sys.readFile) {
            sys.readFile = function(path, encoding) {
                if (typeof globalThis.__edgebox_fs_read === 'function') {
                    try {
                        return globalThis.__edgebox_fs_read(path);
                    } catch(e) {
                        return undefined; // TypeScript expects undefined on read failure
                    }
                }
                return undefined;
            };
        }

        // Replace sys.fileExists with native implementation
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

        // Replace sys.directoryExists with native implementation
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

        // Replace sys.createDirectory with native implementation
        sys.createDirectory = function(path) {
            if (typeof globalThis.__edgebox_fs_mkdir === 'function') {
                try {
                    globalThis.__edgebox_fs_mkdir(path, true); // recursive
                } catch(e) {
                    // Ignore errors - directory may already exist
                }
            }
        };

        // Replace sys.getDirectories with native implementation
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

        // Replace sys.readDirectory with native implementation
        if (sys.readDirectory) {
            var origReadDirectory = sys.readDirectory;
            sys.readDirectory = function(path, extensions, excludes, includes, depth) {
                // For complex glob patterns, fall back to original if it works
                // For simple cases, use native readdir
                if (typeof globalThis.__edgebox_fs_readdir === 'function') {
                    try {
                        var entries = globalThis.__edgebox_fs_readdir(path);
                        var files = entries.filter(function(e) { return !e.isDirectory; }).map(function(e) { return e.name; });
                        // Filter by extensions if provided
                        if (extensions && extensions.length > 0) {
                            files = files.filter(function(f) {
                                return extensions.some(function(ext) { return f.endsWith(ext); });
                            });
                        }
                        return files;
                    } catch(e) {
                        // Fall back to original
                        return origReadDirectory ? origReadDirectory.call(this, path, extensions, excludes, includes, depth) : [];
                    }
                }
                return origReadDirectory ? origReadDirectory.call(this, path, extensions, excludes, includes, depth) : [];
            };
        }

        return true;
    }

    // Hook immediately and also set up delayed check
    hookTypescriptSys();
    if (!hooked) {
        setTimeout(hookTypescriptSys, 10);
    }
})();

// Mark runtime polyfills as initialized
globalThis._runtimePolyfillsInitialized = true;

} // End of guard block
