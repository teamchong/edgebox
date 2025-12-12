// EdgeBox Runtime Polyfills
// These are bundled with user code at build time for bytecode caching

// GUARD: Skip if already initialized (Wizer pre-initialized case)
if (globalThis._runtimePolyfillsInitialized) {
    // Runtime polyfills already loaded by Wizer, skip to user code
} else {

// Global error handler - catch any unhandled errors
globalThis._edgebox_debug = typeof scriptArgs !== 'undefined' && scriptArgs.includes('--debug');

// Spoof process.platform/arch EARLY - bundle code checks these before our full polyfills
if (!globalThis.process) globalThis.process = {};
globalThis.process.platform = 'darwin';
globalThis.process.arch = 'x64';

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

// WebAssembly stub - nested WASM not supported inside WASM runtime
// Returns mock objects that allow tree-sitter and similar libs to initialize without crashing
if (typeof globalThis.WebAssembly === 'undefined') {
    // Mock WASM module that does nothing but doesn't crash
    const MockModule = function() { this.exports = {}; };
    const MockInstance = function(module) { this.exports = module ? module.exports || {} : {}; };
    const MockMemory = function(desc) { this.buffer = new ArrayBuffer(desc && desc.initial ? desc.initial * 65536 : 65536); };
    const MockTable = function() { this.length = 0; this.get = function() { return null; }; this.set = function() {}; this.grow = function() { return 0; }; };
    const MockGlobal = function(desc, val) { this.value = val !== undefined ? val : 0; };

    globalThis.WebAssembly = {
        // Return empty module/instance that won't crash
        instantiate: function(bytes, imports) {
            var mod = new MockModule();
            var inst = new MockInstance(mod);
            return Promise.resolve({ module: mod, instance: inst });
        },
        instantiateStreaming: function(source, imports) {
            var mod = new MockModule();
            var inst = new MockInstance(mod);
            return Promise.resolve({ module: mod, instance: inst });
        },
        compile: function(bytes) { return Promise.resolve(new MockModule()); },
        compileStreaming: function(source) { return Promise.resolve(new MockModule()); },
        validate: function() { return true; },
        Module: MockModule,
        Instance: MockInstance,
        Memory: MockMemory,
        Table: MockTable,
        Global: MockGlobal,
        CompileError: Error,
        LinkError: Error,
        RuntimeError: Error
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

// Hook process.exit for error tracking (only if debug enabled)
(function() {
    const checkProcess = () => {
        if (typeof globalThis.process !== 'undefined') {
            const _origExit = globalThis.process.exit;
            globalThis.process.exit = function(code) {
                if (globalThis._edgebox_debug) {
                    print('[EDGEBOX JS] process.exit called with code:', code);
                }
                if (_origExit) _origExit(code);
            };
        }
    };
    // Check now and after a microtask (in case process is set up later)
    checkProcess();
    if (typeof queueMicrotask === 'function') {
        queueMicrotask(checkProcess);
    }
})();

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

// Console polyfill - ALWAYS override to ensure output via print()
// Some bundled code tries to replace console with no-ops, so we make it non-configurable
{
    const _print = typeof print === 'function' ? print : () => {};
    const consoleImpl = {
        log: (...args) => _print(...args),
        error: (...args) => _print('ERROR:', ...args),
        warn: (...args) => _print('WARN:', ...args),
        info: (...args) => _print('INFO:', ...args),
        debug: (...args) => _print('DEBUG:', ...args),
        trace: (...args) => _print('TRACE:', ...args),
        dir: (obj) => _print(JSON.stringify(obj, null, 2)),
        table: (data) => _print(JSON.stringify(data, null, 2)),
        assert: (cond, ...args) => { if (!cond) _print('ASSERTION FAILED:', ...args); },
        time: () => {},
        timeEnd: () => {},
        timeLog: () => {},
        group: () => {},
        groupEnd: () => {},
        groupCollapsed: () => {},
        clear: () => {},
        count: () => {},
        countReset: () => {},
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
        if (globalThis._edgebox_debug) print('[TIMER] Using _os.setTimeout for proper event loop integration');
        let _setTimeoutCount = 0;
        globalThis.setTimeout = function(callback, delay = 0, ...args) {
            const id = _timerId++;
            _setTimeoutCount++;
            if (globalThis._edgebox_debug && _setTimeoutCount <= 5) {
                print('[setTimeout] #' + _setTimeoutCount + ' delay=' + delay + 'ms');
            }
            const handle = _os.setTimeout(() => {
                _timers.delete(id);
                callback(...args);
            }, delay);
            _timers.set(id, handle);
            return new Timeout(id, callback, delay, args, false);
        };

        globalThis.clearTimeout = function(timer) {
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
            const id = typeof timer === 'object' ? timer._id : timer;
            _timers.delete(id);
        };
        globalThis.setInterval = function(callback, delay = 0, ...args) {
            const id = _timerId++;
            return new Timeout(id, callback, delay, args, true);
        };
        globalThis.clearInterval = function(timer) {
            const id = typeof timer === 'object' ? timer._id : timer;
            _timers.delete(id);
        };
    }

    globalThis.setImmediate = function(callback, ...args) { return setTimeout(callback, 0, ...args); };
    globalThis.clearImmediate = function(id) { clearTimeout(id); };
})();

// TextEncoder/TextDecoder polyfills
if (typeof TextEncoder === 'undefined') {
    globalThis.TextEncoder = class TextEncoder {
        constructor(encoding = 'utf-8') { this.encoding = encoding; }
        encode(str) {
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

if (typeof TextDecoder === 'undefined') {
    globalThis.TextDecoder = class TextDecoder {
        constructor(encoding = 'utf-8') { this.encoding = encoding; }
        decode(input) {
            const bytes = input instanceof Uint8Array ? input : new Uint8Array(input);
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
            // Return a minimal ReadableStream-like object for Axios compatibility
            const self = this;
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
            if (globalThis._edgebox_debug) print('[FETCH] Called with: ' + (typeof input === 'string' ? input : input?.url || 'unknown'));
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
    };
}

// Process polyfill (minimal) - Always set up full process object
// Previous polyfills may have created an incomplete process object
globalThis.process = {
        platform: 'wasi',
        arch: 'wasm32',
        version: 'v20.0.0',
        versions: { node: '20.0.0', v8: '11.0.0', uv: '1.0.0', modules: '115' },
        // process.argv should be: [node_path, script_path, ...args]
        // scriptArgs is [wasm_path, ...args], we transform it to [node, wasm_path, ...args]
        argv: (typeof scriptArgs !== 'undefined') ? ['node'].concat(scriptArgs) : ['node'],
        execArgv: [], // Node.js flags like --inspect, --max-old-space-size, etc.
        execPath: '/usr/bin/node',
        env: {},
        cwd: () => '/',
        chdir: (dir) => { /* no-op in WASM */ },
        exit: (code) => {
            if (globalThis._edgebox_debug) print('[EDGEBOX JS] process.exit(' + (code || 0) + ') called');
            throw new Error('process.exit(' + (code || 0) + ')');
        },
        stdout: { write: (s) => print(s), isTTY: false },
        stderr: { write: (s) => print(s), isTTY: false },
        stdin: (function() {
            // Create a minimal readable stream for stdin
            const listeners = {};
            const stdin = {
                isTTY: false,
                fd: 0,
                _encoding: null,
                _data: '',
                _ended: false,
                setEncoding: function(encoding) {
                    this._encoding = encoding;
                    return this;
                },
                on: function(event, callback) {
                    if (!listeners[event]) listeners[event] = [];
                    listeners[event].push(callback);
                    // For 'end' event, emit after a tick since we have no actual stdin in WASM
                    if (event === 'end') {
                        setTimeout(() => {
                            this._ended = true;
                            callback();
                        }, 0);
                    }
                    return this;
                },
                once: function(event, callback) {
                    const wrapper = (...args) => {
                        this.off(event, wrapper);
                        callback(...args);
                    };
                    return this.on(event, wrapper);
                },
                off: function(event, callback) {
                    if (listeners[event]) {
                        listeners[event] = listeners[event].filter(cb => cb !== callback);
                    }
                    return this;
                },
                removeListener: function(event, callback) {
                    return this.off(event, callback);
                },
                emit: function(event, ...args) {
                    if (listeners[event]) {
                        listeners[event].forEach(cb => cb(...args));
                    }
                    return listeners[event] && listeners[event].length > 0;
                },
                read: function() {
                    return null; // No data available
                },
                pause: function() { return this; },
                resume: function() { return this; },
                pipe: function(dest) { return dest; },
                unpipe: function() { return this; },
                destroy: function() { return this; },
                readable: true,
                readableEnded: false,
                readableFlowing: null,
                readableHighWaterMark: 16384,
                readableLength: 0,
                [Symbol.asyncIterator]: async function*() {
                    // Empty iterator - no stdin data in WASM
                }
            };
            return stdin;
        })(),
        nextTick: (fn, ...args) => { setTimeout(() => fn(...args), 0); },
        hrtime: { bigint: () => BigInt(Date.now()) * 1000000n },
        pid: 1,
        ppid: 0,
        title: 'node',
        memoryUsage: () => ({ rss: 0, heapTotal: 0, heapUsed: 0, external: 0, arrayBuffers: 0 }),
        cpuUsage: () => ({ user: 0, system: 0 }),
        uptime: () => 0,
        kill: () => { /* no-op */ },
        on: function() { return this; },
        once: function() { return this; },
        off: function() { return this; },
        emit: function() { return false; },
        removeListener: function() { return this; },
        removeAllListeners: function() { return this; },
        setMaxListeners: function() { return this; },
        listeners: function() { return []; },
        features: { inspector: false, debug: false, uv: false, ipv6: true, tls_alpn: false, tls_sni: false, tls_ocsp: false, tls: false },
};

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

// Mark runtime polyfills as initialized
globalThis._runtimePolyfillsInitialized = true;

} // End of guard block
