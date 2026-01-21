// EdgeBox Runtime: WebAssembly stub

if (typeof globalThis.WebAssembly === 'undefined') {
    if (globalThis._edgebox_debug && typeof print === 'function') {
        print('[WASM] Providing mock WebAssembly stub');
    }

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

    function MockModule(bytes) {
        this._bytes = bytes;
    }
    MockModule.imports = function(mod) { return []; };
    MockModule.exports = function(mod) { return []; };
    MockModule.customSections = function(mod, name) { return []; };

    function MockInstance(module, imports) {
        this.module = module;
        let _initialized = false;
        this.exports = new Proxy({}, {
            get: function(target, prop) {
                try {
                    if (prop !== 'then' && prop !== 'memory' && typeof print === 'function') {
                        print('[WASM] export accessed: ' + String(prop));
                    }
                    if (prop === 'memory' || prop === 'E') {
                        if (!target.memory) {
                            target.memory = new MockMemory({initial: 256});
                            print('[WASM] created memory for ' + prop + ', buffer size: ' + target.memory.buffer.byteLength);
                        }
                        return target.memory;
                    }
                    if (prop === '__wasm_call_ctors' || prop === '__initialize' || prop === '_initialize' || prop === 'F') {
                        return function() {
                            if (typeof print === 'function') print('[WASM] init function called: ' + String(prop));
                            _initialized = true;
                            return 0;
                        };
                    }
                    if (prop === 'malloc' || prop === '_malloc') {
                        return function(size) {
                            if (typeof print === 'function') print('[WASM] malloc(' + size + ')');
                            return 1024;
                        };
                    }
                    if (prop === 'free' || prop === '_free' || prop === 'F') {
                        return function() { return 0; };
                    }
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

    const WasmError = class extends Error {
        constructor(msg) { super(msg); this.name = 'WebAssemblyNotSupported'; }
    };

    globalThis.WebAssembly = {
        validate: function(bytes) { return true; },
        compile: function(bytes) {
            if (typeof print === 'function') print('[WASM] compile called - returning mock module');
            return Promise.resolve(new MockModule(bytes));
        },
        instantiate: function(bytes, imports) {
            if (typeof print === 'function') print('[WASM] instantiate called, bytes type: ' + (bytes ? bytes.constructor.name : 'null') + ', size: ' + (bytes && bytes.byteLength ? bytes.byteLength : 'N/A'));
            const module = new MockModule(bytes);
            const instance = new MockInstance(module, imports);
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
        CompileError: class extends Error { constructor(m) { super(m); this.name = 'CompileError'; } },
        LinkError: class extends Error { constructor(m) { super(m); this.name = 'LinkError'; } },
        RuntimeError: class extends Error { constructor(m) { super(m); this.name = 'RuntimeError'; } }
    };
}
