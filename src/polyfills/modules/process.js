    // ===== PROCESS OBJECT =====
    if (!globalThis.process) globalThis.process = {};

    // Create TTY streams for stdout and stderr (defined later, but referenced here)
    // Set isTTY: false for pipe mode (-p) compatibility
    const _stdout = {
        isTTY: false,
        columns: 80,
        rows: 24,
        write: function(data) {
            if (typeof __edgebox_stdout_write === 'function') {
                __edgebox_stdout_write(String(data));
            } else {
                print(String(data).replace(/\n$/, '')); // print adds newline
            }
            return true;
        },
        end: function() {},
        on: function() { return this; },
        once: function() { return this; },
        emit: function() { return false; },
        getWindowSize: function() { return [this.columns, this.rows]; },
        cursorTo: function(x, y) { this.write(y !== undefined ? `\x1b[${y+1};${x+1}H` : `\x1b[${x+1}G`); },
        moveCursor: function(dx, dy) {
            if (dx > 0) this.write(`\x1b[${dx}C`);
            else if (dx < 0) this.write(`\x1b[${-dx}D`);
            if (dy > 0) this.write(`\x1b[${dy}B`);
            else if (dy < 0) this.write(`\x1b[${-dy}A`);
        },
        clearLine: function(dir) { this.write(`\x1b[${dir === -1 ? 1 : dir === 1 ? 0 : 2}K`); },
        clearScreenDown: function() { this.write('\x1b[J'); },
        getColorDepth: function() { return 8; },
        hasColors: function(count) { return (count || 16) <= 256; }
    };

    const _stderr = {
        isTTY: false,
        columns: 80,
        rows: 24,
        write: function(data) {
            if (typeof __edgebox_stderr_write === 'function') {
                __edgebox_stderr_write(String(data));
            } else {
                print('[stderr] ' + String(data).replace(/\n$/, ''));
            }
            return true;
        },
        end: function() {},
        on: function() { return this; },
        once: function() { return this; },
        emit: function() { return false; },
        getWindowSize: function() { return [this.columns, this.rows]; },
        cursorTo: function(x, y) { this.write(y !== undefined ? `\x1b[${y+1};${x+1}H` : `\x1b[${x+1}G`); },
        moveCursor: function(dx, dy) {
            if (dx > 0) this.write(`\x1b[${dx}C`);
            else if (dx < 0) this.write(`\x1b[${-dx}D`);
            if (dy > 0) this.write(`\x1b[${dy}B`);
            else if (dy < 0) this.write(`\x1b[${-dy}A`);
        },
        clearLine: function(dir) { this.write(`\x1b[${dir === -1 ? 1 : dir === 1 ? 0 : 2}K`); },
        clearScreenDown: function() { this.write('\x1b[J'); },
        getColorDepth: function() { return 8; },
        hasColors: function(count) { return (count || 16) <= 256; }
    };

    // Stdin with proper EventEmitter support
    class StdinStream extends Readable {
        constructor() {
            super();
            this.isTTY = typeof __edgebox_isatty === 'function' ? __edgebox_isatty(0) : false;
            this.isRaw = false;
            this.fd = 0;
            this._encoding = null;
            this._paused = true;
            // In WASM/pipe mode, stdin is immediately closed (no interactive input)
            this._closed = !this.isTTY;
            this._endEmitted = false;
        }
        // Override on() to emit 'end' immediately if stdin is already closed
        on(event, listener) {
            super.on(event, listener);
            // If adding 'end' listener on closed stdin, emit 'end' asynchronously
            if ((event === 'end' || event === 'close') && this._closed && !this._endEmitted) {
                this._endEmitted = true;
                setTimeout(() => {
                    this.emit('end');
                    this.emit('close');
                }, 0);
            }
            return this;
        }
        setRawMode(mode) { this.isRaw = mode; return this; }
        setEncoding(enc) { this._encoding = enc; return this; }
        pause() { this._paused = true; return this; }
        resume() {
            this._paused = false;
            // Start reading if we have listeners
            if (this.listenerCount('data') > 0) {
                this._startReading();
            }
            return this;
        }
        _startReading() {
            // If stdin is already closed (non-TTY/pipe mode), emit end immediately
            if (this._closed) {
                setTimeout(() => {
                    this.emit('end');
                    this.emit('close');
                }, 0);
                return;
            }
            if (this._paused) return;
            // Only read if we have active data listeners waiting
            if (this.listenerCount('data') === 0) return;

            // Check if stdin has data ready (non-blocking)
            if (typeof __edgebox_stdin_ready === 'function') {
                if (!__edgebox_stdin_ready()) {
                    // No data ready, retry later (non-blocking poll)
                    if (!this._paused && !this._closed) {
                        setTimeout(() => this._startReading(), 50);
                    }
                    return;
                }
            }

            // Data is ready (or no readiness check available), now safe to read
            if (typeof __edgebox_read_stdin === 'function') {
                const data = __edgebox_read_stdin(4096);
                if (data && data.length > 0) {
                    const buf = Buffer.from(data);
                    this.emit('data', this._encoding ? buf.toString(this._encoding) : buf);
                    // Schedule next read
                    if (!this._paused && !this._closed) {
                        setTimeout(() => this._startReading(), 0);
                    }
                } else if (data === null) {
                    this._closed = true;
                    this.emit('end');
                    this.emit('close');
                }
            }
        }
        read(size) {
            if (typeof __edgebox_read_stdin === 'function') {
                const data = __edgebox_read_stdin(size || 4096);
                return data ? Buffer.from(data) : null;
            }
            return null;
        }
    }
    const _stdin = new StdinStream();

    // Preserve native process properties if they exist
    // Native bindings (process.zig) set up real stdout/stderr/exit - don't overwrite!
    // CRITICAL: Capture references to native functions BEFORE Object.assign modifies globalThis.process
    var _existingProc = globalThis.process || {};
    var _nativeExit = _existingProc.exit;       // Capture native exit function
    var _nativeCwd = _existingProc.cwd;         // Capture native cwd function
    var _nativeHrtime = _existingProc.hrtime;   // Capture native hrtime function
    var _nativeHrtimeBigint = _nativeHrtime && _nativeHrtime.bigint;  // Capture native bigint method
    var _nativeNextTick = _existingProc.nextTick; // Capture native nextTick function
    Object.assign(globalThis.process, {
        env: _existingProc.env || {},
        argv: _existingProc.argv || [],
        // Preserve native cwd if it exists (from process.zig)
        cwd: _nativeCwd || (() => typeof __edgebox_cwd === 'function' ? __edgebox_cwd() : '/'),
        version: _existingProc.version || 'v18.0.0-edgebox',
        versions: _existingProc.versions || { node: '18.0.0', v8: '0.0.0', quickjs: '2024.1' },
        platform: _existingProc.platform || 'darwin',
        arch: _existingProc.arch || 'x64',
        // Preserve native hrtime if it exists (with proper bigint support)
        hrtime: (function() {
            const hrtimeFn = _nativeHrtime || function(prev) {
                const now = Date.now();
                const sec = Math.floor(now / 1000);
                const nsec = (now % 1000) * 1000000;
                if (prev) {
                    let dsec = sec - prev[0];
                    let dnsec = nsec - prev[1];
                    if (dnsec < 0) { dsec -= 1; dnsec += 1000000000; }
                    return [dsec, dnsec];
                }
                return [sec, nsec];
            };
            // bigint always returns BigInt - wrap native result if needed
            hrtimeFn.bigint = function() {
                if (_nativeHrtimeBigint) {
                    const result = _nativeHrtimeBigint();
                    // Ensure it's a BigInt - native may return number
                    return typeof result === 'bigint' ? result : BigInt(result);
                }
                return BigInt(Date.now()) * BigInt(1000000);
            };
            return hrtimeFn;
        })(),
        // Preserve native nextTick if it exists
        nextTick: _nativeNextTick || ((fn, ...args) => { setTimeout(() => fn(...args), 0); }),
        // CRITICAL: Preserve native stdout/stderr - these write to real stdout/stderr
        stdout: _existingProc.stdout || _stdout,
        stderr: _existingProc.stderr || _stderr,
        stdin: _existingProc.stdin || _stdin,
        // CRITICAL: Use captured _nativeExit to avoid self-reference after Object.assign
        exit: _nativeExit ? function(code) {
            return _nativeExit(code);  // Call the captured native function directly
        } : ((code) => { throw new Error('process.exit(' + code + ')'); }),
    });

    // Make process an EventEmitter for signal and process events
    // Note: In a sandboxed WASM environment, OS signals can't be caught,
    // but we provide the API for compatibility with code that expects it.
    {
        const _emitter = new EventEmitter();
        const _exitListeners = [];
        const _beforeExitListeners = [];
        let _exitCode = undefined;
        let _exiting = false;

        // Wrap exit to emit 'exit' event first
        const _originalExit = globalThis.process.exit;
        globalThis.process.exit = function(code) {
            if (_exiting) return; // Prevent re-entry
            _exiting = true;
            _exitCode = code === undefined ? 0 : code;

            // Emit 'beforeExit' (can be prevented by scheduling more work)
            try {
                _emitter.emit('beforeExit', _exitCode);
            } catch (e) { /* ignore errors in beforeExit handlers */ }

            // Emit 'exit' (cannot be prevented, only cleanup)
            try {
                _emitter.emit('exit', _exitCode);
            } catch (e) { /* ignore errors in exit handlers */ }

            return _originalExit(_exitCode);
        };

        // Set exitCode property
        Object.defineProperty(globalThis.process, 'exitCode', {
            get() { return _exitCode; },
            set(code) { _exitCode = code; }
        });

        // EventEmitter methods
        globalThis.process.on = function(event, listener) {
            if (event === 'exit') _exitListeners.push(listener);
            else if (event === 'beforeExit') _beforeExitListeners.push(listener);
            _emitter.on(event, listener);
            return this;
        };
        globalThis.process.once = function(event, listener) {
            _emitter.once(event, listener);
            return this;
        };
        globalThis.process.emit = function(event, ...args) {
            return _emitter.emit(event, ...args);
        };
        globalThis.process.off = function(event, listener) {
            _emitter.off(event, listener);
            return this;
        };
        globalThis.process.removeListener = function(event, listener) {
            _emitter.removeListener(event, listener);
            return this;
        };
        globalThis.process.removeAllListeners = function(event) {
            _emitter.removeAllListeners(event);
            return this;
        };
        globalThis.process.listeners = function(event) {
            return _emitter.listeners(event);
        };
        globalThis.process.listenerCount = function(event) {
            return _emitter.listenerCount(event);
        };
        globalThis.process.prependListener = function(event, listener) {
            _emitter.prependListener(event, listener);
            return this;
        };
        globalThis.process.prependOnceListener = function(event, listener) {
            _emitter.prependOnceListener(event, listener);
            return this;
        };
        globalThis.process.eventNames = function() {
            return _emitter.eventNames();
        };
        globalThis.process.setMaxListeners = function(n) {
            _emitter.setMaxListeners(n);
            return this;
        };
        globalThis.process.getMaxListeners = function() {
            return _emitter.getMaxListeners();
        };
        globalThis.process.rawListeners = function(event) {
            return _emitter.rawListeners(event);
        };

        // Process-specific methods
        globalThis.process.kill = function(pid, signal) {
            // In sandboxed environment, we can only send signals to ourselves
            if (pid !== globalThis.process.pid && pid !== 0) {
                const err = new Error('ESRCH: process not found');
                err.code = 'ESRCH';
                throw err;
            }
            signal = signal || 'SIGTERM';
            // Emit the signal event
            return _emitter.emit(signal);
        };

        globalThis.process.abort = function() {
            _emitter.emit('SIGABRT');
            return _originalExit(134); // Standard abort exit code
        };

        // Add addListener alias
        globalThis.process.addListener = globalThis.process.on;
    }

