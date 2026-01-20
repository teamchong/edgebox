    // ===== CHILD_PROCESS MODULE =====
    // Async child process with EventEmitter-based ChildProcess class

    // Get EventEmitter from _modules (using _cp prefix to avoid conflicts with class name)
    const _cpEventEmitter = _modules.events;

    // Get native sync functions from _modules
    const nativeExecSync = _modules.child_process?.execSync || _modules['node:child_process']?.execSync;
    const nativeSpawnSync = _modules.child_process?.spawnSync || _modules['node:child_process']?.spawnSync;

    // Writable stream for stdin (EventEmitter-based, minimal implementation)
    class ChildProcessStdin extends _cpEventEmitter {
        constructor(childProcess) {
            super();
            this._childProcess = childProcess;
            this._buffer = [];
            this.writable = true;
        }
        write(chunk, encoding, callback) {
            if (typeof encoding === 'function') {
                callback = encoding;
                encoding = 'utf8';
            }
            if (typeof chunk === 'string') {
                this._buffer.push(Buffer.from(chunk, encoding || 'utf8'));
            } else {
                this._buffer.push(chunk);
            }
            if (callback) callback();
            return true;
        }
        end(chunk, encoding, callback) {
            if (chunk) this.write(chunk, encoding);
            this.writable = false;
            this.emit('finish');
            if (typeof callback === 'function') callback();
            else if (typeof encoding === 'function') encoding();
        }
        getBufferedData() {
            if (this._buffer.length === 0) return null;
            return Buffer.concat(this._buffer);
        }
    }

    // Readable stream for stdout/stderr (EventEmitter-based, minimal implementation)
    class ChildProcessStream extends _cpEventEmitter {
        constructor() {
            super();
            this._data = null;
            this.readable = true;
        }
        setData(data) {
            this._data = data;
            if (data && data.length > 0) {
                this.emit('data', data);
            }
            this.readable = false;
            this.emit('end');
        }
        setEncoding(encoding) {
            this._encoding = encoding;
            return this;
        }
        read() {
            return this._data;
        }
        pipe(dest) {
            this.on('data', (chunk) => dest.write(chunk));
            this.on('end', () => dest.end());
            return dest;
        }
    }

    // ChildProcess class - EventEmitter with streams
    class ChildProcess extends _cpEventEmitter {
        constructor() {
            super();
            this.pid = null;
            this.connected = false;
            this.signalCode = null;
            this.exitCode = null;
            this.killed = false;
            this.spawnfile = null;
            this.spawnargs = [];

            // Create streams
            this.stdin = new ChildProcessStdin(this);
            this.stdout = new ChildProcessStream();
            this.stderr = new ChildProcessStream();
            this.stdio = [this.stdin, this.stdout, this.stderr];

            this._spawned = false;
            this._command = null;
            this._args = [];
            this._options = {};

            // IPC channel state
            this._ipcFd = null;
            this._ipcBuffer = '';
            this._ipcPollInterval = null;
        }

        kill(signal) {
            signal = signal || 'SIGTERM';
            this.killed = true;
            this.signalCode = signal;

            // If using async spawn, kill the native process
            if (this._procId !== undefined && typeof __edgebox_kill_process === 'function') {
                // Map signal name to number
                const signalNum = signal === 'SIGKILL' ? 9
                                : signal === 'SIGTERM' ? 15
                                : signal === 'SIGINT' ? 2
                                : signal === 'SIGHUP' ? 1
                                : 15; // Default SIGTERM
                __edgebox_kill_process(this._procId, signalNum);

                // Stop polling
                if (this._pollInterval) {
                    clearInterval(this._pollInterval);
                    this._pollInterval = null;
                }

                // Clean up native process slot
                if (typeof __edgebox_cleanup_process === 'function') {
                    __edgebox_cleanup_process(this._procId);
                }
            }

            // Emit events
            this.emit('exit', null, signal);
            this.emit('close', null, signal);
            return true;
        }

        ref() { return this; }
        unref() { return this; }

        disconnect() {
            this.connected = false;
            this.emit('disconnect');
        }

        send(message, sendHandle, options, callback) {
            // Handle argument variations
            if (typeof options === 'function') {
                callback = options;
                options = undefined;
            }
            if (typeof sendHandle === 'function') {
                callback = sendHandle;
                sendHandle = undefined;
            }

            // Check if IPC channel is available
            if (this._ipcFd === null) {
                if (callback) {
                    process.nextTick(() => callback(new Error('IPC channel not available')));
                }
                return false;
            }

            // Serialize message as JSON with newline delimiter
            try {
                const data = JSON.stringify(message) + '\n';

                // Write to IPC channel using native function
                if (typeof __edgebox_ipc_write === 'function') {
                    const result = __edgebox_ipc_write(this._ipcFd, data);
                    if (result < 0) {
                        if (callback) {
                            process.nextTick(() => callback(new Error('Failed to write to IPC channel')));
                        }
                        return false;
                    }
                    if (callback) {
                        process.nextTick(() => callback(null));
                    }
                    return true;
                } else {
                    if (callback) {
                        process.nextTick(() => callback(new Error('IPC write not available')));
                    }
                    return false;
                }
            } catch (err) {
                if (callback) {
                    process.nextTick(() => callback(err));
                }
                return false;
            }
        }

        // Start polling for IPC messages
        _startIpcPolling() {
            if (this._ipcPollInterval || this._ipcFd === null) return;

            const self = this;
            this._ipcPollInterval = setInterval(() => {
                if (self._ipcFd === null || typeof __edgebox_ipc_read !== 'function') {
                    self._stopIpcPolling();
                    return;
                }

                // Read from IPC channel
                const data = __edgebox_ipc_read(self._ipcFd);
                if (data) {
                    self._ipcBuffer += data;

                    // Parse newline-delimited JSON messages
                    let newlineIdx;
                    while ((newlineIdx = self._ipcBuffer.indexOf('\n')) !== -1) {
                        const line = self._ipcBuffer.slice(0, newlineIdx);
                        self._ipcBuffer = self._ipcBuffer.slice(newlineIdx + 1);

                        if (line.trim()) {
                            try {
                                const msg = JSON.parse(line);
                                self.emit('message', msg);
                            } catch (e) {
                                // Ignore parse errors for malformed messages
                            }
                        }
                    }
                }
            }, 50); // Poll every 50ms
        }

        // Stop polling for IPC messages
        _stopIpcPolling() {
            if (this._ipcPollInterval) {
                clearInterval(this._ipcPollInterval);
                this._ipcPollInterval = null;
            }
        }

        // Close IPC channel
        _closeIpc() {
            this._stopIpcPolling();
            if (this._ipcFd !== null && typeof __edgebox_close_fd === 'function') {
                __edgebox_close_fd(this._ipcFd);
                this._ipcFd = null;
            }
            this.connected = false;
        }

        // Internal spawn method
        _spawn(command, args, options) {
            this.spawnfile = command;
            // Make copies of strings to prevent GC issues with nextTick
            this._command = String(command);
            this._args = args.map(a => String(a));
            this.spawnargs = [this._command, ...this._args];
            this._options = options || {};
            this._spawned = true;

            const self = this;

            // Execute in next tick to allow event listeners to be attached
            process.nextTick(() => {
                self._execute();
            });
        }

        _execute() {
            const self = this;
            const options = this._options;

            // Check if native async spawn is available
            if (typeof __edgebox_spawn_async === 'function') {
                this._executeAsync();
                return;
            }

            // Fallback to sync execution wrapped with nextTick
            try {
                // Use native spawnSync - note: don't pass shell option here
                // because spawn() already handles shell wrapping
                const result = nativeSpawnSync(this._command, this._args, {
                    cwd: options.cwd,
                    env: options.env,
                    encoding: options.encoding,
                    timeout: options.timeout,
                    maxBuffer: options.maxBuffer,
                    killSignal: options.killSignal
                    // shell is NOT passed - we handle shell wrapping in spawn()
                });

                // Set pid (synthetic since sync execution)
                this.pid = result.pid || Math.floor(Math.random() * 100000) + 1000;
                this.emit('spawn');

                // Handle stdout
                if (result.stdout) {
                    const stdoutData = Buffer.isBuffer(result.stdout)
                        ? result.stdout
                        : Buffer.from(result.stdout);
                    this.stdout.setData(stdoutData);
                } else {
                    this.stdout.setData(Buffer.alloc(0));
                }

                // Handle stderr
                if (result.stderr) {
                    const stderrData = Buffer.isBuffer(result.stderr)
                        ? result.stderr
                        : Buffer.from(result.stderr);
                    this.stderr.setData(stderrData);
                } else {
                    this.stderr.setData(Buffer.alloc(0));
                }

                // Handle exit
                if (result.error) {
                    this.emit('error', result.error);
                    this.exitCode = result.status !== null ? result.status : 1;
                } else {
                    this.exitCode = result.status !== null ? result.status : 0;
                }

                this.signalCode = result.signal || null;

                // Emit exit and close events
                process.nextTick(() => {
                    self.emit('exit', self.exitCode, self.signalCode);
                    self.emit('close', self.exitCode, self.signalCode);
                });

            } catch (err) {
                this.pid = null;
                this.exitCode = 1;
                this.emit('error', err);
                process.nextTick(() => {
                    self.emit('close', 1, null);
                });
            }
        }

        // True async execution using native async spawn
        _executeAsync() {
            const self = this;

            // Build full command with args
            const fullCommand = this._command + (this._args.length ? ' ' + this._args.join(' ') : '');

            // Spawn process asynchronously
            const procId = __edgebox_spawn_async(fullCommand);
            if (procId < 0) {
                const err = new Error('Failed to spawn process: ' + procId);
                this.emit('error', err);
                process.nextTick(() => {
                    self.emit('close', 1, null);
                });
                return;
            }

            this._procId = procId;
            this.pid = procId + 1000; // Synthetic PID offset
            this.emit('spawn');

            // Poll for output and completion
            const pollInterval = setInterval(() => {
                if (self.killed) {
                    clearInterval(pollInterval);
                    return;
                }

                const result = __edgebox_poll_process(procId);
                if (!result) {
                    clearInterval(pollInterval);
                    self.emit('error', new Error('Process polling failed'));
                    self.emit('close', 1, null);
                    return;
                }

                // Stream stdout if available
                if (result.stdout) {
                    const chunk = Buffer.from(result.stdout);
                    self.stdout.emit('data', chunk);
                }

                // Stream stderr if available
                if (result.stderr) {
                    const chunk = Buffer.from(result.stderr);
                    self.stderr.emit('data', chunk);
                }

                // Check if process completed
                if (result.done) {
                    clearInterval(pollInterval);

                    // Emit end events on streams
                    self.stdout.readable = false;
                    self.stdout.emit('end');
                    self.stderr.readable = false;
                    self.stderr.emit('end');

                    // Set exit code
                    self.exitCode = result.code;

                    // Clean up native process slot
                    if (typeof __edgebox_cleanup_process === 'function') {
                        __edgebox_cleanup_process(procId);
                    }

                    // Emit exit and close events
                    process.nextTick(() => {
                        self.emit('exit', self.exitCode, self.signalCode);
                        self.emit('close', self.exitCode, self.signalCode);
                    });
                }
            }, 10);

            this._pollInterval = pollInterval;
        }
    }

    // spawn(command, [args], [options])
    function spawn(command, args, options) {
        if (Array.isArray(args)) {
            options = options || {};
        } else if (args && typeof args === 'object') {
            options = args;
            args = [];
        } else {
            args = [];
            options = {};
        }

        const child = new ChildProcess();

        // Handle shell option
        if (options.shell) {
            const shellCmd = typeof options.shell === 'string' ? options.shell : '/bin/sh';
            // Force string conversion to prevent memory address issues
            const commandStr = '' + command;
            const argsJoined = args.length ? args.map(a => '' + a).join(' ') : '';
            const cmdStr = commandStr + (argsJoined ? ' ' + argsJoined : '');
            const shellArgs = ['-c', cmdStr];
            child._spawn(shellCmd, shellArgs, options);
        } else {
            child._spawn(command, args, options);
        }

        return child;
    }

    // exec(command, [options], callback)
    function exec(command, options, callback) {
        if (typeof options === 'function') {
            callback = options;
            options = {};
        }
        options = options || {};

        const child = spawn(command, [], {
            ...options,
            shell: options.shell !== false ? (options.shell || true) : false
        });

        let stdout = [];
        let stderr = [];
        const encoding = options.encoding || 'buffer';
        const maxBuffer = options.maxBuffer || 1024 * 1024;

        child.stdout.on('data', (data) => {
            stdout.push(data);
        });

        child.stderr.on('data', (data) => {
            stderr.push(data);
        });

        child.on('error', (err) => {
            if (callback) {
                callback(err, null, null);
                callback = null;
            }
        });

        child.on('close', (code, signal) => {
            if (!callback) return;

            let stdoutResult = Buffer.concat(stdout);
            let stderrResult = Buffer.concat(stderr);

            // Check max buffer
            if (stdoutResult.length > maxBuffer || stderrResult.length > maxBuffer) {
                const err = new Error('maxBuffer exceeded');
                err.code = 'ERR_CHILD_PROCESS_STDIO_MAXBUFFER';
                callback(err, stdoutResult, stderrResult);
                return;
            }

            // Apply encoding
            if (encoding !== 'buffer') {
                stdoutResult = stdoutResult.toString(encoding);
                stderrResult = stderrResult.toString(encoding);
            }

            if (code !== 0) {
                const err = new Error('Command failed: ' + command);
                err.code = code;
                err.signal = signal;
                err.killed = child.killed;
                err.cmd = command;
                callback(err, stdoutResult, stderrResult);
            } else {
                callback(null, stdoutResult, stderrResult);
            }
        });

        return child;
    }

    // execFile(file, [args], [options], callback)
    function execFile(file, args, options, callback) {
        if (typeof args === 'function') {
            callback = args;
            args = [];
            options = {};
        } else if (typeof options === 'function') {
            callback = options;
            if (Array.isArray(args)) {
                options = {};
            } else {
                options = args;
                args = [];
            }
        }
        options = options || {};
        args = args || [];

        const child = spawn(file, args, {
            ...options,
            shell: false
        });

        let stdout = [];
        let stderr = [];
        const encoding = options.encoding || 'buffer';
        const maxBuffer = options.maxBuffer || 1024 * 1024;

        child.stdout.on('data', (data) => {
            stdout.push(data);
        });

        child.stderr.on('data', (data) => {
            stderr.push(data);
        });

        child.on('error', (err) => {
            if (callback) {
                callback(err, null, null);
                callback = null;
            }
        });

        child.on('close', (code, signal) => {
            if (!callback) return;

            let stdoutResult = Buffer.concat(stdout);
            let stderrResult = Buffer.concat(stderr);

            if (stdoutResult.length > maxBuffer || stderrResult.length > maxBuffer) {
                const err = new Error('maxBuffer exceeded');
                err.code = 'ERR_CHILD_PROCESS_STDIO_MAXBUFFER';
                callback(err, stdoutResult, stderrResult);
                return;
            }

            if (encoding !== 'buffer') {
                stdoutResult = stdoutResult.toString(encoding);
                stderrResult = stderrResult.toString(encoding);
            }

            if (code !== 0) {
                const err = new Error('Command failed: ' + file);
                err.code = code;
                err.signal = signal;
                err.killed = child.killed;
                callback(err, stdoutResult, stderrResult);
            } else {
                callback(null, stdoutResult, stderrResult);
            }
        });

        return child;
    }

    // fork(modulePath, [args], [options]) - Node.js-compatible IPC support
    function fork(modulePath, args, options) {
        if (Array.isArray(args)) {
            options = options || {};
        } else if (args && typeof args === 'object') {
            options = args;
            args = [];
        } else {
            args = [];
            options = {};
        }

        const execPath = options.execPath || process.execPath || 'node';
        const execArgv = options.execArgv || process.execArgv || [];

        // Try to create IPC channel using socketpair
        let ipcPair = null;
        if (typeof __edgebox_socketpair === 'function') {
            ipcPair = __edgebox_socketpair();
        }

        // Set up environment with IPC FD if available
        const childEnv = { ...process.env, ...options.env };
        if (ipcPair) {
            // Pass child FD as NODE_CHANNEL_FD environment variable (Node.js convention)
            childEnv.NODE_CHANNEL_FD = String(ipcPair.childFd);
        }

        const child = spawn(execPath, [...execArgv, modulePath, ...args], {
            ...options,
            env: childEnv,
            stdio: options.stdio || 'pipe'
        });

        // Set up IPC channel on parent side
        if (ipcPair) {
            child._ipcFd = ipcPair.parentFd;
            child.connected = true;

            // Close child FD on parent side (child process has its own copy)
            if (typeof __edgebox_close_fd === 'function') {
                __edgebox_close_fd(ipcPair.childFd);
            }

            // Start polling for incoming IPC messages
            child._startIpcPolling();

            // Clean up IPC on process exit
            child.on('exit', () => {
                child._closeIpc();
            });
        } else {
            // Fallback: mark as connected but IPC won't work
            child.connected = true;
        }

        return child;
    }

    // execSync - wrapper around native
    function execSync(command, options) {
        options = options || {};
        return nativeExecSync(command, options);
    }

    // spawnSync - wrapper around native
    function spawnSync(command, args, options) {
        if (Array.isArray(args)) {
            options = options || {};
        } else if (args && typeof args === 'object') {
            options = args;
            args = [];
        } else {
            args = [];
            options = {};
        }
        return nativeSpawnSync(command, args, options);
    }

    // execFileSync
    function execFileSync(file, args, options) {
        if (Array.isArray(args)) {
            options = options || {};
        } else if (args && typeof args === 'object') {
            options = args;
            args = [];
        } else {
            args = [];
            options = {};
        }

        const result = nativeSpawnSync(file, args, options);

        if (result.error) {
            throw result.error;
        }
        if (result.status !== 0) {
            const err = new Error('Command failed: ' + file);
            err.status = result.status;
            err.signal = result.signal;
            err.stdout = result.stdout;
            err.stderr = result.stderr;
            throw err;
        }

        return result.stdout;
    }

    // forkSync - not in Node.js but useful
    function forkSync(modulePath, args, options) {
        if (Array.isArray(args)) {
            options = options || {};
        } else if (args && typeof args === 'object') {
            options = args;
            args = [];
        } else {
            args = [];
            options = {};
        }

        const execPath = options.execPath || process.execPath || 'node';
        const execArgv = options.execArgv || process.execArgv || [];

        return spawnSync(execPath, [...execArgv, modulePath, ...args], options);
    }

    // Create module object
    const childProcess = {
        ChildProcess,
        spawn,
        exec,
        execFile,
        fork,
        execSync,
        spawnSync,
        execFileSync,
        forkSync
    };

    // Merge with existing native module
    if (_modules.child_process) {
        Object.assign(_modules.child_process, childProcess);
    } else {
        _modules.child_process = childProcess;
    }
    _modules['node:child_process'] = _modules.child_process;

