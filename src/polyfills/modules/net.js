    // ===== NET MODULE (lazy loaded) =====
    _lazyModule('net', function() {
        // Socket states: 0=created, 1=bound, 2=listening, 3=connected, 4=closed
        const SOCKET_STATE = { CREATED: 0, BOUND: 1, LISTENING: 2, CONNECTED: 3, CLOSED: 4 };

        class Socket extends EventEmitter {
            constructor(options = {}) {
                super();
                this._socketId = null;
                this._encoding = null;
                this._readPollInterval = null;
                this.connecting = false;
                this.destroyed = false;
                this.readable = true;
                this.writable = true;
                this.remoteAddress = '127.0.0.1';
                this.remotePort = null;
                this.localAddress = '127.0.0.1';
                this.localPort = null;
                this.bytesRead = 0;
                this.bytesWritten = 0;
                this.pending = true;
                this.readyState = 'opening';

                // If fd provided, wrap existing socket
                if (options.fd !== undefined) {
                    this._socketId = options.fd;
                    this.pending = false;
                    this.readyState = 'open';
                    this._startReadPolling();
                }
            }

            _createSocket() {
                if (this._socketId === null) {
                    this._socketId = __edgebox_socket_create();
                    if (this._socketId < 0) {
                        throw new Error('Failed to create socket');
                    }
                }
            }

            _startReadPolling() {
                if (this._readPollInterval) return;
                this._readPollInterval = setInterval(() => {
                    if (this.destroyed || !this._socketId) {
                        this._stopReadPolling();
                        return;
                    }
                    const state = __edgebox_socket_state(this._socketId);
                    if (state === SOCKET_STATE.CLOSED) {
                        this._stopReadPolling();
                        this.emit('end');
                        this.emit('close', false);
                        return;
                    }
                    if (state !== SOCKET_STATE.CONNECTED) return;

                    const data = __edgebox_socket_read(this._socketId, 65536);
                    if (data === null) {
                        // EOF - peer closed connection
                        this._stopReadPolling();
                        this.readable = false;
                        this.emit('end');
                        this.emit('close', false);
                        return;
                    }
                    if (data && data.length > 0) {
                        this.bytesRead += data.length;
                        const chunk = this._encoding ? data : Buffer.from(data);
                        this.emit('data', chunk);
                    }
                }, 10);
            }

            _stopReadPolling() {
                if (this._readPollInterval) {
                    clearInterval(this._readPollInterval);
                    this._readPollInterval = null;
                }
            }

            connect(optionsOrPort, hostOrCallback, maybeCallback) {
                let port, host, path, callback;
                if (typeof optionsOrPort === 'object') {
                    port = optionsOrPort.port;
                    host = optionsOrPort.host || '127.0.0.1';
                    path = optionsOrPort.path; // Unix domain socket path
                    callback = hostOrCallback;
                } else if (typeof optionsOrPort === 'string' && optionsOrPort.startsWith('/')) {
                    // Unix socket path as first argument
                    path = optionsOrPort;
                    callback = hostOrCallback;
                } else {
                    port = optionsOrPort;
                    host = typeof hostOrCallback === 'string' ? hostOrCallback : '127.0.0.1';
                    callback = typeof hostOrCallback === 'function' ? hostOrCallback : maybeCallback;
                }

                this.connecting = true;
                this._isUnixSocket = !!path;
                if (path) {
                    this.remoteAddress = path;
                } else {
                    this.remotePort = port;
                    this.remoteAddress = host;
                }
                if (callback) this.once('connect', callback);

                setTimeout(() => {
                    try {
                        // Create appropriate socket type
                        if (path) {
                            // Unix domain socket
                            if (typeof __edgebox_socket_create_unix !== 'function') {
                                throw new Error('Unix sockets not supported');
                            }
                            this._socketId = __edgebox_socket_create_unix();
                            if (this._socketId < 0) {
                                throw new Error('Failed to create Unix socket');
                            }
                            const result = __edgebox_socket_connect_unix(this._socketId, path);
                            if (result < 0) {
                                this.connecting = false;
                                const err = new Error('Unix socket connection failed: ' + result);
                                err.code = 'ECONNREFUSED';
                                this.emit('error', err);
                                return;
                            }
                        } else {
                            // TCP socket
                            this._createSocket();
                            const result = __edgebox_socket_connect(this._socketId, port, host);
                            if (result < 0) {
                                this.connecting = false;
                                const errorMsg = result === -2 ? 'DNS resolution failed for ' + host
                                              : result === -3 ? 'Connection refused'
                                              : result === -4 ? 'Socket error'
                                              : 'Connection failed: ' + result;
                                const err = new Error(errorMsg);
                                err.code = result === -2 ? 'ENOTFOUND' : result === -3 ? 'ECONNREFUSED' : 'ECONNFAILED';
                                this.emit('error', err);
                                return;
                            }
                        }
                        this.connecting = false;
                        this.pending = false;
                        this.readyState = 'open';
                        this._startReadPolling();
                        this.emit('connect');
                        this.emit('ready');
                    } catch (err) {
                        this.connecting = false;
                        this.emit('error', err);
                    }
                }, 0);

                return this;
            }

            write(data, encoding, callback) {
                if (typeof encoding === 'function') {
                    callback = encoding;
                    encoding = null;
                }
                if (this.destroyed || !this.writable) {
                    const err = new Error('Socket is not writable');
                    if (callback) setTimeout(() => callback(err), 0);
                    return false;
                }
                if (!this._socketId) {
                    const err = new Error('Socket not connected');
                    if (callback) setTimeout(() => callback(err), 0);
                    return false;
                }

                const str = typeof data === 'string' ? data : data.toString(encoding || 'utf8');

                // Track buffer for backpressure
                if (!this._writableState) {
                    this._writableState = {
                        length: 0,
                        highWaterMark: 16 * 1024, // 16KB default
                        needDrain: false
                    };
                }

                const result = __edgebox_socket_write(this._socketId, str);
                if (result < 0) {
                    const err = new Error(`Write failed: ${result}`);
                    if (callback) setTimeout(() => callback(err), 0);
                    this.emit('error', err);
                    return false;
                }

                this.bytesWritten += result;

                // Track pending bytes for backpressure
                const pendingBytes = typeof __edgebox_socket_pending_bytes === 'function'
                    ? __edgebox_socket_pending_bytes(this._socketId)
                    : 0;

                this._writableState.length = pendingBytes;

                // Schedule drain event if buffer was above threshold and is now empty
                if (this._writableState.needDrain && pendingBytes === 0) {
                    this._writableState.needDrain = false;
                    setTimeout(() => this.emit('drain'), 0);
                }

                if (callback) setTimeout(callback, 0);

                // Return false (backpressure) if buffer exceeds highWaterMark
                const shouldApplyBackpressure = pendingBytes > this._writableState.highWaterMark;
                if (shouldApplyBackpressure) {
                    this._writableState.needDrain = true;
                }
                return !shouldApplyBackpressure;
            }

            end(data, encoding, callback) {
                if (typeof data === 'function') {
                    callback = data;
                    data = null;
                } else if (typeof encoding === 'function') {
                    callback = encoding;
                    encoding = null;
                }

                if (data) this.write(data, encoding);
                this.writable = false;
                this.readyState = 'writeOnly';

                if (callback) this.once('finish', callback);
                setTimeout(() => this.emit('finish'), 0);
                return this;
            }

            destroy(error) {
                if (this.destroyed) return this;
                this.destroyed = true;
                this.readable = false;
                this.writable = false;
                this.readyState = 'closed';
                this._stopReadPolling();

                if (this._socketId !== null) {
                    __edgebox_socket_close(this._socketId);
                    this._socketId = null;
                }

                if (error) this.emit('error', error);
                this.emit('close', !!error);
                return this;
            }

            setEncoding(encoding) {
                this._encoding = encoding;
                return this;
            }
            setKeepAlive(enable, delay) {
                if (this._socketId !== null && typeof __edgebox_socket_set_keepalive === 'function') {
                    __edgebox_socket_set_keepalive(this._socketId, enable ? 1 : 0, delay || 0);
                }
                return this;
            }
            setNoDelay(noDelay) {
                if (this._socketId !== null && typeof __edgebox_socket_set_nodelay === 'function') {
                    __edgebox_socket_set_nodelay(this._socketId, noDelay !== false ? 1 : 0);
                }
                return this;
            }
            setTimeout(timeout, callback) {
                if (callback) this.once('timeout', callback);
                return this;
            }
            ref() { return this; }
            unref() { return this; }
            address() {
                return { port: this.localPort, address: this.localAddress, family: 'IPv4' };
            }
            pause() { this._stopReadPolling(); return this; }
            resume() { this._startReadPolling(); return this; }
        }

        class Server extends EventEmitter {
            constructor(options, connectionListener) {
                super();
                if (typeof options === 'function') {
                    connectionListener = options;
                    options = {};
                }
                this._options = options || {};
                this._socketId = null;
                this._acceptPollInterval = null;
                this._connections = new Set();
                this.listening = false;
                this.maxConnections = 0;

                if (connectionListener) {
                    this.on('connection', connectionListener);
                }
            }

            listen(optionsOrPort, hostOrBacklogOrCallback, backlogOrCallback, maybeCallback) {
                let port, host, path, backlog, callback;

                if (typeof optionsOrPort === 'object') {
                    port = optionsOrPort.port;
                    host = optionsOrPort.host || '0.0.0.0';
                    path = optionsOrPort.path; // Unix domain socket path
                    backlog = optionsOrPort.backlog || 511;
                    callback = hostOrBacklogOrCallback;
                } else if (typeof optionsOrPort === 'string' && optionsOrPort.startsWith('/')) {
                    // Unix socket path as first argument
                    path = optionsOrPort;
                    backlog = typeof hostOrBacklogOrCallback === 'number' ? hostOrBacklogOrCallback : 511;
                    callback = typeof hostOrBacklogOrCallback === 'function' ? hostOrBacklogOrCallback : backlogOrCallback;
                } else {
                    port = optionsOrPort;
                    if (typeof hostOrBacklogOrCallback === 'string') {
                        host = hostOrBacklogOrCallback;
                        backlog = typeof backlogOrCallback === 'number' ? backlogOrCallback : 511;
                        callback = typeof backlogOrCallback === 'function' ? backlogOrCallback : maybeCallback;
                    } else if (typeof hostOrBacklogOrCallback === 'number') {
                        host = '0.0.0.0';
                        backlog = hostOrBacklogOrCallback;
                        callback = backlogOrCallback;
                    } else {
                        host = '0.0.0.0';
                        backlog = 511;
                        callback = hostOrBacklogOrCallback;
                    }
                }

                if (callback) this.once('listening', callback);
                this._isUnixSocket = !!path;

                setTimeout(() => {
                    try {
                        if (path) {
                            // Unix domain socket server
                            if (typeof __edgebox_socket_create_unix !== 'function') {
                                throw new Error('Unix sockets not supported');
                            }
                            this._socketId = __edgebox_socket_create_unix();
                            if (this._socketId < 0) {
                                throw new Error('Failed to create Unix server socket');
                            }

                            const bindResult = __edgebox_socket_bind_unix(this._socketId, path);
                            if (bindResult < 0) {
                                throw new Error(`Failed to bind to path ${path}: ${bindResult}`);
                            }

                            this._path = path;
                        } else {
                            // TCP socket server
                            this._socketId = __edgebox_socket_create();
                            if (this._socketId < 0) {
                                throw new Error('Failed to create server socket');
                            }

                            const bindResult = __edgebox_socket_bind(this._socketId, port);
                            if (bindResult < 0) {
                                throw new Error(`Failed to bind to port ${port}: ${bindResult}`);
                            }

                            this._port = port;
                            this._host = host;
                        }

                        const listenResult = __edgebox_socket_listen(this._socketId, backlog);
                        if (listenResult < 0) {
                            throw new Error(`Failed to listen: ${listenResult}`);
                        }

                        this.listening = true;

                        // Start polling for connections
                        this._acceptPollInterval = setInterval(() => {
                            if (!this.listening || this._socketId === null) {
                                this._stopAcceptPolling();
                                return;
                            }
                            const clientSocketId = __edgebox_socket_accept(this._socketId);
                            if (clientSocketId > 0) {
                                const clientSocket = new Socket({ fd: clientSocketId });
                                clientSocket._isUnixSocket = this._isUnixSocket;
                                if (this._isUnixSocket) {
                                    clientSocket.remoteAddress = path;
                                } else {
                                    clientSocket.remotePort = port;
                                }
                                this._connections.add(clientSocket);
                                clientSocket.on('close', () => this._connections.delete(clientSocket));
                                this.emit('connection', clientSocket);
                            }
                        }, 10);

                        this.emit('listening');
                    } catch (err) {
                        this.emit('error', err);
                    }
                }, 0);

                return this;
            }

            _stopAcceptPolling() {
                if (this._acceptPollInterval) {
                    clearInterval(this._acceptPollInterval);
                    this._acceptPollInterval = null;
                }
            }

            close(callback) {
                if (callback) this.once('close', callback);

                this._stopAcceptPolling();
                this.listening = false;

                // Close all connections
                for (const conn of this._connections) {
                    conn.destroy();
                }
                this._connections.clear();

                if (this._socketId !== null) {
                    __edgebox_socket_close(this._socketId);
                    this._socketId = null;
                }

                setTimeout(() => this.emit('close'), 0);
                return this;
            }

            address() {
                if (!this.listening) return null;
                if (this._isUnixSocket) {
                    return this._path;
                }
                return { port: this._port, address: this._host, family: 'IPv4' };
            }

            getConnections(callback) {
                const count = this._connections.size;
                if (callback) setTimeout(() => callback(null, count), 0);
                return this;
            }

            ref() { return this; }
            unref() { return this; }
        }

        return {
            Socket,
            Server,
            connect: function(optionsOrPathOrPort, hostOrCallback, callback) {
                const socket = new Socket();
                let options;
                let connectCallback;

                if (typeof optionsOrPathOrPort === 'object') {
                    options = optionsOrPathOrPort;
                    connectCallback = hostOrCallback;
                } else if (typeof optionsOrPathOrPort === 'string' && optionsOrPathOrPort.startsWith('/')) {
                    // Unix socket path
                    options = { path: optionsOrPathOrPort };
                    connectCallback = hostOrCallback;
                } else {
                    // Port (and optionally host)
                    options = { port: optionsOrPathOrPort };
                    if (typeof hostOrCallback === 'string') {
                        options.host = hostOrCallback;
                        connectCallback = callback;
                    } else {
                        connectCallback = hostOrCallback;
                    }
                }

                socket.connect(options, connectCallback);
                return socket;
            },
            createConnection: function(optionsOrPathOrPort, hostOrCallback, callback) {
                return this.connect(optionsOrPathOrPort, hostOrCallback, callback);
            },
            createServer: function(options, connectionListener) {
                return new Server(options, connectionListener);
            },
            isIP: function(input) {
                // IPv4: validate format and octet range (0-255)
                if (/^(\d{1,3}\.){3}\d{1,3}$/.test(input)) {
                    var parts = input.split('.');
                    if (parts.every(function(p) { return parseInt(p, 10) <= 255; })) {
                        return 4;
                    }
                }
                // IPv6: must have at least 2 colons (e.g., "::1", "2001:db8::1")
                // Security: Also validate only hex digits and colons allowed, no triple colons
                if (typeof input === 'string' && input.includes(':') && (input.match(/:/g) || []).length >= 2) {
                    // Must only contain hex digits (0-9, a-f, A-F) and colons
                    if (/^[0-9a-fA-F:]+$/.test(input) && !/:::/.test(input)) {
                        return 6;
                    }
                }
                return 0;
            },
            isIPv4: function(input) { return this.isIP(input) === 4; },
            isIPv6: function(input) { return this.isIP(input) === 6; },
        };
    });

