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
                let port, host, callback;
                if (typeof optionsOrPort === 'object') {
                    port = optionsOrPort.port;
                    host = optionsOrPort.host || '127.0.0.1';
                    callback = hostOrCallback;
                } else {
                    port = optionsOrPort;
                    host = typeof hostOrCallback === 'string' ? hostOrCallback : '127.0.0.1';
                    callback = typeof hostOrCallback === 'function' ? hostOrCallback : maybeCallback;
                }

                this.connecting = true;
                this.remotePort = port;
                this.remoteAddress = host;
                if (callback) this.once('connect', callback);

                setTimeout(() => {
                    try {
                        this._createSocket();
                        const result = __edgebox_socket_connect(this._socketId, port);
                        if (result < 0) {
                            this.connecting = false;
                            this.emit('error', new Error(`Connection failed: ${result}`));
                            return;
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
                const result = __edgebox_socket_write(this._socketId, str);
                if (result < 0) {
                    const err = new Error(`Write failed: ${result}`);
                    if (callback) setTimeout(() => callback(err), 0);
                    this.emit('error', err);
                    return false;
                }
                this.bytesWritten += result;
                if (callback) setTimeout(callback, 0);
                return true;
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
            setKeepAlive(enable, delay) { return this; }
            setNoDelay(noDelay) { return this; }
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
                let port, host, backlog, callback;

                if (typeof optionsOrPort === 'object') {
                    port = optionsOrPort.port;
                    host = optionsOrPort.host || '0.0.0.0';
                    backlog = optionsOrPort.backlog || 511;
                    callback = hostOrBacklogOrCallback;
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

                setTimeout(() => {
                    try {
                        this._socketId = __edgebox_socket_create();
                        if (this._socketId < 0) {
                            throw new Error('Failed to create server socket');
                        }

                        const bindResult = __edgebox_socket_bind(this._socketId, port);
                        if (bindResult < 0) {
                            throw new Error(`Failed to bind to port ${port}: ${bindResult}`);
                        }

                        const listenResult = __edgebox_socket_listen(this._socketId, backlog);
                        if (listenResult < 0) {
                            throw new Error(`Failed to listen: ${listenResult}`);
                        }

                        this.listening = true;
                        this._port = port;
                        this._host = host;

                        // Start polling for connections
                        this._acceptPollInterval = setInterval(() => {
                            if (!this.listening || this._socketId === null) {
                                this._stopAcceptPolling();
                                return;
                            }
                            const clientSocketId = __edgebox_socket_accept(this._socketId);
                            if (clientSocketId > 0) {
                                const clientSocket = new Socket({ fd: clientSocketId });
                                clientSocket.remotePort = port;
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
            connect: function(options, callback) {
                const socket = new Socket();
                socket.connect(options, callback);
                return socket;
            },
            createConnection: function(options, callback) {
                return this.connect(options, callback);
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

