    // ===== TLS MODULE (lazy loaded) =====
    // Native TLS 1.3 implementation using Zig crypto
    _lazyModule('tls', function() {
        // Check if native TLS is available
        var hasNativeTls = typeof __edgebox_tls_connect === 'function';

        // TLS connection states
        var TLS_STATE = { CLOSED: 0, CONNECTING: 1, CONNECTED: 2 };

        class TLSSocket extends EventEmitter {
            constructor(socket, options) {
                super();
                options = options || {};
                this._tlsId = null;
                this._readPollInterval = null;
                this.authorized = false;
                this.encrypted = true;
                this.alpnProtocol = null;
                this.remoteAddress = null;
                this.remotePort = null;
                this.destroyed = false;
                this.readable = true;
                this.writable = true;
                this._servername = options.servername || null;
                this._rejectUnauthorized = options.rejectUnauthorized !== false; // Default: true
            }

            _startReadPolling() {
                if (this._readPollInterval) return;
                var self = this;
                this._readPollInterval = setInterval(function() {
                    if (self.destroyed || self._tlsId === null) {
                        self._stopReadPolling();
                        return;
                    }
                    var state = __edgebox_tls_state(self._tlsId);
                    if (state === TLS_STATE.CLOSED) {
                        self._stopReadPolling();
                        self.emit('end');
                        self.emit('close', false);
                        return;
                    }
                    if (state !== TLS_STATE.CONNECTED) return;

                    var data = __edgebox_tls_read(self._tlsId, 65536);
                    if (data === null) {
                        self._stopReadPolling();
                        self.readable = false;
                        self.emit('end');
                        self.emit('close', false);
                        return;
                    }
                    if (data !== undefined && data.length > 0) {
                        self.emit('data', Buffer.from(data));
                    }
                }, 10);
            }

            _stopReadPolling() {
                if (this._readPollInterval) {
                    clearInterval(this._readPollInterval);
                    this._readPollInterval = null;
                }
            }

            connect(optionsOrPort, hostOrCallback, callback) {
                var self = this;
                var port, host, servername, rejectUnauthorized;

                if (typeof optionsOrPort === 'object') {
                    port = optionsOrPort.port;
                    host = optionsOrPort.host || 'localhost';
                    servername = optionsOrPort.servername || host;
                    rejectUnauthorized = optionsOrPort.rejectUnauthorized !== false;
                    callback = hostOrCallback;
                } else {
                    port = optionsOrPort;
                    host = typeof hostOrCallback === 'string' ? hostOrCallback : 'localhost';
                    servername = host;
                    rejectUnauthorized = this._rejectUnauthorized;
                    callback = typeof hostOrCallback === 'function' ? hostOrCallback : callback;
                }

                this.remoteAddress = host;
                this.remotePort = port;
                this._servername = servername;
                this._rejectUnauthorized = rejectUnauthorized;

                if (callback) this.once('secureConnect', callback);

                setTimeout(function() {
                    if (!hasNativeTls) {
                        self.emit('error', new Error('TLS not available in this environment'));
                        return;
                    }

                    // Pass rejectUnauthorized flag to native TLS connect
                    var rejectFlag = self._rejectUnauthorized ? 1 : 0;
                    var result = __edgebox_tls_connect(servername, port, rejectFlag);
                    if (result < 0) {
                        var errMsg = result === -5 ? 'DNS resolution failed for ' + host
                                   : result === -7 ? 'Connection refused'
                                   : result === -8 ? 'TLS handshake failed'
                                   : result === -9 ? 'Certificate validation failed'
                                   : 'TLS connection failed: ' + result;
                        var err = new Error(errMsg);
                        err.code = result === -5 ? 'ENOTFOUND'
                                 : result === -7 ? 'ECONNREFUSED'
                                 : result === -9 ? 'UNABLE_TO_VERIFY_LEAF_SIGNATURE'
                                 : 'ETLSFAILED';
                        self.emit('error', err);
                        return;
                    }

                    self._tlsId = result;
                    self.authorized = self._rejectUnauthorized; // Only authorized if validation was performed
                    self._startReadPolling();
                    self.emit('connect');
                    self.emit('secureConnect');
                    self.emit('ready');
                }, 0);

                return this;
            }

            write(data, encoding, callback) {
                if (typeof encoding === 'function') {
                    callback = encoding;
                    encoding = null;
                }
                if (this.destroyed || !this.writable || this._tlsId === null) {
                    var err = new Error('Socket is not writable');
                    if (callback) setTimeout(function() { callback(err); }, 0);
                    return false;
                }

                var str = typeof data === 'string' ? data : data.toString(encoding || 'utf8');
                var result = __edgebox_tls_write(this._tlsId, str);

                if (result < 0) {
                    var writeErr = new Error('TLS write failed: ' + result);
                    if (callback) setTimeout(function() { callback(writeErr); }, 0);
                    this.emit('error', writeErr);
                    return false;
                }

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

                if (callback) this.once('finish', callback);
                var self = this;
                setTimeout(function() { self.emit('finish'); }, 0);
                return this;
            }

            destroy(error) {
                if (this.destroyed) return this;
                this.destroyed = true;
                this.readable = false;
                this.writable = false;
                this._stopReadPolling();

                if (this._tlsId !== null) {
                    __edgebox_tls_close(this._tlsId);
                    this._tlsId = null;
                }

                if (error) this.emit('error', error);
                this.emit('close', !!error);
                return this;
            }

            setEncoding(encoding) {
                this._encoding = encoding;
                return this;
            }

            getPeerCertificate(detailed) { return {}; }
            getCipher() { return { name: 'TLS_AES_128_GCM_SHA256', version: 'TLSv1.3' }; }
            getProtocol() { return 'TLSv1.3'; }
            getSession() { return null; }
            getTLSTicket() { return null; }
            isSessionReused() { return false; }
            setMaxSendFragment(size) { return true; }
            setServername(name) { this._servername = name; }
            exportKeyingMaterial(length, label, context) { return Buffer.alloc(0); }
            ref() { return this; }
            unref() { return this; }
        }

        // TLS Server class
        class TLSServer extends EventEmitter {
            constructor(options, secureConnectionListener) {
                super();
                this._options = options || {};
                this._serverId = null;
                this._server = null;
                this.listening = false;

                if (secureConnectionListener) {
                    this.on('secureConnection', secureConnectionListener);
                }
            }

            listen(optionsOrPort, hostOrCallback, callback) {
                var self = this;
                var port, host;

                if (typeof optionsOrPort === 'object') {
                    port = optionsOrPort.port;
                    host = optionsOrPort.host || '0.0.0.0';
                    callback = hostOrCallback;
                } else {
                    port = optionsOrPort;
                    host = typeof hostOrCallback === 'string' ? hostOrCallback : '0.0.0.0';
                    callback = typeof hostOrCallback === 'function' ? hostOrCallback : callback;
                }

                if (callback) this.once('listening', callback);

                // Check if native TLS server functions are available
                if (typeof __edgebox_tls_create_server !== 'function') {
                    setTimeout(function() {
                        self.emit('error', new Error('TLS server not available in this environment'));
                    }, 0);
                    return this;
                }

                // Get certificate and key from options
                var cert = this._options.cert;
                var key = this._options.key;

                if (!cert || !key) {
                    setTimeout(function() {
                        self.emit('error', new Error('Certificate and key required for TLS server'));
                    }, 0);
                    return this;
                }

                // Convert to string if Buffer
                if (Buffer.isBuffer(cert)) cert = cert.toString();
                if (Buffer.isBuffer(key)) key = key.toString();

                // Create TLS server context
                var serverId = __edgebox_tls_create_server(cert, key);
                if (serverId < 0) {
                    setTimeout(function() {
                        var errMsg = serverId === -2 ? 'No server slots available'
                                   : serverId === -3 ? 'Invalid certificate'
                                   : serverId === -4 ? 'Invalid private key'
                                   : 'Failed to create TLS server: ' + serverId;
                        self.emit('error', new Error(errMsg));
                    }, 0);
                    return this;
                }

                this._serverId = serverId;

                // Create underlying TCP server
                var net = _modules.net;
                this._server = net.createServer(function(socket) {
                    // Perform TLS handshake on the connection
                    var tlsId = __edgebox_tls_accept(self._serverId, socket._socketId);
                    if (tlsId < 0) {
                        var errMsg = tlsId === -4 ? 'Timeout during handshake'
                                   : tlsId === -7 ? 'TLS handshake failed'
                                   : tlsId === -8 ? 'Invalid ClientHello'
                                   : 'TLS accept failed: ' + tlsId;
                        socket.destroy(new Error(errMsg));
                        return;
                    }

                    // Create TLS socket wrapper
                    var tlsSocket = new TLSSocket(socket, self._options);
                    tlsSocket._tlsId = tlsId;
                    tlsSocket.authorized = true;
                    tlsSocket.encrypted = true;
                    tlsSocket.remoteAddress = socket.remoteAddress;
                    tlsSocket.remotePort = socket.remotePort;

                    // Start read polling for the TLS socket
                    tlsSocket._startReadPolling();

                    // Emit secureConnection event
                    self.emit('secureConnection', tlsSocket);
                });

                this._server.on('error', function(err) {
                    self.emit('error', err);
                });

                this._server.on('listening', function() {
                    self.listening = true;
                    self.emit('listening');
                });

                this._server.listen(port, host);
                return this;
            }

            close(callback) {
                if (callback) this.once('close', callback);

                if (this._server) {
                    this._server.close();
                }

                if (this._serverId !== null && typeof __edgebox_tls_destroy_server === 'function') {
                    __edgebox_tls_destroy_server(this._serverId);
                    this._serverId = null;
                }

                this.listening = false;
                var self = this;
                setTimeout(function() { self.emit('close'); }, 0);
                return this;
            }

            address() {
                if (this._server) {
                    return this._server.address();
                }
                return null;
            }

            getConnections(callback) {
                if (this._server) {
                    return this._server.getConnections(callback);
                }
                if (callback) setTimeout(function() { callback(null, 0); }, 0);
                return this;
            }

            ref() { return this; }
            unref() { return this; }
        }

        return {
            TLSSocket: TLSSocket,
            Server: TLSServer,
            connect: function(optionsOrPort, hostOrCallback, callback) {
                var socket = new TLSSocket(null, typeof optionsOrPort === 'object' ? optionsOrPort : {});
                socket.connect(optionsOrPort, hostOrCallback, callback);
                return socket;
            },
            createServer: function(options, secureConnectionListener) {
                if (typeof options === 'function') {
                    secureConnectionListener = options;
                    options = {};
                }
                return new TLSServer(options || {}, secureConnectionListener);
            },
            createSecureContext: function(options) {
                return { context: {} };
            },
            getCiphers: function() {
                return ['TLS_AES_256_GCM_SHA384', 'TLS_AES_128_GCM_SHA256'];
            },
            DEFAULT_ECDH_CURVE: 'auto',
            DEFAULT_MAX_VERSION: 'TLSv1.3',
            DEFAULT_MIN_VERSION: 'TLSv1.2',
            rootCertificates: []
        };
    });

