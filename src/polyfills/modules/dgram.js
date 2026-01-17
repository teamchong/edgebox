    // ===== DGRAM MODULE (lazy loaded) =====
    // UDP sockets implemented via Unix domain sockets (SOCK_DGRAM)
    _lazyModule('dgram', function() {
        class DgramSocket extends EventEmitter {
            constructor(type, callback) {
                super();
                var self = this;
                this._type = type || 'udp4';
                this._bound = false;
                this._socketId = null;
                this._port = null;
                this._address = '0.0.0.0';

                if (callback) this.on('message', callback);

                // Create socket
                if (typeof __edgebox_socket_create === 'function') {
                    this._socketId = __edgebox_socket_create();
                    if (this._socketId < 0) {
                        setTimeout(function() {
                            self.emit('error', new Error('Failed to create UDP socket'));
                        }, 0);
                    }
                }
            }

            bind(port, address, callback) {
                var self = this;
                if (typeof port === 'object') {
                    var options = port;
                    port = options.port;
                    address = options.address;
                    callback = address;
                }
                if (typeof address === 'function') {
                    callback = address;
                    address = '0.0.0.0';
                }

                this._port = port || 0;
                this._address = address || '0.0.0.0';

                if (callback) this.once('listening', callback);

                setTimeout(function() {
                    if (self._socketId !== null && typeof __edgebox_socket_bind === 'function') {
                        var result = __edgebox_socket_bind(self._socketId, self._port);
                        if (result < 0) {
                            self.emit('error', new Error('Failed to bind UDP socket: ' + result));
                            return;
                        }
                    }
                    self._bound = true;
                    self.emit('listening');

                    // Start receiving messages
                    self._startReceiving();
                }, 0);

                return self;
            }

            _startReceiving() {
                var self = this;
                if (!this._socketId) return;

                this._recvInterval = setInterval(function() {
                    if (!self._bound || !self._socketId) {
                        self._stopReceiving();
                        return;
                    }
                    if (typeof __edgebox_socket_read === 'function') {
                        var data = __edgebox_socket_read(self._socketId, 65536);
                        if (data === null) {
                            // Socket closed
                            self._stopReceiving();
                            self.emit('close');
                            return;
                        }
                        if (data && data.length > 0) {
                            var msg = Buffer.from(data);
                            var rinfo = { address: self._address, family: self._type === 'udp6' ? 'IPv6' : 'IPv4', port: self._port, size: msg.length };
                            self.emit('message', msg, rinfo);
                        }
                    }
                }, 10);
            }

            _stopReceiving() {
                if (this._recvInterval) {
                    clearInterval(this._recvInterval);
                    this._recvInterval = null;
                }
            }

            send(msg, offset, length, port, address, callback) {
                var self = this;

                // Handle different argument patterns
                if (typeof offset === 'number' && typeof length === 'number') {
                    // Full signature: msg, offset, length, port, address, callback
                } else if (typeof offset === 'number') {
                    // msg, port, address, callback
                    callback = address;
                    address = length;
                    port = offset;
                    offset = 0;
                    length = msg.length;
                } else if (Array.isArray(msg)) {
                    // Array of buffers
                    msg = Buffer.concat(msg);
                    port = offset;
                    address = length;
                    callback = port;
                    offset = 0;
                    length = msg.length;
                }

                if (typeof callback !== 'function') callback = null;

                var buf = Buffer.isBuffer(msg) ? msg : Buffer.from(msg);
                var data = buf.toString().substring(offset, offset + length);

                setTimeout(function() {
                    if (self._socketId !== null && typeof __edgebox_socket_write === 'function') {
                        var result = __edgebox_socket_write(self._socketId, data);
                        if (result < 0) {
                            var err = new Error('Send failed: ' + result);
                            if (callback) callback(err);
                            else self.emit('error', err);
                            return;
                        }
                    }
                    if (callback) callback(null);
                }, 0);
            }

            close(callback) {
                var self = this;
                this._stopReceiving();
                this._bound = false;

                if (this._socketId !== null && typeof __edgebox_socket_close === 'function') {
                    __edgebox_socket_close(this._socketId);
                    this._socketId = null;
                }

                if (callback) this.once('close', callback);
                setTimeout(function() { self.emit('close'); }, 0);
            }

            address() {
                return { address: this._address, family: this._type === 'udp6' ? 'IPv6' : 'IPv4', port: this._port };
            }

            setBroadcast(flag) { return this; }
            setMulticastTTL(ttl) { return this; }
            setMulticastLoopback(flag) { return this; }
            setTTL(ttl) { return this; }
            addMembership(multicastAddress, multicastInterface) { return this; }
            dropMembership(multicastAddress, multicastInterface) { return this; }
            addSourceSpecificMembership(sourceAddress, groupAddress, multicastInterface) { return this; }
            dropSourceSpecificMembership(sourceAddress, groupAddress, multicastInterface) { return this; }
            setMulticastInterface(multicastInterface) { return this; }
            setRecvBufferSize(size) { return this; }
            setSendBufferSize(size) { return this; }
            getRecvBufferSize() { return 65536; }
            getSendBufferSize() { return 65536; }
            ref() { return this; }
            unref() { return this; }
            remoteAddress() { return undefined; }
            connect(port, address, callback) {
                if (callback) setTimeout(callback, 0);
            }
            disconnect() {}
        }

        return {
            createSocket: function(type, callback) {
                if (typeof type === 'object') {
                    return new DgramSocket(type.type, callback);
                }
                return new DgramSocket(type, callback);
            },
            Socket: DgramSocket
        };
    });

