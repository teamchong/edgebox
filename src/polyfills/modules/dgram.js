    // ===== DGRAM MODULE (lazy loaded) =====
    // UDP sockets implemented via native SOCK_DGRAM support
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
                this._connected = false;
                this._connectedPort = null;
                this._connectedAddress = null;

                if (callback) this.on('message', callback);

                // Create UDP socket with the correct type
                if (typeof __edgebox_udp_socket_create === 'function') {
                    this._socketId = __edgebox_udp_socket_create(this._type);
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
                    address = this._type === 'udp6' ? '::' : '0.0.0.0';
                }

                this._port = port || 0;
                this._address = address || (this._type === 'udp6' ? '::' : '0.0.0.0');

                if (callback) this.once('listening', callback);

                setTimeout(function() {
                    if (self._socketId !== null && typeof __edgebox_udp_socket_bind === 'function') {
                        var result = __edgebox_udp_socket_bind(self._socketId, self._port, self._address);
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
                if (this._socketId === null) return;

                this._recvInterval = setInterval(function() {
                    if (!self._bound || self._socketId === null) {
                        self._stopReceiving();
                        return;
                    }
                    if (typeof __edgebox_udp_socket_recv === 'function') {
                        var result = __edgebox_udp_socket_recv(self._socketId, 65536);
                        if (result === null) {
                            // Socket closed
                            self._stopReceiving();
                            self.emit('close');
                            return;
                        }
                        if (result && result.data) {
                            var msg = Buffer.from(result.data);
                            var rinfo = {
                                address: result.address,
                                family: result.family,
                                port: result.port,
                                size: result.size
                            };
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
                if (typeof offset === 'number' && typeof length === 'number' && typeof port === 'number') {
                    // Full signature: msg, offset, length, port, address, callback
                } else if (typeof offset === 'number' && typeof length === 'string') {
                    // msg, port, address, callback
                    callback = port;
                    address = length;
                    port = offset;
                    offset = 0;
                    length = msg.length;
                } else if (typeof offset === 'number') {
                    // msg, port, address, callback (with address being callback sometimes)
                    if (typeof length === 'function') {
                        callback = length;
                        address = this._connectedAddress;
                        port = offset;
                        offset = 0;
                        length = msg.length;
                    } else {
                        callback = address;
                        address = length;
                        port = offset;
                        offset = 0;
                        length = msg.length;
                    }
                } else if (Array.isArray(msg)) {
                    // Array of buffers
                    msg = Buffer.concat(msg);
                    callback = port;
                    address = length;
                    port = offset;
                    offset = 0;
                    length = msg.length;
                }

                // Use connected address/port if not provided
                if (!address && this._connected) {
                    address = this._connectedAddress;
                }
                if (!port && this._connected) {
                    port = this._connectedPort;
                }

                if (!address || !port) {
                    var err = new Error('send EDESTADDRREQ');
                    if (callback) {
                        setTimeout(function() { callback(err); }, 0);
                    } else {
                        setTimeout(function() { self.emit('error', err); }, 0);
                    }
                    return;
                }

                if (typeof callback !== 'function') callback = null;

                var buf = Buffer.isBuffer(msg) ? msg : Buffer.from(msg);
                var data = buf.slice(offset, offset + length).toString();

                setTimeout(function() {
                    if (self._socketId !== null && typeof __edgebox_udp_socket_send === 'function') {
                        var result = __edgebox_udp_socket_send(self._socketId, data, port, address);
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
                this._connected = false;

                if (this._socketId !== null && typeof __edgebox_udp_socket_close === 'function') {
                    __edgebox_udp_socket_close(this._socketId);
                    this._socketId = null;
                }

                if (callback) this.once('close', callback);
                setTimeout(function() { self.emit('close'); }, 0);
            }

            address() {
                return {
                    address: this._address,
                    family: this._type === 'udp6' ? 'IPv6' : 'IPv4',
                    port: this._port
                };
            }

            setBroadcast(flag) {
                if (this._socketId !== null && typeof __edgebox_udp_set_broadcast === 'function') {
                    __edgebox_udp_set_broadcast(this._socketId, !!flag);
                }
                return this;
            }

            setMulticastTTL(ttl) {
                if (this._socketId !== null && typeof __edgebox_udp_set_multicast_ttl === 'function') {
                    __edgebox_udp_set_multicast_ttl(this._socketId, ttl);
                }
                return this;
            }

            setMulticastLoopback(flag) {
                if (this._socketId !== null && typeof __edgebox_udp_set_multicast_loopback === 'function') {
                    __edgebox_udp_set_multicast_loopback(this._socketId, !!flag);
                }
                return this;
            }

            setTTL(ttl) {
                if (this._socketId !== null && typeof __edgebox_udp_set_ttl === 'function') {
                    __edgebox_udp_set_ttl(this._socketId, ttl);
                }
                return this;
            }

            addMembership(multicastAddress, multicastInterface) {
                if (this._socketId !== null && typeof __edgebox_udp_add_membership === 'function') {
                    __edgebox_udp_add_membership(this._socketId, multicastAddress, multicastInterface);
                }
                return this;
            }

            dropMembership(multicastAddress, multicastInterface) {
                if (this._socketId !== null && typeof __edgebox_udp_drop_membership === 'function') {
                    __edgebox_udp_drop_membership(this._socketId, multicastAddress, multicastInterface);
                }
                return this;
            }

            addSourceSpecificMembership(sourceAddress, groupAddress, multicastInterface) {
                // SSM not implemented - stub for compatibility
                return this;
            }

            dropSourceSpecificMembership(sourceAddress, groupAddress, multicastInterface) {
                // SSM not implemented - stub for compatibility
                return this;
            }

            setMulticastInterface(multicastInterface) {
                if (this._socketId !== null && typeof __edgebox_udp_set_multicast_interface === 'function') {
                    __edgebox_udp_set_multicast_interface(this._socketId, multicastInterface);
                }
                return this;
            }

            setRecvBufferSize(size) {
                if (this._socketId !== null && typeof __edgebox_udp_set_recv_buffer_size === 'function') {
                    __edgebox_udp_set_recv_buffer_size(this._socketId, size);
                }
                return this;
            }

            setSendBufferSize(size) {
                if (this._socketId !== null && typeof __edgebox_udp_set_send_buffer_size === 'function') {
                    __edgebox_udp_set_send_buffer_size(this._socketId, size);
                }
                return this;
            }

            getRecvBufferSize() {
                if (this._socketId !== null && typeof __edgebox_udp_get_recv_buffer_size === 'function') {
                    var size = __edgebox_udp_get_recv_buffer_size(this._socketId);
                    return size > 0 ? size : 65536;
                }
                return 65536;
            }

            getSendBufferSize() {
                if (this._socketId !== null && typeof __edgebox_udp_get_send_buffer_size === 'function') {
                    var size = __edgebox_udp_get_send_buffer_size(this._socketId);
                    return size > 0 ? size : 65536;
                }
                return 65536;
            }

            ref() {
                // Reference counting not needed in this implementation
                return this;
            }

            unref() {
                // Reference counting not needed in this implementation
                return this;
            }

            remoteAddress() {
                if (this._connected) {
                    return {
                        address: this._connectedAddress,
                        family: this._type === 'udp6' ? 'IPv6' : 'IPv4',
                        port: this._connectedPort
                    };
                }
                return undefined;
            }

            connect(port, address, callback) {
                var self = this;
                if (typeof address === 'function') {
                    callback = address;
                    address = '127.0.0.1';
                }
                this._connectedPort = port;
                this._connectedAddress = address || '127.0.0.1';
                this._connected = true;

                if (callback) {
                    setTimeout(function() {
                        self.emit('connect');
                        callback();
                    }, 0);
                } else {
                    setTimeout(function() { self.emit('connect'); }, 0);
                }
            }

            disconnect() {
                this._connected = false;
                this._connectedPort = null;
                this._connectedAddress = null;
            }
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

