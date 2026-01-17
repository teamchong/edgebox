    // ===== HTTP2 MODULE (lazy loaded) =====
    _lazyModule('http2', function() {
        var _h2ConnectionId = 0;
        var _h2StreamId = 0;

        // HTTP/2 Session (Client)
        class Http2Session extends EventEmitter {
            constructor(authority, options) {
                super();
                var self = this;
                this._id = ++_h2ConnectionId;
                this._authority = authority;
                this._options = options || {};
                this._socket = null;
                this._streams = new Map();
                this._closed = false;
                this._nextStreamId = 1;
                this._settings = {
                    headerTableSize: 4096,
                    enablePush: true,
                    maxConcurrentStreams: 100,
                    initialWindowSize: 65535,
                    maxFrameSize: 16384,
                    maxHeaderListSize: 8192
                };

                // Parse authority URL
                var url = new URL(authority.startsWith('http') ? authority : 'https://' + authority);
                this._host = url.hostname;
                this._port = parseInt(url.port) || (url.protocol === 'https:' ? 443 : 80);
                this._secure = url.protocol === 'https:';

                // Connect via TCP socket
                var net = _modules.net;
                this._socket = net.connect({ host: this._host, port: this._port }, function() {
                    // Send HTTP/2 connection preface
                    self._sendPreface();
                    self.emit('connect', self);
                });

                this._socket.on('data', function(data) {
                    self._handleData(data);
                });

                this._socket.on('error', function(err) {
                    self.emit('error', err);
                });

                this._socket.on('close', function() {
                    self._closed = true;
                    self.emit('close');
                });
            }

            _sendPreface() {
                // Connection preface
                this._socket.write('PRI * HTTP/2.0\r\n\r\nSM\r\n\r\n');
                // Send SETTINGS frame
                this._sendSettings();
            }

            _sendSettings() {
                // Build SETTINGS frame
                var payload = Buffer.alloc(18); // 3 settings * 6 bytes each
                var pos = 0;
                // SETTINGS_MAX_CONCURRENT_STREAMS
                payload.writeUInt16BE(0x3, pos); pos += 2;
                payload.writeUInt32BE(100, pos); pos += 4;
                // SETTINGS_INITIAL_WINDOW_SIZE
                payload.writeUInt16BE(0x4, pos); pos += 2;
                payload.writeUInt32BE(65535, pos); pos += 4;
                // SETTINGS_MAX_FRAME_SIZE
                payload.writeUInt16BE(0x5, pos); pos += 2;
                payload.writeUInt32BE(16384, pos); pos += 4;

                this._sendFrame(0x4, 0, 0, payload.slice(0, pos)); // SETTINGS frame
            }

            _sendFrame(type, flags, streamId, payload) {
                var header = Buffer.alloc(9);
                var len = payload ? payload.length : 0;
                header.writeUIntBE(len, 0, 3);
                header.writeUInt8(type, 3);
                header.writeUInt8(flags, 4);
                header.writeUInt32BE(streamId & 0x7FFFFFFF, 5);
                this._socket.write(header);
                if (payload && payload.length > 0) {
                    this._socket.write(payload);
                }
            }

            _handleData(data) {
                // Parse HTTP/2 frames
                var pos = 0;
                while (pos + 9 <= data.length) {
                    var len = (data[pos] << 16) | (data[pos+1] << 8) | data[pos+2];
                    var type = data[pos+3];
                    var flags = data[pos+4];
                    var streamId = ((data[pos+5] & 0x7F) << 24) | (data[pos+6] << 16) | (data[pos+7] << 8) | data[pos+8];
                    pos += 9;

                    if (pos + len > data.length) break;
                    var payload = data.slice(pos, pos + len);
                    pos += len;

                    this._handleFrame(type, flags, streamId, payload);
                }
            }

            _handleFrame(type, flags, streamId, payload) {
                switch (type) {
                    case 0x0: // DATA
                        var stream = this._streams.get(streamId);
                        if (stream) {
                            stream.emit('data', payload);
                            if (flags & 0x1) { // END_STREAM
                                stream.emit('end');
                            }
                        }
                        break;
                    case 0x1: // HEADERS
                        var stream = this._streams.get(streamId);
                        if (stream) {
                            // Parse HPACK headers (simplified - just emit raw for now)
                            stream.emit('response', { ':status': '200' }, flags);
                            if (flags & 0x1) { // END_STREAM
                                stream.emit('end');
                            }
                        }
                        break;
                    case 0x4: // SETTINGS
                        if (!(flags & 0x1)) { // Not ACK
                            // Send SETTINGS ACK
                            this._sendFrame(0x4, 0x1, 0, Buffer.alloc(0));
                        }
                        this.emit('remoteSettings', this._settings);
                        break;
                    case 0x6: // PING
                        if (!(flags & 0x1)) {
                            // Send PING ACK
                            this._sendFrame(0x6, 0x1, 0, payload);
                        }
                        break;
                    case 0x7: // GOAWAY
                        this.emit('goaway', payload);
                        break;
                }
            }

            request(headers, options) {
                var self = this;
                var streamId = this._nextStreamId;
                this._nextStreamId += 2; // Client uses odd stream IDs

                var stream = new Http2Stream(this, streamId);
                this._streams.set(streamId, stream);

                // Encode headers (simplified HPACK - literal without indexing)
                var headerBuf = [];
                for (var name in headers) {
                    var value = headers[name];
                    // Literal header field without indexing
                    headerBuf.push(0x00);
                    headerBuf.push(name.length);
                    for (var i = 0; i < name.length; i++) headerBuf.push(name.charCodeAt(i));
                    headerBuf.push(value.length);
                    for (var i = 0; i < value.length; i++) headerBuf.push(value.charCodeAt(i));
                }

                var payload = Buffer.from(headerBuf);
                var flags = 0x4; // END_HEADERS
                if (!options || !options.endStream === false) {
                    // Will send data separately
                }

                this._sendFrame(0x1, flags, streamId, payload); // HEADERS frame

                return stream;
            }

            close(callback) {
                var self = this;
                // Send GOAWAY
                var payload = Buffer.alloc(8);
                payload.writeUInt32BE(this._nextStreamId - 2, 0); // Last stream ID
                payload.writeUInt32BE(0, 4); // NO_ERROR
                this._sendFrame(0x7, 0, 0, payload);

                this._closed = true;
                if (this._socket) {
                    this._socket.end();
                }
                if (callback) setTimeout(callback, 0);
            }

            get closed() { return this._closed; }
            get destroyed() { return this._closed; }
            get encrypted() { return this._secure; }
            get alpnProtocol() { return 'h2'; }
            get originSet() { return [this._authority]; }
            get pendingSettingsAck() { return false; }
            get remoteSettings() { return this._settings; }
            get localSettings() { return this._settings; }
            get socket() { return this._socket; }
            get state() { return { effectiveLocalWindowSize: 65535, effectiveRecvDataLength: 0, nextStreamID: this._nextStreamId, localWindowSize: 65535, lastProcStreamID: 0, remoteWindowSize: 65535, deflateDynamicTableSize: 4096, inflateDynamicTableSize: 4096 }; }
            get type() { return 1; } // NGHTTP2_SESSION_CLIENT

            ping(payload, callback) {
                this._sendFrame(0x6, 0, 0, payload || Buffer.alloc(8));
                if (callback) setTimeout(function() { callback(null, 0, payload); }, 10);
                return true;
            }

            settings(settings, callback) {
                if (settings) Object.assign(this._settings, settings);
                this._sendSettings();
                if (callback) setTimeout(callback, 0);
            }

            goaway(code, lastStreamId, data) {
                var payload = Buffer.alloc(8 + (data ? data.length : 0));
                payload.writeUInt32BE(lastStreamId || 0, 0);
                payload.writeUInt32BE(code || 0, 4);
                if (data) data.copy(payload, 8);
                this._sendFrame(0x7, 0, 0, payload);
            }

            destroy(err) {
                this._closed = true;
                if (this._socket) this._socket.destroy();
                if (err) this.emit('error', err);
                this.emit('close');
            }

            ref() { return this; }
            unref() { return this; }
            setTimeout(ms, callback) { if (callback) this.on('timeout', callback); return this; }
            setLocalWindowSize(size) {}
        }

        // HTTP/2 Stream
        class Http2Stream extends EventEmitter {
            constructor(session, id) {
                super();
                this._session = session;
                this._id = id;
                this._closed = false;
                this._sentHeaders = false;
                this._sentTrailers = false;
                this._state = 'open';
                this.rstCode = 0;
            }

            write(data, encoding, callback) {
                if (typeof encoding === 'function') { callback = encoding; encoding = 'utf8'; }
                var buf = Buffer.isBuffer(data) ? data : Buffer.from(data, encoding);
                this._session._sendFrame(0x0, 0, this._id, buf); // DATA frame
                if (callback) setTimeout(callback, 0);
                return true;
            }

            end(data, encoding, callback) {
                if (typeof data === 'function') { callback = data; data = null; }
                if (typeof encoding === 'function') { callback = encoding; encoding = 'utf8'; }

                var flags = 0x1; // END_STREAM
                var buf = data ? (Buffer.isBuffer(data) ? data : Buffer.from(data, encoding)) : Buffer.alloc(0);
                this._session._sendFrame(0x0, flags, this._id, buf);

                this._closed = true;
                this._state = 'closed';
                if (callback) setTimeout(callback, 0);
            }

            close(code, callback) {
                if (typeof code === 'function') { callback = code; code = 0; }
                // Send RST_STREAM
                var payload = Buffer.alloc(4);
                payload.writeUInt32BE(code || 0, 0);
                this._session._sendFrame(0x3, 0, this._id, payload);
                this._closed = true;
                this._state = 'closed';
                this.rstCode = code || 0;
                if (callback) setTimeout(callback, 0);
            }

            get id() { return this._id; }
            get pending() { return !this._sentHeaders; }
            get destroyed() { return this._closed; }
            get closed() { return this._closed; }
            get aborted() { return false; }
            get session() { return this._session; }
            get sentHeaders() { return this._sentHeaders; }
            get sentTrailers() { return this._sentTrailers; }
            get state() { return this._state; }

            priority(options) {}
            setTimeout(ms, callback) { if (callback) this.on('timeout', callback); return this; }
            sendTrailers(headers) { this._sentTrailers = true; }
        }

        // HTTP/2 Server Session (incoming)
        class Http2ServerSession extends Http2Session {
            constructor(socket, options) {
                super('', options);
                this._socket = socket;
                this._nextStreamId = 2; // Server uses even stream IDs
            }
        }

        var http2Module = {
            constants: {
                HTTP2_HEADER_METHOD: ':method',
                HTTP2_HEADER_PATH: ':path',
                HTTP2_HEADER_STATUS: ':status',
                HTTP2_HEADER_AUTHORITY: ':authority',
                HTTP2_HEADER_SCHEME: ':scheme',
                HTTP2_HEADER_CONTENT_TYPE: 'content-type',
                HTTP2_HEADER_CONTENT_LENGTH: 'content-length',
                HTTP2_HEADER_ACCEPT: 'accept',
                HTTP2_HEADER_ACCEPT_ENCODING: 'accept-encoding',
                HTTP2_HEADER_USER_AGENT: 'user-agent',
                // Error codes
                NGHTTP2_NO_ERROR: 0,
                NGHTTP2_PROTOCOL_ERROR: 1,
                NGHTTP2_INTERNAL_ERROR: 2,
                NGHTTP2_FLOW_CONTROL_ERROR: 3,
                NGHTTP2_SETTINGS_TIMEOUT: 4,
                NGHTTP2_STREAM_CLOSED: 5,
                NGHTTP2_FRAME_SIZE_ERROR: 6,
                NGHTTP2_REFUSED_STREAM: 7,
                NGHTTP2_CANCEL: 8,
                NGHTTP2_COMPRESSION_ERROR: 9,
                NGHTTP2_CONNECT_ERROR: 10,
                NGHTTP2_ENHANCE_YOUR_CALM: 11,
                NGHTTP2_INADEQUATE_SECURITY: 12,
                // Settings
                NGHTTP2_DEFAULT_WEIGHT: 16,
                HTTP2_HEADER_COOKIE: 'cookie',
                HTTP2_HEADER_SET_COOKIE: 'set-cookie',
            },
            connect: function(authority, options, listener) {
                if (typeof options === 'function') {
                    listener = options;
                    options = {};
                }
                var session = new Http2Session(authority, options);
                if (listener) session.on('connect', listener);
                return session;
            },
            createServer: function(options, onRequestHandler) {
                if (typeof options === 'function') {
                    onRequestHandler = options;
                    options = {};
                }
                var net = _modules.net;
                var server = net.createServer(function(socket) {
                    var session = new Http2ServerSession(socket, options);
                    if (onRequestHandler) {
                        session.on('stream', function(stream, headers) {
                            onRequestHandler(stream, headers);
                        });
                    }
                });
                server.setTimeout = function(ms, callback) { if (callback) server.on('timeout', callback); return server; };
                return server;
            },
            createSecureServer: function(options, onRequestHandler) {
                // For now, same as createServer (TLS would wrap the socket)
                return http2Module.createServer(options, onRequestHandler);
            },
            getDefaultSettings: function() {
                return {
                    headerTableSize: 4096,
                    enablePush: true,
                    maxConcurrentStreams: 100,
                    initialWindowSize: 65535,
                    maxFrameSize: 16384,
                    maxHeaderListSize: 8192
                };
            },
            getPackedSettings: function(settings) {
                var buf = Buffer.alloc(36);
                var pos = 0;
                if (settings.headerTableSize !== undefined) {
                    buf.writeUInt16BE(0x1, pos); buf.writeUInt32BE(settings.headerTableSize, pos + 2); pos += 6;
                }
                if (settings.maxConcurrentStreams !== undefined) {
                    buf.writeUInt16BE(0x3, pos); buf.writeUInt32BE(settings.maxConcurrentStreams, pos + 2); pos += 6;
                }
                if (settings.initialWindowSize !== undefined) {
                    buf.writeUInt16BE(0x4, pos); buf.writeUInt32BE(settings.initialWindowSize, pos + 2); pos += 6;
                }
                if (settings.maxFrameSize !== undefined) {
                    buf.writeUInt16BE(0x5, pos); buf.writeUInt32BE(settings.maxFrameSize, pos + 2); pos += 6;
                }
                return buf.slice(0, pos);
            },
            getUnpackedSettings: function(buffer) {
                var settings = {};
                for (var i = 0; i + 6 <= buffer.length; i += 6) {
                    var id = buffer.readUInt16BE(i);
                    var value = buffer.readUInt32BE(i + 2);
                    switch (id) {
                        case 0x1: settings.headerTableSize = value; break;
                        case 0x2: settings.enablePush = value === 1; break;
                        case 0x3: settings.maxConcurrentStreams = value; break;
                        case 0x4: settings.initialWindowSize = value; break;
                        case 0x5: settings.maxFrameSize = value; break;
                        case 0x6: settings.maxHeaderListSize = value; break;
                    }
                }
                return settings;
            },
            sensitiveHeaders: Symbol('nodejs.http2.sensitiveHeaders'),
            Http2Session: Http2Session,
            Http2Stream: Http2Stream,
            Http2ServerSession: Http2ServerSession,
        };
        return http2Module;
    });

