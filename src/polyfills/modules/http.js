    // ===== HTTP MODULE (lazy loaded) =====
    _lazyModule('http', function() {
        // IncomingMessage - Readable stream for request/response body
        class IncomingMessage extends EventEmitter {
            constructor() {
                super();
                this.headers = {};
                this.rawHeaders = [];
                this.statusCode = 200;
                this.statusMessage = 'OK';
                this.httpVersion = '1.1';
                this.httpVersionMajor = 1;
                this.httpVersionMinor = 1;
                this.method = null;
                this.url = null;
                this.complete = false;
                this.aborted = false;
                this.socket = null;
                this.connection = null;
                this._body = [];
                this._bodyReceived = false;
            }
            // Readable stream interface
            read(size) {
                if (this._body.length === 0) return null;
                const chunk = this._body.shift();
                return size && chunk.length > size ? chunk.slice(0, size) : chunk;
            }
            setEncoding(encoding) { this._encoding = encoding; }
            pause() { this.isPaused = true; }
            resume() { this.isPaused = false; }
            destroy(error) {
                this.aborted = true;
                if (error) this.emit('error', error);
                this.emit('close');
            }
            setTimeout(ms, callback) {
                if (callback) this.on('timeout', callback);
                return this;
            }
        }

        // ServerResponse - Writable stream for response body
        class ServerResponse extends EventEmitter {
            constructor() {
                super();
                this.statusCode = 200;
                this.statusMessage = 'OK';
                this._headers = {};
                this._headerNames = {}; // Original casing
                this._body = [];
                this.headersSent = false;
                this.finished = false;
                this.writableEnded = false;
                this.writableFinished = false;
                this.socket = null;
                this.connection = null;
            }
            setHeader(name, value) {
                this._headers[name.toLowerCase()] = value;
                this._headerNames[name.toLowerCase()] = name;
            }
            getHeader(name) { return this._headers[name.toLowerCase()]; }
            getHeaderNames() { return Object.keys(this._headers); }
            getHeaders() { return { ...this._headers }; }
            hasHeader(name) { return name.toLowerCase() in this._headers; }
            removeHeader(name) {
                delete this._headers[name.toLowerCase()];
                delete this._headerNames[name.toLowerCase()];
            }
            writeHead(status, statusMessage, headers) {
                if (typeof statusMessage === 'object') {
                    headers = statusMessage;
                    statusMessage = null;
                }
                this.statusCode = status;
                if (statusMessage) this.statusMessage = statusMessage;
                if (headers) {
                    for (const key in headers) {
                        if (Object.prototype.hasOwnProperty.call(headers, key)) {
                            this.setHeader(key, headers[key]);
                        }
                    }
                }
                return this;
            }
            write(chunk, encoding, callback) {
                if (typeof encoding === 'function') { callback = encoding; encoding = 'utf8'; }
                this._body.push(chunk);
                if (callback) setImmediate(callback);
                return true;
            }
            end(data, encoding, callback) {
                if (typeof data === 'function') { callback = data; data = null; encoding = null; }
                if (typeof encoding === 'function') { callback = encoding; encoding = 'utf8'; }
                if (data) this._body.push(data);
                this.finished = true;
                this.writableEnded = true;
                this.writableFinished = true;
                this.emit('finish');
                if (callback) setImmediate(callback);
            }
            // Writable interface
            cork() {}
            uncork() {}
            setTimeout(ms, callback) {
                if (callback) this.on('timeout', callback);
                return this;
            }
            flushHeaders() { this.headersSent = true; }
        }
        // HTTP Agent class for connection pooling
        // Provides connection reuse semantics for fetch-based requests
        class Agent extends EventEmitter {
            constructor(options = {}) {
                super();
                this.options = options;
                this.keepAlive = options.keepAlive || false;
                this.keepAliveMsecs = options.keepAliveMsecs || 1000;
                this.maxSockets = options.maxSockets || Infinity;
                this.maxFreeSockets = options.maxFreeSockets || 256;
                this.maxTotalSockets = options.maxTotalSockets || Infinity;
                this.scheduling = options.scheduling || 'lifo'; // 'lifo' or 'fifo'
                this.timeout = options.timeout;
                // Active socket tracking per host:port
                this.sockets = {};      // { name: [socket1, socket2, ...] }
                this.freeSockets = {};  // { name: [socket1, ...] } - idle keepAlive sockets
                this.requests = {};     // { name: [req1, req2, ...] } - pending requests queue
                this._totalSocketCount = 0;
            }

            getName(options) {
                const host = options.host || options.hostname || 'localhost';
                const port = options.port || (options.protocol === 'https:' ? 443 : 80);
                const localAddress = options.localAddress || '';
                return `${host}:${port}:${localAddress}`;
            }

            // Get total active socket count
            getTotalSocketCount() {
                return this._totalSocketCount;
            }

            // Check if we can create a new socket for this host
            canCreateSocket(name) {
                const hostSockets = this.sockets[name] || [];
                if (hostSockets.length >= this.maxSockets) return false;
                if (this._totalSocketCount >= this.maxTotalSockets) return false;
                return true;
            }

            // Create a virtual socket for tracking
            createConnection(options, callback) {
                const name = this.getName(options);

                // Check for available free socket
                if (this.keepAlive && this.freeSockets[name] && this.freeSockets[name].length > 0) {
                    const socket = this.scheduling === 'lifo'
                        ? this.freeSockets[name].pop()
                        : this.freeSockets[name].shift();

                    // Move from free to active
                    if (!this.sockets[name]) this.sockets[name] = [];
                    this.sockets[name].push(socket);

                    socket._lastUsed = Date.now();
                    if (callback) setImmediate(() => callback(null, socket));
                    return socket;
                }

                // Check if we can create new socket
                if (!this.canCreateSocket(name)) {
                    // Queue the request for later processing
                    if (!this.requests[name]) this.requests[name] = [];
                    const pendingReq = { options, callback };
                    this.requests[name].push(pendingReq);

                    // Return a deferred socket that will be connected when a slot opens
                    const socket = new EventEmitter();
                    socket._deferred = true;
                    socket._name = name;
                    socket._agent = this;
                    return socket;
                }

                // Create new socket
                const socket = new EventEmitter();
                socket._name = name;
                socket._agent = this;
                socket._lastUsed = Date.now();
                socket.localAddress = options.localAddress;
                socket.localPort = Math.floor(Math.random() * 60000) + 1024; // Simulated ephemeral port
                socket.remoteAddress = options.host || options.hostname;
                socket.remotePort = options.port;
                socket.writable = true;
                socket.readable = true;
                socket.destroyed = false;

                socket.destroy = () => {
                    socket.destroyed = true;
                    socket.writable = false;
                    socket.readable = false;
                    this._removeSocket(socket, name);
                };

                socket.end = () => {
                    this._releaseSocket(socket, name);
                };

                socket.setTimeout = (ms, cb) => {
                    if (cb) socket.on('timeout', cb);
                    return socket;
                };

                if (!this.sockets[name]) this.sockets[name] = [];
                this.sockets[name].push(socket);
                this._totalSocketCount++;

                if (callback) setImmediate(() => callback(null, socket));
                return socket;
            }

            // Release socket back to pool or destroy it
            _releaseSocket(socket, name) {
                const hostSockets = this.sockets[name];
                if (hostSockets) {
                    const idx = hostSockets.indexOf(socket);
                    if (idx !== -1) hostSockets.splice(idx, 1);
                }

                // Process queued requests first
                if (this.requests[name] && this.requests[name].length > 0) {
                    const pending = this.requests[name].shift();
                    if (!this.sockets[name]) this.sockets[name] = [];
                    this.sockets[name].push(socket);
                    socket._lastUsed = Date.now();
                    if (pending.callback) setImmediate(() => pending.callback(null, socket));
                    return;
                }

                // Put in free pool if keepAlive is enabled
                if (this.keepAlive && !socket.destroyed) {
                    if (!this.freeSockets[name]) this.freeSockets[name] = [];

                    // Enforce maxFreeSockets
                    while (this.freeSockets[name].length >= this.maxFreeSockets) {
                        const oldSocket = this.freeSockets[name].shift();
                        oldSocket.destroyed = true;
                        this._totalSocketCount--;
                    }

                    this.freeSockets[name].push(socket);
                    socket._lastUsed = Date.now();

                    // Set keepAlive timeout
                    socket._keepAliveTimer = setTimeout(() => {
                        this._removeFromFreePool(socket, name);
                    }, this.keepAliveMsecs);
                } else {
                    this._totalSocketCount--;
                }
            }

            _removeSocket(socket, name) {
                // Remove from active sockets
                const hostSockets = this.sockets[name];
                if (hostSockets) {
                    const idx = hostSockets.indexOf(socket);
                    if (idx !== -1) {
                        hostSockets.splice(idx, 1);
                        this._totalSocketCount--;
                    }
                }

                // Also remove from free sockets if present
                this._removeFromFreePool(socket, name);

                // Process any pending requests
                if (this.requests[name] && this.requests[name].length > 0) {
                    const pending = this.requests[name].shift();
                    this.createConnection(pending.options, pending.callback);
                }
            }

            _removeFromFreePool(socket, name) {
                const freeSockets = this.freeSockets[name];
                if (freeSockets) {
                    const idx = freeSockets.indexOf(socket);
                    if (idx !== -1) {
                        freeSockets.splice(idx, 1);
                        if (socket._keepAliveTimer) {
                            clearTimeout(socket._keepAliveTimer);
                        }
                    }
                }
            }

            // Get current socket counts for monitoring
            getCurrentStatus() {
                const status = {
                    totalSockets: this._totalSocketCount,
                    sockets: {},
                    freeSockets: {},
                    requests: {}
                };
                for (const name in this.sockets) {
                    status.sockets[name] = this.sockets[name].length;
                }
                for (const name in this.freeSockets) {
                    status.freeSockets[name] = this.freeSockets[name].length;
                }
                for (const name in this.requests) {
                    status.requests[name] = this.requests[name].length;
                }
                return status;
            }

            destroy() {
                // Clear all keepAlive timers
                for (const name in this.freeSockets) {
                    for (const socket of this.freeSockets[name]) {
                        if (socket._keepAliveTimer) {
                            clearTimeout(socket._keepAliveTimer);
                        }
                    }
                }

                // Destroy all sockets
                for (const name in this.sockets) {
                    for (const socket of this.sockets[name]) {
                        socket.destroyed = true;
                    }
                }

                this.sockets = {};
                this.freeSockets = {};
                this.requests = {};
                this._totalSocketCount = 0;
            }
        }

        // ClientRequest - writable stream for sending request body
        class ClientRequest extends EventEmitter {
            constructor(options, callback) {
                super();
                this.method = (options.method || 'GET').toUpperCase();
                this.path = options.path || '/';
                this.headers = options.headers || {};
                this._body = [];
                this._ended = false;
                this.socket = null;
                this.connection = null;
                this.finished = false;
                this.writableEnded = false;
                this.aborted = false;

                // Timeout support
                this._timeoutMs = options.timeout || 0;
                this._timeoutTimer = null;
                this._abortController = null;

                // Build URL
                if (typeof options === 'string') {
                    this._url = options;
                } else if (options.url) {
                    this._url = options.url;
                } else {
                    const protocol = options.protocol || 'http:';
                    const port = options.port && options.port !== 80 ? ':' + options.port : '';
                    this._url = protocol + '//' + (options.hostname || options.host || 'localhost') + port + this.path;
                }

                if (callback) this.on('response', callback);
            }

            setHeader(name, value) {
                this.headers[name.toLowerCase()] = value;
            }

            getHeader(name) {
                return this.headers[name.toLowerCase()];
            }

            removeHeader(name) {
                delete this.headers[name.toLowerCase()];
            }

            write(chunk, encoding, callback) {
                if (typeof encoding === 'function') { callback = encoding; encoding = 'utf8'; }
                if (this._ended) {
                    if (callback) setImmediate(() => callback(new Error('write after end')));
                    return false;
                }
                this._body.push(Buffer.isBuffer(chunk) ? chunk : Buffer.from(chunk, encoding || 'utf8'));
                if (callback) setImmediate(callback);
                return true;
            }

            end(data, encoding, callback) {
                if (typeof data === 'function') { callback = data; data = null; }
                if (typeof encoding === 'function') { callback = encoding; encoding = 'utf8'; }
                if (data) this._body.push(Buffer.isBuffer(data) ? data : Buffer.from(data, encoding || 'utf8'));
                this._ended = true;
                this.finished = true;
                this.writableEnded = true;

                // Build request body
                const bodyBuffer = this._body.length > 0 ? Buffer.concat(this._body) : null;

                // Prepare fetch options
                const fetchOptions = {
                    method: this.method,
                    headers: this.headers
                };

                // Add body for non-GET/HEAD requests
                if (bodyBuffer && !['GET', 'HEAD'].includes(this.method)) {
                    fetchOptions.body = bodyBuffer;
                }

                // Set up AbortController for timeout
                const self = this;
                if (typeof AbortController !== 'undefined') {
                    this._abortController = new AbortController();
                    fetchOptions.signal = this._abortController.signal;
                }

                // Set up timeout timer if timeout is specified
                if (this._timeoutMs > 0) {
                    this._timeoutTimer = setTimeout(() => {
                        self.emit('timeout');
                        // Abort the request
                        if (self._abortController) {
                            self._abortController.abort();
                        }
                        self.aborted = true;
                    }, this._timeoutMs);
                }

                // Helper to clear timeout
                const clearRequestTimeout = () => {
                    if (self._timeoutTimer) {
                        clearTimeout(self._timeoutTimer);
                        self._timeoutTimer = null;
                    }
                };

                // Execute fetch
                fetch(this._url, fetchOptions)
                    .then(async response => {
                        clearRequestTimeout();
                        const res = new IncomingMessage();
                        res.statusCode = response.status;
                        res.statusMessage = response.statusText || httpModule.STATUS_CODES[response.status] || 'Unknown';
                        res.httpVersion = '1.1';

                        // Convert headers
                        res.headers = {};
                        res.rawHeaders = [];
                        response.headers.forEach((value, key) => {
                            res.headers[key.toLowerCase()] = value;
                            res.rawHeaders.push(key, value);
                        });

                        // Emit response event
                        self.emit('response', res);

                        // Stream the body in chunks if possible
                        if (response.body && response.body.getReader) {
                            // Use ReadableStream for true streaming
                            const reader = response.body.getReader();
                            try {
                                while (true) {
                                    const { done, value } = await reader.read();
                                    if (done) break;
                                    const chunk = Buffer.from(value);
                                    res.emit('data', chunk);
                                }
                            } catch (e) {
                                if (!self.aborted) res.emit('error', e);
                            }
                        } else {
                            // Fallback: read entire body
                            try {
                                const arrayBuffer = await response.arrayBuffer();
                                if (arrayBuffer.byteLength > 0) {
                                    // Emit in chunks for large responses
                                    const buffer = Buffer.from(arrayBuffer);
                                    const chunkSize = 16384; // 16KB chunks
                                    for (let i = 0; i < buffer.length; i += chunkSize) {
                                        res.emit('data', buffer.slice(i, Math.min(i + chunkSize, buffer.length)));
                                    }
                                }
                            } catch (e) {
                                // Ignore body read errors for responses that don't have body
                            }
                        }

                        res.complete = true;
                        res.emit('end');
                    })
                    .catch(err => {
                        clearRequestTimeout();
                        // Check if this was an abort due to timeout
                        if (self.aborted && err.name === 'AbortError') {
                            // Timeout already emitted, emit abort event
                            self.emit('abort');
                        } else {
                            self.emit('error', err);
                        }
                    });

                if (callback) setImmediate(callback);
            }

            abort() {
                this.aborted = true;
                // Abort the fetch if in progress
                if (this._abortController) {
                    this._abortController.abort();
                }
                // Clear timeout timer
                if (this._timeoutTimer) {
                    clearTimeout(this._timeoutTimer);
                    this._timeoutTimer = null;
                }
                this.emit('abort');
            }

            destroy(error) {
                this.aborted = true;
                // Abort the fetch if in progress
                if (this._abortController) {
                    this._abortController.abort();
                }
                // Clear timeout timer
                if (this._timeoutTimer) {
                    clearTimeout(this._timeoutTimer);
                    this._timeoutTimer = null;
                }
                if (error) this.emit('error', error);
                this.emit('close');
            }

            setTimeout(ms, callback) {
                if (callback) this.on('timeout', callback);
                this._timeoutMs = ms || 0;

                // If request is already in flight and timeout is set, start timer now
                if (this._ended && ms > 0 && !this._timeoutTimer) {
                    const self = this;
                    this._timeoutTimer = setTimeout(() => {
                        self.emit('timeout');
                        if (self._abortController) {
                            self._abortController.abort();
                        }
                        self.aborted = true;
                    }, ms);
                }
                return this;
            }

            setNoDelay(noDelay) { return this; }
            setSocketKeepAlive(enable, delay) { return this; }

            // Node.js compatibility
            flushHeaders() {}
            cork() {}
            uncork() {}
        }

        var httpModule = {
            IncomingMessage, ServerResponse, Agent, ClientRequest,
            globalAgent: new Agent(),
            request: function(options, callback) {
                if (typeof options === 'string') {
                    options = { url: options };
                }
                return new ClientRequest(options, callback);
            },
            get: function(options, callback) {
                if (typeof options === 'string') options = { url: options };
                options.method = 'GET';
                const req = this.request(options, callback);
                req.end();
                return req;
            },
            createServer: function(options, requestListener) {
                if (typeof options === 'function') {
                    requestListener = options;
                    options = {};
                }
                const net = _modules.net;
                const server = net.createServer(function(socket) {
                    var buffer = '';
                    socket.on('data', function(chunk) {
                        buffer += chunk.toString();
                        // Check if we have a complete HTTP request (headers end with \r\n\r\n)
                        var headerEnd = buffer.indexOf('\r\n\r\n');
                        if (headerEnd === -1) return;

                        var headerPart = buffer.substring(0, headerEnd);
                        var bodyPart = buffer.substring(headerEnd + 4);
                        var lines = headerPart.split('\r\n');
                        var requestLine = lines[0].split(' ');

                        // Parse request
                        var req = new IncomingMessage();
                        req.method = requestLine[0];
                        req.url = requestLine[1];
                        req.httpVersion = (requestLine[2] || 'HTTP/1.1').replace('HTTP/', '');
                        req.headers = {};
                        req.socket = socket;
                        req.connection = socket;

                        // Parse headers
                        for (var i = 1; i < lines.length; i++) {
                            var colonIdx = lines[i].indexOf(':');
                            if (colonIdx > 0) {
                                var key = lines[i].substring(0, colonIdx).toLowerCase().trim();
                                var val = lines[i].substring(colonIdx + 1).trim();
                                req.headers[key] = val;
                            }
                        }

                        // Create response
                        var res = new ServerResponse();
                        res.socket = socket;
                        res.connection = socket;
                        res._headerSent = false;
                        res._headers = { 'content-type': 'text/html' };
                        res._statusCode = 200;

                        res.writeHead = function(statusCode, statusMessage, headers) {
                            if (typeof statusMessage === 'object') {
                                headers = statusMessage;
                                statusMessage = null;
                            }
                            res._statusCode = statusCode;
                            if (headers) {
                                for (var k in headers) {
                                    // Security: Protect against prototype pollution
                                    if (Object.prototype.hasOwnProperty.call(headers, k)) {
                                        res._headers[k.toLowerCase()] = headers[k];
                                    }
                                }
                            }
                        };

                        res.setHeader = function(name, value) {
                            res._headers[name.toLowerCase()] = value;
                        };

                        res.getHeader = function(name) {
                            return res._headers[name.toLowerCase()];
                        };

                        res.removeHeader = function(name) {
                            delete res._headers[name.toLowerCase()];
                        };

                        res.write = function(chunk) {
                            if (!res._headerSent) {
                                var statusText = httpModule.STATUS_CODES[res._statusCode] || 'Unknown';
                                var headerLines = ['HTTP/1.1 ' + res._statusCode + ' ' + statusText];

                                // Check if Content-Length is set, otherwise use chunked transfer encoding
                                var hasContentLength = 'content-length' in res._headers;
                                if (!hasContentLength) {
                                    res._headers['transfer-encoding'] = 'chunked';
                                    res._chunked = true;
                                }

                                for (var k in res._headers) {
                                    headerLines.push(k + ': ' + res._headers[k]);
                                }
                                headerLines.push('', '');
                                socket.write(headerLines.join('\r\n'));
                                res._headerSent = true;
                            }
                            if (chunk) {
                                var data = typeof chunk === 'string' ? chunk : chunk.toString();
                                if (res._chunked) {
                                    // Write chunk in chunked encoding format: size\r\ndata\r\n
                                    var size = Buffer.byteLength(data).toString(16);
                                    socket.write(size + '\r\n' + data + '\r\n');
                                } else {
                                    socket.write(data);
                                }
                            }
                            return true;
                        };

                        res.end = function(chunk) {
                            if (chunk) res.write(chunk);
                            else if (!res._headerSent) res.write('');

                            // Send final chunk for chunked encoding
                            if (res._chunked) {
                                socket.write('0\r\n\r\n');
                            }

                            socket.end();
                            res.emit('finish');
                        };

                        // Handle body for POST/PUT
                        var contentLength = parseInt(req.headers['content-length'] || '0', 10);
                        var isChunked = (req.headers['transfer-encoding'] || '').toLowerCase() === 'chunked';

                        if (isChunked) {
                            // Parse chunked transfer encoding
                            // Format: <size-hex>\r\n<data>\r\n ... 0\r\n\r\n
                            var chunkedBuffer = bodyPart;
                            var bodyChunks = [];
                            var parseComplete = false;

                            // Helper function to parse chunked body
                            function parseChunkedBody() {
                                while (true) {
                                    // Find chunk size line
                                    var sizeEnd = chunkedBuffer.indexOf('\r\n');
                                    if (sizeEnd === -1) break;

                                    var sizeHex = chunkedBuffer.substring(0, sizeEnd).trim();
                                    // Handle chunk extensions (ignore them)
                                    var semiIdx = sizeHex.indexOf(';');
                                    if (semiIdx !== -1) sizeHex = sizeHex.substring(0, semiIdx);

                                    var chunkSize = parseInt(sizeHex, 16);
                                    if (isNaN(chunkSize)) break;

                                    if (chunkSize === 0) {
                                        // Final chunk - look for trailing \r\n
                                        parseComplete = true;
                                        break;
                                    }

                                    // Check if we have enough data for this chunk + \r\n
                                    var chunkStart = sizeEnd + 2;
                                    var chunkEnd = chunkStart + chunkSize;
                                    if (chunkedBuffer.length < chunkEnd + 2) break;

                                    // Extract chunk data
                                    bodyChunks.push(chunkedBuffer.substring(chunkStart, chunkEnd));

                                    // Move past chunk and trailing \r\n
                                    chunkedBuffer = chunkedBuffer.substring(chunkEnd + 2);
                                }
                            }

                            parseChunkedBody();

                            if (parseComplete) {
                                // All chunks received
                                setTimeout(function() {
                                    if (bodyChunks.length > 0) {
                                        req.emit('data', bodyChunks.join(''));
                                    }
                                    req.emit('end');
                                }, 0);
                                buffer = '';
                            } else {
                                // Need more data - setup continuation handler
                                req._chunkedBuffer = chunkedBuffer;
                                req._bodyChunks = bodyChunks;
                                req._awaitingChunks = true;

                                socket.on('data', function onChunkData(moreChunk) {
                                    if (!req._awaitingChunks) return;
                                    req._chunkedBuffer += moreChunk.toString();

                                    // Try to parse more chunks
                                    var cb = req._chunkedBuffer;
                                    while (true) {
                                        var se = cb.indexOf('\r\n');
                                        if (se === -1) break;
                                        var sh = cb.substring(0, se).trim();
                                        var si = sh.indexOf(';');
                                        if (si !== -1) sh = sh.substring(0, si);
                                        var cs = parseInt(sh, 16);
                                        if (isNaN(cs)) break;
                                        if (cs === 0) {
                                            // Final chunk
                                            req._awaitingChunks = false;
                                            socket.removeListener('data', onChunkData);
                                            if (req._bodyChunks.length > 0) {
                                                req.emit('data', req._bodyChunks.join(''));
                                            }
                                            req.emit('end');
                                            return;
                                        }
                                        var cst = se + 2;
                                        var ced = cst + cs;
                                        if (cb.length < ced + 2) break;
                                        req._bodyChunks.push(cb.substring(cst, ced));
                                        cb = cb.substring(ced + 2);
                                    }
                                    req._chunkedBuffer = cb;
                                });
                                buffer = '';
                            }
                        } else if (contentLength > 0 && bodyPart.length >= contentLength) {
                            // Body complete (Content-Length)
                            setTimeout(function() {
                                req.emit('data', bodyPart.substring(0, contentLength));
                                req.emit('end');
                            }, 0);
                            buffer = '';
                        } else if (contentLength > 0) {
                            // Need more body data
                            req._bodyBuffer = bodyPart;
                            req._contentLength = contentLength;
                            req._awaitingBody = true;

                            socket.on('data', function onBodyData(moreChunk) {
                                if (!req._awaitingBody) return;
                                req._bodyBuffer += moreChunk.toString();
                                if (req._bodyBuffer.length >= req._contentLength) {
                                    req._awaitingBody = false;
                                    socket.removeListener('data', onBodyData);
                                    req.emit('data', req._bodyBuffer.substring(0, req._contentLength));
                                    req.emit('end');
                                }
                            });
                            buffer = '';
                        } else {
                            // No body
                            setTimeout(function() { req.emit('end'); }, 0);
                            buffer = '';
                        }

                        if (requestListener) requestListener(req, res);
                        server.emit('request', req, res);
                    });

                    socket.on('error', function(err) {
                        server.emit('clientError', err, socket);
                    });
                });

                // Forward server events
                server.setTimeout = function(ms, callback) {
                    if (callback) server.on('timeout', callback);
                    return server;
                };

                return server;
            },
            METHODS: ['GET', 'HEAD', 'POST', 'PUT', 'DELETE', 'CONNECT', 'OPTIONS', 'TRACE', 'PATCH'],
            STATUS_CODES: {
                100: 'Continue', 101: 'Switching Protocols', 200: 'OK', 201: 'Created',
                204: 'No Content', 301: 'Moved Permanently', 302: 'Found', 304: 'Not Modified',
                400: 'Bad Request', 401: 'Unauthorized', 403: 'Forbidden', 404: 'Not Found',
                500: 'Internal Server Error', 502: 'Bad Gateway', 503: 'Service Unavailable'
            }
        };
        return httpModule;
    });

