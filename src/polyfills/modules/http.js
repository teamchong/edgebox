    // ===== HTTP MODULE (lazy loaded) =====
    _lazyModule('http', function() {
        class IncomingMessage extends EventEmitter {
            constructor() { super(); this.headers = {}; this.statusCode = 200; this.statusMessage = 'OK'; }
        }
        class ServerResponse extends EventEmitter {
            constructor() { super(); this.statusCode = 200; this._headers = {}; this._body = []; }
            setHeader(name, value) { this._headers[name.toLowerCase()] = value; }
            getHeader(name) { return this._headers[name.toLowerCase()]; }
            writeHead(status, headers) { this.statusCode = status; Object.assign(this._headers, headers); }
            write(chunk) { this._body.push(chunk); return true; }
            end(data) { if (data) this._body.push(data); this.emit('finish'); }
        }
        // HTTP Agent class for connection pooling
        class Agent extends EventEmitter {
            constructor(options = {}) {
                super();
                this.options = options;
                this.keepAlive = options.keepAlive || false;
                this.keepAliveMsecs = options.keepAliveMsecs || 1000;
                this.maxSockets = options.maxSockets || Infinity;
                this.maxFreeSockets = options.maxFreeSockets || 256;
                this.maxTotalSockets = options.maxTotalSockets || Infinity;
                this.scheduling = options.scheduling || 'lifo';
                this.timeout = options.timeout;
                this.sockets = {};
                this.freeSockets = {};
                this.requests = {};
            }
            createConnection(options, callback) {
                // Stub - in WASM we use fetch instead of sockets
                if (callback) setTimeout(callback, 0);
                return new EventEmitter();
            }
            getName(options) {
                return `${options.host || options.hostname || 'localhost'}:${options.port || 80}:${options.localAddress || ''}`;
            }
            destroy() {
                this.sockets = {};
                this.freeSockets = {};
                this.requests = {};
            }
        }

        var httpModule = {
            IncomingMessage, ServerResponse, Agent,
            globalAgent: new Agent(),
            request: function(options, callback) {
                // Handle URL string, options.url, or construct from hostname/host
                let url;
                if (typeof options === 'string') {
                    url = options;
                } else if (options.url) {
                    url = options.url;
                } else {
                    const port = options.port ? ':' + options.port : '';
                    url = (options.protocol || 'http:') + '//' + (options.hostname || options.host || 'localhost') + port + (options.path || '/');
                }
                const req = new EventEmitter();
                req._body = [];
                req.write = chunk => { req._body.push(chunk); return true; };
                req.end = data => {
                    if (data) req._body.push(data);
                    fetch(url, { method: options.method || 'GET', headers: options.headers, body: req._body.length ? req._body.join('') : undefined })
                        .then(async response => {
                            const res = new IncomingMessage();
                            res.statusCode = response.status;
                            res.headers = Object.fromEntries(response.headers);
                            if (callback) callback(res);
                            req.emit('response', res);
                            const text = await response.text();
                            res.emit('data', text);
                            res.emit('end');
                        }).catch(err => req.emit('error', err));
                };
                return req;
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
                                for (var k in res._headers) {
                                    headerLines.push(k + ': ' + res._headers[k]);
                                }
                                headerLines.push('', '');
                                socket.write(headerLines.join('\r\n'));
                                res._headerSent = true;
                            }
                            if (chunk) socket.write(typeof chunk === 'string' ? chunk : chunk.toString());
                            return true;
                        };

                        res.end = function(chunk) {
                            if (chunk) res.write(chunk);
                            else if (!res._headerSent) res.write('');
                            socket.end();
                            res.emit('finish');
                        };

                        // Handle body for POST/PUT
                        var contentLength = parseInt(req.headers['content-length'] || '0', 10);
                        if (contentLength > 0 && bodyPart.length >= contentLength) {
                            // Body complete
                            setTimeout(function() {
                                req.emit('data', bodyPart.substring(0, contentLength));
                                req.emit('end');
                            }, 0);
                        } else if (contentLength === 0) {
                            setTimeout(function() { req.emit('end'); }, 0);
                        }

                        buffer = '';
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

