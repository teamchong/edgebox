    // ===== HTTPS MODULE (lazy loaded) =====
    _lazyModule('https', function() {
        var http = _modules.http;
        var tls = _modules.tls;

        // Check if native TLS is available
        var hasNativeTls = typeof __edgebox_tls_connect === 'function';

        // HTTPS Agent extends HTTP Agent with TLS options
        class Agent extends http.Agent {
            constructor(options) {
                options = options || {};
                super(options);
                this.rejectUnauthorized = options.rejectUnauthorized !== false;
                this.ca = options.ca;
                this.cert = options.cert;
                this.key = options.key;
                this.pfx = options.pfx;
                this.passphrase = options.passphrase;
                this.servername = options.servername;
                this.minVersion = options.minVersion || 'TLSv1.2';
                this.maxVersion = options.maxVersion || 'TLSv1.3';
            }
            getName(options) {
                return (options.host || options.hostname || 'localhost') + ':' + (options.port || 443) + ':' + (options.localAddress || '');
            }
        }

        // Parse URL helper
        function parseUrl(urlStr) {
            try {
                var url = new URL(urlStr);
                return {
                    protocol: url.protocol,
                    hostname: url.hostname,
                    port: url.port || 443,
                    path: url.pathname + url.search
                };
            } catch (e) {
                return null;
            }
        }

        var httpsModule = {
            Agent: Agent,
            globalAgent: new Agent(),
            IncomingMessage: http.IncomingMessage,
            ServerResponse: http.ServerResponse,

            // Make request using native TLS socket
            request: function(options, callback) {
                var self = this;
                var url, hostname, port, path, method, headers;

                // Parse options
                if (typeof options === 'string') {
                    var parsed = parseUrl(options);
                    if (parsed) {
                        hostname = parsed.hostname;
                        port = parsed.port;
                        path = parsed.path;
                    } else {
                        hostname = options;
                        port = 443;
                        path = '/';
                    }
                    method = 'GET';
                    headers = {};
                } else {
                    if (options.url) {
                        var parsedUrl = parseUrl(options.url);
                        if (parsedUrl) {
                            hostname = parsedUrl.hostname;
                            port = parsedUrl.port;
                            path = parsedUrl.path;
                        }
                    }
                    hostname = hostname || options.hostname || options.host || 'localhost';
                    port = port || options.port || 443;
                    path = path || options.path || '/';
                    method = options.method || 'GET';
                    headers = options.headers || {};
                }

                // Ensure Host header
                if (!headers['Host'] && !headers['host']) {
                    headers['Host'] = port === 443 ? hostname : hostname + ':' + port;
                }

                var req = new EventEmitter();
                req._body = [];
                req._socket = null;
                req._ended = false;

                req.write = function(chunk) {
                    req._body.push(typeof chunk === 'string' ? chunk : chunk.toString());
                    return true;
                };

                req.end = function(data) {
                    if (req._ended) return;
                    req._ended = true;

                    if (data) req._body.push(typeof data === 'string' ? data : data.toString());

                    // If native TLS available and not using fetch mode, use TLS socket
                    if (hasNativeTls && !options._useFetch) {
                        self._requestWithTls(hostname, port, path, method, headers, req._body, req, callback);
                    } else {
                        // Fallback to fetch
                        self._requestWithFetch(hostname, port, path, method, headers, req._body, req, callback);
                    }
                };

                req.setTimeout = function(ms, cb) {
                    if (cb) req.on('timeout', cb);
                    return req;
                };

                req.abort = function() {
                    if (req._socket) req._socket.destroy();
                    req.emit('abort');
                };

                req.destroy = function(err) {
                    if (req._socket) req._socket.destroy();
                    if (err) req.emit('error', err);
                    req.emit('close');
                };

                return req;
            },

            // Request using native TLS socket
            _requestWithTls: function(hostname, port, path, method, headers, body, req, callback) {
                var socket = tls.connect({ host: hostname, port: port, servername: hostname });
                req._socket = socket;

                socket.on('error', function(err) {
                    req.emit('error', err);
                });

                socket.on('secureConnect', function() {
                    // Build HTTP request
                    var httpReq = method + ' ' + path + ' HTTP/1.1\r\n';
                    var headerKeys = Object.keys(headers);
                    for (var i = 0; i < headerKeys.length; i++) {
                        var key = headerKeys[i];
                        httpReq += key + ': ' + headers[key] + '\r\n';
                    }

                    // Add Content-Length for body
                    var bodyStr = body.join('');
                    if (bodyStr.length > 0 && method !== 'GET' && method !== 'HEAD') {
                        httpReq += 'Content-Length: ' + bodyStr.length + '\r\n';
                    }

                    httpReq += '\r\n';
                    if (bodyStr.length > 0 && method !== 'GET' && method !== 'HEAD') {
                        httpReq += bodyStr;
                    }

                    socket.write(httpReq);
                });

                // Parse HTTP response
                var responseBuffer = '';
                var headersParsed = false;
                var res = null;
                var contentLength = -1;
                var bodyReceived = 0;

                socket.on('data', function(chunk) {
                    responseBuffer += chunk.toString();

                    if (!headersParsed) {
                        var headerEnd = responseBuffer.indexOf('\r\n\r\n');
                        if (headerEnd !== -1) {
                            headersParsed = true;
                            var headerPart = responseBuffer.substring(0, headerEnd);
                            var bodyPart = responseBuffer.substring(headerEnd + 4);

                            // Parse status line
                            var lines = headerPart.split('\r\n');
                            var statusLine = lines[0];
                            var statusMatch = statusLine.match(/HTTP\/\d\.\d\s+(\d+)\s*(.*)/);

                            res = new http.IncomingMessage();
                            res.statusCode = statusMatch ? parseInt(statusMatch[1], 10) : 200;
                            res.statusMessage = statusMatch ? statusMatch[2] : 'OK';
                            res.headers = {};

                            // Parse headers
                            for (var i = 1; i < lines.length; i++) {
                                var colonIdx = lines[i].indexOf(':');
                                if (colonIdx > 0) {
                                    var hdrName = lines[i].substring(0, colonIdx).toLowerCase();
                                    var hdrValue = lines[i].substring(colonIdx + 1).trim();
                                    res.headers[hdrName] = hdrValue;
                                    if (hdrName === 'content-length') {
                                        contentLength = parseInt(hdrValue, 10);
                                    }
                                }
                            }

                            if (callback) callback(res);
                            req.emit('response', res);

                            // Emit any body we already have
                            if (bodyPart.length > 0) {
                                bodyReceived += bodyPart.length;
                                res.emit('data', Buffer.from(bodyPart));
                            }

                            // Check if response is complete
                            if (contentLength >= 0 && bodyReceived >= contentLength) {
                                res.emit('end');
                                socket.destroy();
                            }
                        }
                    } else if (res) {
                        // More body data
                        bodyReceived += chunk.length;
                        res.emit('data', chunk);

                        if (contentLength >= 0 && bodyReceived >= contentLength) {
                            res.emit('end');
                            socket.destroy();
                        }
                    }
                });

                socket.on('end', function() {
                    if (res && !res._ended) {
                        res._ended = true;
                        res.emit('end');
                    }
                });

                socket.on('close', function() {
                    if (res && !res._ended) {
                        res._ended = true;
                        res.emit('end');
                    }
                    req.emit('close');
                });
            },

            // Request using fetch (fallback)
            _requestWithFetch: function(hostname, port, path, method, headers, body, req, callback) {
                var url = 'https://' + hostname;
                if (port !== 443) url += ':' + port;
                url += path;

                var fetchOptions = {
                    method: method,
                    headers: headers
                };

                var bodyStr = body.join('');
                if (bodyStr.length > 0 && method !== 'GET' && method !== 'HEAD') {
                    fetchOptions.body = bodyStr;
                }

                fetch(url, fetchOptions)
                    .then(function(response) {
                        var res = new http.IncomingMessage();
                        res.statusCode = response.status;
                        res.statusMessage = response.statusText;
                        res.headers = {};

                        response.headers.forEach(function(value, key) {
                            res.headers[key.toLowerCase()] = value;
                        });

                        if (callback) callback(res);
                        req.emit('response', res);

                        return response.text().then(function(text) {
                            if (text) res.emit('data', text);
                            res.emit('end');
                        });
                    })
                    .catch(function(err) {
                        req.emit('error', err);
                    });
            },

            get: function(options, callback) {
                if (typeof options === 'string') {
                    options = { url: options };
                }
                options.method = 'GET';
                var req = this.request(options, callback);
                req.end();
                return req;
            },

            createServer: function(options, requestListener) {
                if (typeof options === 'function') {
                    requestListener = options;
                    options = {};
                }

                if (!options.key || !options.cert) {
                    console.warn('https.createServer requires key and cert options for TLS');
                }

                var httpServer = http.createServer(requestListener);

                httpServer._tlsOptions = {
                    key: options.key,
                    cert: options.cert,
                    ca: options.ca,
                    requestCert: options.requestCert || false,
                    rejectUnauthorized: options.rejectUnauthorized !== false
                };

                return httpServer;
            },

            METHODS: http.METHODS,
            STATUS_CODES: http.STATUS_CODES
        };

        return httpsModule;
    });

