    // ===== HTTPS MODULE (lazy loaded) =====
    _lazyModule('https', function() {
        const http = _modules.http;

        // HTTPS Agent extends HTTP Agent with TLS options
        class Agent extends http.Agent {
            constructor(options = {}) {
                super(options);
                // TLS-specific options
                this.rejectUnauthorized = options.rejectUnauthorized !== false; // Default true
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
                return `${options.host || options.hostname || 'localhost'}:${options.port || 443}:${options.localAddress || ''}`;
            }
        }

        var httpsModule = {
            Agent,
            globalAgent: new Agent(),
            IncomingMessage: http.IncomingMessage,
            ServerResponse: http.ServerResponse,

            request: function(options, callback) {
                // Handle URL string, options.url, or construct from hostname/host
                let url;
                if (typeof options === 'string') {
                    url = options;
                    // Ensure HTTPS
                    if (url.startsWith('http://')) {
                        url = 'https://' + url.slice(7);
                    } else if (!url.startsWith('https://')) {
                        url = 'https://' + url;
                    }
                } else if (options.url) {
                    url = options.url;
                    if (!url.startsWith('https://')) {
                        url = url.replace(/^http:\/\//, 'https://');
                    }
                } else {
                    const protocol = 'https:';
                    const port = options.port && options.port !== 443 ? ':' + options.port : '';
                    url = protocol + '//' + (options.hostname || options.host || 'localhost') + port + (options.path || '/');
                }

                const req = new EventEmitter();
                req._body = [];
                req.write = chunk => { req._body.push(chunk); return true; };
                req.end = data => {
                    if (data) req._body.push(data);

                    // Build fetch options
                    const fetchOptions = {
                        method: options.method || 'GET',
                        headers: options.headers || {},
                    };

                    // Add body for non-GET/HEAD requests
                    if (req._body.length && !['GET', 'HEAD'].includes(fetchOptions.method.toUpperCase())) {
                        fetchOptions.body = req._body.join('');
                    }

                    fetch(url, fetchOptions)
                        .then(async response => {
                            const res = new http.IncomingMessage();
                            res.statusCode = response.status;
                            res.statusMessage = response.statusText;
                            res.headers = {};

                            // Convert headers
                            response.headers.forEach((value, key) => {
                                res.headers[key.toLowerCase()] = value;
                            });

                            if (callback) callback(res);
                            req.emit('response', res);

                            // Stream the body
                            try {
                                const text = await response.text();
                                if (text) res.emit('data', text);
                            } catch (e) {
                                // Ignore body read errors
                            }
                            res.emit('end');
                        })
                        .catch(err => {
                            req.emit('error', err);
                        });
                };

                req.setTimeout = function(ms, callback) {
                    if (callback) req.on('timeout', callback);
                    return req;
                };

                req.abort = function() {
                    req.emit('abort');
                };

                req.destroy = function(err) {
                    if (err) req.emit('error', err);
                    req.emit('close');
                };

                return req;
            },

            get: function(options, callback) {
                if (typeof options === 'string') {
                    options = { url: options };
                }
                options.method = 'GET';
                const req = this.request(options, callback);
                req.end();
                return req;
            },

            createServer: function(options, requestListener) {
                // HTTPS server requires TLS certificates
                // In sandbox environment, we can't directly create TLS servers
                // This is a stub that creates an HTTP server and adds TLS properties
                if (typeof options === 'function') {
                    requestListener = options;
                    options = {};
                }

                // Check for required TLS options
                if (!options.key || !options.cert) {
                    console.warn('https.createServer requires key and cert options for TLS');
                }

                // Create underlying HTTP server with TLS properties
                const httpServer = http.createServer(requestListener);

                // Add TLS-specific properties
                httpServer._tlsOptions = {
                    key: options.key,
                    cert: options.cert,
                    ca: options.ca,
                    requestCert: options.requestCert || false,
                    rejectUnauthorized: options.rejectUnauthorized !== false,
                };

                // Override listen to log HTTPS
                const originalListen = httpServer.listen.bind(httpServer);
                httpServer.listen = function(...args) {
                    console.log('HTTPS server listening (Note: TLS handled at platform level in sandbox)');
                    return originalListen(...args);
                };

                return httpServer;
            },

            // Constants from HTTP module
            METHODS: http.METHODS,
            STATUS_CODES: http.STATUS_CODES
        };

        return httpsModule;
    });

