    // ===== DNS MODULE (lazy loaded) =====
    // Wraps native Zig DNS functions with promises API
    _lazyModule('dns', function() {
        // Get native DNS module registered from Zig
        const _dns = globalThis._dns || {};

        // DNS server configuration (stored but system resolver is used)
        let _dnsServers = [];

        // DNS module with sync functions
        const dns = {
            // Core lookup function
            lookup: function(hostname, options, callback) {
                if (typeof options === 'function') {
                    callback = options;
                    options = {};
                }
                try {
                    const result = _dns.lookup(hostname, options);
                    if (callback) {
                        setImmediate(() => callback(null, result.address, result.family));
                    }
                    return result;
                } catch (err) {
                    if (callback) {
                        setImmediate(() => callback(err));
                    } else {
                        throw err;
                    }
                }
            },

            // Resolve functions with callback support
            resolve: function(hostname, rrtype, callback) {
                if (typeof rrtype === 'function') {
                    callback = rrtype;
                    rrtype = 'A';
                }
                try {
                    const result = _dns.resolve(hostname, rrtype);
                    if (callback) setImmediate(() => callback(null, result));
                    return result;
                } catch (err) {
                    if (callback) setImmediate(() => callback(err));
                    else throw err;
                }
            },

            resolve4: function(hostname, options, callback) {
                if (typeof options === 'function') { callback = options; options = {}; }
                try {
                    const result = _dns.resolve4(hostname);
                    if (callback) setImmediate(() => callback(null, result));
                    return result;
                } catch (err) {
                    if (callback) setImmediate(() => callback(err));
                    else throw err;
                }
            },

            resolve6: function(hostname, options, callback) {
                if (typeof options === 'function') { callback = options; options = {}; }
                try {
                    const result = _dns.resolve6(hostname);
                    if (callback) setImmediate(() => callback(null, result));
                    return result;
                } catch (err) {
                    if (callback) setImmediate(() => callback(err));
                    else throw err;
                }
            },

            resolveCname: function(hostname, callback) {
                try {
                    const result = _dns.resolveCname(hostname);
                    if (callback) setImmediate(() => callback(null, result));
                    return result;
                } catch (err) {
                    if (callback) setImmediate(() => callback(err));
                    else throw err;
                }
            },

            resolveMx: function(hostname, callback) {
                try {
                    const result = _dns.resolveMx(hostname);
                    if (callback) setImmediate(() => callback(null, result));
                    return result;
                } catch (err) {
                    if (callback) setImmediate(() => callback(err));
                    else throw err;
                }
            },

            resolveNs: function(hostname, callback) {
                try {
                    const result = _dns.resolveNs(hostname);
                    if (callback) setImmediate(() => callback(null, result));
                    return result;
                } catch (err) {
                    if (callback) setImmediate(() => callback(err));
                    else throw err;
                }
            },

            resolvePtr: function(hostname, callback) {
                try {
                    const result = _dns.resolvePtr(hostname);
                    if (callback) setImmediate(() => callback(null, result));
                    return result;
                } catch (err) {
                    if (callback) setImmediate(() => callback(err));
                    else throw err;
                }
            },

            resolveSoa: function(hostname, callback) {
                try {
                    const result = _dns.resolveSoa(hostname);
                    if (callback) setImmediate(() => callback(null, result));
                    return result;
                } catch (err) {
                    if (callback) setImmediate(() => callback(err));
                    else throw err;
                }
            },

            resolveSrv: function(hostname, callback) {
                try {
                    const result = _dns.resolveSrv(hostname);
                    if (callback) setImmediate(() => callback(null, result));
                    return result;
                } catch (err) {
                    if (callback) setImmediate(() => callback(err));
                    else throw err;
                }
            },

            resolveTxt: function(hostname, callback) {
                try {
                    const result = _dns.resolveTxt(hostname);
                    if (callback) setImmediate(() => callback(null, result));
                    return result;
                } catch (err) {
                    if (callback) setImmediate(() => callback(err));
                    else throw err;
                }
            },

            reverse: function(ip, callback) {
                try {
                    const result = _dns.reverse(ip);
                    if (callback) setImmediate(() => callback(null, result));
                    return result;
                } catch (err) {
                    if (callback) setImmediate(() => callback(err));
                    else throw err;
                }
            },

            // DNS error codes
            NODATA: 'ENODATA',
            FORMERR: 'EFORMERR',
            SERVFAIL: 'ESERVFAIL',
            NOTFOUND: 'ENOTFOUND',
            NOTIMP: 'ENOTIMP',
            REFUSED: 'EREFUSED',
            BADQUERY: 'EBADQUERY',
            BADNAME: 'EBADNAME',
            BADFAMILY: 'EBADFAMILY',
            BADRESP: 'EBADRESP',
            CONNREFUSED: 'ECONNREFUSED',
            TIMEOUT: 'ETIMEOUT',
            EOF: 'EEOF',
            FILE: 'EFILE',
            NOMEM: 'ENOMEM',
            DESTRUCTION: 'EDESTRUCTION',
            BADSTR: 'EBADSTR',
            BADFLAGS: 'EBADFLAGS',
            NONAME: 'ENONAME',
            BADHINTS: 'EBADHINTS',
            NOTINITIALIZED: 'ENOTINITIALIZED',
            LOADIPHLPAPI: 'ELOADIPHLPAPI',
            ADDRGETNETWORKPARAMS: 'EADDRGETNETWORKPARAMS',
            CANCELLED: 'ECANCELLED',

            // Lookup service - reverse lookup address and port to hostname and service
            lookupService: function(address, port, callback) {
                try {
                    const result = _dns.lookupService(address, port);
                    if (callback) {
                        setImmediate(() => callback(null, result.hostname, result.service));
                    }
                    return result;
                } catch (err) {
                    if (callback) {
                        setImmediate(() => callback(err));
                    } else {
                        throw err;
                    }
                }
            },

            // Get configured DNS servers
            // Note: Returns configured servers but system resolver is always used
            getServers: function() {
                if (_dnsServers.length > 0) {
                    return _dnsServers.slice();
                }
                // Return system defaults as fallback
                return ['8.8.8.8', '8.8.4.4'];
            },

            // Set DNS servers for resolution
            // Note: Stores servers but system resolver is used (WASI limitation)
            setServers: function(servers) {
                if (!Array.isArray(servers)) {
                    throw new TypeError('servers must be an array');
                }

                const validated = [];
                for (let i = 0; i < servers.length; i++) {
                    let server = servers[i];
                    if (typeof server !== 'string') {
                        throw new TypeError('each server must be a string');
                    }

                    // Handle [ipv6]:port format
                    server = server.replace(/^\[(.+)\](?::\d+)?$/, '$1');
                    // Handle ipv4:port format
                    server = server.replace(/^(.+?):\d+$/, '$1');

                    // Basic IP validation
                    const ipv4Regex = /^(\d{1,3}\.){3}\d{1,3}$/;
                    const ipv6Regex = /^([0-9a-fA-F]{0,4}:){2,7}[0-9a-fA-F]{0,4}$/;

                    if (!ipv4Regex.test(server) && !ipv6Regex.test(server) && server !== '::1' && server !== '::') {
                        throw new Error('Invalid IP address: ' + servers[i]);
                    }

                    validated.push(server);
                }

                _dnsServers = validated;
            }
        };

        // Promises API - Node.js dns.promises
        dns.promises = {
            lookup: function(hostname, options) {
                return new Promise((resolve, reject) => {
                    try {
                        const result = _dns.lookup(hostname, options);
                        resolve(result);
                    } catch (err) {
                        reject(err);
                    }
                });
            },

            resolve: function(hostname, rrtype) {
                rrtype = rrtype || 'A';
                return new Promise((resolve, reject) => {
                    try {
                        const result = _dns.resolve(hostname, rrtype);
                        resolve(result);
                    } catch (err) {
                        reject(err);
                    }
                });
            },

            resolve4: function(hostname, options) {
                return new Promise((resolve, reject) => {
                    try {
                        resolve(_dns.resolve4(hostname));
                    } catch (err) {
                        reject(err);
                    }
                });
            },

            resolve6: function(hostname, options) {
                return new Promise((resolve, reject) => {
                    try {
                        resolve(_dns.resolve6(hostname));
                    } catch (err) {
                        reject(err);
                    }
                });
            },

            resolveCname: function(hostname) {
                return new Promise((resolve, reject) => {
                    try {
                        resolve(_dns.resolveCname(hostname));
                    } catch (err) {
                        reject(err);
                    }
                });
            },

            resolveMx: function(hostname) {
                return new Promise((resolve, reject) => {
                    try {
                        resolve(_dns.resolveMx(hostname));
                    } catch (err) {
                        reject(err);
                    }
                });
            },

            resolveNs: function(hostname) {
                return new Promise((resolve, reject) => {
                    try {
                        resolve(_dns.resolveNs(hostname));
                    } catch (err) {
                        reject(err);
                    }
                });
            },

            resolvePtr: function(hostname) {
                return new Promise((resolve, reject) => {
                    try {
                        resolve(_dns.resolvePtr(hostname));
                    } catch (err) {
                        reject(err);
                    }
                });
            },

            resolveSoa: function(hostname) {
                return new Promise((resolve, reject) => {
                    try {
                        resolve(_dns.resolveSoa(hostname));
                    } catch (err) {
                        reject(err);
                    }
                });
            },

            resolveSrv: function(hostname) {
                return new Promise((resolve, reject) => {
                    try {
                        resolve(_dns.resolveSrv(hostname));
                    } catch (err) {
                        reject(err);
                    }
                });
            },

            resolveTxt: function(hostname) {
                return new Promise((resolve, reject) => {
                    try {
                        resolve(_dns.resolveTxt(hostname));
                    } catch (err) {
                        reject(err);
                    }
                });
            },

            reverse: function(ip) {
                return new Promise((resolve, reject) => {
                    try {
                        resolve(_dns.reverse(ip));
                    } catch (err) {
                        reject(err);
                    }
                });
            },

            lookupService: function(address, port) {
                return new Promise((resolve, reject) => {
                    try {
                        const result = _dns.lookupService(address, port);
                        resolve(result);
                    } catch (err) {
                        reject(err);
                    }
                });
            },

            getServers: function() {
                return Promise.resolve(dns.getServers());
            },

            setServers: function(servers) {
                return new Promise((resolve, reject) => {
                    try {
                        dns.setServers(servers);
                        resolve();
                    } catch (err) {
                        reject(err);
                    }
                });
            },

            // Resolver class for the promises API
            Resolver: class Resolver {
                constructor(options) {
                    this._servers = null; // null means use module defaults
                }

                getServers() {
                    return this._servers !== null ? this._servers.slice() : dns.getServers();
                }

                setServers(servers) {
                    // Validate servers
                    if (!Array.isArray(servers)) {
                        throw new TypeError('servers must be an array');
                    }
                    this._servers = servers.slice();
                }

                resolve(hostname, rrtype) { return dns.promises.resolve(hostname, rrtype); }
                resolve4(hostname, options) { return dns.promises.resolve4(hostname, options); }
                resolve6(hostname, options) { return dns.promises.resolve6(hostname, options); }
                resolveCname(hostname) { return dns.promises.resolveCname(hostname); }
                resolveMx(hostname) { return dns.promises.resolveMx(hostname); }
                resolveNs(hostname) { return dns.promises.resolveNs(hostname); }
                resolvePtr(hostname) { return dns.promises.resolvePtr(hostname); }
                resolveSoa(hostname) { return dns.promises.resolveSoa(hostname); }
                resolveSrv(hostname) { return dns.promises.resolveSrv(hostname); }
                resolveTxt(hostname) { return dns.promises.resolveTxt(hostname); }
                reverse(ip) { return dns.promises.reverse(ip); }
                lookupService(address, port) { return dns.promises.lookupService(address, port); }

                cancel() {
                    // Cancellation not supported - synchronous resolution
                }
            }
        };

        // Resolver class for callback API
        dns.Resolver = class Resolver {
            constructor(options) {
                this._servers = null; // null means use module defaults
            }

            getServers() {
                return this._servers !== null ? this._servers.slice() : dns.getServers();
            }

            setServers(servers) {
                // Validate servers
                if (!Array.isArray(servers)) {
                    throw new TypeError('servers must be an array');
                }
                this._servers = servers.slice();
            }

            resolve(hostname, rrtype, callback) { return dns.resolve(hostname, rrtype, callback); }
            resolve4(hostname, options, callback) { return dns.resolve4(hostname, options, callback); }
            resolve6(hostname, options, callback) { return dns.resolve6(hostname, options, callback); }
            resolveCname(hostname, callback) { return dns.resolveCname(hostname, callback); }
            resolveMx(hostname, callback) { return dns.resolveMx(hostname, callback); }
            resolveNs(hostname, callback) { return dns.resolveNs(hostname, callback); }
            resolvePtr(hostname, callback) { return dns.resolvePtr(hostname, callback); }
            resolveSoa(hostname, callback) { return dns.resolveSoa(hostname, callback); }
            resolveSrv(hostname, callback) { return dns.resolveSrv(hostname, callback); }
            resolveTxt(hostname, callback) { return dns.resolveTxt(hostname, callback); }
            reverse(ip, callback) { return dns.reverse(ip, callback); }
            lookupService(address, port, callback) { return dns.lookupService(address, port, callback); }

            cancel() {
                // Cancellation not supported - synchronous resolution
            }
        };

        return dns;
    });

