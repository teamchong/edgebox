    // ===== DNS MODULE (lazy loaded) =====
    // Wraps native Zig DNS functions with promises API
    _lazyModule('dns', function() {
        // Get native DNS module registered from Zig
        const _dns = globalThis._dns || {};

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

            // Lookup service - requires getnameinfo implementation
            lookupService: function(address, port, callback) {
                const err = new Error('lookupService not implemented');
                err.code = 'ENOSYS';
                if (callback) setImmediate(() => callback(err));
                else throw err;
            },

            // Get servers - returns empty array (would need res_getservers)
            getServers: function() {
                return [];
            },

            setServers: function(servers) {
                // Server configuration not supported in WASI environment
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
                return Promise.reject(Object.assign(new Error('lookupService not implemented'), { code: 'ENOSYS' }));
            },

            getServers: function() {
                return Promise.resolve([]);
            },

            setServers: function(servers) {
                return Promise.resolve();
            },

            // Resolver class for the promises API
            Resolver: class Resolver {
                constructor(options) {
                    this._servers = [];
                }

                getServers() { return this._servers; }
                setServers(servers) { this._servers = servers; }

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

                cancel() {
                    // Cancellation not supported - synchronous resolution
                }
            }
        };

        // Resolver class for callback API
        dns.Resolver = class Resolver {
            constructor(options) {
                this._servers = [];
            }

            getServers() { return this._servers; }
            setServers(servers) { this._servers = servers; }

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

            cancel() {
                // Cancellation not supported - synchronous resolution
            }
        };

        return dns;
    });

