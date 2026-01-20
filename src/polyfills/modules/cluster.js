    // ===== CLUSTER MODULE (lazy loaded) =====
    _lazyModule('cluster', function() {
        var _clusterWorkerId = 0;
        var _clusterWorkers = {};
        var _isClusterWorker = typeof process.env.CLUSTER_WORKER_ID !== 'undefined';
        var _clusterSettings = { exec: null, args: [], silent: false };

        class ClusterWorker extends EventEmitter {
            constructor(id, child) {
                super();
                this.id = id;
                this.process = child;
                this.exitedAfterDisconnect = false;
                this.state = 'online';
                this.isDead = function() { return this.state === 'dead'; };
                this.isConnected = function() { return this.state === 'online'; };
            }
            send(message, sendHandle, options, callback) {
                if (typeof options === 'function') { callback = options; options = undefined; }
                if (typeof sendHandle === 'function') { callback = sendHandle; sendHandle = undefined; }
                var msg = '__CLUSTER_MSG__:' + JSON.stringify(message) + '\n';
                if (this.process && this.process.stdin) {
                    this.process.stdin.write(msg);
                }
                if (callback) setTimeout(callback, 0);
                return true;
            }
            kill(signal) {
                this.state = 'dead';
                if (this.process && this.process.kill) {
                    this.process.kill(signal);
                }
            }
            disconnect() {
                this.exitedAfterDisconnect = true;
                this.state = 'disconnected';
                this.emit('disconnect');
            }
        }

        var clusterModule = Object.assign(new EventEmitter(), {
            isMaster: !_isClusterWorker,
            isPrimary: !_isClusterWorker,
            isWorker: _isClusterWorker,
            workers: _clusterWorkers,
            worker: null,
            settings: _clusterSettings,
            SCHED_NONE: 1,
            SCHED_RR: 2,
            schedulingPolicy: 2,

            setupPrimary: function(settings) {
                if (settings) Object.assign(_clusterSettings, settings);
            },
            setupMaster: function(settings) { this.setupPrimary(settings); },

            fork: function(env) {
                var self = this;
                var id = ++_clusterWorkerId;
                var script = _clusterSettings.exec || process.argv[1];
                var args = _clusterSettings.args || [];

                var workerEnv = Object.assign({}, process.env, env || {});
                workerEnv.CLUSTER_WORKER_ID = String(id);

                var cp = _modules.child_process;
                var child = cp.spawn(process.argv[0], [script].concat(args), {
                    env: workerEnv,
                    stdio: _clusterSettings.silent ? 'pipe' : 'inherit'
                });

                var worker = new ClusterWorker(id, child);
                _clusterWorkers[id] = worker;

                child.on('exit', function(code, signal) {
                    worker.state = 'dead';
                    worker.emit('exit', code, signal);
                    self.emit('exit', worker, code, signal);
                    delete _clusterWorkers[id];
                });

                child.on('error', function(err) {
                    worker.emit('error', err);
                });

                if (!_clusterSettings.silent && child.stdout) {
                    child.stdout.on('data', function(data) {
                        var lines = data.toString().split('\n');
                        for (var i = 0; i < lines.length; i++) {
                            var line = lines[i].trim();
                            if (line.startsWith('__CLUSTER_MSG__:')) {
                                try {
                                    var msg = JSON.parse(line.slice(16));
                                    worker.emit('message', msg);
                                    self.emit('message', worker, msg);
                                } catch (e) {}
                            }
                        }
                    });
                }

                setTimeout(function() {
                    worker.emit('online');
                    self.emit('online', worker);
                }, 0);

                return worker;
            },

            disconnect: function(callback) {
                var workers = Object.values(_clusterWorkers);
                var pending = workers.length;
                if (pending === 0 && callback) return setTimeout(callback, 0);
                workers.forEach(function(worker) {
                    worker.disconnect();
                    worker.once('disconnect', function() {
                        pending--;
                        if (pending === 0 && callback) callback();
                    });
                });
            }
        });

        // Set up worker-side if in cluster worker
        if (_isClusterWorker) {
            clusterModule.worker = {
                id: parseInt(process.env.CLUSTER_WORKER_ID, 10),
                send: function(message, sendHandle, options, callback) {
                    if (typeof options === 'function') { callback = options; options = undefined; }
                    if (typeof sendHandle === 'function') { callback = sendHandle; sendHandle = undefined; }
                    print('__CLUSTER_MSG__:' + JSON.stringify(message));
                    if (callback) setTimeout(callback, 0);
                    return true;
                },
                disconnect: function() { process.exit(0); },
                kill: function(signal) { process.exit(1); },
                isDead: function() { return false; },
                isConnected: function() { return true; }
            };
        }

        return clusterModule;
    });

    // Diagnostics channel module
    class DiagnosticsChannel {
        constructor(name) { this.name = name; this._subscribers = []; }
        subscribe(fn) { this._subscribers.push(fn); }
        unsubscribe(fn) { this._subscribers = this._subscribers.filter(s => s !== fn); }
        publish(message) { this._subscribers.forEach(fn => fn(message, this.name)); }
        get hasSubscribers() { return this._subscribers.length > 0; }
    }
    const _channels = new Map();
    _modules.diagnostics_channel = {
        channel: (name) => {
            if (!_channels.has(name)) _channels.set(name, new DiagnosticsChannel(name));
            return _channels.get(name);
        },
        hasSubscribers: (name) => _channels.has(name) && _channels.get(name).hasSubscribers,
        subscribe: (name, fn) => _modules.diagnostics_channel.channel(name).subscribe(fn),
        unsubscribe: (name, fn) => { if (_channels.has(name)) _channels.get(name).unsubscribe(fn); },
        Channel: DiagnosticsChannel
    };

    // Punycode module (deprecated but still used)
    _modules.punycode = {
        encode: (s) => s,
        decode: (s) => s,
        toASCII: (s) => s,
        toUnicode: (s) => s
    };

    // Console module (alias to global console)
    _modules.console = globalThis.console || {
        log: print,
        error: print,
        warn: print,
        info: print,
        debug: print,
        trace: print,
        dir: (obj) => print(JSON.stringify(obj, null, 2)),
        time: () => {},
        timeEnd: () => {},
        assert: (cond, msg) => { if (!cond) throw new Error(msg); }
    };

    // node-fetch module (uses our global fetch)
    // Claude Code uses node-fetch for HTTP requests to Anthropic API
    const nodeFetch = globalThis.fetch || function() { throw new Error('fetch not available'); };
    nodeFetch.default = nodeFetch;
    nodeFetch.Headers = globalThis.Headers || function(init) { this._headers = init || {}; };
    nodeFetch.Request = globalThis.Request || function(url, opts) { this.url = url; this.method = opts?.method || 'GET'; };
    nodeFetch.Response = globalThis.Response || function(body, opts) { this.body = body; this.status = opts?.status || 200; };
    _modules['node-fetch'] = nodeFetch;
    _modules['node:node-fetch'] = _modules['node-fetch'];
