    // ===== STREAM MODULE =====
    class Stream extends EventEmitter {
        constructor(options) { super(); this._options = options || {}; }
        pipe(dest, options) {
            options = options || {};
            this.on('data', chunk => dest.write(chunk));
            // Node.js pipe options.end defaults to true - call dest.end() when source ends
            if (options.end !== false) {
                this.on('end', () => dest.end());
            }
            return dest;
        }
    }
    class Readable extends Stream {
        constructor(options) {
            super(options);
            options = options || {};
            this._readableState = {
                ended: false,
                buffer: [],
                highWaterMark: options.highWaterMark ?? 16384,
                encoding: options.encoding || null,
                objectMode: options.objectMode ?? false,
                flowing: null
            };
            if (options.read) this._read = options.read;
            if (options.destroy) this._destroy = options.destroy;
        }
        read(size) { return null; }
        _read(size) { /* subclass implements */ }
        push(chunk) {
            if (chunk === null) { this._readableState.ended = true; this.emit('end'); }
            else {
                if (this._readableState.encoding && Buffer.isBuffer(chunk)) {
                    chunk = chunk.toString(this._readableState.encoding);
                }
                this.emit('data', chunk);
            }
            return true;
        }
        setEncoding(encoding) {
            this._readableState.encoding = encoding;
            return this;
        }
        unshift(chunk) {
            if (chunk !== null && chunk !== undefined) {
                this._readableState.buffer.unshift(chunk);
            }
            return true;
        }
        // Flow control methods
        pause() { this._readableState.flowing = false; return this; }
        resume() { this._readableState.flowing = true; this.emit('resume'); return this; }
        isPaused() { return this._readableState.flowing === false; }
        // Properties
        get readable() { return !this._readableState.ended && !this._destroyed; }
        get readableLength() { return this._readableState.buffer?.length || 0; }
        get readableEnded() { return this._readableState.ended; }
        get readableFlowing() { return this._readableState.flowing; }
        get destroyed() { return this._destroyed || false; }
        destroy(err) {
            this._destroyed = true;
            if (this._destroy) this._destroy(err, () => {});
            if (err) this.emit('error', err);
            this.emit('close');
            return this;
        }
        // Async iterator support - required by SDK for stream validation
        async *[Symbol.asyncIterator]() {
            const chunks = [];
            let ended = false;
            let resolveWait = null;

            this.on('data', (chunk) => {
                chunks.push(chunk);
                if (resolveWait) { resolveWait(); resolveWait = null; }
            });
            this.on('end', () => {
                ended = true;
                if (resolveWait) { resolveWait(); resolveWait = null; }
            });

            while (true) {
                if (chunks.length > 0) {
                    yield chunks.shift();
                } else if (ended) {
                    return;
                } else {
                    await new Promise(resolve => { resolveWait = resolve; });
                }
            }
        }
    }
    class Writable extends Stream {
        constructor(options) {
            super(options);
            options = options || {};
            this._writableState = {
                ended: false,
                highWaterMark: options.highWaterMark ?? 16384,
                decodeStrings: options.decodeStrings ?? true,
                objectMode: options.objectMode ?? false,
                defaultEncoding: options.defaultEncoding || 'utf8'
            };
            if (options.write) this._write = options.write;
            if (options.writev) this._writev = options.writev;
            if (options.final) this._final = options.final;
            if (options.destroy) this._destroy = options.destroy;
        }
        write(chunk, encoding, callback) {
            if (typeof encoding === 'function') { callback = encoding; encoding = this._writableState.defaultEncoding; }
            encoding = encoding || this._writableState.defaultEncoding;
            this._write(chunk, encoding, callback || (() => {}));
            return true;
        }
        _write(chunk, encoding, callback) { callback(); }
        end(chunk, encoding, callback) {
            if (typeof chunk === 'function') { callback = chunk; chunk = null; encoding = null; }
            if (typeof encoding === 'function') { callback = encoding; encoding = null; }
            if (chunk) this.write(chunk, encoding);
            const doEnd = () => {
                this._writableState.ended = true;
                this.emit('finish');
                if (callback) callback();
            };
            if (this._final) {
                this._final(doEnd);
            } else {
                doEnd();
            }
            return this;
        }
        destroy(err) {
            this._destroyed = true;
            if (this._destroy) this._destroy(err, () => {});
            if (err) this.emit('error', err);
            this.emit('close');
            return this;
        }
        // Flow control methods
        cork() { this._writableState.corked = (this._writableState.corked || 0) + 1; }
        uncork() { if (this._writableState.corked > 0) this._writableState.corked--; }
        setDefaultEncoding(encoding) {
            this._writableState.defaultEncoding = encoding;
            return this;
        }
        // Properties
        get writable() { return !this._writableState.ended && !this._destroyed; }
        get writableLength() { return this._writableState.bufferedLength || 0; }
        get writableEnded() { return this._writableState.ended; }
        get writableFinished() { return this._writableState.finished || false; }
        get writableCorked() { return this._writableState.corked || 0; }
        get writableNeedDrain() { return this._writableState.needDrain || false; }
        get destroyed() { return this._destroyed || false; }
    }
    class Duplex extends Stream {
        constructor(options) {
            super(options);
            options = options || {};
            this._readableState = {
                ended: false,
                buffer: [],
                highWaterMark: options.readableHighWaterMark ?? options.highWaterMark ?? 16384,
                objectMode: options.readableObjectMode ?? options.objectMode ?? false
            };
            this._writableState = {
                ended: false,
                highWaterMark: options.writableHighWaterMark ?? options.highWaterMark ?? 16384,
                objectMode: options.writableObjectMode ?? options.objectMode ?? false,
                defaultEncoding: options.defaultEncoding || 'utf8'
            };
            this.allowHalfOpen = options.allowHalfOpen !== false;  // Default true
            if (options.read) this._read = options.read;
            if (options.write) this._write = options.write;
        }
        read() { return null; }
        push(chunk) {
            if (chunk === null) { this._readableState.ended = true; this.emit('end'); }
            else { this.emit('data', chunk); }
            return true;
        }
        write(chunk, encoding, callback) {
            if (typeof encoding === 'function') { callback = encoding; encoding = this._writableState.defaultEncoding; }
            if (this._write) this._write(chunk, encoding || this._writableState.defaultEncoding, callback || (() => {}));
            return true;
        }
        end(chunk, encoding, callback) {
            if (typeof chunk === 'function') { callback = chunk; chunk = null; }
            if (typeof encoding === 'function') { callback = encoding; encoding = null; }
            if (chunk) this.write(chunk, encoding);
            this._writableState.ended = true;
            this.emit('finish');
            // If allowHalfOpen is false, end the readable side too
            if (!this.allowHalfOpen && !this._readableState.ended) {
                this._readableState.ended = true;
                this.emit('end');
            }
            if (callback) callback();
            return this;
        }
        // Properties
        get readable() { return !this._readableState.ended && !this._destroyed; }
        get writable() { return !this._writableState.ended && !this._destroyed; }
        get readableEnded() { return this._readableState.ended; }
        get writableEnded() { return this._writableState.ended; }
        get destroyed() { return this._destroyed || false; }
        destroy(err) {
            this._destroyed = true;
            if (err) this.emit('error', err);
            this.emit('close');
            return this;
        }
    }
    class Transform extends Duplex {
        constructor(options) {
            super(options);
            if (options && options.transform) this._transform = options.transform;
            if (options && options.flush) this._flush = options.flush;
        }
        _transform(chunk, encoding, callback) { callback(null, chunk); }
        write(chunk, encoding, callback) {
            if (typeof encoding === 'function') { callback = encoding; encoding = this._writableState.defaultEncoding; }
            const transformCallback = (err, data) => {
                if (err) { if (callback) callback(err); return; }
                if (data !== undefined && data !== null) this.push(data);
                if (callback) callback();
            };
            this._transform(chunk, encoding || this._writableState.defaultEncoding, transformCallback);
            return true;
        }
        end(chunk, encoding, callback) {
            if (typeof chunk === 'function') { callback = chunk; chunk = null; }
            if (typeof encoding === 'function') { callback = encoding; encoding = null; }
            if (chunk) this.write(chunk, encoding);
            // Call _flush before finishing
            const doEnd = () => {
                this._writableState.ended = true;
                this.emit('finish');
                if (!this.allowHalfOpen && !this._readableState.ended) {
                    this._readableState.ended = true;
                    this.emit('end');
                }
                if (callback) callback();
            };
            if (this._flush) {
                this._flush((err, data) => {
                    if (data !== undefined && data !== null) this.push(data);
                    doEnd();
                });
            } else {
                doEnd();
            }
            return this;
        }
    }
    class PassThrough extends Transform {
        constructor(options) { super(options); }
    }

    // stream.pipeline(...streams, callback) - chain streams with proper error handling
    function pipeline(...args) {
        const callback = typeof args[args.length - 1] === 'function' ? args.pop() : null;
        const streams = args.flat();

        if (streams.length < 2) {
            const err = new Error('pipeline requires at least 2 streams');
            if (callback) { callback(err); return; }
            return Promise.reject(err);
        }

        const destroyer = (stream, err) => {
            if (stream && typeof stream.destroy === 'function' && !stream.destroyed) {
                stream.destroy(err);
            }
        };

        const promise = new Promise((resolve, reject) => {
            let error = null;

            const cleanup = (err) => {
                if (error) return;
                error = err;
                streams.forEach(s => destroyer(s, err));
            };

            // Chain streams with pipe
            for (let i = 0; i < streams.length - 1; i++) {
                const src = streams[i];
                const dest = streams[i + 1];

                src.pipe(dest);

                src.on('error', (err) => {
                    cleanup(err);
                    reject(err);
                    if (callback) callback(err);
                });
            }

            const lastStream = streams[streams.length - 1];
            lastStream.on('error', (err) => {
                cleanup(err);
                reject(err);
                if (callback) callback(err);
            });

            lastStream.on('finish', () => {
                if (!error) {
                    resolve(lastStream);
                    if (callback) callback(null, lastStream);
                }
            });

            lastStream.on('end', () => {
                if (!error) {
                    resolve(lastStream);
                    if (callback) callback(null, lastStream);
                }
            });
        });

        if (callback) return lastStream;
        return promise;
    }

    // stream.finished(stream, options, callback) - detect when stream is done
    function finished(stream, options, callback) {
        if (typeof options === 'function') {
            callback = options;
            options = {};
        }
        options = options || {};

        const promise = new Promise((resolve, reject) => {
            let done = false;

            const onFinish = () => {
                if (done) return;
                done = true;
                resolve();
                if (callback) callback();
            };

            const onError = (err) => {
                if (done) return;
                done = true;
                reject(err);
                if (callback) callback(err);
            };

            const onClose = () => {
                if (done) return;
                // Check if stream ended properly
                if (stream._readableState?.ended || stream._writableState?.ended) {
                    onFinish();
                } else {
                    onError(new Error('Premature close'));
                }
            };

            stream.on('end', onFinish);
            stream.on('finish', onFinish);
            stream.on('error', onError);
            stream.on('close', onClose);

            // Return cleanup function
            return () => {
                stream.removeListener('end', onFinish);
                stream.removeListener('finish', onFinish);
                stream.removeListener('error', onError);
                stream.removeListener('close', onClose);
            };
        });

        if (callback) return stream;
        return promise;
    }

    // Readable.from() - create readable from iterable
    Readable.from = function(iterable, options) {
        const readable = new Readable();
        (async () => {
            try {
                for await (const chunk of iterable) {
                    readable.push(chunk);
                }
                readable.push(null);
            } catch (err) {
                readable.emit('error', err);
            }
        })();
        return readable;
    };

    // Stream module - export Stream class as default (like events exports EventEmitter)
    // Some code does `require("stream")` and uses it directly as base class
    _modules.stream = Stream;
    _modules.stream.Stream = Stream;
    _modules.stream.Readable = Readable;
    _modules.stream.Writable = Writable;
    _modules.stream.Duplex = Duplex;
    _modules.stream.Transform = Transform;
    _modules.stream.PassThrough = PassThrough;
    _modules.stream.pipeline = pipeline;
    _modules.stream.finished = finished;
    _modules['node:stream'] = _modules.stream;

    // stream/promises module
    _modules['stream/promises'] = {
        pipeline: (...args) => {
            // Filter out callbacks - always return Promise
            const streams = args.filter(a => typeof a !== 'function');
            return pipeline(...streams);
        },
        finished: (stream, options) => finished(stream, options)
    };
    _modules['node:stream/promises'] = _modules['stream/promises'];

