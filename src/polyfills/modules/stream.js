    // ===== STREAM MODULE =====
    class Stream extends EventEmitter {
        constructor(options) { super(); this._options = options || {}; }
        pipe(dest, options) {
            options = options || {};
            const src = this;
            let flowing = true;

            const onData = (chunk) => {
                const ret = dest.write(chunk);
                // If write returns false, pause the source until drain
                if (ret === false && src.pause) {
                    flowing = false;
                    src.pause();
                }
            };

            const onDrain = () => {
                if (!flowing && src.resume) {
                    flowing = true;
                    src.resume();
                }
            };

            this.on('data', onData);
            dest.on('drain', onDrain);

            // Node.js pipe options.end defaults to true - call dest.end() when source ends
            if (options.end !== false) {
                this.on('end', () => dest.end());
            }

            // Handle errors
            const onError = (err) => {
                dest.emit('error', err);
            };
            this.on('error', onError);

            // Cleanup function
            dest._pipeCleanup = () => {
                src.removeListener('data', onData);
                src.removeListener('error', onError);
                dest.removeListener('drain', onDrain);
            };

            return dest;
        }
        unpipe(dest) {
            if (dest && dest._pipeCleanup) {
                dest._pipeCleanup();
            }
            return this;
        }
    }
    class Readable extends Stream {
        constructor(options) {
            super(options);
            options = options || {};
            this._readableState = {
                ended: false,
                buffer: [],
                length: 0, // Track buffer byte length for backpressure
                highWaterMark: options.highWaterMark ?? 16384,
                encoding: options.encoding || null,
                objectMode: options.objectMode ?? false,
                flowing: null,
                reading: false,
                needReadable: false
            };
            if (options.read) this._read = options.read;
            if (options.destroy) this._destroy = options.destroy;
        }
        read(size) {
            const state = this._readableState;
            if (state.length === 0 && state.ended) return null;

            // If no data available
            if (state.buffer.length === 0) {
                if (!state.reading && !state.ended) {
                    state.reading = true;
                    state.needReadable = true;
                    this._read(state.highWaterMark);
                }
                return null;
            }

            // Get data from buffer
            let chunk;
            if (state.objectMode) {
                chunk = state.buffer.shift();
                state.length -= 1;
            } else {
                // For non-object mode, concatenate or return single chunk
                if (size === undefined || size >= state.length) {
                    // Return all buffered data
                    if (state.buffer.length === 1) {
                        chunk = state.buffer.shift();
                    } else {
                        chunk = Buffer.concat(state.buffer);
                        state.buffer = [];
                    }
                    state.length = 0;
                } else {
                    // Return requested size
                    chunk = state.buffer[0];
                    if (chunk.length <= size) {
                        state.buffer.shift();
                        state.length -= chunk.length;
                    } else {
                        state.buffer[0] = chunk.slice(size);
                        chunk = chunk.slice(0, size);
                        state.length -= size;
                    }
                }
            }

            // Trigger more reads if buffer is low
            if (!state.reading && state.length < state.highWaterMark && !state.ended) {
                state.reading = true;
                this._read(state.highWaterMark);
            }

            return chunk;
        }
        _read(size) { /* subclass implements */ }
        push(chunk) {
            const state = this._readableState;
            state.reading = false;

            if (chunk === null) {
                state.ended = true;
                // Emit 'end' when buffer is empty
                if (state.length === 0) {
                    this.emit('end');
                }
                return false;
            }

            // Track chunk size
            let chunkSize;
            if (state.objectMode) {
                chunkSize = 1;
            } else if (typeof chunk === 'string') {
                if (state.encoding) {
                    chunk = Buffer.from(chunk, state.encoding);
                } else {
                    chunk = Buffer.from(chunk);
                }
                chunkSize = chunk.length;
            } else if (Buffer.isBuffer(chunk) || chunk instanceof Uint8Array) {
                chunkSize = chunk.length;
            } else {
                chunkSize = 1;
            }

            // Add to buffer
            state.buffer.push(chunk);
            state.length += chunkSize;

            // Emit data if flowing
            if (state.flowing !== false) {
                this.emit('data', chunk);
            }

            // Return false if buffer exceeds highWaterMark (backpressure signal)
            return state.length < state.highWaterMark;
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
        get readableLength() { return this._readableState.length || 0; }
        get readableEnded() { return this._readableState.ended; }
        get readableFlowing() { return this._readableState.flowing; }
        get readableHighWaterMark() { return this._readableState.highWaterMark; }
        get readableObjectMode() { return this._readableState.objectMode; }
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
                defaultEncoding: options.defaultEncoding || 'utf8',
                bufferedLength: 0,
                needDrain: false,
                writing: false,
                buffer: [],
                corked: 0,
                finished: false
            };
            if (options.write) this._write = options.write;
            if (options.writev) this._writev = options.writev;
            if (options.final) this._final = options.final;
            if (options.destroy) this._destroy = options.destroy;
        }
        write(chunk, encoding, callback) {
            if (typeof encoding === 'function') { callback = encoding; encoding = this._writableState.defaultEncoding; }
            encoding = encoding || this._writableState.defaultEncoding;
            const state = this._writableState;

            // Calculate chunk size for backpressure
            let chunkSize;
            if (state.objectMode) {
                chunkSize = 1;
            } else if (typeof chunk === 'string') {
                chunkSize = Buffer.byteLength(chunk, encoding);
            } else if (Buffer.isBuffer(chunk) || chunk instanceof Uint8Array) {
                chunkSize = chunk.length;
            } else {
                chunkSize = 1;
            }

            // Add to buffer length
            state.bufferedLength += chunkSize;

            // Determine if we need to signal backpressure
            const ret = state.bufferedLength < state.highWaterMark;
            if (!ret) {
                state.needDrain = true;
            }

            // If corked, buffer the write
            if (state.corked > 0) {
                state.buffer.push({ chunk, encoding, callback, size: chunkSize });
                return ret;
            }

            // Perform the write
            const self = this;
            const writeCallback = (err) => {
                state.bufferedLength -= chunkSize;
                if (err) {
                    if (callback) callback(err);
                    self.emit('error', err);
                    return;
                }
                if (callback) callback();
                // Emit drain if we were above highWaterMark and now below
                if (state.needDrain && state.bufferedLength < state.highWaterMark) {
                    state.needDrain = false;
                    self.emit('drain');
                }
            };

            this._write(chunk, encoding, writeCallback);
            return ret;
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
        uncork() {
            const state = this._writableState;
            if (state.corked > 0) state.corked--;
            // Flush buffer if uncorked
            if (state.corked === 0 && state.buffer.length > 0) {
                const buffered = state.buffer;
                state.buffer = [];
                const self = this;
                const processNext = (index) => {
                    if (index >= buffered.length) {
                        // Check for drain after flushing buffer
                        if (state.needDrain && state.bufferedLength < state.highWaterMark) {
                            state.needDrain = false;
                            self.emit('drain');
                        }
                        return;
                    }
                    const { chunk, encoding, callback, size } = buffered[index];
                    self._write(chunk, encoding, (err) => {
                        state.bufferedLength -= size;
                        if (err) {
                            if (callback) callback(err);
                            self.emit('error', err);
                            return;
                        }
                        if (callback) callback();
                        processNext(index + 1);
                    });
                };
                processNext(0);
            }
        }
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
        get writableHighWaterMark() { return this._writableState.highWaterMark; }
        get writableObjectMode() { return this._writableState.objectMode; }
        get destroyed() { return this._destroyed || false; }
    }
    class Duplex extends Stream {
        constructor(options) {
            super(options);
            options = options || {};
            this._readableState = {
                ended: false,
                buffer: [],
                length: 0,
                highWaterMark: options.readableHighWaterMark ?? options.highWaterMark ?? 16384,
                objectMode: options.readableObjectMode ?? options.objectMode ?? false,
                flowing: null
            };
            this._writableState = {
                ended: false,
                highWaterMark: options.writableHighWaterMark ?? options.highWaterMark ?? 16384,
                objectMode: options.writableObjectMode ?? options.objectMode ?? false,
                defaultEncoding: options.defaultEncoding || 'utf8',
                bufferedLength: 0,
                needDrain: false
            };
            this.allowHalfOpen = options.allowHalfOpen !== false;  // Default true
            if (options.read) this._read = options.read;
            if (options.write) this._write = options.write;
        }
        read() { return null; }
        push(chunk) {
            const state = this._readableState;
            if (chunk === null) {
                state.ended = true;
                if (state.length === 0) this.emit('end');
                return false;
            }
            // Track chunk size
            let chunkSize = state.objectMode ? 1 : (chunk.length || 1);
            state.buffer.push(chunk);
            state.length += chunkSize;
            if (state.flowing !== false) this.emit('data', chunk);
            return state.length < state.highWaterMark;
        }
        write(chunk, encoding, callback) {
            if (typeof encoding === 'function') { callback = encoding; encoding = this._writableState.defaultEncoding; }
            const state = this._writableState;
            let chunkSize = state.objectMode ? 1 : (chunk.length || Buffer.byteLength(chunk) || 1);
            state.bufferedLength += chunkSize;
            const ret = state.bufferedLength < state.highWaterMark;
            if (!ret) state.needDrain = true;

            const self = this;
            if (this._write) this._write(chunk, encoding || state.defaultEncoding, (err) => {
                state.bufferedLength -= chunkSize;
                if (err) { if (callback) callback(err); self.emit('error', err); return; }
                if (callback) callback();
                if (state.needDrain && state.bufferedLength < state.highWaterMark) {
                    state.needDrain = false;
                    self.emit('drain');
                }
            });
            return ret;
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
        get readableLength() { return this._readableState.length || 0; }
        get writableLength() { return this._writableState.bufferedLength || 0; }
        get readableHighWaterMark() { return this._readableState.highWaterMark; }
        get writableHighWaterMark() { return this._writableState.highWaterMark; }
        get writableNeedDrain() { return this._writableState.needDrain || false; }
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

    // stream.pipeline(...streams, [options], callback) - chain streams with proper error handling
    function pipeline(...args) {
        let callback = typeof args[args.length - 1] === 'function' ? args.pop() : null;
        let options = {};

        // Check if last arg is options object with signal
        if (args.length > 0 && args[args.length - 1] && typeof args[args.length - 1] === 'object' &&
            !(args[args.length - 1].pipe) && !(args[args.length - 1]._read) && !(args[args.length - 1]._write)) {
            options = args.pop();
        }

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

        const lastStream = streams[streams.length - 1];

        const promise = new Promise((resolve, reject) => {
            let error = null;
            let abortHandler = null;

            const cleanup = (err) => {
                if (error) return;
                error = err;
                // Remove abort handler if set
                if (abortHandler && options.signal) {
                    options.signal.removeEventListener('abort', abortHandler);
                }
                streams.forEach(s => destroyer(s, err));
            };

            // Handle AbortSignal
            if (options.signal) {
                if (options.signal.aborted) {
                    // Already aborted
                    const err = new Error('The operation was aborted');
                    err.name = 'AbortError';
                    err.code = 'ABORT_ERR';
                    cleanup(err);
                    if (callback) callback(err);
                    reject(err);
                    return;
                }

                abortHandler = () => {
                    const err = new Error('The operation was aborted');
                    err.name = 'AbortError';
                    err.code = 'ABORT_ERR';
                    cleanup(err);
                    reject(err);
                    if (callback) callback(err);
                };
                options.signal.addEventListener('abort', abortHandler);
            }

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

            lastStream.on('error', (err) => {
                cleanup(err);
                reject(err);
                if (callback) callback(err);
            });

            lastStream.on('finish', () => {
                if (!error) {
                    if (abortHandler && options.signal) {
                        options.signal.removeEventListener('abort', abortHandler);
                    }
                    resolve(lastStream);
                    if (callback) callback(null, lastStream);
                }
            });

            lastStream.on('end', () => {
                if (!error) {
                    if (abortHandler && options.signal) {
                        options.signal.removeEventListener('abort', abortHandler);
                    }
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
            let abortHandler = null;

            const cleanup = () => {
                stream.removeListener('end', onFinish);
                stream.removeListener('finish', onFinish);
                stream.removeListener('error', onError);
                stream.removeListener('close', onClose);
                if (abortHandler && options.signal) {
                    options.signal.removeEventListener('abort', abortHandler);
                }
            };

            const onFinish = () => {
                if (done) return;
                done = true;
                cleanup();
                resolve();
                if (callback) callback();
            };

            const onError = (err) => {
                if (done) return;
                done = true;
                cleanup();
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

            // Handle AbortSignal
            if (options.signal) {
                if (options.signal.aborted) {
                    // Already aborted
                    const err = new Error('The operation was aborted');
                    err.name = 'AbortError';
                    err.code = 'ABORT_ERR';
                    if (stream.destroy) stream.destroy(err);
                    if (callback) callback(err);
                    reject(err);
                    return;
                }

                abortHandler = () => {
                    const err = new Error('The operation was aborted');
                    err.name = 'AbortError';
                    err.code = 'ABORT_ERR';
                    done = true;
                    cleanup();
                    if (stream.destroy) stream.destroy(err);
                    reject(err);
                    if (callback) callback(err);
                };
                options.signal.addEventListener('abort', abortHandler);
            }

            stream.on('end', onFinish);
            stream.on('finish', onFinish);
            stream.on('error', onError);
            stream.on('close', onClose);
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

    // Duplex.from() - create Duplex from async generator or iterable pair
    Duplex.from = function(source) {
        if (typeof source === 'function') {
            // Async generator function
            const duplex = new Duplex({
                objectMode: true,
                read() {},
                write(chunk, enc, cb) {
                    this._inputBuffer.push(chunk);
                    if (this._inputResolve) {
                        this._inputResolve();
                        this._inputResolve = null;
                    }
                    cb();
                }
            });
            duplex._inputBuffer = [];
            duplex._inputResolve = null;
            duplex._inputEnded = false;

            // Create async iterable for input
            const inputIterable = {
                [Symbol.asyncIterator]: () => ({
                    async next() {
                        while (duplex._inputBuffer.length === 0 && !duplex._inputEnded) {
                            await new Promise(resolve => { duplex._inputResolve = resolve; });
                        }
                        if (duplex._inputBuffer.length > 0) {
                            return { done: false, value: duplex._inputBuffer.shift() };
                        }
                        return { done: true, value: undefined };
                    }
                })
            };

            // Handle end of writable side
            duplex.on('finish', () => {
                duplex._inputEnded = true;
                if (duplex._inputResolve) {
                    duplex._inputResolve();
                    duplex._inputResolve = null;
                }
            });

            // Run generator
            (async () => {
                try {
                    const gen = source(inputIterable);
                    for await (const chunk of gen) {
                        duplex.push(chunk);
                    }
                    duplex.push(null);
                } catch (err) {
                    duplex.destroy(err);
                }
            })();

            return duplex;
        }

        // Iterable pair { readable, writable }
        if (source && typeof source === 'object') {
            if (source.readable && source.writable) {
                const duplex = new Duplex({
                    read() {
                        const chunk = source.readable.read();
                        if (chunk !== null) this.push(chunk);
                    },
                    write(chunk, enc, cb) {
                        source.writable.write(chunk, enc, cb);
                    }
                });
                source.readable.on('data', (chunk) => duplex.push(chunk));
                source.readable.on('end', () => duplex.push(null));
                return duplex;
            }
        }

        throw new TypeError('Duplex.from: source must be a function or { readable, writable } pair');
    };

    // stream.addAbortSignal() - add AbortSignal to an existing stream
    function addAbortSignal(signal, stream) {
        if (!signal || typeof signal.aborted !== 'boolean') {
            throw new TypeError('The first argument must be an AbortSignal');
        }
        if (!stream || typeof stream.destroy !== 'function') {
            throw new TypeError('The second argument must be a stream');
        }

        if (signal.aborted) {
            const err = new Error('The operation was aborted');
            err.name = 'AbortError';
            err.code = 'ABORT_ERR';
            stream.destroy(err);
            return stream;
        }

        const onAbort = () => {
            const err = new Error('The operation was aborted');
            err.name = 'AbortError';
            err.code = 'ABORT_ERR';
            stream.destroy(err);
        };

        signal.addEventListener('abort', onAbort, { once: true });

        // Clean up on stream close
        const cleanup = () => {
            signal.removeEventListener('abort', onAbort);
        };
        stream.once('close', cleanup);
        stream.once('error', cleanup);

        return stream;
    }

    // stream.compose() - compose multiple streams into a single Duplex
    function compose(...streams) {
        // Handle arrays as single argument
        if (streams.length === 1 && Array.isArray(streams[0])) {
            streams = streams[0];
        }

        if (streams.length === 0) {
            throw new Error('compose requires at least one stream');
        }
        if (streams.length === 1) {
            return streams[0];
        }

        const first = streams[0];
        const last = streams[streams.length - 1];

        // Pipe all streams together
        for (let i = 0; i < streams.length - 1; i++) {
            streams[i].pipe(streams[i + 1]);
        }

        // Create Duplex that wraps the pipeline
        const composed = new Duplex({
            readableObjectMode: last._readableState?.objectMode || false,
            writableObjectMode: first._writableState?.objectMode || false,
            read(size) {
                // Data flows through from last stream via 'data' event
            },
            write(chunk, encoding, callback) {
                first.write(chunk, encoding, callback);
            },
            final(callback) {
                first.end(callback);
            }
        });

        // Forward data from last stream
        last.on('data', (chunk) => composed.push(chunk));
        last.on('end', () => composed.push(null));

        // Forward errors from any stream
        for (const stream of streams) {
            stream.on('error', (err) => composed.emit('error', err));
        }

        return composed;
    }

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
    _modules.stream.addAbortSignal = addAbortSignal;
    _modules.stream.compose = compose;
    _modules['node:stream'] = _modules.stream;

    // Make stream classes available globally for other modules (e.g., zlib)
    globalThis.Readable = Readable;
    globalThis.Writable = Writable;
    globalThis.Duplex = Duplex;
    globalThis.Transform = Transform;
    globalThis.PassThrough = PassThrough;

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

