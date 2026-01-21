// EdgeBox Runtime: Web Streams API (ReadableStream, WritableStream, TransformStream)

if (typeof ReadableStream === 'undefined') {
    class ReadableStreamDefaultReader {
        constructor(stream) {
            if (stream._locked) {
                throw new TypeError('ReadableStream is locked');
            }
            this._stream = stream;
            this._stream._locked = true;
            this._stream._reader = this;
            this._closedPromise = new Promise((resolve, reject) => {
                this._closedResolve = resolve;
                this._closedReject = reject;
            });
        }
        read() {
            if (!this._stream) {
                return Promise.reject(new TypeError('Reader has been released'));
            }
            return this._stream._read();
        }
        releaseLock() {
            if (this._stream) {
                this._stream._locked = false;
                this._stream._reader = null;
                this._stream = null;
            }
        }
        get closed() {
            return this._closedPromise;
        }
        cancel(reason) {
            if (!this._stream) {
                return Promise.reject(new TypeError('Reader has been released'));
            }
            return this._stream.cancel(reason);
        }
    }

    globalThis.ReadableStream = class ReadableStream {
        constructor(underlyingSource = {}, strategy = {}) {
            this._source = underlyingSource;
            this._queue = [];
            this._closed = false;
            this._locked = false;
            this._reader = null;
            this._error = null;
            this._pullPending = false;
            this._started = false;

            const self = this;
            this._controller = {
                desiredSize: strategy.highWaterMark || 1,
                enqueue: (chunk) => {
                    if (self._closed) return;
                    self._queue.push(chunk);
                    self._resolveWaitingReaders();
                },
                close: () => {
                    self._closed = true;
                    self._resolveWaitingReaders();
                    if (self._reader && self._reader._closedResolve) {
                        self._reader._closedResolve();
                    }
                },
                error: (e) => {
                    self._error = e;
                    self._closed = true;
                    self._resolveWaitingReaders();
                    if (self._reader && self._reader._closedReject) {
                        self._reader._closedReject(e);
                    }
                }
            };

            this._waitingReaders = [];

            if (underlyingSource.start) {
                setTimeout(() => {
                    try {
                        const result = underlyingSource.start(this._controller);
                        if (result && typeof result.then === 'function') {
                            result.then(() => { this._started = true; }).catch(e => this._controller.error(e));
                        } else {
                            this._started = true;
                        }
                    } catch (e) {
                        this._controller.error(e);
                    }
                });
            } else {
                this._started = true;
            }
        }

        _resolveWaitingReaders() {
            while (this._waitingReaders.length > 0 && (this._queue.length > 0 || this._closed || this._error)) {
                const { resolve, reject } = this._waitingReaders.shift();
                if (this._error) {
                    reject(this._error);
                } else if (this._queue.length > 0) {
                    resolve({ value: this._queue.shift(), done: false });
                } else {
                    resolve({ value: undefined, done: true });
                }
            }
        }

        getReader(options) {
            if (options && options.mode === 'byob') {
                throw new TypeError('BYOB readers not supported');
            }
            return new ReadableStreamDefaultReader(this);
        }

        _read() {
            if (this._error) return Promise.reject(this._error);
            if (this._queue.length > 0) {
                return Promise.resolve({ value: this._queue.shift(), done: false });
            }
            if (this._closed) {
                return Promise.resolve({ value: undefined, done: true });
            }

            if (this._source.pull && !this._pullPending) {
                this._pullPending = true;
                const pullPromise = new Promise((resolve, reject) => {
                    this._waitingReaders.push({ resolve, reject });
                });

                setTimeout(() => {
                    try {
                        const result = this._source.pull(this._controller);
                        this._pullPending = false;
                        if (result && typeof result.then === 'function') {
                            result.then(() => {
                                this._resolveWaitingReaders();
                            }).catch(e => {
                                this._controller.error(e);
                            });
                        } else {
                            this._resolveWaitingReaders();
                        }
                    } catch (e) {
                        this._pullPending = false;
                        this._controller.error(e);
                    }
                });

                return pullPromise;
            }

            return new Promise((resolve, reject) => {
                this._waitingReaders.push({ resolve, reject });
                setTimeout(() => {
                    const idx = this._waitingReaders.findIndex(r => r.resolve === resolve);
                    if (idx >= 0) {
                        this._waitingReaders.splice(idx, 1);
                        resolve({ value: undefined, done: true });
                    }
                }, 30000);
            });
        }

        pipeThrough(transform, options) {
            const reader = this.getReader();
            const writer = transform.writable.getWriter();

            (async () => {
                try {
                    while (true) {
                        const { done, value } = await reader.read();
                        if (done) {
                            await writer.close();
                            break;
                        }
                        await writer.write(value);
                    }
                } catch (e) {
                    await writer.abort(e);
                } finally {
                    reader.releaseLock();
                    writer.releaseLock();
                }
            })();

            return transform.readable;
        }

        async pipeTo(dest, options = {}) {
            const reader = this.getReader();
            const writer = dest.getWriter();

            try {
                while (true) {
                    const { done, value } = await reader.read();
                    if (done) {
                        if (!options.preventClose) await writer.close();
                        break;
                    }
                    await writer.write(value);
                }
            } catch (e) {
                if (!options.preventAbort) await writer.abort(e);
                throw e;
            } finally {
                reader.releaseLock();
                writer.releaseLock();
            }
        }

        tee() {
            const reader = this.getReader();
            const queue1 = [];
            const queue2 = [];
            let closed = false;

            const pullFromSource = async () => {
                try {
                    const { done, value } = await reader.read();
                    if (done) {
                        closed = true;
                        return;
                    }
                    queue1.push(value);
                    queue2.push(value);
                } catch (e) {
                    closed = true;
                    throw e;
                }
            };

            const stream1 = new ReadableStream({
                async pull(controller) {
                    if (queue1.length > 0) {
                        controller.enqueue(queue1.shift());
                    } else if (closed) {
                        controller.close();
                    } else {
                        await pullFromSource();
                        if (queue1.length > 0) {
                            controller.enqueue(queue1.shift());
                        } else {
                            controller.close();
                        }
                    }
                }
            });

            const stream2 = new ReadableStream({
                async pull(controller) {
                    if (queue2.length > 0) {
                        controller.enqueue(queue2.shift());
                    } else if (closed) {
                        controller.close();
                    } else {
                        await pullFromSource();
                        if (queue2.length > 0) {
                            controller.enqueue(queue2.shift());
                        } else {
                            controller.close();
                        }
                    }
                }
            });

            return [stream1, stream2];
        }

        cancel(reason) {
            this._closed = true;
            if (this._source.cancel) {
                return Promise.resolve(this._source.cancel(reason));
            }
            return Promise.resolve();
        }

        get locked() {
            return this._locked;
        }

        [Symbol.asyncIterator]() {
            const reader = this.getReader();
            return {
                async next() {
                    return reader.read();
                },
                async return() {
                    reader.releaseLock();
                    return { done: true, value: undefined };
                }
            };
        }
    };
}

if (typeof WritableStream === 'undefined') {
    class WritableStreamDefaultWriter {
        constructor(stream) {
            this._stream = stream;
            this._closed = false;
        }
        write(chunk) {
            return this._stream._write(chunk);
        }
        close() {
            this._closed = true;
            return Promise.resolve();
        }
        abort(reason) {
            return Promise.resolve();
        }
        releaseLock() {}
        get closed() {
            return Promise.resolve(this._closed);
        }
        get ready() {
            return Promise.resolve();
        }
        get desiredSize() {
            return 1;
        }
    }

    globalThis.WritableStream = class WritableStream {
        constructor(underlyingSink = {}, strategy = {}) {
            this._sink = underlyingSink;
            this._closed = false;
            if (underlyingSink.start) {
                try { underlyingSink.start({ error: () => {} }); }
                catch (e) {}
            }
        }
        getWriter() {
            return new WritableStreamDefaultWriter(this);
        }
        _write(chunk) {
            if (this._sink.write) {
                return Promise.resolve(this._sink.write(chunk, { error: () => {} }));
            }
            return Promise.resolve();
        }
        close() {
            this._closed = true;
            if (this._sink.close) {
                return Promise.resolve(this._sink.close());
            }
            return Promise.resolve();
        }
        abort(reason) {
            return Promise.resolve();
        }
        get locked() {
            return false;
        }
    };
}

if (typeof TransformStream === 'undefined') {
    globalThis.TransformStream = class TransformStream {
        constructor(transformer = {}, writableStrategy = {}, readableStrategy = {}) {
            this._transformer = transformer;
            const queue = [];
            let closed = false;

            this.readable = new ReadableStream({
                pull(controller) {
                    while (queue.length > 0) {
                        controller.enqueue(queue.shift());
                    }
                    if (closed) controller.close();
                }
            });

            this.writable = new WritableStream({
                write(chunk) {
                    if (transformer.transform) {
                        transformer.transform(chunk, {
                            enqueue: (c) => queue.push(c)
                        });
                    } else {
                        queue.push(chunk);
                    }
                },
                close() {
                    closed = true;
                    if (transformer.flush) {
                        transformer.flush({ enqueue: (c) => queue.push(c) });
                    }
                }
            });
        }
    };
}

if (typeof ByteLengthQueuingStrategy === 'undefined') {
    globalThis.ByteLengthQueuingStrategy = class ByteLengthQueuingStrategy {
        constructor({ highWaterMark }) {
            this.highWaterMark = highWaterMark;
        }
        size(chunk) {
            return chunk.byteLength;
        }
    };
}

if (typeof CountQueuingStrategy === 'undefined') {
    globalThis.CountQueuingStrategy = class CountQueuingStrategy {
        constructor({ highWaterMark }) {
            this.highWaterMark = highWaterMark;
        }
        size() {
            return 1;
        }
    };
}
