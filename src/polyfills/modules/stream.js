    // ===== STREAM MODULE =====
    class Stream extends EventEmitter {
        pipe(dest) { this.on('data', chunk => dest.write(chunk)); this.on('end', () => dest.end()); return dest; }
    }
    class Readable extends Stream {
        constructor() { super(); this._readableState = { ended: false, buffer: [] }; }
        read() { return null; }
        push(chunk) {
            if (chunk === null) { this._readableState.ended = true; this.emit('end'); }
            else this.emit('data', chunk);
            return true;
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
        constructor() { super(); this._writableState = { ended: false }; }
        write(chunk, encoding, callback) {
            if (typeof encoding === 'function') callback = encoding;
            this._write(chunk, 'utf8', callback || (() => {}));
            return true;
        }
        _write(chunk, encoding, callback) { callback(); }
        end(chunk) { if (chunk) this.write(chunk); this._writableState.ended = true; this.emit('finish'); }
    }
    class Duplex extends Stream {
        constructor() { super(); this._readableState = { ended: false }; this._writableState = { ended: false }; }
        read() { return null; }
        write(chunk) { return true; }
        end() { this.emit('finish'); }
    }
    class Transform extends Duplex { _transform(chunk, encoding, callback) { callback(null, chunk); } }
    class PassThrough extends Transform {}
    // Stream module - export Stream class as default (like events exports EventEmitter)
    // Some code does `require("stream")` and uses it directly as base class
    _modules.stream = Stream;
    _modules.stream.Stream = Stream;
    _modules.stream.Readable = Readable;
    _modules.stream.Writable = Writable;
    _modules.stream.Duplex = Duplex;
    _modules.stream.Transform = Transform;
    _modules.stream.PassThrough = PassThrough;
    _modules['node:stream'] = _modules.stream;

