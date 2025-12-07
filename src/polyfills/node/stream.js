// Node.js Stream Module - Adapted from Bun's node-fallbacks
// Removed Bun-specific syntax ($call, $toClass, $Buffer)

const EventEmitter = require("events");

const ReflectOwnKeys = Reflect.ownKeys;
const ArrayIsArray = Array.isArray;

// Stream base class - extends EventEmitter
function Stream(opts) {
  EventEmitter.call(this, opts);
}

// Set up inheritance
Stream.prototype = Object.create(EventEmitter.prototype);
Stream.prototype.constructor = Stream;

Stream.prototype.pipe = function (dest, options) {
  const source = this;

  function ondata(chunk) {
    if (dest.writable && dest.write(chunk) === false && source.pause) {
      source.pause();
    }
  }

  source.on("data", ondata);

  function ondrain() {
    if (source.readable && source.resume) {
      source.resume();
    }
  }

  dest.on("drain", ondrain);

  if (!dest._isStdio && (!options || options.end !== false)) {
    source.on("end", onend);
    source.on("close", onclose);
  }

  let didOnEnd = false;
  function onend() {
    if (didOnEnd) return;
    didOnEnd = true;
    dest.end();
  }

  function onclose() {
    if (didOnEnd) return;
    didOnEnd = true;
    if (typeof dest.destroy === "function") dest.destroy();
  }

  function onerror(er) {
    cleanup();
    if (EventEmitter.listenerCount(this, "error") === 0) {
      this.emit("error", er);
    }
  }

  prependListener(source, "error", onerror);
  prependListener(dest, "error", onerror);

  function cleanup() {
    source.removeListener("data", ondata);
    dest.removeListener("drain", ondrain);
    source.removeListener("end", onend);
    source.removeListener("close", onclose);
    source.removeListener("error", onerror);
    dest.removeListener("error", onerror);
    source.removeListener("end", cleanup);
    source.removeListener("close", cleanup);
    dest.removeListener("close", cleanup);
  }

  source.on("end", cleanup);
  source.on("close", cleanup);
  dest.on("close", cleanup);
  dest.emit("pipe", source);

  return dest;
};

Stream.prototype.eventNames = function eventNames() {
  const names = [];
  if (!this._events) return names;
  for (const key of ReflectOwnKeys(this._events)) {
    if (typeof this._events[key] === "function" || (ArrayIsArray(this._events[key]) && this._events[key].length > 0)) {
      names.push(key);
    }
  }
  return names;
};

function prependListener(emitter, event, fn) {
  if (typeof emitter.prependListener === "function") return emitter.prependListener(event, fn);
  if (!emitter._events || !emitter._events[event]) emitter.on(event, fn);
  else if (ArrayIsArray(emitter._events[event])) emitter._events[event].unshift(fn);
  else emitter._events[event] = [fn, emitter._events[event]];
}

// Readable stream
function Readable(opts) {
  if (!(this instanceof Readable)) return new Readable(opts);
  Stream.call(this, opts);
  this.readable = true;
  this._readableState = {
    flowing: null,
    ended: false,
    endEmitted: false,
    reading: false,
    buffer: [],
    length: 0,
    highWaterMark: (opts && opts.highWaterMark) || 16384,
  };
  if (opts) {
    if (typeof opts.read === "function") this._read = opts.read;
    if (typeof opts.destroy === "function") this._destroy = opts.destroy;
  }
}

Readable.prototype = Object.create(Stream.prototype);
Readable.prototype.constructor = Readable;

Readable.prototype.read = function(size) {
  const state = this._readableState;
  if (state.buffer.length === 0) return null;
  if (size === undefined || size >= state.length) {
    const result = Buffer.concat(state.buffer);
    state.buffer = [];
    state.length = 0;
    return result;
  }
  return state.buffer.shift();
};

Readable.prototype._read = function(size) {};

Readable.prototype.push = function(chunk) {
  const state = this._readableState;
  if (chunk === null) {
    state.ended = true;
    if (state.flowing !== false) {
      this.emit("end");
      state.endEmitted = true;
    }
    return false;
  }
  const buf = Buffer.isBuffer(chunk) ? chunk : Buffer.from(chunk);
  state.buffer.push(buf);
  state.length += buf.length;
  if (state.flowing) this.emit("data", buf);
  return state.length < state.highWaterMark;
};

Readable.prototype.pipe = Stream.prototype.pipe;
Readable.prototype.pause = function() { this._readableState.flowing = false; return this; };
Readable.prototype.resume = function() {
  const state = this._readableState;
  if (!state.flowing) {
    state.flowing = true;
    while (state.buffer.length && state.flowing) {
      this.emit("data", state.buffer.shift());
    }
    if (state.ended && !state.endEmitted) {
      this.emit("end");
      state.endEmitted = true;
    }
  }
  return this;
};
Readable.prototype.destroy = function(err) {
  if (err) this.emit("error", err);
  this.emit("close");
  return this;
};
Readable.prototype.setEncoding = function(enc) { this._encoding = enc; return this; };

Readable.from = function(iterable, opts) {
  const readable = new Readable(opts);
  (async () => {
    try {
      for await (const chunk of iterable) readable.push(chunk);
      readable.push(null);
    } catch (err) {
      readable.destroy(err);
    }
  })();
  return readable;
};

// Writable stream
function Writable(opts) {
  if (!(this instanceof Writable)) return new Writable(opts);
  Stream.call(this, opts);
  this.writable = true;
  this._writableState = {
    ended: false,
    finished: false,
    writing: false,
    corked: 0,
    buffer: [],
  };
  if (opts) {
    if (typeof opts.write === "function") this._write = opts.write;
    if (typeof opts.writev === "function") this._writev = opts.writev;
    if (typeof opts.final === "function") this._final = opts.final;
    if (typeof opts.destroy === "function") this._destroy = opts.destroy;
  }
}

Writable.prototype = Object.create(Stream.prototype);
Writable.prototype.constructor = Writable;

Writable.prototype._write = function(chunk, encoding, cb) { cb(); };

Writable.prototype.write = function(chunk, encoding, cb) {
  if (typeof encoding === "function") { cb = encoding; encoding = "utf8"; }
  const state = this._writableState;
  if (state.ended) {
    const err = new Error("write after end");
    if (cb) cb(err);
    this.emit("error", err);
    return false;
  }
  this._write(chunk, encoding, cb || (() => {}));
  this.emit("drain");
  return true;
};

Writable.prototype.end = function(chunk, encoding, cb) {
  if (typeof chunk === "function") { cb = chunk; chunk = null; encoding = null; }
  if (typeof encoding === "function") { cb = encoding; encoding = null; }
  const state = this._writableState;
  if (chunk !== null && chunk !== undefined) this.write(chunk, encoding);
  state.ended = true;
  const finish = () => {
    state.finished = true;
    this.emit("finish");
    if (cb) cb();
  };
  if (this._final) this._final(finish);
  else finish();
  return this;
};

Writable.prototype.cork = function() { this._writableState.corked++; };
Writable.prototype.uncork = function() {
  const state = this._writableState;
  if (state.corked > 0) state.corked--;
};
Writable.prototype.destroy = function(err) {
  if (err) this.emit("error", err);
  this.emit("close");
  return this;
};
Writable.prototype.setDefaultEncoding = function(enc) { this._defaultEncoding = enc; return this; };

// Duplex stream
function Duplex(opts) {
  if (!(this instanceof Duplex)) return new Duplex(opts);
  Readable.call(this, opts);
  Writable.call(this, opts);
  this.readable = true;
  this.writable = true;
  if (opts) {
    if (opts.readable === false) this.readable = false;
    if (opts.writable === false) this.writable = false;
  }
}

Duplex.prototype = Object.create(Readable.prototype);
Object.assign(Duplex.prototype, Writable.prototype);
Duplex.prototype.constructor = Duplex;

// Transform stream
function Transform(opts) {
  if (!(this instanceof Transform)) return new Transform(opts);
  Duplex.call(this, opts);
  this._transformState = { transforming: false, writecb: null, writechunk: null };
  if (opts && typeof opts.transform === "function") this._transform = opts.transform;
  if (opts && typeof opts.flush === "function") this._flush = opts.flush;
}

Transform.prototype = Object.create(Duplex.prototype);
Transform.prototype.constructor = Transform;

Transform.prototype._transform = function(chunk, encoding, cb) { cb(null, chunk); };

Transform.prototype._write = function(chunk, encoding, cb) {
  this._transform(chunk, encoding, (err, data) => {
    if (err) { cb(err); return; }
    if (data !== null && data !== undefined) this.push(data);
    cb();
  });
};

Transform.prototype._read = function(size) {};

// PassThrough stream
function PassThrough(opts) {
  if (!(this instanceof PassThrough)) return new PassThrough(opts);
  Transform.call(this, opts);
}

PassThrough.prototype = Object.create(Transform.prototype);
PassThrough.prototype.constructor = PassThrough;
PassThrough.prototype._transform = function(chunk, encoding, cb) { cb(null, chunk); };

// pipeline helper
function pipeline(...streams) {
  const callback = typeof streams[streams.length - 1] === "function" ? streams.pop() : null;
  if (streams.length < 2) {
    const err = new Error("pipeline requires at least 2 streams");
    if (callback) { callback(err); return; }
    throw err;
  }

  let error;
  const destroys = [];

  function destroyer(stream, reading, writing) {
    return (err) => {
      if (err) error = err;
      if (reading && typeof stream.destroy === "function") stream.destroy(err);
    };
  }

  let current = streams[0];
  for (let i = 1; i < streams.length; i++) {
    const next = streams[i];
    current.on("error", destroyer(current, true, false));
    current = current.pipe(next);
  }

  const last = streams[streams.length - 1];
  last.on("finish", () => { if (callback) callback(error); });
  last.on("error", (err) => { if (callback) callback(err); });

  return last;
}

// finished helper
function finished(stream, opts, callback) {
  if (typeof opts === "function") { callback = opts; opts = {}; }
  opts = opts || {};

  const onfinish = () => { cleanup(); callback(); };
  const onend = () => { cleanup(); callback(); };
  const onerror = (err) => { cleanup(); callback(err); };
  const onclose = () => { cleanup(); callback(); };

  function cleanup() {
    stream.removeListener("finish", onfinish);
    stream.removeListener("end", onend);
    stream.removeListener("error", onerror);
    stream.removeListener("close", onclose);
  }

  if (stream.writable) stream.on("finish", onfinish);
  if (stream.readable) stream.on("end", onend);
  stream.on("error", onerror);
  stream.on("close", onclose);

  return cleanup;
}

// Exports
Stream.Stream = Stream;
Stream.Readable = Readable;
Stream.Writable = Writable;
Stream.Duplex = Duplex;
Stream.Transform = Transform;
Stream.PassThrough = PassThrough;
Stream.pipeline = pipeline;
Stream.finished = finished;

module.exports = Stream;
module.exports.Stream = Stream;
module.exports.Readable = Readable;
module.exports.Writable = Writable;
module.exports.Duplex = Duplex;
module.exports.Transform = Transform;
module.exports.PassThrough = PassThrough;
module.exports.pipeline = pipeline;
module.exports.finished = finished;
