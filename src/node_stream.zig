/// Node.js Stream Module - Native Zig Implementation
/// Provides Stream, Readable, Writable, Duplex, Transform, PassThrough
const std = @import("std");
const qjs = @import("quickjs_core.zig");

const c = qjs.c;

/// Register the stream module in QuickJS context
pub fn register(ctx: *qjs.Context) void {
    const stream_module = c.JS_NewObject(ctx.inner);

    // Create Stream base class (extends EventEmitter)
    const stream_class = createStreamClass(ctx);
    _ = c.JS_SetPropertyStr(ctx.inner, stream_module, "Stream", stream_class);

    // Create Readable class
    const readable_class = createReadableClass(ctx);
    _ = c.JS_SetPropertyStr(ctx.inner, stream_module, "Readable", readable_class);

    // Create Writable class
    const writable_class = createWritableClass(ctx);
    _ = c.JS_SetPropertyStr(ctx.inner, stream_module, "Writable", writable_class);

    // Create Duplex class
    const duplex_class = createDuplexClass(ctx);
    _ = c.JS_SetPropertyStr(ctx.inner, stream_module, "Duplex", duplex_class);

    // Create Transform class
    const transform_class = createTransformClass(ctx);
    _ = c.JS_SetPropertyStr(ctx.inner, stream_module, "Transform", transform_class);

    // Create PassThrough class
    const passthrough_class = createPassThroughClass(ctx);
    _ = c.JS_SetPropertyStr(ctx.inner, stream_module, "PassThrough", passthrough_class);

    // Add pipeline function
    const pipeline_fn = c.JS_NewCFunction(ctx.inner, pipelineImpl, "pipeline", 0);
    _ = c.JS_SetPropertyStr(ctx.inner, stream_module, "pipeline", pipeline_fn);

    // Add finished function
    const finished_fn = c.JS_NewCFunction(ctx.inner, finishedImpl, "finished", 2);
    _ = c.JS_SetPropertyStr(ctx.inner, stream_module, "finished", finished_fn);

    // Register as global module
    const global = c.JS_GetGlobalObject(ctx.inner);
    defer c.JS_FreeValue(ctx.inner, global);

    // Get or create _modules object
    var modules = c.JS_GetPropertyStr(ctx.inner, global, "_modules");
    if (c.JS_IsUndefined(modules)) {
        modules = c.JS_NewObject(ctx.inner);
        _ = c.JS_SetPropertyStr(ctx.inner, global, "_modules", modules);
    }

    _ = c.JS_SetPropertyStr(ctx.inner, modules, "stream", c.JS_DupValue(ctx.inner, stream_module));
    _ = c.JS_SetPropertyStr(ctx.inner, modules, "node:stream", c.JS_DupValue(ctx.inner, stream_module));

    c.JS_FreeValue(ctx.inner, stream_module);
}

fn createStreamClass(ctx: *qjs.Context) c.JSValue {
    // Stream is EventEmitter with pipe method
    // We'll create it using JS since class syntax is cleaner
    const stream_js =
        \\(function() {
        \\  class Stream extends EventEmitter {
        \\    constructor(opts) {
        \\      super();
        \\      this.readable = false;
        \\      this.writable = false;
        \\      if (opts) Object.assign(this, opts);
        \\    }
        \\    pipe(dest, options) {
        \\      const source = this;
        \\      function ondata(chunk) {
        \\        if (dest.writable && dest.write(chunk) === false && source.pause) source.pause();
        \\      }
        \\      source.on('data', ondata);
        \\      dest.on('drain', function() { if (source.readable && source.resume) source.resume(); });
        \\      if (!dest._isStdio && (!options || options.end !== false)) {
        \\        source.on('end', function() { dest.end(); });
        \\        source.on('close', function() { if (typeof dest.destroy === 'function') dest.destroy(); });
        \\      }
        \\      dest.emit('pipe', source);
        \\      return dest;
        \\    }
        \\  }
        \\  return Stream;
        \\})()
    ;

    const result = c.JS_Eval(ctx.inner, stream_js.ptr, stream_js.len, "<stream>", c.JS_EVAL_TYPE_GLOBAL);
    return result;
}

fn createReadableClass(ctx: *qjs.Context) c.JSValue {
    const readable_js =
        \\(function() {
        \\  class Readable extends EventEmitter {
        \\    constructor(opts) {
        \\      super();
        \\      this.readable = true;
        \\      this._readableState = { flowing: null, ended: false, reading: false };
        \\      if (opts) {
        \\        if (typeof opts.read === 'function') this._read = opts.read;
        \\        if (typeof opts.destroy === 'function') this._destroy = opts.destroy;
        \\      }
        \\    }
        \\    read(size) { return null; }
        \\    _read(size) {}
        \\    push(chunk) {
        \\      if (chunk === null) { this._readableState.ended = true; this.emit('end'); return false; }
        \\      this.emit('data', chunk);
        \\      return true;
        \\    }
        \\    pipe(dest, opts) { return Stream.prototype.pipe.call(this, dest, opts); }
        \\    pause() { this._readableState.flowing = false; return this; }
        \\    resume() { this._readableState.flowing = true; return this; }
        \\    destroy(err) { if (err) this.emit('error', err); this.emit('close'); return this; }
        \\    static from(iterable, opts) {
        \\      const r = new Readable(opts);
        \\      (async () => { for await (const chunk of iterable) r.push(chunk); r.push(null); })();
        \\      return r;
        \\    }
        \\  }
        \\  return Readable;
        \\})()
    ;

    const result = c.JS_Eval(ctx.inner, readable_js.ptr, readable_js.len, "<stream>", c.JS_EVAL_TYPE_GLOBAL);
    return result;
}

fn createWritableClass(ctx: *qjs.Context) c.JSValue {
    const writable_js =
        \\(function() {
        \\  class Writable extends EventEmitter {
        \\    constructor(opts) {
        \\      super();
        \\      this.writable = true;
        \\      this._writableState = { ended: false, finished: false };
        \\      if (opts) {
        \\        if (typeof opts.write === 'function') this._write = opts.write;
        \\        if (typeof opts.final === 'function') this._final = opts.final;
        \\        if (typeof opts.destroy === 'function') this._destroy = opts.destroy;
        \\      }
        \\    }
        \\    write(chunk, encoding, cb) {
        \\      if (typeof encoding === 'function') { cb = encoding; encoding = 'utf8'; }
        \\      if (this._write) this._write(chunk, encoding, cb || (() => {}));
        \\      else if (cb) cb();
        \\      return true;
        \\    }
        \\    end(chunk, encoding, cb) {
        \\      if (typeof chunk === 'function') { cb = chunk; chunk = null; }
        \\      if (typeof encoding === 'function') { cb = encoding; encoding = 'utf8'; }
        \\      if (chunk) this.write(chunk, encoding);
        \\      this._writableState.ended = true;
        \\      if (this._final) this._final(() => { this._writableState.finished = true; this.emit('finish'); if (cb) cb(); });
        \\      else { this._writableState.finished = true; this.emit('finish'); if (cb) cb(); }
        \\      return this;
        \\    }
        \\    destroy(err) { if (err) this.emit('error', err); this.emit('close'); return this; }
        \\  }
        \\  return Writable;
        \\})()
    ;

    const result = c.JS_Eval(ctx.inner, writable_js.ptr, writable_js.len, "<stream>", c.JS_EVAL_TYPE_GLOBAL);
    return result;
}

fn createDuplexClass(ctx: *qjs.Context) c.JSValue {
    const duplex_js =
        \\(function() {
        \\  class Duplex extends EventEmitter {
        \\    constructor(opts) {
        \\      super();
        \\      this.readable = true;
        \\      this.writable = true;
        \\      this._readableState = { flowing: null, ended: false };
        \\      this._writableState = { ended: false, finished: false };
        \\      if (opts) {
        \\        if (typeof opts.read === 'function') this._read = opts.read;
        \\        if (typeof opts.write === 'function') this._write = opts.write;
        \\      }
        \\    }
        \\    read(size) { return null; }
        \\    _read(size) {}
        \\    push(chunk) {
        \\      if (chunk === null) { this._readableState.ended = true; this.emit('end'); return false; }
        \\      this.emit('data', chunk);
        \\      return true;
        \\    }
        \\    write(chunk, encoding, cb) {
        \\      if (typeof encoding === 'function') { cb = encoding; encoding = 'utf8'; }
        \\      if (this._write) this._write(chunk, encoding, cb || (() => {}));
        \\      return true;
        \\    }
        \\    end(chunk, encoding, cb) {
        \\      if (chunk) this.write(chunk, encoding);
        \\      this._writableState.ended = true;
        \\      this.emit('finish');
        \\      if (cb) cb();
        \\      return this;
        \\    }
        \\    pipe(dest, opts) { return Stream.prototype.pipe.call(this, dest, opts); }
        \\    destroy(err) { if (err) this.emit('error', err); this.emit('close'); return this; }
        \\  }
        \\  return Duplex;
        \\})()
    ;

    const result = c.JS_Eval(ctx.inner, duplex_js.ptr, duplex_js.len, "<stream>", c.JS_EVAL_TYPE_GLOBAL);
    return result;
}

fn createTransformClass(ctx: *qjs.Context) c.JSValue {
    const transform_js =
        \\(function() {
        \\  class Transform extends EventEmitter {
        \\    constructor(opts) {
        \\      super();
        \\      this.readable = true;
        \\      this.writable = true;
        \\      this._readableState = { flowing: null, ended: false };
        \\      this._writableState = { ended: false, finished: false };
        \\      if (opts && typeof opts.transform === 'function') this._transform = opts.transform;
        \\    }
        \\    _transform(chunk, encoding, cb) { cb(null, chunk); }
        \\    push(chunk) {
        \\      if (chunk === null) { this._readableState.ended = true; this.emit('end'); return false; }
        \\      this.emit('data', chunk);
        \\      return true;
        \\    }
        \\    write(chunk, encoding, cb) {
        \\      if (typeof encoding === 'function') { cb = encoding; encoding = 'utf8'; }
        \\      this._transform(chunk, encoding, (err, data) => {
        \\        if (err) this.emit('error', err);
        \\        else if (data) this.push(data);
        \\        if (cb) cb(err);
        \\      });
        \\      return true;
        \\    }
        \\    end(chunk, encoding, cb) {
        \\      if (chunk) this.write(chunk, encoding);
        \\      this._writableState.ended = true;
        \\      this.push(null);
        \\      this.emit('finish');
        \\      if (cb) cb();
        \\      return this;
        \\    }
        \\    pipe(dest, opts) { return Stream.prototype.pipe.call(this, dest, opts); }
        \\    destroy(err) { if (err) this.emit('error', err); this.emit('close'); return this; }
        \\  }
        \\  return Transform;
        \\})()
    ;

    const result = c.JS_Eval(ctx.inner, transform_js.ptr, transform_js.len, "<stream>", c.JS_EVAL_TYPE_GLOBAL);
    return result;
}

fn createPassThroughClass(ctx: *qjs.Context) c.JSValue {
    const passthrough_js =
        \\(function() {
        \\  class PassThrough extends Transform {
        \\    constructor(opts) { super(opts); }
        \\    _transform(chunk, encoding, cb) { cb(null, chunk); }
        \\  }
        \\  return PassThrough;
        \\})()
    ;

    const result = c.JS_Eval(ctx.inner, passthrough_js.ptr, passthrough_js.len, "<stream>", c.JS_EVAL_TYPE_GLOBAL);
    return result;
}

fn pipelineImpl(ctx: ?*c.JSContext, _: c.JSValue, argc: c_int, argv: [*c]c.JSValue) callconv(.c) c.JSValue {
    // pipeline(source, ...transforms, dest, callback)
    // Simplified implementation
    if (argc < 2) return c.JS_ThrowTypeError(ctx, "pipeline requires at least 2 arguments");

    // Just pipe them together sequentially
    const pipeline_js =
        \\(function(streams, callback) {
        \\  if (streams.length < 2) { if (callback) callback(new Error('pipeline requires at least 2 streams')); return; }
        \\  let current = streams[0];
        \\  for (let i = 1; i < streams.length; i++) {
        \\    current = current.pipe(streams[i]);
        \\    current.on('error', (err) => { if (callback) callback(err); });
        \\  }
        \\  current.on('finish', () => { if (callback) callback(); });
        \\  return current;
        \\})
    ;

    const fn_val = c.JS_Eval(ctx, pipeline_js.ptr, pipeline_js.len, "<pipeline>", c.JS_EVAL_TYPE_GLOBAL);
    defer c.JS_FreeValue(ctx, fn_val);

    // Build array of streams from arguments (except last which might be callback)
    const arr = c.JS_NewArray(ctx);
    var cb = c.JS_UNDEFINED;

    var i: c_int = 0;
    while (i < argc) : (i += 1) {
        if (i == argc - 1 and c.JS_IsFunction(ctx, argv[@intCast(i)])) {
            cb = argv[@intCast(i)];
        } else {
            _ = c.JS_SetPropertyUint32(ctx, arr, @intCast(i), c.JS_DupValue(ctx, argv[@intCast(i)]));
        }
    }

    var call_args = [_]c.JSValue{ arr, cb };
    const result = c.JS_Call(ctx, fn_val, c.JS_UNDEFINED, 2, &call_args);
    c.JS_FreeValue(ctx, arr);

    return result;
}

fn finishedImpl(ctx: ?*c.JSContext, _: c.JSValue, argc: c_int, argv: [*c]c.JSValue) callconv(.c) c.JSValue {
    // finished(stream, callback) - calls callback when stream ends
    if (argc < 2) return c.JS_ThrowTypeError(ctx, "finished requires stream and callback");

    const finished_js =
        \\(function(stream, callback) {
        \\  const onend = () => { cleanup(); callback(); };
        \\  const onfinish = () => { cleanup(); callback(); };
        \\  const onerror = (err) => { cleanup(); callback(err); };
        \\  const onclose = () => { cleanup(); callback(); };
        \\  function cleanup() {
        \\    stream.removeListener('end', onend);
        \\    stream.removeListener('finish', onfinish);
        \\    stream.removeListener('error', onerror);
        \\    stream.removeListener('close', onclose);
        \\  }
        \\  stream.on('end', onend);
        \\  stream.on('finish', onfinish);
        \\  stream.on('error', onerror);
        \\  stream.on('close', onclose);
        \\  return cleanup;
        \\})
    ;

    const fn_val = c.JS_Eval(ctx, finished_js.ptr, finished_js.len, "<finished>", c.JS_EVAL_TYPE_GLOBAL);
    defer c.JS_FreeValue(ctx, fn_val);

    var call_args = [_]c.JSValue{ argv[0], argv[1] };
    return c.JS_Call(ctx, fn_val, c.JS_UNDEFINED, 2, &call_args);
}
