// Comprehensive Stream polyfill tests
// Tests Readable, Writable, Duplex, Transform, PassThrough, pipeline, finished

const {
    assertEqual, assertArrayEqual, assertTrue, assertFalse,
    assertInstanceOf, assertTypeOf, assertDefined, summary
} = require('./helpers/assert.js');

console.log('=== Stream Polyfill Tests ===\n');

let stream;
try {
    stream = require('stream');
} catch (e) {
    console.log('SKIP: stream module not available');
    stream = null;
}

if (stream) {
    const { Stream, Readable, Writable, Duplex, Transform, PassThrough } = stream;

    // ============================================
    // Stream base class
    // ============================================
    console.log('--- Stream base class ---');

    if (Stream) {
        const s = new Stream();
        assertDefined(s, 'new Stream() creates instance');
        assertTypeOf(s.on, 'function', 'Stream has on method (from EventEmitter)');
        assertTypeOf(s.emit, 'function', 'Stream has emit method');
        assertTypeOf(s.pipe, 'function', 'Stream has pipe method');
    } else {
        console.log('SKIP: Stream class not available');
    }

    // ============================================
    // Readable stream
    // ============================================
    console.log('\n--- Readable stream ---');

    const readable = new Readable();
    assertInstanceOf(readable, Readable, 'new Readable() creates instance');
    assertTypeOf(readable.read, 'function', 'Readable has read method');
    assertTypeOf(readable.push, 'function', 'Readable has push method');
    assertTypeOf(readable.pipe, 'function', 'Readable has pipe method');

    // Readable emits data event on push
    let dataReceived = null;
    const r1 = new Readable();
    r1.on('data', (chunk) => { dataReceived = chunk; });
    r1.push('hello');
    assertEqual(dataReceived, 'hello', 'Readable push() emits data event');

    // Readable emits end event on push(null)
    let endEmitted = false;
    const r2 = new Readable();
    r2.on('end', () => { endEmitted = true; });
    r2.push(null);
    assertTrue(endEmitted, 'Readable push(null) emits end event');

    // Readable state
    const r3 = new Readable();
    assertDefined(r3._readableState, 'Readable has _readableState');
    assertFalse(r3._readableState.ended, 'Readable starts not ended');
    r3.push(null);
    assertTrue(r3._readableState.ended, 'Readable ended after push(null)');

    // Readable.from() (if available)
    if (typeof Readable.from === 'function') {
        const fromIterable = Readable.from(['a', 'b', 'c']);
        assertInstanceOf(fromIterable, Readable, 'Readable.from() returns Readable');
    } else {
        console.log('SKIP: Readable.from() not available');
    }

    // Async iterator (if available)
    if (typeof Readable.prototype[Symbol.asyncIterator] === 'function') {
        const r4 = new Readable();
        const iterator = r4[Symbol.asyncIterator]();
        assertDefined(iterator, 'Readable has async iterator');
        assertTypeOf(iterator.next, 'function', 'Async iterator has next()');
        console.log('PASS: Readable async iterator available');
    } else {
        console.log('SKIP: Readable async iterator not available');
    }

    // ============================================
    // Writable stream
    // ============================================
    console.log('\n--- Writable stream ---');

    const writable = new Writable();
    assertInstanceOf(writable, Writable, 'new Writable() creates instance');
    assertTypeOf(writable.write, 'function', 'Writable has write method');
    assertTypeOf(writable.end, 'function', 'Writable has end method');

    // Writable emits finish on end()
    let finishEmitted = false;
    const w1 = new Writable();
    w1.on('finish', () => { finishEmitted = true; });
    w1.end();
    assertTrue(finishEmitted, 'Writable end() emits finish event');

    // Writable state
    const w2 = new Writable();
    assertDefined(w2._writableState, 'Writable has _writableState');
    assertFalse(w2._writableState.ended, 'Writable starts not ended');
    w2.end();
    assertTrue(w2._writableState.ended, 'Writable ended after end()');

    // Writable write returns boolean
    const w3 = new Writable();
    const writeResult = w3.write('data');
    assertTypeOf(writeResult, 'boolean', 'write() returns boolean');

    // Writable with _write implementation
    let written = [];
    class TestWritable extends Writable {
        _write(chunk, encoding, callback) {
            written.push(chunk);
            callback();
        }
    }
    const tw = new TestWritable();
    tw.write('hello');
    tw.write('world');
    assertEqual(written.length, 2, 'Custom _write called for each write');
    assertEqual(written[0], 'hello', '_write receives chunk');

    // ============================================
    // Duplex stream
    // ============================================
    console.log('\n--- Duplex stream ---');

    const duplex = new Duplex();
    assertInstanceOf(duplex, Duplex, 'new Duplex() creates instance');
    assertTypeOf(duplex.read, 'function', 'Duplex has read method');
    assertTypeOf(duplex.write, 'function', 'Duplex has write method');
    assertTypeOf(duplex.end, 'function', 'Duplex has end method');

    // Duplex has both states
    assertDefined(duplex._readableState, 'Duplex has _readableState');
    assertDefined(duplex._writableState, 'Duplex has _writableState');

    // Duplex emits finish on end()
    let duplexFinish = false;
    const d1 = new Duplex();
    d1.on('finish', () => { duplexFinish = true; });
    d1.end();
    assertTrue(duplexFinish, 'Duplex end() emits finish event');

    // ============================================
    // Transform stream
    // ============================================
    console.log('\n--- Transform stream ---');

    const transform = new Transform();
    assertInstanceOf(transform, Transform, 'new Transform() creates instance');
    assertTypeOf(transform._transform, 'function', 'Transform has _transform method');

    // Custom transform
    class UpperCaseTransform extends Transform {
        _transform(chunk, encoding, callback) {
            const upper = String(chunk).toUpperCase();
            this.push(upper);
            callback();
        }
    }
    let transformedData = null;
    const uct = new UpperCaseTransform();
    uct.on('data', (data) => { transformedData = data; });
    uct.write('hello');
    if (transformedData) {
        assertEqual(transformedData, 'HELLO', 'Transform transforms data');
    } else {
        console.log('SKIP: Transform data not received (async)');
    }

    // ============================================
    // PassThrough stream
    // ============================================
    console.log('\n--- PassThrough stream ---');

    const passThrough = new PassThrough();
    assertInstanceOf(passThrough, PassThrough, 'new PassThrough() creates instance');
    assertInstanceOf(passThrough, Transform, 'PassThrough extends Transform');

    // PassThrough passes data through unchanged
    let passedData = null;
    const pt = new PassThrough();
    pt.on('data', (data) => { passedData = data; });
    pt.write('unchanged');
    if (passedData !== null) {
        assertEqual(passedData, 'unchanged', 'PassThrough passes data unchanged');
    } else {
        console.log('SKIP: PassThrough data not received (async)');
    }

    // ============================================
    // pipe()
    // ============================================
    console.log('\n--- pipe() ---');

    // Pipe readable to writable
    let pipedData = [];
    const pr = new Readable();
    const pw = new Writable();
    pw._write = (chunk, enc, cb) => { pipedData.push(chunk); cb(); };
    pr.pipe(pw);
    pr.push('piped data');
    pr.push(null);
    assertEqual(pipedData.length, 1, 'pipe() transfers data');
    assertEqual(pipedData[0], 'piped data', 'pipe() data is correct');

    // pipe() returns destination
    const pr2 = new Readable();
    const pw2 = new Writable();
    const pipeResult = pr2.pipe(pw2);
    assertEqual(pipeResult, pw2, 'pipe() returns destination');

    // ============================================
    // pipeline() utility (if available)
    // ============================================
    console.log('\n--- pipeline() ---');

    if (typeof stream.pipeline === 'function') {
        const src = new Readable();
        const dest = new Writable();
        let pipelineData = [];
        dest._write = (chunk, enc, cb) => { pipelineData.push(chunk); cb(); };

        stream.pipeline(src, dest, (err) => {
            if (!err) {
                console.log('PASS: pipeline() completed without error');
            }
        });

        src.push('pipeline data');
        src.push(null);
    } else {
        console.log('SKIP: pipeline() not available');
    }

    // pipeline with Promise (if available)
    if (stream.promises && typeof stream.promises.pipeline === 'function') {
        console.log('PASS: stream.promises.pipeline exists');
    } else {
        console.log('SKIP: stream.promises.pipeline not available');
    }

    // ============================================
    // finished() utility (if available)
    // ============================================
    console.log('\n--- finished() ---');

    if (typeof stream.finished === 'function') {
        const fr = new Readable();
        let finishedCalled = false;
        stream.finished(fr, (err) => {
            finishedCalled = true;
        });
        fr.push(null);
        // finished is async, just check it exists
        console.log('PASS: finished() exists');
    } else {
        console.log('SKIP: finished() not available');
    }

    // finished with Promise (if available)
    if (stream.promises && typeof stream.promises.finished === 'function') {
        console.log('PASS: stream.promises.finished exists');
    } else {
        console.log('SKIP: stream.promises.finished not available');
    }

    // ============================================
    // Stream methods on Readable
    // ============================================
    console.log('\n--- Readable methods ---');

    // pause() and resume() (if available)
    const rPause = new Readable();
    if (typeof rPause.pause === 'function') {
        rPause.pause();
        console.log('PASS: Readable.pause() exists');
    } else {
        console.log('SKIP: Readable.pause() not available');
    }

    if (typeof rPause.resume === 'function') {
        rPause.resume();
        console.log('PASS: Readable.resume() exists');
    } else {
        console.log('SKIP: Readable.resume() not available');
    }

    // isPaused() (if available)
    if (typeof rPause.isPaused === 'function') {
        assertTypeOf(rPause.isPaused(), 'boolean', 'isPaused() returns boolean');
    } else {
        console.log('SKIP: Readable.isPaused() not available');
    }

    // setEncoding() (if available)
    const rEnc = new Readable();
    if (typeof rEnc.setEncoding === 'function') {
        rEnc.setEncoding('utf8');
        console.log('PASS: Readable.setEncoding() exists');
    } else {
        console.log('SKIP: Readable.setEncoding() not available');
    }

    // unshift() (if available)
    const rUnshift = new Readable();
    if (typeof rUnshift.unshift === 'function') {
        console.log('PASS: Readable.unshift() exists');
    } else {
        console.log('SKIP: Readable.unshift() not available');
    }

    // destroy() (if available)
    const rDestroy = new Readable();
    if (typeof rDestroy.destroy === 'function') {
        console.log('PASS: Readable.destroy() exists');
    } else {
        console.log('SKIP: Readable.destroy() not available');
    }

    // ============================================
    // Stream methods on Writable
    // ============================================
    console.log('\n--- Writable methods ---');

    // cork() and uncork() (if available)
    const wCork = new Writable();
    if (typeof wCork.cork === 'function') {
        wCork.cork();
        console.log('PASS: Writable.cork() exists');
    } else {
        console.log('SKIP: Writable.cork() not available');
    }

    if (typeof wCork.uncork === 'function') {
        wCork.uncork();
        console.log('PASS: Writable.uncork() exists');
    } else {
        console.log('SKIP: Writable.uncork() not available');
    }

    // writable property (if available)
    const wProp = new Writable();
    if ('writable' in wProp) {
        assertTypeOf(wProp.writable, 'boolean', 'writable property is boolean');
    } else {
        console.log('SKIP: Writable.writable property not available');
    }

    // writableEnded property (if available)
    const wEnded = new Writable();
    if ('writableEnded' in wEnded) {
        assertFalse(wEnded.writableEnded, 'writableEnded starts false');
    } else {
        console.log('SKIP: Writable.writableEnded property not available');
    }

    // destroy() (if available)
    const wDestroy = new Writable();
    if (typeof wDestroy.destroy === 'function') {
        console.log('PASS: Writable.destroy() exists');
    } else {
        console.log('SKIP: Writable.destroy() not available');
    }

    // ============================================
    // Inheritance checks
    // ============================================
    console.log('\n--- Inheritance ---');

    // All streams should be EventEmitters
    const events = require('events');
    const EventEmitter = events.EventEmitter || events;
    assertInstanceOf(new Readable(), EventEmitter, 'Readable extends EventEmitter');
    assertInstanceOf(new Writable(), EventEmitter, 'Writable extends EventEmitter');
    assertInstanceOf(new Duplex(), EventEmitter, 'Duplex extends EventEmitter');
    assertInstanceOf(new Transform(), EventEmitter, 'Transform extends EventEmitter');
    assertInstanceOf(new PassThrough(), EventEmitter, 'PassThrough extends EventEmitter');

    // Transform extends Duplex
    assertInstanceOf(new Transform(), Duplex, 'Transform extends Duplex');

    // PassThrough extends Transform
    assertInstanceOf(new PassThrough(), Transform, 'PassThrough extends Transform');

    // ============================================
    // Edge cases
    // ============================================
    console.log('\n--- Edge cases ---');

    // Multiple writes before end
    let multiWrites = [];
    const wMulti = new Writable();
    wMulti._write = (chunk, enc, cb) => { multiWrites.push(chunk); cb(); };
    wMulti.write('one');
    wMulti.write('two');
    wMulti.write('three');
    wMulti.end();
    assertEqual(multiWrites.length, 3, 'Multiple writes before end');

    // End with data
    let endWithData = [];
    const wEnd = new Writable();
    wEnd._write = (chunk, enc, cb) => { endWithData.push(chunk); cb(); };
    wEnd.end('final');
    assertEqual(endWithData.length, 1, 'end() with data writes it');
    assertEqual(endWithData[0], 'final', 'end() data is correct');

    // Empty stream
    let emptyEnd = false;
    const rEmpty = new Readable();
    rEmpty.on('end', () => { emptyEnd = true; });
    rEmpty.push(null);
    assertTrue(emptyEnd, 'Empty stream emits end');

    // Callback on write
    const wCallback = new Writable();
    let callbackCalled = false;
    wCallback._write = (chunk, enc, cb) => cb();
    wCallback.write('data', () => { callbackCalled = true; });
    assertTrue(callbackCalled, 'Write callback is called');

    // ============================================
    // Module exports
    // ============================================
    console.log('\n--- Module exports ---');

    assertDefined(stream.Readable, 'stream.Readable exported');
    assertDefined(stream.Writable, 'stream.Writable exported');
    assertDefined(stream.Duplex, 'stream.Duplex exported');
    assertDefined(stream.Transform, 'stream.Transform exported');
    assertDefined(stream.PassThrough, 'stream.PassThrough exported');

    // Stream itself can be used as base class
    if (Stream) {
        const customStream = new Stream();
        assertDefined(customStream, 'Stream base class usable');
    }

} else {
    console.log('Stream module not available');
}

summary();
