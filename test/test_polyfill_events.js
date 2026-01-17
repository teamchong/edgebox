// Comprehensive EventEmitter polyfill tests
// Tests all EventEmitter methods with edge cases

const {
    assertEqual, assertArrayEqual, assertTrue, assertFalse,
    assertInstanceOf, assertTypeOf, assertGreater, summary
} = require('./helpers/assert.js');

console.log('=== EventEmitter Polyfill Tests ===\n');

let events;
try {
    events = require('events');
} catch (e) {
    console.log('SKIP: events module not available');
    events = null;
}

if (events) {
    const EventEmitter = events.EventEmitter || events;

    // ============================================
    // Basic EventEmitter instance
    // ============================================
    console.log('--- Constructor ---');

    const emitter = new EventEmitter();
    assertInstanceOf(emitter, EventEmitter, 'new EventEmitter() creates instance');
    assertTypeOf(emitter.on, 'function', 'emitter has on method');
    assertTypeOf(emitter.emit, 'function', 'emitter has emit method');
    assertTypeOf(emitter.off, 'function', 'emitter has off method');

    // ============================================
    // on() and emit()
    // ============================================
    console.log('\n--- on() and emit() ---');

    const em1 = new EventEmitter();
    let called = false;
    em1.on('test', () => { called = true; });
    em1.emit('test');
    assertTrue(called, 'on() registers listener, emit() calls it');

    // Multiple arguments
    const em2 = new EventEmitter();
    let args = [];
    em2.on('data', (a, b, c) => { args = [a, b, c]; });
    em2.emit('data', 1, 2, 3);
    assertArrayEqual(args, [1, 2, 3], 'emit() passes multiple arguments');

    // Multiple listeners
    const em3 = new EventEmitter();
    let count = 0;
    em3.on('inc', () => { count++; });
    em3.on('inc', () => { count++; });
    em3.on('inc', () => { count++; });
    em3.emit('inc');
    assertEqual(count, 3, 'Multiple listeners all called');

    // Emit returns true when listeners exist
    const em4 = new EventEmitter();
    em4.on('exists', () => {});
    assertTrue(em4.emit('exists'), 'emit() returns true when listeners exist');
    assertFalse(em4.emit('notexists'), 'emit() returns false when no listeners');

    // ============================================
    // addListener() - alias for on()
    // ============================================
    console.log('\n--- addListener() ---');

    const em5 = new EventEmitter();
    let addListenerCalled = false;
    em5.addListener('test', () => { addListenerCalled = true; });
    em5.emit('test');
    assertTrue(addListenerCalled, 'addListener() works like on()');

    // ============================================
    // once()
    // ============================================
    console.log('\n--- once() ---');

    const em6 = new EventEmitter();
    let onceCount = 0;
    em6.once('oneshot', () => { onceCount++; });
    em6.emit('oneshot');
    em6.emit('oneshot');
    em6.emit('oneshot');
    assertEqual(onceCount, 1, 'once() listener called only once');

    // once with arguments
    const em7 = new EventEmitter();
    let onceArgs = null;
    em7.once('data', (x) => { onceArgs = x; });
    em7.emit('data', 42);
    assertEqual(onceArgs, 42, 'once() passes arguments correctly');

    // ============================================
    // off() and removeListener()
    // ============================================
    console.log('\n--- off() and removeListener() ---');

    const em8 = new EventEmitter();
    let removedCount = 0;
    const listener = () => { removedCount++; };
    em8.on('test', listener);
    em8.emit('test');
    em8.off('test', listener);
    em8.emit('test');
    assertEqual(removedCount, 1, 'off() removes listener');

    // removeListener() same as off()
    const em9 = new EventEmitter();
    let removed2 = 0;
    const listener2 = () => { removed2++; };
    em9.on('test', listener2);
    em9.emit('test');
    em9.removeListener('test', listener2);
    em9.emit('test');
    assertEqual(removed2, 1, 'removeListener() removes listener');

    // Remove from multiple listeners
    const em10 = new EventEmitter();
    let values = [];
    const listenerA = () => { values.push('A'); };
    const listenerB = () => { values.push('B'); };
    const listenerC = () => { values.push('C'); };
    em10.on('multi', listenerA);
    em10.on('multi', listenerB);
    em10.on('multi', listenerC);
    em10.off('multi', listenerB);
    em10.emit('multi');
    assertArrayEqual(values, ['A', 'C'], 'off() removes correct listener from multiple');

    // Remove once listener before it fires
    const em11 = new EventEmitter();
    let onceRemoved = false;
    const onceListener = () => { onceRemoved = true; };
    em11.once('event', onceListener);
    em11.removeListener('event', onceListener);
    em11.emit('event');
    assertFalse(onceRemoved, 'removeListener() can remove once() listener');

    // ============================================
    // removeAllListeners()
    // ============================================
    console.log('\n--- removeAllListeners() ---');

    const em12 = new EventEmitter();
    let allCount = 0;
    em12.on('a', () => { allCount++; });
    em12.on('a', () => { allCount++; });
    em12.on('b', () => { allCount++; });
    em12.removeAllListeners('a');
    em12.emit('a');
    em12.emit('b');
    assertEqual(allCount, 1, 'removeAllListeners(event) removes only that event');

    // Remove all listeners for all events
    const em13 = new EventEmitter();
    let allCount2 = 0;
    em13.on('x', () => { allCount2++; });
    em13.on('y', () => { allCount2++; });
    em13.removeAllListeners();
    em13.emit('x');
    em13.emit('y');
    assertEqual(allCount2, 0, 'removeAllListeners() removes all events');

    // ============================================
    // listenerCount()
    // ============================================
    console.log('\n--- listenerCount() ---');

    const em14 = new EventEmitter();
    assertEqual(em14.listenerCount('test'), 0, 'listenerCount() returns 0 for no listeners');
    em14.on('test', () => {});
    assertEqual(em14.listenerCount('test'), 1, 'listenerCount() returns 1 after one listener');
    em14.on('test', () => {});
    em14.on('test', () => {});
    assertEqual(em14.listenerCount('test'), 3, 'listenerCount() returns 3 after three listeners');

    // ============================================
    // listeners()
    // ============================================
    console.log('\n--- listeners() ---');

    const em15 = new EventEmitter();
    const fn1 = () => {};
    const fn2 = () => {};
    em15.on('ev', fn1);
    em15.on('ev', fn2);
    const list = em15.listeners('ev');
    assertEqual(list.length, 2, 'listeners() returns array of listeners');
    assertEqual(list[0], fn1, 'listeners()[0] is first listener');
    assertEqual(list[1], fn2, 'listeners()[1] is second listener');

    // listeners() returns copy
    const em16 = new EventEmitter();
    const fnA = () => {};
    em16.on('copy', fnA);
    const copy = em16.listeners('copy');
    copy.push(() => {});
    assertEqual(em16.listenerCount('copy'), 1, 'listeners() returns copy, not original');

    // Empty listeners
    const em17 = new EventEmitter();
    assertArrayEqual(em17.listeners('none'), [], 'listeners() returns empty array for no listeners');

    // ============================================
    // eventNames()
    // ============================================
    console.log('\n--- eventNames() ---');

    const em18 = new EventEmitter();
    assertArrayEqual(em18.eventNames(), [], 'eventNames() returns empty array initially');
    em18.on('alpha', () => {});
    em18.on('beta', () => {});
    em18.on('gamma', () => {});
    const names = em18.eventNames();
    assertEqual(names.length, 3, 'eventNames() returns correct count');
    assertTrue(names.includes('alpha'), 'eventNames() includes alpha');
    assertTrue(names.includes('beta'), 'eventNames() includes beta');
    assertTrue(names.includes('gamma'), 'eventNames() includes gamma');

    // ============================================
    // setMaxListeners() and getMaxListeners()
    // ============================================
    console.log('\n--- setMaxListeners() and getMaxListeners() ---');

    const em19 = new EventEmitter();
    assertEqual(em19.getMaxListeners(), 10, 'getMaxListeners() default is 10');
    em19.setMaxListeners(20);
    assertEqual(em19.getMaxListeners(), 20, 'setMaxListeners() changes max');
    em19.setMaxListeners(0);
    assertEqual(em19.getMaxListeners(), 0, 'setMaxListeners(0) sets unlimited');

    // setMaxListeners returns this for chaining
    const em20 = new EventEmitter();
    const chainResult = em20.setMaxListeners(5);
    assertEqual(chainResult, em20, 'setMaxListeners() returns this');

    // ============================================
    // prependListener()
    // ============================================
    console.log('\n--- prependListener() ---');

    const em21 = new EventEmitter();
    let order = [];
    em21.on('order', () => { order.push(1); });
    em21.on('order', () => { order.push(2); });
    if (typeof em21.prependListener === 'function') {
        em21.prependListener('order', () => { order.push(0); });
        em21.emit('order');
        assertEqual(order[0], 0, 'prependListener() adds listener at beginning');
    } else {
        console.log('SKIP: prependListener not available');
    }

    // ============================================
    // prependOnceListener()
    // ============================================
    console.log('\n--- prependOnceListener() ---');

    const em22 = new EventEmitter();
    let prependOnceOrder = [];
    em22.on('order2', () => { prependOnceOrder.push(1); });
    if (typeof em22.prependOnceListener === 'function') {
        em22.prependOnceListener('order2', () => { prependOnceOrder.push(0); });
        em22.emit('order2');
        em22.emit('order2');
        assertEqual(prependOnceOrder.length, 3, 'prependOnceListener() listener fires once');
        assertEqual(prependOnceOrder[0], 0, 'prependOnceListener() adds at beginning');
    } else {
        console.log('SKIP: prependOnceListener not available');
    }

    // ============================================
    // rawListeners()
    // ============================================
    console.log('\n--- rawListeners() ---');

    const em23 = new EventEmitter();
    const rawFn = () => {};
    em23.on('raw', rawFn);
    if (typeof em23.rawListeners === 'function') {
        const raw = em23.rawListeners('raw');
        assertEqual(raw[0], rawFn, 'rawListeners() returns listeners');
    } else {
        console.log('SKIP: rawListeners not available');
    }

    // ============================================
    // Static methods
    // ============================================
    console.log('\n--- Static methods ---');

    // EventEmitter.listenerCount() (deprecated but still used)
    if (typeof EventEmitter.listenerCount === 'function') {
        const em24 = new EventEmitter();
        em24.on('static', () => {});
        em24.on('static', () => {});
        assertEqual(EventEmitter.listenerCount(em24, 'static'), 2, 'EventEmitter.listenerCount() works');
    } else {
        console.log('SKIP: EventEmitter.listenerCount not available');
    }

    // EventEmitter.setMaxListeners() (static)
    if (typeof events.setMaxListeners === 'function') {
        const em25 = new EventEmitter();
        events.setMaxListeners(5, em25);
        assertEqual(em25.getMaxListeners(), 5, 'events.setMaxListeners() works');
    } else {
        console.log('SKIP: events.setMaxListeners not available');
    }

    // EventEmitter.getMaxListeners() (static)
    if (typeof events.getMaxListeners === 'function') {
        const em26 = new EventEmitter();
        em26.setMaxListeners(15);
        assertEqual(events.getMaxListeners(em26), 15, 'events.getMaxListeners() works');
    } else {
        console.log('SKIP: events.getMaxListeners not available');
    }

    // ============================================
    // Chaining
    // ============================================
    console.log('\n--- Method chaining ---');

    const em27 = new EventEmitter();
    const chainedResult = em27
        .on('a', () => {})
        .on('b', () => {})
        .once('c', () => {})
        .setMaxListeners(20);
    assertEqual(chainedResult, em27, 'Methods return this for chaining');

    // ============================================
    // this context in listeners
    // ============================================
    console.log('\n--- this context ---');

    const em28 = new EventEmitter();
    let thisValue = null;
    em28.on('context', function() { thisValue = this; });
    em28.emit('context');
    assertEqual(thisValue, em28, 'this in listener is the emitter');

    // ============================================
    // Special event names
    // ============================================
    console.log('\n--- Special event names ---');

    const em29 = new EventEmitter();
    let specialCalled = false;
    em29.on('error', () => { specialCalled = true; });
    em29.emit('error', new Error('test'));
    assertTrue(specialCalled, 'error event can be handled');

    // Symbol event names (if supported)
    const sym = Symbol('test');
    const em30 = new EventEmitter();
    let symCalled = false;
    em30.on(sym, () => { symCalled = true; });
    em30.emit(sym);
    assertTrue(symCalled, 'Symbol event names work');

    // ============================================
    // Edge cases
    // ============================================
    console.log('\n--- Edge cases ---');

    // Emit with no arguments
    const em31 = new EventEmitter();
    let noArgsCalled = false;
    em31.on('noargs', () => { noArgsCalled = true; });
    em31.emit('noargs');
    assertTrue(noArgsCalled, 'emit() with no extra arguments works');

    // Multiple same listeners
    const em32 = new EventEmitter();
    let sameCount = 0;
    const sameFn = () => { sameCount++; };
    em32.on('same', sameFn);
    em32.on('same', sameFn);
    em32.emit('same');
    assertEqual(sameCount, 2, 'Same listener can be added multiple times');

    // Remove non-existent listener (should not throw)
    const em33 = new EventEmitter();
    let noThrow = true;
    try {
        em33.removeListener('notexist', () => {});
    } catch (e) {
        noThrow = false;
    }
    assertTrue(noThrow, 'Removing non-existent listener does not throw');

    // Listener that removes itself
    const em34 = new EventEmitter();
    let selfRemoveCount = 0;
    const selfRemover = () => {
        selfRemoveCount++;
        em34.removeListener('self', selfRemover);
    };
    em34.on('self', selfRemover);
    em34.emit('self');
    em34.emit('self');
    assertEqual(selfRemoveCount, 1, 'Listener can remove itself');

    // Listener that adds another listener
    const em35 = new EventEmitter();
    let addInEmitCount = 0;
    em35.on('add', () => {
        addInEmitCount++;
        em35.on('add', () => { addInEmitCount++; });
    });
    em35.emit('add');
    assertEqual(addInEmitCount, 1, 'Listener added during emit not called in same emit');
    em35.emit('add');
    assertGreater(addInEmitCount, 1, 'Added listener called on next emit');

    // ============================================
    // EventEmitter.once() static (Promise-based)
    // ============================================
    console.log('\n--- EventEmitter.once() static ---');

    if (typeof EventEmitter.once === 'function') {
        const em36 = new EventEmitter();
        const promise = EventEmitter.once(em36, 'async');
        assertInstanceOf(promise, Promise, 'EventEmitter.once() returns Promise');

        // Can't easily test async in sync context, but verify it exists
        console.log('PASS: EventEmitter.once() static exists');
    } else {
        console.log('SKIP: EventEmitter.once() static not available');
    }

    // ============================================
    // EventEmitter.on() static (async iterator)
    // ============================================
    console.log('\n--- EventEmitter.on() static ---');

    if (typeof EventEmitter.on === 'function') {
        const em37 = new EventEmitter();
        const iterator = EventEmitter.on(em37, 'iter');
        assertTypeOf(iterator[Symbol.asyncIterator], 'function', 'EventEmitter.on() returns async iterator');
        console.log('PASS: EventEmitter.on() static exists');
    } else {
        console.log('SKIP: EventEmitter.on() static not available');
    }

} else {
    console.log('Events module not available, testing with globalThis.EventEmitter');

    if (typeof globalThis.EventEmitter !== 'undefined') {
        const emitter = new EventEmitter();
        let called = false;
        emitter.on('test', () => { called = true; });
        emitter.emit('test');
        assertTrue(called, 'globalThis.EventEmitter works');
    }
}

summary();
