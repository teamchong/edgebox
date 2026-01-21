// EdgeBox Runtime: AbortController, AbortSignal
// Dependencies: events.js (Event, EventTarget)

// EARLY v9 definition for SDK AbortController creation
if (typeof globalThis.v9 === 'undefined') {
    globalThis.v9 = function() {
        if (typeof AbortController !== 'undefined') {
            return new AbortController();
        }
        return { signal: { aborted: false }, abort: function() {} };
    };
}

if (typeof AbortController === 'undefined') {
    globalThis.AbortSignal = class AbortSignal extends EventTarget {
        constructor() { super(); this.aborted = false; this.reason = undefined; }
        throwIfAborted() { if (this.aborted) throw this.reason; }
        static abort(reason) { const s = new AbortSignal(); s.aborted = true; s.reason = reason || new DOMException('Aborted', 'AbortError'); return s; }
        static timeout(ms) { const s = new AbortSignal(); setTimeout(() => { s.aborted = true; s.reason = new DOMException('Timeout', 'TimeoutError'); s.dispatchEvent(new Event('abort')); }, ms); return s; }
    };
    globalThis.AbortController = class AbortController {
        constructor() { this.signal = new AbortSignal(); }
        abort(reason) { this.signal.aborted = true; this.signal.reason = reason || new DOMException('Aborted', 'AbortError'); this.signal.dispatchEvent(new Event('abort')); }
    };
}
