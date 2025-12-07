// EdgeBox Runtime Polyfills
// These are bundled with user code at build time for bytecode caching

// Node.js global aliases
globalThis.global = globalThis;
globalThis.self = globalThis;

// Basic console polyfill
if (typeof console === 'undefined') {
    globalThis.console = {
        log: (...args) => print(...args),
        error: (...args) => print('ERROR:', ...args),
        warn: (...args) => print('WARN:', ...args),
        info: (...args) => print('INFO:', ...args),
        debug: (...args) => print('DEBUG:', ...args),
        trace: (...args) => print('TRACE:', ...args),
    };
}

// Timer polyfills
(function() {
    let _timerId = 1;
    const _timers = new Map();

    globalThis.setTimeout = function(callback, delay = 0, ...args) {
        const id = _timerId++;
        _timers.set(id, { callback, args, delay });
        if (delay === 0) {
            queueMicrotask(() => {
                if (_timers.has(id)) {
                    _timers.delete(id);
                    callback(...args);
                }
            });
        }
        return id;
    };

    globalThis.clearTimeout = function(id) { _timers.delete(id); };
    globalThis.setInterval = function(callback, delay = 0, ...args) { return _timerId++; };
    globalThis.clearInterval = function(id) {};
    globalThis.setImmediate = function(callback, ...args) { return setTimeout(callback, 0, ...args); };
    globalThis.clearImmediate = function(id) { clearTimeout(id); };
})();

// TextEncoder/TextDecoder polyfills
if (typeof TextEncoder === 'undefined') {
    globalThis.TextEncoder = class TextEncoder {
        constructor(encoding = 'utf-8') { this.encoding = encoding; }
        encode(str) {
            const bytes = [];
            for (let i = 0; i < str.length; i++) {
                let code = str.charCodeAt(i);
                if (code < 0x80) bytes.push(code);
                else if (code < 0x800) { bytes.push(0xC0 | (code >> 6)); bytes.push(0x80 | (code & 0x3F)); }
                else if (code >= 0xD800 && code <= 0xDBFF && i + 1 < str.length) {
                    const next = str.charCodeAt(++i);
                    if (next >= 0xDC00 && next <= 0xDFFF) {
                        code = ((code - 0xD800) << 10) + (next - 0xDC00) + 0x10000;
                        bytes.push(0xF0 | (code >> 18)); bytes.push(0x80 | ((code >> 12) & 0x3F));
                        bytes.push(0x80 | ((code >> 6) & 0x3F)); bytes.push(0x80 | (code & 0x3F));
                    }
                } else { bytes.push(0xE0 | (code >> 12)); bytes.push(0x80 | ((code >> 6) & 0x3F)); bytes.push(0x80 | (code & 0x3F)); }
            }
            return new Uint8Array(bytes);
        }
    };
}

if (typeof TextDecoder === 'undefined') {
    globalThis.TextDecoder = class TextDecoder {
        constructor(encoding = 'utf-8') { this.encoding = encoding; }
        decode(input) {
            const bytes = input instanceof Uint8Array ? input : new Uint8Array(input);
            let str = '';
            for (let i = 0; i < bytes.length; ) {
                const b = bytes[i++];
                if (b < 0x80) str += String.fromCharCode(b);
                else if (b < 0xE0) str += String.fromCharCode(((b & 0x1F) << 6) | (bytes[i++] & 0x3F));
                else if (b < 0xF0) str += String.fromCharCode(((b & 0x0F) << 12) | ((bytes[i++] & 0x3F) << 6) | (bytes[i++] & 0x3F));
                else {
                    const code = ((b & 0x07) << 18) | ((bytes[i++] & 0x3F) << 12) | ((bytes[i++] & 0x3F) << 6) | (bytes[i++] & 0x3F);
                    if (code > 0xFFFF) { const offset = code - 0x10000; str += String.fromCharCode(0xD800 + (offset >> 10), 0xDC00 + (offset & 0x3FF)); }
                    else str += String.fromCharCode(code);
                }
            }
            return str;
        }
    };
}

// DOMException polyfill
if (typeof DOMException === 'undefined') {
    globalThis.DOMException = class DOMException extends Error {
        constructor(message, name) { super(message); this.name = name || 'Error'; }
    };
}

// Event polyfills
if (typeof Event === 'undefined') {
    globalThis.Event = class Event {
        constructor(type, options = {}) {
            this.type = type;
            this.bubbles = options.bubbles || false;
            this.cancelable = options.cancelable || false;
            this.composed = options.composed || false;
            this.defaultPrevented = false;
            this.target = null;
            this.currentTarget = null;
            this.timeStamp = Date.now();
        }
        preventDefault() { this.defaultPrevented = true; }
        stopPropagation() {}
        stopImmediatePropagation() {}
    };
}
if (typeof CustomEvent === 'undefined') {
    globalThis.CustomEvent = class CustomEvent extends Event {
        constructor(type, options = {}) { super(type, options); this.detail = options.detail || null; }
    };
}
if (typeof EventTarget === 'undefined') {
    globalThis.EventTarget = class EventTarget {
        constructor() { this._listeners = {}; }
        addEventListener(type, listener) { (this._listeners[type] = this._listeners[type] || []).push(listener); }
        removeEventListener(type, listener) { if (this._listeners[type]) this._listeners[type] = this._listeners[type].filter(l => l !== listener); }
        dispatchEvent(event) { event.target = this; (this._listeners[event.type] || []).forEach(l => l(event)); return !event.defaultPrevented; }
    };
}

// Crypto polyfill
globalThis.crypto = globalThis.crypto || {
    getRandomValues(array) {
        for (let i = 0; i < array.length; i++) {
            if (array instanceof Uint8Array) array[i] = Math.floor(Math.random() * 256);
            else if (array instanceof Uint16Array) array[i] = Math.floor(Math.random() * 65536);
            else if (array instanceof Uint32Array) array[i] = Math.floor(Math.random() * 4294967296);
            else array[i] = Math.floor(Math.random() * 256);
        }
        return array;
    },
    randomUUID() {
        const bytes = new Uint8Array(16);
        crypto.getRandomValues(bytes);
        bytes[6] = (bytes[6] & 0x0f) | 0x40;
        bytes[8] = (bytes[8] & 0x3f) | 0x80;
        const hex = Array.from(bytes, b => b.toString(16).padStart(2, '0')).join('');
        return hex.slice(0,8) + '-' + hex.slice(8,12) + '-' + hex.slice(12,16) + '-' + hex.slice(16,20) + '-' + hex.slice(20);
    },
};

// Performance polyfill
globalThis.performance = globalThis.performance || { now: () => Date.now(), mark: () => {}, measure: () => {}, getEntriesByType: () => [] };

// Intl polyfill (minimal)
if (typeof Intl === 'undefined') {
    globalThis.Intl = {
        DateTimeFormat: class DateTimeFormat { constructor(locale, opts) { this.locale = locale; this.opts = opts; } format(date) { return date.toISOString(); } formatToParts(date) { return [{ type: 'literal', value: date.toISOString() }]; } resolvedOptions() { return this.opts || {}; } },
        NumberFormat: class NumberFormat { constructor(locale, opts) { this.locale = locale; this.opts = opts; } format(num) { return String(num); } formatToParts(num) { return [{ type: 'integer', value: String(num) }]; } resolvedOptions() { return this.opts || {}; } },
        Collator: class Collator { constructor(locale, opts) { this.locale = locale; } compare(a, b) { return a < b ? -1 : a > b ? 1 : 0; } resolvedOptions() { return {}; } },
        PluralRules: class PluralRules { constructor(locale, opts) { this.locale = locale; } select(n) { return n === 1 ? 'one' : 'other'; } resolvedOptions() { return {}; } },
        RelativeTimeFormat: class RelativeTimeFormat { constructor(locale, opts) { this.locale = locale; } format(value, unit) { return value + ' ' + unit + (Math.abs(value) !== 1 ? 's' : '') + (value < 0 ? ' ago' : ''); } formatToParts(value, unit) { return [{ type: 'literal', value: this.format(value, unit) }]; } resolvedOptions() { return {}; } },
        ListFormat: class ListFormat { constructor(locale, opts) { this.locale = locale; } format(list) { return list.join(', '); } formatToParts(list) { return list.map(v => ({ type: 'element', value: v })); } resolvedOptions() { return {}; } },
        Segmenter: class Segmenter { constructor(locale, opts) { this.locale = locale; this.granularity = opts?.granularity || 'grapheme'; } segment(str) { return { [Symbol.iterator]: function*() { for (let i = 0; i < str.length; i++) yield { segment: str[i], index: i }; } }; } resolvedOptions() { return { granularity: this.granularity }; } },
        getCanonicalLocales: (locales) => Array.isArray(locales) ? locales : [locales],
        supportedValuesOf: (key) => []
    };
}

// URLSearchParams polyfill
if (typeof URLSearchParams === 'undefined') {
    globalThis.URLSearchParams = class URLSearchParams {
        constructor(init) {
            this._params = [];
            if (typeof init === 'string') {
                const query = init.startsWith('?') ? init.slice(1) : init;
                query.split('&').forEach(pair => {
                    const [k, v] = pair.split('=').map(decodeURIComponent);
                    if (k) this._params.push([k, v || '']);
                });
            } else if (init && typeof init === 'object') {
                Object.entries(init).forEach(([k, v]) => this._params.push([k, String(v)]));
            }
        }
        get(name) { const p = this._params.find(([k]) => k === name); return p ? p[1] : null; }
        getAll(name) { return this._params.filter(([k]) => k === name).map(([, v]) => v); }
        has(name) { return this._params.some(([k]) => k === name); }
        set(name, value) { this.delete(name); this._params.push([name, String(value)]); }
        append(name, value) { this._params.push([name, String(value)]); }
        delete(name) { this._params = this._params.filter(([k]) => k !== name); }
        keys() { return this._params.map(([k]) => k)[Symbol.iterator](); }
        values() { return this._params.map(([, v]) => v)[Symbol.iterator](); }
        entries() { return this._params[Symbol.iterator](); }
        forEach(fn) { this._params.forEach(([k, v]) => fn(v, k, this)); }
        toString() { return this._params.map(([k, v]) => encodeURIComponent(k) + '=' + encodeURIComponent(v)).join('&'); }
        [Symbol.iterator]() { return this.entries(); }
    };
}

// URL polyfill
if (typeof URL === 'undefined') {
    globalThis.URL = class URL {
        constructor(url, base) {
            if (base) {
                const baseUrl = new URL(base);
                if (url.startsWith('/')) url = baseUrl.origin + url;
                else if (!url.includes('://')) url = baseUrl.href.replace(/[^/]*$/, '') + url;
            }
            const match = url.match(/^([^:]+):\/\/([^/:]+)(?::(\d+))?(\/[^?#]*)?(\?[^#]*)?(#.*)?$/);
            if (!match) throw new TypeError('Invalid URL');
            this.protocol = match[1] + ':';
            this.hostname = match[2];
            this.port = match[3] || '';
            this.pathname = match[4] || '/';
            this.search = match[5] || '';
            this.hash = match[6] || '';
            this.searchParams = new URLSearchParams(this.search);
        }
        get host() { return this.hostname + (this.port ? ':' + this.port : ''); }
        get origin() { return this.protocol + '//' + this.host; }
        get href() { return this.origin + this.pathname + this.search + this.hash; }
        toString() { return this.href; }
        toJSON() { return this.href; }
    };
}

// AbortController polyfill
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
