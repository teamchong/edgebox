// EdgeBox Runtime: Event, CustomEvent, EventTarget

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
