    // ===== EVENTS MODULE =====
    // Optimized EventEmitter - minimal overhead per emit
    class EventEmitter {
        constructor() { this._events = Object.create(null); this._maxListeners = 10; }
        on(event, listener) {
            const e = this._events;
            if (!e[event]) e[event] = listener;
            else if (typeof e[event] === 'function') e[event] = [e[event], listener];
            else e[event].push(listener);
            return this;
        }
        addListener(event, listener) { return this.on(event, listener); }
        once(event, listener) {
            const self = this;
            function wrapper() { self.removeListener(event, wrapper); listener.apply(this, arguments); }
            wrapper._original = listener;
            return this.on(event, wrapper);
        }
        off(event, listener) { return this.removeListener(event, listener); }
        removeListener(event, listener) {
            const e = this._events[event];
            if (!e) return this;
            if (e === listener || e._original === listener) delete this._events[event];
            else if (Array.isArray(e)) {
                for (let i = 0; i < e.length; i++) {
                    if (e[i] === listener || e[i]._original === listener) { e.splice(i, 1); break; }
                }
                if (e.length === 1) this._events[event] = e[0];
            }
            return this;
        }
        removeAllListeners(event) { if (event) delete this._events[event]; else this._events = Object.create(null); return this; }
        emit(event, a1, a2, a3) {
            const e = this._events[event];
            if (!e) return false;
            // Fast path: single listener (most common case)
            if (typeof e === 'function') e.call(this, a1, a2, a3);
            else { const len = e.length; for (let i = 0; i < len; i++) e[i].call(this, a1, a2, a3); }
            return true;
        }
        listenerCount(event) {
            const e = this._events[event];
            if (!e) return 0;
            return typeof e === 'function' ? 1 : e.length;
        }
        listeners(event) {
            const e = this._events[event];
            if (!e) return [];
            return typeof e === 'function' ? [e] : e.slice();
        }
        eventNames() { return Object.keys(this._events); }
        setMaxListeners(n) { this._maxListeners = n; return this; }
        getMaxListeners() { return this._maxListeners; }
    }
    _modules.events = EventEmitter;
    // Also expose as object with EventEmitter property for Node.js compat
    _modules.events.EventEmitter = EventEmitter;
    // Static setMaxListeners for AbortSignal (Node.js 15+)
    _modules.events.setMaxListeners = function(n, ...targets) {
        // For signals, this is a no-op in our polyfill since we don't track signal listeners
        // In real Node.js this sets a limit on listeners for the given targets
        for (const target of targets) {
            if (target && typeof target.setMaxListeners === 'function') {
                target.setMaxListeners(n);
            }
        }
    };
    _modules.events.getMaxListeners = function(target) {
        if (target && typeof target.getMaxListeners === 'function') {
            return target.getMaxListeners();
        }
        return 10;
    };
    _modules['node:events'] = _modules.events;

