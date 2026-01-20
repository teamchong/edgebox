    // ===== TIMERS MODULE =====
    // Node.js timers module - provides Timeout/Immediate objects with ref/unref/refresh/hasRef

    // Store original timer functions
    const _nativeSetTimeout = globalThis.setTimeout;
    const _nativeClearTimeout = globalThis.clearTimeout;
    const _nativeSetInterval = globalThis.setInterval;
    const _nativeClearInterval = globalThis.clearInterval;
    const _nativeSetImmediate = globalThis.setImmediate || function(callback, ...args) {
        return _nativeSetTimeout(callback, 0, ...args);
    };
    const _nativeClearImmediate = globalThis.clearImmediate || function(immediate) {
        return _nativeClearTimeout(immediate);
    };

    // Timeout class - Node.js compatible timer object
    class Timeout {
        constructor(callback, delay, args, isInterval = false) {
            this._callback = callback;
            this._delay = delay;
            this._args = args;
            this._isInterval = isInterval;
            this._refed = true;
            this._destroyed = false;
            this._start();
        }

        _start() {
            if (this._isInterval) {
                this._id = _nativeSetInterval(() => this._callback(...this._args), this._delay);
            } else {
                this._id = _nativeSetTimeout(() => {
                    this._destroyed = true;
                    this._callback(...this._args);
                }, this._delay);
            }
        }

        ref() {
            this._refed = true;
            return this;
        }

        unref() {
            this._refed = false;
            return this;
        }

        hasRef() {
            return this._refed && !this._destroyed;
        }

        refresh() {
            if (this._destroyed) return this;
            // Clear existing timer and restart
            if (this._isInterval) {
                _nativeClearInterval(this._id);
            } else {
                _nativeClearTimeout(this._id);
            }
            this._start();
            return this;
        }

        close() {
            this._destroy();
            return this;
        }

        _destroy() {
            if (this._destroyed) return;
            this._destroyed = true;
            if (this._isInterval) {
                _nativeClearInterval(this._id);
            } else {
                _nativeClearTimeout(this._id);
            }
        }

        // Allow coercion to number (the ID)
        [Symbol.toPrimitive](hint) {
            if (hint === 'number') return this._id;
            return this._id;
        }
    }

    // Immediate class - Node.js compatible immediate object
    class Immediate {
        constructor(callback, args) {
            this._callback = callback;
            this._args = args;
            this._refed = true;
            this._destroyed = false;
            this._id = _nativeSetImmediate(() => {
                this._destroyed = true;
                this._callback(...this._args);
            });
        }

        ref() {
            this._refed = true;
            return this;
        }

        unref() {
            this._refed = false;
            return this;
        }

        hasRef() {
            return this._refed && !this._destroyed;
        }

        _destroy() {
            if (this._destroyed) return;
            this._destroyed = true;
            _nativeClearImmediate(this._id);
        }

        [Symbol.toPrimitive](hint) {
            if (hint === 'number') return this._id;
            return this._id;
        }
    }

    // Custom setTimeout that returns Timeout object
    function setTimeout(callback, delay, ...args) {
        if (typeof callback !== 'function') {
            throw new TypeError('Callback must be a function');
        }
        return new Timeout(callback, delay || 0, args, false);
    }

    // Custom clearTimeout that handles both Timeout objects and IDs
    function clearTimeout(timer) {
        if (timer instanceof Timeout) {
            timer._destroy();
        } else {
            _nativeClearTimeout(timer);
        }
    }

    // Custom setInterval that returns Timeout object
    function setInterval(callback, delay, ...args) {
        if (typeof callback !== 'function') {
            throw new TypeError('Callback must be a function');
        }
        return new Timeout(callback, delay || 0, args, true);
    }

    // Custom clearInterval that handles both Timeout objects and IDs
    function clearInterval(timer) {
        if (timer instanceof Timeout) {
            timer._destroy();
        } else {
            _nativeClearInterval(timer);
        }
    }

    // Custom setImmediate that returns Immediate object
    function setImmediate(callback, ...args) {
        if (typeof callback !== 'function') {
            throw new TypeError('Callback must be a function');
        }
        return new Immediate(callback, args);
    }

    // Custom clearImmediate that handles both Immediate objects and IDs
    function clearImmediate(immediate) {
        if (immediate instanceof Immediate) {
            immediate._destroy();
        } else {
            _nativeClearImmediate(immediate);
        }
    }

    _modules.timers = {
        setTimeout,
        clearTimeout,
        setInterval,
        clearInterval,
        setImmediate,
        clearImmediate,
        Timeout,
        Immediate
    };

    // Add promises API
    _modules.timers.promises = {
        setTimeout: function(delay, value, options) {
            return new Promise((resolve, reject) => {
                const signal = options?.signal;
                if (signal?.aborted) {
                    reject(new Error('The operation was aborted'));
                    return;
                }
                const timeoutId = globalThis.setTimeout(() => resolve(value), delay);
                if (signal) {
                    signal.addEventListener('abort', () => {
                        globalThis.clearTimeout(timeoutId);
                        reject(new Error('The operation was aborted'));
                    }, { once: true });
                }
            });
        },
        setImmediate: function(value, options) {
            return new Promise((resolve, reject) => {
                const signal = options?.signal;
                if (signal?.aborted) {
                    reject(new Error('The operation was aborted'));
                    return;
                }
                const immediateId = (_modules.timers.setImmediate || globalThis.setImmediate ||
                    ((cb) => globalThis.setTimeout(cb, 0)))(() => resolve(value));
                if (signal) {
                    signal.addEventListener('abort', () => {
                        (_modules.timers.clearImmediate || globalThis.clearImmediate ||
                            globalThis.clearTimeout)(immediateId);
                        reject(new Error('The operation was aborted'));
                    }, { once: true });
                }
            });
        },
        setInterval: function(delay, value, options) {
            // Returns an async iterator
            const signal = options?.signal;
            return {
                [Symbol.asyncIterator]() {
                    let intervalId;
                    let resolve;
                    let rejected = false;

                    return {
                        next() {
                            if (signal?.aborted || rejected) {
                                return Promise.resolve({ done: true, value: undefined });
                            }
                            return new Promise((res, rej) => {
                                resolve = res;
                                intervalId = globalThis.setInterval(() => {
                                    res({ done: false, value });
                                }, delay);
                                if (signal) {
                                    signal.addEventListener('abort', () => {
                                        globalThis.clearInterval(intervalId);
                                        rejected = true;
                                        res({ done: true, value: undefined });
                                    }, { once: true });
                                }
                            });
                        },
                        return() {
                            if (intervalId) globalThis.clearInterval(intervalId);
                            return Promise.resolve({ done: true, value: undefined });
                        }
                    };
                }
            };
        }
    };

    // Node.js aliases
    _modules['node:timers'] = _modules.timers;
    _modules['timers/promises'] = _modules.timers.promises;
    _modules['node:timers/promises'] = _modules.timers.promises;
