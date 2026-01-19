    // ===== TIMERS MODULE =====
    // Node.js timers module - re-exports global timer functions
    // These are already available globally, this module just provides the module interface

    _modules.timers = {
        setTimeout: globalThis.setTimeout,
        clearTimeout: globalThis.clearTimeout,
        setInterval: globalThis.setInterval,
        clearInterval: globalThis.clearInterval,
        setImmediate: globalThis.setImmediate || function(callback, ...args) {
            return globalThis.setTimeout(callback, 0, ...args);
        },
        clearImmediate: globalThis.clearImmediate || function(immediate) {
            return globalThis.clearTimeout(immediate);
        }
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
