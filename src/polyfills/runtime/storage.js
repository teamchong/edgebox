// EdgeBox Runtime: localStorage

if (typeof globalThis.localStorage === 'undefined') {
    const _storage = {};
    globalThis.localStorage = {
        getItem: (key) => _storage[key] !== undefined ? _storage[key] : null,
        setItem: (key, value) => { _storage[key] = String(value); },
        removeItem: (key) => { delete _storage[key]; },
        clear: () => { for (const k in _storage) delete _storage[k]; },
        key: (i) => Object.keys(_storage)[i] || null,
        get length() { return Object.keys(_storage).length; }
    };
}
