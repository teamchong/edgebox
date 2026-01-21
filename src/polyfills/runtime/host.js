// EdgeBox Runtime: HostArray, HostMap (Zig-backed data structures)

if (typeof __edgebox_array_new === 'function') {
    /**
     * HostArray - Native i32 array backed by Zig host functions
     * ~10x faster than JS arrays for numeric sort operations
     * IMPORTANT: Must call free() when done to release host resources
     */
    globalThis.HostArray = class HostArray {
        constructor() {
            this._handle = __edgebox_array_new();
            if (this._handle < 0) throw new Error('Failed to create HostArray');
        }

        push(value) {
            __edgebox_array_push(this._handle, value | 0);
            return this;
        }

        pop() {
            return __edgebox_array_pop(this._handle);
        }

        get(index) {
            return __edgebox_array_get(this._handle, index | 0);
        }

        set(index, value) {
            __edgebox_array_set(this._handle, index | 0, value | 0);
            return this;
        }

        get length() {
            return __edgebox_array_len(this._handle);
        }

        sort() {
            __edgebox_array_sort(this._handle);
            return this;
        }

        sortDesc() {
            __edgebox_array_sort_desc(this._handle);
            return this;
        }

        reverse() {
            __edgebox_array_reverse(this._handle);
            return this;
        }

        clear() {
            __edgebox_array_clear(this._handle);
            return this;
        }

        indexOf(value) {
            return __edgebox_array_index_of(this._handle, value | 0);
        }

        free() {
            if (this._handle >= 0) {
                __edgebox_array_free(this._handle);
                this._handle = -1;
            }
        }

        toArray() {
            const len = this.length;
            const arr = new Array(len);
            for (let i = 0; i < len; i++) {
                arr[i] = this.get(i);
            }
            return arr;
        }

        static from(jsArray) {
            const arr = new HostArray();
            for (const v of jsArray) {
                arr.push(v | 0);
            }
            return arr;
        }
    };
}

if (typeof __edgebox_map_new === 'function') {
    /**
     * HostMap - Native string->i32 map backed by Zig host functions
     * IMPORTANT: Must call free() when done to release host resources
     */
    globalThis.HostMap = class HostMap {
        constructor() {
            this._handle = __edgebox_map_new();
            if (this._handle < 0) throw new Error('Failed to create HostMap');
        }

        set(key, value) {
            if (typeof key !== 'string') throw new TypeError('key must be a string');
            const result = __edgebox_map_set(this._handle, key, value | 0);
            if (result < 0) throw new Error('map_set failed');
            return this;
        }

        get(key) {
            if (typeof key !== 'string') return undefined;
            const result = __edgebox_map_get(this._handle, key);
            return result === -1 ? undefined : result;
        }

        has(key) {
            if (typeof key !== 'string') return false;
            return __edgebox_map_has(this._handle, key);
        }

        delete(key) {
            if (typeof key !== 'string') return false;
            return __edgebox_map_delete(this._handle, key);
        }

        get size() {
            return __edgebox_map_len(this._handle);
        }

        clear() {
            __edgebox_map_clear(this._handle);
            return this;
        }

        free() {
            if (this._handle >= 0) {
                __edgebox_map_free(this._handle);
                this._handle = -1;
            }
        }
    };
}
