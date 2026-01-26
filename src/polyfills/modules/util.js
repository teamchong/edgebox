    // ===== UTIL MODULE (JavaScript additions) =====
    // This creates the initial _modules.util object with JavaScript-based utilities
    // The Zig native util module will merge in its functions (format, inspect, etc.)

    // MIMEParams class - parameter handling for MIME types
    class MIMEParams {
        constructor() {
            this._map = new Map();
        }

        get(name) {
            return this._map.get(name.toLowerCase());
        }

        set(name, value) {
            this._map.set(name.toLowerCase(), String(value));
        }

        has(name) {
            return this._map.has(name.toLowerCase());
        }

        delete(name) {
            return this._map.delete(name.toLowerCase());
        }

        entries() {
            return this._map.entries();
        }

        keys() {
            return this._map.keys();
        }

        values() {
            return this._map.values();
        }

        [Symbol.iterator]() {
            return this._map[Symbol.iterator]();
        }

        toString() {
            const parts = [];
            for (const [key, value] of this._map) {
                // Quote values with special characters
                if (/[\s";=]/.test(value)) {
                    parts.push(`${key}="${value.replace(/"/g, '\\"')}"`);
                } else {
                    parts.push(`${key}=${value}`);
                }
            }
            return parts.join('; ');
        }
    }

    // MIMEType class - MIME type parsing and manipulation
    class MIMEType {
        constructor(input) {
            if (typeof input !== 'string') {
                throw new TypeError('The "input" argument must be a string');
            }

            // Parse MIME type: type/subtype[; param=value]*
            const trimmed = input.trim();
            const semicolonIndex = trimmed.indexOf(';');
            const essencePart = semicolonIndex === -1 ? trimmed : trimmed.slice(0, semicolonIndex);
            const paramsPart = semicolonIndex === -1 ? '' : trimmed.slice(semicolonIndex + 1);

            // Parse type/subtype
            const slashIndex = essencePart.indexOf('/');
            if (slashIndex === -1) {
                throw new TypeError('Invalid MIME type: missing slash');
            }

            this._type = essencePart.slice(0, slashIndex).trim().toLowerCase();
            this._subtype = essencePart.slice(slashIndex + 1).trim().toLowerCase();

            if (!this._type || !this._subtype) {
                throw new TypeError('Invalid MIME type: empty type or subtype');
            }

            // Parse parameters
            this._params = new MIMEParams();
            if (paramsPart) {
                // Split by semicolon, handling quoted values
                const paramMatches = paramsPart.matchAll(/([^=;\s]+)=(?:"([^"]*(?:\\.[^"]*)*)"|([^;\s]*))/g);
                for (const match of paramMatches) {
                    const key = match[1].trim().toLowerCase();
                    // Use quoted value (match[2]) or unquoted value (match[3])
                    const value = match[2] !== undefined ? match[2].replace(/\\"/g, '"') : match[3];
                    this._params.set(key, value);
                }
            }
        }

        get type() {
            return this._type;
        }

        set type(value) {
            this._type = String(value).toLowerCase();
        }

        get subtype() {
            return this._subtype;
        }

        set subtype(value) {
            this._subtype = String(value).toLowerCase();
        }

        get essence() {
            return `${this._type}/${this._subtype}`;
        }

        get params() {
            return this._params;
        }

        toString() {
            let result = this.essence;
            const paramsStr = this._params.toString();
            if (paramsStr) {
                result += '; ' + paramsStr;
            }
            return result;
        }

        toJSON() {
            return this.toString();
        }
    }

    // Initialize _modules.util if not exists (Zig code will merge in native functions)
    if (!_modules.util) {
        _modules.util = {};
    }

    // Add MIMEType and MIMEParams to util module
    _modules.util.MIMEType = MIMEType;
    _modules.util.MIMEParams = MIMEParams;

    // Add promisify if not already defined by native code
    if (!_modules.util.promisify) {
        _modules.util.promisify = function(fn) {
            if (typeof fn !== 'function') {
                throw new TypeError('The "original" argument must be of type Function');
            }
            return function(...args) {
                return new Promise((resolve, reject) => {
                    fn.call(this, ...args, (err, result) => {
                        if (err) reject(err);
                        else resolve(result);
                    });
                });
            };
        };
    }

    // Add util.types if not already defined by native code
    if (!_modules.util.types) {
        _modules.util.types = {
            isArray: Array.isArray,
            isDate: (obj) => obj instanceof Date,
            isRegExp: (obj) => obj instanceof RegExp,
            isMap: (obj) => obj instanceof Map,
            isSet: (obj) => obj instanceof Set,
            isPromise: (obj) => obj instanceof Promise,
            isNativeError: (obj) => obj instanceof Error,
            isArrayBuffer: (obj) => obj instanceof ArrayBuffer,
            isTypedArray: (obj) => ArrayBuffer.isView(obj) && !(obj instanceof DataView),
            isDataView: (obj) => obj instanceof DataView,
            isWeakMap: (obj) => obj instanceof WeakMap,
            isWeakSet: (obj) => obj instanceof WeakSet,
            isUint8Array: (obj) => obj instanceof Uint8Array,
            isUint16Array: (obj) => obj instanceof Uint16Array,
            isUint32Array: (obj) => obj instanceof Uint32Array,
            isInt8Array: (obj) => obj instanceof Int8Array,
            isInt16Array: (obj) => obj instanceof Int16Array,
            isInt32Array: (obj) => obj instanceof Int32Array,
            isFloat32Array: (obj) => obj instanceof Float32Array,
            isFloat64Array: (obj) => obj instanceof Float64Array,
            isBigInt64Array: (obj) => typeof BigInt64Array !== 'undefined' && obj instanceof BigInt64Array,
            isBigUint64Array: (obj) => typeof BigUint64Array !== 'undefined' && obj instanceof BigUint64Array,
        };
    }

    // Also set node: alias if not set by Zig
    if (!_modules['node:util']) {
        _modules['node:util'] = _modules.util;
    }

