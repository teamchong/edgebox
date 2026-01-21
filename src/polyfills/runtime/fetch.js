// EdgeBox Runtime: fetch, Headers, Request, Response, FormData
// Dependencies: text.js (TextEncoder/TextDecoder), base64.js (atob)

// FormData polyfill
if (typeof FormData === 'undefined') {
    globalThis.FormData = class FormData {
        constructor() { this._entries = []; }
        append(name, value, filename) { this._entries.push([String(name), value, filename]); }
        delete(name) { this._entries = this._entries.filter(([n]) => n !== name); }
        get(name) { const e = this._entries.find(([n]) => n === name); return e ? e[1] : null; }
        getAll(name) { return this._entries.filter(([n]) => n === name).map(e => e[1]); }
        has(name) { return this._entries.some(([n]) => n === name); }
        set(name, value, filename) { this.delete(name); this.append(name, value, filename); }
        keys() { return this._entries.map(e => e[0])[Symbol.iterator](); }
        values() { return this._entries.map(e => e[1])[Symbol.iterator](); }
        entries() { return this._entries.map(e => [e[0], e[1]])[Symbol.iterator](); }
        forEach(cb, thisArg) { this._entries.forEach(([n, v]) => cb.call(thisArg, v, n, this)); }
        [Symbol.iterator]() { return this.entries(); }
    };
}

// Headers class
if (typeof Headers === 'undefined') {
    globalThis.Headers = class Headers {
        constructor(init) {
            this._headers = {};
            if (init instanceof Headers) {
                init.forEach((value, key) => this.set(key, value));
            } else if (typeof init === 'object' && init !== null) {
                Object.entries(init).forEach(([key, value]) => this.set(key, value));
            }
        }
        get(name) { return this._headers[name.toLowerCase()] || null; }
        set(name, value) { this._headers[name.toLowerCase()] = String(value); }
        has(name) { return name.toLowerCase() in this._headers; }
        delete(name) { delete this._headers[name.toLowerCase()]; }
        append(name, value) {
            const key = name.toLowerCase();
            if (this._headers[key]) this._headers[key] += ', ' + value;
            else this._headers[key] = String(value);
        }
        forEach(callback) { Object.entries(this._headers).forEach(([k, v]) => callback(v, k, this)); }
        entries() { return Object.entries(this._headers)[Symbol.iterator](); }
        keys() { return Object.keys(this._headers)[Symbol.iterator](); }
        values() { return Object.values(this._headers)[Symbol.iterator](); }
        [Symbol.iterator]() { return this.entries(); }
        normalize() { return this; }
        toJSON() { return { ...this._headers }; }
    };
}

// Request class
if (typeof Request === 'undefined') {
    globalThis.Request = class Request {
        constructor(input, init = {}) {
            if (input instanceof Request) {
                this.url = input.url;
                this.method = init.method || input.method || 'GET';
                this.headers = new Headers(init.headers || input.headers);
                this.body = init.body !== undefined ? init.body : input.body;
            } else {
                this.url = String(input);
                this.method = init.method || 'GET';
                this.headers = new Headers(init.headers);
                this.body = init.body || null;
            }
            this.signal = init.signal || null;
            this.credentials = init.credentials || 'same-origin';
            this.mode = init.mode || 'cors';
            this.cache = init.cache || 'default';
            this.redirect = init.redirect || 'follow';
            this.referrer = init.referrer || '';
            this.duplex = init.duplex || 'half';
        }
        clone() { return new Request(this); }
    };
}

// Response class
if (typeof Response === 'undefined') {
    globalThis.Response = class Response {
        constructor(body, init = {}) {
            this._body = body;
            this._bodyUsed = false;
            this.status = init.status || 200;
            this.statusText = init.statusText || 'OK';
            this.ok = this.status >= 200 && this.status < 300;
            this.headers = init.headers instanceof Headers ? init.headers : new Headers(init.headers);
            this.type = 'basic';
            this.url = init.url || '';
            this.redirected = false;
        }
        get bodyUsed() { return this._bodyUsed; }
        get body() {
            const self = this;
            const contentType = this.headers && typeof this.headers.get === 'function'
                ? this.headers.get('content-type') : '';
            const headerIsSSE = contentType && contentType.includes('text/event-stream');
            const bodyLooksLikeSSE = typeof self._body === 'string' &&
                (self._body.startsWith('event:') || self._body.startsWith('data:') ||
                 self._body.includes('\n\ndata:') || self._body.includes('\n\nevent:'));
            const isSSE = headerIsSSE || bodyLooksLikeSSE;

            if (isSSE && typeof self._body === 'string') {
                return {
                    getReader() {
                        const events = self._body.split('\n\n').filter(e => e.trim());
                        let index = 0;
                        return {
                            async read() {
                                if (index >= events.length) {
                                    return { done: true, value: undefined };
                                }
                                self._bodyUsed = true;
                                const eventStr = events[index++];
                                const dataMatch = eventStr.match(/^data:\s*(.*)$/m);
                                if (dataMatch) {
                                    const chunk = new TextEncoder().encode(dataMatch[1] + '\n');
                                    return { done: false, value: chunk };
                                }
                                return this.read();
                            },
                            releaseLock() {}
                        };
                    },
                    [Symbol.asyncIterator]() {
                        const reader = this.getReader();
                        return {
                            async next() { return reader.read(); }
                        };
                    }
                };
            }

            return {
                getReader() {
                    let done = false;
                    return {
                        async read() {
                            if (done) return { done: true, value: undefined };
                            done = true;
                            self._bodyUsed = true;
                            const data = typeof self._body === 'string'
                                ? new TextEncoder().encode(self._body)
                                : self._body;
                            return { done: false, value: data };
                        },
                        releaseLock() {}
                    };
                },
                [Symbol.asyncIterator]() {
                    const reader = this.getReader();
                    return {
                        async next() { return reader.read(); }
                    };
                }
            };
        }
        async text() { this._bodyUsed = true; return typeof this._body === 'string' ? this._body : new TextDecoder().decode(this._body); }
        async json() { return JSON.parse(await this.text()); }
        async arrayBuffer() {
            this._bodyUsed = true;
            if (this._body instanceof ArrayBuffer) return this._body;
            if (typeof this._body === 'string') return new TextEncoder().encode(this._body).buffer;
            return this._body.buffer || this._body;
        }
        async blob() { return new Blob([await this.arrayBuffer()]); }
        clone() { return new Response(this._body, { status: this.status, statusText: this.statusText, headers: this.headers }); }
    };
}

// Fetch polyfill using native binding
{
    const hasSyncApi = typeof globalThis.__edgebox_fetch === 'function';
    if (globalThis._edgebox_debug) print('[FETCH POLYFILL] hasSyncApi=' + hasSyncApi);

    if (hasSyncApi) {
        const _edgebox_fetch = async function(input, options = {}) {
            let url, method, headers, body;
            if (input instanceof Request) {
                url = input.url;
                method = options.method || input.method || 'GET';
                headers = options.headers || input.headers || {};
                body = options.body !== undefined ? options.body : input.body;
            } else {
                url = typeof input === 'string' ? input : String(input);
                method = options.method || 'GET';
                headers = options.headers || {};
                body = options.body || null;
            }

            let headersObj = headers;
            if (headers instanceof Headers) {
                headersObj = {};
                headers.forEach((value, key) => { headersObj[key] = value; });
            } else if (headers && typeof headers.toJSON === 'function') {
                headersObj = headers.toJSON();
            }

            let bodyStr = null;
            if (body !== null && body !== undefined) {
                if (typeof body === 'string') {
                    bodyStr = body;
                } else if (body instanceof ArrayBuffer) {
                    bodyStr = new TextDecoder().decode(body);
                } else if (ArrayBuffer.isView(body)) {
                    bodyStr = new TextDecoder().decode(body);
                } else if (typeof body === 'object') {
                    bodyStr = JSON.stringify(body);
                } else {
                    bodyStr = String(body);
                }
            }

            // Handle data: URLs
            if (url.startsWith('data:')) {
                try {
                    const commaIndex = url.indexOf(',');
                    if (commaIndex === -1) throw new Error('Invalid data URL');
                    const meta = url.slice(5, commaIndex);
                    const dataStr = url.slice(commaIndex + 1);
                    const isBase64 = meta.includes(';base64');
                    const mimeType = meta.replace(';base64', '') || 'text/plain';

                    let bodyBytes;
                    if (isBase64) {
                        const binaryStr = atob(dataStr);
                        bodyBytes = new Uint8Array(binaryStr.length);
                        for (let i = 0; i < binaryStr.length; i++) {
                            bodyBytes[i] = binaryStr.charCodeAt(i);
                        }
                    } else {
                        bodyBytes = new TextEncoder().encode(decodeURIComponent(dataStr));
                    }

                    const responseHeaders = new Headers({ 'content-type': mimeType });
                    return new Response(bodyBytes, {
                        status: 200,
                        statusText: 'OK',
                        headers: responseHeaders,
                        url: url
                    });
                } catch (e) {
                    throw e;
                }
            }

            const result = globalThis.__edgebox_fetch(url, method, JSON.stringify(headersObj), bodyStr);

            const responseHeaders = new Headers(result.headers || {});
            return new Response(result.body, {
                status: result.status,
                statusText: result.ok ? 'OK' : 'Error',
                headers: responseHeaders,
                url: url
            });
        };

        globalThis.fetch = _edgebox_fetch;
        try {
            if (typeof global !== 'undefined') global.fetch = _edgebox_fetch;
        } catch(e) {}
        if (globalThis._edgebox_debug) print('[FETCH POLYFILL] fetch installed on globalThis');
    }
}
