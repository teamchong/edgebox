// EdgeBox Runtime: URL, URLSearchParams, Intl

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
            let username = '', password = '';
            const userinfoMatch = url.match(/^([^:]+):\/\/([^:@]+):([^@]+)@(.*)$/);
            if (userinfoMatch) {
                username = userinfoMatch[2];
                password = userinfoMatch[3];
                url = userinfoMatch[1] + '://' + userinfoMatch[4];
            } else {
                const userOnlyMatch = url.match(/^([^:]+):\/\/([^:@]+)@(.*)$/);
                if (userOnlyMatch) {
                    username = userOnlyMatch[2];
                    url = userOnlyMatch[1] + '://' + userOnlyMatch[3];
                }
            }
            const match = url.match(/^([^:]+):\/\/([^/:?#]+)(?::(\d+))?(\/[^?#]*)?(\?[^#]*)?(#.*)?$/);
            if (!match) throw new TypeError('Invalid URL');
            this.protocol = match[1] + ':';
            this.hostname = match[2];
            this.port = match[3] || '';
            this.pathname = match[4] || '/';
            this.search = match[5] || '';
            this.hash = match[6] || '';
            this.username = username;
            this.password = password;
            this.searchParams = new URLSearchParams(this.search);
        }
        get host() { return this.hostname + (this.port ? ':' + this.port : ''); }
        get origin() { return this.protocol + '//' + this.host; }
        get href() {
            const userinfo = this.username ? (this.password ? this.username + ':' + this.password + '@' : this.username + '@') : '';
            return this.protocol + '//' + userinfo + this.host + this.pathname + this.search + this.hash;
        }
        toString() { return this.href; }
        toJSON() { return this.href; }
    };
}

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
