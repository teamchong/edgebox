// EdgeBox Runtime: Base64 (atob, btoa, DOMException)

// DOMException polyfill
if (typeof globalThis.DOMException === 'undefined') {
    globalThis.DOMException = class DOMException extends Error {
        constructor(message, name) {
            super(message);
            this.name = name || 'DOMException';
        }
    };
}

// atob/btoa polyfills for base64 encoding/decoding
if (typeof globalThis.atob === 'undefined') {
    const base64chars = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/';
    const base64lookup = new Uint8Array(256);
    for (let i = 0; i < base64chars.length; i++) {
        base64lookup[base64chars.charCodeAt(i)] = i;
    }

    globalThis.atob = function(str) {
        str = String(str).replace(/[\t\n\f\r ]/g, '');
        if (str.length % 4 === 1) throw new DOMException('Invalid base64 string', 'InvalidCharacterError');

        let output = '';
        let buffer = 0;
        let bits = 0;

        for (let i = 0; i < str.length; i++) {
            const c = str.charCodeAt(i);
            if (c === 61) break; // '=' padding
            const val = base64lookup[c];
            if (val === undefined) continue;
            buffer = (buffer << 6) | val;
            bits += 6;
            if (bits >= 8) {
                bits -= 8;
                output += String.fromCharCode((buffer >> bits) & 0xFF);
            }
        }
        return output;
    };

    globalThis.btoa = function(str) {
        str = String(str);
        let output = '';
        for (let i = 0; i < str.length; i += 3) {
            const a = str.charCodeAt(i);
            const b = i + 1 < str.length ? str.charCodeAt(i + 1) : 0;
            const c = i + 2 < str.length ? str.charCodeAt(i + 2) : 0;

            output += base64chars[a >> 2];
            output += base64chars[((a & 3) << 4) | (b >> 4)];
            output += i + 1 < str.length ? base64chars[((b & 15) << 2) | (c >> 6)] : '=';
            output += i + 2 < str.length ? base64chars[c & 63] : '=';
        }
        return output;
    };
}
