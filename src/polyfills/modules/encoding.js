    // ===== TextEncoder/TextDecoder POLYFILL (must be before util module) =====
    if (typeof globalThis.TextEncoder === 'undefined') {
        globalThis.TextEncoder = class TextEncoder {
            constructor(encoding = 'utf-8') { this.encoding = encoding; }
            encode(str) {
                const bytes = [];
                for (let i = 0; i < str.length; i++) {
                    let c = str.charCodeAt(i);
                    if (c < 0x80) bytes.push(c);
                    else if (c < 0x800) bytes.push(0xc0 | (c >> 6), 0x80 | (c & 0x3f));
                    else if (c < 0x10000) bytes.push(0xe0 | (c >> 12), 0x80 | ((c >> 6) & 0x3f), 0x80 | (c & 0x3f));
                    else bytes.push(0xf0 | (c >> 18), 0x80 | ((c >> 12) & 0x3f), 0x80 | ((c >> 6) & 0x3f), 0x80 | (c & 0x3f));
                }
                return new Uint8Array(bytes);
            }
            encodeInto(str, u8arr) {
                const encoded = this.encode(str);
                const len = Math.min(encoded.length, u8arr.length);
                u8arr.set(encoded.slice(0, len));
                return { read: str.length, written: len };
            }
        };
    }
    if (typeof globalThis.TextDecoder === 'undefined') {
        globalThis.TextDecoder = class TextDecoder {
            constructor(encoding = 'utf-8') { this.encoding = encoding; }
            decode(buffer) {
                const bytes = buffer instanceof Uint8Array ? buffer : new Uint8Array(buffer);
                let result = '', i = 0;
                while (i < bytes.length) {
                    const b = bytes[i++];
                    if (b < 0x80) result += String.fromCharCode(b);
                    else if ((b & 0xe0) === 0xc0) result += String.fromCharCode(((b & 0x1f) << 6) | (bytes[i++] & 0x3f));
                    else if ((b & 0xf0) === 0xe0) result += String.fromCharCode(((b & 0x0f) << 12) | ((bytes[i++] & 0x3f) << 6) | (bytes[i++] & 0x3f));
                    else if ((b & 0xf8) === 0xf0) { i += 3; result += '?'; }
                }
                return result;
            }
        };
    }

