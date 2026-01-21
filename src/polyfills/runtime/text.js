// EdgeBox Runtime: TextEncoder/TextDecoder

var _needTextEncoderPolyfill = typeof TextEncoder === 'undefined';
if (!_needTextEncoderPolyfill) {
    try { new TextEncoder(); } catch(e) { _needTextEncoderPolyfill = true; }
}
if (_needTextEncoderPolyfill) {
    const _nativeFromUtf8 = typeof _modules !== 'undefined' && _modules._nativeBuffer && _modules._nativeBuffer.fromUtf8String;
    globalThis.TextEncoder = class TextEncoder {
        constructor(encoding = 'utf-8') { this.encoding = encoding; }
        encode(str) {
            if (_nativeFromUtf8) return _nativeFromUtf8(str);
            const bytes = [];
            for (let i = 0; i < str.length; i++) {
                let code = str.charCodeAt(i);
                if (code < 0x80) bytes.push(code);
                else if (code < 0x800) { bytes.push(0xC0 | (code >> 6)); bytes.push(0x80 | (code & 0x3F)); }
                else if (code >= 0xD800 && code <= 0xDBFF && i + 1 < str.length) {
                    const next = str.charCodeAt(++i);
                    if (next >= 0xDC00 && next <= 0xDFFF) {
                        code = ((code - 0xD800) << 10) + (next - 0xDC00) + 0x10000;
                        bytes.push(0xF0 | (code >> 18)); bytes.push(0x80 | ((code >> 12) & 0x3F));
                        bytes.push(0x80 | ((code >> 6) & 0x3F)); bytes.push(0x80 | (code & 0x3F));
                    }
                } else { bytes.push(0xE0 | (code >> 12)); bytes.push(0x80 | ((code >> 6) & 0x3F)); bytes.push(0x80 | (code & 0x3F)); }
            }
            return new Uint8Array(bytes);
        }
    };
}

var _needTextDecoderPolyfill = typeof TextDecoder === 'undefined';
if (!_needTextDecoderPolyfill) {
    try { new TextDecoder(); } catch(e) { _needTextDecoderPolyfill = true; }
}
if (_needTextDecoderPolyfill) {
    const _nativeToUtf8 = typeof _modules !== 'undefined' && _modules._nativeBuffer && _modules._nativeBuffer.toUtf8String;
    globalThis.TextDecoder = class TextDecoder {
        constructor(encoding = 'utf-8') { this.encoding = encoding; }
        decode(input) {
            const bytes = input instanceof Uint8Array ? input : new Uint8Array(input);
            if (_nativeToUtf8) return _nativeToUtf8(bytes);
            let str = '';
            for (let i = 0; i < bytes.length; ) {
                const b = bytes[i++];
                if (b < 0x80) str += String.fromCharCode(b);
                else if (b < 0xE0) str += String.fromCharCode(((b & 0x1F) << 6) | (bytes[i++] & 0x3F));
                else if (b < 0xF0) str += String.fromCharCode(((b & 0x0F) << 12) | ((bytes[i++] & 0x3F) << 6) | (bytes[i++] & 0x3F));
                else {
                    const code = ((b & 0x07) << 18) | ((bytes[i++] & 0x3F) << 12) | ((bytes[i++] & 0x3F) << 6) | (bytes[i++] & 0x3F);
                    if (code > 0xFFFF) { const offset = code - 0x10000; str += String.fromCharCode(0xD800 + (offset >> 10), 0xDC00 + (offset & 0x3FF)); }
                    else str += String.fromCharCode(code);
                }
            }
            return str;
        }
    };
}
