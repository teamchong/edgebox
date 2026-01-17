    // ===== BUFFER CLASS =====
    // Use native Zig helpers from _modules._nativeBuffer when available
    // Native Buffer is in src/polyfills/buffer.zig with zero-allocation implementations
    // ALWAYS override (runtime.js Buffer doesn't have native helpers)
    {
        const _native = _modules._nativeBuffer;
        class Buffer extends Uint8Array {
            static from(data, encoding) {
                // Handle base64 encoding with native helper (811x faster)
                if (typeof data === 'string' && (encoding === 'base64' || encoding === 'base64url')) {
                    if (_native && _native.fromBase64) {
                        const arr = _native.fromBase64(data.replace(/-/g, '+').replace(/_/g, '/'));
                        return Object.setPrototypeOf(arr, Buffer.prototype);
                    }
                    // JS fallback for base64
                    const binary = atob(data.replace(/-/g, '+').replace(/_/g, '/'));
                    const bytes = new Uint8Array(binary.length);
                    for (let i = 0; i < binary.length; i++) bytes[i] = binary.charCodeAt(i);
                    return Object.setPrototypeOf(bytes, Buffer.prototype);
                }
                // Use native helper for strings (fast memcpy)
                if (_native && typeof data === 'string') {
                    const arr = _native.from(data);
                    // Create Buffer view on same backing buffer (no copy)
                    return Object.setPrototypeOf(arr, Buffer.prototype);
                }
                if (typeof data === 'string') return new Buffer(new TextEncoder().encode(data));
                if (data instanceof ArrayBuffer) return new Buffer(new Uint8Array(data));
                if (Array.isArray(data) || data instanceof Uint8Array) return new Buffer(data);
                return new Buffer(0);
            }
            static alloc(size, fill) {
                // Use native helper (fast)
                if (_native && fill === undefined) {
                    const arr = _native.alloc(size);
                    return Object.setPrototypeOf(arr, Buffer.prototype);
                }
                const buf = new Buffer(size);
                if (fill !== undefined) buf.fill(fill);
                return buf;
            }
            static allocUnsafe(size) {
                if (_native) {
                    const arr = _native.allocUnsafe(size);
                    return Object.setPrototypeOf(arr, Buffer.prototype);
                }
                return new Buffer(size);
            }
            static concat(list, totalLength) {
                // Use native helper (fast memcpy)
                if (_native) {
                    const arr = _native.concat(list, totalLength);
                    return Object.setPrototypeOf(arr, Buffer.prototype);
                }
                if (totalLength === undefined) totalLength = list.reduce((sum, buf) => sum + buf.length, 0);
                const result = new Buffer(totalLength);
                let offset = 0;
                for (const buf of list) { result.set(buf, offset); offset += buf.length; }
                return result;
            }
            static isBuffer(obj) { return obj instanceof Buffer || obj instanceof Uint8Array; }
            static byteLength(str) { return new TextEncoder().encode(str).length; }
            toString(encoding, start, end) {
                // Support Node.js Buffer.toString(encoding, start, end) signature
                // Default values: encoding='utf-8', start=0, end=this.length
                const startOffset = (typeof start === 'number') ? start : 0;
                const endOffset = (typeof end === 'number') ? end : this.length;
                const slice = (startOffset > 0 || endOffset < this.length) ? this.slice(startOffset, endOffset) : this;

                // Handle base64 encoding with native helper (811x faster)
                if (encoding === 'base64' || encoding === 'base64url') {
                    if (_native && _native.toBase64) {
                        const result = _native.toBase64(slice);
                        return encoding === 'base64url' ? result.replace(/\+/g, '-').replace(/\//g, '_').replace(/=/g, '') : result;
                    }
                    // JS fallback for base64
                    let binary = '';
                    for (let i = 0; i < slice.length; i++) binary += String.fromCharCode(slice[i]);
                    const result = btoa(binary);
                    return encoding === 'base64url' ? result.replace(/\+/g, '-').replace(/\//g, '_').replace(/=/g, '') : result;
                }
                // Handle utf16le/ucs2 encoding (used by tsc for BOM detection)
                if (encoding === 'utf16le' || encoding === 'ucs2') {
                    let result = '';
                    for (let i = 0; i < slice.length - 1; i += 2) {
                        result += String.fromCharCode(slice[i] | (slice[i + 1] << 8));
                    }
                    return result;
                }
                return new TextDecoder(encoding || 'utf-8').decode(slice);
            }
            write(string, offset, length) {
                offset = offset || 0;
                const encoded = new TextEncoder().encode(string);
                const toWrite = length ? encoded.slice(0, length) : encoded;
                this.set(toWrite, offset);
                return toWrite.length;
            }
            slice(start, end) {
                const sliced = super.slice(start, end);
                return Object.setPrototypeOf(sliced, Buffer.prototype);
            }
            copy(target, targetStart, sourceStart, sourceEnd) {
                const slice = this.slice(sourceStart || 0, sourceEnd || this.length);
                target.set(slice, targetStart || 0);
                return slice.length;
            }
            equals(other) {
                if (this.length !== other.length) return false;
                for (let i = 0; i < this.length; i++) if (this[i] !== other[i]) return false;
                return true;
            }
            compare(other) {
                const len = Math.min(this.length, other.length);
                for (let i = 0; i < len; i++) {
                    if (this[i] < other[i]) return -1;
                    if (this[i] > other[i]) return 1;
                }
                return this.length - other.length;
            }
            // Numeric read methods
            readInt8(offset) { return this[offset] > 127 ? this[offset] - 256 : this[offset]; }
            readUInt8(offset) { return this[offset]; }
            readInt16LE(offset) { const val = this[offset] | (this[offset + 1] << 8); return val > 32767 ? val - 65536 : val; }
            readUInt16LE(offset) { return this[offset] | (this[offset + 1] << 8); }
            readInt16BE(offset) { const val = (this[offset] << 8) | this[offset + 1]; return val > 32767 ? val - 65536 : val; }
            readUInt16BE(offset) { return (this[offset] << 8) | this[offset + 1]; }
            readInt32LE(offset) { return this[offset] | (this[offset + 1] << 8) | (this[offset + 2] << 16) | (this[offset + 3] << 24); }
            readUInt32LE(offset) { return (this[offset] | (this[offset + 1] << 8) | (this[offset + 2] << 16) | (this[offset + 3] << 24)) >>> 0; }
            readInt32BE(offset) { return (this[offset] << 24) | (this[offset + 1] << 16) | (this[offset + 2] << 8) | this[offset + 3]; }
            readUInt32BE(offset) { return ((this[offset] << 24) | (this[offset + 1] << 16) | (this[offset + 2] << 8) | this[offset + 3]) >>> 0; }
            readFloatLE(offset) { const view = new DataView(this.buffer, this.byteOffset + offset, 4); return view.getFloat32(0, true); }
            readFloatBE(offset) { const view = new DataView(this.buffer, this.byteOffset + offset, 4); return view.getFloat32(0, false); }
            readDoubleLE(offset) { const view = new DataView(this.buffer, this.byteOffset + offset, 8); return view.getFloat64(0, true); }
            readDoubleBE(offset) { const view = new DataView(this.buffer, this.byteOffset + offset, 8); return view.getFloat64(0, false); }
            readBigInt64LE(offset) { const view = new DataView(this.buffer, this.byteOffset + offset, 8); return view.getBigInt64(0, true); }
            readBigInt64BE(offset) { const view = new DataView(this.buffer, this.byteOffset + offset, 8); return view.getBigInt64(0, false); }
            readBigUInt64LE(offset) { const view = new DataView(this.buffer, this.byteOffset + offset, 8); return view.getBigUint64(0, true); }
            readBigUInt64BE(offset) { const view = new DataView(this.buffer, this.byteOffset + offset, 8); return view.getBigUint64(0, false); }
            // Variable-length read methods
            readIntLE(offset, byteLength) { let val = 0; for (let i = 0; i < byteLength; i++) val |= this[offset + i] << (8 * i); const max = 1 << (8 * byteLength - 1); return val >= max ? val - (1 << (8 * byteLength)) : val; }
            readUIntLE(offset, byteLength) { let val = 0; for (let i = 0; i < byteLength; i++) val |= this[offset + i] << (8 * i); return val >>> 0; }
            readIntBE(offset, byteLength) { let val = 0; for (let i = 0; i < byteLength; i++) val = (val << 8) | this[offset + i]; const max = 1 << (8 * byteLength - 1); return val >= max ? val - (1 << (8 * byteLength)) : val; }
            readUIntBE(offset, byteLength) { let val = 0; for (let i = 0; i < byteLength; i++) val = (val << 8) | this[offset + i]; return val >>> 0; }
            // Numeric write methods
            writeInt8(value, offset) { this[offset] = value < 0 ? value + 256 : value; return offset + 1; }
            writeUInt8(value, offset) { this[offset] = value & 0xff; return offset + 1; }
            writeInt16LE(value, offset) { this[offset] = value & 0xff; this[offset + 1] = (value >> 8) & 0xff; return offset + 2; }
            writeUInt16LE(value, offset) { this[offset] = value & 0xff; this[offset + 1] = (value >> 8) & 0xff; return offset + 2; }
            writeInt16BE(value, offset) { this[offset] = (value >> 8) & 0xff; this[offset + 1] = value & 0xff; return offset + 2; }
            writeUInt16BE(value, offset) { this[offset] = (value >> 8) & 0xff; this[offset + 1] = value & 0xff; return offset + 2; }
            writeInt32LE(value, offset) { this[offset] = value & 0xff; this[offset + 1] = (value >> 8) & 0xff; this[offset + 2] = (value >> 16) & 0xff; this[offset + 3] = (value >> 24) & 0xff; return offset + 4; }
            writeUInt32LE(value, offset) { this[offset] = value & 0xff; this[offset + 1] = (value >> 8) & 0xff; this[offset + 2] = (value >> 16) & 0xff; this[offset + 3] = (value >>> 24) & 0xff; return offset + 4; }
            writeInt32BE(value, offset) { this[offset] = (value >> 24) & 0xff; this[offset + 1] = (value >> 16) & 0xff; this[offset + 2] = (value >> 8) & 0xff; this[offset + 3] = value & 0xff; return offset + 4; }
            writeUInt32BE(value, offset) { this[offset] = (value >>> 24) & 0xff; this[offset + 1] = (value >> 16) & 0xff; this[offset + 2] = (value >> 8) & 0xff; this[offset + 3] = value & 0xff; return offset + 4; }
            writeFloatLE(value, offset) { const view = new DataView(this.buffer, this.byteOffset + offset, 4); view.setFloat32(0, value, true); return offset + 4; }
            writeFloatBE(value, offset) { const view = new DataView(this.buffer, this.byteOffset + offset, 4); view.setFloat32(0, value, false); return offset + 4; }
            writeDoubleLE(value, offset) { const view = new DataView(this.buffer, this.byteOffset + offset, 8); view.setFloat64(0, value, true); return offset + 8; }
            writeDoubleBE(value, offset) { const view = new DataView(this.buffer, this.byteOffset + offset, 8); view.setFloat64(0, value, false); return offset + 8; }
            writeBigInt64LE(value, offset) { const view = new DataView(this.buffer, this.byteOffset + offset, 8); view.setBigInt64(0, value, true); return offset + 8; }
            writeBigInt64BE(value, offset) { const view = new DataView(this.buffer, this.byteOffset + offset, 8); view.setBigInt64(0, value, false); return offset + 8; }
            writeBigUInt64LE(value, offset) { const view = new DataView(this.buffer, this.byteOffset + offset, 8); view.setBigUint64(0, value, true); return offset + 8; }
            writeBigUInt64BE(value, offset) { const view = new DataView(this.buffer, this.byteOffset + offset, 8); view.setBigUint64(0, value, false); return offset + 8; }
            // Search methods
            indexOf(value, byteOffset) {
                if (typeof value === 'string') value = Buffer.from(value);
                if (typeof value === 'number') { for (let i = byteOffset || 0; i < this.length; i++) if (this[i] === value) return i; return -1; }
                outer: for (let i = byteOffset || 0; i <= this.length - value.length; i++) { for (let j = 0; j < value.length; j++) if (this[i + j] !== value[j]) continue outer; return i; }
                return -1;
            }
            lastIndexOf(value, byteOffset) {
                if (typeof value === 'string') value = Buffer.from(value);
                if (typeof value === 'number') { for (let i = Math.min(byteOffset !== undefined ? byteOffset : this.length - 1, this.length - 1); i >= 0; i--) if (this[i] === value) return i; return -1; }
                const start = Math.min(byteOffset !== undefined ? byteOffset : this.length - value.length, this.length - value.length);
                outer: for (let i = start; i >= 0; i--) { for (let j = 0; j < value.length; j++) if (this[i + j] !== value[j]) continue outer; return i; }
                return -1;
            }
            includes(value, byteOffset) { return this.indexOf(value, byteOffset) !== -1; }
            // Swap methods
            swap16() { for (let i = 0; i < this.length; i += 2) { const tmp = this[i]; this[i] = this[i + 1]; this[i + 1] = tmp; } return this; }
            swap32() { for (let i = 0; i < this.length; i += 4) { const t0 = this[i], t1 = this[i + 1]; this[i] = this[i + 3]; this[i + 1] = this[i + 2]; this[i + 2] = t1; this[i + 3] = t0; } return this; }
            swap64() { for (let i = 0; i < this.length; i += 8) { const t = [this[i], this[i+1], this[i+2], this[i+3]]; this[i] = this[i+7]; this[i+1] = this[i+6]; this[i+2] = this[i+5]; this[i+3] = this[i+4]; this[i+4] = t[3]; this[i+5] = t[2]; this[i+6] = t[1]; this[i+7] = t[0]; } return this; }
            // Other methods
            toJSON() { return { type: 'Buffer', data: Array.from(this) }; }
            reverse() { return Object.setPrototypeOf(super.reverse(), Buffer.prototype); }
        }
        _modules.buffer = { Buffer };
        globalThis.Buffer = Buffer;
    }

