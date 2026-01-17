    // ===== BUFFER CLASS =====
    // Zig native: src/polyfills/buffer.zig (_modules._nativeBuffer)
    // JS: thin wrapper class that delegates to Zig for performance-critical ops
    {
        const _native = _modules._nativeBuffer;
        class Buffer extends Uint8Array {
            // Static methods - delegate to Zig
            static from(data, encoding) {
                if (typeof data === 'string') {
                    if (encoding === 'base64' || encoding === 'base64url') {
                        if (_native?.fromBase64) {
                            const arr = _native.fromBase64(data.replace(/-/g, '+').replace(/_/g, '/'));
                            return Object.setPrototypeOf(arr, Buffer.prototype);
                        }
                    }
                    if (_native?.from) {
                        return Object.setPrototypeOf(_native.from(data), Buffer.prototype);
                    }
                    return new Buffer(new TextEncoder().encode(data));
                }
                if (data instanceof ArrayBuffer) return new Buffer(new Uint8Array(data));
                if (Array.isArray(data) || data instanceof Uint8Array) return new Buffer(data);
                return new Buffer(0);
            }
            static alloc(size, fill) {
                if (_native?.alloc && fill === undefined) {
                    return Object.setPrototypeOf(_native.alloc(size), Buffer.prototype);
                }
                const buf = new Buffer(size);
                if (fill !== undefined) buf.fill(fill);
                return buf;
            }
            static allocUnsafe(size) {
                if (_native?.allocUnsafe) {
                    return Object.setPrototypeOf(_native.allocUnsafe(size), Buffer.prototype);
                }
                return new Buffer(size);
            }
            static concat(list, totalLength) {
                if (_native?.concat) {
                    return Object.setPrototypeOf(_native.concat(list, totalLength), Buffer.prototype);
                }
                if (totalLength === undefined) totalLength = list.reduce((sum, buf) => sum + buf.length, 0);
                const result = new Buffer(totalLength);
                let offset = 0;
                for (const buf of list) { result.set(buf, offset); offset += buf.length; }
                return result;
            }
            static isBuffer(obj) { return obj instanceof Buffer || (_native?.isBuffer?.(obj) ?? obj instanceof Uint8Array); }
            static byteLength(str) { return new TextEncoder().encode(str).length; }

            // Instance methods - delegate to Zig where possible
            toString(encoding, start, end) {
                const startOffset = (typeof start === 'number') ? start : 0;
                const endOffset = (typeof end === 'number') ? end : this.length;
                const slice = (startOffset > 0 || endOffset < this.length) ? this.slice(startOffset, endOffset) : this;
                if (encoding === 'base64' || encoding === 'base64url') {
                    if (_native?.toBase64) {
                        const result = _native.toBase64(slice);
                        return encoding === 'base64url' ? result.replace(/\+/g, '-').replace(/\//g, '_').replace(/=/g, '') : result;
                    }
                }
                if (encoding === 'hex' && _native?.toHex) return _native.toHex(slice);
                if (encoding === 'utf16le' || encoding === 'ucs2') {
                    let result = '';
                    for (let i = 0; i < slice.length - 1; i += 2) result += String.fromCharCode(slice[i] | (slice[i + 1] << 8));
                    return result;
                }
                if (_native?.toUtf8String && (!encoding || encoding === 'utf-8' || encoding === 'utf8')) {
                    return _native.toUtf8String(slice);
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
            slice(start, end) { return Object.setPrototypeOf(super.slice(start, end), Buffer.prototype); }
            subarray(start, end) {
                if (_native?.subarray) return Object.setPrototypeOf(_native.subarray(this, start, end), Buffer.prototype);
                return Object.setPrototypeOf(super.subarray(start, end), Buffer.prototype);
            }
            copy(target, targetStart, sourceStart, sourceEnd) {
                if (_native?.copy) return _native.copy(this, target, targetStart || 0, sourceStart || 0, sourceEnd || this.length);
                const slice = this.slice(sourceStart || 0, sourceEnd || this.length);
                target.set(slice, targetStart || 0);
                return slice.length;
            }
            equals(other) {
                if (_native?.equals) return _native.equals(this, other);
                if (this.length !== other.length) return false;
                for (let i = 0; i < this.length; i++) if (this[i] !== other[i]) return false;
                return true;
            }
            compare(other) {
                if (_native?.compare) return _native.compare(this, other);
                const len = Math.min(this.length, other.length);
                for (let i = 0; i < len; i++) {
                    if (this[i] < other[i]) return -1;
                    if (this[i] > other[i]) return 1;
                }
                return this.length - other.length;
            }
            indexOf(value, byteOffset) {
                if (_native?.indexOf) return _native.indexOf(this, value, byteOffset || 0);
                if (typeof value === 'string') value = Buffer.from(value);
                if (typeof value === 'number') { for (let i = byteOffset || 0; i < this.length; i++) if (this[i] === value) return i; return -1; }
                outer: for (let i = byteOffset || 0; i <= this.length - value.length; i++) { for (let j = 0; j < value.length; j++) if (this[i + j] !== value[j]) continue outer; return i; }
                return -1;
            }
            lastIndexOf(value, byteOffset) {
                if (_native?.lastIndexOf) return _native.lastIndexOf(this, value, byteOffset ?? this.length - 1);
                if (typeof value === 'string') value = Buffer.from(value);
                if (typeof value === 'number') { for (let i = Math.min(byteOffset ?? this.length - 1, this.length - 1); i >= 0; i--) if (this[i] === value) return i; return -1; }
                const start = Math.min(byteOffset ?? this.length - value.length, this.length - value.length);
                outer: for (let i = start; i >= 0; i--) { for (let j = 0; j < value.length; j++) if (this[i + j] !== value[j]) continue outer; return i; }
                return -1;
            }
            includes(value, byteOffset) { return this.indexOf(value, byteOffset) !== -1; }

            // Numeric read methods - use DataView for guaranteed correctness
            readInt8(offset) { return this[offset] > 127 ? this[offset] - 256 : this[offset]; }
            readUInt8(offset) { return this[offset]; }
            readInt16LE(offset) { const v = new DataView(this.buffer, this.byteOffset + offset, 2); return v.getInt16(0, true); }
            readUInt16LE(offset) { const v = new DataView(this.buffer, this.byteOffset + offset, 2); return v.getUint16(0, true); }
            readInt16BE(offset) { const v = new DataView(this.buffer, this.byteOffset + offset, 2); return v.getInt16(0, false); }
            readUInt16BE(offset) { const v = new DataView(this.buffer, this.byteOffset + offset, 2); return v.getUint16(0, false); }
            readInt32LE(offset) { const v = new DataView(this.buffer, this.byteOffset + offset, 4); return v.getInt32(0, true); }
            readUInt32LE(offset) { const v = new DataView(this.buffer, this.byteOffset + offset, 4); return v.getUint32(0, true); }
            readInt32BE(offset) { const v = new DataView(this.buffer, this.byteOffset + offset, 4); return v.getInt32(0, false); }
            readUInt32BE(offset) { const v = new DataView(this.buffer, this.byteOffset + offset, 4); return v.getUint32(0, false); }
            readFloatLE(offset) { const v = new DataView(this.buffer, this.byteOffset + offset, 4); return v.getFloat32(0, true); }
            readFloatBE(offset) { const v = new DataView(this.buffer, this.byteOffset + offset, 4); return v.getFloat32(0, false); }
            readDoubleLE(offset) { const v = new DataView(this.buffer, this.byteOffset + offset, 8); return v.getFloat64(0, true); }
            readDoubleBE(offset) { const v = new DataView(this.buffer, this.byteOffset + offset, 8); return v.getFloat64(0, false); }
            readBigInt64LE(offset) { const v = new DataView(this.buffer, this.byteOffset + offset, 8); return v.getBigInt64(0, true); }
            readBigInt64BE(offset) { const v = new DataView(this.buffer, this.byteOffset + offset, 8); return v.getBigInt64(0, false); }
            readBigUInt64LE(offset) { const v = new DataView(this.buffer, this.byteOffset + offset, 8); return v.getBigUint64(0, true); }
            readBigUInt64BE(offset) { const v = new DataView(this.buffer, this.byteOffset + offset, 8); return v.getBigUint64(0, false); }

            // Numeric write methods
            writeInt8(value, offset) { this[offset] = value < 0 ? value + 256 : value; return offset + 1; }
            writeUInt8(value, offset) { this[offset] = value & 0xff; return offset + 1; }
            writeInt16LE(value, offset) { const v = new DataView(this.buffer, this.byteOffset + offset, 2); v.setInt16(0, value, true); return offset + 2; }
            writeUInt16LE(value, offset) { const v = new DataView(this.buffer, this.byteOffset + offset, 2); v.setUint16(0, value, true); return offset + 2; }
            writeInt16BE(value, offset) { const v = new DataView(this.buffer, this.byteOffset + offset, 2); v.setInt16(0, value, false); return offset + 2; }
            writeUInt16BE(value, offset) { const v = new DataView(this.buffer, this.byteOffset + offset, 2); v.setUint16(0, value, false); return offset + 2; }
            writeInt32LE(value, offset) { const v = new DataView(this.buffer, this.byteOffset + offset, 4); v.setInt32(0, value, true); return offset + 4; }
            writeUInt32LE(value, offset) { const v = new DataView(this.buffer, this.byteOffset + offset, 4); v.setUint32(0, value, true); return offset + 4; }
            writeInt32BE(value, offset) { const v = new DataView(this.buffer, this.byteOffset + offset, 4); v.setInt32(0, value, false); return offset + 4; }
            writeUInt32BE(value, offset) { const v = new DataView(this.buffer, this.byteOffset + offset, 4); v.setUint32(0, value, false); return offset + 4; }
            writeFloatLE(value, offset) { const v = new DataView(this.buffer, this.byteOffset + offset, 4); v.setFloat32(0, value, true); return offset + 4; }
            writeFloatBE(value, offset) { const v = new DataView(this.buffer, this.byteOffset + offset, 4); v.setFloat32(0, value, false); return offset + 4; }
            writeDoubleLE(value, offset) { const v = new DataView(this.buffer, this.byteOffset + offset, 8); v.setFloat64(0, value, true); return offset + 8; }
            writeDoubleBE(value, offset) { const v = new DataView(this.buffer, this.byteOffset + offset, 8); v.setFloat64(0, value, false); return offset + 8; }
            writeBigInt64LE(value, offset) { const v = new DataView(this.buffer, this.byteOffset + offset, 8); v.setBigInt64(0, value, true); return offset + 8; }
            writeBigInt64BE(value, offset) { const v = new DataView(this.buffer, this.byteOffset + offset, 8); v.setBigInt64(0, value, false); return offset + 8; }
            writeBigUInt64LE(value, offset) { const v = new DataView(this.buffer, this.byteOffset + offset, 8); v.setBigUint64(0, value, true); return offset + 8; }
            writeBigUInt64BE(value, offset) { const v = new DataView(this.buffer, this.byteOffset + offset, 8); v.setBigUint64(0, value, false); return offset + 8; }

            // Swap/other methods
            swap16() { for (let i = 0; i < this.length; i += 2) { const t = this[i]; this[i] = this[i + 1]; this[i + 1] = t; } return this; }
            swap32() { for (let i = 0; i < this.length; i += 4) { const t0 = this[i], t1 = this[i + 1]; this[i] = this[i + 3]; this[i + 1] = this[i + 2]; this[i + 2] = t1; this[i + 3] = t0; } return this; }
            swap64() { for (let i = 0; i < this.length; i += 8) { const t = [this[i], this[i+1], this[i+2], this[i+3]]; this[i] = this[i+7]; this[i+1] = this[i+6]; this[i+2] = this[i+5]; this[i+3] = this[i+4]; this[i+4] = t[3]; this[i+5] = t[2]; this[i+6] = t[1]; this[i+7] = t[0]; } return this; }
            toJSON() { return { type: 'Buffer', data: Array.from(this) }; }
            reverse() { return Object.setPrototypeOf(super.reverse(), Buffer.prototype); }
        }
        _modules.buffer = { Buffer };
        _modules['node:buffer'] = _modules.buffer;
        globalThis.Buffer = Buffer;
    }

