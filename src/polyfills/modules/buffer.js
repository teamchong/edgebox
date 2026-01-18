    // ===== BUFFER CLASS =====
    // Zig native: src/polyfills/buffer.zig (_modules._nativeBuffer)
    // JS: thin wrapper class that delegates to Zig for performance-critical ops
    {
        const _native = _modules._nativeBuffer;
        class Buffer extends Uint8Array {
            // Static methods - delegate to Zig
            static from(data, encodingOrOffset, length) {
                if (typeof data === 'string') {
                    const encoding = encodingOrOffset;
                    if (encoding === 'base64' || encoding === 'base64url') {
                        if (_native?.fromBase64) {
                            // Convert base64url to standard base64 and add padding
                            let b64 = data.replace(/-/g, '+').replace(/_/g, '/');
                            const pad = b64.length % 4;
                            if (pad) b64 += '='.repeat(4 - pad);
                            const arr = _native.fromBase64(b64);
                            return Object.setPrototypeOf(arr, Buffer.prototype);
                        }
                    }
                    if (encoding === 'hex') {
                        const bytes = new Uint8Array(data.length / 2);
                        for (let i = 0; i < data.length; i += 2) {
                            bytes[i / 2] = parseInt(data.substring(i, i + 2), 16);
                        }
                        return Object.setPrototypeOf(bytes, Buffer.prototype);
                    }
                    if (_native?.from) {
                        return Object.setPrototypeOf(_native.from(data), Buffer.prototype);
                    }
                    return new Buffer(new TextEncoder().encode(data));
                }
                // ArrayBuffer with optional offset and length
                if (data instanceof ArrayBuffer) {
                    const offset = typeof encodingOrOffset === 'number' ? encodingOrOffset : 0;
                    const len = typeof length === 'number' ? length : data.byteLength - offset;
                    return new Buffer(new Uint8Array(data, offset, len));
                }
                if (Array.isArray(data) || data instanceof Uint8Array) return new Buffer(data);
                return new Buffer(0);
            }
            static alloc(size, fill, encoding) {
                if (_native?.alloc && fill === undefined) {
                    return Object.setPrototypeOf(_native.alloc(size), Buffer.prototype);
                }
                const buf = new Buffer(size);
                if (fill !== undefined) {
                    if (typeof fill === 'string' && encoding) {
                        buf.fill(Buffer.from(fill, encoding));
                    } else {
                        buf.fill(fill);
                    }
                }
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
            static isEncoding(encoding) {
                if (!encoding) return false;
                const enc = encoding.toLowerCase();
                return ['utf8', 'utf-8', 'hex', 'base64', 'base64url', 'latin1', 'binary', 'ascii', 'utf16le', 'ucs2', 'ucs-2'].includes(enc);
            }
            static compare(a, b) { return a.compare(b); }
            static transcode(source, fromEnc, toEnc) {
                if (_native?.transcode) {
                    const result = _native.transcode(source, fromEnc, toEnc);
                    return Object.setPrototypeOf(new Uint8Array(result), Buffer.prototype);
                }
                throw new Error('Buffer.transcode not available');
            }
            static of(...items) { return Buffer.from(items); }
            static isAscii(input) {
                const buf = Buffer.isBuffer(input) ? input : Buffer.from(input);
                for (let i = 0; i < buf.length; i++) {
                    if (buf[i] > 127) return false;
                }
                return true;
            }
            static isUtf8(input) {
                const buf = Buffer.isBuffer(input) ? input : Buffer.from(input);
                let i = 0;
                while (i < buf.length) {
                    const b = buf[i];
                    if (b <= 0x7F) { i++; continue; }  // ASCII
                    if ((b & 0xE0) === 0xC0) {  // 2-byte: 110xxxxx 10xxxxxx
                        if (i + 1 >= buf.length || (buf[i + 1] & 0xC0) !== 0x80) return false;
                        if (b < 0xC2) return false;  // Overlong encoding
                        i += 2; continue;
                    }
                    if ((b & 0xF0) === 0xE0) {  // 3-byte: 1110xxxx 10xxxxxx 10xxxxxx
                        if (i + 2 >= buf.length || (buf[i + 1] & 0xC0) !== 0x80 || (buf[i + 2] & 0xC0) !== 0x80) return false;
                        if (b === 0xE0 && buf[i + 1] < 0xA0) return false;  // Overlong
                        i += 3; continue;
                    }
                    if ((b & 0xF8) === 0xF0) {  // 4-byte: 11110xxx 10xxxxxx 10xxxxxx 10xxxxxx
                        if (i + 3 >= buf.length || (buf[i + 1] & 0xC0) !== 0x80 || (buf[i + 2] & 0xC0) !== 0x80 || (buf[i + 3] & 0xC0) !== 0x80) return false;
                        if (b === 0xF0 && buf[i + 1] < 0x90) return false;  // Overlong
                        if (b > 0xF4) return false;  // Beyond Unicode range
                        i += 4; continue;
                    }
                    return false;  // Invalid leading byte
                }
                return true;
            }

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
            write(string, offset, length, encoding) {
                // Detect argument pattern - Node.js supports multiple signatures:
                // write(string[, offset[, length]][, encoding])
                if (typeof offset === 'string') { encoding = offset; offset = 0; length = undefined; }
                else if (typeof length === 'string') { encoding = length; length = undefined; }
                offset = offset ?? 0;
                encoding = encoding || 'utf8';
                let encoded;
                if (!encoding || encoding === 'utf8' || encoding === 'utf-8') {
                    encoded = new TextEncoder().encode(string);
                } else if (encoding === 'hex') {
                    encoded = new Uint8Array(string.length / 2);
                    for (let i = 0; i < string.length; i += 2) encoded[i / 2] = parseInt(string.substring(i, i + 2), 16);
                } else if (encoding === 'base64' || encoding === 'base64url') {
                    let b64 = encoding === 'base64url' ? string.replace(/-/g, '+').replace(/_/g, '/') : string;
                    const pad = b64.length % 4; if (pad) b64 += '='.repeat(4 - pad);
                    encoded = Uint8Array.from(atob(b64), c => c.charCodeAt(0));
                } else if (encoding === 'utf16le' || encoding === 'ucs2') {
                    encoded = new Uint8Array(string.length * 2);
                    for (let i = 0; i < string.length; i++) { const c = string.charCodeAt(i); encoded[i*2] = c & 0xFF; encoded[i*2+1] = c >> 8; }
                } else if (encoding === 'latin1' || encoding === 'binary') {
                    encoded = Uint8Array.from(string, c => c.charCodeAt(0) & 0xFF);
                } else if (encoding === 'ascii') {
                    encoded = Uint8Array.from(string, c => c.charCodeAt(0) & 0x7F);
                } else {
                    encoded = new TextEncoder().encode(string);
                }
                const maxLen = this.length - offset;
                const toWrite = length !== undefined ? encoded.slice(0, Math.min(length, maxLen)) : encoded.slice(0, maxLen);
                this.set(toWrite, offset);
                return toWrite.length;
            }
            slice(start, end) { return Object.setPrototypeOf(super.slice(start, end), Buffer.prototype); }
            subarray(start, end) {
                if (_native?.subarray) return Object.setPrototypeOf(_native.subarray(this, start, end), Buffer.prototype);
                return Object.setPrototypeOf(super.subarray(start, end), Buffer.prototype);
            }
            copy(target, targetStart, sourceStart, sourceEnd) {
                // Use ?? to properly handle 0 values (|| would treat 0 as falsy)
                targetStart = targetStart ?? 0;
                sourceStart = sourceStart ?? 0;
                sourceEnd = sourceEnd ?? this.length;
                if (_native?.copy) return _native.copy(this, target, targetStart, sourceStart, sourceEnd);
                const slice = this.slice(sourceStart, sourceEnd);
                target.set(slice, targetStart);
                return slice.length;
            }
            equals(other) {
                if (_native?.equals) return _native.equals(this, other);
                if (this.length !== other.length) return false;
                for (let i = 0; i < this.length; i++) if (this[i] !== other[i]) return false;
                return true;
            }
            compare(target, targetStart, targetEnd, sourceStart, sourceEnd) {
                // Node.js signature: compare(target[, targetStart[, targetEnd[, sourceStart[, sourceEnd]]]])
                targetStart = targetStart ?? 0;
                targetEnd = targetEnd ?? target.length;
                sourceStart = sourceStart ?? 0;
                sourceEnd = sourceEnd ?? this.length;
                const sourceSlice = sourceStart === 0 && sourceEnd === this.length ? this : this.slice(sourceStart, sourceEnd);
                const targetSlice = targetStart === 0 && targetEnd === target.length ? target : target.slice(targetStart, targetEnd);
                if (_native?.compare) return _native.compare(sourceSlice, targetSlice);
                const len = Math.min(sourceSlice.length, targetSlice.length);
                for (let i = 0; i < len; i++) {
                    if (sourceSlice[i] < targetSlice[i]) return -1;
                    if (sourceSlice[i] > targetSlice[i]) return 1;
                }
                if (sourceSlice.length < targetSlice.length) return -1;
                if (sourceSlice.length > targetSlice.length) return 1;
                return 0;
            }
            indexOf(value, byteOffset, encoding) {
                // Node.js signature: indexOf(value[, byteOffset][, encoding])
                if (typeof byteOffset === 'string') { encoding = byteOffset; byteOffset = 0; }
                byteOffset = byteOffset ?? 0;
                // Handle negative offset
                if (byteOffset < 0) byteOffset = Math.max(0, this.length + byteOffset);
                // Convert string with encoding
                if (typeof value === 'string') value = Buffer.from(value, encoding || 'utf8');
                if (_native?.indexOf && !(typeof value === 'number')) return _native.indexOf(this, value, byteOffset);
                if (typeof value === 'number') { for (let i = byteOffset; i < this.length; i++) if (this[i] === value) return i; return -1; }
                outer: for (let i = byteOffset; i <= this.length - value.length; i++) { for (let j = 0; j < value.length; j++) if (this[i + j] !== value[j]) continue outer; return i; }
                return -1;
            }
            lastIndexOf(value, byteOffset, encoding) {
                // Node.js signature: lastIndexOf(value[, byteOffset][, encoding])
                if (typeof byteOffset === 'string') { encoding = byteOffset; byteOffset = this.length; }
                byteOffset = byteOffset ?? this.length;
                // Handle negative offset
                if (byteOffset < 0) byteOffset = Math.max(0, this.length + byteOffset);
                // Convert string with encoding
                if (typeof value === 'string') value = Buffer.from(value, encoding || 'utf8');
                if (_native?.lastIndexOf && !(typeof value === 'number')) return _native.lastIndexOf(this, value, byteOffset);
                if (typeof value === 'number') { for (let i = Math.min(byteOffset, this.length - 1); i >= 0; i--) if (this[i] === value) return i; return -1; }
                const start = Math.min(byteOffset, this.length - value.length);
                outer: for (let i = start; i >= 0; i--) { for (let j = 0; j < value.length; j++) if (this[i + j] !== value[j]) continue outer; return i; }
                return -1;
            }
            includes(value, byteOffset, encoding) { return this.indexOf(value, byteOffset, encoding) !== -1; }
            fill(value, offset, end, encoding) {
                // Node.js signature: fill(value[, offset[, end]][, encoding])
                if (typeof offset === 'string') { encoding = offset; offset = 0; end = this.length; }
                else if (typeof end === 'string') { encoding = end; end = this.length; }
                offset = offset ?? 0;
                end = end ?? this.length;
                if (typeof value === 'string') {
                    const bytes = Buffer.from(value, encoding || 'utf8');
                    for (let i = offset; i < end; i++) this[i] = bytes[(i - offset) % bytes.length];
                } else if (typeof value === 'number') {
                    super.fill(value, offset, end);
                } else if (value instanceof Uint8Array) {
                    for (let i = offset; i < end; i++) this[i] = value[(i - offset) % value.length];
                }
                return this;
            }

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

            // Variable-width integer read methods (1-6 bytes)
            readIntLE(offset, byteLength) {
                if (byteLength < 1 || byteLength > 6) throw new RangeError('byteLength must be 1-6');
                let val = 0, mul = 1;
                for (let i = 0; i < byteLength; i++) { val += this[offset + i] * mul; mul *= 256; }
                if (val >= mul / 2) val -= mul;
                return val;
            }
            readIntBE(offset, byteLength) {
                if (byteLength < 1 || byteLength > 6) throw new RangeError('byteLength must be 1-6');
                let val = 0;
                for (let i = 0; i < byteLength; i++) val = val * 256 + this[offset + i];
                const mul = Math.pow(256, byteLength);
                if (val >= mul / 2) val -= mul;
                return val;
            }
            readUIntLE(offset, byteLength) {
                if (byteLength < 1 || byteLength > 6) throw new RangeError('byteLength must be 1-6');
                let val = 0, mul = 1;
                for (let i = 0; i < byteLength; i++) { val += this[offset + i] * mul; mul *= 256; }
                return val;
            }
            readUIntBE(offset, byteLength) {
                if (byteLength < 1 || byteLength > 6) throw new RangeError('byteLength must be 1-6');
                let val = 0;
                for (let i = 0; i < byteLength; i++) val = val * 256 + this[offset + i];
                return val;
            }

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

            // Variable-width integer write methods (1-6 bytes)
            writeIntLE(value, offset, byteLength) {
                if (byteLength < 1 || byteLength > 6) throw new RangeError('byteLength must be 1-6');
                let val = value < 0 ? value + Math.pow(256, byteLength) : value;
                for (let i = 0; i < byteLength; i++) { this[offset + i] = val & 0xFF; val = Math.floor(val / 256); }
                return offset + byteLength;
            }
            writeIntBE(value, offset, byteLength) {
                if (byteLength < 1 || byteLength > 6) throw new RangeError('byteLength must be 1-6');
                let val = value < 0 ? value + Math.pow(256, byteLength) : value;
                for (let i = byteLength - 1; i >= 0; i--) { this[offset + i] = val & 0xFF; val = Math.floor(val / 256); }
                return offset + byteLength;
            }
            writeUIntLE(value, offset, byteLength) {
                if (byteLength < 1 || byteLength > 6) throw new RangeError('byteLength must be 1-6');
                let val = value;
                for (let i = 0; i < byteLength; i++) { this[offset + i] = val & 0xFF; val = Math.floor(val / 256); }
                return offset + byteLength;
            }
            writeUIntBE(value, offset, byteLength) {
                if (byteLength < 1 || byteLength > 6) throw new RangeError('byteLength must be 1-6');
                let val = value;
                for (let i = byteLength - 1; i >= 0; i--) { this[offset + i] = val & 0xFF; val = Math.floor(val / 256); }
                return offset + byteLength;
            }

            // Swap/other methods
            swap16() { for (let i = 0; i < this.length; i += 2) { const t = this[i]; this[i] = this[i + 1]; this[i + 1] = t; } return this; }
            swap32() { for (let i = 0; i < this.length; i += 4) { const t0 = this[i], t1 = this[i + 1]; this[i] = this[i + 3]; this[i + 1] = this[i + 2]; this[i + 2] = t1; this[i + 3] = t0; } return this; }
            swap64() { for (let i = 0; i < this.length; i += 8) { const t = [this[i], this[i+1], this[i+2], this[i+3]]; this[i] = this[i+7]; this[i+1] = this[i+6]; this[i+2] = this[i+5]; this[i+3] = this[i+4]; this[i+4] = t[3]; this[i+5] = t[2]; this[i+6] = t[1]; this[i+7] = t[0]; } return this; }
            toJSON() { return { type: 'Buffer', data: Array.from(this) }; }
            reverse() { return Object.setPrototypeOf(super.reverse(), Buffer.prototype); }
            // Iterator methods
            *entries() { for (let i = 0; i < this.length; i++) yield [i, this[i]]; }
            *keys() { for (let i = 0; i < this.length; i++) yield i; }
            *values() { for (let i = 0; i < this.length; i++) yield this[i]; }
        }
        // Static properties
        Buffer.poolSize = 8192;
        Buffer.constants = {
            MAX_LENGTH: 0x7FFFFFFF,
            MAX_STRING_LENGTH: 0x1FFFFFFF
        };
        _modules.buffer = { Buffer };
        _modules['node:buffer'] = _modules.buffer;
        globalThis.Buffer = Buffer;
    }

