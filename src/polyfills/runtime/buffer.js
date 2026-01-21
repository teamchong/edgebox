// EdgeBox Runtime: Buffer class
// Dependencies: text.js (TextEncoder/TextDecoder)

if (typeof Buffer === 'undefined') {
    globalThis.Buffer = class Buffer extends Uint8Array {
        constructor(arg, encodingOrOffset, length) {
            if (typeof arg === 'number') { super(arg); }
            else if (typeof arg === 'string') { super(new TextEncoder().encode(arg)); }
            else if (arg instanceof ArrayBuffer) { super(arg, encodingOrOffset, length); }
            else if (ArrayBuffer.isView(arg)) { super(arg.buffer, arg.byteOffset, arg.byteLength); }
            else if (Array.isArray(arg)) { super(arg); }
            else { super(0); }
        }
        static from(value, encodingOrOffset, length) {
            if (typeof value === 'string') return new Buffer(new TextEncoder().encode(value));
            if (value instanceof ArrayBuffer) return new Buffer(value, encodingOrOffset, length);
            if (ArrayBuffer.isView(value)) return new Buffer(value.buffer, value.byteOffset, value.byteLength);
            if (Array.isArray(value)) return new Buffer(value);
            return new Buffer(value);
        }
        static alloc(size, fill) { const buf = new Buffer(size); if (fill !== undefined) buf.fill(fill); return buf; }
        static allocUnsafe(size) { return new Buffer(size); }
        static allocUnsafeSlow(size) { return new Buffer(size); }
        static isBuffer(obj) { return obj instanceof Buffer || obj instanceof Uint8Array; }
        static concat(list, totalLength) {
            if (list.length === 0) return Buffer.alloc(0);
            totalLength = totalLength ?? list.reduce((acc, buf) => acc + buf.length, 0);
            const result = Buffer.alloc(totalLength);
            let offset = 0;
            for (const buf of list) { result.set(buf, offset); offset += buf.length; }
            return result;
        }
        toString(encoding = 'utf8') { return new TextDecoder(encoding === 'utf8' ? 'utf-8' : encoding).decode(this); }
        slice(start, end) { return new Buffer(super.slice(start, end)); }
        write(string, offset = 0, length, encoding = 'utf8') {
            const bytes = new TextEncoder().encode(string);
            const len = Math.min(bytes.length, length ?? this.length - offset);
            this.set(bytes.subarray(0, len), offset);
            return len;
        }
        copy(target, targetStart = 0, sourceStart = 0, sourceEnd = this.length) {
            const slice = this.subarray(sourceStart, sourceEnd);
            target.set(slice, targetStart);
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
        indexOf(value, byteOffset = 0) {
            if (typeof value === 'string') value = new TextEncoder().encode(value);
            if (typeof value === 'number') {
                for (let i = byteOffset; i < this.length; i++) if (this[i] === value) return i;
                return -1;
            }
            outer: for (let i = byteOffset; i <= this.length - value.length; i++) {
                for (let j = 0; j < value.length; j++) if (this[i + j] !== value[j]) continue outer;
                return i;
            }
            return -1;
        }
        includes(value, byteOffset = 0) { return this.indexOf(value, byteOffset) !== -1; }
        static byteLength(str, encoding = 'utf8') { return new TextEncoder().encode(str).length; }
        toJSON() { return { type: 'Buffer', data: Array.from(this) }; }
        readInt32LE(offset = 0) { return new DataView(this.buffer, this.byteOffset, this.byteLength).getInt32(offset, true); }
        readInt32BE(offset = 0) { return new DataView(this.buffer, this.byteOffset, this.byteLength).getInt32(offset, false); }
        writeInt32LE(value, offset = 0) { new DataView(this.buffer, this.byteOffset, this.byteLength).setInt32(offset, value, true); return offset + 4; }
        writeInt32BE(value, offset = 0) { new DataView(this.buffer, this.byteOffset, this.byteLength).setInt32(offset, value, false); return offset + 4; }
        readUInt32LE(offset = 0) { return new DataView(this.buffer, this.byteOffset, this.byteLength).getUint32(offset, true); }
        readUInt32BE(offset = 0) { return new DataView(this.buffer, this.byteOffset, this.byteLength).getUint32(offset, false); }
        writeUInt32LE(value, offset = 0) { new DataView(this.buffer, this.byteOffset, this.byteLength).setUint32(offset, value, true); return offset + 4; }
        writeUInt32BE(value, offset = 0) { new DataView(this.buffer, this.byteOffset, this.byteLength).setUint32(offset, value, false); return offset + 4; }
        readUInt16BE(offset = 0) { return new DataView(this.buffer, this.byteOffset, this.byteLength).getUint16(offset, false); }
        writeUInt16BE(value, offset = 0) { new DataView(this.buffer, this.byteOffset, this.byteLength).setUint16(offset, value, false); return offset + 2; }
        readFloatLE(offset = 0) { return new DataView(this.buffer, this.byteOffset, this.byteLength).getFloat32(offset, true); }
        writeFloatLE(value, offset = 0) { new DataView(this.buffer, this.byteOffset, this.byteLength).setFloat32(offset, value, true); return offset + 4; }
        readDoubleLE(offset = 0) { return new DataView(this.buffer, this.byteOffset, this.byteLength).getFloat64(offset, true); }
        writeDoubleLE(value, offset = 0) { new DataView(this.buffer, this.byteOffset, this.byteLength).setFloat64(offset, value, true); return offset + 8; }
    };
}
