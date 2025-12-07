// Minimal Buffer polyfill for EdgeBox
// Based on Node.js Buffer API

class Buffer extends Uint8Array {
  constructor(arg, encodingOrOffset, length) {
    if (typeof arg === 'number') {
      super(arg);
    } else if (typeof arg === 'string') {
      const encoded = new TextEncoder().encode(arg);
      super(encoded);
    } else if (arg instanceof ArrayBuffer) {
      super(arg, encodingOrOffset || 0, length);
    } else if (ArrayBuffer.isView(arg)) {
      super(arg.buffer, arg.byteOffset, arg.byteLength);
    } else if (Array.isArray(arg)) {
      super(arg);
    } else {
      super(0);
    }
  }

  static from(value, encodingOrOffset, length) {
    if (typeof value === 'string') {
      return new Buffer(new TextEncoder().encode(value));
    }
    if (value instanceof ArrayBuffer) {
      return new Buffer(value, encodingOrOffset, length);
    }
    if (ArrayBuffer.isView(value)) {
      return new Buffer(value.buffer, value.byteOffset, value.byteLength);
    }
    if (Array.isArray(value)) {
      return new Buffer(value);
    }
    return new Buffer(value);
  }

  static alloc(size, fill, encoding) {
    const buf = new Buffer(size);
    if (fill !== undefined) {
      if (typeof fill === 'number') {
        buf.fill(fill);
      } else if (typeof fill === 'string') {
        const fillBuf = Buffer.from(fill, encoding);
        for (let i = 0; i < size; i++) {
          buf[i] = fillBuf[i % fillBuf.length];
        }
      }
    }
    return buf;
  }

  static allocUnsafe(size) {
    return new Buffer(size);
  }

  static allocUnsafeSlow(size) {
    return new Buffer(size);
  }

  static isBuffer(obj) {
    return obj instanceof Buffer || obj instanceof Uint8Array;
  }

  static isEncoding(encoding) {
    return ['utf8', 'utf-8', 'ascii', 'latin1', 'binary', 'base64', 'hex', 'ucs2', 'ucs-2', 'utf16le', 'utf-16le'].includes(encoding?.toLowerCase());
  }

  static byteLength(string, encoding) {
    if (typeof string !== 'string') return string.length;
    return new TextEncoder().encode(string).length;
  }

  static concat(list, totalLength) {
    if (list.length === 0) return Buffer.alloc(0);
    if (totalLength === undefined) {
      totalLength = 0;
      for (const buf of list) totalLength += buf.length;
    }
    const result = Buffer.alloc(totalLength);
    let offset = 0;
    for (const buf of list) {
      result.set(buf, offset);
      offset += buf.length;
      if (offset >= totalLength) break;
    }
    return result;
  }

  static compare(buf1, buf2) {
    for (let i = 0; i < Math.min(buf1.length, buf2.length); i++) {
      if (buf1[i] < buf2[i]) return -1;
      if (buf1[i] > buf2[i]) return 1;
    }
    if (buf1.length < buf2.length) return -1;
    if (buf1.length > buf2.length) return 1;
    return 0;
  }

  toString(encoding, start, end) {
    start = start || 0;
    end = end || this.length;
    const slice = this.subarray(start, end);

    encoding = (encoding || 'utf8').toLowerCase();
    if (encoding === 'utf8' || encoding === 'utf-8') {
      return new TextDecoder('utf-8').decode(slice);
    }
    if (encoding === 'hex') {
      return Array.from(slice).map(b => b.toString(16).padStart(2, '0')).join('');
    }
    if (encoding === 'base64') {
      return btoa(String.fromCharCode(...slice));
    }
    if (encoding === 'ascii' || encoding === 'latin1' || encoding === 'binary') {
      return String.fromCharCode(...slice);
    }
    return new TextDecoder('utf-8').decode(slice);
  }

  write(string, offset, length, encoding) {
    if (typeof offset === 'string') {
      encoding = offset;
      offset = 0;
      length = this.length;
    } else if (typeof length === 'string') {
      encoding = length;
      length = this.length - offset;
    }
    offset = offset || 0;
    length = length || this.length - offset;

    const bytes = Buffer.from(string, encoding);
    const written = Math.min(bytes.length, length, this.length - offset);
    this.set(bytes.subarray(0, written), offset);
    return written;
  }

  slice(start, end) {
    return Buffer.from(this.subarray(start, end));
  }

  copy(target, targetStart, sourceStart, sourceEnd) {
    targetStart = targetStart || 0;
    sourceStart = sourceStart || 0;
    sourceEnd = sourceEnd || this.length;
    const bytes = this.subarray(sourceStart, sourceEnd);
    target.set(bytes, targetStart);
    return bytes.length;
  }

  equals(otherBuffer) {
    if (this.length !== otherBuffer.length) return false;
    for (let i = 0; i < this.length; i++) {
      if (this[i] !== otherBuffer[i]) return false;
    }
    return true;
  }

  compare(target, targetStart, targetEnd, sourceStart, sourceEnd) {
    targetStart = targetStart || 0;
    targetEnd = targetEnd || target.length;
    sourceStart = sourceStart || 0;
    sourceEnd = sourceEnd || this.length;
    return Buffer.compare(this.subarray(sourceStart, sourceEnd), target.subarray(targetStart, targetEnd));
  }

  indexOf(value, byteOffset, encoding) {
    byteOffset = byteOffset || 0;
    if (typeof value === 'string') value = Buffer.from(value, encoding);
    if (typeof value === 'number') {
      for (let i = byteOffset; i < this.length; i++) {
        if (this[i] === value) return i;
      }
      return -1;
    }
    outer: for (let i = byteOffset; i <= this.length - value.length; i++) {
      for (let j = 0; j < value.length; j++) {
        if (this[i + j] !== value[j]) continue outer;
      }
      return i;
    }
    return -1;
  }

  includes(value, byteOffset, encoding) {
    return this.indexOf(value, byteOffset, encoding) !== -1;
  }

  // Read methods
  readUInt8(offset) { return this[offset]; }
  readUInt16LE(offset) { return this[offset] | (this[offset + 1] << 8); }
  readUInt16BE(offset) { return (this[offset] << 8) | this[offset + 1]; }
  readUInt32LE(offset) { return this[offset] | (this[offset + 1] << 8) | (this[offset + 2] << 16) | (this[offset + 3] << 24) >>> 0; }
  readUInt32BE(offset) { return ((this[offset] << 24) | (this[offset + 1] << 16) | (this[offset + 2] << 8) | this[offset + 3]) >>> 0; }
  readInt8(offset) { const val = this[offset]; return val > 127 ? val - 256 : val; }
  readInt16LE(offset) { const val = this.readUInt16LE(offset); return val > 32767 ? val - 65536 : val; }
  readInt16BE(offset) { const val = this.readUInt16BE(offset); return val > 32767 ? val - 65536 : val; }
  readInt32LE(offset) { return this[offset] | (this[offset + 1] << 8) | (this[offset + 2] << 16) | (this[offset + 3] << 24); }
  readInt32BE(offset) { return (this[offset] << 24) | (this[offset + 1] << 16) | (this[offset + 2] << 8) | this[offset + 3]; }

  // Write methods
  writeUInt8(value, offset) { this[offset] = value & 0xff; return offset + 1; }
  writeUInt16LE(value, offset) { this[offset] = value & 0xff; this[offset + 1] = (value >> 8) & 0xff; return offset + 2; }
  writeUInt16BE(value, offset) { this[offset] = (value >> 8) & 0xff; this[offset + 1] = value & 0xff; return offset + 2; }
  writeUInt32LE(value, offset) { this[offset] = value & 0xff; this[offset + 1] = (value >> 8) & 0xff; this[offset + 2] = (value >> 16) & 0xff; this[offset + 3] = (value >> 24) & 0xff; return offset + 4; }
  writeUInt32BE(value, offset) { this[offset] = (value >> 24) & 0xff; this[offset + 1] = (value >> 16) & 0xff; this[offset + 2] = (value >> 8) & 0xff; this[offset + 3] = value & 0xff; return offset + 4; }
  writeInt8(value, offset) { if (value < 0) value = 256 + value; this[offset] = value & 0xff; return offset + 1; }
  writeInt16LE(value, offset) { if (value < 0) value = 65536 + value; return this.writeUInt16LE(value, offset); }
  writeInt16BE(value, offset) { if (value < 0) value = 65536 + value; return this.writeUInt16BE(value, offset); }
  writeInt32LE(value, offset) { if (value < 0) value = 0x100000000 + value; return this.writeUInt32LE(value, offset); }
  writeInt32BE(value, offset) { if (value < 0) value = 0x100000000 + value; return this.writeUInt32BE(value, offset); }

  toJSON() {
    return { type: 'Buffer', data: Array.from(this) };
  }
}

// Constants
Buffer.poolSize = 8192;
const kMaxLength = 0x7fffffff;
const constants = { MAX_LENGTH: kMaxLength, MAX_STRING_LENGTH: 0x1fffffe8 };

module.exports = Buffer;
module.exports.Buffer = Buffer;
module.exports.constants = constants;
module.exports.kMaxLength = kMaxLength;
module.exports.INSPECT_MAX_BYTES = 50;
module.exports.SlowBuffer = Buffer;
