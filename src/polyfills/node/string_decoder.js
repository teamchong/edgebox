// Minimal string_decoder polyfill for EdgeBox
// Based on Node.js StringDecoder API

class StringDecoder {
  constructor(encoding) {
    this.encoding = (encoding || 'utf8').toLowerCase().replace(/-/g, '');
    this.lastNeed = 0;
    this.lastTotal = 0;
    this.lastChar = new Uint8Array(4);
  }

  write(buf) {
    if (!buf || buf.length === 0) return '';

    // Convert to Uint8Array if needed
    if (!(buf instanceof Uint8Array)) {
      if (typeof buf === 'string') return buf;
      if (Array.isArray(buf)) buf = new Uint8Array(buf);
      else if (buf.buffer) buf = new Uint8Array(buf.buffer, buf.byteOffset, buf.byteLength);
    }

    if (this.encoding === 'utf8' || this.encoding === 'utf-8') {
      return this._utf8Write(buf);
    }
    if (this.encoding === 'base64') {
      return this._base64Write(buf);
    }
    if (this.encoding === 'hex') {
      return Array.from(buf).map(b => b.toString(16).padStart(2, '0')).join('');
    }
    // ascii, latin1, binary
    return String.fromCharCode.apply(null, buf);
  }

  _utf8Write(buf) {
    let result = '';
    let i = 0;

    // Handle leftover bytes from previous write
    if (this.lastNeed > 0) {
      const needed = Math.min(buf.length, this.lastNeed);
      for (let j = 0; j < needed; j++) {
        this.lastChar[this.lastTotal - this.lastNeed + j] = buf[j];
      }
      this.lastNeed -= needed;
      i = needed;

      if (this.lastNeed === 0) {
        // Complete the character
        result += new TextDecoder('utf-8').decode(this.lastChar.subarray(0, this.lastTotal));
      } else {
        // Still need more bytes
        return '';
      }
    }

    // Find incomplete multi-byte character at end
    let incomplete = 0;
    if (buf.length > i) {
      const lastByte = buf[buf.length - 1];
      if (lastByte >= 0xC0) {
        // This byte starts a multi-byte sequence
        incomplete = lastByte >= 0xF0 ? 4 : lastByte >= 0xE0 ? 3 : 2;
        if (buf.length - i < incomplete) {
          // Not enough bytes, save for later
          this.lastNeed = incomplete - (buf.length - i);
          this.lastTotal = incomplete;
          for (let j = 0; j < buf.length - i; j++) {
            this.lastChar[j] = buf[i + j];
          }
          if (i > 0) {
            result += new TextDecoder('utf-8').decode(buf.subarray(0, i));
          }
          return result;
        }
        incomplete = 0;
      } else {
        // Check for incomplete sequence at end
        for (let j = Math.min(3, buf.length - 1); j >= 1; j--) {
          const b = buf[buf.length - j];
          if (b >= 0xC0) {
            const expectedLen = b >= 0xF0 ? 4 : b >= 0xE0 ? 3 : 2;
            if (j < expectedLen) {
              incomplete = j;
              this.lastNeed = expectedLen - j;
              this.lastTotal = expectedLen;
              for (let k = 0; k < j; k++) {
                this.lastChar[k] = buf[buf.length - j + k];
              }
            }
            break;
          }
        }
      }
    }

    if (incomplete > 0) {
      result += new TextDecoder('utf-8').decode(buf.subarray(i, buf.length - incomplete));
    } else {
      result += new TextDecoder('utf-8').decode(buf.subarray(i));
    }

    return result;
  }

  _base64Write(buf) {
    return btoa(String.fromCharCode.apply(null, buf));
  }

  end(buf) {
    let result = '';
    if (buf && buf.length > 0) {
      result = this.write(buf);
    }
    if (this.lastNeed > 0) {
      // Return replacement character for incomplete sequence
      result += '\ufffd';
      this.lastNeed = 0;
    }
    return result;
  }

  text(buf, offset) {
    return this.write(buf.subarray(offset));
  }
}

module.exports = StringDecoder;
module.exports.StringDecoder = StringDecoder;
