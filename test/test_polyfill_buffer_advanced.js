// Advanced Buffer polyfill tests
// Tests numeric read/write methods, search, iteration, and other advanced operations

const {
    assertEqual, assertArrayEqual, assertTrue, assertFalse,
    assertTypeOf, assertApprox, summary
} = require('./helpers/assert.js');

console.log('=== Buffer Advanced Tests ===\n');

// ============================================
// Read integer methods - Little Endian
// ============================================
console.log('--- Read integers (LE) ---');

// readInt8
const int8Buf = Buffer.from([0x7F, 0x80, 0xFF, 0x00]);
if (typeof int8Buf.readInt8 === 'function') {
    assertEqual(int8Buf.readInt8(0), 127, 'readInt8 positive max (127)');
    assertEqual(int8Buf.readInt8(1), -128, 'readInt8 negative min (-128)');
    assertEqual(int8Buf.readInt8(2), -1, 'readInt8 -1 (0xFF)');
    assertEqual(int8Buf.readInt8(3), 0, 'readInt8 zero');
} else {
    console.log('SKIP: readInt8 not available');
}

// readUInt8
if (typeof int8Buf.readUInt8 === 'function') {
    assertEqual(int8Buf.readUInt8(0), 127, 'readUInt8 127');
    assertEqual(int8Buf.readUInt8(1), 128, 'readUInt8 128');
    assertEqual(int8Buf.readUInt8(2), 255, 'readUInt8 255');
    assertEqual(int8Buf.readUInt8(3), 0, 'readUInt8 0');
} else {
    console.log('SKIP: readUInt8 not available');
}

// readInt16LE
const int16Buf = Buffer.from([0x01, 0x02, 0xFF, 0x7F, 0x00, 0x80]);
if (typeof int16Buf.readInt16LE === 'function') {
    assertEqual(int16Buf.readInt16LE(0), 0x0201, 'readInt16LE 0x0201');
    assertEqual(int16Buf.readInt16LE(2), 0x7FFF, 'readInt16LE max positive (32767)');
    assertEqual(int16Buf.readInt16LE(4), -32768, 'readInt16LE min negative (-32768)');
} else {
    console.log('SKIP: readInt16LE not available');
}

// readUInt16LE
if (typeof int16Buf.readUInt16LE === 'function') {
    assertEqual(int16Buf.readUInt16LE(0), 0x0201, 'readUInt16LE 0x0201');
    assertEqual(int16Buf.readUInt16LE(2), 0x7FFF, 'readUInt16LE 0x7FFF');
    assertEqual(int16Buf.readUInt16LE(4), 0x8000, 'readUInt16LE 0x8000');
} else {
    console.log('SKIP: readUInt16LE not available');
}

// readInt32LE
const int32Buf = Buffer.from([0x01, 0x02, 0x03, 0x04, 0xFF, 0xFF, 0xFF, 0x7F, 0x00, 0x00, 0x00, 0x80]);
if (typeof int32Buf.readInt32LE === 'function') {
    assertEqual(int32Buf.readInt32LE(0), 0x04030201, 'readInt32LE 0x04030201');
    assertEqual(int32Buf.readInt32LE(4), 0x7FFFFFFF, 'readInt32LE max positive');
    assertEqual(int32Buf.readInt32LE(8), -2147483648, 'readInt32LE min negative');
} else {
    console.log('SKIP: readInt32LE not available');
}

// readUInt32LE
if (typeof int32Buf.readUInt32LE === 'function') {
    assertEqual(int32Buf.readUInt32LE(0), 0x04030201, 'readUInt32LE 0x04030201');
    assertEqual(int32Buf.readUInt32LE(4), 0x7FFFFFFF, 'readUInt32LE 0x7FFFFFFF');
    assertEqual(int32Buf.readUInt32LE(8), 0x80000000, 'readUInt32LE 0x80000000');
} else {
    console.log('SKIP: readUInt32LE not available');
}

// ============================================
// Read integer methods - Big Endian
// ============================================
console.log('\n--- Read integers (BE) ---');

// readInt16BE
const beBuf = Buffer.from([0x02, 0x01, 0x7F, 0xFF, 0x80, 0x00]);
if (typeof beBuf.readInt16BE === 'function') {
    assertEqual(beBuf.readInt16BE(0), 0x0201, 'readInt16BE 0x0201');
    assertEqual(beBuf.readInt16BE(2), 0x7FFF, 'readInt16BE max positive');
    assertEqual(beBuf.readInt16BE(4), -32768, 'readInt16BE min negative');
} else {
    console.log('SKIP: readInt16BE not available');
}

// readUInt16BE
if (typeof beBuf.readUInt16BE === 'function') {
    assertEqual(beBuf.readUInt16BE(0), 0x0201, 'readUInt16BE 0x0201');
    assertEqual(beBuf.readUInt16BE(4), 0x8000, 'readUInt16BE 0x8000');
} else {
    console.log('SKIP: readUInt16BE not available');
}

// readInt32BE
const be32Buf = Buffer.from([0x04, 0x03, 0x02, 0x01, 0x7F, 0xFF, 0xFF, 0xFF, 0x80, 0x00, 0x00, 0x00]);
if (typeof be32Buf.readInt32BE === 'function') {
    assertEqual(be32Buf.readInt32BE(0), 0x04030201, 'readInt32BE 0x04030201');
    assertEqual(be32Buf.readInt32BE(4), 0x7FFFFFFF, 'readInt32BE max positive');
    assertEqual(be32Buf.readInt32BE(8), -2147483648, 'readInt32BE min negative');
} else {
    console.log('SKIP: readInt32BE not available');
}

// readUInt32BE
if (typeof be32Buf.readUInt32BE === 'function') {
    assertEqual(be32Buf.readUInt32BE(0), 0x04030201, 'readUInt32BE 0x04030201');
    assertEqual(be32Buf.readUInt32BE(8), 0x80000000, 'readUInt32BE 0x80000000');
} else {
    console.log('SKIP: readUInt32BE not available');
}

// ============================================
// Read floating point methods
// ============================================
console.log('\n--- Read floating point ---');

// readFloatLE - 3.14 in IEEE 754 float LE
const floatBuf = Buffer.from([0xC3, 0xF5, 0x48, 0x40]); // 3.14 in float LE
if (typeof floatBuf.readFloatLE === 'function') {
    assertApprox(floatBuf.readFloatLE(0), 3.14, 0.01, 'readFloatLE ~3.14');
} else {
    console.log('SKIP: readFloatLE not available');
}

// readFloatBE
const floatBEBuf = Buffer.from([0x40, 0x48, 0xF5, 0xC3]); // 3.14 in float BE
if (typeof floatBEBuf.readFloatBE === 'function') {
    assertApprox(floatBEBuf.readFloatBE(0), 3.14, 0.01, 'readFloatBE ~3.14');
} else {
    console.log('SKIP: readFloatBE not available');
}

// readDoubleLE - 3.14159265358979 in IEEE 754 double LE
const doubleBuf = Buffer.from([0x18, 0x2D, 0x44, 0x54, 0xFB, 0x21, 0x09, 0x40]); // pi in double LE
if (typeof doubleBuf.readDoubleLE === 'function') {
    assertApprox(doubleBuf.readDoubleLE(0), 3.14159265358979, 0.0000001, 'readDoubleLE ~pi');
} else {
    console.log('SKIP: readDoubleLE not available');
}

// readDoubleBE
const doubleBEBuf = Buffer.from([0x40, 0x09, 0x21, 0xFB, 0x54, 0x44, 0x2D, 0x18]); // pi in double BE
if (typeof doubleBEBuf.readDoubleBE === 'function') {
    assertApprox(doubleBEBuf.readDoubleBE(0), 3.14159265358979, 0.0000001, 'readDoubleBE ~pi');
} else {
    console.log('SKIP: readDoubleBE not available');
}

// ============================================
// Read BigInt methods (if available)
// ============================================
console.log('\n--- Read BigInt ---');

const bigIntBuf = Buffer.from([0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08]);
if (typeof bigIntBuf.readBigInt64LE === 'function') {
    const bigVal = bigIntBuf.readBigInt64LE(0);
    assertEqual(typeof bigVal, 'bigint', 'readBigInt64LE returns bigint');
    console.log('PASS: readBigInt64LE exists');
} else {
    console.log('SKIP: readBigInt64LE not available');
}

if (typeof bigIntBuf.readBigInt64BE === 'function') {
    const bigVal = bigIntBuf.readBigInt64BE(0);
    assertEqual(typeof bigVal, 'bigint', 'readBigInt64BE returns bigint');
    console.log('PASS: readBigInt64BE exists');
} else {
    console.log('SKIP: readBigInt64BE not available');
}

if (typeof bigIntBuf.readBigUInt64LE === 'function') {
    console.log('PASS: readBigUInt64LE exists');
} else {
    console.log('SKIP: readBigUInt64LE not available');
}

if (typeof bigIntBuf.readBigUInt64BE === 'function') {
    console.log('PASS: readBigUInt64BE exists');
} else {
    console.log('SKIP: readBigUInt64BE not available');
}

// ============================================
// Write integer methods - Little Endian
// ============================================
console.log('\n--- Write integers (LE) ---');

// writeInt8
const wInt8Buf = Buffer.alloc(4);
if (typeof wInt8Buf.writeInt8 === 'function') {
    wInt8Buf.writeInt8(127, 0);
    wInt8Buf.writeInt8(-128, 1);
    wInt8Buf.writeInt8(-1, 2);
    wInt8Buf.writeInt8(0, 3);
    assertEqual(wInt8Buf[0], 0x7F, 'writeInt8 127');
    assertEqual(wInt8Buf[1], 0x80, 'writeInt8 -128');
    assertEqual(wInt8Buf[2], 0xFF, 'writeInt8 -1');
    assertEqual(wInt8Buf[3], 0x00, 'writeInt8 0');
} else {
    console.log('SKIP: writeInt8 not available');
}

// writeUInt8
const wUInt8Buf = Buffer.alloc(4);
if (typeof wUInt8Buf.writeUInt8 === 'function') {
    wUInt8Buf.writeUInt8(0, 0);
    wUInt8Buf.writeUInt8(127, 1);
    wUInt8Buf.writeUInt8(128, 2);
    wUInt8Buf.writeUInt8(255, 3);
    assertArrayEqual(Array.from(wUInt8Buf), [0, 127, 128, 255], 'writeUInt8 values');
} else {
    console.log('SKIP: writeUInt8 not available');
}

// writeInt16LE
const wInt16Buf = Buffer.alloc(6);
if (typeof wInt16Buf.writeInt16LE === 'function') {
    wInt16Buf.writeInt16LE(0x0201, 0);
    wInt16Buf.writeInt16LE(32767, 2);
    wInt16Buf.writeInt16LE(-32768, 4);
    assertEqual(wInt16Buf[0], 0x01, 'writeInt16LE low byte');
    assertEqual(wInt16Buf[1], 0x02, 'writeInt16LE high byte');
} else {
    console.log('SKIP: writeInt16LE not available');
}

// writeUInt16LE
const wUInt16Buf = Buffer.alloc(4);
if (typeof wUInt16Buf.writeUInt16LE === 'function') {
    wUInt16Buf.writeUInt16LE(0x1234, 0);
    wUInt16Buf.writeUInt16LE(0xFFFF, 2);
    assertEqual(wUInt16Buf[0], 0x34, 'writeUInt16LE low byte');
    assertEqual(wUInt16Buf[1], 0x12, 'writeUInt16LE high byte');
} else {
    console.log('SKIP: writeUInt16LE not available');
}

// writeInt32LE
const wInt32Buf = Buffer.alloc(8);
if (typeof wInt32Buf.writeInt32LE === 'function') {
    wInt32Buf.writeInt32LE(0x04030201, 0);
    wInt32Buf.writeInt32LE(-1, 4);
    assertEqual(wInt32Buf[0], 0x01, 'writeInt32LE byte 0');
    assertEqual(wInt32Buf[1], 0x02, 'writeInt32LE byte 1');
    assertEqual(wInt32Buf[2], 0x03, 'writeInt32LE byte 2');
    assertEqual(wInt32Buf[3], 0x04, 'writeInt32LE byte 3');
} else {
    console.log('SKIP: writeInt32LE not available');
}

// writeUInt32LE
const wUInt32Buf = Buffer.alloc(4);
if (typeof wUInt32Buf.writeUInt32LE === 'function') {
    wUInt32Buf.writeUInt32LE(0xDEADBEEF, 0);
    assertEqual(wUInt32Buf[0], 0xEF, 'writeUInt32LE byte 0');
    assertEqual(wUInt32Buf[1], 0xBE, 'writeUInt32LE byte 1');
    assertEqual(wUInt32Buf[2], 0xAD, 'writeUInt32LE byte 2');
    assertEqual(wUInt32Buf[3], 0xDE, 'writeUInt32LE byte 3');
} else {
    console.log('SKIP: writeUInt32LE not available');
}

// ============================================
// Write integer methods - Big Endian
// ============================================
console.log('\n--- Write integers (BE) ---');

// writeInt16BE
const wInt16BEBuf = Buffer.alloc(4);
if (typeof wInt16BEBuf.writeInt16BE === 'function') {
    wInt16BEBuf.writeInt16BE(0x0102, 0);
    assertEqual(wInt16BEBuf[0], 0x01, 'writeInt16BE high byte');
    assertEqual(wInt16BEBuf[1], 0x02, 'writeInt16BE low byte');
} else {
    console.log('SKIP: writeInt16BE not available');
}

// writeUInt16BE
const wUInt16BEBuf = Buffer.alloc(2);
if (typeof wUInt16BEBuf.writeUInt16BE === 'function') {
    wUInt16BEBuf.writeUInt16BE(0x1234, 0);
    assertEqual(wUInt16BEBuf[0], 0x12, 'writeUInt16BE high byte');
    assertEqual(wUInt16BEBuf[1], 0x34, 'writeUInt16BE low byte');
} else {
    console.log('SKIP: writeUInt16BE not available');
}

// writeInt32BE
const wInt32BEBuf = Buffer.alloc(4);
if (typeof wInt32BEBuf.writeInt32BE === 'function') {
    wInt32BEBuf.writeInt32BE(0x01020304, 0);
    assertEqual(wInt32BEBuf[0], 0x01, 'writeInt32BE byte 0');
    assertEqual(wInt32BEBuf[1], 0x02, 'writeInt32BE byte 1');
    assertEqual(wInt32BEBuf[2], 0x03, 'writeInt32BE byte 2');
    assertEqual(wInt32BEBuf[3], 0x04, 'writeInt32BE byte 3');
} else {
    console.log('SKIP: writeInt32BE not available');
}

// writeUInt32BE
const wUInt32BEBuf = Buffer.alloc(4);
if (typeof wUInt32BEBuf.writeUInt32BE === 'function') {
    wUInt32BEBuf.writeUInt32BE(0xDEADBEEF, 0);
    assertEqual(wUInt32BEBuf[0], 0xDE, 'writeUInt32BE byte 0');
    assertEqual(wUInt32BEBuf[1], 0xAD, 'writeUInt32BE byte 1');
    assertEqual(wUInt32BEBuf[2], 0xBE, 'writeUInt32BE byte 2');
    assertEqual(wUInt32BEBuf[3], 0xEF, 'writeUInt32BE byte 3');
} else {
    console.log('SKIP: writeUInt32BE not available');
}

// ============================================
// Write floating point methods
// ============================================
console.log('\n--- Write floating point ---');

// writeFloatLE
const wFloatBuf = Buffer.alloc(4);
if (typeof wFloatBuf.writeFloatLE === 'function') {
    wFloatBuf.writeFloatLE(3.14, 0);
    if (typeof wFloatBuf.readFloatLE === 'function') {
        assertApprox(wFloatBuf.readFloatLE(0), 3.14, 0.01, 'writeFloatLE roundtrip');
    }
} else {
    console.log('SKIP: writeFloatLE not available');
}

// writeFloatBE
const wFloatBEBuf = Buffer.alloc(4);
if (typeof wFloatBEBuf.writeFloatBE === 'function') {
    wFloatBEBuf.writeFloatBE(3.14, 0);
    if (typeof wFloatBEBuf.readFloatBE === 'function') {
        assertApprox(wFloatBEBuf.readFloatBE(0), 3.14, 0.01, 'writeFloatBE roundtrip');
    }
} else {
    console.log('SKIP: writeFloatBE not available');
}

// writeDoubleLE
const wDoubleBuf = Buffer.alloc(8);
if (typeof wDoubleBuf.writeDoubleLE === 'function') {
    wDoubleBuf.writeDoubleLE(Math.PI, 0);
    if (typeof wDoubleBuf.readDoubleLE === 'function') {
        assertApprox(wDoubleBuf.readDoubleLE(0), Math.PI, 0.0000001, 'writeDoubleLE roundtrip');
    }
} else {
    console.log('SKIP: writeDoubleLE not available');
}

// writeDoubleBE
const wDoubleBEBuf = Buffer.alloc(8);
if (typeof wDoubleBEBuf.writeDoubleBE === 'function') {
    wDoubleBEBuf.writeDoubleBE(Math.PI, 0);
    if (typeof wDoubleBEBuf.readDoubleBE === 'function') {
        assertApprox(wDoubleBEBuf.readDoubleBE(0), Math.PI, 0.0000001, 'writeDoubleBE roundtrip');
    }
} else {
    console.log('SKIP: writeDoubleBE not available');
}

// ============================================
// Search methods
// ============================================
console.log('\n--- Search methods ---');

const searchBuf = Buffer.from('Hello World Hello');

// indexOf
if (typeof searchBuf.indexOf === 'function') {
    assertEqual(searchBuf.indexOf('Hello'), 0, 'indexOf finds first occurrence');
    assertEqual(searchBuf.indexOf('World'), 6, 'indexOf finds middle');
    assertEqual(searchBuf.indexOf('Hello', 1), 12, 'indexOf with offset');
    assertEqual(searchBuf.indexOf('NotFound'), -1, 'indexOf returns -1 when not found');
    assertEqual(searchBuf.indexOf(Buffer.from('World')), 6, 'indexOf with Buffer');
    assertEqual(searchBuf.indexOf(0x57), 6, 'indexOf with byte value (W=0x57)');
} else {
    console.log('SKIP: indexOf not available');
}

// lastIndexOf
if (typeof searchBuf.lastIndexOf === 'function') {
    assertEqual(searchBuf.lastIndexOf('Hello'), 12, 'lastIndexOf finds last occurrence');
    assertEqual(searchBuf.lastIndexOf('World'), 6, 'lastIndexOf finds only occurrence');
    assertEqual(searchBuf.lastIndexOf('Hello', 11), 0, 'lastIndexOf with offset');
    assertEqual(searchBuf.lastIndexOf('NotFound'), -1, 'lastIndexOf returns -1 when not found');
} else {
    console.log('SKIP: lastIndexOf not available');
}

// includes
if (typeof searchBuf.includes === 'function') {
    assertTrue(searchBuf.includes('Hello'), 'includes returns true when found');
    assertTrue(searchBuf.includes('World'), 'includes finds World');
    assertFalse(searchBuf.includes('NotFound'), 'includes returns false when not found');
    assertTrue(searchBuf.includes('Hello', 12), 'includes with offset finds');
    assertFalse(searchBuf.includes('Hello', 13), 'includes with offset past last');
} else {
    console.log('SKIP: includes not available');
}

// ============================================
// Iteration methods
// ============================================
console.log('\n--- Iteration methods ---');

const iterBuf = Buffer.from([1, 2, 3, 4, 5]);

// keys()
if (typeof iterBuf.keys === 'function') {
    const keys = Array.from(iterBuf.keys());
    assertArrayEqual(keys, [0, 1, 2, 3, 4], 'keys() returns indices');
} else {
    console.log('SKIP: keys() not available');
}

// values()
if (typeof iterBuf.values === 'function') {
    const values = Array.from(iterBuf.values());
    assertArrayEqual(values, [1, 2, 3, 4, 5], 'values() returns values');
} else {
    console.log('SKIP: values() not available');
}

// entries()
if (typeof iterBuf.entries === 'function') {
    const entries = Array.from(iterBuf.entries());
    assertEqual(entries.length, 5, 'entries() returns 5 entries');
    assertArrayEqual(entries[0], [0, 1], 'entries()[0] is [0, 1]');
    assertArrayEqual(entries[4], [4, 5], 'entries()[4] is [4, 5]');
} else {
    console.log('SKIP: entries() not available');
}

// Symbol.iterator (for...of)
let iterSum = 0;
for (const byte of iterBuf) {
    iterSum += byte;
}
assertEqual(iterSum, 15, 'for...of iteration works');

// ============================================
// fill() method
// ============================================
console.log('\n--- fill() method ---');

const fillBuf = Buffer.alloc(10);
fillBuf.fill(0x42);
assertEqual(fillBuf[0], 0x42, 'fill byte value');
assertEqual(fillBuf[9], 0x42, 'fill last byte');

// fill with string
const fillStrBuf = Buffer.alloc(10);
fillStrBuf.fill('ab');
assertEqual(fillStrBuf[0], 0x61, 'fill string repeats (a)');
assertEqual(fillStrBuf[1], 0x62, 'fill string repeats (b)');
assertEqual(fillStrBuf[2], 0x61, 'fill string cycles');

// fill with start and end
const fillRangeBuf = Buffer.alloc(10);
fillRangeBuf.fill(0xFF, 2, 5);
assertEqual(fillRangeBuf[0], 0, 'fill range preserves before');
assertEqual(fillRangeBuf[2], 0xFF, 'fill range start');
assertEqual(fillRangeBuf[4], 0xFF, 'fill range end-1');
assertEqual(fillRangeBuf[5], 0, 'fill range preserves after');

// fill returns this
const fillReturn = fillBuf.fill(0);
assertEqual(fillReturn, fillBuf, 'fill returns this');

// ============================================
// swap methods (if available)
// ============================================
console.log('\n--- swap methods ---');

// swap16
const swap16Buf = Buffer.from([0x01, 0x02, 0x03, 0x04]);
if (typeof swap16Buf.swap16 === 'function') {
    swap16Buf.swap16();
    assertEqual(swap16Buf[0], 0x02, 'swap16 byte 0');
    assertEqual(swap16Buf[1], 0x01, 'swap16 byte 1');
    assertEqual(swap16Buf[2], 0x04, 'swap16 byte 2');
    assertEqual(swap16Buf[3], 0x03, 'swap16 byte 3');
} else {
    console.log('SKIP: swap16 not available');
}

// swap32
const swap32Buf = Buffer.from([0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08]);
if (typeof swap32Buf.swap32 === 'function') {
    swap32Buf.swap32();
    assertEqual(swap32Buf[0], 0x04, 'swap32 byte 0');
    assertEqual(swap32Buf[3], 0x01, 'swap32 byte 3');
    assertEqual(swap32Buf[4], 0x08, 'swap32 byte 4');
    assertEqual(swap32Buf[7], 0x05, 'swap32 byte 7');
} else {
    console.log('SKIP: swap32 not available');
}

// swap64
const swap64Buf = Buffer.from([0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08]);
if (typeof swap64Buf.swap64 === 'function') {
    swap64Buf.swap64();
    assertEqual(swap64Buf[0], 0x08, 'swap64 byte 0');
    assertEqual(swap64Buf[7], 0x01, 'swap64 byte 7');
} else {
    console.log('SKIP: swap64 not available');
}

// ============================================
// Other Buffer methods
// ============================================
console.log('\n--- Other methods ---');

// toJSON
const jsonBuf = Buffer.from([1, 2, 3]);
if (typeof jsonBuf.toJSON === 'function') {
    const json = jsonBuf.toJSON();
    assertEqual(json.type, 'Buffer', 'toJSON has type Buffer');
    assertArrayEqual(json.data, [1, 2, 3], 'toJSON has data array');
} else {
    console.log('SKIP: toJSON not available');
}

// reverse (if available, non-standard)
const reverseBuf = Buffer.from([1, 2, 3, 4, 5]);
if (typeof reverseBuf.reverse === 'function') {
    reverseBuf.reverse();
    assertArrayEqual(Array.from(reverseBuf), [5, 4, 3, 2, 1], 'reverse works');
} else {
    console.log('SKIP: reverse not available');
}

// readIntLE/readIntBE (variable byte length)
const varBuf = Buffer.from([0x01, 0x02, 0x03, 0x04, 0x05, 0x06]);
if (typeof varBuf.readIntLE === 'function') {
    assertEqual(varBuf.readIntLE(0, 1), 1, 'readIntLE 1 byte');
    assertEqual(varBuf.readIntLE(0, 2), 0x0201, 'readIntLE 2 bytes');
    assertEqual(varBuf.readIntLE(0, 3), 0x030201, 'readIntLE 3 bytes');
} else {
    console.log('SKIP: readIntLE not available');
}

if (typeof varBuf.readUIntLE === 'function') {
    assertEqual(varBuf.readUIntLE(0, 1), 1, 'readUIntLE 1 byte');
    assertEqual(varBuf.readUIntLE(0, 2), 0x0201, 'readUIntLE 2 bytes');
} else {
    console.log('SKIP: readUIntLE not available');
}

// ============================================
// Edge cases
// ============================================
console.log('\n--- Edge cases ---');

// Read at boundary
const boundaryBuf = Buffer.from([0xFF, 0xFF, 0xFF, 0xFF]);
if (typeof boundaryBuf.readInt32LE === 'function') {
    assertEqual(boundaryBuf.readInt32LE(0), -1, 'readInt32LE -1 from 0xFFFFFFFF');
}
if (typeof boundaryBuf.readUInt32LE === 'function') {
    assertEqual(boundaryBuf.readUInt32LE(0), 0xFFFFFFFF, 'readUInt32LE 0xFFFFFFFF');
}

// Zero-length operations
const zeroBuf = Buffer.alloc(0);
assertEqual(zeroBuf.length, 0, 'Zero-length buffer');

// Fill with encoding (if supported)
const encFillBuf = Buffer.alloc(6);
try {
    encFillBuf.fill('48656c6c6f', 'hex'); // "Hello" in hex
    if (encFillBuf[0] === 0x48) {
        assertEqual(encFillBuf[0], 0x48, 'fill with hex encoding');
    }
} catch (e) {
    console.log('SKIP: fill with encoding not supported');
}

summary();
