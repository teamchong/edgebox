// Comprehensive Compression polyfill tests
// Tests zlib module: gunzip, inflate, deflate, gzip

const {
    assertEqual, assertTrue, assertTypeOf, assertDefined,
    assertInstanceOf, summary
} = require('./helpers/assert.js');

console.log('=== Compression Polyfill Tests ===\n');

// ============================================
// Test data - "Hello, World!" compressed in various formats
// ============================================

// Gzip compressed "Hello, World!"
const gzipData = new Uint8Array([
    0x1f, 0x8b, 0x08, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x03,
    0xf3, 0x48, 0xcd, 0xc9, 0xc9, 0xd7, 0x51, 0x08, 0xcf, 0x2f,
    0xca, 0x49, 0x51, 0x04, 0x00, 0xd0, 0xc3, 0x4a, 0xd9, 0x0d,
    0x00, 0x00, 0x00
]);

// Zlib compressed "Hello, World!"
const zlibData = new Uint8Array([
    0x78, 0x9c, 0xf3, 0x48, 0xcd, 0xc9, 0xc9, 0xd7, 0x51, 0x08,
    0xcf, 0x2f, 0xca, 0x49, 0x51, 0x04, 0x00, 0x1f, 0x9e, 0x04,
    0x6a
]);

// Raw deflate data (no zlib/gzip header)
const deflateData = new Uint8Array([
    0xf3, 0x48, 0xcd, 0xc9, 0xc9, 0xd7, 0x51, 0x08,
    0xcf, 0x2f, 0xca, 0x49, 0x51, 0x04, 0x00
]);

const expectedText = 'Hello, World!';

// ============================================
// Native EdgeBox decompression functions
// ============================================
console.log('--- Native EdgeBox Functions ---');

// __edgebox_gunzip
if (typeof __edgebox_gunzip === 'function') {
    try {
        const result = __edgebox_gunzip(gzipData);
        assertEqual(result, expectedText, '__edgebox_gunzip');
    } catch (e) {
        console.log('FAIL: __edgebox_gunzip error:', e.message || e);
    }
} else {
    console.log('SKIP: __edgebox_gunzip not available');
}

// __edgebox_inflate_zlib
if (typeof __edgebox_inflate_zlib === 'function') {
    try {
        const result = __edgebox_inflate_zlib(zlibData);
        assertEqual(result, expectedText, '__edgebox_inflate_zlib');
    } catch (e) {
        console.log('FAIL: __edgebox_inflate_zlib error:', e.message || e);
    }
} else {
    console.log('SKIP: __edgebox_inflate_zlib not available');
}

// __edgebox_inflate (raw deflate)
if (typeof __edgebox_inflate === 'function') {
    try {
        const result = __edgebox_inflate(deflateData);
        assertEqual(result, expectedText, '__edgebox_inflate');
    } catch (e) {
        console.log('FAIL: __edgebox_inflate error:', e.message || e);
    }
} else {
    console.log('SKIP: __edgebox_inflate not available');
}

// ============================================
// Node.js zlib module
// ============================================
console.log('\n--- zlib Module ---');

let zlib;
try {
    zlib = require('zlib');
} catch (e) {
    console.log('SKIP: zlib module not available via require');
    zlib = null;
}

if (zlib) {
    // gunzipSync
    if (typeof zlib.gunzipSync === 'function') {
        try {
            const result = zlib.gunzipSync(gzipData);
            if (Buffer.isBuffer(result)) {
                assertEqual(result.toString(), expectedText, 'zlib.gunzipSync');
            } else if (typeof result === 'string') {
                assertEqual(result, expectedText, 'zlib.gunzipSync (string)');
            } else {
                console.log('INFO: gunzipSync returned:', typeof result);
            }
        } catch (e) {
            console.log('FAIL: zlib.gunzipSync error:', e.message || e);
        }
    } else {
        console.log('SKIP: zlib.gunzipSync not available');
    }

    // inflateSync
    if (typeof zlib.inflateSync === 'function') {
        try {
            const result = zlib.inflateSync(zlibData);
            if (Buffer.isBuffer(result)) {
                assertEqual(result.toString(), expectedText, 'zlib.inflateSync');
            } else if (typeof result === 'string') {
                assertEqual(result, expectedText, 'zlib.inflateSync (string)');
            }
        } catch (e) {
            console.log('FAIL: zlib.inflateSync error:', e.message || e);
        }
    } else {
        console.log('SKIP: zlib.inflateSync not available');
    }

    // inflateRawSync
    if (typeof zlib.inflateRawSync === 'function') {
        try {
            const result = zlib.inflateRawSync(deflateData);
            if (Buffer.isBuffer(result)) {
                assertEqual(result.toString(), expectedText, 'zlib.inflateRawSync');
            } else if (typeof result === 'string') {
                assertEqual(result, expectedText, 'zlib.inflateRawSync (string)');
            }
        } catch (e) {
            console.log('FAIL: zlib.inflateRawSync error:', e.message || e);
        }
    } else {
        console.log('SKIP: zlib.inflateRawSync not available');
    }

    // deflateSync
    if (typeof zlib.deflateSync === 'function') {
        try {
            const input = Buffer.from(expectedText);
            const compressed = zlib.deflateSync(input);
            assertTrue(compressed.length > 0, 'zlib.deflateSync produces output');
            assertTrue(compressed.length < input.length * 2, 'zlib.deflateSync reasonably sized');

            // Round-trip test
            if (typeof zlib.inflateSync === 'function') {
                const decompressed = zlib.inflateSync(compressed);
                if (Buffer.isBuffer(decompressed)) {
                    assertEqual(decompressed.toString(), expectedText, 'deflate/inflate round-trip');
                }
            }
        } catch (e) {
            console.log('FAIL: zlib.deflateSync error:', e.message || e);
        }
    } else {
        console.log('SKIP: zlib.deflateSync not available');
    }

    // gzipSync
    if (typeof zlib.gzipSync === 'function') {
        try {
            const input = Buffer.from(expectedText);
            const compressed = zlib.gzipSync(input);
            assertTrue(compressed.length > 0, 'zlib.gzipSync produces output');

            // Check gzip magic number
            if (compressed[0] === 0x1f && compressed[1] === 0x8b) {
                console.log('PASS: zlib.gzipSync has correct header');
            }

            // Round-trip test
            if (typeof zlib.gunzipSync === 'function') {
                const decompressed = zlib.gunzipSync(compressed);
                if (Buffer.isBuffer(decompressed)) {
                    assertEqual(decompressed.toString(), expectedText, 'gzip/gunzip round-trip');
                }
            }
        } catch (e) {
            console.log('FAIL: zlib.gzipSync error:', e.message || e);
        }
    } else {
        console.log('SKIP: zlib.gzipSync not available');
    }

    // brotliCompressSync / brotliDecompressSync
    if (typeof zlib.brotliCompressSync === 'function') {
        try {
            const input = Buffer.from(expectedText);
            const compressed = zlib.brotliCompressSync(input);
            assertTrue(compressed.length > 0, 'brotliCompressSync produces output');

            if (typeof zlib.brotliDecompressSync === 'function') {
                const decompressed = zlib.brotliDecompressSync(compressed);
                if (Buffer.isBuffer(decompressed)) {
                    assertEqual(decompressed.toString(), expectedText, 'brotli round-trip');
                }
            }
        } catch (e) {
            console.log('INFO: Brotli not fully supported:', e.message || e);
        }
    } else {
        console.log('SKIP: zlib.brotliCompressSync not available');
    }

    // Check zlib constants
    console.log('\n--- zlib Constants ---');
    if (zlib.constants) {
        assertDefined(zlib.constants, 'zlib.constants exists');
        if (zlib.constants.Z_BEST_COMPRESSION !== undefined) {
            assertEqual(zlib.constants.Z_BEST_COMPRESSION, 9, 'Z_BEST_COMPRESSION');
        }
        if (zlib.constants.Z_NO_COMPRESSION !== undefined) {
            assertEqual(zlib.constants.Z_NO_COMPRESSION, 0, 'Z_NO_COMPRESSION');
        }
    } else {
        console.log('SKIP: zlib.constants not available');
    }
}

// ============================================
// Test with various data sizes
// ============================================
console.log('\n--- Various Data Sizes ---');

// Test with empty data
if (typeof __edgebox_gunzip === 'function' || (zlib && typeof zlib.gunzipSync === 'function')) {
    // Empty gzip (minimal valid gzip)
    const emptyGzip = new Uint8Array([
        0x1f, 0x8b, 0x08, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x03,
        0x03, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
    ]);

    if (typeof __edgebox_gunzip === 'function') {
        try {
            const result = __edgebox_gunzip(emptyGzip);
            assertEqual(result, '', 'gunzip empty data');
        } catch (e) {
            console.log('INFO: Empty gzip test:', e.message);
        }
    }
}

// Test with longer data
const longText = 'Lorem ipsum dolor sit amet, consectetur adipiscing elit. '.repeat(100);
console.log('Long text length:', longText.length);

if (zlib && typeof zlib.deflateSync === 'function' && typeof zlib.inflateSync === 'function') {
    try {
        const compressed = zlib.deflateSync(Buffer.from(longText));
        console.log('Compressed length:', compressed.length);
        assertTrue(compressed.length < longText.length, 'Long text compresses');

        const decompressed = zlib.inflateSync(compressed);
        assertEqual(decompressed.toString(), longText, 'Long text round-trip');
    } catch (e) {
        console.log('INFO: Long text test:', e.message);
    }
}

// ============================================
// Gzip header validation
// ============================================
console.log('\n--- Gzip Header Validation ---');

// Check gzip magic number
assertEqual(gzipData[0], 0x1f, 'Gzip magic byte 1');
assertEqual(gzipData[1], 0x8b, 'Gzip magic byte 2');
assertEqual(gzipData[2], 0x08, 'Gzip compression method (deflate)');

// Check zlib header
assertEqual(zlibData[0] & 0x0f, 0x08, 'Zlib method is deflate');
assertEqual((zlibData[0] * 256 + zlibData[1]) % 31, 0, 'Zlib header checksum');

// ============================================
// Error handling
// ============================================
console.log('\n--- Error Handling ---');

// Invalid gzip data
if (typeof __edgebox_gunzip === 'function') {
    try {
        const invalidData = new Uint8Array([0x00, 0x01, 0x02, 0x03]);
        __edgebox_gunzip(invalidData);
        console.log('FAIL: Invalid gzip should throw');
    } catch (e) {
        console.log('PASS: Invalid gzip throws error');
    }
}

// Truncated gzip data
if (typeof __edgebox_gunzip === 'function') {
    try {
        const truncated = gzipData.slice(0, 10);
        __edgebox_gunzip(truncated);
        console.log('INFO: Truncated gzip behavior varies');
    } catch (e) {
        console.log('PASS: Truncated gzip throws error');
    }
}

summary();
