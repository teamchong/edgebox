// Test native zlib decompression with Uint8Array
console.log("Testing zlib native bindings...");

// Gzip compressed "Hello, World!"
const gzipData = new Uint8Array([
  0x1f, 0x8b, 0x08, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x03,
  0xf3, 0x48, 0xcd, 0xc9, 0xc9, 0xd7, 0x51, 0x08, 0xcf, 0x2f,
  0xca, 0x49, 0x51, 0x04, 0x00, 0xd0, 0xc3, 0x4a, 0xd9, 0x0d,
  0x00, 0x00, 0x00
]);

// Test gunzip
if (typeof __edgebox_gunzip === 'function') {
  try {
    const decompressed = __edgebox_gunzip(gzipData);
    if (decompressed === "Hello, World!") {
      console.log("gunzip test PASSED");
    } else {
      console.log("gunzip test FAILED - unexpected output:", decompressed);
    }
  } catch (e) {
    console.log("gunzip test FAILED:", e.message || e);
  }
} else {
  console.log("__edgebox_gunzip not available");
}

// Zlib compressed "Hello, World!"
const zlibData = new Uint8Array([
  0x78, 0x9c, 0xf3, 0x48, 0xcd, 0xc9, 0xc9, 0xd7, 0x51, 0x08,
  0xcf, 0x2f, 0xca, 0x49, 0x51, 0x04, 0x00, 0x1f, 0x9e, 0x04,
  0x6a
]);

// Test inflateZlib
if (typeof __edgebox_inflate_zlib === 'function') {
  try {
    const decompressed = __edgebox_inflate_zlib(zlibData);
    if (decompressed === "Hello, World!") {
      console.log("inflateZlib test PASSED");
    } else {
      console.log("inflateZlib test FAILED - unexpected output:", decompressed);
    }
  } catch (e) {
    console.log("inflateZlib test FAILED:", e.message || e);
  }
} else {
  console.log("__edgebox_inflate_zlib not available");
}

// Raw deflate data
const deflateData = new Uint8Array([
  0xf3, 0x48, 0xcd, 0xc9, 0xc9, 0xd7, 0x51, 0x08,
  0xcf, 0x2f, 0xca, 0x49, 0x51, 0x04, 0x00
]);

// Test inflate
if (typeof __edgebox_inflate === 'function') {
  try {
    const decompressed = __edgebox_inflate(deflateData);
    if (decompressed === "Hello, World!") {
      console.log("inflate test PASSED");
    } else {
      console.log("inflate test FAILED - unexpected output:", decompressed);
    }
  } catch (e) {
    console.log("inflate test FAILED:", e.message || e);
  }
} else {
  console.log("__edgebox_inflate not available");
}

console.log("Zlib tests complete!");
