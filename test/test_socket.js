// Test WASI socket support
print("Testing socket support...");

// Check if __edgebox_fetch is available
if (typeof globalThis.__edgebox_fetch !== 'undefined') {
    print("Native fetch binding available!");
} else {
    print("Native fetch binding NOT available - need to register it");
}

// Test fetch polyfill
if (typeof fetch !== 'undefined') {
    print("fetch() polyfill available!");
} else {
    print("fetch() NOT available");
}

print("Done!");
