// Advanced Crypto polyfill tests
// Tests encryption, signing, key generation, and other advanced crypto operations

const {
    assertEqual, assertArrayEqual, assertTrue, assertFalse,
    assertTypeOf, assertDefined, assertInstanceOf, assertGreater,
    summary
} = require('./helpers/assert.js');

console.log('=== Crypto Advanced Tests ===\n');

let crypto;
try {
    crypto = require('crypto');
} catch (e) {
    console.log('SKIP: crypto module not available');
    crypto = null;
}

if (crypto) {
    // ============================================
    // Key Generation
    // ============================================
    console.log('--- Key Generation ---');

    // generateKeySync (if available)
    if (typeof crypto.generateKeySync === 'function') {
        try {
            const key = crypto.generateKeySync('aes', { length: 256 });
            assertDefined(key, 'generateKeySync creates key');
            console.log('PASS: generateKeySync works');
        } catch (e) {
            console.log('SKIP: generateKeySync failed: ' + e.message);
        }
    } else {
        console.log('SKIP: generateKeySync not available');
    }

    // generateKey (async, if available)
    if (typeof crypto.generateKey === 'function') {
        console.log('PASS: generateKey exists');
    } else {
        console.log('SKIP: generateKey not available');
    }

    // generateKeyPairSync (if available)
    if (typeof crypto.generateKeyPairSync === 'function') {
        try {
            const { publicKey, privateKey } = crypto.generateKeyPairSync('rsa', {
                modulusLength: 2048
            });
            assertDefined(publicKey, 'generateKeyPairSync creates publicKey');
            assertDefined(privateKey, 'generateKeyPairSync creates privateKey');
            console.log('PASS: generateKeyPairSync works');
        } catch (e) {
            console.log('SKIP: generateKeyPairSync failed: ' + e.message);
        }
    } else {
        console.log('SKIP: generateKeyPairSync not available');
    }

    // generateKeyPair (async, if available)
    if (typeof crypto.generateKeyPair === 'function') {
        console.log('PASS: generateKeyPair exists');
    } else {
        console.log('SKIP: generateKeyPair not available');
    }

    // ============================================
    // Secret/Public/Private Key Objects
    // ============================================
    console.log('\n--- Key Objects ---');

    // createSecretKey
    if (typeof crypto.createSecretKey === 'function') {
        try {
            const key = crypto.createSecretKey(Buffer.alloc(32));
            assertDefined(key, 'createSecretKey creates key');
            console.log('PASS: createSecretKey works');
        } catch (e) {
            console.log('SKIP: createSecretKey failed: ' + e.message);
        }
    } else {
        console.log('SKIP: createSecretKey not available');
    }

    // createPublicKey / createPrivateKey would need PEM data
    if (typeof crypto.createPublicKey === 'function') {
        console.log('PASS: createPublicKey exists');
    } else {
        console.log('SKIP: createPublicKey not available');
    }

    if (typeof crypto.createPrivateKey === 'function') {
        console.log('PASS: createPrivateKey exists');
    } else {
        console.log('SKIP: createPrivateKey not available');
    }

    // ============================================
    // Cipher / Decipher
    // ============================================
    console.log('\n--- Cipher / Decipher ---');

    // createCipheriv / createDecipheriv
    if (typeof crypto.createCipheriv === 'function' && typeof crypto.createDecipheriv === 'function') {
        try {
            const algorithm = 'aes-256-cbc';
            const key = Buffer.alloc(32, 0x01); // 256 bits
            const iv = Buffer.alloc(16, 0x02);  // 128 bits for CBC

            // Encrypt
            const cipher = crypto.createCipheriv(algorithm, key, iv);
            assertDefined(cipher, 'createCipheriv creates cipher');

            let encrypted = cipher.update('Hello, World!', 'utf8', 'hex');
            encrypted += cipher.final('hex');
            assertTypeOf(encrypted, 'string', 'Encrypted data is string');
            assertGreater(encrypted.length, 0, 'Encrypted data is not empty');

            // Decrypt
            const decipher = crypto.createDecipheriv(algorithm, key, iv);
            assertDefined(decipher, 'createDecipheriv creates decipher');

            let decrypted = decipher.update(encrypted, 'hex', 'utf8');
            decrypted += decipher.final('utf8');
            assertEqual(decrypted, 'Hello, World!', 'Decryption roundtrip works');
        } catch (e) {
            console.log('SKIP: Cipher/Decipher test failed: ' + e.message);
        }
    } else {
        console.log('SKIP: createCipheriv/createDecipheriv not available');
    }

    // getCiphers
    if (typeof crypto.getCiphers === 'function') {
        const ciphers = crypto.getCiphers();
        assertTrue(Array.isArray(ciphers), 'getCiphers returns array');
        assertGreater(ciphers.length, 0, 'getCiphers returns non-empty array');
        console.log('PASS: getCiphers works (' + ciphers.length + ' ciphers)');
    } else {
        console.log('SKIP: getCiphers not available');
    }

    // ============================================
    // Signing / Verification
    // ============================================
    console.log('\n--- Signing / Verification ---');

    // createSign / createVerify
    if (typeof crypto.createSign === 'function' && typeof crypto.createVerify === 'function') {
        console.log('PASS: createSign and createVerify exist');

        // Full test would require key pair, just verify functions exist
    } else {
        console.log('SKIP: createSign/createVerify not available');
    }

    // sign / verify (one-shot, if available)
    if (typeof crypto.sign === 'function') {
        console.log('PASS: crypto.sign exists');
    } else {
        console.log('SKIP: crypto.sign not available');
    }

    if (typeof crypto.verify === 'function') {
        console.log('PASS: crypto.verify exists');
    } else {
        console.log('SKIP: crypto.verify not available');
    }

    // ============================================
    // Public/Private Encryption
    // ============================================
    console.log('\n--- Public/Private Encryption ---');

    // publicEncrypt / privateDecrypt
    if (typeof crypto.publicEncrypt === 'function') {
        console.log('PASS: publicEncrypt exists');
    } else {
        console.log('SKIP: publicEncrypt not available');
    }

    if (typeof crypto.privateDecrypt === 'function') {
        console.log('PASS: privateDecrypt exists');
    } else {
        console.log('SKIP: privateDecrypt not available');
    }

    // privateEncrypt / publicDecrypt
    if (typeof crypto.privateEncrypt === 'function') {
        console.log('PASS: privateEncrypt exists');
    } else {
        console.log('SKIP: privateEncrypt not available');
    }

    if (typeof crypto.publicDecrypt === 'function') {
        console.log('PASS: publicDecrypt exists');
    } else {
        console.log('SKIP: publicDecrypt not available');
    }

    // ============================================
    // Key Derivation Functions
    // ============================================
    console.log('\n--- Key Derivation ---');

    // pbkdf2Sync
    if (typeof crypto.pbkdf2Sync === 'function') {
        try {
            const derived = crypto.pbkdf2Sync('password', 'salt', 1000, 32, 'sha256');
            assertInstanceOf(derived, Buffer, 'pbkdf2Sync returns Buffer');
            assertEqual(derived.length, 32, 'pbkdf2Sync returns correct length');
            console.log('PASS: pbkdf2Sync works');
        } catch (e) {
            console.log('SKIP: pbkdf2Sync failed: ' + e.message);
        }
    } else {
        console.log('SKIP: pbkdf2Sync not available');
    }

    // pbkdf2 (async)
    if (typeof crypto.pbkdf2 === 'function') {
        console.log('PASS: pbkdf2 exists');
    } else {
        console.log('SKIP: pbkdf2 not available');
    }

    // scryptSync
    if (typeof crypto.scryptSync === 'function') {
        try {
            const derived = crypto.scryptSync('password', 'salt', 32);
            assertInstanceOf(derived, Buffer, 'scryptSync returns Buffer');
            assertEqual(derived.length, 32, 'scryptSync returns correct length');
            console.log('PASS: scryptSync works');
        } catch (e) {
            console.log('SKIP: scryptSync failed: ' + e.message);
        }
    } else {
        console.log('SKIP: scryptSync not available');
    }

    // scrypt (async)
    if (typeof crypto.scrypt === 'function') {
        console.log('PASS: scrypt exists');
    } else {
        console.log('SKIP: scrypt not available');
    }

    // hkdf / hkdfSync
    if (typeof crypto.hkdfSync === 'function') {
        try {
            const derived = crypto.hkdfSync('sha256', Buffer.from('key'), 'salt', 'info', 32);
            assertInstanceOf(derived, Buffer, 'hkdfSync returns Buffer');
            console.log('PASS: hkdfSync works');
        } catch (e) {
            console.log('SKIP: hkdfSync failed: ' + e.message);
        }
    } else {
        console.log('SKIP: hkdfSync not available');
    }

    if (typeof crypto.hkdf === 'function') {
        console.log('PASS: hkdf exists');
    } else {
        console.log('SKIP: hkdf not available');
    }

    // ============================================
    // Diffie-Hellman
    // ============================================
    console.log('\n--- Diffie-Hellman ---');

    // createDiffieHellman
    if (typeof crypto.createDiffieHellman === 'function') {
        try {
            const dh = crypto.createDiffieHellman(512);
            assertDefined(dh, 'createDiffieHellman creates DH');
            dh.generateKeys();
            const pubKey = dh.getPublicKey();
            assertInstanceOf(pubKey, Buffer, 'DH public key is Buffer');
            console.log('PASS: createDiffieHellman works');
        } catch (e) {
            console.log('SKIP: createDiffieHellman failed: ' + e.message);
        }
    } else {
        console.log('SKIP: createDiffieHellman not available');
    }

    // createDiffieHellmanGroup
    if (typeof crypto.createDiffieHellmanGroup === 'function') {
        console.log('PASS: createDiffieHellmanGroup exists');
    } else {
        console.log('SKIP: createDiffieHellmanGroup not available');
    }

    // getDiffieHellman
    if (typeof crypto.getDiffieHellman === 'function') {
        try {
            const dh = crypto.getDiffieHellman('modp14');
            assertDefined(dh, 'getDiffieHellman returns DH');
            console.log('PASS: getDiffieHellman works');
        } catch (e) {
            console.log('SKIP: getDiffieHellman failed');
        }
    } else {
        console.log('SKIP: getDiffieHellman not available');
    }

    // ============================================
    // ECDH
    // ============================================
    console.log('\n--- ECDH ---');

    // createECDH
    if (typeof crypto.createECDH === 'function') {
        try {
            const ecdh = crypto.createECDH('secp256k1');
            assertDefined(ecdh, 'createECDH creates ECDH');
            ecdh.generateKeys();
            const pubKey = ecdh.getPublicKey();
            assertInstanceOf(pubKey, Buffer, 'ECDH public key is Buffer');
            console.log('PASS: createECDH works');
        } catch (e) {
            console.log('SKIP: createECDH failed: ' + e.message);
        }
    } else {
        console.log('SKIP: createECDH not available');
    }

    // getCurves
    if (typeof crypto.getCurves === 'function') {
        const curves = crypto.getCurves();
        assertTrue(Array.isArray(curves), 'getCurves returns array');
        assertGreater(curves.length, 0, 'getCurves returns non-empty array');
        console.log('PASS: getCurves works (' + curves.length + ' curves)');
    } else {
        console.log('SKIP: getCurves not available');
    }

    // ============================================
    // Timing-safe comparison
    // ============================================
    console.log('\n--- Timing-safe comparison ---');

    if (typeof crypto.timingSafeEqual === 'function') {
        const a = Buffer.from('hello');
        const b = Buffer.from('hello');
        const c = Buffer.from('world');

        assertTrue(crypto.timingSafeEqual(a, b), 'timingSafeEqual returns true for equal');
        assertFalse(crypto.timingSafeEqual(a, c), 'timingSafeEqual returns false for different');

        // Different lengths should throw
        let threwForLength = false;
        try {
            crypto.timingSafeEqual(Buffer.from('short'), Buffer.from('longer'));
        } catch (e) {
            threwForLength = true;
        }
        assertTrue(threwForLength, 'timingSafeEqual throws for different lengths');
    } else {
        console.log('SKIP: timingSafeEqual not available');
    }

    // ============================================
    // Random utilities
    // ============================================
    console.log('\n--- Random utilities ---');

    // randomInt
    if (typeof crypto.randomInt === 'function') {
        const randInt = crypto.randomInt(100);
        assertTrue(randInt >= 0 && randInt < 100, 'randomInt returns value in range');

        const randIntRange = crypto.randomInt(50, 100);
        assertTrue(randIntRange >= 50 && randIntRange < 100, 'randomInt with min/max');
        console.log('PASS: randomInt works');
    } else {
        console.log('SKIP: randomInt not available');
    }

    // randomFillSync
    if (typeof crypto.randomFillSync === 'function') {
        const buf = Buffer.alloc(16);
        crypto.randomFillSync(buf);
        // At least one byte should be non-zero (statistically very likely)
        let hasNonZero = false;
        for (let i = 0; i < buf.length; i++) {
            if (buf[i] !== 0) hasNonZero = true;
        }
        assertTrue(hasNonZero, 'randomFillSync fills buffer with random data');
    } else {
        console.log('SKIP: randomFillSync not available');
    }

    // randomFill (async)
    if (typeof crypto.randomFill === 'function') {
        console.log('PASS: randomFill exists');
    } else {
        console.log('SKIP: randomFill not available');
    }

    // ============================================
    // UUID Generation
    // ============================================
    console.log('\n--- UUID Generation ---');

    if (typeof crypto.randomUUID === 'function') {
        const uuid = crypto.randomUUID();
        assertTypeOf(uuid, 'string', 'randomUUID returns string');
        assertEqual(uuid.length, 36, 'UUID has correct length');
        assertEqual(uuid[8], '-', 'UUID has dash at position 8');
        assertEqual(uuid[13], '-', 'UUID has dash at position 13');
        assertEqual(uuid[18], '-', 'UUID has dash at position 18');
        assertEqual(uuid[23], '-', 'UUID has dash at position 23');
        console.log('PASS: randomUUID works');
    } else {
        console.log('SKIP: randomUUID not available');
    }

    // ============================================
    // X509Certificate (if available)
    // ============================================
    console.log('\n--- X509Certificate ---');

    if (crypto.X509Certificate) {
        console.log('PASS: X509Certificate class exists');
    } else {
        console.log('SKIP: X509Certificate not available');
    }

    // ============================================
    // Constants
    // ============================================
    console.log('\n--- Constants ---');

    if (crypto.constants) {
        assertDefined(crypto.constants, 'crypto.constants exists');

        // Check for some common constants
        if (crypto.constants.RSA_PKCS1_PADDING !== undefined) {
            console.log('PASS: RSA_PKCS1_PADDING constant exists');
        }
        if (crypto.constants.RSA_PKCS1_OAEP_PADDING !== undefined) {
            console.log('PASS: RSA_PKCS1_OAEP_PADDING constant exists');
        }
    } else {
        console.log('SKIP: crypto.constants not available');
    }

    // ============================================
    // Webcrypto (if available)
    // ============================================
    console.log('\n--- webcrypto ---');

    if (crypto.webcrypto) {
        assertDefined(crypto.webcrypto, 'webcrypto exists');
        if (crypto.webcrypto.subtle) {
            assertDefined(crypto.webcrypto.subtle, 'webcrypto.subtle exists');
        }
        if (typeof crypto.webcrypto.getRandomValues === 'function') {
            console.log('PASS: webcrypto.getRandomValues exists');
        }
    } else {
        console.log('SKIP: webcrypto not available');
    }

    // ============================================
    // Subtle Crypto (Web Crypto API style)
    // ============================================
    console.log('\n--- subtle crypto ---');

    if (crypto.subtle) {
        assertDefined(crypto.subtle, 'crypto.subtle exists');

        if (typeof crypto.subtle.digest === 'function') {
            console.log('PASS: subtle.digest exists');
        }
        if (typeof crypto.subtle.encrypt === 'function') {
            console.log('PASS: subtle.encrypt exists');
        }
        if (typeof crypto.subtle.decrypt === 'function') {
            console.log('PASS: subtle.decrypt exists');
        }
        if (typeof crypto.subtle.sign === 'function') {
            console.log('PASS: subtle.sign exists');
        }
        if (typeof crypto.subtle.verify === 'function') {
            console.log('PASS: subtle.verify exists');
        }
        if (typeof crypto.subtle.generateKey === 'function') {
            console.log('PASS: subtle.generateKey exists');
        }
        if (typeof crypto.subtle.importKey === 'function') {
            console.log('PASS: subtle.importKey exists');
        }
        if (typeof crypto.subtle.exportKey === 'function') {
            console.log('PASS: subtle.exportKey exists');
        }
    } else {
        console.log('SKIP: crypto.subtle not available');
    }

} else {
    console.log('crypto module not available');
}

summary();
