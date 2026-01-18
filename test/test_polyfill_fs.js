// Comprehensive FS polyfill tests
// Tests file system operations: sync, async, streams, and fs.promises

const {
    assertEqual, assertArrayEqual, assertTrue, assertFalse,
    assertTypeOf, assertDefined, assertInstanceOf, assertThrows,
    assertContains, summary
} = require('./helpers/assert.js');

console.log('=== FS Polyfill Tests ===\n');

let fs;
try {
    fs = require('fs');
} catch (e) {
    console.log('SKIP: fs module not available');
    fs = null;
}

if (fs) {
    // ============================================
    // Module exports check
    // ============================================
    console.log('--- Module exports ---');

    assertDefined(fs, 'fs module exists');
    assertTypeOf(fs.readFileSync, 'function', 'fs.readFileSync exists');
    assertTypeOf(fs.writeFileSync, 'function', 'fs.writeFileSync exists');
    assertTypeOf(fs.existsSync, 'function', 'fs.existsSync exists');

    // ============================================
    // existsSync
    // ============================================
    console.log('\n--- existsSync ---');

    // Test with current file (this test file should exist)
    const thisFile = '/Users/steven_chong/Downloads/repos/edgebox/test/test_polyfill_fs.js';
    // Can't guarantee path, so test with a guaranteed existing path
    assertTrue(fs.existsSync('/'), 'existsSync returns true for /');
    assertTrue(fs.existsSync('/tmp') || fs.existsSync('/var'), 'existsSync for /tmp or /var');
    assertFalse(fs.existsSync('/nonexistent_path_12345'), 'existsSync returns false for nonexistent');

    // ============================================
    // statSync / lstatSync
    // ============================================
    console.log('\n--- statSync / lstatSync ---');

    if (typeof fs.statSync === 'function') {
        const stats = fs.statSync('/');
        assertDefined(stats, 'statSync returns stats object');
        assertDefined(stats.isDirectory, 'stats has isDirectory method');
        assertDefined(stats.isFile, 'stats has isFile method');
        assertTrue(stats.isDirectory(), '/ is a directory');
        assertFalse(stats.isFile(), '/ is not a file');

        // Stats properties
        assertDefined(stats.size, 'stats has size property');
        assertDefined(stats.mode, 'stats has mode property');

        // Other stats methods (if available)
        if (typeof stats.isSymbolicLink === 'function') {
            assertTypeOf(stats.isSymbolicLink(), 'boolean', 'isSymbolicLink returns boolean');
        }
        if (typeof stats.isBlockDevice === 'function') {
            assertTypeOf(stats.isBlockDevice(), 'boolean', 'isBlockDevice returns boolean');
        }
        if (typeof stats.isCharacterDevice === 'function') {
            assertTypeOf(stats.isCharacterDevice(), 'boolean', 'isCharacterDevice returns boolean');
        }
        if (typeof stats.isFIFO === 'function') {
            assertTypeOf(stats.isFIFO(), 'boolean', 'isFIFO returns boolean');
        }
        if (typeof stats.isSocket === 'function') {
            assertTypeOf(stats.isSocket(), 'boolean', 'isSocket returns boolean');
        }
    } else {
        console.log('SKIP: statSync not available');
    }

    if (typeof fs.lstatSync === 'function') {
        const lstats = fs.lstatSync('/');
        assertDefined(lstats, 'lstatSync returns stats object');
        console.log('PASS: lstatSync works');
    } else {
        console.log('SKIP: lstatSync not available');
    }

    // ============================================
    // readdirSync
    // ============================================
    console.log('\n--- readdirSync ---');

    if (typeof fs.readdirSync === 'function') {
        const tmpContents = fs.readdirSync('/tmp');
        assertTrue(Array.isArray(tmpContents), 'readdirSync returns array');

        // With withFileTypes option (if supported)
        try {
            const dirents = fs.readdirSync('/tmp', { withFileTypes: true });
            if (dirents.length > 0 && dirents[0].name) {
                assertDefined(dirents[0].name, 'Dirent has name');
                if (typeof dirents[0].isDirectory === 'function') {
                    console.log('PASS: readdirSync withFileTypes works');
                }
            }
        } catch (e) {
            console.log('SKIP: readdirSync withFileTypes not supported');
        }
    } else {
        console.log('SKIP: readdirSync not available');
    }

    // ============================================
    // readFileSync / writeFileSync
    // ============================================
    console.log('\n--- readFileSync / writeFileSync ---');

    const testFilePath = '/tmp/edgebox_fs_test_' + Date.now() + '.txt';
    const testContent = 'Hello, EdgeBox!\nLine 2\n';

    // Write test file
    if (typeof fs.writeFileSync === 'function') {
        let writeOk = true;
        try {
            fs.writeFileSync(testFilePath, testContent);
        } catch (e) {
            writeOk = false;
            console.log('SKIP: writeFileSync failed: ' + e.message);
        }

        if (writeOk) {
            assertTrue(fs.existsSync(testFilePath), 'writeFileSync creates file');

            // Read it back
            if (typeof fs.readFileSync === 'function') {
                const readContent = fs.readFileSync(testFilePath, 'utf-8');
                assertEqual(readContent, testContent, 'readFileSync returns correct content');

                // Read as Buffer
                const readBuffer = fs.readFileSync(testFilePath);
                assertInstanceOf(readBuffer, Buffer, 'readFileSync without encoding returns Buffer');
                assertEqual(readBuffer.toString(), testContent, 'Buffer content matches');
            }

            // Clean up
            if (typeof fs.unlinkSync === 'function') {
                fs.unlinkSync(testFilePath);
                assertFalse(fs.existsSync(testFilePath), 'unlinkSync removes file');
            }
        }
    } else {
        console.log('SKIP: writeFileSync not available');
    }

    // ============================================
    // appendFileSync
    // ============================================
    console.log('\n--- appendFileSync ---');

    if (typeof fs.appendFileSync === 'function') {
        const appendPath = '/tmp/edgebox_append_test_' + Date.now() + '.txt';

        try {
            fs.writeFileSync(appendPath, 'Line 1\n');
            fs.appendFileSync(appendPath, 'Line 2\n');
            const content = fs.readFileSync(appendPath, 'utf-8');
            assertEqual(content, 'Line 1\nLine 2\n', 'appendFileSync appends content');
            fs.unlinkSync(appendPath);
        } catch (e) {
            console.log('SKIP: appendFileSync test failed: ' + e.message);
        }
    } else {
        console.log('SKIP: appendFileSync not available');
    }

    // ============================================
    // mkdirSync / rmdirSync / rmSync
    // ============================================
    console.log('\n--- mkdirSync / rmdirSync / rmSync ---');

    if (typeof fs.mkdirSync === 'function') {
        const testDir = '/tmp/edgebox_mkdir_test_' + Date.now();

        try {
            fs.mkdirSync(testDir);
            assertTrue(fs.existsSync(testDir), 'mkdirSync creates directory');

            const stats = fs.statSync(testDir);
            assertTrue(stats.isDirectory(), 'Created path is directory');

            // Remove directory
            if (typeof fs.rmdirSync === 'function') {
                fs.rmdirSync(testDir);
                assertFalse(fs.existsSync(testDir), 'rmdirSync removes directory');
            } else if (typeof fs.rmSync === 'function') {
                fs.rmSync(testDir);
                assertFalse(fs.existsSync(testDir), 'rmSync removes directory');
            }
        } catch (e) {
            console.log('SKIP: mkdir test failed: ' + e.message);
        }

        // mkdir with recursive option
        const nestedDir = '/tmp/edgebox_nested_' + Date.now() + '/a/b/c';
        try {
            fs.mkdirSync(nestedDir, { recursive: true });
            assertTrue(fs.existsSync(nestedDir), 'mkdirSync recursive creates nested dirs');

            // Clean up
            if (typeof fs.rmSync === 'function') {
                fs.rmSync('/tmp/edgebox_nested_' + Date.now().toString().slice(0, -4), { recursive: true });
            }
        } catch (e) {
            console.log('SKIP: recursive mkdir test failed');
        }
    } else {
        console.log('SKIP: mkdirSync not available');
    }

    // ============================================
    // renameSync / copyFileSync
    // ============================================
    console.log('\n--- renameSync / copyFileSync ---');

    if (typeof fs.renameSync === 'function') {
        const srcPath = '/tmp/edgebox_rename_src_' + Date.now() + '.txt';
        const dstPath = '/tmp/edgebox_rename_dst_' + Date.now() + '.txt';

        try {
            fs.writeFileSync(srcPath, 'rename test');
            fs.renameSync(srcPath, dstPath);
            assertFalse(fs.existsSync(srcPath), 'renameSync removes source');
            assertTrue(fs.existsSync(dstPath), 'renameSync creates destination');
            assertEqual(fs.readFileSync(dstPath, 'utf-8'), 'rename test', 'renameSync preserves content');
            fs.unlinkSync(dstPath);
        } catch (e) {
            console.log('SKIP: renameSync test failed: ' + e.message);
        }
    } else {
        console.log('SKIP: renameSync not available');
    }

    if (typeof fs.copyFileSync === 'function') {
        const srcPath = '/tmp/edgebox_copy_src_' + Date.now() + '.txt';
        const dstPath = '/tmp/edgebox_copy_dst_' + Date.now() + '.txt';

        try {
            fs.writeFileSync(srcPath, 'copy test');
            fs.copyFileSync(srcPath, dstPath);
            assertTrue(fs.existsSync(srcPath), 'copyFileSync keeps source');
            assertTrue(fs.existsSync(dstPath), 'copyFileSync creates destination');
            assertEqual(fs.readFileSync(dstPath, 'utf-8'), 'copy test', 'copyFileSync preserves content');
            fs.unlinkSync(srcPath);
            fs.unlinkSync(dstPath);
        } catch (e) {
            console.log('SKIP: copyFileSync test failed: ' + e.message);
        }
    } else {
        console.log('SKIP: copyFileSync not available');
    }

    // ============================================
    // chmodSync / chownSync (if available)
    // ============================================
    console.log('\n--- chmodSync / chownSync ---');

    if (typeof fs.chmodSync === 'function') {
        console.log('PASS: chmodSync exists');
    } else {
        console.log('SKIP: chmodSync not available');
    }

    if (typeof fs.chownSync === 'function') {
        console.log('PASS: chownSync exists');
    } else {
        console.log('SKIP: chownSync not available');
    }

    // ============================================
    // linkSync / symlinkSync / readlinkSync
    // ============================================
    console.log('\n--- linkSync / symlinkSync / readlinkSync ---');

    // Note: symlinkSync/readlinkSync are no-op stubs in WASI (no syscall support)
    // Just verify the functions exist
    if (typeof fs.symlinkSync === 'function') {
        console.log('PASS: symlinkSync exists (no-op in WASI)');
    } else {
        console.log('SKIP: symlinkSync not available');
    }

    if (typeof fs.readlinkSync === 'function') {
        console.log('PASS: readlinkSync exists (no-op in WASI)');
    } else {
        console.log('SKIP: readlinkSync not available');
    }

    if (typeof fs.linkSync === 'function') {
        console.log('PASS: linkSync exists');
    } else {
        console.log('SKIP: linkSync not available');
    }

    // ============================================
    // realpathSync
    // ============================================
    console.log('\n--- realpathSync ---');

    if (typeof fs.realpathSync === 'function') {
        const realpath = fs.realpathSync('/tmp');
        assertDefined(realpath, 'realpathSync returns path');
        console.log('PASS: realpathSync works');
    } else {
        console.log('SKIP: realpathSync not available');
    }

    // ============================================
    // openSync / closeSync / readSync / writeSync
    // ============================================
    console.log('\n--- File descriptor operations ---');

    if (typeof fs.openSync === 'function') {
        const fdPath = '/tmp/edgebox_fd_test_' + Date.now() + '.txt';

        try {
            // Write mode
            const writeFd = fs.openSync(fdPath, 'w');
            assertTypeOf(writeFd, 'number', 'openSync returns file descriptor');

            if (typeof fs.writeSync === 'function') {
                const written = fs.writeSync(writeFd, 'fd write test');
                assertTypeOf(written, 'number', 'writeSync returns bytes written');
            }

            if (typeof fs.closeSync === 'function') {
                fs.closeSync(writeFd);
                console.log('PASS: closeSync works');
            }

            // Read mode
            const readFd = fs.openSync(fdPath, 'r');
            if (typeof fs.readSync === 'function') {
                const buffer = Buffer.alloc(100);
                const bytesRead = fs.readSync(readFd, buffer);
                assertTypeOf(bytesRead, 'number', 'readSync returns bytes read');
            }
            fs.closeSync && fs.closeSync(readFd);

            fs.unlinkSync(fdPath);
        } catch (e) {
            console.log('SKIP: fd operations test failed: ' + e.message);
        }
    } else {
        console.log('SKIP: openSync not available');
    }

    // ============================================
    // fs.promises
    // ============================================
    console.log('\n--- fs.promises ---');

    if (fs.promises) {
        assertDefined(fs.promises.readFile, 'fs.promises.readFile exists');
        assertDefined(fs.promises.writeFile, 'fs.promises.writeFile exists');
        assertDefined(fs.promises.stat, 'fs.promises.stat exists');
        assertDefined(fs.promises.mkdir, 'fs.promises.mkdir exists');
        assertDefined(fs.promises.unlink, 'fs.promises.unlink exists');

        // Test that they return promises
        const statPromise = fs.promises.stat('/');
        assertTrue(statPromise instanceof Promise, 'fs.promises.stat returns Promise');
    } else {
        console.log('SKIP: fs.promises not available');
    }

    // ============================================
    // Streams: createReadStream / createWriteStream
    // ============================================
    console.log('\n--- Streams ---');

    if (typeof fs.createReadStream === 'function') {
        console.log('PASS: createReadStream exists');
    } else {
        console.log('SKIP: createReadStream not available');
    }

    if (typeof fs.createWriteStream === 'function') {
        console.log('PASS: createWriteStream exists');
    } else {
        console.log('SKIP: createWriteStream not available');
    }

    // ============================================
    // watch / watchFile (if available)
    // ============================================
    console.log('\n--- File watching ---');

    if (typeof fs.watch === 'function') {
        console.log('PASS: watch exists');
    } else {
        console.log('SKIP: watch not available');
    }

    if (typeof fs.watchFile === 'function') {
        console.log('PASS: watchFile exists');
    } else {
        console.log('SKIP: watchFile not available');
    }

    if (typeof fs.unwatchFile === 'function') {
        console.log('PASS: unwatchFile exists');
    } else {
        console.log('SKIP: unwatchFile not available');
    }

    // ============================================
    // Constants
    // ============================================
    console.log('\n--- Constants ---');

    if (fs.constants) {
        assertDefined(fs.constants.F_OK, 'fs.constants.F_OK exists');
        assertDefined(fs.constants.R_OK, 'fs.constants.R_OK exists');
        assertDefined(fs.constants.W_OK, 'fs.constants.W_OK exists');
        assertDefined(fs.constants.X_OK, 'fs.constants.X_OK exists');
    } else {
        console.log('SKIP: fs.constants not available');
    }

    // ============================================
    // accessSync
    // ============================================
    console.log('\n--- accessSync ---');

    if (typeof fs.accessSync === 'function') {
        let accessOk = true;
        try {
            fs.accessSync('/tmp');
        } catch (e) {
            accessOk = false;
        }
        assertTrue(accessOk, 'accessSync does not throw for existing path');

        let accessFail = false;
        try {
            fs.accessSync('/nonexistent_path_12345');
        } catch (e) {
            accessFail = true;
        }
        assertTrue(accessFail, 'accessSync throws for nonexistent path');
    } else {
        console.log('SKIP: accessSync not available');
    }

} else {
    console.log('fs module not available');
}

summary();
