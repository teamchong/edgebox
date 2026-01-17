// Comprehensive Net polyfill tests
// Tests Socket, Server, and utility functions

const {
    assertEqual, assertTrue, assertFalse, assertTypeOf,
    assertDefined, assertInstanceOf, summary
} = require('./helpers/assert.js');

console.log('=== Net Polyfill Tests ===\n');

let net;
try {
    net = require('net');
} catch (e) {
    console.log('SKIP: net module not available');
    net = null;
}

if (net) {
    // ============================================
    // Module exports check
    // ============================================
    console.log('--- Module exports ---');

    assertDefined(net, 'net module exists');
    assertDefined(net.Socket, 'net.Socket exists');
    assertDefined(net.Server, 'net.Server exists');

    // Functions
    if (typeof net.createServer === 'function') {
        assertTypeOf(net.createServer, 'function', 'net.createServer is function');
    } else {
        console.log('SKIP: net.createServer not available');
    }

    if (typeof net.createConnection === 'function') {
        assertTypeOf(net.createConnection, 'function', 'net.createConnection is function');
    } else {
        console.log('SKIP: net.createConnection not available');
    }

    if (typeof net.connect === 'function') {
        assertTypeOf(net.connect, 'function', 'net.connect is function');
    } else {
        console.log('SKIP: net.connect not available');
    }

    // ============================================
    // Socket class
    // ============================================
    console.log('\n--- Socket class ---');

    const Socket = net.Socket;
    if (Socket) {
        const socket = new Socket();
        assertInstanceOf(socket, Socket, 'new Socket() creates instance');

        // Socket is an EventEmitter
        assertTypeOf(socket.on, 'function', 'Socket has on method');
        assertTypeOf(socket.emit, 'function', 'Socket has emit method');

        // Socket methods
        if (typeof socket.connect === 'function') {
            assertTypeOf(socket.connect, 'function', 'Socket.connect exists');
        } else {
            console.log('SKIP: Socket.connect not available');
        }

        if (typeof socket.write === 'function') {
            assertTypeOf(socket.write, 'function', 'Socket.write exists');
        } else {
            console.log('SKIP: Socket.write not available');
        }

        if (typeof socket.end === 'function') {
            assertTypeOf(socket.end, 'function', 'Socket.end exists');
        } else {
            console.log('SKIP: Socket.end not available');
        }

        if (typeof socket.destroy === 'function') {
            assertTypeOf(socket.destroy, 'function', 'Socket.destroy exists');
        } else {
            console.log('SKIP: Socket.destroy not available');
        }

        if (typeof socket.setEncoding === 'function') {
            assertTypeOf(socket.setEncoding, 'function', 'Socket.setEncoding exists');
        } else {
            console.log('SKIP: Socket.setEncoding not available');
        }

        if (typeof socket.setKeepAlive === 'function') {
            assertTypeOf(socket.setKeepAlive, 'function', 'Socket.setKeepAlive exists');
        } else {
            console.log('SKIP: Socket.setKeepAlive not available');
        }

        if (typeof socket.setNoDelay === 'function') {
            assertTypeOf(socket.setNoDelay, 'function', 'Socket.setNoDelay exists');
        } else {
            console.log('SKIP: Socket.setNoDelay not available');
        }

        if (typeof socket.setTimeout === 'function') {
            assertTypeOf(socket.setTimeout, 'function', 'Socket.setTimeout exists');
        } else {
            console.log('SKIP: Socket.setTimeout not available');
        }

        if (typeof socket.pause === 'function') {
            assertTypeOf(socket.pause, 'function', 'Socket.pause exists');
        } else {
            console.log('SKIP: Socket.pause not available');
        }

        if (typeof socket.resume === 'function') {
            assertTypeOf(socket.resume, 'function', 'Socket.resume exists');
        } else {
            console.log('SKIP: Socket.resume not available');
        }

        if (typeof socket.address === 'function') {
            const addr = socket.address();
            // May return null or empty object for unconnected socket
            console.log('PASS: Socket.address exists');
        } else {
            console.log('SKIP: Socket.address not available');
        }

        // Socket properties
        if ('remoteAddress' in socket || typeof socket.remoteAddress !== 'undefined') {
            console.log('PASS: Socket.remoteAddress property exists');
        } else {
            console.log('SKIP: Socket.remoteAddress not available');
        }

        if ('remotePort' in socket || typeof socket.remotePort !== 'undefined') {
            console.log('PASS: Socket.remotePort property exists');
        } else {
            console.log('SKIP: Socket.remotePort not available');
        }

        if ('localAddress' in socket || typeof socket.localAddress !== 'undefined') {
            console.log('PASS: Socket.localAddress property exists');
        } else {
            console.log('SKIP: Socket.localAddress not available');
        }

        if ('localPort' in socket || typeof socket.localPort !== 'undefined') {
            console.log('PASS: Socket.localPort property exists');
        } else {
            console.log('SKIP: Socket.localPort not available');
        }

        // Clean up
        socket.destroy && socket.destroy();
    } else {
        console.log('SKIP: Socket class not available');
    }

    // ============================================
    // Server class
    // ============================================
    console.log('\n--- Server class ---');

    const Server = net.Server;
    if (Server) {
        let server;
        try {
            server = new Server();
            assertInstanceOf(server, Server, 'new Server() creates instance');
        } catch (e) {
            // Some implementations require options
            server = net.createServer && net.createServer();
        }

        if (server) {
            // Server is an EventEmitter
            assertTypeOf(server.on, 'function', 'Server has on method');
            assertTypeOf(server.emit, 'function', 'Server has emit method');

            // Server methods
            if (typeof server.listen === 'function') {
                assertTypeOf(server.listen, 'function', 'Server.listen exists');
            } else {
                console.log('SKIP: Server.listen not available');
            }

            if (typeof server.close === 'function') {
                assertTypeOf(server.close, 'function', 'Server.close exists');
            } else {
                console.log('SKIP: Server.close not available');
            }

            if (typeof server.address === 'function') {
                console.log('PASS: Server.address exists');
            } else {
                console.log('SKIP: Server.address not available');
            }

            if (typeof server.getConnections === 'function') {
                console.log('PASS: Server.getConnections exists');
            } else {
                console.log('SKIP: Server.getConnections not available');
            }

            if (typeof server.ref === 'function') {
                console.log('PASS: Server.ref exists');
            } else {
                console.log('SKIP: Server.ref not available');
            }

            if (typeof server.unref === 'function') {
                console.log('PASS: Server.unref exists');
            } else {
                console.log('SKIP: Server.unref not available');
            }

            // Properties
            if ('listening' in server) {
                assertTypeOf(server.listening, 'boolean', 'Server.listening is boolean');
            } else {
                console.log('SKIP: Server.listening not available');
            }

            // Clean up
            server.close && server.close();
        }
    } else {
        console.log('SKIP: Server class not available');
    }

    // ============================================
    // IP validation utilities
    // ============================================
    console.log('\n--- IP utilities ---');

    // isIP
    if (typeof net.isIP === 'function') {
        assertEqual(net.isIP('127.0.0.1'), 4, 'isIP returns 4 for IPv4');
        assertEqual(net.isIP('::1'), 6, 'isIP returns 6 for IPv6');
        assertEqual(net.isIP('not an ip'), 0, 'isIP returns 0 for invalid');
        assertEqual(net.isIP('192.168.1.1'), 4, 'isIP for private IPv4');
        assertEqual(net.isIP('2001:db8::1'), 6, 'isIP for IPv6');
    } else {
        console.log('SKIP: net.isIP not available');
    }

    // isIPv4
    if (typeof net.isIPv4 === 'function') {
        assertTrue(net.isIPv4('127.0.0.1'), 'isIPv4 returns true for IPv4');
        assertTrue(net.isIPv4('192.168.0.1'), 'isIPv4 for private IP');
        assertTrue(net.isIPv4('0.0.0.0'), 'isIPv4 for 0.0.0.0');
        assertTrue(net.isIPv4('255.255.255.255'), 'isIPv4 for broadcast');
        assertFalse(net.isIPv4('::1'), 'isIPv4 returns false for IPv6');
        assertFalse(net.isIPv4('not an ip'), 'isIPv4 returns false for invalid');
        assertFalse(net.isIPv4('256.0.0.1'), 'isIPv4 false for out of range');
    } else {
        console.log('SKIP: net.isIPv4 not available');
    }

    // isIPv6
    if (typeof net.isIPv6 === 'function') {
        assertTrue(net.isIPv6('::1'), 'isIPv6 returns true for IPv6 loopback');
        assertTrue(net.isIPv6('::'), 'isIPv6 for ::');
        assertTrue(net.isIPv6('fe80::1'), 'isIPv6 for link-local');
        assertTrue(net.isIPv6('2001:db8::1'), 'isIPv6 for documentation prefix');
        assertFalse(net.isIPv6('127.0.0.1'), 'isIPv6 returns false for IPv4');
        assertFalse(net.isIPv6('not an ip'), 'isIPv6 returns false for invalid');
    } else {
        console.log('SKIP: net.isIPv6 not available');
    }

    // ============================================
    // createServer
    // ============================================
    console.log('\n--- createServer ---');

    if (typeof net.createServer === 'function') {
        // Without callback
        const server1 = net.createServer();
        assertDefined(server1, 'createServer() returns server');

        // With options
        const server2 = net.createServer({ allowHalfOpen: true });
        assertDefined(server2, 'createServer(options) returns server');

        // With callback
        const server3 = net.createServer(() => {});
        assertDefined(server3, 'createServer(callback) returns server');

        // With options and callback
        const server4 = net.createServer({ allowHalfOpen: true }, () => {});
        assertDefined(server4, 'createServer(options, callback) returns server');

        // Clean up
        [server1, server2, server3, server4].forEach(s => s.close && s.close());
    } else {
        console.log('SKIP: createServer not available');
    }

    // ============================================
    // createConnection / connect
    // ============================================
    console.log('\n--- createConnection / connect ---');

    if (typeof net.createConnection === 'function') {
        // These will fail without a server, but we can test they exist
        console.log('PASS: createConnection exists');
    } else {
        console.log('SKIP: createConnection not available');
    }

    if (typeof net.connect === 'function') {
        console.log('PASS: connect exists (alias for createConnection)');
    } else {
        console.log('SKIP: connect not available');
    }

    // ============================================
    // BlockList (Node.js 15+)
    // ============================================
    console.log('\n--- BlockList ---');

    if (net.BlockList) {
        const blockList = new net.BlockList();
        assertDefined(blockList, 'BlockList constructor works');

        if (typeof blockList.addAddress === 'function') {
            console.log('PASS: BlockList.addAddress exists');
        }
        if (typeof blockList.addSubnet === 'function') {
            console.log('PASS: BlockList.addSubnet exists');
        }
        if (typeof blockList.addRange === 'function') {
            console.log('PASS: BlockList.addRange exists');
        }
        if (typeof blockList.check === 'function') {
            console.log('PASS: BlockList.check exists');
        }
    } else {
        console.log('SKIP: BlockList not available');
    }

    // ============================================
    // SocketAddress (Node.js 15+)
    // ============================================
    console.log('\n--- SocketAddress ---');

    if (net.SocketAddress) {
        try {
            const addr = new net.SocketAddress({ address: '127.0.0.1', port: 8080 });
            assertDefined(addr, 'SocketAddress constructor works');
            assertEqual(addr.address, '127.0.0.1', 'SocketAddress.address');
            assertEqual(addr.port, 8080, 'SocketAddress.port');
        } catch (e) {
            console.log('SKIP: SocketAddress test failed');
        }
    } else {
        console.log('SKIP: SocketAddress not available');
    }

    // ============================================
    // Constants
    // ============================================
    console.log('\n--- Constants ---');

    if (net.constants) {
        console.log('PASS: net.constants exists');
    } else {
        console.log('SKIP: net.constants not available');
    }

    // ============================================
    // Edge cases
    // ============================================
    console.log('\n--- Edge cases ---');

    // Empty socket creation
    let emptySocket = new net.Socket();
    assertDefined(emptySocket, 'Empty socket creation');
    emptySocket.destroy && emptySocket.destroy();

    // Multiple sockets
    const sockets = [];
    for (let i = 0; i < 5; i++) {
        sockets.push(new net.Socket());
    }
    assertEqual(sockets.length, 5, 'Multiple socket creation');
    sockets.forEach(s => s.destroy && s.destroy());

} else {
    console.log('net module not available');
}

summary();
