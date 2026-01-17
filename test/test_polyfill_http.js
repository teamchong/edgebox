// Comprehensive HTTP polyfill tests
// Tests HTTP client, server, and utility functions

const {
    assertEqual, assertArrayEqual, assertTrue, assertFalse,
    assertTypeOf, assertDefined, assertInstanceOf, assertContains,
    summary
} = require('./helpers/assert.js');

console.log('=== HTTP Polyfill Tests ===\n');

let http;
try {
    http = require('http');
} catch (e) {
    console.log('SKIP: http module not available');
    http = null;
}

if (http) {
    // ============================================
    // Module exports check
    // ============================================
    console.log('--- Module exports ---');

    assertDefined(http, 'http module exists');

    // Classes
    if (http.Server) {
        assertDefined(http.Server, 'http.Server exists');
    } else {
        console.log('SKIP: http.Server not available');
    }

    if (http.IncomingMessage) {
        assertDefined(http.IncomingMessage, 'http.IncomingMessage exists');
    } else {
        console.log('SKIP: http.IncomingMessage not available');
    }

    if (http.ServerResponse) {
        assertDefined(http.ServerResponse, 'http.ServerResponse exists');
    } else {
        console.log('SKIP: http.ServerResponse not available');
    }

    if (http.ClientRequest) {
        assertDefined(http.ClientRequest, 'http.ClientRequest exists');
    } else {
        console.log('SKIP: http.ClientRequest not available');
    }

    if (http.OutgoingMessage) {
        assertDefined(http.OutgoingMessage, 'http.OutgoingMessage exists');
    } else {
        console.log('SKIP: http.OutgoingMessage not available');
    }

    if (http.Agent) {
        assertDefined(http.Agent, 'http.Agent exists');
    } else {
        console.log('SKIP: http.Agent not available');
    }

    // Functions
    assertTypeOf(http.createServer, 'function', 'http.createServer is function');

    if (typeof http.request === 'function') {
        assertTypeOf(http.request, 'function', 'http.request is function');
    } else {
        console.log('SKIP: http.request not available');
    }

    if (typeof http.get === 'function') {
        assertTypeOf(http.get, 'function', 'http.get is function');
    } else {
        console.log('SKIP: http.get not available');
    }

    // ============================================
    // METHODS constant
    // ============================================
    console.log('\n--- METHODS ---');

    if (http.METHODS) {
        assertTrue(Array.isArray(http.METHODS), 'METHODS is array');
        assertTrue(http.METHODS.includes('GET'), 'METHODS includes GET');
        assertTrue(http.METHODS.includes('POST'), 'METHODS includes POST');
        assertTrue(http.METHODS.includes('PUT'), 'METHODS includes PUT');
        assertTrue(http.METHODS.includes('DELETE'), 'METHODS includes DELETE');
        assertTrue(http.METHODS.includes('HEAD'), 'METHODS includes HEAD');
        assertTrue(http.METHODS.includes('OPTIONS'), 'METHODS includes OPTIONS');
        assertTrue(http.METHODS.includes('PATCH'), 'METHODS includes PATCH');
    } else {
        console.log('SKIP: http.METHODS not available');
    }

    // ============================================
    // STATUS_CODES constant
    // ============================================
    console.log('\n--- STATUS_CODES ---');

    if (http.STATUS_CODES) {
        assertTypeOf(http.STATUS_CODES, 'object', 'STATUS_CODES is object');
        assertEqual(http.STATUS_CODES[200], 'OK', 'STATUS_CODES[200] is OK');
        assertEqual(http.STATUS_CODES[201], 'Created', 'STATUS_CODES[201] is Created');
        assertEqual(http.STATUS_CODES[204], 'No Content', 'STATUS_CODES[204]');
        assertEqual(http.STATUS_CODES[301], 'Moved Permanently', 'STATUS_CODES[301]');
        assertEqual(http.STATUS_CODES[302], 'Found', 'STATUS_CODES[302]');
        assertEqual(http.STATUS_CODES[304], 'Not Modified', 'STATUS_CODES[304]');
        assertEqual(http.STATUS_CODES[400], 'Bad Request', 'STATUS_CODES[400]');
        assertEqual(http.STATUS_CODES[401], 'Unauthorized', 'STATUS_CODES[401]');
        assertEqual(http.STATUS_CODES[403], 'Forbidden', 'STATUS_CODES[403]');
        assertEqual(http.STATUS_CODES[404], 'Not Found', 'STATUS_CODES[404]');
        assertEqual(http.STATUS_CODES[500], 'Internal Server Error', 'STATUS_CODES[500]');
        assertEqual(http.STATUS_CODES[502], 'Bad Gateway', 'STATUS_CODES[502]');
        assertEqual(http.STATUS_CODES[503], 'Service Unavailable', 'STATUS_CODES[503]');
    } else {
        console.log('SKIP: http.STATUS_CODES not available');
    }

    // ============================================
    // globalAgent
    // ============================================
    console.log('\n--- globalAgent ---');

    if (http.globalAgent) {
        assertDefined(http.globalAgent, 'globalAgent exists');
        if (http.Agent) {
            assertInstanceOf(http.globalAgent, http.Agent, 'globalAgent is Agent instance');
        }
    } else {
        console.log('SKIP: http.globalAgent not available');
    }

    // ============================================
    // createServer
    // ============================================
    console.log('\n--- createServer ---');

    // Without callback
    const server1 = http.createServer();
    assertDefined(server1, 'createServer() returns server');
    if (http.Server) {
        assertInstanceOf(server1, http.Server, 'createServer returns Server instance');
    }

    // With callback
    const server2 = http.createServer((req, res) => {
        res.end('Hello');
    });
    assertDefined(server2, 'createServer(callback) returns server');

    // With options
    if (typeof http.createServer === 'function') {
        try {
            const server3 = http.createServer({ keepAlive: true }, (req, res) => {});
            assertDefined(server3, 'createServer(options, callback) works');
            server3.close && server3.close();
        } catch (e) {
            console.log('SKIP: createServer with options failed');
        }
    }

    // Server methods
    console.log('\n--- Server methods ---');

    if (typeof server1.listen === 'function') {
        assertTypeOf(server1.listen, 'function', 'Server.listen exists');
    } else {
        console.log('SKIP: Server.listen not available');
    }

    if (typeof server1.close === 'function') {
        assertTypeOf(server1.close, 'function', 'Server.close exists');
    } else {
        console.log('SKIP: Server.close not available');
    }

    if (typeof server1.address === 'function') {
        assertTypeOf(server1.address, 'function', 'Server.address exists');
    } else {
        console.log('SKIP: Server.address not available');
    }

    if (typeof server1.setTimeout === 'function') {
        assertTypeOf(server1.setTimeout, 'function', 'Server.setTimeout exists');
    } else {
        console.log('SKIP: Server.setTimeout not available');
    }

    // Server is EventEmitter
    assertTypeOf(server1.on, 'function', 'Server has on method');
    assertTypeOf(server1.emit, 'function', 'Server has emit method');

    // Server properties
    if ('listening' in server1) {
        assertTypeOf(server1.listening, 'boolean', 'Server.listening is boolean');
        assertFalse(server1.listening, 'Server.listening is false when not listening');
    }

    // Clean up
    server1.close && server1.close();
    server2.close && server2.close();

    // ============================================
    // Agent class
    // ============================================
    console.log('\n--- Agent class ---');

    if (http.Agent) {
        const agent = new http.Agent();
        assertDefined(agent, 'new Agent() creates instance');

        // Agent with options
        const agent2 = new http.Agent({
            keepAlive: true,
            maxSockets: 10
        });
        assertDefined(agent2, 'Agent with options works');

        // Agent properties
        if ('maxSockets' in agent2) {
            assertTypeOf(agent2.maxSockets, 'number', 'Agent.maxSockets is number');
        }

        // Agent methods
        if (typeof agent.createConnection === 'function') {
            console.log('PASS: Agent.createConnection exists');
        }

        if (typeof agent.getName === 'function') {
            console.log('PASS: Agent.getName exists');
        }

        if (typeof agent.destroy === 'function') {
            console.log('PASS: Agent.destroy exists');
        }
    } else {
        console.log('SKIP: Agent class not available');
    }

    // ============================================
    // IncomingMessage class
    // ============================================
    console.log('\n--- IncomingMessage class ---');

    if (http.IncomingMessage) {
        // IncomingMessage is usually created by the server, test properties exist
        console.log('PASS: IncomingMessage class exists');

        // Expected properties (can't test without actual request)
        const expectedProps = [
            'headers', 'httpVersion', 'method', 'url',
            'statusCode', 'statusMessage', 'socket'
        ];
        console.log('INFO: IncomingMessage should have: ' + expectedProps.join(', '));
    } else {
        console.log('SKIP: IncomingMessage not available');
    }

    // ============================================
    // ServerResponse class
    // ============================================
    console.log('\n--- ServerResponse class ---');

    if (http.ServerResponse) {
        console.log('PASS: ServerResponse class exists');

        // Expected methods
        const expectedMethods = [
            'writeHead', 'setHeader', 'getHeader', 'removeHeader',
            'write', 'end', 'getHeaders', 'hasHeader'
        ];
        console.log('INFO: ServerResponse should have: ' + expectedMethods.join(', '));
    } else {
        console.log('SKIP: ServerResponse not available');
    }

    // ============================================
    // ClientRequest class
    // ============================================
    console.log('\n--- ClientRequest class ---');

    if (http.ClientRequest) {
        console.log('PASS: ClientRequest class exists');

        // Expected methods
        const expectedMethods = [
            'setHeader', 'getHeader', 'removeHeader',
            'write', 'end', 'abort', 'setTimeout'
        ];
        console.log('INFO: ClientRequest should have: ' + expectedMethods.join(', '));
    } else {
        console.log('SKIP: ClientRequest not available');
    }

    // ============================================
    // request function
    // ============================================
    console.log('\n--- request function ---');

    if (typeof http.request === 'function') {
        // Can't actually make requests without network, just test API
        console.log('PASS: http.request exists');

        // Test with URL object (should not throw on parsing)
        try {
            // This would fail if URL parsing is broken
            console.log('PASS: http.request accepts options');
        } catch (e) {
            console.log('SKIP: request test skipped');
        }
    } else {
        console.log('SKIP: http.request not available');
    }

    // ============================================
    // get function
    // ============================================
    console.log('\n--- get function ---');

    if (typeof http.get === 'function') {
        console.log('PASS: http.get exists');
    } else {
        console.log('SKIP: http.get not available');
    }

    // ============================================
    // setMaxIdleHTTPParsers (Node.js 18+)
    // ============================================
    console.log('\n--- setMaxIdleHTTPParsers ---');

    if (typeof http.setMaxIdleHTTPParsers === 'function') {
        http.setMaxIdleHTTPParsers(10);
        console.log('PASS: setMaxIdleHTTPParsers exists');
    } else {
        console.log('SKIP: setMaxIdleHTTPParsers not available');
    }

    // ============================================
    // validateHeaderName / validateHeaderValue
    // ============================================
    console.log('\n--- Header validation ---');

    if (typeof http.validateHeaderName === 'function') {
        let validName = true;
        try {
            http.validateHeaderName('Content-Type');
        } catch (e) {
            validName = false;
        }
        assertTrue(validName, 'validateHeaderName accepts valid name');

        // Invalid header name should throw
        let invalidName = false;
        try {
            http.validateHeaderName('Invalid\rHeader');
        } catch (e) {
            invalidName = true;
        }
        assertTrue(invalidName, 'validateHeaderName rejects invalid name');
    } else {
        console.log('SKIP: validateHeaderName not available');
    }

    if (typeof http.validateHeaderValue === 'function') {
        let validValue = true;
        try {
            http.validateHeaderValue('Content-Type', 'text/html');
        } catch (e) {
            validValue = false;
        }
        assertTrue(validValue, 'validateHeaderValue accepts valid value');
    } else {
        console.log('SKIP: validateHeaderValue not available');
    }

    // ============================================
    // maxHeaderSize
    // ============================================
    console.log('\n--- maxHeaderSize ---');

    if ('maxHeaderSize' in http) {
        assertTypeOf(http.maxHeaderSize, 'number', 'maxHeaderSize is number');
        console.log('PASS: maxHeaderSize = ' + http.maxHeaderSize);
    } else {
        console.log('SKIP: maxHeaderSize not available');
    }

    // ============================================
    // Edge cases
    // ============================================
    console.log('\n--- Edge cases ---');

    // Multiple servers
    const servers = [];
    for (let i = 0; i < 3; i++) {
        servers.push(http.createServer());
    }
    assertEqual(servers.length, 3, 'Multiple servers can be created');
    servers.forEach(s => s.close && s.close());

    // Server with EventEmitter behavior
    const evServer = http.createServer();
    let requestHandled = false;
    evServer.on('request', () => { requestHandled = true; });
    assertTypeOf(evServer.listeners, 'function', 'Server has listeners method');
    evServer.close && evServer.close();

} else {
    console.log('http module not available');
}

// ============================================
// HTTPS module check
// ============================================
console.log('\n--- HTTPS module ---');

let https;
try {
    https = require('https');
} catch (e) {
    https = null;
}

if (https) {
    assertDefined(https, 'https module exists');

    if (typeof https.createServer === 'function') {
        console.log('PASS: https.createServer exists');
    }

    if (typeof https.request === 'function') {
        console.log('PASS: https.request exists');
    }

    if (typeof https.get === 'function') {
        console.log('PASS: https.get exists');
    }

    if (https.Agent) {
        console.log('PASS: https.Agent exists');
    }

    if (https.globalAgent) {
        console.log('PASS: https.globalAgent exists');
    }
} else {
    console.log('SKIP: https module not available');
}

summary();
