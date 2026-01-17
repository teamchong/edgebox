    // ===== TLS MODULE (lazy loaded) =====
    // Stub tls module - TLS connections not supported in WASM
    _lazyModule('tls', function() {
        // Get Socket from net module (triggers lazy load if needed)
        var Socket = _modules.net.Socket;

        class TLSSocket extends Socket {
            constructor(socket, options = {}) {
                super();
                this.authorized = false;
                this.encrypted = true;
                this.alpnProtocol = null;
            }
            getPeerCertificate(detailed) { return {}; }
            getCipher() { return { name: 'TLS_NULL', version: 'TLSv1.3' }; }
            getProtocol() { return 'TLSv1.3'; }
            getSession() { return null; }
            getTLSTicket() { return null; }
            isSessionReused() { return false; }
            setMaxSendFragment(size) { return true; }
            setServername(name) {}
            exportKeyingMaterial(length, label, context) { return Buffer.alloc(0); }
        }

        return {
            TLSSocket,
            connect: function(options, callback) {
                const socket = new TLSSocket();
                setTimeout(() => {
                    socket.emit('error', new Error('TLS connections not supported in WASM environment'));
                }, 0);
                if (callback) socket.on('secureConnect', callback);
                return socket;
            },
            createServer: function(options, secureConnectionListener) {
                throw new Error('tls.createServer() not supported in WASM environment');
            },
            createSecureContext: function(options) {
                return { context: {} };
            },
            getCiphers: function() {
                return ['TLS_AES_256_GCM_SHA384', 'TLS_AES_128_GCM_SHA256'];
            },
            DEFAULT_ECDH_CURVE: 'auto',
            DEFAULT_MAX_VERSION: 'TLSv1.3',
            DEFAULT_MIN_VERSION: 'TLSv1.2',
            rootCertificates: [],
        };
    });

