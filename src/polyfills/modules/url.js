    // ===== URL MODULE =====
    // ONLY create JS url if native Zig url doesn't exist
    // Native url is in src/polyfills/url.zig
    if (!_modules.url) {
        // Add URL.canParse if not already present (Node.js v18.17+)
        if (globalThis.URL && !globalThis.URL.canParse) {
            globalThis.URL.canParse = function(url, base) {
                try {
                    new URL(url, base);
                    return true;
                } catch {
                    return false;
                }
            };
        }

        _modules.url = {
        URL: globalThis.URL,
        URLSearchParams: globalThis.URLSearchParams,
        // canParse static method (Node.js v18.17+)
        canParse: globalThis.URL ? globalThis.URL.canParse : undefined,
        parse: function(urlStr) {
            try {
                // Extract auth before parsing (QuickJS URL doesn't support auth in URL)
                let auth = '', username = '', password = '';
                let cleanUrl = urlStr;
                const authMatch = urlStr.match(/^(\w+:\/\/)([^@]+)@(.+)$/);
                if (authMatch) {
                    auth = authMatch[2];
                    cleanUrl = authMatch[1] + authMatch[3];
                    const authParts = auth.split(':');
                    username = decodeURIComponent(authParts[0] || '');
                    password = decodeURIComponent(authParts.slice(1).join(':') || '');
                }
                const u = new URL(cleanUrl);
                return { href: urlStr, protocol: u.protocol, host: u.host, hostname: u.hostname, port: u.port, pathname: u.pathname, search: u.search, hash: u.hash, query: Object.fromEntries(u.searchParams), auth: auth || null, username: username || '', password: password || '', origin: u.origin };
            } catch (e) { return { href: urlStr }; }
        },
        format: function(obj) {
            if (typeof obj === 'string') return obj;
            let url = '';
            if (obj.protocol) url += obj.protocol + '//';
            // Handle auth (username:password@)
            if (obj.username) {
                url += encodeURIComponent(obj.username);
                if (obj.password) url += ':' + encodeURIComponent(obj.password);
                url += '@';
            }
            if (obj.hostname || obj.host) url += obj.hostname || obj.host;
            if (obj.port) url += ':' + obj.port;
            if (obj.pathname) url += obj.pathname;
            if (obj.search) url += obj.search;
            if (obj.hash) url += obj.hash;
            return url;
        },
        resolve: (from, to) => new URL(to, from).href,
        // fileURLToPath - convert file:// URL to path
        fileURLToPath: function(urlOrString) {
            let url = typeof urlOrString === 'string' ? urlOrString : urlOrString.href;
            if (!url.startsWith('file://')) {
                throw new TypeError('The URL must be of scheme file');
            }
            // Remove file:// prefix and decode
            let path = decodeURIComponent(url.slice(7));
            // Handle Windows paths (file:///C:/...)
            if (path.match(/^\/[A-Za-z]:\//)) {
                path = path.slice(1);
            }
            return path;
        },
        // pathToFileURL - convert path to file:// URL
        pathToFileURL: function(path) {
            // Ensure absolute path
            if (!path.startsWith('/')) {
                path = '/' + path;
            }
            return new URL('file://' + encodeURIComponent(path).replace(/%2F/g, '/'));
        }
        };
    }

