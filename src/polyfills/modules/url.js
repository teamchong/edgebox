    // ===== URL MODULE =====
    // ONLY create JS url if native Zig url doesn't exist
    // Native url is in src/polyfills/url.zig
    if (!_modules.url) {
        _modules.url = {
        URL: globalThis.URL,
        URLSearchParams: globalThis.URLSearchParams,
        parse: function(urlStr) {
            try {
                const u = new URL(urlStr);
                return { href: u.href, protocol: u.protocol, host: u.host, hostname: u.hostname, port: u.port, pathname: u.pathname, search: u.search, hash: u.hash, query: Object.fromEntries(u.searchParams) };
            } catch (e) { return { href: urlStr }; }
        },
        format: function(obj) {
            if (typeof obj === 'string') return obj;
            let url = '';
            if (obj.protocol) url += obj.protocol + '//';
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

