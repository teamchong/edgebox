// Polyfill module loader - concatenates all modules in dependency order
// This file is used by the build system to create the bundled polyfill

// Load order (dependencies first):
// 1. core.js - _modules, _lazyModule, _remapPath
// 2. path.js - no deps
// 3. buffer.js - no deps
// 4. encoding.js - no deps
// 5. events.js - no deps
// 6. stream.js - depends on events
// 7. fs.js - depends on buffer, path
// 8. crypto.js - depends on buffer
// 9. url.js - no deps
// 10. os.js - no deps
// 11. process.js - depends on stream
// 12. http.js - depends on stream, url
// 13. https.js - depends on http
// 14. http2.js - depends on http
// 15. net.js - depends on stream, events
// 16. tls.js - depends on net
// 17. dgram.js - depends on events
// 18. zlib.js - depends on buffer
// 19. cluster.js - depends on events, net
// 20. perf_hooks.js - no deps
// 21. timers.js - no deps
// 22. dns.js - wraps native Zig DNS with promises API
// 23. child_process.js - depends on stream, events, buffer
// 24. util.js - MIMEType, MIMEParams (merged with native Zig util)
