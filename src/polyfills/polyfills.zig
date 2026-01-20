/// Polyfill module definitions - single source of truth for all embedders
/// All Zig files that need polyfills should import this module.

// Individual modules (defined once, used for both concat and hashing)
const mod_core = @embedFile("modules/core.js");
const mod_path = @embedFile("modules/path.js");
const mod_buffer = @embedFile("modules/buffer.js");
const mod_encoding = @embedFile("modules/encoding.js");
const mod_events = @embedFile("modules/events.js");
const mod_stream = @embedFile("modules/stream.js");
const mod_fs = @embedFile("modules/fs.js");
const mod_crypto = @embedFile("modules/crypto.js");
const mod_http = @embedFile("modules/http.js");
const mod_http2 = @embedFile("modules/http2.js");
const mod_net = @embedFile("modules/net.js");
const mod_tls = @embedFile("modules/tls.js");
const mod_dgram = @embedFile("modules/dgram.js");
const mod_url = @embedFile("modules/url.js");
const mod_os = @embedFile("modules/os.js");
const mod_process = @embedFile("modules/process.js");
const mod_zlib = @embedFile("modules/zlib.js");
const mod_cluster = @embedFile("modules/cluster.js");
const mod_util = @embedFile("modules/util.js");

/// Runtime polyfills (console helpers, error handlers, globals)
pub const runtime_js = @embedFile("runtime.js");

/// Node.js polyfills concatenated (in dependency order)
/// NOTE: mod_process removed - process is fully implemented in native Zig (process.zig)
/// NOTE: mod_os removed - os is fully implemented in native Zig (os.zig)
pub const node_polyfill_js = mod_core ++ mod_path ++ mod_buffer ++ mod_encoding ++
    mod_events ++ mod_stream ++ mod_fs ++ mod_crypto ++ mod_http ++ mod_http2 ++
    mod_net ++ mod_tls ++ mod_dgram ++ mod_url ++
    mod_zlib ++ mod_cluster ++ mod_util;

/// All module sources for cache invalidation hashing
pub const all_sources = [_][]const u8{
    "EdgeBox-Polyfills-v6", // Bumped version - added mod_util (MIMEType)
    runtime_js,
    mod_core, mod_path, mod_buffer, mod_encoding, mod_events, mod_stream,
    mod_fs, mod_crypto, mod_http, mod_http2, mod_net, mod_tls, mod_dgram,
    mod_url, mod_zlib, mod_cluster, mod_util,
};
