/// Wizer Pre-initialization Support for EdgeBox
///
/// This module provides "instant startup" by pre-initializing the QuickJS
/// runtime at build time. Wizer snapshots the memory state after wizer_init()
/// runs, embedding it into the WASM binary.
///
/// Build pipeline:
/// 1. zig build wasm -> edgebox-base.wasm (with wizer_init export)
/// 2. wizer edgebox-base.wasm --init-func=wizer_init -> edgebox-wizer.wasm
/// 3. wasmedge compile edgebox-wizer.wasm -> edgebox-aot.dylib
///
/// At runtime, main() detects wizer_initialized=true and uses the
/// pre-initialized runtime, skipping JS_NewRuntime/JS_NewContext.
const std = @import("std");
const quickjs = @import("quickjs_core.zig");
const wasm_bump = @import("wasm_bump.zig");

const qjs = quickjs.c;

// ============================================================================
// GLOBAL STATE (Persists across Wizer snapshot)
// ============================================================================

/// Wizer-initialized QuickJS runtime (populated at build time)
pub var wizer_runtime: ?*qjs.JSRuntime = null;

/// Wizer-initialized QuickJS context (populated at build time)
pub var wizer_context: ?*qjs.JSContext = null;

/// Flag indicating Wizer initialization completed successfully
pub var wizer_initialized: bool = false;

// ============================================================================
// WIZER INITIALIZATION (Runs at BUILD TIME)
// ============================================================================

/// Wizer initialization function - exported and called at build time.
/// Creates the QuickJS runtime/context and pre-compiles static polyfills.
///
/// This function MUST NOT:
/// - Open files or sockets (WASI state is not snapshotted)
/// - Call Date.now() or Math.random() (seeds would be fixed)
/// - Read environment variables (dynamic)
/// - Read command line args (dynamic)
pub fn wizer_init() void {
    // Initialize bump allocator for QuickJS memory
    wasm_bump.init();

    // Create malloc functions using our bump allocator
    const malloc_funcs = qjs.JSMallocFunctions{
        .js_calloc = wasm_bump.js_calloc,
        .js_malloc = wasm_bump.js_malloc,
        .js_free = wasm_bump.js_free,
        .js_realloc = wasm_bump.js_realloc,
        .js_malloc_usable_size = wasm_bump.js_malloc_usable_size,
    };

    // Create QuickJS runtime with bump allocator
    wizer_runtime = qjs.JS_NewRuntime2(&malloc_funcs, null);
    if (wizer_runtime == null) return;

    // Create context
    wizer_context = qjs.JS_NewContext(wizer_runtime);
    if (wizer_context == null) return;

    // Initialize std/os modules (module loaders, not fd bindings)
    _ = qjs.js_init_module_std(wizer_context, "std");
    _ = qjs.js_init_module_os(wizer_context, "os");

    // Pre-compile static polyfills (class definitions, pure functions)
    initStaticPolyfills();

    wizer_initialized = true;
}

/// Initialize static polyfills that are safe to snapshot.
/// These are pure class definitions and helpers with no side effects.
fn initStaticPolyfills() void {
    const ctx = wizer_context orelse return;

    // Static polyfills - class definitions only, no dynamic state
    const static_polyfills =
        \\// === Static class definitions (safe to snapshot) ===
        \\globalThis.global = globalThis;
        \\globalThis.self = globalThis;
        \\globalThis._modules = {};
        \\globalThis._wizerInitialized = true;
        \\
        \\// Note: console is set up by js_std_add_helpers at runtime
        \\
        \\// TextEncoder (pure string->bytes, safe to snapshot)
        \\if (typeof TextEncoder === 'undefined') {
        \\    globalThis.TextEncoder = class TextEncoder {
        \\        constructor(encoding = 'utf-8') { this.encoding = encoding; }
        \\        encode(str) {
        \\            const bytes = [];
        \\            for (let i = 0; i < str.length; i++) {
        \\                let code = str.charCodeAt(i);
        \\                if (code < 0x80) bytes.push(code);
        \\                else if (code < 0x800) {
        \\                    bytes.push(0xC0 | (code >> 6));
        \\                    bytes.push(0x80 | (code & 0x3F));
        \\                } else {
        \\                    bytes.push(0xE0 | (code >> 12));
        \\                    bytes.push(0x80 | ((code >> 6) & 0x3F));
        \\                    bytes.push(0x80 | (code & 0x3F));
        \\                }
        \\            }
        \\            return new Uint8Array(bytes);
        \\        }
        \\    };
        \\}
        \\
        \\// TextDecoder (pure bytes->string, safe to snapshot)
        \\if (typeof TextDecoder === 'undefined') {
        \\    globalThis.TextDecoder = class TextDecoder {
        \\        constructor(encoding = 'utf-8') { this.encoding = encoding; }
        \\        decode(input) {
        \\            const bytes = input instanceof Uint8Array ? input : new Uint8Array(input);
        \\            let str = '';
        \\            for (let i = 0; i < bytes.length; ) {
        \\                const b = bytes[i++];
        \\                if (b < 0x80) str += String.fromCharCode(b);
        \\                else if (b < 0xE0) str += String.fromCharCode(((b & 0x1F) << 6) | (bytes[i++] & 0x3F));
        \\                else str += String.fromCharCode(((b & 0x0F) << 12) | ((bytes[i++] & 0x3F) << 6) | (bytes[i++] & 0x3F));
        \\            }
        \\            return str;
        \\        }
        \\    };
        \\}
        \\
        \\// URL/URLSearchParams (pure string parsing, safe to snapshot)
        \\if (typeof URLSearchParams === 'undefined') {
        \\    globalThis.URLSearchParams = class URLSearchParams {
        \\        constructor(init) {
        \\            this._params = [];
        \\            if (typeof init === 'string') {
        \\                (init.startsWith('?') ? init.slice(1) : init).split('&').forEach(p => {
        \\                    const [k, v] = p.split('=').map(decodeURIComponent);
        \\                    if (k) this._params.push([k, v || '']);
        \\                });
        \\            }
        \\        }
        \\        get(name) { const p = this._params.find(([k]) => k === name); return p ? p[1] : null; }
        \\        has(name) { return this._params.some(([k]) => k === name); }
        \\        toString() { return this._params.map(([k, v]) => encodeURIComponent(k) + '=' + encodeURIComponent(v)).join('&'); }
        \\    };
        \\}
        \\
        \\if (typeof URL === 'undefined') {
        \\    globalThis.URL = class URL {
        \\        constructor(url, base) {
        \\            const m = url.match(/^(([^:/?#]+):)?(\/\/([^/?#]*))?([^?#]*)(\?([^#]*))?(#(.*))?/);
        \\            this.protocol = (m[2] || '') + ':';
        \\            this.host = m[4] || '';
        \\            this.hostname = this.host.split(':')[0];
        \\            this.port = this.host.split(':')[1] || '';
        \\            this.pathname = m[5] || '/';
        \\            this.search = m[6] || '';
        \\            this.hash = m[8] || '';
        \\            this.searchParams = new URLSearchParams(this.search);
        \\        }
        \\        get origin() { return this.protocol + '//' + this.host; }
        \\        get href() { return this.origin + this.pathname + this.search + this.hash; }
        \\        toString() { return this.href; }
        \\    };
        \\}
        \\
        \\// Event classes (pure, safe to snapshot)
        \\if (typeof Event === 'undefined') {
        \\    globalThis.Event = class Event {
        \\        constructor(type, opts = {}) {
        \\            this.type = type;
        \\            this.bubbles = opts.bubbles || false;
        \\            this.defaultPrevented = false;
        \\        }
        \\        preventDefault() { this.defaultPrevented = true; }
        \\    };
        \\    globalThis.CustomEvent = class CustomEvent extends Event {
        \\        constructor(type, opts = {}) { super(type, opts); this.detail = opts.detail; }
        \\    };
        \\    globalThis.EventTarget = class EventTarget {
        \\        constructor() { this._l = {}; }
        \\        addEventListener(t, fn) { (this._l[t] = this._l[t] || []).push(fn); }
        \\        removeEventListener(t, fn) { if (this._l[t]) this._l[t] = this._l[t].filter(f => f !== fn); }
        \\        dispatchEvent(e) { (this._l[e.type] || []).forEach(fn => fn(e)); return !e.defaultPrevented; }
        \\    };
        \\}
        \\
        \\// DOMException (pure error class)
        \\if (typeof DOMException === 'undefined') {
        \\    globalThis.DOMException = class DOMException extends Error {
        \\        constructor(msg, name) { super(msg); this.name = name || 'Error'; }
        \\    };
        \\}
    ;

    const val = qjs.JS_Eval(
        ctx,
        static_polyfills.ptr,
        static_polyfills.len,
        "<wizer-static>",
        qjs.JS_EVAL_TYPE_GLOBAL,
    );
    qjs.JS_FreeValue(ctx, val);
}

// ============================================================================
// RUNTIME HELPERS (Called at runtime to bind dynamic state)
// ============================================================================

/// Check if we're running with Wizer pre-initialization
pub fn isWizerInitialized() bool {
    return wizer_initialized and wizer_runtime != null and wizer_context != null;
}

/// Get the pre-initialized context (for use in main())
pub fn getContext() ?*qjs.JSContext {
    return wizer_context;
}

/// Get the pre-initialized runtime
pub fn getRuntime() ?*qjs.JSRuntime {
    return wizer_runtime;
}
