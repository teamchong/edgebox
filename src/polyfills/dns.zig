/// Native DNS module - QuickJS C functions
/// Real DNS resolution using system getaddrinfo
const std = @import("std");
const builtin = @import("builtin");
const quickjs = @import("../quickjs_core.zig");
const qjs = quickjs.c;

// Platform-specific DNS imports
const c = @cImport({
    @cInclude("netdb.h");
    @cInclude("sys/socket.h");
    @cInclude("netinet/in.h");
    @cInclude("arpa/inet.h");
});

/// dns.lookup(hostname, [options], callback) - Resolve hostname to IP
/// Returns: { address: string, family: 4|6 }
fn dnsLookup(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) {
        return qjs.JS_ThrowTypeError(ctx, "dns.lookup requires hostname argument");
    }

    // Get hostname
    const hostname_cstr = qjs.JS_ToCString(ctx, argv[0]);
    if (hostname_cstr == null) {
        return qjs.JS_ThrowTypeError(ctx, "hostname must be a string");
    }
    defer qjs.JS_FreeCString(ctx, hostname_cstr);

    // Parse options (family preference)
    var family: c_int = c.AF_UNSPEC; // Default: any
    if (argc >= 2 and !qjs.JS_IsUndefined(argv[1]) and !qjs.JS_IsFunction(ctx, argv[1])) {
        if (qjs.JS_IsNumber(argv[1])) {
            var fam: i32 = 0;
            _ = qjs.JS_ToInt32(ctx, &fam, argv[1]);
            if (fam == 4) family = c.AF_INET;
            if (fam == 6) family = c.AF_INET6;
        } else if (qjs.JS_IsObject(argv[1])) {
            const fam_val = qjs.JS_GetPropertyStr(ctx, argv[1], "family");
            defer qjs.JS_FreeValue(ctx, fam_val);
            if (qjs.JS_IsNumber(fam_val)) {
                var fam: i32 = 0;
                _ = qjs.JS_ToInt32(ctx, &fam, fam_val);
                if (fam == 4) family = c.AF_INET;
                if (fam == 6) family = c.AF_INET6;
            }
        }
    }

    // Setup hints for getaddrinfo
    var hints: c.addrinfo = std.mem.zeroes(c.addrinfo);
    hints.ai_family = family;
    hints.ai_socktype = c.SOCK_STREAM;

    // Perform DNS lookup
    var result: ?*c.addrinfo = null;
    const status = c.getaddrinfo(hostname_cstr, null, &hints, &result);
    if (status != 0 or result == null) {
        // Return error object
        const err = qjs.JS_NewObject(ctx);
        _ = qjs.JS_SetPropertyStr(ctx, err, "code", qjs.JS_NewString(ctx, "ENOTFOUND"));
        _ = qjs.JS_SetPropertyStr(ctx, err, "hostname", qjs.JS_DupValue(ctx, argv[0]));
        return qjs.JS_Throw(ctx, err);
    }
    defer c.freeaddrinfo(result);

    // Extract IP address
    var addr_buf: [c.INET6_ADDRSTRLEN]u8 = undefined;
    var result_family: i32 = 4;

    if (result.?.ai_family == c.AF_INET) {
        const sockaddr_in: *c.sockaddr_in = @ptrCast(@alignCast(result.?.ai_addr));
        _ = c.inet_ntop(c.AF_INET, &sockaddr_in.sin_addr, &addr_buf, c.INET6_ADDRSTRLEN);
        result_family = 4;
    } else if (result.?.ai_family == c.AF_INET6) {
        const sockaddr_in6: *c.sockaddr_in6 = @ptrCast(@alignCast(result.?.ai_addr));
        _ = c.inet_ntop(c.AF_INET6, &sockaddr_in6.sin6_addr, &addr_buf, c.INET6_ADDRSTRLEN);
        result_family = 6;
    } else {
        return qjs.JS_ThrowInternalError(ctx, "Unknown address family");
    }

    // Create result object { address, family }
    const addr_str = std.mem.sliceTo(&addr_buf, 0);
    const obj = qjs.JS_NewObject(ctx);
    _ = qjs.JS_SetPropertyStr(ctx, obj, "address", qjs.JS_NewStringLen(ctx, addr_str.ptr, addr_str.len));
    _ = qjs.JS_SetPropertyStr(ctx, obj, "family", qjs.JS_NewInt32(ctx, result_family));

    return obj;
}

/// dns.resolve4(hostname) - Resolve hostname to IPv4 addresses
fn dnsResolve4(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) {
        return qjs.JS_ThrowTypeError(ctx, "dns.resolve4 requires hostname argument");
    }

    const hostname_cstr = qjs.JS_ToCString(ctx, argv[0]);
    if (hostname_cstr == null) {
        return qjs.JS_ThrowTypeError(ctx, "hostname must be a string");
    }
    defer qjs.JS_FreeCString(ctx, hostname_cstr);

    // Setup hints for IPv4 only
    var hints: c.addrinfo = std.mem.zeroes(c.addrinfo);
    hints.ai_family = c.AF_INET;
    hints.ai_socktype = c.SOCK_STREAM;

    var result: ?*c.addrinfo = null;
    const status = c.getaddrinfo(hostname_cstr, null, &hints, &result);
    if (status != 0 or result == null) {
        const err = qjs.JS_NewObject(ctx);
        _ = qjs.JS_SetPropertyStr(ctx, err, "code", qjs.JS_NewString(ctx, "ENOTFOUND"));
        return qjs.JS_Throw(ctx, err);
    }
    defer c.freeaddrinfo(result);

    // Collect all IPv4 addresses
    const arr = qjs.JS_NewArray(ctx);
    var idx: u32 = 0;
    var current = result;

    while (current) |addr| : (current = addr.ai_next) {
        if (addr.ai_family == c.AF_INET) {
            var addr_buf: [c.INET_ADDRSTRLEN]u8 = undefined;
            const sockaddr_in: *c.sockaddr_in = @ptrCast(@alignCast(addr.ai_addr));
            _ = c.inet_ntop(c.AF_INET, &sockaddr_in.sin_addr, &addr_buf, c.INET_ADDRSTRLEN);
            const addr_str = std.mem.sliceTo(&addr_buf, 0);
            _ = qjs.JS_SetPropertyUint32(ctx, arr, idx, qjs.JS_NewStringLen(ctx, addr_str.ptr, addr_str.len));
            idx += 1;
        }
    }

    return arr;
}

/// dns.resolve6(hostname) - Resolve hostname to IPv6 addresses
fn dnsResolve6(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) {
        return qjs.JS_ThrowTypeError(ctx, "dns.resolve6 requires hostname argument");
    }

    const hostname_cstr = qjs.JS_ToCString(ctx, argv[0]);
    if (hostname_cstr == null) {
        return qjs.JS_ThrowTypeError(ctx, "hostname must be a string");
    }
    defer qjs.JS_FreeCString(ctx, hostname_cstr);

    var hints: c.addrinfo = std.mem.zeroes(c.addrinfo);
    hints.ai_family = c.AF_INET6;
    hints.ai_socktype = c.SOCK_STREAM;

    var result: ?*c.addrinfo = null;
    const status = c.getaddrinfo(hostname_cstr, null, &hints, &result);
    if (status != 0 or result == null) {
        const err = qjs.JS_NewObject(ctx);
        _ = qjs.JS_SetPropertyStr(ctx, err, "code", qjs.JS_NewString(ctx, "ENOTFOUND"));
        return qjs.JS_Throw(ctx, err);
    }
    defer c.freeaddrinfo(result);

    const arr = qjs.JS_NewArray(ctx);
    var idx: u32 = 0;
    var current = result;

    while (current) |addr| : (current = addr.ai_next) {
        if (addr.ai_family == c.AF_INET6) {
            var addr_buf: [c.INET6_ADDRSTRLEN]u8 = undefined;
            const sockaddr_in6: *c.sockaddr_in6 = @ptrCast(@alignCast(addr.ai_addr));
            _ = c.inet_ntop(c.AF_INET6, &sockaddr_in6.sin6_addr, &addr_buf, c.INET6_ADDRSTRLEN);
            const addr_str = std.mem.sliceTo(&addr_buf, 0);
            _ = qjs.JS_SetPropertyUint32(ctx, arr, idx, qjs.JS_NewStringLen(ctx, addr_str.ptr, addr_str.len));
            idx += 1;
        }
    }

    return arr;
}

/// dns.reverse(ip) - Reverse lookup IP to hostnames
fn dnsReverse(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) {
        return qjs.JS_ThrowTypeError(ctx, "dns.reverse requires ip argument");
    }

    const ip_cstr = qjs.JS_ToCString(ctx, argv[0]);
    if (ip_cstr == null) {
        return qjs.JS_ThrowTypeError(ctx, "ip must be a string");
    }
    defer qjs.JS_FreeCString(ctx, ip_cstr);

    // Try to parse as IPv4
    var sa4: c.sockaddr_in = std.mem.zeroes(c.sockaddr_in);
    sa4.sin_family = c.AF_INET;

    var sa6: c.sockaddr_in6 = std.mem.zeroes(c.sockaddr_in6);
    sa6.sin6_family = c.AF_INET6;

    var host_buf: [c.NI_MAXHOST]u8 = undefined;

    if (c.inet_pton(c.AF_INET, ip_cstr, &sa4.sin_addr) == 1) {
        // IPv4 reverse lookup
        const status = c.getnameinfo(
            @ptrCast(&sa4),
            @sizeOf(c.sockaddr_in),
            &host_buf,
            c.NI_MAXHOST,
            null,
            0,
            c.NI_NAMEREQD,
        );
        if (status != 0) {
            const arr = qjs.JS_NewArray(ctx);
            return arr; // Return empty array on failure
        }
    } else if (c.inet_pton(c.AF_INET6, ip_cstr, &sa6.sin6_addr) == 1) {
        // IPv6 reverse lookup
        const status = c.getnameinfo(
            @ptrCast(&sa6),
            @sizeOf(c.sockaddr_in6),
            &host_buf,
            c.NI_MAXHOST,
            null,
            0,
            c.NI_NAMEREQD,
        );
        if (status != 0) {
            const arr = qjs.JS_NewArray(ctx);
            return arr;
        }
    } else {
        return qjs.JS_ThrowTypeError(ctx, "Invalid IP address");
    }

    // Return array with single hostname
    const hostname = std.mem.sliceTo(&host_buf, 0);
    const arr = qjs.JS_NewArray(ctx);
    _ = qjs.JS_SetPropertyUint32(ctx, arr, 0, qjs.JS_NewStringLen(ctx, hostname.ptr, hostname.len));
    return arr;
}

/// Register DNS module functions
pub fn register(ctx: ?*qjs.JSContext) void {
    const global = qjs.JS_GetGlobalObject(ctx);
    defer qjs.JS_FreeValue(ctx, global);

    // Create _dns object
    const dns_obj = qjs.JS_NewObject(ctx);

    // Register functions
    inline for (.{
        .{ "lookup", dnsLookup, 3 },
        .{ "resolve4", dnsResolve4, 1 },
        .{ "resolve6", dnsResolve6, 1 },
        .{ "reverse", dnsReverse, 1 },
    }) |binding| {
        const func = qjs.JS_NewCFunction(ctx, binding[1], binding[0], binding[2]);
        _ = qjs.JS_SetPropertyStr(ctx, dns_obj, binding[0], func);
    }

    // Set as _dns on global (JS polyfill will use this)
    _ = qjs.JS_SetPropertyStr(ctx, global, "_dns", dns_obj);
}
