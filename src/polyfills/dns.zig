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
    @cInclude("resolv.h");
    @cInclude("arpa/nameser.h");
});

// DNS record types from arpa/nameser.h
const T_A = 1;
const T_NS = 2;
const T_CNAME = 5;
const T_SOA = 6;
const T_PTR = 12;
const T_MX = 15;
const T_TXT = 16;
const T_AAAA = 28;
const T_SRV = 33;
const C_IN = 1; // Internet class

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

/// dns.lookupService(address, port) - Reverse lookup address and port to hostname and service
/// Returns: { hostname: string, service: string }
fn dnsLookupService(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 2) {
        return qjs.JS_ThrowTypeError(ctx, "dns.lookupService requires address and port arguments");
    }

    // Get address
    const addr_cstr = qjs.JS_ToCString(ctx, argv[0]);
    if (addr_cstr == null) {
        return qjs.JS_ThrowTypeError(ctx, "address must be a string");
    }
    defer qjs.JS_FreeCString(ctx, addr_cstr);

    // Get port
    var port: i32 = 0;
    if (qjs.JS_ToInt32(ctx, &port, argv[1]) < 0) {
        return qjs.JS_ThrowTypeError(ctx, "port must be a number");
    }
    if (port < 0 or port > 65535) {
        return qjs.JS_ThrowRangeError(ctx, "port must be between 0 and 65535");
    }

    var host_buf: [c.NI_MAXHOST]u8 = undefined;
    var serv_buf: [c.NI_MAXSERV]u8 = undefined;

    // Try to parse as IPv4
    var sa4: c.sockaddr_in = std.mem.zeroes(c.sockaddr_in);
    sa4.sin_family = c.AF_INET;
    sa4.sin_port = std.mem.nativeToBig(u16, @intCast(port));

    var sa6: c.sockaddr_in6 = std.mem.zeroes(c.sockaddr_in6);
    sa6.sin6_family = c.AF_INET6;
    sa6.sin6_port = std.mem.nativeToBig(u16, @intCast(port));

    var status: c_int = 0;

    if (c.inet_pton(c.AF_INET, addr_cstr, &sa4.sin_addr) == 1) {
        // IPv4 lookup
        status = c.getnameinfo(
            @ptrCast(&sa4),
            @sizeOf(c.sockaddr_in),
            &host_buf,
            c.NI_MAXHOST,
            &serv_buf,
            c.NI_MAXSERV,
            0,
        );
    } else if (c.inet_pton(c.AF_INET6, addr_cstr, &sa6.sin6_addr) == 1) {
        // IPv6 lookup
        status = c.getnameinfo(
            @ptrCast(&sa6),
            @sizeOf(c.sockaddr_in6),
            &host_buf,
            c.NI_MAXHOST,
            &serv_buf,
            c.NI_MAXSERV,
            0,
        );
    } else {
        const err = qjs.JS_NewObject(ctx);
        _ = qjs.JS_SetPropertyStr(ctx, err, "code", qjs.JS_NewString(ctx, "EINVAL"));
        _ = qjs.JS_SetPropertyStr(ctx, err, "message", qjs.JS_NewString(ctx, "Invalid IP address"));
        return qjs.JS_Throw(ctx, err);
    }

    if (status != 0) {
        const err = qjs.JS_NewObject(ctx);
        _ = qjs.JS_SetPropertyStr(ctx, err, "code", qjs.JS_NewString(ctx, "ENOTFOUND"));
        _ = qjs.JS_SetPropertyStr(ctx, err, "message", qjs.JS_NewString(ctx, "getNameInfo ENOTFOUND"));
        return qjs.JS_Throw(ctx, err);
    }

    // Return { hostname, service } object
    const hostname = std.mem.sliceTo(&host_buf, 0);
    const service = std.mem.sliceTo(&serv_buf, 0);
    const obj = qjs.JS_NewObject(ctx);
    _ = qjs.JS_SetPropertyStr(ctx, obj, "hostname", qjs.JS_NewStringLen(ctx, hostname.ptr, hostname.len));
    _ = qjs.JS_SetPropertyStr(ctx, obj, "service", qjs.JS_NewStringLen(ctx, service.ptr, service.len));
    return obj;
}

// DNS response buffer
var dns_response_buf: [4096]u8 = undefined;

// Helper to skip a DNS name in the response
fn skipDnsName(data: []const u8, offset: usize) usize {
    var pos = offset;
    while (pos < data.len) {
        const len = data[pos];
        if (len == 0) {
            return pos + 1;
        }
        if ((len & 0xC0) == 0xC0) {
            // Compressed name pointer
            return pos + 2;
        }
        pos += 1 + len;
    }
    return pos;
}

// Helper to read a DNS name (expanding compression)
fn readDnsName(data: []const u8, offset: usize, buf: []u8) struct { len: usize, next_offset: usize } {
    var pos = offset;
    var buf_pos: usize = 0;
    var jumps: u8 = 0;
    var final_pos = offset;
    var first_jump = true;

    while (pos < data.len and jumps < 10) {
        const len = data[pos];
        if (len == 0) {
            if (first_jump) final_pos = pos + 1;
            break;
        }
        if ((len & 0xC0) == 0xC0) {
            // Compression pointer
            if (first_jump) {
                final_pos = pos + 2;
                first_jump = false;
            }
            if (pos + 1 >= data.len) break;
            const new_offset = (@as(usize, len & 0x3F) << 8) | @as(usize, data[pos + 1]);
            pos = new_offset;
            jumps += 1;
            continue;
        }
        // Copy label
        if (buf_pos > 0 and buf_pos < buf.len) {
            buf[buf_pos] = '.';
            buf_pos += 1;
        }
        const label_len: usize = len;
        if (pos + 1 + label_len > data.len) break;
        const copy_len = @min(label_len, buf.len - buf_pos);
        @memcpy(buf[buf_pos..][0..copy_len], data[pos + 1 ..][0..copy_len]);
        buf_pos += copy_len;
        pos += 1 + label_len;
        if (first_jump) final_pos = pos;
    }
    return .{ .len = buf_pos, .next_offset = final_pos };
}

/// dns.resolveMx(hostname) - Resolve MX records
/// Returns: [{ exchange: string, priority: number }]
fn dnsResolveMx(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) {
        return qjs.JS_ThrowTypeError(ctx, "dns.resolveMx requires hostname argument");
    }

    const hostname_cstr = qjs.JS_ToCString(ctx, argv[0]);
    if (hostname_cstr == null) {
        return qjs.JS_ThrowTypeError(ctx, "hostname must be a string");
    }
    defer qjs.JS_FreeCString(ctx, hostname_cstr);

    // Query DNS for MX records
    const len = c.res_query(hostname_cstr, C_IN, T_MX, &dns_response_buf, dns_response_buf.len);
    if (len < 0 or len < 12) {
        // Return empty array on failure
        return qjs.JS_NewArray(ctx);
    }

    const response_len: usize = @intCast(len);
    const data = dns_response_buf[0..response_len];

    // Parse DNS header
    const qdcount = (@as(u16, data[4]) << 8) | @as(u16, data[5]);
    const ancount = (@as(u16, data[6]) << 8) | @as(u16, data[7]);

    // Skip header (12 bytes) and questions
    var offset: usize = 12;
    for (0..qdcount) |_| {
        offset = skipDnsName(data, offset);
        offset += 4; // QTYPE + QCLASS
    }

    // Parse answers
    const arr = qjs.JS_NewArray(ctx);
    var idx: u32 = 0;

    for (0..ancount) |_| {
        if (offset + 12 > data.len) break;

        offset = skipDnsName(data, offset); // Skip name
        const rtype = (@as(u16, data[offset]) << 8) | @as(u16, data[offset + 1]);
        const rdlength = (@as(u16, data[offset + 8]) << 8) | @as(u16, data[offset + 9]);
        offset += 10; // TYPE(2) + CLASS(2) + TTL(4) + RDLENGTH(2)

        if (rtype == T_MX and rdlength >= 3) {
            // MX record: preference (2 bytes) + exchange name
            const priority = (@as(u16, data[offset]) << 8) | @as(u16, data[offset + 1]);

            var name_buf: [256]u8 = undefined;
            const name_result = readDnsName(data, offset + 2, &name_buf);

            const obj = qjs.JS_NewObject(ctx);
            _ = qjs.JS_SetPropertyStr(ctx, obj, "exchange", qjs.JS_NewStringLen(ctx, &name_buf, name_result.len));
            _ = qjs.JS_SetPropertyStr(ctx, obj, "priority", qjs.JS_NewInt32(ctx, @intCast(priority)));
            _ = qjs.JS_SetPropertyUint32(ctx, arr, idx, obj);
            idx += 1;
        }
        offset += rdlength;
    }

    return arr;
}

/// dns.resolveTxt(hostname) - Resolve TXT records
/// Returns: [[string]] (array of arrays of strings, each TXT record can have multiple strings)
fn dnsResolveTxt(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) {
        return qjs.JS_ThrowTypeError(ctx, "dns.resolveTxt requires hostname argument");
    }

    const hostname_cstr = qjs.JS_ToCString(ctx, argv[0]);
    if (hostname_cstr == null) {
        return qjs.JS_ThrowTypeError(ctx, "hostname must be a string");
    }
    defer qjs.JS_FreeCString(ctx, hostname_cstr);

    const len = c.res_query(hostname_cstr, C_IN, T_TXT, &dns_response_buf, dns_response_buf.len);
    if (len < 0 or len < 12) {
        return qjs.JS_NewArray(ctx);
    }

    const response_len: usize = @intCast(len);
    const data = dns_response_buf[0..response_len];

    const qdcount = (@as(u16, data[4]) << 8) | @as(u16, data[5]);
    const ancount = (@as(u16, data[6]) << 8) | @as(u16, data[7]);

    var offset: usize = 12;
    for (0..qdcount) |_| {
        offset = skipDnsName(data, offset);
        offset += 4;
    }

    const arr = qjs.JS_NewArray(ctx);
    var idx: u32 = 0;

    for (0..ancount) |_| {
        if (offset + 12 > data.len) break;

        offset = skipDnsName(data, offset);
        const rtype = (@as(u16, data[offset]) << 8) | @as(u16, data[offset + 1]);
        const rdlength = (@as(u16, data[offset + 8]) << 8) | @as(u16, data[offset + 9]);
        offset += 10;

        if (rtype == T_TXT and rdlength >= 1) {
            // TXT record: one or more length-prefixed strings
            const txt_arr = qjs.JS_NewArray(ctx);
            var txt_offset: usize = 0;
            var txt_idx: u32 = 0;

            while (txt_offset < rdlength) {
                const str_len: usize = data[offset + txt_offset];
                txt_offset += 1;
                if (txt_offset + str_len > rdlength) break;

                _ = qjs.JS_SetPropertyUint32(ctx, txt_arr, txt_idx, qjs.JS_NewStringLen(ctx, @ptrCast(&data[offset + txt_offset]), str_len));
                txt_idx += 1;
                txt_offset += str_len;
            }

            _ = qjs.JS_SetPropertyUint32(ctx, arr, idx, txt_arr);
            idx += 1;
        }
        offset += rdlength;
    }

    return arr;
}

/// dns.resolveSrv(hostname) - Resolve SRV records
/// Returns: [{ name: string, port: number, priority: number, weight: number }]
fn dnsResolveSrv(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) {
        return qjs.JS_ThrowTypeError(ctx, "dns.resolveSrv requires hostname argument");
    }

    const hostname_cstr = qjs.JS_ToCString(ctx, argv[0]);
    if (hostname_cstr == null) {
        return qjs.JS_ThrowTypeError(ctx, "hostname must be a string");
    }
    defer qjs.JS_FreeCString(ctx, hostname_cstr);

    const len = c.res_query(hostname_cstr, C_IN, T_SRV, &dns_response_buf, dns_response_buf.len);
    if (len < 0 or len < 12) {
        return qjs.JS_NewArray(ctx);
    }

    const response_len: usize = @intCast(len);
    const data = dns_response_buf[0..response_len];

    const qdcount = (@as(u16, data[4]) << 8) | @as(u16, data[5]);
    const ancount = (@as(u16, data[6]) << 8) | @as(u16, data[7]);

    var offset: usize = 12;
    for (0..qdcount) |_| {
        offset = skipDnsName(data, offset);
        offset += 4;
    }

    const arr = qjs.JS_NewArray(ctx);
    var idx: u32 = 0;

    for (0..ancount) |_| {
        if (offset + 12 > data.len) break;

        offset = skipDnsName(data, offset);
        const rtype = (@as(u16, data[offset]) << 8) | @as(u16, data[offset + 1]);
        const rdlength = (@as(u16, data[offset + 8]) << 8) | @as(u16, data[offset + 9]);
        offset += 10;

        if (rtype == T_SRV and rdlength >= 7) {
            // SRV record: priority(2) + weight(2) + port(2) + target name
            const priority = (@as(u16, data[offset]) << 8) | @as(u16, data[offset + 1]);
            const weight = (@as(u16, data[offset + 2]) << 8) | @as(u16, data[offset + 3]);
            const port = (@as(u16, data[offset + 4]) << 8) | @as(u16, data[offset + 5]);

            var name_buf: [256]u8 = undefined;
            const name_result = readDnsName(data, offset + 6, &name_buf);

            const obj = qjs.JS_NewObject(ctx);
            _ = qjs.JS_SetPropertyStr(ctx, obj, "name", qjs.JS_NewStringLen(ctx, &name_buf, name_result.len));
            _ = qjs.JS_SetPropertyStr(ctx, obj, "port", qjs.JS_NewInt32(ctx, @intCast(port)));
            _ = qjs.JS_SetPropertyStr(ctx, obj, "priority", qjs.JS_NewInt32(ctx, @intCast(priority)));
            _ = qjs.JS_SetPropertyStr(ctx, obj, "weight", qjs.JS_NewInt32(ctx, @intCast(weight)));
            _ = qjs.JS_SetPropertyUint32(ctx, arr, idx, obj);
            idx += 1;
        }
        offset += rdlength;
    }

    return arr;
}

/// dns.resolveCname(hostname) - Resolve CNAME records
/// Returns: [string] (array of canonical names)
fn dnsResolveCname(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) {
        return qjs.JS_ThrowTypeError(ctx, "dns.resolveCname requires hostname argument");
    }

    const hostname_cstr = qjs.JS_ToCString(ctx, argv[0]);
    if (hostname_cstr == null) {
        return qjs.JS_ThrowTypeError(ctx, "hostname must be a string");
    }
    defer qjs.JS_FreeCString(ctx, hostname_cstr);

    const len = c.res_query(hostname_cstr, C_IN, T_CNAME, &dns_response_buf, dns_response_buf.len);
    if (len < 0 or len < 12) {
        return qjs.JS_NewArray(ctx);
    }

    const response_len: usize = @intCast(len);
    const data = dns_response_buf[0..response_len];

    const qdcount = (@as(u16, data[4]) << 8) | @as(u16, data[5]);
    const ancount = (@as(u16, data[6]) << 8) | @as(u16, data[7]);

    var offset: usize = 12;
    for (0..qdcount) |_| {
        offset = skipDnsName(data, offset);
        offset += 4;
    }

    const arr = qjs.JS_NewArray(ctx);
    var idx: u32 = 0;

    for (0..ancount) |_| {
        if (offset + 12 > data.len) break;

        offset = skipDnsName(data, offset);
        const rtype = (@as(u16, data[offset]) << 8) | @as(u16, data[offset + 1]);
        const rdlength = (@as(u16, data[offset + 8]) << 8) | @as(u16, data[offset + 9]);
        offset += 10;

        if (rtype == T_CNAME and rdlength >= 1) {
            var name_buf: [256]u8 = undefined;
            const name_result = readDnsName(data, offset, &name_buf);

            _ = qjs.JS_SetPropertyUint32(ctx, arr, idx, qjs.JS_NewStringLen(ctx, &name_buf, name_result.len));
            idx += 1;
        }
        offset += rdlength;
    }

    return arr;
}

/// dns.resolveNs(hostname) - Resolve NS (nameserver) records
/// Returns: [string] (array of nameserver hostnames)
fn dnsResolveNs(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) {
        return qjs.JS_ThrowTypeError(ctx, "dns.resolveNs requires hostname argument");
    }

    const hostname_cstr = qjs.JS_ToCString(ctx, argv[0]);
    if (hostname_cstr == null) {
        return qjs.JS_ThrowTypeError(ctx, "hostname must be a string");
    }
    defer qjs.JS_FreeCString(ctx, hostname_cstr);

    const len = c.res_query(hostname_cstr, C_IN, T_NS, &dns_response_buf, dns_response_buf.len);
    if (len < 0 or len < 12) {
        return qjs.JS_NewArray(ctx);
    }

    const response_len: usize = @intCast(len);
    const data = dns_response_buf[0..response_len];

    const qdcount = (@as(u16, data[4]) << 8) | @as(u16, data[5]);
    const ancount = (@as(u16, data[6]) << 8) | @as(u16, data[7]);

    var offset: usize = 12;
    for (0..qdcount) |_| {
        offset = skipDnsName(data, offset);
        offset += 4;
    }

    const arr = qjs.JS_NewArray(ctx);
    var idx: u32 = 0;

    for (0..ancount) |_| {
        if (offset + 12 > data.len) break;

        offset = skipDnsName(data, offset);
        const rtype = (@as(u16, data[offset]) << 8) | @as(u16, data[offset + 1]);
        const rdlength = (@as(u16, data[offset + 8]) << 8) | @as(u16, data[offset + 9]);
        offset += 10;

        if (rtype == T_NS and rdlength >= 1) {
            var name_buf: [256]u8 = undefined;
            const name_result = readDnsName(data, offset, &name_buf);

            _ = qjs.JS_SetPropertyUint32(ctx, arr, idx, qjs.JS_NewStringLen(ctx, &name_buf, name_result.len));
            idx += 1;
        }
        offset += rdlength;
    }

    return arr;
}

/// dns.resolveSoa(hostname) - Resolve SOA (Start of Authority) record
/// Returns: { nsname: string, hostmaster: string, serial: number, refresh: number, retry: number, expire: number, minttl: number }
fn dnsResolveSoa(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) {
        return qjs.JS_ThrowTypeError(ctx, "dns.resolveSoa requires hostname argument");
    }

    const hostname_cstr = qjs.JS_ToCString(ctx, argv[0]);
    if (hostname_cstr == null) {
        return qjs.JS_ThrowTypeError(ctx, "hostname must be a string");
    }
    defer qjs.JS_FreeCString(ctx, hostname_cstr);

    const len = c.res_query(hostname_cstr, C_IN, T_SOA, &dns_response_buf, dns_response_buf.len);
    if (len < 0 or len < 12) {
        const err = qjs.JS_NewObject(ctx);
        _ = qjs.JS_SetPropertyStr(ctx, err, "code", qjs.JS_NewString(ctx, "ENODATA"));
        return qjs.JS_Throw(ctx, err);
    }

    const response_len: usize = @intCast(len);
    const data = dns_response_buf[0..response_len];

    const qdcount = (@as(u16, data[4]) << 8) | @as(u16, data[5]);
    const ancount = (@as(u16, data[6]) << 8) | @as(u16, data[7]);

    var offset: usize = 12;
    for (0..qdcount) |_| {
        offset = skipDnsName(data, offset);
        offset += 4;
    }

    for (0..ancount) |_| {
        if (offset + 12 > data.len) break;

        offset = skipDnsName(data, offset);
        const rtype = (@as(u16, data[offset]) << 8) | @as(u16, data[offset + 1]);
        const rdlength = (@as(u16, data[offset + 8]) << 8) | @as(u16, data[offset + 9]);
        offset += 10;

        if (rtype == T_SOA and rdlength >= 22) {
            // SOA record: nsname + hostmaster + serial + refresh + retry + expire + minttl
            var nsname_buf: [256]u8 = undefined;
            const nsname_result = readDnsName(data, offset, &nsname_buf);
            var soa_offset = nsname_result.next_offset;

            var hostmaster_buf: [256]u8 = undefined;
            const hostmaster_result = readDnsName(data, soa_offset, &hostmaster_buf);
            soa_offset = hostmaster_result.next_offset;

            // Read 5 32-bit integers
            if (soa_offset + 20 > data.len) break;

            const serial = (@as(u32, data[soa_offset]) << 24) | (@as(u32, data[soa_offset + 1]) << 16) |
                (@as(u32, data[soa_offset + 2]) << 8) | @as(u32, data[soa_offset + 3]);
            const refresh = (@as(u32, data[soa_offset + 4]) << 24) | (@as(u32, data[soa_offset + 5]) << 16) |
                (@as(u32, data[soa_offset + 6]) << 8) | @as(u32, data[soa_offset + 7]);
            const retry = (@as(u32, data[soa_offset + 8]) << 24) | (@as(u32, data[soa_offset + 9]) << 16) |
                (@as(u32, data[soa_offset + 10]) << 8) | @as(u32, data[soa_offset + 11]);
            const expire = (@as(u32, data[soa_offset + 12]) << 24) | (@as(u32, data[soa_offset + 13]) << 16) |
                (@as(u32, data[soa_offset + 14]) << 8) | @as(u32, data[soa_offset + 15]);
            const minttl = (@as(u32, data[soa_offset + 16]) << 24) | (@as(u32, data[soa_offset + 17]) << 16) |
                (@as(u32, data[soa_offset + 18]) << 8) | @as(u32, data[soa_offset + 19]);

            const obj = qjs.JS_NewObject(ctx);
            _ = qjs.JS_SetPropertyStr(ctx, obj, "nsname", qjs.JS_NewStringLen(ctx, &nsname_buf, nsname_result.len));
            _ = qjs.JS_SetPropertyStr(ctx, obj, "hostmaster", qjs.JS_NewStringLen(ctx, &hostmaster_buf, hostmaster_result.len));
            _ = qjs.JS_SetPropertyStr(ctx, obj, "serial", qjs.JS_NewUint32(ctx, serial));
            _ = qjs.JS_SetPropertyStr(ctx, obj, "refresh", qjs.JS_NewUint32(ctx, refresh));
            _ = qjs.JS_SetPropertyStr(ctx, obj, "retry", qjs.JS_NewUint32(ctx, retry));
            _ = qjs.JS_SetPropertyStr(ctx, obj, "expire", qjs.JS_NewUint32(ctx, expire));
            _ = qjs.JS_SetPropertyStr(ctx, obj, "minttl", qjs.JS_NewUint32(ctx, minttl));
            return obj;
        }
        offset += rdlength;
    }

    const err = qjs.JS_NewObject(ctx);
    _ = qjs.JS_SetPropertyStr(ctx, err, "code", qjs.JS_NewString(ctx, "ENODATA"));
    return qjs.JS_Throw(ctx, err);
}

/// dns.resolvePtr(ip) - Resolve PTR (pointer) records for reverse DNS
/// Returns: [string] (array of hostnames)
fn dnsResolvePtr(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) {
        return qjs.JS_ThrowTypeError(ctx, "dns.resolvePtr requires hostname argument");
    }

    const hostname_cstr = qjs.JS_ToCString(ctx, argv[0]);
    if (hostname_cstr == null) {
        return qjs.JS_ThrowTypeError(ctx, "hostname must be a string");
    }
    defer qjs.JS_FreeCString(ctx, hostname_cstr);

    const len = c.res_query(hostname_cstr, C_IN, T_PTR, &dns_response_buf, dns_response_buf.len);
    if (len < 0 or len < 12) {
        return qjs.JS_NewArray(ctx);
    }

    const response_len: usize = @intCast(len);
    const data = dns_response_buf[0..response_len];

    const qdcount = (@as(u16, data[4]) << 8) | @as(u16, data[5]);
    const ancount = (@as(u16, data[6]) << 8) | @as(u16, data[7]);

    var offset: usize = 12;
    for (0..qdcount) |_| {
        offset = skipDnsName(data, offset);
        offset += 4;
    }

    const arr = qjs.JS_NewArray(ctx);
    var idx: u32 = 0;

    for (0..ancount) |_| {
        if (offset + 12 > data.len) break;

        offset = skipDnsName(data, offset);
        const rtype = (@as(u16, data[offset]) << 8) | @as(u16, data[offset + 1]);
        const rdlength = (@as(u16, data[offset + 8]) << 8) | @as(u16, data[offset + 9]);
        offset += 10;

        if (rtype == T_PTR and rdlength >= 1) {
            var name_buf: [256]u8 = undefined;
            const name_result = readDnsName(data, offset, &name_buf);

            _ = qjs.JS_SetPropertyUint32(ctx, arr, idx, qjs.JS_NewStringLen(ctx, &name_buf, name_result.len));
            idx += 1;
        }
        offset += rdlength;
    }

    return arr;
}

/// dns.resolve(hostname, rrtype) - Generic resolver for any record type
/// Returns: varies by record type
fn dnsResolve(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) {
        return qjs.JS_ThrowTypeError(ctx, "dns.resolve requires hostname argument");
    }

    // Default to 'A' record type
    var rrtype_cstr: [*c]const u8 = "A";
    var should_free = false;

    if (argc >= 2 and qjs.JS_IsString(argv[1])) {
        const tmp = qjs.JS_ToCString(ctx, argv[1]);
        if (tmp != null) {
            rrtype_cstr = tmp;
            should_free = true;
        }
    }
    defer if (should_free) qjs.JS_FreeCString(ctx, rrtype_cstr);

    const rrtype = std.mem.span(rrtype_cstr);

    // Dispatch to appropriate resolver based on record type
    if (std.ascii.eqlIgnoreCase(rrtype, "A")) {
        return dnsResolve4(ctx, quickjs.jsUndefined(), argc, argv);
    } else if (std.ascii.eqlIgnoreCase(rrtype, "AAAA")) {
        return dnsResolve6(ctx, quickjs.jsUndefined(), argc, argv);
    } else if (std.ascii.eqlIgnoreCase(rrtype, "CNAME")) {
        return dnsResolveCname(ctx, quickjs.jsUndefined(), argc, argv);
    } else if (std.ascii.eqlIgnoreCase(rrtype, "MX")) {
        return dnsResolveMx(ctx, quickjs.jsUndefined(), argc, argv);
    } else if (std.ascii.eqlIgnoreCase(rrtype, "NS")) {
        return dnsResolveNs(ctx, quickjs.jsUndefined(), argc, argv);
    } else if (std.ascii.eqlIgnoreCase(rrtype, "PTR")) {
        return dnsResolvePtr(ctx, quickjs.jsUndefined(), argc, argv);
    } else if (std.ascii.eqlIgnoreCase(rrtype, "SOA")) {
        return dnsResolveSoa(ctx, quickjs.jsUndefined(), argc, argv);
    } else if (std.ascii.eqlIgnoreCase(rrtype, "SRV")) {
        return dnsResolveSrv(ctx, quickjs.jsUndefined(), argc, argv);
    } else if (std.ascii.eqlIgnoreCase(rrtype, "TXT")) {
        return dnsResolveTxt(ctx, quickjs.jsUndefined(), argc, argv);
    } else {
        const err = qjs.JS_NewObject(ctx);
        _ = qjs.JS_SetPropertyStr(ctx, err, "code", qjs.JS_NewString(ctx, "ENOTSUP"));
        _ = qjs.JS_SetPropertyStr(ctx, err, "message", qjs.JS_NewString(ctx, "Unsupported record type"));
        return qjs.JS_Throw(ctx, err);
    }
}

/// Register DNS module functions
pub fn register(ctx: ?*qjs.JSContext) void {
    const global = qjs.JS_GetGlobalObject(ctx);
    defer qjs.JS_FreeValue(ctx, global);

    // Create dns object
    const dns_obj = qjs.JS_NewObject(ctx);

    // Register functions
    inline for (.{
        .{ "lookup", dnsLookup, 3 },
        .{ "resolve", dnsResolve, 2 },
        .{ "resolve4", dnsResolve4, 1 },
        .{ "resolve6", dnsResolve6, 1 },
        .{ "reverse", dnsReverse, 1 },
        .{ "resolveCname", dnsResolveCname, 1 },
        .{ "resolveMx", dnsResolveMx, 1 },
        .{ "resolveNs", dnsResolveNs, 1 },
        .{ "resolvePtr", dnsResolvePtr, 1 },
        .{ "resolveSoa", dnsResolveSoa, 1 },
        .{ "resolveSrv", dnsResolveSrv, 1 },
        .{ "resolveTxt", dnsResolveTxt, 1 },
        .{ "lookupService", dnsLookupService, 2 },
    }) |binding| {
        const func = qjs.JS_NewCFunction(ctx, binding[1], binding[0], binding[2]);
        _ = qjs.JS_SetPropertyStr(ctx, dns_obj, binding[0], func);
    }

    // Set as _dns on global (for backwards compatibility)
    _ = qjs.JS_SetPropertyStr(ctx, global, "_dns", qjs.JS_DupValue(ctx, dns_obj));

    // Set in _modules for require('dns')
    const modules_val = qjs.JS_GetPropertyStr(ctx, global, "_modules");
    if (!qjs.JS_IsUndefined(modules_val)) {
        _ = qjs.JS_SetPropertyStr(ctx, modules_val, "dns", qjs.JS_DupValue(ctx, dns_obj));
        _ = qjs.JS_SetPropertyStr(ctx, modules_val, "node:dns", dns_obj);
        qjs.JS_FreeValue(ctx, modules_val);
    } else {
        qjs.JS_FreeValue(ctx, dns_obj);
    }
}
