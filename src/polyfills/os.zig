/// Native os module - QuickJS C functions
/// Implements Node.js os module functionality in native Zig
const std = @import("std");
const builtin = @import("builtin");
const quickjs = @import("../quickjs_core.zig");
const qjs = quickjs.c;

// External C functions for system info
const c = struct {
    // getloadavg (macOS/Linux)
    extern fn getloadavg(loadavg: [*]f64, nelem: c_int) c_int;

    // macOS sysctl
    extern fn sysctlbyname(name: [*:0]const u8, oldp: ?*anyopaque, oldlenp: ?*usize, newp: ?*const anyopaque, newlen: usize) c_int;

    // POSIX getpwuid
    extern fn getpwuid(uid: c_uint) ?*const passwd;

    // Network interfaces
    extern fn getifaddrs(ifap: *?*ifaddrs) c_int;
    extern fn freeifaddrs(ifa: ?*ifaddrs) void;
};

// passwd structure for getpwuid (platform-specific)
const passwd = if (builtin.os.tag == .macos)
    extern struct {
        pw_name: [*:0]const u8,
        pw_passwd: [*:0]const u8,
        pw_uid: c_uint,
        pw_gid: c_uint,
        pw_change: c_long,
        pw_class: [*:0]const u8,
        pw_gecos: [*:0]const u8,
        pw_dir: [*:0]const u8,
        pw_shell: [*:0]const u8,
        pw_expire: c_long,
    }
else
    extern struct {
        pw_name: [*:0]const u8,
        pw_passwd: [*:0]const u8,
        pw_uid: c_uint,
        pw_gid: c_uint,
        pw_gecos: [*:0]const u8,
        pw_dir: [*:0]const u8,
        pw_shell: [*:0]const u8,
    };

// ifaddrs structure for getifaddrs
const ifaddrs = extern struct {
    ifa_next: ?*ifaddrs,
    ifa_name: [*:0]const u8,
    ifa_flags: c_uint,
    ifa_addr: ?*sockaddr,
    ifa_netmask: ?*sockaddr,
    ifa_dstaddr: ?*sockaddr,
    ifa_data: ?*anyopaque,
};

// sockaddr structure
const sockaddr = extern struct {
    sa_len: u8,
    sa_family: u8,
    sa_data: [14]u8,
};

// sockaddr_in for IPv4
const sockaddr_in = extern struct {
    sin_len: u8,
    sin_family: u8,
    sin_port: u16,
    sin_addr: u32,
    sin_zero: [8]u8,
};

// Address family constants
const AF_INET: u8 = 2;
const AF_INET6: u8 = if (builtin.os.tag == .linux) 10 else 30;

/// os.hostname() - Get system hostname
fn osHostname(ctx: ?*qjs.JSContext, _: qjs.JSValue, _: c_int, _: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (builtin.os.tag == .wasi) {
        return qjs.JS_NewString(ctx, "edgebox");
    }

    var hostname_buf: [72]u8 = undefined;
    if (std.posix.gethostname(&hostname_buf)) |hostname| {
        return qjs.JS_NewStringLen(ctx, hostname.ptr, hostname.len);
    } else |_| {
        return qjs.JS_NewString(ctx, "localhost");
    }
}

/// os.loadavg() - Get system load averages [1min, 5min, 15min]
fn osLoadavg(ctx: ?*qjs.JSContext, _: qjs.JSValue, _: c_int, _: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    const arr = qjs.JS_NewArray(ctx);

    if (builtin.os.tag == .wasi or builtin.os.tag == .windows) {
        // Return [0, 0, 0] for unsupported platforms
        _ = qjs.JS_SetPropertyUint32(ctx, arr, 0, qjs.JS_NewFloat64(ctx, 0));
        _ = qjs.JS_SetPropertyUint32(ctx, arr, 1, qjs.JS_NewFloat64(ctx, 0));
        _ = qjs.JS_SetPropertyUint32(ctx, arr, 2, qjs.JS_NewFloat64(ctx, 0));
        return arr;
    }

    var loadavg: [3]f64 = undefined;
    const result = c.getloadavg(&loadavg, 3);

    if (result == 3) {
        _ = qjs.JS_SetPropertyUint32(ctx, arr, 0, qjs.JS_NewFloat64(ctx, loadavg[0]));
        _ = qjs.JS_SetPropertyUint32(ctx, arr, 1, qjs.JS_NewFloat64(ctx, loadavg[1]));
        _ = qjs.JS_SetPropertyUint32(ctx, arr, 2, qjs.JS_NewFloat64(ctx, loadavg[2]));
    } else {
        _ = qjs.JS_SetPropertyUint32(ctx, arr, 0, qjs.JS_NewFloat64(ctx, 0));
        _ = qjs.JS_SetPropertyUint32(ctx, arr, 1, qjs.JS_NewFloat64(ctx, 0));
        _ = qjs.JS_SetPropertyUint32(ctx, arr, 2, qjs.JS_NewFloat64(ctx, 0));
    }

    return arr;
}

/// os.uptime() - Get system uptime in seconds
fn osUptime(ctx: ?*qjs.JSContext, _: qjs.JSValue, _: c_int, _: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (builtin.os.tag == .wasi or builtin.os.tag == .windows) {
        return qjs.JS_NewFloat64(ctx, 0);
    }

    if (builtin.os.tag == .macos) {
        // macOS: sysctl kern.boottime
        var boottime: std.posix.timeval = undefined;
        var size: usize = @sizeOf(std.posix.timeval);
        const result = c.sysctlbyname("kern.boottime", &boottime, &size, null, 0);
        if (result == 0) {
            const now = std.time.timestamp();
            const uptime = now - boottime.sec;
            return qjs.JS_NewFloat64(ctx, @floatFromInt(uptime));
        }
    } else if (builtin.os.tag == .linux) {
        // Linux: read from /proc/uptime
        var buf: [64]u8 = undefined;
        if (std.fs.cwd().openFile("/proc/uptime", .{})) |file| {
            defer file.close();
            const bytes_read = file.read(&buf) catch 0;
            if (bytes_read > 0) {
                var it = std.mem.splitScalar(u8, buf[0..bytes_read], ' ');
                if (it.next()) |uptime_str| {
                    const uptime = std.fmt.parseFloat(f64, uptime_str) catch 0;
                    return qjs.JS_NewFloat64(ctx, uptime);
                }
            }
        } else |_| {}
    }

    return qjs.JS_NewFloat64(ctx, 0);
}

/// os.totalmem() - Get total system memory in bytes
fn osTotalmem(ctx: ?*qjs.JSContext, _: qjs.JSValue, _: c_int, _: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (builtin.os.tag == .wasi) {
        return qjs.JS_NewInt64(ctx, 4 * 1024 * 1024 * 1024); // 4GB default
    }

    if (builtin.os.tag == .macos) {
        // macOS: sysctl hw.memsize
        var memsize: u64 = 0;
        var size: usize = @sizeOf(u64);
        const result = c.sysctlbyname("hw.memsize", &memsize, &size, null, 0);
        if (result == 0) {
            return qjs.JS_NewInt64(ctx, @intCast(memsize));
        }
    } else if (builtin.os.tag == .linux) {
        // Linux: read from /proc/meminfo
        var buf: [256]u8 = undefined;
        if (std.fs.cwd().openFile("/proc/meminfo", .{})) |file| {
            defer file.close();
            const bytes_read = file.read(&buf) catch 0;
            if (bytes_read > 0) {
                // Parse "MemTotal:       XXXXX kB"
                if (std.mem.indexOf(u8, buf[0..bytes_read], "MemTotal:")) |idx| {
                    var start = idx + 9;
                    while (start < bytes_read and buf[start] == ' ') : (start += 1) {}
                    var end = start;
                    while (end < bytes_read and buf[end] >= '0' and buf[end] <= '9') : (end += 1) {}
                    if (end > start) {
                        const kb = std.fmt.parseInt(i64, buf[start..end], 10) catch 0;
                        return qjs.JS_NewInt64(ctx, kb * 1024);
                    }
                }
            }
        } else |_| {}
    }

    return qjs.JS_NewInt64(ctx, 4 * 1024 * 1024 * 1024);
}

/// os.freemem() - Get free system memory in bytes
fn osFreemem(ctx: ?*qjs.JSContext, _: qjs.JSValue, _: c_int, _: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (builtin.os.tag == .wasi) {
        return qjs.JS_NewInt64(ctx, 2 * 1024 * 1024 * 1024); // 2GB default
    }

    if (builtin.os.tag == .macos) {
        // macOS: vm_statistics64 via host_statistics64
        // Simplified: use sysctl vm.page_free_count * page_size
        var page_size: u32 = 0;
        var page_size_len: usize = @sizeOf(u32);
        _ = c.sysctlbyname("hw.pagesize", &page_size, &page_size_len, null, 0);

        var free_pages: u32 = 0;
        var free_pages_len: usize = @sizeOf(u32);
        const result = c.sysctlbyname("vm.page_free_count", &free_pages, &free_pages_len, null, 0);

        if (result == 0 and page_size > 0) {
            const free_mem: i64 = @as(i64, free_pages) * @as(i64, page_size);
            return qjs.JS_NewInt64(ctx, free_mem);
        }
    } else if (builtin.os.tag == .linux) {
        // Linux: read MemAvailable from /proc/meminfo
        var buf: [512]u8 = undefined;
        if (std.fs.cwd().openFile("/proc/meminfo", .{})) |file| {
            defer file.close();
            const bytes_read = file.read(&buf) catch 0;
            if (bytes_read > 0) {
                // Parse "MemAvailable:       XXXXX kB"
                if (std.mem.indexOf(u8, buf[0..bytes_read], "MemAvailable:")) |idx| {
                    var start = idx + 13;
                    while (start < bytes_read and buf[start] == ' ') : (start += 1) {}
                    var end = start;
                    while (end < bytes_read and buf[end] >= '0' and buf[end] <= '9') : (end += 1) {}
                    if (end > start) {
                        const kb = std.fmt.parseInt(i64, buf[start..end], 10) catch 0;
                        return qjs.JS_NewInt64(ctx, kb * 1024);
                    }
                }
            }
        } else |_| {}
    }

    return qjs.JS_NewInt64(ctx, 2 * 1024 * 1024 * 1024);
}

/// os.cpus() - Get CPU information
fn osCpus(ctx: ?*qjs.JSContext, _: qjs.JSValue, _: c_int, _: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    const arr = qjs.JS_NewArray(ctx);

    if (builtin.os.tag == .wasi) {
        // Return single WASM CPU
        const cpu_obj = qjs.JS_NewObject(ctx);
        _ = qjs.JS_SetPropertyStr(ctx, cpu_obj, "model", qjs.JS_NewString(ctx, "WASM"));
        _ = qjs.JS_SetPropertyStr(ctx, cpu_obj, "speed", qjs.JS_NewInt32(ctx, 0));
        const times_obj = qjs.JS_NewObject(ctx);
        _ = qjs.JS_SetPropertyStr(ctx, times_obj, "user", qjs.JS_NewInt32(ctx, 0));
        _ = qjs.JS_SetPropertyStr(ctx, times_obj, "nice", qjs.JS_NewInt32(ctx, 0));
        _ = qjs.JS_SetPropertyStr(ctx, times_obj, "sys", qjs.JS_NewInt32(ctx, 0));
        _ = qjs.JS_SetPropertyStr(ctx, times_obj, "idle", qjs.JS_NewInt32(ctx, 0));
        _ = qjs.JS_SetPropertyStr(ctx, times_obj, "irq", qjs.JS_NewInt32(ctx, 0));
        _ = qjs.JS_SetPropertyStr(ctx, cpu_obj, "times", times_obj);
        _ = qjs.JS_SetPropertyUint32(ctx, arr, 0, cpu_obj);
        return arr;
    }

    // Get number of CPUs
    var ncpu: u32 = 1;
    if (builtin.os.tag == .macos) {
        var ncpu_size: usize = @sizeOf(u32);
        _ = c.sysctlbyname("hw.ncpu", &ncpu, &ncpu_size, null, 0);
    } else if (builtin.os.tag == .linux) {
        // Read from /proc/cpuinfo or use sysconf
        ncpu = @intCast(std.Thread.getCpuCount() catch 1);
    }

    // Get CPU model (macOS)
    var cpu_model: [256]u8 = undefined;
    var cpu_model_slice: []const u8 = "Unknown";
    if (builtin.os.tag == .macos) {
        var model_size: usize = cpu_model.len;
        const result = c.sysctlbyname("machdep.cpu.brand_string", &cpu_model, &model_size, null, 0);
        if (result == 0 and model_size > 0) {
            cpu_model_slice = cpu_model[0 .. model_size - 1]; // Remove null terminator
        }
    }

    // Create CPU objects
    var i: u32 = 0;
    while (i < ncpu) : (i += 1) {
        const cpu_obj = qjs.JS_NewObject(ctx);
        _ = qjs.JS_SetPropertyStr(ctx, cpu_obj, "model", qjs.JS_NewStringLen(ctx, cpu_model_slice.ptr, cpu_model_slice.len));
        _ = qjs.JS_SetPropertyStr(ctx, cpu_obj, "speed", qjs.JS_NewInt32(ctx, 0));

        const times_obj = qjs.JS_NewObject(ctx);
        _ = qjs.JS_SetPropertyStr(ctx, times_obj, "user", qjs.JS_NewInt32(ctx, 0));
        _ = qjs.JS_SetPropertyStr(ctx, times_obj, "nice", qjs.JS_NewInt32(ctx, 0));
        _ = qjs.JS_SetPropertyStr(ctx, times_obj, "sys", qjs.JS_NewInt32(ctx, 0));
        _ = qjs.JS_SetPropertyStr(ctx, times_obj, "idle", qjs.JS_NewInt32(ctx, 0));
        _ = qjs.JS_SetPropertyStr(ctx, times_obj, "irq", qjs.JS_NewInt32(ctx, 0));
        _ = qjs.JS_SetPropertyStr(ctx, cpu_obj, "times", times_obj);

        _ = qjs.JS_SetPropertyUint32(ctx, arr, i, cpu_obj);
    }

    return arr;
}

/// os.type() - Get OS type name
fn osType(ctx: ?*qjs.JSContext, _: qjs.JSValue, _: c_int, _: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    const type_str: [*:0]const u8 = switch (builtin.os.tag) {
        .macos => "Darwin",
        .linux => "Linux",
        .windows => "Windows_NT",
        .freebsd => "FreeBSD",
        else => "Unknown",
    };
    return qjs.JS_NewString(ctx, type_str);
}

/// os.release() - Get OS release version
fn osRelease(ctx: ?*qjs.JSContext, _: qjs.JSValue, _: c_int, _: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (builtin.os.tag == .wasi) {
        return qjs.JS_NewString(ctx, "1.0.0");
    }

    if (builtin.os.tag == .macos) {
        var release: [64]u8 = undefined;
        var release_size: usize = release.len;
        const result = c.sysctlbyname("kern.osrelease", &release, &release_size, null, 0);
        if (result == 0 and release_size > 0) {
            return qjs.JS_NewStringLen(ctx, &release, release_size - 1);
        }
    } else if (builtin.os.tag == .linux) {
        // Read from /proc/version or uname
        var buf: [256]u8 = undefined;
        if (std.fs.cwd().openFile("/proc/version", .{})) |file| {
            defer file.close();
            const bytes_read = file.read(&buf) catch 0;
            if (bytes_read > 0) {
                // Extract version number from "Linux version X.Y.Z ..."
                if (std.mem.indexOf(u8, buf[0..bytes_read], "version ")) |idx| {
                    const start = idx + 8;
                    var end = start;
                    while (end < bytes_read and buf[end] != ' ' and buf[end] != '\n') : (end += 1) {}
                    if (end > start) {
                        return qjs.JS_NewStringLen(ctx, buf[start..end].ptr, end - start);
                    }
                }
            }
        } else |_| {}
    }

    return qjs.JS_NewString(ctx, "1.0.0");
}

/// os.platform() - Get platform name
fn osPlatform(ctx: ?*qjs.JSContext, _: qjs.JSValue, _: c_int, _: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    const platform: [*:0]const u8 = switch (builtin.os.tag) {
        .macos => "darwin",
        .linux => "linux",
        .windows => "win32",
        .freebsd => "freebsd",
        else => "unknown",
    };
    return qjs.JS_NewString(ctx, platform);
}

/// os.arch() - Get CPU architecture
fn osArch(ctx: ?*qjs.JSContext, _: qjs.JSValue, _: c_int, _: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    const arch: [*:0]const u8 = switch (builtin.cpu.arch) {
        .aarch64 => "arm64",
        .x86_64 => "x64",
        .x86 => "ia32",
        .arm => "arm",
        .wasm32 => "wasm32",
        else => "unknown",
    };
    return qjs.JS_NewString(ctx, arch);
}

/// os.endianness() - Get CPU endianness
fn osEndianness(ctx: ?*qjs.JSContext, _: qjs.JSValue, _: c_int, _: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    const endian: [*:0]const u8 = if (builtin.cpu.arch.endian() == .little) "LE" else "BE";
    return qjs.JS_NewString(ctx, endian);
}

/// os.homedir() - Get user home directory
fn osHomedir(ctx: ?*qjs.JSContext, _: qjs.JSValue, _: c_int, _: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (builtin.os.tag == .wasi) {
        return qjs.JS_NewString(ctx, "/home/user");
    }

    // Try HOME environment variable first
    if (std.posix.getenv("HOME")) |home| {
        return qjs.JS_NewStringLen(ctx, home.ptr, home.len);
    }

    // Fallback to getpwuid
    if (builtin.os.tag != .windows) {
        const uid = std.c.getuid();
        if (c.getpwuid(uid)) |pw| {
            const dir = std.mem.span(pw.pw_dir);
            return qjs.JS_NewStringLen(ctx, dir.ptr, dir.len);
        }
    }

    return qjs.JS_NewString(ctx, "/home/user");
}

/// os.tmpdir() - Get temp directory
fn osTmpdir(ctx: ?*qjs.JSContext, _: qjs.JSValue, _: c_int, _: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    // Check TMPDIR, TEMP, TMP env vars
    if (std.posix.getenv("TMPDIR")) |tmpdir| {
        return qjs.JS_NewStringLen(ctx, tmpdir.ptr, tmpdir.len);
    }
    if (std.posix.getenv("TEMP")) |temp| {
        return qjs.JS_NewStringLen(ctx, temp.ptr, temp.len);
    }
    if (std.posix.getenv("TMP")) |tmp| {
        return qjs.JS_NewStringLen(ctx, tmp.ptr, tmp.len);
    }

    // Default to /tmp
    return qjs.JS_NewString(ctx, "/tmp");
}

/// os.userInfo() - Get current user info
fn osUserInfo(ctx: ?*qjs.JSContext, _: qjs.JSValue, _: c_int, _: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    const obj = qjs.JS_NewObject(ctx);

    if (builtin.os.tag == .wasi or builtin.os.tag == .windows) {
        _ = qjs.JS_SetPropertyStr(ctx, obj, "username", qjs.JS_NewString(ctx, "user"));
        _ = qjs.JS_SetPropertyStr(ctx, obj, "uid", qjs.JS_NewInt32(ctx, 1000));
        _ = qjs.JS_SetPropertyStr(ctx, obj, "gid", qjs.JS_NewInt32(ctx, 1000));
        _ = qjs.JS_SetPropertyStr(ctx, obj, "shell", qjs.JS_NewString(ctx, "/bin/sh"));
        _ = qjs.JS_SetPropertyStr(ctx, obj, "homedir", qjs.JS_NewString(ctx, "/home/user"));
        return obj;
    }

    const uid = std.c.getuid();
    if (c.getpwuid(uid)) |pw| {
        const username = std.mem.span(pw.pw_name);
        const homedir = std.mem.span(pw.pw_dir);
        const shell = std.mem.span(pw.pw_shell);

        _ = qjs.JS_SetPropertyStr(ctx, obj, "username", qjs.JS_NewStringLen(ctx, username.ptr, username.len));
        _ = qjs.JS_SetPropertyStr(ctx, obj, "uid", qjs.JS_NewInt32(ctx, @intCast(pw.pw_uid)));
        _ = qjs.JS_SetPropertyStr(ctx, obj, "gid", qjs.JS_NewInt32(ctx, @intCast(pw.pw_gid)));
        _ = qjs.JS_SetPropertyStr(ctx, obj, "shell", qjs.JS_NewStringLen(ctx, shell.ptr, shell.len));
        _ = qjs.JS_SetPropertyStr(ctx, obj, "homedir", qjs.JS_NewStringLen(ctx, homedir.ptr, homedir.len));
    } else {
        _ = qjs.JS_SetPropertyStr(ctx, obj, "username", qjs.JS_NewString(ctx, "user"));
        _ = qjs.JS_SetPropertyStr(ctx, obj, "uid", qjs.JS_NewInt32(ctx, @intCast(uid)));
        _ = qjs.JS_SetPropertyStr(ctx, obj, "gid", qjs.JS_NewInt32(ctx, 0));
        _ = qjs.JS_SetPropertyStr(ctx, obj, "shell", qjs.JS_NewString(ctx, "/bin/sh"));
        _ = qjs.JS_SetPropertyStr(ctx, obj, "homedir", qjs.JS_NewString(ctx, "/home/user"));
    }

    return obj;
}

/// os.networkInterfaces() - Get network interfaces
/// TODO: Full implementation with getifaddrs() causes crashes, returning stub for now
fn osNetworkInterfaces(ctx: ?*qjs.JSContext, _: qjs.JSValue, _: c_int, _: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    // Return empty object for now - full implementation needs debugging
    return qjs.JS_NewObject(ctx);
}

/// Register all os functions to globalThis._modules.os
pub fn register(ctx: *qjs.JSContext) void {
    const os_obj = qjs.JS_NewObject(ctx);

    // Register functions
    _ = qjs.JS_SetPropertyStr(ctx, os_obj, "hostname", qjs.JS_NewCFunction(ctx, osHostname, "hostname", 0));
    _ = qjs.JS_SetPropertyStr(ctx, os_obj, "loadavg", qjs.JS_NewCFunction(ctx, osLoadavg, "loadavg", 0));
    _ = qjs.JS_SetPropertyStr(ctx, os_obj, "uptime", qjs.JS_NewCFunction(ctx, osUptime, "uptime", 0));
    _ = qjs.JS_SetPropertyStr(ctx, os_obj, "totalmem", qjs.JS_NewCFunction(ctx, osTotalmem, "totalmem", 0));
    _ = qjs.JS_SetPropertyStr(ctx, os_obj, "freemem", qjs.JS_NewCFunction(ctx, osFreemem, "freemem", 0));
    _ = qjs.JS_SetPropertyStr(ctx, os_obj, "cpus", qjs.JS_NewCFunction(ctx, osCpus, "cpus", 0));
    _ = qjs.JS_SetPropertyStr(ctx, os_obj, "type", qjs.JS_NewCFunction(ctx, osType, "type", 0));
    _ = qjs.JS_SetPropertyStr(ctx, os_obj, "release", qjs.JS_NewCFunction(ctx, osRelease, "release", 0));
    _ = qjs.JS_SetPropertyStr(ctx, os_obj, "platform", qjs.JS_NewCFunction(ctx, osPlatform, "platform", 0));
    _ = qjs.JS_SetPropertyStr(ctx, os_obj, "arch", qjs.JS_NewCFunction(ctx, osArch, "arch", 0));
    _ = qjs.JS_SetPropertyStr(ctx, os_obj, "endianness", qjs.JS_NewCFunction(ctx, osEndianness, "endianness", 0));
    _ = qjs.JS_SetPropertyStr(ctx, os_obj, "homedir", qjs.JS_NewCFunction(ctx, osHomedir, "homedir", 0));
    _ = qjs.JS_SetPropertyStr(ctx, os_obj, "tmpdir", qjs.JS_NewCFunction(ctx, osTmpdir, "tmpdir", 0));
    _ = qjs.JS_SetPropertyStr(ctx, os_obj, "userInfo", qjs.JS_NewCFunction(ctx, osUserInfo, "userInfo", 0));
    _ = qjs.JS_SetPropertyStr(ctx, os_obj, "networkInterfaces", qjs.JS_NewCFunction(ctx, osNetworkInterfaces, "networkInterfaces", 0));

    // Constants
    _ = qjs.JS_SetPropertyStr(ctx, os_obj, "EOL", qjs.JS_NewString(ctx, "\n"));

    // os.constants object
    const constants_obj = qjs.JS_NewObject(ctx);
    _ = qjs.JS_SetPropertyStr(ctx, constants_obj, "signals", qjs.JS_NewObject(ctx));
    _ = qjs.JS_SetPropertyStr(ctx, constants_obj, "errno", qjs.JS_NewObject(ctx));
    _ = qjs.JS_SetPropertyStr(ctx, os_obj, "constants", constants_obj);

    // Add to _modules.os
    const global = qjs.JS_GetGlobalObject(ctx);
    const modules_val = qjs.JS_GetPropertyStr(ctx, global, "_modules");
    if (!qjs.JS_IsUndefined(modules_val)) {
        _ = qjs.JS_SetPropertyStr(ctx, modules_val, "os", os_obj);
        qjs.JS_FreeValue(ctx, modules_val);
    } else {
        qjs.JS_FreeValue(ctx, os_obj);
    }
    qjs.JS_FreeValue(ctx, global);
}
