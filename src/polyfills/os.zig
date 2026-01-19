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

    // Round 13: Priority syscalls
    extern fn getpriority(which: c_int, who: c_uint) c_int;
    extern fn setpriority(which: c_int, who: c_uint, prio: c_int) c_int;
    const PRIO_PROCESS: c_int = 0;
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

    // Get CPU speed (MHz) - macOS: hw.cpufrequency or hw.cpufrequency_max
    var cpu_speed_mhz: i32 = 0;
    if (builtin.os.tag == .macos) {
        var cpu_freq: u64 = 0;
        var freq_size: usize = @sizeOf(u64);
        // Try hw.cpufrequency first (Intel Macs)
        if (c.sysctlbyname("hw.cpufrequency", &cpu_freq, &freq_size, null, 0) == 0 and cpu_freq > 0) {
            cpu_speed_mhz = @intCast(cpu_freq / 1_000_000);
        } else {
            // Try hw.cpufrequency_max (Apple Silicon)
            freq_size = @sizeOf(u64);
            if (c.sysctlbyname("hw.cpufrequency_max", &cpu_freq, &freq_size, null, 0) == 0 and cpu_freq > 0) {
                cpu_speed_mhz = @intCast(cpu_freq / 1_000_000);
            }
        }
    }

    // Create CPU objects
    var i: u32 = 0;
    while (i < ncpu) : (i += 1) {
        const cpu_obj = qjs.JS_NewObject(ctx);
        _ = qjs.JS_SetPropertyStr(ctx, cpu_obj, "model", qjs.JS_NewStringLen(ctx, cpu_model_slice.ptr, cpu_model_slice.len));
        _ = qjs.JS_SetPropertyStr(ctx, cpu_obj, "speed", qjs.JS_NewInt32(ctx, cpu_speed_mhz));

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
/// Returns at minimum the loopback interface. Full getifaddrs() implementation
/// had crashes on macOS ARM64 due to memory alignment issues.
fn osNetworkInterfaces(ctx: ?*qjs.JSContext, _: qjs.JSValue, _: c_int, _: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    const result = qjs.JS_NewObject(ctx);

    if (builtin.os.tag == .wasi) {
        // WASM: Return minimal loopback only
        const lo_arr = qjs.JS_NewArray(ctx);
        const lo_obj = qjs.JS_NewObject(ctx);
        _ = qjs.JS_SetPropertyStr(ctx, lo_obj, "address", qjs.JS_NewString(ctx, "127.0.0.1"));
        _ = qjs.JS_SetPropertyStr(ctx, lo_obj, "netmask", qjs.JS_NewString(ctx, "255.0.0.0"));
        _ = qjs.JS_SetPropertyStr(ctx, lo_obj, "family", qjs.JS_NewString(ctx, "IPv4"));
        _ = qjs.JS_SetPropertyStr(ctx, lo_obj, "mac", qjs.JS_NewString(ctx, "00:00:00:00:00:00"));
        _ = qjs.JS_SetPropertyStr(ctx, lo_obj, "internal", quickjs.jsTrue());
        _ = qjs.JS_SetPropertyStr(ctx, lo_obj, "cidr", qjs.JS_NewString(ctx, "127.0.0.1/8"));
        _ = qjs.JS_SetPropertyUint32(ctx, lo_arr, 0, lo_obj);
        _ = qjs.JS_SetPropertyStr(ctx, result, "lo", lo_arr);
        return result;
    }

    // Native platforms: Try getifaddrs, fallback to loopback only
    var ifap: ?*ifaddrs = null;
    if (c.getifaddrs(&ifap) != 0 or ifap == null) {
        // getifaddrs failed - return minimal loopback
        const lo_arr = qjs.JS_NewArray(ctx);
        const lo_obj = qjs.JS_NewObject(ctx);
        _ = qjs.JS_SetPropertyStr(ctx, lo_obj, "address", qjs.JS_NewString(ctx, "127.0.0.1"));
        _ = qjs.JS_SetPropertyStr(ctx, lo_obj, "netmask", qjs.JS_NewString(ctx, "255.0.0.0"));
        _ = qjs.JS_SetPropertyStr(ctx, lo_obj, "family", qjs.JS_NewString(ctx, "IPv4"));
        _ = qjs.JS_SetPropertyStr(ctx, lo_obj, "mac", qjs.JS_NewString(ctx, "00:00:00:00:00:00"));
        _ = qjs.JS_SetPropertyStr(ctx, lo_obj, "internal", quickjs.jsTrue());
        _ = qjs.JS_SetPropertyStr(ctx, lo_obj, "cidr", qjs.JS_NewString(ctx, "127.0.0.1/8"));
        _ = qjs.JS_SetPropertyUint32(ctx, lo_arr, 0, lo_obj);
        _ = qjs.JS_SetPropertyStr(ctx, result, "lo0", lo_arr);
        return result;
    }
    defer c.freeifaddrs(ifap);

    // Iterate through interfaces
    var ifa = ifap;
    while (ifa) |iface| : (ifa = iface.ifa_next) {
        const name = std.mem.span(iface.ifa_name);
        const addr = iface.ifa_addr orelse continue;

        // Only handle IPv4 for now (IPv6 has more complex struct)
        if (addr.sa_family != AF_INET) continue;

        // Get or create interface array
        var iface_arr = qjs.JS_GetPropertyStr(ctx, result, iface.ifa_name);
        var arr_len: u32 = 0;
        const is_new_arr = qjs.JS_IsUndefined(iface_arr);
        if (is_new_arr) {
            iface_arr = qjs.JS_NewArray(ctx);
        } else {
            // Get current array length for proper indexing
            const len_val = qjs.JS_GetPropertyStr(ctx, iface_arr, "length");
            _ = qjs.JS_ToUint32(ctx, &arr_len, len_val);
            qjs.JS_FreeValue(ctx, len_val);
        }

        // Create address object
        const addr_obj = qjs.JS_NewObject(ctx);

        // Extract IPv4 address
        const sin: *const sockaddr_in = @ptrCast(@alignCast(addr));
        var addr_buf: [16]u8 = undefined;
        const ip_bytes: [4]u8 = @bitCast(sin.sin_addr);
        const addr_len = std.fmt.bufPrint(&addr_buf, "{d}.{d}.{d}.{d}", .{
            ip_bytes[0],
            ip_bytes[1],
            ip_bytes[2],
            ip_bytes[3],
        }) catch continue;
        _ = qjs.JS_SetPropertyStr(ctx, addr_obj, "address", qjs.JS_NewStringLen(ctx, &addr_buf, addr_len.len));

        // Extract netmask if available
        if (iface.ifa_netmask) |netmask_sa| {
            const netmask_sin: *const sockaddr_in = @ptrCast(@alignCast(netmask_sa));
            var netmask_buf: [16]u8 = undefined;
            const netmask_bytes: [4]u8 = @bitCast(netmask_sin.sin_addr);
            const netmask_len = std.fmt.bufPrint(&netmask_buf, "{d}.{d}.{d}.{d}", .{
                netmask_bytes[0],
                netmask_bytes[1],
                netmask_bytes[2],
                netmask_bytes[3],
            }) catch continue;
            _ = qjs.JS_SetPropertyStr(ctx, addr_obj, "netmask", qjs.JS_NewStringLen(ctx, &netmask_buf, netmask_len.len));

            // Calculate CIDR prefix length
            var prefix_len: u8 = 0;
            for (netmask_bytes) |b| {
                prefix_len += @popCount(b);
            }
            var cidr_buf: [20]u8 = undefined;
            const cidr_len = std.fmt.bufPrint(&cidr_buf, "{s}/{d}", .{ addr_len, prefix_len }) catch continue;
            _ = qjs.JS_SetPropertyStr(ctx, addr_obj, "cidr", qjs.JS_NewStringLen(ctx, &cidr_buf, cidr_len.len));
        }

        _ = qjs.JS_SetPropertyStr(ctx, addr_obj, "family", qjs.JS_NewString(ctx, "IPv4"));
        _ = qjs.JS_SetPropertyStr(ctx, addr_obj, "mac", qjs.JS_NewString(ctx, "00:00:00:00:00:00"));

        // Determine if internal (loopback)
        const is_internal = std.mem.startsWith(u8, name, "lo") or ip_bytes[0] == 127;
        _ = qjs.JS_SetPropertyStr(ctx, addr_obj, "internal", if (is_internal) quickjs.jsTrue() else quickjs.jsFalse());

        // Add to interface array at the correct index
        _ = qjs.JS_SetPropertyUint32(ctx, iface_arr, arr_len, addr_obj);

        // Store array if newly created, otherwise free our reference
        if (is_new_arr) {
            _ = qjs.JS_SetPropertyStr(ctx, result, iface.ifa_name, iface_arr);
        } else {
            qjs.JS_FreeValue(ctx, iface_arr);
        }
    }

    return result;
}

/// os.machine() - Get CPU architecture/machine type
fn osMachine(ctx: ?*qjs.JSContext, _: qjs.JSValue, _: c_int, _: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (builtin.os.tag == .wasi) {
        return qjs.JS_NewString(ctx, "wasm32");
    }

    var uname_buf: std.c.utsname = undefined;
    if (std.c.uname(&uname_buf) == 0) {
        return qjs.JS_NewString(ctx, &uname_buf.machine);
    }

    // Fallback based on target architecture
    const machine: [*:0]const u8 = switch (builtin.cpu.arch) {
        .aarch64 => "arm64",
        .x86_64 => "x86_64",
        .x86 => "i686",
        .arm => "arm",
        .wasm32 => "wasm32",
        else => "unknown",
    };
    return qjs.JS_NewString(ctx, machine);
}

/// os.version() - Get OS kernel version string
fn osVersion(ctx: ?*qjs.JSContext, _: qjs.JSValue, _: c_int, _: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (builtin.os.tag == .wasi) {
        return qjs.JS_NewString(ctx, "WASI");
    }

    var uname_buf: std.c.utsname = undefined;
    if (std.c.uname(&uname_buf) == 0) {
        return qjs.JS_NewString(ctx, &uname_buf.version);
    }

    return qjs.JS_NewString(ctx, "unknown");
}

/// os.availableParallelism() - Get number of CPUs available for parallelism
fn osAvailableParallelism(ctx: ?*qjs.JSContext, _: qjs.JSValue, _: c_int, _: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    const count = std.Thread.getCpuCount() catch 1;
    return qjs.JS_NewInt32(ctx, @intCast(count));
}

/// os.getPriority(pid) - Get process scheduling priority
fn osGetPriority(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (builtin.os.tag == .wasi) {
        return qjs.JS_NewInt32(ctx, 0);
    }

    var pid: i32 = 0;
    if (argc > 0) {
        _ = qjs.JS_ToInt32(ctx, &pid, argv[0]);
    }

    // Reset errno before calling getpriority (it can legitimately return -1)
    const prio = c.getpriority(c.PRIO_PROCESS, @intCast(pid));
    return qjs.JS_NewInt32(ctx, prio);
}

/// os.setPriority(pid, priority) or os.setPriority(priority)
fn osSetPriority(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (builtin.os.tag == .wasi) {
        return qjs.JS_ThrowTypeError(ctx, "os.setPriority not supported on WASI");
    }

    if (argc < 1) {
        return qjs.JS_ThrowTypeError(ctx, "priority required");
    }

    var pid: i32 = 0;
    var prio: i32 = 0;

    if (argc == 1) {
        // setPriority(priority) - current process
        _ = qjs.JS_ToInt32(ctx, &prio, argv[0]);
    } else {
        // setPriority(pid, priority)
        _ = qjs.JS_ToInt32(ctx, &pid, argv[0]);
        _ = qjs.JS_ToInt32(ctx, &prio, argv[1]);
    }

    const result = c.setpriority(c.PRIO_PROCESS, @intCast(pid), prio);
    if (result < 0) {
        return qjs.JS_ThrowTypeError(ctx, "setPriority failed: permission denied");
    }

    return quickjs.jsUndefined();
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
    // Round 13: machine, version, availableParallelism, getPriority, setPriority
    _ = qjs.JS_SetPropertyStr(ctx, os_obj, "machine", qjs.JS_NewCFunction(ctx, osMachine, "machine", 0));
    _ = qjs.JS_SetPropertyStr(ctx, os_obj, "version", qjs.JS_NewCFunction(ctx, osVersion, "version", 0));
    _ = qjs.JS_SetPropertyStr(ctx, os_obj, "availableParallelism", qjs.JS_NewCFunction(ctx, osAvailableParallelism, "availableParallelism", 0));
    _ = qjs.JS_SetPropertyStr(ctx, os_obj, "getPriority", qjs.JS_NewCFunction(ctx, osGetPriority, "getPriority", 1));
    _ = qjs.JS_SetPropertyStr(ctx, os_obj, "setPriority", qjs.JS_NewCFunction(ctx, osSetPriority, "setPriority", 2));

    // Constants
    _ = qjs.JS_SetPropertyStr(ctx, os_obj, "EOL", qjs.JS_NewString(ctx, "\n"));
    _ = qjs.JS_SetPropertyStr(ctx, os_obj, "devNull", qjs.JS_NewString(ctx, "/dev/null"));

    // os.constants object - Round 14: populate with actual values
    const constants_obj = qjs.JS_NewObject(ctx);

    // signals - platform-specific signal numbers
    const signals = qjs.JS_NewObject(ctx);
    _ = qjs.JS_SetPropertyStr(ctx, signals, "SIGHUP", qjs.JS_NewInt32(ctx, 1));
    _ = qjs.JS_SetPropertyStr(ctx, signals, "SIGINT", qjs.JS_NewInt32(ctx, 2));
    _ = qjs.JS_SetPropertyStr(ctx, signals, "SIGQUIT", qjs.JS_NewInt32(ctx, 3));
    _ = qjs.JS_SetPropertyStr(ctx, signals, "SIGILL", qjs.JS_NewInt32(ctx, 4));
    _ = qjs.JS_SetPropertyStr(ctx, signals, "SIGTRAP", qjs.JS_NewInt32(ctx, 5));
    _ = qjs.JS_SetPropertyStr(ctx, signals, "SIGABRT", qjs.JS_NewInt32(ctx, 6));
    _ = qjs.JS_SetPropertyStr(ctx, signals, "SIGBUS", qjs.JS_NewInt32(ctx, if (builtin.os.tag == .macos) 10 else 7));
    _ = qjs.JS_SetPropertyStr(ctx, signals, "SIGFPE", qjs.JS_NewInt32(ctx, 8));
    _ = qjs.JS_SetPropertyStr(ctx, signals, "SIGKILL", qjs.JS_NewInt32(ctx, 9));
    _ = qjs.JS_SetPropertyStr(ctx, signals, "SIGUSR1", qjs.JS_NewInt32(ctx, if (builtin.os.tag == .macos) 30 else 10));
    _ = qjs.JS_SetPropertyStr(ctx, signals, "SIGSEGV", qjs.JS_NewInt32(ctx, 11));
    _ = qjs.JS_SetPropertyStr(ctx, signals, "SIGUSR2", qjs.JS_NewInt32(ctx, if (builtin.os.tag == .macos) 31 else 12));
    _ = qjs.JS_SetPropertyStr(ctx, signals, "SIGPIPE", qjs.JS_NewInt32(ctx, 13));
    _ = qjs.JS_SetPropertyStr(ctx, signals, "SIGALRM", qjs.JS_NewInt32(ctx, 14));
    _ = qjs.JS_SetPropertyStr(ctx, signals, "SIGTERM", qjs.JS_NewInt32(ctx, 15));
    _ = qjs.JS_SetPropertyStr(ctx, signals, "SIGCHLD", qjs.JS_NewInt32(ctx, if (builtin.os.tag == .macos) 20 else 17));
    _ = qjs.JS_SetPropertyStr(ctx, signals, "SIGCONT", qjs.JS_NewInt32(ctx, if (builtin.os.tag == .macos) 19 else 18));
    _ = qjs.JS_SetPropertyStr(ctx, signals, "SIGSTOP", qjs.JS_NewInt32(ctx, if (builtin.os.tag == .macos) 17 else 19));
    _ = qjs.JS_SetPropertyStr(ctx, signals, "SIGTSTP", qjs.JS_NewInt32(ctx, if (builtin.os.tag == .macos) 18 else 20));
    _ = qjs.JS_SetPropertyStr(ctx, signals, "SIGTTIN", qjs.JS_NewInt32(ctx, 21));
    _ = qjs.JS_SetPropertyStr(ctx, signals, "SIGTTOU", qjs.JS_NewInt32(ctx, 22));
    _ = qjs.JS_SetPropertyStr(ctx, signals, "SIGURG", qjs.JS_NewInt32(ctx, if (builtin.os.tag == .macos) 16 else 23));
    _ = qjs.JS_SetPropertyStr(ctx, signals, "SIGXCPU", qjs.JS_NewInt32(ctx, 24));
    _ = qjs.JS_SetPropertyStr(ctx, signals, "SIGXFSZ", qjs.JS_NewInt32(ctx, 25));
    _ = qjs.JS_SetPropertyStr(ctx, signals, "SIGVTALRM", qjs.JS_NewInt32(ctx, 26));
    _ = qjs.JS_SetPropertyStr(ctx, signals, "SIGPROF", qjs.JS_NewInt32(ctx, 27));
    _ = qjs.JS_SetPropertyStr(ctx, signals, "SIGWINCH", qjs.JS_NewInt32(ctx, 28));
    _ = qjs.JS_SetPropertyStr(ctx, signals, "SIGIO", qjs.JS_NewInt32(ctx, if (builtin.os.tag == .macos) 23 else 29));
    _ = qjs.JS_SetPropertyStr(ctx, signals, "SIGSYS", qjs.JS_NewInt32(ctx, if (builtin.os.tag == .macos) 12 else 31));
    _ = qjs.JS_SetPropertyStr(ctx, constants_obj, "signals", signals);

    // errno - common error codes
    const errnos = qjs.JS_NewObject(ctx);
    _ = qjs.JS_SetPropertyStr(ctx, errnos, "EPERM", qjs.JS_NewInt32(ctx, 1));
    _ = qjs.JS_SetPropertyStr(ctx, errnos, "ENOENT", qjs.JS_NewInt32(ctx, 2));
    _ = qjs.JS_SetPropertyStr(ctx, errnos, "ESRCH", qjs.JS_NewInt32(ctx, 3));
    _ = qjs.JS_SetPropertyStr(ctx, errnos, "EINTR", qjs.JS_NewInt32(ctx, 4));
    _ = qjs.JS_SetPropertyStr(ctx, errnos, "EIO", qjs.JS_NewInt32(ctx, 5));
    _ = qjs.JS_SetPropertyStr(ctx, errnos, "ENXIO", qjs.JS_NewInt32(ctx, 6));
    _ = qjs.JS_SetPropertyStr(ctx, errnos, "E2BIG", qjs.JS_NewInt32(ctx, 7));
    _ = qjs.JS_SetPropertyStr(ctx, errnos, "ENOEXEC", qjs.JS_NewInt32(ctx, 8));
    _ = qjs.JS_SetPropertyStr(ctx, errnos, "EBADF", qjs.JS_NewInt32(ctx, 9));
    _ = qjs.JS_SetPropertyStr(ctx, errnos, "ECHILD", qjs.JS_NewInt32(ctx, 10));
    _ = qjs.JS_SetPropertyStr(ctx, errnos, "EAGAIN", qjs.JS_NewInt32(ctx, if (builtin.os.tag == .macos) 35 else 11));
    _ = qjs.JS_SetPropertyStr(ctx, errnos, "ENOMEM", qjs.JS_NewInt32(ctx, 12));
    _ = qjs.JS_SetPropertyStr(ctx, errnos, "EACCES", qjs.JS_NewInt32(ctx, 13));
    _ = qjs.JS_SetPropertyStr(ctx, errnos, "EFAULT", qjs.JS_NewInt32(ctx, 14));
    _ = qjs.JS_SetPropertyStr(ctx, errnos, "EBUSY", qjs.JS_NewInt32(ctx, 16));
    _ = qjs.JS_SetPropertyStr(ctx, errnos, "EEXIST", qjs.JS_NewInt32(ctx, 17));
    _ = qjs.JS_SetPropertyStr(ctx, errnos, "EXDEV", qjs.JS_NewInt32(ctx, 18));
    _ = qjs.JS_SetPropertyStr(ctx, errnos, "ENODEV", qjs.JS_NewInt32(ctx, 19));
    _ = qjs.JS_SetPropertyStr(ctx, errnos, "ENOTDIR", qjs.JS_NewInt32(ctx, 20));
    _ = qjs.JS_SetPropertyStr(ctx, errnos, "EISDIR", qjs.JS_NewInt32(ctx, 21));
    _ = qjs.JS_SetPropertyStr(ctx, errnos, "EINVAL", qjs.JS_NewInt32(ctx, 22));
    _ = qjs.JS_SetPropertyStr(ctx, errnos, "ENFILE", qjs.JS_NewInt32(ctx, 23));
    _ = qjs.JS_SetPropertyStr(ctx, errnos, "EMFILE", qjs.JS_NewInt32(ctx, 24));
    _ = qjs.JS_SetPropertyStr(ctx, errnos, "ENOTTY", qjs.JS_NewInt32(ctx, 25));
    _ = qjs.JS_SetPropertyStr(ctx, errnos, "EFBIG", qjs.JS_NewInt32(ctx, 27));
    _ = qjs.JS_SetPropertyStr(ctx, errnos, "ENOSPC", qjs.JS_NewInt32(ctx, 28));
    _ = qjs.JS_SetPropertyStr(ctx, errnos, "ESPIPE", qjs.JS_NewInt32(ctx, 29));
    _ = qjs.JS_SetPropertyStr(ctx, errnos, "EROFS", qjs.JS_NewInt32(ctx, 30));
    _ = qjs.JS_SetPropertyStr(ctx, errnos, "EMLINK", qjs.JS_NewInt32(ctx, 31));
    _ = qjs.JS_SetPropertyStr(ctx, errnos, "EPIPE", qjs.JS_NewInt32(ctx, 32));
    _ = qjs.JS_SetPropertyStr(ctx, errnos, "EDOM", qjs.JS_NewInt32(ctx, 33));
    _ = qjs.JS_SetPropertyStr(ctx, errnos, "ERANGE", qjs.JS_NewInt32(ctx, 34));
    _ = qjs.JS_SetPropertyStr(ctx, errnos, "ENOTEMPTY", qjs.JS_NewInt32(ctx, if (builtin.os.tag == .macos) 66 else 39));
    _ = qjs.JS_SetPropertyStr(ctx, errnos, "ELOOP", qjs.JS_NewInt32(ctx, if (builtin.os.tag == .macos) 62 else 40));
    _ = qjs.JS_SetPropertyStr(ctx, errnos, "ENAMETOOLONG", qjs.JS_NewInt32(ctx, if (builtin.os.tag == .macos) 63 else 36));
    _ = qjs.JS_SetPropertyStr(ctx, errnos, "ENOSYS", qjs.JS_NewInt32(ctx, if (builtin.os.tag == .macos) 78 else 38));
    _ = qjs.JS_SetPropertyStr(ctx, errnos, "ENODATA", qjs.JS_NewInt32(ctx, if (builtin.os.tag == .macos) 96 else 61));
    _ = qjs.JS_SetPropertyStr(ctx, errnos, "ETIMEDOUT", qjs.JS_NewInt32(ctx, if (builtin.os.tag == .macos) 60 else 110));
    _ = qjs.JS_SetPropertyStr(ctx, errnos, "ECONNREFUSED", qjs.JS_NewInt32(ctx, if (builtin.os.tag == .macos) 61 else 111));
    _ = qjs.JS_SetPropertyStr(ctx, errnos, "ECONNRESET", qjs.JS_NewInt32(ctx, if (builtin.os.tag == .macos) 54 else 104));
    _ = qjs.JS_SetPropertyStr(ctx, errnos, "EADDRINUSE", qjs.JS_NewInt32(ctx, if (builtin.os.tag == .macos) 48 else 98));
    _ = qjs.JS_SetPropertyStr(ctx, errnos, "EADDRNOTAVAIL", qjs.JS_NewInt32(ctx, if (builtin.os.tag == .macos) 49 else 99));
    _ = qjs.JS_SetPropertyStr(ctx, errnos, "ENETUNREACH", qjs.JS_NewInt32(ctx, if (builtin.os.tag == .macos) 51 else 101));
    _ = qjs.JS_SetPropertyStr(ctx, errnos, "ENOTCONN", qjs.JS_NewInt32(ctx, if (builtin.os.tag == .macos) 57 else 107));
    _ = qjs.JS_SetPropertyStr(ctx, constants_obj, "errno", errnos);

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
