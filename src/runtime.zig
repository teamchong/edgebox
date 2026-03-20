/// EdgeBox Build Tool
/// Compiles JS to AOT WASM using WAMR with SIMD support
///
/// Usage:
///   edgebox build [app-directory]  - Build app (bundle + bytecode + AOT)
const std = @import("std");
const builtin = @import("builtin");
const qjsc_wrapper = @import("qjsc_wrapper.zig");
const freeze = @import("freeze/frozen_registry.zig");
const wasm_opt = @import("wasm_opt.zig");

const VERSION = "0.1.0";

// ============================================================================
// EdgeBox Configuration
// ============================================================================

/// Output mode for command emulation
pub const OutputMode = enum {
    server, // Standard stdout/stderr (default)
    browser, // HTML output
    viewkit, // ViewKit UI engine
};

/// Command permission: binary name -> allowed subcommands (null = all allowed)
pub const CommandPermission = struct {
    binary: []const u8,
    subcommands: ?[]const []const u8 = null, // null = allow all args, empty = binary only
    deny_subcommands: ?[]const []const u8 = null, // Deny list (takes precedence over allow)

    // Credential proxy: env vars to inject when spawning this command
    credentials: ?std.StringHashMapUnmanaged([]const u8) = null,

    // Output mode for Component Model emulators (optional)
    output_mode: OutputMode = .server,
    emulator_component: ?[]const u8 = null, // WASM Component path
};

/// EdgeBox app configuration parsed from .edgebox.json
pub const EdgeBoxConfig = struct {
    name: ?[]const u8 = null,
    npm: ?[]const u8 = null,
    dirs: []const []const u8 = &.{},
    env: []const []const u8 = &.{},
    commands: []const CommandPermission = &.{},

    // HTTP security settings
    allowed_urls: []const []const u8 = &.{}, // URL patterns allowed for fetch (glob: https://api.anthropic.com/*)
    blocked_urls: []const []const u8 = &.{}, // URL patterns blocked (takes precedence over allowed)
    rate_limit_rps: u32 = 0, // Max requests per second (0 = unlimited)
    max_connections: u32 = 10, // Max concurrent HTTP connections

    // Command security settings
    sensitive_files: []const []const u8 = &.{}, // Glob patterns for sensitive files (.env, ~/.ssh/*, etc.)
    blocked_patterns: []const []const u8 = &.{}, // Patterns for destructive commands (git reset, > .env, etc.)

    /// Parse .edgebox.json from a directory
    pub fn load(allocator: std.mem.Allocator, dir_path: []const u8) ?EdgeBoxConfig {
        var path_buf: [4096]u8 = undefined;
        const config_path = std.fmt.bufPrint(&path_buf, "{s}/.edgebox.json", .{dir_path}) catch return null;

        const file = std.fs.cwd().openFile(config_path, .{}) catch return null;
        defer file.close();

        const content = file.readToEndAlloc(allocator, 1024 * 1024) catch return null;
        defer allocator.free(content);

        return parseJson(allocator, content);
    }

    /// Parse .edgebox.json from current directory
    pub fn loadFromCwd(allocator: std.mem.Allocator) ?EdgeBoxConfig {
        const file = std.fs.cwd().openFile(".edgebox.json", .{}) catch return null;
        defer file.close();

        const content = file.readToEndAlloc(allocator, 1024 * 1024) catch return null;
        defer allocator.free(content);

        return parseJson(allocator, content);
    }

    fn parseJson(allocator: std.mem.Allocator, content: []const u8) ?EdgeBoxConfig {
        const parsed = std.json.parseFromSlice(std.json.Value, allocator, content, .{}) catch return null;
        defer parsed.deinit();

        var config = EdgeBoxConfig{};

        if (parsed.value.object.get("name")) |v| {
            if (v == .string) config.name = allocator.dupe(u8, v.string) catch null;
        }

        if (parsed.value.object.get("npm")) |v| {
            if (v == .string) config.npm = allocator.dupe(u8, v.string) catch null;
        }

        // Parse dirs array
        if (parsed.value.object.get("dirs")) |v| {
            if (v == .array) {
                var dirs: std.ArrayListUnmanaged([]const u8) = .{};
                for (v.array.items) |item| {
                    if (item == .string) {
                        const dup = allocator.dupe(u8, item.string) catch |err| {
                            std.debug.print("[warn] Failed to parse dirs entry: {}\n", .{err});
                            continue;
                        };
                        dirs.append(allocator, dup) catch |err| {
                            std.debug.print("[warn] Failed to append dirs entry: {}\n", .{err});
                            continue;
                        };
                    }
                }
                config.dirs = dirs.toOwnedSlice(allocator) catch &.{};
            }
        }

        // Parse env array
        if (parsed.value.object.get("env")) |v| {
            if (v == .array) {
                var envs: std.ArrayListUnmanaged([]const u8) = .{};
                for (v.array.items) |item| {
                    if (item == .string) {
                        const dup = allocator.dupe(u8, item.string) catch |err| {
                            std.debug.print("[warn] Failed to parse env entry: {}\n", .{err});
                            continue;
                        };
                        envs.append(allocator, dup) catch |err| {
                            std.debug.print("[warn] Failed to append env entry: {}\n", .{err});
                            continue;
                        };
                    }
                }
                config.env = envs.toOwnedSlice(allocator) catch &.{};
            }
        }

        // Parse allowedUrls array (HTTP security)
        if (parsed.value.object.get("allowedUrls")) |v| {
            if (v == .array) {
                var urls: std.ArrayListUnmanaged([]const u8) = .{};
                for (v.array.items) |item| {
                    if (item == .string) {
                        const dup = allocator.dupe(u8, item.string) catch |err| {
                            std.debug.print("[warn] Failed to parse allowedUrls entry: {}\n", .{err});
                            continue;
                        };
                        urls.append(allocator, dup) catch |err| {
                            std.debug.print("[warn] Failed to append allowedUrls entry: {}\n", .{err});
                            continue;
                        };
                    }
                }
                config.allowed_urls = urls.toOwnedSlice(allocator) catch &.{};
            }
        }

        // Parse blockedUrls array (HTTP security)
        if (parsed.value.object.get("blockedUrls")) |v| {
            if (v == .array) {
                var urls: std.ArrayListUnmanaged([]const u8) = .{};
                for (v.array.items) |item| {
                    if (item == .string) {
                        const dup = allocator.dupe(u8, item.string) catch |err| {
                            std.debug.print("[warn] Failed to parse blockedUrls entry: {}\n", .{err});
                            continue;
                        };
                        urls.append(allocator, dup) catch |err| {
                            std.debug.print("[warn] Failed to append blockedUrls entry: {}\n", .{err});
                            continue;
                        };
                    }
                }
                config.blocked_urls = urls.toOwnedSlice(allocator) catch &.{};
            }
        }

        // Parse rateLimitRps (HTTP security) - with bounds: 0 = unlimited, max 100k
        if (parsed.value.object.get("rateLimitRps")) |v| {
            if (v == .integer) {
                const val = @max(0, @min(100_000, v.integer));
                config.rate_limit_rps = @intCast(val);
                if (v.integer > 100_000) {
                    std.debug.print("[warn] rateLimitRps clamped to 100000 (was {})\n", .{v.integer});
                }
            }
        }

        // Parse maxConnections (HTTP security) - with bounds: 1-10000
        if (parsed.value.object.get("maxConnections")) |v| {
            if (v == .integer) {
                const val = @max(1, @min(10_000, v.integer));
                config.max_connections = @intCast(val);
                if (v.integer > 10_000) {
                    std.debug.print("[warn] maxConnections clamped to 10000 (was {})\n", .{v.integer});
                }
            }
        }

        // Parse commands object
        if (parsed.value.object.get("commands")) |v| {
            if (v == .object) {
                var cmds: std.ArrayListUnmanaged(CommandPermission) = .{};
                var iter = v.object.iterator();
                while (iter.next()) |entry| {
                    const binary = allocator.dupe(u8, entry.key_ptr.*) catch continue;
                    var perm = CommandPermission{ .binary = binary };

                    // Value can be: true (allow all), array (simple), or object (extended format)
                    if (entry.value_ptr.* == .bool and entry.value_ptr.bool) {
                        // true = allow all arguments
                        perm.subcommands = null;
                    } else if (entry.value_ptr.* == .array) {
                        // Array = list of allowed subcommands
                        var subs: std.ArrayListUnmanaged([]const u8) = .{};
                        for (entry.value_ptr.array.items) |item| {
                            if (item == .string) {
                                // "*" means allow all
                                if (std.mem.eql(u8, item.string, "*")) {
                                    subs.deinit(allocator);
                                    perm.subcommands = null;
                                    break;
                                }
                                subs.append(allocator, allocator.dupe(u8, item.string) catch continue) catch continue;
                            }
                        } else {
                            perm.subcommands = subs.toOwnedSlice(allocator) catch null;
                        }
                    } else if (entry.value_ptr.* == .object) {
                        // Object = extended format with allow, deny, credentials, etc.
                        const cmd_obj = entry.value_ptr.object;

                        // Parse "allow" field
                        if (cmd_obj.get("allow")) |allow_val| {
                            if (allow_val == .bool and allow_val.bool) {
                                perm.subcommands = null;
                            } else if (allow_val == .array) {
                                var subs: std.ArrayListUnmanaged([]const u8) = .{};
                                for (allow_val.array.items) |item| {
                                    if (item == .string) {
                                        if (std.mem.eql(u8, item.string, "*")) {
                                            subs.deinit(allocator);
                                            perm.subcommands = null;
                                            break;
                                        }
                                        subs.append(allocator, allocator.dupe(u8, item.string) catch continue) catch continue;
                                    }
                                }
                                if (perm.subcommands) |_| {} else {
                                    perm.subcommands = subs.toOwnedSlice(allocator) catch null;
                                }
                            }
                        }

                        // Parse "deny" field
                        if (cmd_obj.get("deny")) |deny_val| {
                            if (deny_val == .array) {
                                var deny_subs: std.ArrayListUnmanaged([]const u8) = .{};
                                for (deny_val.array.items) |item| {
                                    if (item == .string) {
                                        deny_subs.append(allocator, allocator.dupe(u8, item.string) catch continue) catch continue;
                                    }
                                }
                                perm.deny_subcommands = deny_subs.toOwnedSlice(allocator) catch null;
                            }
                        }

                        // Parse "credentials" field
                        if (cmd_obj.get("credentials")) |cred_val| {
                            if (cred_val == .object) {
                                var creds = std.StringHashMapUnmanaged([]const u8){};
                                var cred_iter = cred_val.object.iterator();
                                while (cred_iter.next()) |cred_entry| {
                                    const key = allocator.dupe(u8, cred_entry.key_ptr.*) catch continue;
                                    const value = if (cred_entry.value_ptr.* == .string)
                                        allocator.dupe(u8, cred_entry.value_ptr.string) catch continue
                                    else
                                        continue;
                                    // Expand ${VAR} references from host environment
                                    const expanded = expandEnvVar(allocator, value) catch value;
                                    if (expanded.ptr != value.ptr) {
                                        allocator.free(value);
                                    }
                                    creds.put(allocator, key, expanded) catch continue;
                                }
                                perm.credentials = creds;
                            }
                        }

                        // Parse "outputMode" field
                        if (cmd_obj.get("outputMode")) |mode_val| {
                            if (mode_val == .string) {
                                if (std.mem.eql(u8, mode_val.string, "browser")) {
                                    perm.output_mode = .browser;
                                } else if (std.mem.eql(u8, mode_val.string, "viewkit")) {
                                    perm.output_mode = .viewkit;
                                }
                            }
                        }

                        // Parse "emulator" field
                        if (cmd_obj.get("emulator")) |emul_val| {
                            if (emul_val == .string) {
                                perm.emulator_component = allocator.dupe(u8, emul_val.string) catch null;
                            }
                        }
                    }

                    cmds.append(allocator, perm) catch continue;
                }
                config.commands = cmds.toOwnedSlice(allocator) catch &.{};
            }
        }

        // Parse sensitiveFiles array (command security)
        if (parsed.value.object.get("sensitiveFiles")) |v| {
            if (v == .array) {
                var files: std.ArrayListUnmanaged([]const u8) = .{};
                for (v.array.items) |item| {
                    if (item == .string) {
                        files.append(allocator, allocator.dupe(u8, item.string) catch continue) catch continue;
                    }
                }
                config.sensitive_files = files.toOwnedSlice(allocator) catch &.{};
            }
        }

        // Parse blockedPatterns array (command security)
        if (parsed.value.object.get("blockedPatterns")) |v| {
            if (v == .array) {
                var patterns: std.ArrayListUnmanaged([]const u8) = .{};
                for (v.array.items) |item| {
                    if (item == .string) {
                        patterns.append(allocator, allocator.dupe(u8, item.string) catch continue) catch continue;
                    }
                }
                config.blocked_patterns = patterns.toOwnedSlice(allocator) catch &.{};
            }
        }

        return config;
    }

    /// Get list of allowed binary names
    pub fn getAllowedBinaries(self: EdgeBoxConfig, allocator: std.mem.Allocator) []const []const u8 {
        var binaries: std.ArrayListUnmanaged([]const u8) = .{};
        for (self.commands) |cmd| {
            binaries.append(allocator, cmd.binary) catch continue;
        }
        return binaries.toOwnedSlice(allocator) catch &.{};
    }

    /// Check if a command with given args is allowed
    pub fn isCommandAllowed(self: EdgeBoxConfig, binary: []const u8, args: []const []const u8) bool {
        for (self.commands) |cmd| {
            if (std.mem.eql(u8, cmd.binary, binary)) {
                // Found the binary
                if (cmd.subcommands == null) {
                    // null = all args allowed
                    return true;
                }
                // Check if first arg is in allowed list
                if (args.len == 0) {
                    // No args, check if empty subcommands means binary-only allowed
                    return cmd.subcommands.?.len == 0 or true;
                }
                for (cmd.subcommands.?) |allowed| {
                    if (std.mem.eql(u8, args[0], allowed)) {
                        return true;
                    }
                }
                return false; // First arg not in allowed list
            }
        }
        return false; // Binary not in allowed list
    }

    /// Serialize commands config to JSON for passing to WASM via env var
    pub fn serializeCommands(self: EdgeBoxConfig, allocator: std.mem.Allocator) ?[]const u8 {
        if (self.commands.len == 0) return null;

        var buf: std.ArrayListUnmanaged(u8) = .{};
        const writer = buf.writer(allocator);

        writer.writeAll("{") catch return null;
        var first = true;
        for (self.commands) |cmd| {
            if (!first) writer.writeAll(",") catch return null;
            first = false;

            writer.print("\"{s}\":", .{cmd.binary}) catch return null;
            if (cmd.subcommands) |subs| {
                writer.writeAll("[") catch return null;
                for (subs, 0..) |sub, i| {
                    if (i > 0) writer.writeAll(",") catch return null;
                    writer.print("\"{s}\"", .{sub}) catch return null;
                }
                writer.writeAll("]") catch return null;
            } else {
                writer.writeAll("true") catch return null;
            }
        }
        writer.writeAll("}") catch return null;

        return buf.toOwnedSlice(allocator) catch null;
    }

    /// Serialize dirs config to JSON array for passing to edgebox-sandbox via env var
    pub fn serializeDirs(self: EdgeBoxConfig, allocator: std.mem.Allocator) ?[]const u8 {
        if (self.dirs.len == 0) return null;

        var buf: std.ArrayListUnmanaged(u8) = .{};
        const writer = buf.writer(allocator);

        writer.writeAll("[") catch return null;
        for (self.dirs, 0..) |dir, i| {
            if (i > 0) writer.writeAll(",") catch return null;
            writer.print("\"{s}\"", .{dir}) catch return null;
        }
        writer.writeAll("]") catch return null;

        return buf.toOwnedSlice(allocator) catch null;
    }
};

/// Check if a byte is a valid JS identifier character (a-z, A-Z, 0-9, _, $)
fn isIdentChar(ch: u8) bool {
    return (ch >= 'a' and ch <= 'z') or (ch >= 'A' and ch <= 'Z') or
        (ch >= '0' and ch <= '9') or ch == '_' or ch == '$';
}

/// WASM function metadata parsed from standalone_manifest.json.
const WasmFunc = struct {
    name: []const u8,
    arg_count: u32,
    instr_count: u32,
    is_recursive: bool,
    array_args: u8,
    mutated_args: u8,
    read_array_args: u8,
    length_args: u8,
    has_loop: bool,
    has_bitwise: bool,
    line_num: u32,
    is_anon: bool,
    is_f64: bool,
    // Struct arg support: which args are JS objects to be flattened into WASM memory
    struct_args: u8 = 0,
    // Array-of-struct arg support: arg is an array whose elements are struct objects
    // (accessed via arr[i].field pattern — get_array_el → get_field)
    array_of_struct_args: u8 = 0,
    // Field names per struct/AOS arg (up to 8 args × 16 fields)
    struct_field_names: [8][16][]const u8 = [_][16][]const u8{[_][]const u8{""} ** 16} ** 8,
    struct_field_counts: [8]u8 = [_]u8{0} ** 8,
};

/// SOA allocation site: a factory function that creates objects with a fixed shape.
/// Detected from bytecodes (object → define_field pattern).
/// Used to rewrite object creation to SOA layout in WASM linear memory.
const AllocSite = struct {
    name: []const u8,
    line_num: u32,
    field_names: [16][]const u8,
    field_count: u8,
    /// Which get_arg index maps to each field (for correct param mapping)
    arg_indices: [16]u8 = .{0} ** 16,
    /// True for constructor pattern (this.X = Y), false for factory (return {X: Y})
    is_constructor: bool = false,
};

/// Decide whether a WASM-compiled function should get a trampoline or stay as JS.
/// Returns true if the function should be trampolined to WASM.
fn shouldTrampolineToWasm(mf: WasmFunc, calls_wasm: bool) bool {
    // f64+bitwise recursive: integer code forced into f64 tier by division.
    // WASM pays fptosi/sitofp roundtrip per |0, fmod polyfill per %.
    // V8 JIT specializes to native i32 ops — always faster.
    if (mf.is_recursive and mf.is_f64 and mf.has_bitwise) return false;

    // Recursive: entire call stack stays in WASM, no JS↔WASM boundary per recursion
    if (mf.is_recursive) return true;

    // Cross-callers: calls stay in WASM
    if (calls_wasm) return true;

    const is_write_only_array = mf.mutated_args != 0 and mf.read_array_args == 0;

    // Compute-heavy write-only: ALWAYS trampoline.
    // Copy-in is skipped (write-only), only copy-back overhead remains.
    // mandelbrot_compute: 157 instrs × 40K iters, Math.abs/sqrt.
    if (is_write_only_array and mf.has_loop and mf.instr_count >= 50) return true;

    // Write-only light: V8 handles write loops perfectly.
    // zero(buf): 162ms WASM vs 43ms V8 (3.8x slower due to copies).
    if (is_write_only_array and (!mf.has_loop or mf.instr_count < 50)) return false;

    // Fixed-size array ops (no .length): copy cost dominates.
    // No-loop: wrapper/delegation (crypto_verify_16, dot16).
    // Large-body loops (>=200 instrs): sha256_compress 408ms WASM vs 305ms V8.
    if (mf.array_args != 0 and mf.length_args == 0 and
        (!mf.has_loop or mf.instr_count >= 200)) return false;

    // Mutated fixed-size array loops: copy-back per element per call dominates.
    // A(o,a,b) in tweetnacl: 16-elem adds, 1313ms WASM vs 184ms V8.
    if (mf.mutated_args != 0 and mf.length_args == 0 and
        mf.has_loop and mf.instr_count < 200) return false;

    // Offset-indexed multi-array: >=2 arrays + >=2 scalars, no .length, small body.
    // vn(x,xi,y,yi,n): offset access, kept as JS.
    const array_arg_count = @popCount(mf.array_args);
    const scalar_arg_count = if (mf.arg_count > array_arg_count) mf.arg_count - array_arg_count else 0;
    if (mf.mutated_args == 0 and mf.array_args != 0 and
        mf.length_args == 0 and mf.has_loop and mf.instr_count < 200 and
        array_arg_count >= 2 and scalar_arg_count >= 2) return false;

    // Array-of-struct arg: function iterates array[i].field in WASM.
    // Trampoline materializes entire array into flat pool — always worth it
    // because the function loops over the pool internally in WASM.
    if (mf.array_of_struct_args != 0) return true;

    // Struct arg functions: object fields are copied to flat WASM memory.
    // Copy cost is O(field_count) per call — cheap but adds up for small functions
    // called millions of times from JS loops.
    // Only trampoline when the function has an internal loop (work scales with data).
    // Switch-case patterns (canHaveJSDoc, canHaveSymbol) have high instr_count
    // but no loops — V8's jump table is faster than WASM + memory copy.
    if (mf.struct_args != 0) return mf.has_loop;

    // Small scalar functions (<150 instrs): V8 JIT already optimal
    if (mf.array_args == 0 and mf.instr_count < 150) return false;

    // Non-looping, non-recursive scalar functions: these are switch-case lookup patterns
    // (e.g., canHaveJSDoc, isDeclarationKind, getOperatorPrecedence).
    // V8 compiles switch(kind) into O(1) jump tables — WASM trampoline overhead
    // (5 calls + memory write per invocation) makes these slower, not faster.
    if (mf.array_args == 0 and !mf.has_loop and !mf.is_recursive) return false;

    return true;
}

/// Skip past a JS function body starting at `{`, handling strings, comments, and nesting.
/// Returns the position after the closing `}`.
fn skipJsFunctionBody(cc: []const u8, brace_pos: usize) usize {
    var depth: i32 = 1;
    var scan = brace_pos + 1;
    while (scan < cc.len and depth > 0) {
        const ch = cc[scan];
        if (ch == '\'' or ch == '"' or ch == '`') {
            scan += 1;
            while (scan < cc.len) {
                if (cc[scan] == '\\') {
                    scan += 2;
                    continue;
                }
                if (cc[scan] == ch) {
                    scan += 1;
                    break;
                }
                scan += 1;
            }
            continue;
        }
        if (ch == '/' and scan + 1 < cc.len) {
            if (cc[scan + 1] == '/') {
                while (scan < cc.len and cc[scan] != '\n') scan += 1;
                continue;
            }
            if (cc[scan + 1] == '*') {
                scan += 2;
                while (scan + 1 < cc.len) {
                    if (cc[scan] == '*' and cc[scan + 1] == '/') {
                        scan += 2;
                        break;
                    }
                    scan += 1;
                }
                continue;
            }
            // Regex literal: / preceded by operator/keyword, not identifier/number
            if (scan > 0) {
                var rp = scan - 1;
                while (rp > 0 and (cc[rp] == ' ' or cc[rp] == '\t')) rp -= 1;
                const prev = cc[rp];
                const is_regex = prev == '=' or prev == '(' or prev == ',' or
                    prev == '[' or prev == '!' or prev == '&' or prev == '|' or
                    prev == ':' or prev == ';' or prev == '{' or prev == '}' or
                    prev == '?' or prev == '+' or prev == '-' or prev == '~' or
                    prev == '^' or prev == '<' or prev == '>' or prev == '\n' or
                    prev == '%' or prev == '*';
                if (is_regex) {
                    scan += 1;
                    while (scan < cc.len and cc[scan] != '/') {
                        if (cc[scan] == '\\') scan += 1; // skip escaped chars
                        if (cc[scan] == '[') { // character class — skip to ]
                            scan += 1;
                            while (scan < cc.len and cc[scan] != ']') {
                                if (cc[scan] == '\\') scan += 1;
                                scan += 1;
                            }
                        }
                        scan += 1;
                    }
                    if (scan < cc.len) scan += 1; // skip closing /
                    // Skip regex flags
                    while (scan < cc.len and (cc[scan] >= 'a' and cc[scan] <= 'z')) scan += 1;
                    continue;
                }
            }
        }
        if (ch == '{') depth += 1;
        if (ch == '}') depth -= 1;
        scan += 1;
    }
    return scan;
}

/// Result of detecting a push-loop pattern (array population).
const PushLoopMatch = struct {
    loop_end: usize,
    arr_name: []const u8, // array being populated
    body: []const u8, // push body content for field matching

    /// Check if a field name appears as a key in the push object literal.
    fn has_field(self: PushLoopMatch, name: []const u8) bool {
        // Search for "fieldname:" or "fieldname :" in the body
        var pos: usize = 0;
        while (pos + name.len < self.body.len) {
            if (std.mem.startsWith(u8, self.body[pos..], name)) {
                const after = pos + name.len;
                if (after < self.body.len) {
                    var ap = after;
                    while (ap < self.body.len and self.body[ap] == ' ') ap += 1;
                    if (ap < self.body.len and self.body[ap] == ':') return true;
                }
            }
            pos += 1;
        }
        return false;
    }
};

/// Detect for-loop patterns that populate an array via push({...}).
/// Pattern: for (...) { ARR.push({ field1: EXPR, field2: EXPR }); }
fn detectPushLoop(cc: []const u8, for_pos: usize, wasm_funcs: []const WasmFunc) ?PushLoopMatch {
    _ = wasm_funcs;
    // Skip to the loop body brace
    var p = for_pos + 3;
    while (p < cc.len and cc[p] == ' ') p += 1;
    if (p >= cc.len or cc[p] != '(') return null;
    // Skip the for(...) header
    var depth: u32 = 1;
    p += 1;
    while (p < cc.len and depth > 0) : (p += 1) {
        if (cc[p] == '(') depth += 1;
        if (cc[p] == ')') depth -= 1;
    }
    // Find loop body brace
    while (p < cc.len and (cc[p] == ' ' or cc[p] == '\n' or cc[p] == '\r')) p += 1;
    if (p >= cc.len or cc[p] != '{') return null;
    const body_start = p + 1;
    const loop_end = skipJsFunctionBody(cc, p);
    const body = cc[body_start..@min(loop_end - 1, cc.len)];

    // Find ARR.push({ in the body
    const push_marker = ".push(";
    const push_pos = std.mem.indexOf(u8, body, push_marker) orelse return null;
    // Extract array name (identifier before .push)
    var arr_end = push_pos;
    while (arr_end > 0 and (body[arr_end - 1] == ' ' or body[arr_end - 1] == '\n')) arr_end -= 1;
    var arr_start = arr_end;
    while (arr_start > 0 and isIdentChar(body[arr_start - 1])) arr_start -= 1;
    const arr_name = body[arr_start..arr_end];
    if (arr_name.len == 0) return null;

    // Verify it's pushing an object literal
    const after_push = push_pos + push_marker.len;
    var ap = after_push;
    while (ap < body.len and (body[ap] == ' ' or body[ap] == '\n')) ap += 1;
    if (ap >= body.len or body[ap] != '{') return null;

    return PushLoopMatch{
        .loop_end = loop_end,
        .arr_name = arr_name,
        .body = body,
    };
}

/// Result of detecting a batch-eligible for-loop.
const BatchLoopMatch = struct {
    loop_end: usize, // position after the closing `}`
    func_name: []const u8, // name of the struct function to batch
    func_idx: usize, // wasm function index (for unique variable names)
    arr_name: []const u8, // array variable name
    acc_name: []const u8, // accumulator variable name
    scalar_args: []const u8, // scalar args after the array element (e.g. ", 3, 0x20")
};

/// Detect for-loop patterns calling struct functions with array element args.
/// Pattern: for (V = 0; V < ARR.length; V++) { ACC = ACC + FN(ARR[V], ...) | 0; }
/// Also matches: for (V = 0; V < ARR.length; V++) { ACC = (ACC + FN(ARR[V], ...)) | 0; }
fn detectBatchLoop(cc: []const u8, for_pos: usize, wasm_funcs: []const WasmFunc) ?BatchLoopMatch {
    // Find the opening paren of for(
    var p = for_pos + 3; // skip "for"
    while (p < cc.len and cc[p] == ' ') p += 1;
    if (p >= cc.len or cc[p] != '(') return null;
    p += 1; // skip '('

    // Parse init: V = 0 or V = EXPR
    while (p < cc.len and cc[p] == ' ') p += 1;
    const var_start = p;
    while (p < cc.len and isIdentChar(cc[p])) p += 1;
    const loop_var = cc[var_start..p];
    if (loop_var.len == 0) return null;
    // Skip to semicolon (past " = 0" or similar)
    while (p < cc.len and cc[p] != ';') p += 1;
    if (p >= cc.len) return null;
    p += 1; // skip ';'

    // Parse condition: V < ARR.length
    while (p < cc.len and cc[p] == ' ') p += 1;
    // Verify loop var matches
    if (!std.mem.startsWith(u8, cc[p..], loop_var)) return null;
    p += loop_var.len;
    while (p < cc.len and cc[p] == ' ') p += 1;
    if (p >= cc.len or cc[p] != '<') return null;
    p += 1;
    while (p < cc.len and cc[p] == ' ') p += 1;
    // Parse ARR.length
    const arr_start = p;
    while (p < cc.len and isIdentChar(cc[p])) p += 1;
    const arr_name = cc[arr_start..p];
    if (arr_name.len == 0) return null;
    if (!std.mem.startsWith(u8, cc[p..], ".length")) return null;
    p += 7; // skip ".length"
    // Skip to semicolon
    while (p < cc.len and cc[p] != ';') p += 1;
    if (p >= cc.len) return null;
    p += 1; // skip ';'

    // Skip increment (V++ or V = V + 1 | 0)
    while (p < cc.len and cc[p] != ')') p += 1;
    if (p >= cc.len) return null;
    p += 1; // skip ')'

    // Find the loop body opening brace
    while (p < cc.len and (cc[p] == ' ' or cc[p] == '\n' or cc[p] == '\r')) p += 1;
    if (p >= cc.len or cc[p] != '{') return null;
    const body_start = p + 1;
    const loop_end = skipJsFunctionBody(cc, p);
    const body = cc[body_start..@min(loop_end - 1, cc.len)];

    // Search body for: ACC = (ACC + FN(ARR[V], ...) | 0;
    // or: ACC = ACC + FN(ARR[V], ...) | 0;
    for (wasm_funcs, 0..) |wfe, wfi| {
        if (wfe.struct_args == 0) continue;
        // Find FN(ARR[V] in body
        const fn_call = std.mem.indexOf(u8, body, wfe.name) orelse continue;
        const call_pos = fn_call + wfe.name.len;
        if (call_pos >= body.len or body[call_pos] != '(') continue;
        // Check ARR[V] follows
        const after_paren = call_pos + 1;
        if (!std.mem.startsWith(u8, body[after_paren..], arr_name)) continue;
        const after_arr = after_paren + arr_name.len;
        if (after_arr >= body.len or body[after_arr] != '[') continue;
        if (!std.mem.startsWith(u8, body[after_arr + 1 ..], loop_var)) continue;

        // Find the accumulator: look for ACC = (ACC + before the function call
        // Scan backwards from fn_call to find pattern: ACC = ACC + or ACC = (ACC +
        var acc_end: usize = 0;
        if (fn_call > 4) {
            // Skip backwards past " + " or " + ("
            var rp = fn_call;
            while (rp > 0 and (body[rp - 1] == ' ' or body[rp - 1] == '+' or body[rp - 1] == '(')) rp -= 1;
            acc_end = rp;
            while (rp > 0 and isIdentChar(body[rp - 1])) rp -= 1;
            // Check this is ACC = ACC + pattern
            const maybe_acc2 = body[rp..acc_end];
            if (rp >= 4) {
                // Scan left past ' =' to find the ACC name on the LHS of assignment
                var rp2 = rp;
                while (rp2 > 0 and (body[rp2 - 1] == ' ' or body[rp2 - 1] == '=' or body[rp2 - 1] == '(')) rp2 -= 1;
                // rp2 now points past the ACC ident chars — scan to find start
                const acc1_end = rp2;
                while (rp2 > 0 and isIdentChar(body[rp2 - 1])) rp2 -= 1;
                const acc_name = body[rp2..acc1_end];
                if (acc_name.len > 0 and std.mem.eql(u8, acc_name, maybe_acc2)) {
                    // Find scalar args: everything between ARR[V] and the closing )
                    const arr_bracket_end = after_arr + 1 + loop_var.len + 1; // skip "]"
                    const fn_close = std.mem.indexOfPos(u8, body, arr_bracket_end, ")") orelse continue;
                    const scalar_args_str = if (fn_close > arr_bracket_end + 1)
                        std.mem.trim(u8, body[arr_bracket_end..fn_close], " ")
                    else
                        "";
                    // Strip leading comma
                    const scalar_args = if (scalar_args_str.len > 0 and scalar_args_str[0] == ',')
                        std.mem.trimLeft(u8, scalar_args_str[1..], " ")
                    else
                        scalar_args_str;

                    return BatchLoopMatch{
                        .loop_end = loop_end,
                        .func_name = wfe.name,
                        .func_idx = wfi,
                        .arr_name = arr_name,
                        .acc_name = acc_name,
                        .scalar_args = scalar_args,
                    };
                }
            }
        }
    }
    return null;
}

/// Check if a function body calls any other WASM function.
fn bodyCallsWasmFunc(cc: []const u8, match_end: usize, self_name: []const u8, wasm_funcs: []const WasmFunc) bool {
    const bp = std.mem.indexOfPos(u8, cc, match_end, "{") orelse return false;
    const end = skipJsFunctionBody(cc, bp);
    const body = cc[bp + 1 .. @min(end, cc.len)];
    for (wasm_funcs) |other| {
        if (std.mem.eql(u8, other.name, self_name)) continue;
        if (std.mem.indexOf(u8, body, other.name) != null) return true;
    }
    return false;
}

/// Extract integer from a JSON object field, defaulting to 0.
fn jsonInt(obj: std.json.ObjectMap, key: []const u8) u32 {
    return jsonIntOr(obj, key, 0);
}

/// Extract integer from a JSON object field with a custom default.
fn jsonIntOr(obj: std.json.ObjectMap, key: []const u8, default: u32) u32 {
    const v = obj.get(key) orelse return default;
    return switch (v) {
        .integer => |i| @intCast(@max(0, i)),
        else => default,
    };
}

/// Extract string from a JSON object field.
fn jsonStr(obj: std.json.ObjectMap, key: []const u8) ?[]const u8 {
    const v = obj.get(key) orelse return null;
    return switch (v) {
        .string => |s| s,
        else => null,
    };
}

/// Expand ${VAR} references in a string with host environment variables
/// Example: "${GH_TOKEN}" -> actual value from host env
fn expandEnvVar(allocator: std.mem.Allocator, value: []const u8) ![]const u8 {
    // Check if value contains ${...} pattern
    if (std.mem.indexOf(u8, value, "${")) |start_idx| {
        if (std.mem.indexOfPos(u8, value, start_idx, "}")) |end_idx| {
            const var_name = value[start_idx + 2 .. end_idx];

            // Get environment variable value from host
            const env_value = std.process.getEnvVarOwned(allocator, var_name) catch {
                // If env var not found, return original value
                return try allocator.dupe(u8, value);
            };
            defer allocator.free(env_value);

            // Build result: prefix + env_value + suffix
            var result: std.ArrayListUnmanaged(u8) = .{};
            defer result.deinit(allocator);

            try result.appendSlice(allocator, value[0..start_idx]);
            try result.appendSlice(allocator, env_value);
            try result.appendSlice(allocator, value[end_idx + 1 ..]);

            return try result.toOwnedSlice(allocator);
        }
    }

    // No ${VAR} pattern found, return as-is
    return try allocator.dupe(u8, value);
}

// ============================================================================
// Tests
// ============================================================================

test "expandEnvVar - no variable" {
    const allocator = std.testing.allocator;
    const result = try expandEnvVar(allocator, "plain text");
    defer allocator.free(result);
    try std.testing.expectEqualStrings("plain text", result);
}

test "expandEnvVar - with HOME variable" {
    const allocator = std.testing.allocator;
    // This test uses HOME which should be set on most systems
    const result = try expandEnvVar(allocator, "${HOME}/test");
    defer allocator.free(result);
    // Just verify it doesn't contain ${HOME} anymore
    try std.testing.expect(std.mem.indexOf(u8, result, "${HOME}") == null);
    try std.testing.expect(std.mem.endsWith(u8, result, "/test"));
}

test "expandEnvVar - nonexistent variable returns original" {
    const allocator = std.testing.allocator;
    const result = try expandEnvVar(allocator, "${NONEXISTENT_VAR_12345}/test");
    defer allocator.free(result);
    try std.testing.expectEqualStrings("${NONEXISTENT_VAR_12345}/test", result);
}

test "expandEnvVar - unclosed brace returns original" {
    const allocator = std.testing.allocator;
    const result = try expandEnvVar(allocator, "${UNCLOSED");
    defer allocator.free(result);
    try std.testing.expectEqualStrings("${UNCLOSED", result);
}

pub fn main() !void {
    const allocator = std.heap.page_allocator;

    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    if (args.len < 2) {
        printUsage();
        std.process.exit(1);
    }

    // Parse size values with K/M suffix (e.g., "1500K", "2M", "3145728")
    const parseSizeValue = struct {
        fn parse(s: []const u8) !u32 {
            if (s.len == 0) return error.InvalidValue;
            const last = s[s.len - 1];
            if (last == 'K' or last == 'k') {
                const n = try std.fmt.parseInt(u32, s[0 .. s.len - 1], 10);
                return n * 1024;
            } else if (last == 'M' or last == 'm') {
                const n = try std.fmt.parseInt(u32, s[0 .. s.len - 1], 10);
                return n * 1024 * 1024;
            }
            return std.fmt.parseInt(u32, s, 10);
        }
    }.parse;

    // Parse arguments
    var force_rebuild = false;
    var no_polyfill = false;
    var no_freeze = false;
    var no_bundle = false;
    var binary_only = false;
    var wasm_only = true; // Default: worker path (V8+WASM). Use --with-binary for native.
    var debug_build = false;
    var allocator_type: AllocatorType = .gpa; // Default: GPA for debugging (use --allocator=c for production)
    var allocator_explicitly_set = false;
    var output_prefix: ?[]const u8 = null; // Custom output prefix (default: zig-out)
    var freeze_max_functions: u32 = 0; // 0 = freeze all, N = freeze only top N largest
    var freeze_profile_path: ?[]const u8 = null; // PGO: path to call profile JSON
    var freeze_code_budget: u32 = 0; // 0 = auto-detect L2, N = max frozen code bytes
    var app_dir: ?[]const u8 = null;

    var i: usize = 1;
    while (i < args.len) : (i += 1) {
        const arg = args[i];
        if (std.mem.eql(u8, arg, "--help") or std.mem.eql(u8, arg, "-h")) {
            printUsage();
            return;
        } else if (std.mem.eql(u8, arg, "--version") or std.mem.eql(u8, arg, "-v")) {
            std.debug.print("edgebox {s}\n", .{VERSION});
            return;
        } else if (std.mem.eql(u8, arg, "--force") or std.mem.eql(u8, arg, "-f")) {
            force_rebuild = true;
        } else if (std.mem.eql(u8, arg, "--no-polyfill")) {
            no_polyfill = true;
        } else if (std.mem.eql(u8, arg, "--no-freeze")) {
            no_freeze = true;
        } else if (std.mem.eql(u8, arg, "--no-bundle")) {
            no_bundle = true;
        } else if (std.mem.eql(u8, arg, "--binary-only")) {
            binary_only = true;
        } else if (std.mem.eql(u8, arg, "--wasm-only")) {
            wasm_only = true;
        } else if (std.mem.eql(u8, arg, "--with-binary")) {
            wasm_only = false; // Opt-in: also build native binary
        } else if (std.mem.eql(u8, arg, "--debug")) {
            debug_build = true;
        } else if (std.mem.startsWith(u8, arg, "--allocator=")) {
            const value = arg[12..];
            allocator_explicitly_set = true;
            if (std.mem.eql(u8, value, "arena")) {
                allocator_type = .arena;
            } else if (std.mem.eql(u8, value, "gpa")) {
                allocator_type = .gpa;
            } else if (std.mem.eql(u8, value, "c")) {
                allocator_type = .c;
            } else {
                std.debug.print("Unknown allocator: {s} (use: arena, c, gpa)\n", .{value});
                std.process.exit(1);
            }
        } else if (std.mem.startsWith(u8, arg, "--output-dir=")) {
            output_prefix = arg[13..];
        } else if (std.mem.startsWith(u8, arg, "--freeze-max-functions=")) {
            const prefix_len = "--freeze-max-functions=".len;
            freeze_max_functions = std.fmt.parseInt(u32, arg[prefix_len..], 10) catch {
                std.debug.print("Invalid --freeze-max-functions value: {s}\n", .{arg[prefix_len..]});
                std.process.exit(1);
            };
        } else if (std.mem.startsWith(u8, arg, "--freeze-code-budget=")) {
            const val_str = arg["--freeze-code-budget=".len..];
            // Parse size with K/M suffix (e.g., "1500K", "2M", "3145728")
            freeze_code_budget = parseSizeValue(val_str) catch {
                std.debug.print("Invalid --freeze-code-budget value: {s} (use e.g., 1500K, 2M, or bytes)\n", .{val_str});
                std.process.exit(1);
            };
        } else if (std.mem.startsWith(u8, arg, "--freeze-profile=")) {
            freeze_profile_path = arg["--freeze-profile=".len..];
        } else if (std.mem.eql(u8, arg, "--minimal")) {
            // Shortcut for test262: skip polyfills, freeze, bundler, and WASM/AOT
            no_polyfill = true;
            no_freeze = true;
            no_bundle = true;
            binary_only = true;
        } else if (std.mem.eql(u8, arg, "build")) {
            continue; // implicit, ignore for backwards compatibility
        } else if (std.mem.endsWith(u8, arg, ".wasm") or std.mem.endsWith(u8, arg, ".aot") or std.mem.endsWith(u8, arg, ".dylib") or std.mem.endsWith(u8, arg, ".so")) {
            std.debug.print("Use 'edgebox {s}' to run WASM/AOT files\n", .{arg});
            std.process.exit(1);
        } else if (!std.mem.startsWith(u8, arg, "-")) {
            app_dir = arg;
        } else {
            std.debug.print("Unknown option: {s}\n", .{arg});
            printUsage();
            std.process.exit(1);
        }
    }

    const dir = app_dir orelse {
        std.debug.print("Error: No app directory specified\n\n", .{});
        printUsage();
        std.process.exit(1);
    };

    // Use libc malloc by default for frozen builds.
    // Arena can't free individual allocations → unbounded memory growth (1.3GB for TSC)
    // which thrashes caches and causes 7.5x slowdown vs libc malloc.
    if (comptime builtin.os.tag == .linux) {
        if (!no_freeze and !allocator_explicitly_set) {
            allocator_type = .c;
        }
    }

    if (force_rebuild) {
        cleanBuildOutputs();
    }

    try runStaticBuild(allocator, dir, .{
        .no_polyfill = no_polyfill,
        .no_freeze = no_freeze,
        .no_bundle = no_bundle,
        .binary_only = binary_only,
        .wasm_only = wasm_only,
        .debug_build = debug_build,
        .allocator_type = allocator_type,
        .output_prefix = output_prefix,
        .freeze_max_functions = freeze_max_functions,
        .freeze_profile_path = freeze_profile_path,
        .freeze_code_budget = freeze_code_budget,
    });
}

fn printUsage() void {
    std.debug.print(
        \\EdgeBox — AOT optimizer for V8 JavaScript
        \\
        \\Usage:
        \\  edgebox <app.js>              Compile (V8 + WASM AOT kernels)
        \\  edgebox pack <worker-dir>     Pack workerd single binary
        \\
        \\Output (in zig-out/bin/<app.js>/):
        \\  <app>-worker.js        Worker module (V8 JIT + WASM AOT)
        \\  <app>-standalone.wasm   WASM numeric kernels
        \\  <app>-config.capnp      workerd configuration
        \\
        \\Options:
        \\  -f, --force      Clean previous build outputs first
        \\  --output-dir=X   Custom output directory (default: zig-out)
        \\  -h, --help       Show this help
        \\  -v, --version    Show version
        \\
        \\Examples:
        \\  edgebox app.js                                  Compile
        \\  edgebox pack zig-out/bin/app.js/                Pack workerd binary
        \\  npx workerd serve zig-out/bin/app.js/app-config.capnp
        \\
    , .{});
}

/// Clean all build outputs
fn cleanBuildOutputs() void {
    const files_to_clean = [_][]const u8{
        "zig-out/bundle.js",
        "zig-out/bundle.bin",
        "zig-out/frozen_manifest.json",
    };
    for (files_to_clean) |file| {
        std.fs.cwd().deleteFile(file) catch {};
    }
}

/// Allocator types for native binaries
pub const AllocatorType = enum { gpa, arena, c };

/// Build options for customizing the compilation pipeline
const BuildOptions = struct {
    no_polyfill: bool = false, // Skip Node.js polyfills
    no_freeze: bool = false, // Skip freeze analysis
    no_bundle: bool = false, // Skip Bun bundler
    binary_only: bool = false, // Only build native binary (skip WASM/AOT)
    wasm_only: bool = false, // Only build WASM + AOT (skip native binary)
    debug_build: bool = false, // Use Debug optimization (faster compile, slower runtime)
    allocator_type: AllocatorType = .gpa, // Allocator for native binary (gpa=debug, c=fastest, arena=batch)
    output_prefix: ?[]const u8 = null, // Custom output prefix (default: zig-out)
    freeze_max_functions: u32 = 0, // 0 = freeze all, N = freeze only top N largest functions
    freeze_profile_path: ?[]const u8 = null, // PGO: path to call profile JSON from --profile-out
    freeze_code_budget: u32 = 0, // 0 = auto (L2 cache size), N = max frozen code bytes
};

/// Static build: compile JS to C bytecode with qjsc, embed in WASM
/// All frozen functions stay in WASM/AOT (sandboxed) - no host function exports
fn runStaticBuild(allocator: std.mem.Allocator, app_dir: []const u8, options: BuildOptions) !void {
    // Check if input is a single JS file or a directory
    const is_js_file = std.mem.endsWith(u8, app_dir, ".js");

    // Derive output base name from input
    var output_base_buf: [256]u8 = undefined;
    const output_base = blk: {
        // Get filename from path
        const path_to_use = if (is_js_file) app_dir else app_dir;
        const last_slash = std.mem.lastIndexOf(u8, path_to_use, "/");
        const filename = if (last_slash) |idx| path_to_use[idx + 1 ..] else path_to_use;

        // Remove .js extension if present
        const base = if (std.mem.endsWith(u8, filename, ".js"))
            filename[0 .. filename.len - 3]
        else
            filename;

        // Copy to buffer
        const len = @min(base.len, output_base_buf.len);
        @memcpy(output_base_buf[0..len], base[0..len]);
        break :blk output_base_buf[0..len];
    };

    // Calculate source directory, cache directory, and output directory
    // e.g., bench/hello.js ->
    //   source_dir = "bench"
    //   cache_dir  = "zig-out/cache/bench"  (intermediate files: bundle.js, bundle.bin)
    //   output_dir = "zig-out/bin/bench"    (final outputs: hello.wasm, hello.aot)
    var source_dir_buf: [4096]u8 = undefined;
    var cache_dir_buf: [4096]u8 = undefined;
    var output_dir_buf: [4096]u8 = undefined;
    const source_dir: []const u8 = blk: {
        // For per-project cache isolation, use app_dir as source_dir
        // e.g., "bench" -> source_dir = "bench" -> cache_dir = "zig-out/cache/bench"
        // e.g., "examples/app" -> source_dir = "examples/app" -> cache_dir = "zig-out/cache/examples/app"
        var dir = app_dir;
        // Strip trailing slash if present
        if (dir.len > 0 and dir[dir.len - 1] == '/') {
            dir = dir[0 .. dir.len - 1];
        }
        // Strip leading slash for absolute paths (build.zig requires relative paths)
        if (dir.len > 0 and dir[0] == '/') {
            dir = dir[1..];
        }
        break :blk dir;
    };
    const out_prefix = options.output_prefix orelse "zig-out";
    const cache_dir = blk: {
        if (source_dir.len > 0) {
            const len = std.fmt.bufPrint(&cache_dir_buf, "{s}/cache/{s}", .{ out_prefix, source_dir }) catch {
                std.debug.print("[error] Source directory path too long: {s}\n", .{source_dir});
                std.process.exit(1);
            };
            break :blk cache_dir_buf[0..len.len];
        }
        const len = std.fmt.bufPrint(&cache_dir_buf, "{s}/cache", .{out_prefix}) catch {
            break :blk "zig-out/cache";
        };
        break :blk cache_dir_buf[0..len.len];
    };
    const output_dir = blk: {
        if (source_dir.len > 0) {
            const len = std.fmt.bufPrint(&output_dir_buf, "{s}/bin/{s}", .{ out_prefix, source_dir }) catch {
                std.debug.print("[error] Source directory path too long: {s}\n", .{source_dir});
                std.process.exit(1);
            };
            break :blk output_dir_buf[0..len.len];
        }
        const len = std.fmt.bufPrint(&output_dir_buf, "{s}/bin", .{out_prefix}) catch {
            break :blk "zig-out/bin";
        };
        break :blk output_dir_buf[0..len.len];
    };
    // Build -Dsource-dir argument for zig build (tells build.zig where to find bundle.bin)
    const source_dir_arg = if (source_dir.len > 0)
        std.fmt.bufPrint(&source_dir_buf, "-Dsource-dir={s}", .{source_dir}) catch ""
    else
        "";

    // Create directories
    std.fs.cwd().makePath(cache_dir) catch |err| std.debug.print("[build] makePath({s}): {}\n", .{ cache_dir, err });
    std.fs.cwd().makePath(output_dir) catch |err| std.debug.print("[build] makePath({s}): {}\n", .{ output_dir, err });

    // Pre-calculate cache file paths (intermediate files)
    var bundle_js_path_buf: [4096]u8 = undefined;
    const bundle_js_path = std.fmt.bufPrint(&bundle_js_path_buf, "{s}/bundle.js", .{cache_dir}) catch "zig-out/cache/bundle.js";

    // bundle.bin path for unified @embedFile flow (used by all targets)

    var bundle_original_path_buf: [4096]u8 = undefined;
    const bundle_original_path = std.fmt.bufPrint(&bundle_original_path_buf, "{s}/bundle_original.c", .{cache_dir}) catch "zig-out/cache/bundle_original.c";

    // Bun --outfile argument
    var bun_outfile_buf: [4096]u8 = undefined;
    const bun_outfile_arg = std.fmt.bufPrint(&bun_outfile_buf, "--outfile={s}/bundle.js", .{cache_dir}) catch "--outfile=zig-out/cache/bundle.js";

    var entry_path_buf: [4096]u8 = undefined;
    var entry_path: []const u8 = undefined;

    if (is_js_file) {
        // Single JS file - use it directly as entry point
        std.debug.print("[build] Entry point: {s}\n", .{app_dir});
        const file = std.fs.cwd().openFile(app_dir, .{}) catch {
            std.debug.print("[error] File not found: {s}\n", .{app_dir});
            std.process.exit(1);
        };
        file.close();
        entry_path = app_dir;
    } else {
        // Directory - find entry point
        std.debug.print("[build] App directory: {s}\n", .{app_dir});

        var dir = std.fs.cwd().openDir(app_dir, .{}) catch {
            std.debug.print("[error] App directory not found: {s}\n", .{app_dir});
            std.process.exit(1);
        };
        dir.close();

        entry_path = findEntryPoint(app_dir, &entry_path_buf) catch {
            std.debug.print("[error] No entry point found in {s}\n", .{app_dir});
            std.process.exit(1);
        };
        std.debug.print("[build] Entry point: {s}\n", .{entry_path});
    }

    // Step 3: Bundle with Bun (or skip if --no-bundle flag)
    if (options.no_bundle) {
        // Skip bundler - just copy the source file directly
        std.debug.print("[build] Skipping bundler (--no-bundle)\n", .{});
        std.fs.cwd().makePath(cache_dir) catch |err| std.debug.print("[build] makePath: {}\n", .{err});
        const src_file = std.fs.cwd().openFile(entry_path, .{}) catch {
            std.debug.print("[error] Cannot open entry point: {s}\n", .{entry_path});
            std.process.exit(1);
        };
        defer src_file.close();
        const content = src_file.readToEndAlloc(allocator, 50 * 1024 * 1024) catch {
            std.debug.print("[error] Cannot read entry point\n", .{});
            std.process.exit(1);
        };
        defer allocator.free(content);
        const out_file = std.fs.cwd().createFile(bundle_js_path, .{}) catch {
            std.debug.print("[error] Cannot create bundle.js\n", .{});
            std.process.exit(1);
        };
        defer out_file.close();
        out_file.writeAll(content) catch |err| std.debug.print("[build] writeAll(bundle.js): {}\n", .{err});
    } else if (std.fs.cwd().statFile(bundle_js_path)) |_| {
        // Bundle already exists — skip rebundling
        std.debug.print("[build] Bundle cached, skipping Bun\n", .{});
    } else |_| {
        // Detect pre-bundled files by checking size (>1MB typically means pre-bundled)
        // But ESM bundles need conversion to CommonJS for QuickJS compatibility
        const entry_file = std.fs.cwd().openFile(entry_path, .{}) catch {
            std.debug.print("[error] Cannot open entry point: {s}\n", .{entry_path});
            std.process.exit(1);
        };
        defer entry_file.close();
        // Always use Bun for bundling - simpler and more reliable
        // Bun handles ESM/CJS detection and conversion automatically
        std.debug.print("[build] Bundling with Bun...\n", .{});
        // NOTE: We don't use --minify because it strips function names
        // which prevents frozen function matching by name
        std.fs.cwd().makePath(output_dir) catch |err| std.debug.print("[build] makePath: {}\n", .{err});
        const bun_result = try runCommand(allocator, &.{
            "bun",           "build",        entry_path,        bun_outfile_arg,
            "--target=node", "--format=cjs", "--external=fs",   "--external=path",
            "--external=os", "--external=crypto", "--external=util", "--external=stream",
            "--external=events", "--external=buffer", "--external=string_decoder",
            "--external=module", "--external=tty", "--external=net", "--external=http",
            "--external=https", "--external=url", "--external=querystring", "--external=zlib",
            "--external=child_process", "--external=worker_threads", "--external=perf_hooks",
            "--external=assert", "--external=constants", "--external=vm", "--external=readline",
        });
        defer {
            if (bun_result.stdout) |s| allocator.free(s);
            if (bun_result.stderr) |s| allocator.free(s);
        }

        if (bun_result.term.Exited != 0) {
            std.debug.print("[error] Bun bundling failed\n", .{});
            if (bun_result.stderr) |s| std.debug.print("{s}\n", .{s});
            std.process.exit(1);
        }
    }

    // Save clean bundle for workerd worker generation (before polyfills/hooks)
    var clean_bundle_path_buf: [4096]u8 = undefined;
    const clean_bundle_path = std.fmt.bufPrint(&clean_bundle_path_buf, "{s}/bundle_clean.js", .{cache_dir}) catch null;
    if (clean_bundle_path) |cbp| {
        std.fs.cwd().copyFile(bundle_js_path, std.fs.cwd(), cbp, .{}) catch |err| std.debug.print("[build] copyFile: {}\n", .{err});
    }

    // Step 4: Prepend polyfills (skip if --no-polyfill flag)
    if (!options.no_polyfill) {
        // All polyfill modules (in dependency order)
        const all_polyfill_modules = [_][]const u8{
            "src/polyfills/modules/util.js",
            "src/polyfills/modules/child_process.js",
            "src/polyfills/modules/readline.js",
            "src/polyfills/modules/dns.js",
            "src/polyfills/modules/timers.js",
            "src/polyfills/modules/perf_hooks.js",
            "src/polyfills/modules/cluster.js",
            "src/polyfills/modules/zlib.js",
            "src/polyfills/modules/dgram.js",
            "src/polyfills/modules/tls.js",
            "src/polyfills/modules/net.js",
            "src/polyfills/modules/https.js",
            "src/polyfills/modules/http2.js",
            "src/polyfills/modules/http.js",
            "src/polyfills/modules/url.js",
            "src/polyfills/modules/crypto.js",
            "src/polyfills/modules/fs.js",
            "src/polyfills/modules/stream.js",
            "src/polyfills/modules/events.js",
            "src/polyfills/modules/encoding.js",
            "src/polyfills/modules/buffer.js",
            "src/polyfills/modules/path.js",
            "src/polyfills/modules/core.js",
        };

        // Core modules always included (required for require() to work)
        const core_modules = [_][]const u8{
            "src/polyfills/modules/core.js",
            "src/polyfills/modules/path.js",
            "src/polyfills/modules/buffer.js",
            "src/polyfills/modules/events.js",
        };

        // Tree shaking: analyze bundle for required modules and globals
        std.debug.print("[build] Analyzing bundle for tree shaking...\n", .{});
        const bundle_content = std.fs.cwd().readFileAlloc(allocator, bundle_js_path, 10 * 1024 * 1024) catch |err| {
            std.debug.print("[error] Could not read bundle for tree shaking: {}\n", .{err});
            std.process.exit(1);
        };
        defer allocator.free(bundle_content);

        const tree_shake = @import("polyfills/tree_shake.zig");

        // Analyze require() calls
        const required = tree_shake.analyzeRequires(allocator, bundle_content) catch |err| {
            std.debug.print("[error] Tree shaking analysis failed: {}\n", .{err});
            std.process.exit(1);
        };
        defer {
            for (required) |r| allocator.free(r);
            allocator.free(required);
        }
        std.debug.print("[build] Found {} required modules: ", .{required.len});
        for (required) |r| std.debug.print("{s} ", .{r});
        std.debug.print("\n", .{});

        // Analyze global API usage for runtime module tree shaking
        const required_runtime = tree_shake.analyzeGlobals(allocator, bundle_content) catch |err| {
            std.debug.print("[error] Global analysis failed: {}\n", .{err});
            std.process.exit(1);
        };
        defer allocator.free(required_runtime);
        std.debug.print("[build] Found {} runtime globals: ", .{required_runtime.len});
        for (required_runtime) |r| {
            const basename = std.fs.path.basename(r);
            std.debug.print("{s} ", .{basename});
        }
        std.debug.print("\n", .{});

        // Check if this is a TypeScript bundle (tsc.js runtime module is included)
        var is_typescript_bundle = false;
        for (required_runtime) |r| {
            const basename = std.fs.path.basename(r);
            if (std.mem.eql(u8, basename, "tsc.js")) {
                is_typescript_bundle = true;
                break;
            }
        }

        // Build list of runtime modules to include (sorted in dependency order)
        var runtime_list = try std.ArrayList([]const u8).initCapacity(allocator, 16);
        defer runtime_list.deinit(allocator);

        // Always include core.js (console, timers, error handling)
        try runtime_list.append(allocator, "src/polyfills/runtime/core.js");

        // Add required runtime modules (but not core.js or end.js)
        for (required_runtime) |mod| {
            if (!std.mem.eql(u8, mod, "src/polyfills/runtime/core.js") and
                !std.mem.eql(u8, mod, "src/polyfills/runtime/end.js"))
            {
                var found = false;
                for (runtime_list.items) |existing| {
                    if (std.mem.eql(u8, existing, mod)) {
                        found = true;
                        break;
                    }
                }
                if (!found) {
                    try runtime_list.append(allocator, mod);
                }
            }
        }

        // Always include end.js last (closes the guard block)
        try runtime_list.append(allocator, "src/polyfills/runtime/end.js");

        // Sort runtime modules in dependency order
        const sorted_runtime = tree_shake.sortRuntimeModules(allocator, runtime_list.items) catch runtime_list.items;

        std.debug.print("[build] Runtime tree shaking: {}/15 modules included\n", .{sorted_runtime.len});

        // Check if code uses require() at all - if not, skip polyfill modules entirely
        const uses_require = tree_shake.usesRequire(bundle_content);
        std.debug.print("[build] Uses require(): {}\n", .{uses_require});

        // Build list of polyfill modules to include (only if require() is used)
        var include_list = try std.ArrayList([]const u8).initCapacity(allocator, 32);
        defer include_list.deinit(allocator);

        if (uses_require) {
            // Include core modules (needed for require() to work)
            for (core_modules) |mod| {
                try include_list.append(allocator, mod);
            }

            // Module-to-file mapping for modules defined inside other files
            const module_file_map = [_]struct { module: []const u8, file: []const u8 }{
                .{ .module = "async_hooks", .file = "src/polyfills/modules/zlib.js" },
                .{ .module = "timers/promises", .file = "src/polyfills/modules/zlib.js" },
            };

            // Add any mapped modules first
            for (required) |req| {
                for (module_file_map) |mapping| {
                    if (std.mem.eql(u8, req, mapping.module)) {
                        var already_added = false;
                        for (include_list.items) |item| {
                            if (std.mem.eql(u8, item, mapping.file)) {
                                already_added = true;
                                break;
                            }
                        }
                        if (!already_added) {
                            try include_list.append(allocator, mapping.file);
                        }
                    }
                }
            }

            // Add required modules (with dependency resolution)
            for (all_polyfill_modules) |mod_path| {
                const basename = std.fs.path.basename(mod_path);
                const mod_name = basename[0 .. basename.len - 3]; // strip .js

                var is_core = false;
                for (core_modules) |core| {
                    if (std.mem.eql(u8, mod_path, core)) {
                        is_core = true;
                        break;
                    }
                }
                if (is_core) continue;

                for (required) |req| {
                    const should_add = std.mem.eql(u8, mod_name, req) or
                        (std.mem.startsWith(u8, req, mod_name) and req.len > mod_name.len and req[mod_name.len] == '/');
                    if (should_add) {
                        var already_in_list = false;
                        for (include_list.items) |item| {
                            if (std.mem.eql(u8, item, mod_path)) {
                                already_in_list = true;
                                break;
                            }
                        }
                        if (!already_in_list) {
                            try include_list.append(allocator, mod_path);
                        }
                        break;
                    }
                }
            }

            // Add end.js to close the IIFE
            try include_list.append(allocator, "src/polyfills/modules/end.js");
        }

        const modules_to_include = try include_list.toOwnedSlice(allocator);
        if (uses_require) {
            std.debug.print("[build] Polyfill tree shaking: {}/{} modules included\n", .{ modules_to_include.len - 1, all_polyfill_modules.len });
        } else {
            std.debug.print("[build] Polyfill tree shaking: 0/{} modules (no require() calls)\n", .{all_polyfill_modules.len});
        }

        // Prepend polyfill modules in REVERSE order (last prepended = first in file)
        if (modules_to_include.len > 0) {
            std.debug.print("[build] Prepending {} polyfill modules...\n", .{modules_to_include.len});
            var i: usize = modules_to_include.len;
            while (i > 0) {
                i -= 1;
                const mod_path = modules_to_include[i];
                if (std.fs.cwd().access(mod_path, .{})) |_| {
                    try prependPolyfills(allocator, mod_path, bundle_js_path);
                } else |_| {}
            }
        }

        // Prepend runtime modules in REVERSE order (core.js ends up at TOP)
        // Use prependRuntimeModule (no IIFE wrapping) instead of prependPolyfills
        std.debug.print("[build] Prepending {} runtime modules...\n", .{sorted_runtime.len});
        var j: usize = sorted_runtime.len;
        while (j > 0) {
            j -= 1;
            const mod_path = sorted_runtime[j];
            if (std.fs.cwd().access(mod_path, .{})) |_| {
                try prependRuntimeModule(allocator, mod_path, bundle_js_path);
            } else |_| {}
        }

        // Step 5b: Patch TypeScript bundles to enable Native Shapes factory interception
        // TypeScript bundles have createSourceFile as a local variable, not on globalThis.ts
        // We inject code right before executeCommandLine() to export the necessary functions
        // and call the factory interception hook
        if (is_typescript_bundle) {
            std.debug.print("[build] Patching TypeScript bundle for Native Shapes...\n", .{});
            const bak_path_ts = try std.fmt.allocPrint(allocator, "{s}.bak", .{bundle_js_path});
            defer allocator.free(bak_path_ts);
            _ = try runCommand(allocator, &.{
                "sed", "-i.bak",
                // Export ts namespace to globalThis, call factory interception, hook sys for I/O, then execute
                // The interception wraps globalThis.ts.createSourceFile, so we copy it back to the global variable
                // __edgebox_hook_tsc_sys modifies sys.write/readFile/etc. for proper stdout and file I/O
                "s/executeCommandLine(sys, noop, sys.args);/globalThis.ts = { createSourceFile: createSourceFile, factory: factory, forEachChild: forEachChild }; if (typeof __edgebox_intercept_tsc_factory === \"function\") { __edgebox_intercept_tsc_factory(); createSourceFile = globalThis.ts.createSourceFile; } if (typeof __edgebox_hook_tsc_sys === \"function\") { __edgebox_hook_tsc_sys(sys); } executeCommandLine(sys, noop, sys.args);/",
                bundle_js_path,
            });
            std.fs.cwd().deleteFile(bak_path_ts) catch {};
        }
    } else {
        std.debug.print("[build] Skipping polyfills (--no-polyfill)\n", .{});
    }

    // Check bundle size - skip debug traces for large bundles (>2MB) as they corrupt complex JavaScript
    const skip_traces = if (std.fs.cwd().statFile(bundle_js_path)) |stat| blk: {
        const size_kb = @as(f64, @floatFromInt(stat.size)) / 1024.0;
        std.debug.print("[build] Bundle: bundle.js ({d:.1}KB)\n", .{size_kb});
        break :blk stat.size > 2 * 1024 * 1024;
    } else |_| false;

    // Step 5: Patch known issues in bundled code
    // Some bundles set console to a no-op, we replace with a working version
    std.debug.print("[build] Patching bundle for EdgeBox compatibility...\n", .{});
    // Note: sed -i.bak works on both macOS and Linux (unlike sed -i "" which is macOS-only)
    _ = try runCommand(allocator, &.{
        "sed", "-i.bak",
        "s/console = { log: function() {} };/console = { log: function(a,b,c,d,e) { print(a||'',b||'',c||'',d||'',e||''); } };/g",
        bundle_js_path,
    });
    // Clean up backup file
    const bak_path = try std.fmt.allocPrint(allocator, "{s}.bak", .{bundle_js_path});
    defer allocator.free(bak_path);
    std.fs.cwd().deleteFile(bak_path) catch {};

    // Skip all debug trace injection for large bundles (>2MB)
    // These sed patches use hardcoded line numbers specific to old Claude CLI versions
    // and will corrupt newer bundles
    if (skip_traces) {
        std.debug.print("[build] Large bundle - skipping debug trace injection\n", .{});
    }
    if (!skip_traces) {
        std.debug.print("[build] Injecting trace hooks (optimized: 1 sed call instead of 25)...\n", .{});
        try applyTracePatterns(allocator, bundle_js_path);
    } // end skip_traces

    // Skip freeze if --no-freeze flag is set (faster builds for test262)
    const skip_freeze = options.no_freeze;
    if (skip_freeze) {
        std.debug.print("[build] Skipping freeze analysis (--no-freeze)\n", .{});
    }

    // Step 6: Freeze ORIGINAL bytecode (no manifest filtering - freeze all functions)
    const manifest_content: ?[]u8 = null;

    // Path for hooked bundle (will be created later)
    const bundle_hooked_path = try std.fmt.allocPrint(allocator, "{s}/bundle_hooked.js", .{cache_dir});
    defer allocator.free(bundle_hooked_path);

    if (!skip_freeze) {
        // Step 6a: Compile ORIGINAL JS to bytecode FIRST (before hooks!)
        std.debug.print("[build] Compiling original JS to bytecode for freezing...\n", .{});
        const exit_code = try qjsc_wrapper.compileJsToBytecode(allocator, &.{
            "qjsc",
            "-N", "bundle",
            "-o", bundle_original_path,
            bundle_js_path, // Original bundle without hooks
        });
        if (exit_code != 0) {
            std.debug.print("[error] qjsc compilation failed\n", .{});
            std.process.exit(1);
        }

        // Step 6b: Copy bundle unchanged (no JS hooks - native dispatch in Zig)
        try std.fs.cwd().copyFile(bundle_js_path, std.fs.cwd(), bundle_hooked_path, .{});
        // Manifest generated from bytecode analysis, not JS scanning
    }

    // Step 6c: Freeze bytecode to optimized C (using manifest for names)
    const freeze_success = if (skip_freeze) false else blk: {
        std.debug.print("[build] Freezing bytecode to optimized C...\n", .{});
        // Read the bytecode C file
        const bytecode_file = std.fs.cwd().openFile(bundle_original_path, .{}) catch |err| {
            std.debug.print("[warn] Could not open bundle_original.c: {}\n", .{err});
            break :blk false;
        };
        defer bytecode_file.close();

        const bytecode_content = bytecode_file.readToEndAlloc(allocator, 1024 * 1024 * 1024) catch |err| { // 1GB limit
            std.debug.print("[warn] Could not read bundle_original.c: {}\n", .{err});
            break :blk false;
        };
        defer allocator.free(bytecode_content);

        // Pure Zig freeze pipeline - no C code generation
        // Parse bytecode from C array format
        const file_content = freeze.module_parser.parseCArrayBytecode(allocator, bytecode_content) catch |err| {
            std.debug.print("[warn] Could not parse bytecode: {}\n", .{err});
            break :blk false;
        };
        defer allocator.free(file_content);

        // Analyze the module
        var analysis = freeze.analyzeModule(allocator, file_content) catch |err| {
            std.debug.print("[warn] Analysis failed: {}\n", .{err});
            break :blk false;
        };
        defer analysis.deinit();

        // Generate closure manifest JSON directly (no C code)
        var closure_manifest_path_buf: [4096]u8 = undefined;
        const closure_manifest_path = std.fmt.bufPrint(&closure_manifest_path_buf, "{s}/closure_manifest.json", .{cache_dir}) catch "zig-out/cache/closure_manifest.json";
        if (freeze.generateClosureManifest(allocator, &analysis, manifest_content) catch null) |closure_json| {
            defer allocator.free(closure_json);
            const manifest_file = std.fs.cwd().createFile(closure_manifest_path, .{}) catch null;
            if (manifest_file) |mf| {
                mf.writeAll(closure_json) catch |err| std.debug.print("[build] manifest write: {}\n", .{err});
                mf.close();
                std.debug.print("[build] Closure manifest: {s}\n", .{closure_manifest_path});
            }
        }

        // Generate frozen module via LLVM IR backend
        zig_gen: {
            var sharded = freeze.generateModuleZigShardedWithBackend(allocator, &analysis, options.freeze_max_functions, options.freeze_profile_path, cache_dir, options.freeze_code_budget, options.wasm_only) catch |err| {
                std.debug.print("[warn] LLVM codegen failed: {}\n", .{err});
                break :zig_gen;
            };
            defer sharded.deinit(allocator);

            // Clean up stale Zig shard files from previous runs (no longer generated)
            {
                var stale_buf: [4096]u8 = undefined;
                var stale_idx: usize = 0;
                while (true) : (stale_idx += 1) {
                    const stale_path = std.fmt.bufPrint(&stale_buf, "{s}/frozen_shard_{d}.zig", .{ cache_dir, stale_idx }) catch break;
                    std.fs.cwd().deleteFile(stale_path) catch break;
                }
            }

            // Write main frozen_module.zig (just extern declarations + init)
            var zig_path_buf: [4096]u8 = undefined;
            const zig_path = std.fmt.bufPrint(&zig_path_buf, "{s}/frozen_module.zig", .{cache_dir}) catch "zig-out/cache/frozen_module.zig";
            const zig_file = std.fs.cwd().createFile(zig_path, .{}) catch {
                std.debug.print("[warn] Could not create frozen_module.zig\n", .{});
                break :zig_gen;
            };
            defer zig_file.close();
            zig_file.writeAll(sharded.main) catch {
                std.debug.print("[warn] Could not write frozen_module.zig\n", .{});
                break :zig_gen;
            };

            std.debug.print("[build] Frozen module: {s} ({d} LLVM shards)\n", .{ zig_path, sharded.llvm_shard_count });
        }

        // Note: Closure-based functions fall back to interpreter (not frozen)
        // Native dispatch in Zig handles frozen functions directly

        break :blk true;
    };

    if (!freeze_success) {
        // Create empty Zig stub so build doesn't break
        var empty_zig_path_buf: [4096]u8 = undefined;
        const empty_zig_path = std.fmt.bufPrint(&empty_zig_path_buf, "{s}/frozen_module.zig", .{cache_dir}) catch "zig-out/cache/frozen_module.zig";
        const empty_frozen = std.fs.cwd().createFile(empty_zig_path, .{}) catch null;
        if (empty_frozen) |f| {
            f.writeAll(
                \\// No frozen functions generated
                \\const zig_runtime = @import("zig_runtime");
                \\pub fn frozen_init_c(_: *zig_runtime.JSContext) c_int { return 0; }
                \\
            ) catch |err| std.debug.print("[build] frozen_module write: {}\n", .{err});
            f.close();
        }
    }

    // Ensure output directory exists
    std.fs.cwd().makePath(output_dir) catch |err| std.debug.print("[build] makePath: {}\n", .{err});

    // Step 6d: Generate raw bytecode + build args (only needed for native/WASM-static binary)
    // Worker-only path skips this — it uses standalone.wasm + source transform, not @embedFile bytecode
    const runtime_bundle_path = if (freeze_success) bundle_hooked_path else bundle_js_path;
    var bundle_bin_path_buf: [4096]u8 = undefined;
    const bundle_bin_path = std.fmt.bufPrint(&bundle_bin_path_buf, "{s}/bundle.bin", .{cache_dir}) catch "zig-out/cache/bundle.bin";

    if (!options.wasm_only) {
        std.debug.print("[build] Compiling JS to bytecode: {s} (freeze_success={})\n", .{ runtime_bundle_path, freeze_success });
        const exit_code_bin = try qjsc_wrapper.compileJsToBytecode(allocator, &.{
            "qjsc",
            "-b", // Raw bytecode output (no C wrapper, just bytes)
            "-s", "-s", // Strip source code AND debug info (index-based dispatch doesn't need line numbers)
            "-o", bundle_bin_path,
            runtime_bundle_path,
        });
        if (exit_code_bin != 0) {
            std.debug.print("[warn] Raw bytecode generation failed (native may not work)\n", .{});
        } else {
            if (std.fs.cwd().statFile(bundle_bin_path)) |stat| {
                const size_mb = @as(f64, @floatFromInt(stat.size)) / 1024.0 / 1024.0;
                std.debug.print("[build] Raw bytecode: {s} ({d:.1}MB)\n", .{ bundle_bin_path, size_mb });
            } else |_| {}
        }
    }

    // Generate output filenames and build args (used by Steps 7-10 for native/WASM-static)
    var wasm_path_buf: [4096]u8 = undefined;
    var aot_path_buf: [4096]u8 = undefined;
    var stripped_path_buf: [4096]u8 = undefined;

    const wasm_path = std.fmt.bufPrint(&wasm_path_buf, "{s}/{s}.wasm", .{ output_dir, output_base }) catch {
        std.debug.print("[error] Output path too long: {s}/{s}.wasm\n", .{ output_dir, output_base });
        std.process.exit(1);
    };
    const aot_path = std.fmt.bufPrint(&aot_path_buf, "{s}/{s}.aot", .{ output_dir, output_base }) catch {
        std.debug.print("[error] Output path too long: {s}/{s}.aot\n", .{ output_dir, output_base });
        std.process.exit(1);
    };
    const stripped_path = std.fmt.bufPrint(&stripped_path_buf, "{s}/{s}-stripped.wasm", .{ output_dir, output_base }) catch {
        std.debug.print("[error] Output path too long: {s}/{s}-stripped.wasm\n", .{ output_dir, output_base });
        std.process.exit(1);
    };

    // Construct bytecode path argument
    var bytecode_arg_buf: [4096]u8 = undefined;
    const bytecode_arg = std.fmt.bufPrint(&bytecode_arg_buf, "-Dbytecode={s}/bundle.bin", .{cache_dir}) catch {
        std.debug.print("[error] Bytecode path too long\n", .{});
        std.process.exit(1);
    };

    // Allocator type argument
    const allocator_arg = switch (options.allocator_type) {
        .arena => "-Dallocator=arena",
        .c => "-Dallocator=c",
        .gpa => "-Dallocator=gpa",
    };

    // Build prefix and cache-dir for isolated builds
    var zig_cache_path_buf: [4096]u8 = undefined;
    const zig_cache_path = std.fmt.bufPrint(&zig_cache_path_buf, "{s}/.zig-cache", .{out_prefix}) catch "zig-out/.zig-cache";
    var cache_prefix_arg_buf: [4096]u8 = undefined;
    const cache_prefix_arg = std.fmt.bufPrint(&cache_prefix_arg_buf, "-Dcache-prefix={s}/cache", .{out_prefix}) catch "-Dcache-prefix=zig-out/cache";

    const optimize_arg = if (options.debug_build) "-Doptimize=Debug" else "-Doptimize=ReleaseFast";
    const frozen_optimize_arg = "-Dfrozen-optimize=ReleaseFast";
    const frozen_thin_optimize_arg = "-Dfrozen-thin-optimize=ReleaseSmall";

    // Step 6e: Link standalone WASM (pure int32 functions, no QuickJS runtime)
    // Produces a tiny .wasm file for use in V8/workerd Workers
    var standalone_wasm_path_buf: [4096]u8 = undefined;
    var has_standalone_wasm = false;
    {
        var standalone_o_buf: [4096]u8 = undefined;
        const standalone_o = std.fmt.bufPrint(&standalone_o_buf, "{s}/standalone.o", .{cache_dir}) catch null;
        if (standalone_o) |so| {
            if (std.fs.cwd().access(so, .{})) |_| {
                const standalone_wasm = std.fmt.bufPrint(&standalone_wasm_path_buf, "{s}/{s}-standalone.wasm", .{ output_dir, output_base }) catch null;
                if (standalone_wasm) |sw| {
                    const link_result = runCommand(allocator, &.{
                        "wasm-ld-19", "--no-entry", "--export-all", "--export-memory",
                        "--allow-undefined", "--initial-memory=131072", "-o", sw, so,
                    }) catch null;
                    if (link_result) |lr| {
                        defer {
                            if (lr.stdout) |s| allocator.free(s);
                            if (lr.stderr) |s| allocator.free(s);
                        }
                        const link_ok = switch (lr.term) {
                            .Exited => |code| code == 0,
                            else => false,
                        };
                        if (link_ok) {
                            has_standalone_wasm = true;
                            if (std.fs.cwd().statFile(sw)) |stat| {
                                std.debug.print("[build] Standalone WASM: {s} ({d} bytes)\n", .{ sw, stat.size });
                            } else |_| {}
                        } else {
                            std.debug.print("[warn] Standalone WASM linking failed\n", .{});
                        }
                    }
                }
            } else |_| {} // No standalone.o — no pure int32 functions
        }
    }

    // If no numeric WASM was produced, emit a minimal WASM (just exported memory)
    // so the worker pipeline always works — SOA pools need a memory buffer.
    if (!has_standalone_wasm) {
        const minimal_wasm = [_]u8{
            0x00, 0x61, 0x73, 0x6d, 0x01, 0x00, 0x00, 0x00, // \0asm v1
            0x05, 0x03, 0x01, 0x00, 0x02, // memory: 1 mem, min 2 pages
            0x07, 0x0a, 0x01, 0x06, 0x6d, 0x65, 0x6d, 0x6f, 0x72, 0x79, 0x02, 0x00, // export "memory"
        };
        const sw = std.fmt.bufPrint(&standalone_wasm_path_buf, "{s}/{s}-standalone.wasm", .{ output_dir, output_base }) catch null;
        if (sw) |swp| {
            if (std.fs.cwd().createFile(swp, .{})) |f| {
                defer f.close();
                f.writeAll(&minimal_wasm) catch |err| std.debug.print("[build] wasm write: {}\n", .{err});
                has_standalone_wasm = true;
            } else |_| {}
        }
    }

    // Step 6f: Generate workerd worker files (worker.mjs + config.capnp)
    // Uses the clean bundle (pre-polyfill) + standalone WASM
    var has_worker_files = false;
    if (has_standalone_wasm) {
        var worker_path_buf: [4096]u8 = undefined;
        var config_path_buf: [4096]u8 = undefined;
        var module_path_buf: [4096]u8 = undefined;
        const worker_path = std.fmt.bufPrint(&worker_path_buf, "{s}/{s}-worker.js", .{ output_dir, output_base }) catch null;
        const config_path = std.fmt.bufPrint(&config_path_buf, "{s}/{s}-config.capnp", .{ output_dir, output_base }) catch null;
        const module_path = std.fmt.bufPrint(&module_path_buf, "{s}/{s}-module.cjs", .{ output_dir, output_base }) catch null;

        if (worker_path != null and config_path != null) {
            // Read standalone manifest to know which functions are in WASM
            var manifest_buf: [4096]u8 = undefined;
            const manifest_path = std.fmt.bufPrint(&manifest_buf, "{s}/standalone_manifest.json", .{cache_dir}) catch null;
            var wasm_manifest: ?[]const u8 = null;
            if (manifest_path) |mp| {
                wasm_manifest = std.fs.cwd().readFileAlloc(allocator, mp, 1024 * 1024) catch null;
            }
            defer if (wasm_manifest) |wm| allocator.free(wm);

            // Read alloc manifest — factory functions that create objects with fixed shapes
            var alloc_manifest_buf: [4096]u8 = undefined;
            const alloc_manifest_path = std.fmt.bufPrint(&alloc_manifest_buf, "{s}/alloc_manifest.json", .{cache_dir}) catch null;
            var alloc_manifest: ?[]const u8 = null;
            if (alloc_manifest_path) |amp| {
                alloc_manifest = std.fs.cwd().readFileAlloc(allocator, amp, 4 * 1024 * 1024) catch null;
            }
            defer if (alloc_manifest) |am| allocator.free(am);

            // Read patch manifest — bytecode-driven source patches (line, field, arg)
            var patch_manifest_buf: [4096]u8 = undefined;
            const patch_manifest_path_rt = std.fmt.bufPrint(&patch_manifest_buf, "{s}/patch_manifest.json", .{cache_dir}) catch null;
            var patch_manifest: ?[]const u8 = null;
            if (patch_manifest_path_rt) |pmp| {
                patch_manifest = std.fs.cwd().readFileAlloc(allocator, pmp, 8 * 1024 * 1024) catch null;
            }
            defer if (patch_manifest) |pm| allocator.free(pm);

            // Read clean bundle
            var clean_content_orig: ?[]u8 = null;
            if (clean_bundle_path) |cbp| {
                clean_content_orig = std.fs.cwd().readFileAlloc(allocator, cbp, 50 * 1024 * 1024) catch null;
            }
            defer if (clean_content_orig) |cc| allocator.free(cc);

            // TSC source patching: replace relation cache Maps with FastRelationCache
            var clean_content_patched: ?[]u8 = null;
            defer if (clean_content_patched) |p| allocator.free(p);

            const clean_content: ?[]const u8 = if (clean_content_orig) |orig| blk: {
                if (std.mem.indexOf(u8, orig, "var assignableRelation") != null) {
                    // Bun bundler outputs "new Map;" (no parens), original TSC has "new Map()"
                    // Also patch getRelationKey to return packed integer (zero-copy key)
                    const needle = "/* @__PURE__ */ new Map;";
                    const repl = "new __FastRelationCache;";
                    const grk_needle = "return isTypeReferenceWithGenericArguments(source) && isTypeReferenceWithGenericArguments(target) ? getGenericTypeReferenceRelationKey(source, target, postFix, ignoreConstraints) : `${source.id},${target.id}${postFix}`;";
                    const grk_repl = "if(!postFix&&!isTypeReferenceWithGenericArguments(source)&&!isTypeReferenceWithGenericArguments(target)&&source.id>0&&source.id<67108864&&target.id>0&&target.id<67108864)return source.id*67108864+target.id+1;return isTypeReferenceWithGenericArguments(source)&&isTypeReferenceWithGenericArguments(target)?getGenericTypeReferenceRelationKey(source,target,postFix,ignoreConstraints):`${source.id},${target.id}${postFix}`;";
                    // Also patch: id.startsWith("*") → (typeof id==='string'&&id.startsWith("*"))
                    const sw_needle = "id.startsWith(\"*\")";
                    const sw_repl = "(typeof id===\"string\"&&id.startsWith(\"*\"))";
                    // Patch getFlowCacheKey: pack 4 IDs into single number instead of string concat
                    const fck_needle = "return symbol !== unknownSymbol ? `${flowContainer ? getNodeId(flowContainer) : \"-1\"}|${getTypeId(declaredType)}|${getTypeId(initialType)}|${getSymbolId(symbol)}` : undefined;";
                    const fck_repl = "if(symbol===unknownSymbol)return undefined;var __fc=flowContainer?getNodeId(flowContainer):0,__dt=getTypeId(declaredType),__it=getTypeId(initialType),__si=getSymbolId(symbol);if(__fc<10000&&__dt<10000&&__it<10000&&__si<10000)return __fc*1000000000000+__dt*100000000+__it*10000+__si+1;return`${flowContainer?getNodeId(flowContainer):\"-1\"}|${__dt}|${__it}|${__si}`;";
                    // Patch getFlowCacheKey "this" case: 3 IDs → integer
                    const fck2_needle = "return `0|${flowContainer ? getNodeId(flowContainer) : \"-1\"}|${getTypeId(declaredType)}|${getTypeId(initialType)}`;";
                    const fck2_repl = "{var __fc2=flowContainer?getNodeId(flowContainer):0,__dt2=getTypeId(declaredType),__it2=getTypeId(initialType);if(__fc2<100000&&__dt2<100000&&__it2<100000)return __fc2*10000000000+__dt2*100000+__it2+1;return`0|${flowContainer?getNodeId(flowContainer):\"-1\"}|${__dt2}|${__it2}`;}";
                    // Patch getFlowCacheKey array/object case: nodeId#typeId → integer
                    const fck3_needle = "return `${getNodeId(node)}#${getTypeId(declaredType)}`;";
                    const fck3_repl = "{var __ni=getNodeId(node),__di=getTypeId(declaredType);if(__ni<100000&&__di<100000)return __ni*100000+__di+1;return`${__ni}#${__di}`;}";
                    // Parallel type checking: shard the forEach(sourceFiles, check) loop
                    // When __EDGEBOX_WORKER env is set, only check files in this shard.
                    const par_needle = "forEach(host.getSourceFiles(), (file) => checkSourceFileWithEagerDiagnostics(file));";
                    const par_repl = "{const __files=host.getSourceFiles();const __shard=parseInt(process.env.__EDGEBOX_SHARD||'0');const __total=parseInt(process.env.__EDGEBOX_TOTAL||'1');const __start=Math.floor(__files.length*__shard/__total);const __end=Math.floor(__files.length*(__shard+1)/__total);for(let __i=__start;__i<__end;__i++)checkSourceFileWithEagerDiagnostics(__files[__i]);}";
                    // Inline relation cache removed — typeof check overhead at call sites
                    // outweighs the cache benefit on large projects (typeorm 1.2x slower).
                    // The getRelationKey integer packing already eliminates string allocation.
                    // Map.get() with integer keys is fast enough without a cache layer.
                    const rc_get_needle = "DISABLED_rc_get";
                    const rc_get_repl = "DISABLED_rc_get";
                    const rc_get2_needle = "DISABLED_rc_get2";
                    const rc_get2_repl = "DISABLED_rc_get2";
                    // getTypeOfSymbol cache disabled — type resolution is context-dependent.
                    // Caching breaks on deferred/instantiated types that vary during checking.
                    // TODO: implement safe caching with invalidation (two-pass approach).
                    const gtos_needle = "DISABLED_getTypeOfSymbol";
                    const gtos_repl = "DISABLED_getTypeOfSymbol";
                    // Patch ALL Maps — FastRelationCache must be complete drop-in.
                    // Broad patching (all 161 Maps) causes silent crashes on some Maps
                    // that use Set-like iteration patterns our FastRelationCache doesn't handle.
                    var rcount: usize = 0;
                    var ri: usize = 0;
                    while (ri + needle.len <= orig.len) : (ri += 1) {
                        if (std.mem.startsWith(u8, orig[ri..], needle) and
                            true)
                            rcount += 1;
                    }
                    if (rcount > 0) {
                        var buf = allocator.alloc(u8, orig.len + rcount * repl.len + grk_repl.len + 10 * sw_repl.len + fck_repl.len + fck2_repl.len + gtos_repl.len + par_repl.len) catch break :blk @as(?[]const u8, orig);
                        var pw: usize = 0;
                        var pr: usize = 0;
                        while (pr < orig.len) {
                            // P1: Relation = new Map → new __FastRelationCache
                            if (pr + needle.len <= orig.len and
                                std.mem.startsWith(u8, orig[pr..], needle) and
                                true)
                            {
                                @memcpy(buf[pw..][0..repl.len], repl);
                                pw += repl.len;
                                pr += needle.len;
                                continue;
                            }
                            // P2: getRelationKey → packed integer return
                            if (pr + grk_needle.len <= orig.len and
                                std.mem.startsWith(u8, orig[pr..], grk_needle))
                            {
                                @memcpy(buf[pw..][0..grk_repl.len], grk_repl);
                                pw += grk_repl.len;
                                pr += grk_needle.len;
                                continue;
                            }
                            // P3: id.startsWith("*") → typeof guard for numeric keys
                            if (pr + sw_needle.len <= orig.len and
                                std.mem.startsWith(u8, orig[pr..], sw_needle))
                            {
                                @memcpy(buf[pw..][0..sw_repl.len], sw_repl);
                                pw += sw_repl.len;
                                pr += sw_needle.len;
                                continue;
                            }
                            // P4: getFlowCacheKey identifier → integer packing for 4-ID keys
                            if (pr + fck_needle.len <= orig.len and
                                std.mem.startsWith(u8, orig[pr..], fck_needle))
                            {
                                @memcpy(buf[pw..][0..fck_repl.len], fck_repl);
                                pw += fck_repl.len;
                                pr += fck_needle.len;
                                continue;
                            }
                            // P5: getTypeOfSymbol → flat array cache
                            if (pr + gtos_needle.len <= orig.len and
                                std.mem.startsWith(u8, orig[pr..], gtos_needle))
                            {
                                @memcpy(buf[pw..][0..gtos_repl.len], gtos_repl);
                                pw += gtos_repl.len;
                                pr += gtos_needle.len;
                                continue;
                            }
                            // P6: getFlowCacheKey array/object → integer packing for 2-ID keys
                            if (pr + fck3_needle.len <= orig.len and
                                std.mem.startsWith(u8, orig[pr..], fck3_needle))
                            {
                                @memcpy(buf[pw..][0..fck3_repl.len], fck3_repl);
                                pw += fck3_repl.len;
                                pr += fck3_needle.len;
                                continue;
                            }
                            // P7: inline relation cache get (fast path)
                            if (pr + rc_get_needle.len <= orig.len and
                                std.mem.startsWith(u8, orig[pr..], rc_get_needle))
                            {
                                @memcpy(buf[pw..][0..rc_get_repl.len], rc_get_repl);
                                pw += rc_get_repl.len;
                                pr += rc_get_needle.len;
                                continue;
                            }
                            // P8: inline relation cache get (entry pattern)
                            if (pr + rc_get2_needle.len <= orig.len and
                                std.mem.startsWith(u8, orig[pr..], rc_get2_needle))
                            {
                                @memcpy(buf[pw..][0..rc_get2_repl.len], rc_get2_repl);
                                pw += rc_get2_repl.len;
                                pr += rc_get2_needle.len;
                                continue;
                            }
                            // P9: parallel type checking — shard forEach loop
                            if (pr + par_needle.len <= orig.len and
                                std.mem.startsWith(u8, orig[pr..], par_needle))
                            {
                                @memcpy(buf[pw..][0..par_repl.len], par_repl);
                                pw += par_repl.len;
                                pr += par_needle.len;
                                continue;
                            }
                            // P8: getFlowCacheKey "this" → integer packing for 3-ID keys
                            if (pr + fck2_needle.len <= orig.len and
                                std.mem.startsWith(u8, orig[pr..], fck2_needle))
                            {
                                @memcpy(buf[pw..][0..fck2_repl.len], fck2_repl);
                                pw += fck2_repl.len;
                                pr += fck2_needle.len;
                                continue;
                            }
                            buf[pw] = orig[pr];
                            pw += 1;
                            pr += 1;
                        }
                        clean_content_patched = buf[0..pw];
                        std.debug.print("[soa] Patched {d} relation caches → FastRelationCache in source\n", .{rcount});
                        break :blk @as(?[]const u8, clean_content_patched.?);
                    }
                }
                break :blk @as(?[]const u8, orig);
            } else null;

            var wasm_fn_buf: [256]u8 = undefined;
            const wasm_filename = std.fmt.bufPrint(&wasm_fn_buf, "{s}-standalone.wasm", .{output_base}) catch "standalone.wasm";

            // Generate worker shim + module — split for V8 compile cache.
            // The shim enables enableCompileCache() and require()'s the module.
            // This matches TypeScript's tsc.js → _tsc.js pattern for fast startup.
            if (worker_path) |wp| {
                // Write package.json to force CommonJS in the output directory
                // (avoids inheriting "type": "module" from parent package.json)
                var pkg_json_buf: [4096]u8 = undefined;
                const pkg_json_path = std.fmt.bufPrint(&pkg_json_buf, "{s}/package.json", .{output_dir}) catch null;
                if (pkg_json_path) |pjp| {
                    if (std.fs.cwd().createFile(pjp, .{})) |pjf| {
                        defer pjf.close();
                        pjf.writeAll("{\"type\":\"commonjs\"}\n") catch {};
                    } else |_| {}
                }

                // Write the thin shim (enableCompileCache + incremental + require)
                var module_fn_buf: [256]u8 = undefined;
                const module_filename = std.fmt.bufPrint(&module_fn_buf, "{s}-module.cjs", .{output_base}) catch "module.cjs";
                if (std.fs.cwd().createFile(wp, .{})) |shim_f| {
                    defer shim_f.close();
                    var shim_buf: [4096]u8 = undefined;
                    var shim_state = shim_f.writer(&shim_buf);
                    const shim_w = &shim_state.interface;
                    shim_w.print(
                        \\// EdgeBox AOT+JIT shim (V8 compile cache + parallel type checking)
                        \\try {{ require("node:module").enableCompileCache(); }} catch {{}}
                        \\// Auto-enable --incremental for faster warm builds (same diagnostics, ~2x faster)
                        \\if (!process.argv.includes('--incremental')) {{
                        \\  const __p = require('path'), __os = require('os');
                        \\  const __d = __p.join(__os.tmpdir(), 'edgebox-incr-cache');
                        \\  try {{ require('fs').mkdirSync(__d, {{ recursive: true }}); }} catch {{}}
                        \\  process.argv.push('--incremental', '--tsBuildInfoFile', __p.join(__d, 'tsinfo.json'));
                        \\}}
                        \\// Parallel type checking: if EDGEBOX_PARALLEL=N, run N TSC instances
                        \\// each checking a subset of files, then merge diagnostics.
                        \\const __PARALLEL = parseInt(process.env.EDGEBOX_PARALLEL || '0', 10);
                        \\if (__PARALLEL > 1 && !process.env.__EDGEBOX_WORKER) {{
                        \\  const {{ Worker, isMainThread }} = require('worker_threads');
                        \\  const __args = process.argv.slice(2);
                        \\  const __workers = [];
                        \\  for (let __i = 0; __i < __PARALLEL; __i++) {{
                        \\    const __env = {{ ...(process.env), __EDGEBOX_WORKER: '1', __EDGEBOX_SHARD: String(__i), __EDGEBOX_TOTAL: String(__PARALLEL) }};
                        \\    __workers.push(new Worker(require.resolve('./{s}'), {{ env: __env, argv: __args }}));
                        \\  }}
                        \\  let __done = 0;
                        \\  __workers.forEach(w => w.on('exit', () => {{ __done++; if (__done === __PARALLEL) process.exit(0); }}));
                        \\}} else {{
                        \\  require('./{s}');
                        \\}}
                        \\
                    , .{module_filename, module_filename}) catch {};
                    shim_w.flush() catch {};
                    std.debug.print("[build] Shim: {s}\n", .{wp});
                } else |_| {}

                // Write the module (WASM init + transformed source)
                if (module_path) |mp_out| {
                if (std.fs.cwd().createFile(mp_out, .{})) |wf| {
                    defer wf.close();

                    // Collect WASM function metadata from manifest (dynamic, no limit)
                    var wasm_func_list: std.ArrayListUnmanaged(WasmFunc) = .{};
                    defer wasm_func_list.deinit(allocator);
                    // Parse manifest JSON (same std.json used throughout the codebase)
                    var json_parsed: ?std.json.Parsed(std.json.Value) = if (wasm_manifest) |mc|
                        std.json.parseFromSlice(std.json.Value, allocator, mc, .{}) catch null
                    else
                        null;
                    defer if (json_parsed) |*p| p.deinit();
                    if (json_parsed) |p| {
                        if (p.value == .array) {
                            for (p.value.array.items) |item| {
                                if (item != .object) continue;
                                const obj = item.object;
                                const name = jsonStr(obj, "name") orelse continue;
                                const aa = jsonInt(obj, "array_args");
                                var entry: WasmFunc = .{
                                    .name = name,
                                    .arg_count = @max(1, jsonInt(obj, "args")),
                                    .instr_count = jsonInt(obj, "instrs"),
                                    .is_recursive = jsonInt(obj, "recursive") == 1,
                                    .array_args = @truncate(aa),
                                    .mutated_args = @truncate(jsonIntOr(obj, "mutated_args", aa)),
                                    .read_array_args = @truncate(jsonIntOr(obj, "read_array_args", aa)),
                                    .length_args = @truncate(jsonInt(obj, "length_args")),
                                    .has_loop = jsonInt(obj, "has_loop") == 1,
                                    .has_bitwise = jsonInt(obj, "has_bitwise") == 1,
                                    .line_num = jsonInt(obj, "line"),
                                    .is_anon = std.mem.startsWith(u8, name, "__anon_"),
                                    .is_f64 = if (jsonStr(obj, "type")) |t| std.mem.eql(u8, t, "f64") else false,
                                };
                                // Parse struct_args, array_of_struct_args, and struct_fields from manifest
                                const sa = jsonInt(obj, "struct_args");
                                const aos = jsonInt(obj, "array_of_struct_args");
                                if (sa != 0) entry.struct_args = @truncate(sa);
                                if (aos != 0) entry.array_of_struct_args = @truncate(aos);
                                if (sa != 0 or aos != 0) {
                                    if (obj.get("struct_fields")) |sf_val| {
                                        if (sf_val == .object) {
                                            var sf_it = sf_val.object.iterator();
                                            while (sf_it.next()) |sf_entry| {
                                                const arg_idx = std.fmt.parseInt(u8, sf_entry.key_ptr.*, 10) catch continue;
                                                if (arg_idx >= 8) continue;
                                                if (sf_entry.value_ptr.* == .array) {
                                                    for (sf_entry.value_ptr.array.items, 0..) |field_val, fi| {
                                                        if (fi >= 16) break;
                                                        if (field_val == .string) {
                                                            entry.struct_field_names[arg_idx][fi] = field_val.string;
                                                            entry.struct_field_counts[arg_idx] = @intCast(fi + 1);
                                                        }
                                                    }
                                                }
                                            }
                                        }
                                    }
                                }
                                wasm_func_list.append(allocator, entry) catch continue;
                            }
                        }
                    }

                    // Pre-scan WASM function list: check which functions will actually be trampolined,
                    // and determine memory sizing requirements.
                    var has_array_funcs = false;
                    var has_i32_array_funcs = false;
                    var has_f64_array_funcs = false;
                    var any_cacheable_args = false;
                    var any_trampolined = false;
                    for (wasm_func_list.items) |wfe| {
                        // Check if this function would be trampolined (conservative: calls_wasm=false)
                        if (shouldTrampolineToWasm(wfe, false)) any_trampolined = true;
                        if (wfe.array_args != 0) {
                            has_array_funcs = true;
                            if (wfe.is_f64) has_f64_array_funcs = true else has_i32_array_funcs = true;
                            const ro = wfe.array_args & ~wfe.mutated_args;
                            if (ro != 0) any_cacheable_args = true;
                        }
                        if (wfe.struct_args != 0 or wfe.array_of_struct_args != 0) {
                            has_array_funcs = true;
                            has_i32_array_funcs = true;
                        }
                    }

                    // Size WASM memory: minimal for scalar-only, larger for array workloads
                    // 16 pages (1MB) for scalar-only, 256 pages (16MB) for array args
                    const mem_grow_pages: u32 = if (has_array_funcs) 256 else 16;

                    // Preamble: load WASM module (compatible with Node.js + workerd)
                    var w_buf: [65536]u8 = undefined;
                    var w_state = wf.writer(&w_buf);
                    const w = &w_state.interface;
                    var w_errs: u32 = 0; // track write errors instead of silent catch {}

                    if (any_trampolined) {
                        w.print(
                            \\// EdgeBox AOT+JIT (auto-generated)
                            \\var __wasm;
                            \\if (globalThis.__edgebox_wasm_module) {{
                            \\  __wasm = new WebAssembly.Instance(globalThis.__edgebox_wasm_module, {{ env: {{ pow: Math.pow }} }});
                            \\}} else {{
                            \\  const {{ readFileSync }} = require('fs');
                            \\  const {{ join }} = require('path');
                            \\  const buf = readFileSync(join(__dirname, '{s}'));
                            \\  __wasm = new WebAssembly.Instance(new WebAssembly.Module(buf), {{ env: {{ pow: Math.pow }} }});
                            \\}}
                            \\__wasm.exports.memory.grow({d});
                            \\
                        , .{ wasm_filename, mem_grow_pages }) catch { w_errs += 1; };
                    } else {
                        std.debug.print("[build] No WASM trampolines needed — skipping WASM loading in module\n", .{});
                        w.writeAll("// EdgeBox AOT+JIT (auto-generated, no WASM trampolines)\n") catch { w_errs += 1; };

                        // Inject FastRelationCache for TSC — zero-copy Map replacement
                        // Detects TSC by checking for assignableRelation in source
                        if (clean_content) |detect_cc| {
                            if (std.mem.indexOf(u8, detect_cc, "var assignableRelation") != null) {
                                w.writeAll(@embedFile("v8_tsc_shim.js")) catch { w_errs += 1; };
                                w.writeAll("\n") catch { w_errs += 1; };
                                std.debug.print("[soa] Injected FastRelationCache shim into compiled worker\n", .{});
                            }
                        }
                    }

                    if (has_array_funcs and any_trampolined) {
                        // Memory views + stack allocator for WASM linear memory
                        w.writeAll("const __wbuf = __wasm ? __wasm.exports.memory.buffer : null;\n") catch { w_errs += 1; };
                        if (has_i32_array_funcs) w.writeAll("const __m = __wasm ? new Int32Array(__wbuf) : null;\n") catch { w_errs += 1; };
                        if (has_f64_array_funcs) w.writeAll("const __m_f64 = __wasm ? new Float64Array(__wbuf) : null;\n") catch { w_errs += 1; };
                        w.writeAll(
                            \\let __sp = __wbuf.byteLength;
                            \\function __wasmStackSave() { return __sp; }
                            \\function __wasmStackRestore(p) { __sp = p; }
                            \\function __wasmStackAlloc(n) { __sp = (__sp - n) & ~7; return __sp; }
                            \\function __wasmArray(T, n) { return new T(__wbuf, __wasmStackAlloc(n * T.BYTES_PER_ELEMENT), n); }
                            \\
                        ) catch { w_errs += 1; };
                        // Per-arg identity cache for repeated calls with same array ref
                        if (any_cacheable_args) {
                            w.writeAll("let __last_fn = -1;\n") catch { w_errs += 1; };
                            for (wasm_func_list.items, 0..) |wfe, wfi| {
                                const read_only = wfe.array_args & ~wfe.mutated_args;
                                if (read_only == 0) continue;
                                var ai: u32 = 0;
                                while (ai < 8) : (ai += 1) {
                                    if (read_only & (@as(u8, 1) << @intCast(ai)) != 0)
                                        w.print("let __c{d}_{d}=null;\n", .{ wfi, ai }) catch { w_errs += 1; };
                                }
                            }
                        }
                    }

                    // Column existence set (populated during column declaration)
                    var col_set = std.StringHashMap(void).init(allocator);
                    defer col_set.deinit();

                    // === Parse patch manifest: bytecode-driven field access patches ===
                    // Each patch: {line, field} — apply at the source line level
                    const PatchEntry = struct { line: u32, field: []const u8 };
                    var patches: std.ArrayListUnmanaged(PatchEntry) = .{};
                    defer patches.deinit(allocator);
                    // Build set of line numbers that have patches
                    var patch_lines = std.AutoHashMap(u32, void).init(allocator);
                    defer patch_lines.deinit();
                    var patch_set = std.AutoHashMap(u64, []const u8).init(allocator);
                    defer patch_set.deinit();
                    if (patch_manifest) |pm_content| {
                        var pm_parsed = std.json.parseFromSlice(std.json.Value, allocator, pm_content, .{}) catch null;
                        defer if (pm_parsed) |*p| p.deinit();
                        if (pm_parsed) |p| {
                            if (p.value == .array) {
                                for (p.value.array.items) |item| {
                                    if (item != .object) continue;
                                    const obj = item.object;
                                    const line_val = obj.get("line") orelse continue;
                                    const line: u32 = if (line_val == .integer) @intCast(@as(u32, @bitCast(@as(i32, @intCast(line_val.integer))))) else continue;
                                    const field_val = obj.get("field") orelse continue;
                                    if (field_val != .string) continue;
                                    const field = allocator.dupe(u8, field_val.string) catch continue;
                                    patches.append(allocator, .{ .line = line, .field = field }) catch continue;
                                }
                            }
                        }
                    }
                    std.debug.print("[soa] Loaded {d} patches for {d} lines\n", .{ patches.items.len, patch_lines.count() });

                    // === SOA Transform: rewrite factory functions to allocate in WASM linear memory ===
                    // Parse alloc manifest and generate SOA pool declarations
                    var alloc_sites: std.ArrayListUnmanaged(AllocSite) = .{};
                    defer alloc_sites.deinit(allocator);
                    if (alloc_manifest) |am_content| {
                        var am_parsed = std.json.parseFromSlice(std.json.Value, allocator, am_content, .{}) catch null;
                        defer if (am_parsed) |*p| p.deinit();
                        if (am_parsed) |p| {
                            if (p.value == .array) {
                                for (p.value.array.items) |item| {
                                    if (item != .object) continue;
                                    const obj = item.object;
                                    const name = jsonStr(obj, "name") orelse continue;
                                    // Skip anonymous/polyfill functions — only transform named factories
                                    if (std.mem.startsWith(u8, name, "__anon_")) continue;
                                    const fields_val = obj.get("alloc_fields") orelse continue;
                                    if (fields_val != .array) continue;
                                    if (fields_val.array.items.len == 0 or fields_val.array.items.len > 16) continue;
                                    // Dupe name — JSON strings are freed when am_parsed goes out of scope
                                    const duped_name = allocator.dupe(u8, name) catch continue;
                                    var site = AllocSite{
                                        .name = duped_name,
                                        .line_num = jsonInt(obj, "line"),
                                        .field_names = [_][]const u8{""} ** 16,
                                        .field_count = 0,
                                    };
                                    for (fields_val.array.items, 0..) |fv, fi| {
                                        if (fi >= 16) break;
                                        if (fv == .string) {
                                            site.field_names[fi] = allocator.dupe(u8, fv.string) catch continue;
                                            site.field_count = @intCast(fi + 1);
                                        }
                                    }
                                    // Only load pass-through factories (proven working)
                                    const pt = obj.get("pass_through") orelse continue;
                                    const is_pt = if (pt == .bool) pt.bool else false;
                                    if (!is_pt or site.field_count == 0) continue;
                                    // Load arg_indices for correct field→param mapping
                                    if (obj.get("arg_indices")) |ai_val| {
                                        if (ai_val == .array) {
                                            for (ai_val.array.items, 0..) |av, ai| {
                                                if (ai >= 16) break;
                                                site.arg_indices[ai] = switch (av) {
                                                    .integer => |v| @intCast(@as(u32, @bitCast(@as(i32, @intCast(v))))),
                                                    else => @intCast(ai),
                                                };
                                            }
                                        }
                                    }
                                    // Check if constructor pattern
                                    if (obj.get("is_constructor")) |ic_val| {
                                        site.is_constructor = if (ic_val == .bool) ic_val.bool else false;
                                    }
                                    alloc_sites.append(allocator, site) catch continue;
                                }
                            }
                        }
                    }


                    // === Pre-scan for inline object literal push patterns ===
                    // Detect VAR.push({ key: val, ... }) in source BEFORE column declarations,
                    // so synthetic alloc sites are included in column generation.
                    if (clean_content) |il_cc| {
                        var isp: usize = 0;
                        while (isp + 7 < il_cc.len) : (isp += 1) {
                            if (!std.mem.startsWith(u8, il_cc[isp..], ".push(")) continue;
                            var lp2 = isp + 6;
                            while (lp2 < il_cc.len and il_cc[lp2] == ' ') lp2 += 1;
                            if (lp2 >= il_cc.len or il_cc[lp2] != '{') continue;
                            // Extract variable name before .push(
                            const ve2 = isp;
                            var vs2 = ve2;
                            while (vs2 > 0 and isIdentChar(il_cc[vs2 - 1])) vs2 -= 1;
                            if (vs2 >= ve2) continue;
                            const vn2 = il_cc[vs2..ve2];
                            // Skip known non-container names
                            if (std.mem.eql(u8, vn2, "arguments") or std.mem.eql(u8, vn2, "result")) continue;
                            // Skip if this variable already has an alloc site (named factory)
                            var has_named = false;
                            for (alloc_sites.items) |site| {
                                if (std.mem.eql(u8, site.name, vn2)) { has_named = true; break; }
                            }
                            if (has_named) continue;
                            // Extract field names from { key: val, key: val, ... }
                            var il_field_names: [16][]const u8 = .{""} ** 16;
                            var il_fc: u8 = 0;
                            var il_fp = lp2 + 1;
                            var il_depth: u32 = 1;
                            while (il_fp < il_cc.len and il_depth > 0) {
                                if (il_cc[il_fp] == '{') { il_depth += 1; il_fp += 1; continue; }
                                if (il_cc[il_fp] == '}') { il_depth -= 1; il_fp += 1; continue; }
                                if (il_depth > 1) { il_fp += 1; continue; }
                                while (il_fp < il_cc.len and (il_cc[il_fp] == ' ' or il_cc[il_fp] == '\n' or il_cc[il_fp] == '\r')) il_fp += 1;
                                if (il_fp >= il_cc.len or il_cc[il_fp] == '}') continue;
                                const il_fks = il_fp;
                                while (il_fp < il_cc.len and isIdentChar(il_cc[il_fp])) il_fp += 1;
                                if (il_fp > il_fks and il_fp < il_cc.len) {
                                    var il_cp = il_fp;
                                    while (il_cp < il_cc.len and il_cc[il_cp] == ' ') il_cp += 1;
                                    if (il_cp < il_cc.len and il_cc[il_cp] == ':' and (il_cp + 1 >= il_cc.len or il_cc[il_cp + 1] != ':')) {
                                        if (il_fc < 16) {
                                            il_field_names[il_fc] = il_cc[il_fks..il_fp];
                                            il_fc += 1;
                                        }
                                        il_fp = il_cp + 1;
                                        var il_vd: u32 = 0;
                                        while (il_fp < il_cc.len) {
                                            if (il_cc[il_fp] == '(' or il_cc[il_fp] == '[' or il_cc[il_fp] == '{') { il_vd += 1; il_fp += 1; continue; }
                                            if (il_cc[il_fp] == ')' or il_cc[il_fp] == ']') { if (il_vd > 0) il_vd -= 1; il_fp += 1; continue; }
                                            if (il_cc[il_fp] == '}') { if (il_vd > 0) { il_vd -= 1; il_fp += 1; continue; } break; }
                                            if (il_cc[il_fp] == ',' and il_vd == 0) { il_fp += 1; break; }
                                            il_fp += 1;
                                        }
                                        continue;
                                    }
                                }
                                while (il_fp < il_cc.len and il_cc[il_fp] != ',' and il_cc[il_fp] != '}') il_fp += 1;
                                if (il_fp < il_cc.len and il_cc[il_fp] == ',') il_fp += 1;
                            }
                            if (il_fc >= 3) {
                                // Check if we already have a synthetic site for this variable
                                var already_syn = false;
                                for (alloc_sites.items) |site| {
                                    if (std.mem.startsWith(u8, site.name, "__inline_")) {
                                        // Check if same fields
                                        if (site.field_count == il_fc) {
                                            var same = true;
                                            for (0..il_fc) |fi| {
                                                if (!std.mem.eql(u8, site.field_names[fi], il_field_names[fi])) { same = false; break; }
                                            }
                                            if (same) { already_syn = true; break; }
                                        }
                                    }
                                }
                                if (!already_syn) {
                                    const syn_name = std.fmt.allocPrint(allocator, "__inline_{d}", .{alloc_sites.items.len}) catch continue;
                                    var syn_site = AllocSite{
                                        .name = syn_name,
                                        .line_num = 0,
                                        .field_names = .{""} ** 16,
                                        .field_count = il_fc,
                                    };
                                    for (0..il_fc) |fi| {
                                        syn_site.field_names[fi] = allocator.dupe(u8, il_field_names[fi]) catch "";
                                    }
                                    alloc_sites.append(allocator, syn_site) catch continue;
                                    std.debug.print("[soa] Pre-scan inline literal: .push({{ {d} fields: ", .{il_fc});
                                    for (0..il_fc) |fi| {
                                        if (fi > 0) std.debug.print(", ", .{});
                                        std.debug.print("{s}", .{il_field_names[fi]});
                                    }
                                    std.debug.print(" }}) → {s}\n", .{syn_name});
                                }
                            }
                        }
                    }

                    // When no WASM trampolines, SOA can still help via factory-level SOA:
                    // factories with ≥5 calls get SOA pools, field reads rewritten.
                    // Only skip if there are truly no alloc sites at all.
                    if (!any_trampolined and alloc_sites.items.len > 0) {
                        std.debug.print("[soa] No WASM trampolines, but {d} alloc sites available for factory-level SOA\n", .{alloc_sites.items.len});
                    } else if (!any_trampolined) {
                        std.debug.print("[soa] Skipped — no WASM trampolines and no alloc sites\n", .{});
                    }

                    // Generate SOA pool declarations for each alloc site shape
                    if (alloc_sites.items.len > 0) {
                        // Ensure WASM memory views exist (may not have been created if no WASM functions)
                        // Only emit if WASM is actually loaded (has trampolines)
                        if (!has_array_funcs and any_trampolined) {
                            w.writeAll("const __wbuf = __wasm ? __wasm.exports.memory.buffer : null;\n") catch { w_errs += 1; };
                            w.writeAll("const __m = __wasm ? new Int32Array(__wbuf) : null;\n") catch { w_errs += 1; };
                            w.writeAll(
                                \\let __sp = __wbuf.byteLength;
                                \\function __wasmStackSave() { return __sp; }
                                \\function __wasmStackRestore(p) { __sp = p; }
                                \\function __wasmStackAlloc(n) { __sp = (__sp - n) & ~7; return __sp; }
                                \\
                            ) catch { w_errs += 1; };
                        }
                        // Global per-field SOA columns: one array per unique field name
                        // across ALL alloc sites. Single __idx counter shared by all factories.
                        // This enables read-site rewrite: node.kind → __col_kind[node.__idx]
                        w.writeAll("let __col_idx = 0;\n") catch { w_errs += 1; };
                        {
                            // Collect unique field names
                            var unique_fields: [256][]const u8 = undefined;
                            var unique_count: usize = 0;
                            for (alloc_sites.items) |site| {
                                for (site.field_names[0..site.field_count]) |fname| {
                                    if (fname.len == 0) continue;
                                    var valid = true;
                                    for (fname) |c| {
                                        if (!isIdentChar(c)) { valid = false; break; }
                                    }
                                    if (!valid) continue;
                                    // Deduplicate
                                    var found = false;
                                    for (unique_fields[0..unique_count]) |existing| {
                                        if (std.mem.eql(u8, existing, fname)) { found = true; break; }
                                    }
                                    if (!found and unique_count < unique_fields.len) {
                                        unique_fields[unique_count] = fname;
                                        unique_count += 1;
                                    }
                                }
                            }
                            for (unique_fields[0..unique_count]) |fname| {
                                w.print("const __col_{s} = [];\n", .{fname}) catch { w_errs += 1; };
                                col_set.put(fname, {}) catch { w_errs += 1; };
                            }
                            std.debug.print("[soa] {d} global columns for {d} alloc sites\n", .{ unique_count, alloc_sites.items.len });
                        }
                        // Declare soa_base variables at module scope (as var, not const)
                        // so they're accessible across function boundaries.
                        // They get assigned at the `VAR = []` declaration site in the char_loop.
                        // This must happen AFTER provenance detection runs (below).
                        // We defer this declaration until we know which provenance entries exist.
                        // Compute polyfill line offset: patches use full-bundle line numbers
                        // but cc (clean_content) doesn't include polyfill lines
                        var bundle_line_count: u32 = 1;
                        if (alloc_manifest) |am| {
                            // Count lines in the full bundle by reading it
                            var bpath_buf: [4096]u8 = undefined;
                            const bpath = std.fmt.bufPrint(&bpath_buf, "{s}/bundle.js", .{cache_dir}) catch null;
                            if (bpath) |bp| {
                                if (std.fs.cwd().readFileAlloc(allocator, bp, 50 * 1024 * 1024)) |bundle_src| {
                                    defer allocator.free(bundle_src);
                                    for (bundle_src) |c| {
                                        if (c == '\n') bundle_line_count += 1;
                                    }
                                } else |_| {}
                            }
                            _ = am;
                        }
                        var clean_line_count: u32 = 1;
                        if (clean_content) |ccc| {
                            for (ccc) |c| {
                                if (c == '\n') clean_line_count += 1;
                            }
                        }
                        const line_offset: i32 = @as(i32, @intCast(bundle_line_count)) - @as(i32, @intCast(clean_line_count));
                        std.debug.print("[soa] Line offset: {d} (bundle={d}, clean={d})\n", .{ line_offset, bundle_line_count, clean_line_count });

                        // Filter patches by col_set and apply line offset
                        for (patches.items) |pe| {
                            if (!col_set.contains(pe.field)) continue;
                            const adjusted_line = @as(i32, @intCast(pe.line)) - line_offset;
                            if (adjusted_line < 1) continue;
                            const line: u32 = @intCast(adjusted_line);
                            patch_lines.put(line, {}) catch { w_errs += 1; };
                            var fh: u64 = 0;
                            for (pe.field) |c| fh = fh *% 31 + c;
                            patch_set.put(line *% 1000003 + fh, pe.field) catch { w_errs += 1; };
                        }
                        std.debug.print("[soa] {d} patches active ({d} lines) after col_set + offset filter\n", .{ patch_set.count(), patch_lines.count() });
                    }

                    // Batch struct helpers: for each struct function with _batch variant,
                    // generate a JS function that copies array fields into WASM pool and
                    // calls the batched WASM function in a single invocation.
                    // Identity cache: if same array ref on repeated calls, skip the copy
                    // (pool persists in WASM stack memory across calls).
                    for (wasm_func_list.items, 0..) |wfe, wfi| {
                        if (!any_trampolined) break; // skip all batch helpers when no WASM
                        if (wfe.struct_args == 0) continue;
                        // Find first struct arg and its fields
                        var sai: u32 = 0;
                        var field_count: u8 = 0;
                        while (sai < 8) : (sai += 1) {
                            if (wfe.struct_args & (@as(u8, 1) << @intCast(sai)) != 0) {
                                field_count = wfe.struct_field_counts[sai];
                                break;
                            }
                        }
                        if (field_count == 0) continue;
                        const stride = @as(u32, field_count);

                        // Per-function identity cache variables
                        w.print("let __bp_{d}_arr = null, __bp_{d}_pool = 0, __bp_{d}_sp = 0;\n", .{ wfi, wfi, wfi }) catch { w_errs += 1; };
                        w.print("function __batch_{d}(arr", .{wfi}) catch { w_errs += 1; };
                        // Scalar params (non-struct args)
                        {
                            var pi: u32 = 0;
                            while (pi < wfe.arg_count) : (pi += 1) {
                                if (wfe.struct_args & (@as(u8, 1) << @intCast(pi)) == 0) {
                                    w.print(", __s{d}", .{pi}) catch { w_errs += 1; };
                                }
                            }
                        }
                        w.writeAll(") {\n") catch { w_errs += 1; };
                        // SOA fast path: if elements are SOA handles, data is already in WASM memory
                        if (alloc_sites.items.len > 0) {
                            // Match batch function's struct fields against SOA sites
                            for (alloc_sites.items, 0..) |site, si| {
                                // Check if the first struct field matches an SOA field
                                const first_field = wfe.struct_field_names[sai][0];
                                if (first_field.len == 0) continue;
                                var soa_match = false;
                                for (site.field_names[0..site.field_count]) |sf| {
                                    if (std.mem.eql(u8, sf, first_field)) {
                                        soa_match = true;
                                        break;
                                    }
                                }
                                if (!soa_match) continue;

                                w.print("  if (arr.__soa === {d}) {{\n", .{si}) catch { w_errs += 1; };
                                if (stride == 1) {
                                    // Zero-copy: pass SOA array pointer directly
                                    w.print("    return __wasm.exports.{s}_batch(__soa_{d}_{s}.byteOffset, arr.length", .{ wfe.name, si, first_field }) catch { w_errs += 1; };
                                } else {
                                    // Multi-field: read from contiguous SOA arrays (cache-friendly) into interleaved pool
                                    w.writeAll("    const __n = arr.length;\n") catch { w_errs += 1; };
                                    w.print("    const __p = __wasmStackAlloc(__n * {d});\n", .{stride * 4}) catch { w_errs += 1; };
                                    w.writeAll("    const __b = __p >> 2;\n") catch { w_errs += 1; };
                                    w.writeAll("    for (let __i = 0; __i < __n; __i++) {\n") catch { w_errs += 1; };
                                    for (wfe.struct_field_names[sai][0..field_count], 0..) |fname, fi| {
                                        if (fname.len == 0) continue;
                                        // Read field from plain object (returned by SOA factory)
                                        w.print("      __m[__b + __i * {d} + {d}] = arr[__i].{s};\n", .{ stride, fi, fname }) catch { w_errs += 1; };
                                    }
                                    w.writeAll("    }\n") catch { w_errs += 1; };
                                    w.print("    return __wasm.exports.{s}_batch(__p, __n", .{wfe.name}) catch { w_errs += 1; };
                                }
                                // Append scalar params
                                {
                                    var pi2: u32 = 0;
                                    while (pi2 < wfe.arg_count) : (pi2 += 1) {
                                        if (wfe.struct_args & (@as(u8, 1) << @intCast(pi2)) == 0) {
                                            w.print(", __s{d}", .{pi2}) catch { w_errs += 1; };
                                        }
                                    }
                                }
                                w.writeAll(");\n  }\n") catch { w_errs += 1; };
                                break; // use first matching SOA site
                            }
                        }
                        // Identity cache fast path: same array ref → zero-copy
                        w.print("  if (arr === __bp_{d}_arr) return __wasm.exports.{s}_batch(__bp_{d}_pool, arr.length", .{ wfi, wfe.name, wfi }) catch { w_errs += 1; };
                        {
                            var pi: u32 = 0;
                            while (pi < wfe.arg_count) : (pi += 1) {
                                if (wfe.struct_args & (@as(u8, 1) << @intCast(pi)) == 0) {
                                    w.print(", __s{d}", .{pi}) catch { w_errs += 1; };
                                }
                            }
                        }
                        w.writeAll(");\n") catch { w_errs += 1; };
                        // Cache miss: free old pool, allocate new, copy fields
                        w.print("  if (__bp_{d}_arr !== null) __wasmStackRestore(__bp_{d}_sp);\n", .{ wfi, wfi }) catch { w_errs += 1; };
                        w.print("  __bp_{d}_sp = __wasmStackSave();\n", .{wfi}) catch { w_errs += 1; };
                        w.writeAll("  const __n = arr.length;\n") catch { w_errs += 1; };
                        w.print("  __bp_{d}_pool = __wasmStackAlloc(__n * {d});\n", .{ wfi, stride * 4 }) catch { w_errs += 1; };
                        w.print("  const __base = __bp_{d}_pool >> 2;\n", .{wfi}) catch { w_errs += 1; };
                        // Copy loop
                        w.writeAll("  for (let __i = 0; __i < __n; __i++) {\n") catch { w_errs += 1; };
                        for (wfe.struct_field_names[sai][0..field_count], 0..) |fname, fi| {
                            if (fname.len == 0) continue;
                            if (stride == 1) {
                                w.print("    __m[__base + __i] = arr[__i].{s};\n", .{fname}) catch { w_errs += 1; };
                            } else {
                                w.print("    __m[__base + __i * {d} + {d}] = arr[__i].{s};\n", .{stride, fi, fname}) catch { w_errs += 1; };
                            }
                        }
                        w.writeAll("  }\n") catch { w_errs += 1; };
                        w.print("  __bp_{d}_arr = arr;\n", .{wfi}) catch { w_errs += 1; };
                        // Call batch WASM
                        w.print("  return __wasm.exports.{s}_batch(__bp_{d}_pool, __n", .{ wfe.name, wfi }) catch { w_errs += 1; };
                        {
                            var pi: u32 = 0;
                            while (pi < wfe.arg_count) : (pi += 1) {
                                if (wfe.struct_args & (@as(u8, 1) << @intCast(pi)) == 0) {
                                    w.print(", __s{d}", .{pi}) catch { w_errs += 1; };
                                }
                            }
                        }
                        w.writeAll(");\n}\n") catch { w_errs += 1; };
                    }

                    w.writeAll("\n") catch { w_errs += 1; };

                    // Source transform: copy original source, but replace function bodies
                    // for WASM-compiled functions with WASM call trampolines.
                    // e.g. `function fib(n) { ... }` → `function fib(n) { return __wasm.exports.fib(n); }`
                    // Also handles arrow functions: `var adler32 = (a, b) => { ... }` → trampoline
                    if (clean_content) |cc| {
                        // Compute preamble line offset: hooked bundle has polyfills prepended.
                        // Module parser line numbers are based on hooked bundle.
                        // Find the first line of clean content in hooked bundle to get offset.
                        var preamble_lines: u32 = 0;
                        {
                            var hooked_path_buf2: [4096]u8 = undefined;
                            const hooked_path2 = std.fmt.bufPrint(&hooked_path_buf2, "{s}/bundle_hooked.js", .{cache_dir}) catch null;
                            if (hooked_path2) |hp| {
                                if (std.fs.cwd().readFileAlloc(allocator, hp, 50 * 1024 * 1024)) |hc| {
                                    defer allocator.free(hc);
                                    // Find where clean content starts in hooked (match first 80 chars)
                                    const match_len = @min(cc.len, 80);
                                    if (match_len > 0) {
                                        if (std.mem.indexOf(u8, hc, cc[0..match_len])) |start_offset| {
                                            // Count newlines before start_offset
                                            for (hc[0..start_offset]) |ch| {
                                                if (ch == '\n') preamble_lines += 1;
                                            }
                                        }
                                    }
                                } else |_| {}
                            }
                        }

                        // Build line→offset table for anonymous function matching
                        var line_offsets: [8192]u32 = undefined;
                        var line_count: u32 = 1;
                        line_offsets[0] = 0; // line 1 starts at offset 0
                        for (cc, 0..) |ch, ci| {
                            if (ch == '\n' and line_count < line_offsets.len) {
                                line_offsets[line_count] = @intCast(ci + 1);
                                line_count += 1;
                            }
                        }

                        // Pre-compute anonymous function replacement positions
                        // For each anonymous WASM function, find its definition on the corresponding line:
                        //   1. Arrow functions: `=> {`
                        //   2. Shorthand methods: `name(params) {` (class/object methods)
                        // Adjust line_num by preamble offset (hooked → clean)
                        const ArrowReplace = struct { brace_pos: u32, func_idx: u16 };
                        var arrow_replacements: [64]ArrowReplace = undefined;
                        var arrow_count: usize = 0;
                        for (wasm_func_list.items, 0..) |wf_entry, wfi| {
                            if (!wf_entry.is_anon) continue;
                            const adjusted_line = if (wf_entry.line_num > preamble_lines) wf_entry.line_num - preamble_lines else wf_entry.line_num;
                            if (adjusted_line == 0 or adjusted_line >= line_count) continue;
                            const line_start = line_offsets[adjusted_line - 1]; // 1-based
                            const line_end = if (adjusted_line < line_count) line_offsets[adjusted_line] else @as(u32, @intCast(cc.len));
                            // Try arrow: `=> {`
                            if (std.mem.indexOf(u8, cc[line_start..line_end], "=> {")) |arrow_off| {
                                const brace_pos = line_start + @as(u32, @intCast(arrow_off)) + 3; // position of `{`
                                if (arrow_count < arrow_replacements.len) {
                                    arrow_replacements[arrow_count] = .{ .brace_pos = brace_pos, .func_idx = @intCast(wfi) };
                                    arrow_count += 1;
                                }
                            } else {
                                // Try shorthand method: `name(params) {` — scan for `identifier(` followed by `) {`
                                // Skip leading whitespace to find the method name
                                const line = cc[line_start..line_end];
                                var pos: usize = 0;
                                while (pos < line.len and (line[pos] == ' ' or line[pos] == '\t')) pos += 1;
                                // Check for `async ` prefix
                                if (pos + 6 < line.len and std.mem.eql(u8, line[pos .. pos + 6], "async ")) pos += 6;
                                // Now pos should be at the start of the method name
                                const name_start = pos;
                                while (pos < line.len and isIdentChar(line[pos])) pos += 1;
                                if (pos > name_start and pos < line.len and line[pos] == '(') {
                                    // Found `name(` — scan to closing `)` then check for `{`
                                    var pdepth: i32 = 1;
                                    var scan = pos + 1;
                                    while (scan < line.len and pdepth > 0) : (scan += 1) {
                                        if (line[scan] == '(') pdepth += 1;
                                        if (line[scan] == ')') pdepth -= 1;
                                    }
                                    // Skip whitespace after `)`, check for `{`
                                    while (scan < line.len and (line[scan] == ' ' or line[scan] == '\n' or line[scan] == '\r' or line[scan] == '\t')) scan += 1;
                                    if (scan < line.len and line[scan] == '{') {
                                        // Exclude `function ` prefix (named functions handled elsewhere)
                                        const abs_name_start = line_start + @as(u32, @intCast(name_start));
                                        const has_fn_prefix = abs_name_start >= 9 and std.mem.eql(u8, cc[abs_name_start - 9 .. abs_name_start], "function ");
                                        if (!has_fn_prefix) {
                                            const brace_pos = line_start + @as(u32, @intCast(scan));
                                            if (arrow_count < arrow_replacements.len) {
                                                arrow_replacements[arrow_count] = .{ .brace_pos = brace_pos, .func_idx = @intCast(wfi) };
                                                arrow_count += 1;
                                            }
                                        }
                                    }
                                }
                            }
                        }

                        // === Local provenance tracking (expanded) ===
                        // Pre-scan for object allocation patterns to find which containers
                        // are filled exclusively from one SOA factory. Detected patterns:
                        //   1. VAR.push(FACTORY(        — array push (original)
                        //   2. VAR.set(KEY, FACTORY(    — Map.set (new)
                        //   3. VAR[EXPR] = FACTORY(     — direct index assignment (new)
                        //   4. VAR.push(new CTOR(       — constructor via push (new)
                        //   5. VAR.set(KEY, new CTOR(   — constructor via Map.set (new)
                        // Then at read sites, transform `VAR[expr].field` or `VAR.get(key).field`
                        // → `__soa_N_field[idx]` for flat typed-array access.
                        const ProvEntry = struct { var_name: []const u8, site_idx: usize, decl_end: usize, raw_index: bool };
                        var provenance_buf: [64]ProvEntry = undefined;
                        var prov_count: usize = 0;
                        if (alloc_sites.items.len > 0) {
                            // Helper: try to match a factory/constructor name and record provenance
                            const matchAndRecord = struct {
                                fn match(
                                    cc_slice: []const u8,
                                    start: usize,
                                    vn: []const u8,
                                    sites: []const AllocSite,
                                    buf: *[64]ProvEntry,
                                    count: *usize,
                                ) void {
                                    var pos = start;
                                    // Skip 'new ' prefix if present (constructor pattern)
                                    if (pos + 4 <= cc_slice.len and std.mem.startsWith(u8, cc_slice[pos..], "new ")) {
                                        pos += 4;
                                        while (pos < cc_slice.len and cc_slice[pos] == ' ') pos += 1;
                                    }
                                    // Read factory/constructor name
                                    var fe = pos;
                                    while (fe < cc_slice.len and isIdentChar(cc_slice[fe])) fe += 1;
                                    if (fe == pos or fe >= cc_slice.len or cc_slice[fe] != '(') return;
                                    const fn_name = cc_slice[pos..fe];
                                    // Match against alloc sites
                                    for (sites, 0..) |site, si| {
                                        if (std.mem.eql(u8, site.name, fn_name)) {
                                            // Deduplicate by variable name
                                            var dup = false;
                                            for (buf.*[0..count.*]) |p| {
                                                if (std.mem.eql(u8, p.var_name, vn)) { dup = true; break; }
                                            }
                                            if (!dup and count.* < buf.len) {
                                                buf.*[count.*] = .{ .var_name = vn, .site_idx = si, .decl_end = 0, .raw_index = false };
                                                count.* += 1;
                                            }
                                            break;
                                        }
                                    }
                                }
                            }.match;

                            var sp: usize = 0;
                            while (sp < cc.len) : (sp += 1) {
                                // Pattern 1: VAR.push(FACTORY( or VAR.push(new CTOR(
                                if (sp + 6 < cc.len and std.mem.startsWith(u8, cc[sp..], ".push(")) {
                                    const ve = sp;
                                    var vs = ve;
                                    while (vs > 0 and isIdentChar(cc[vs - 1])) vs -= 1;
                                    if (vs < ve) {
                                        const vn = cc[vs..ve];
                                        matchAndRecord(cc, sp + 6, vn, alloc_sites.items, &provenance_buf, &prov_count);
                                    }
                                    continue;
                                }
                                // Pattern 2: VAR.set(KEY, FACTORY( or VAR.set(KEY, new CTOR(
                                if (sp + 5 < cc.len and std.mem.startsWith(u8, cc[sp..], ".set(")) {
                                    const ve = sp;
                                    var vs = ve;
                                    while (vs > 0 and isIdentChar(cc[vs - 1])) vs -= 1;
                                    if (vs < ve) {
                                        const vn = cc[vs..ve];
                                        // Skip past the key argument: find ', ' after first arg
                                        var kp = sp + 5;
                                        var depth_s: u32 = 1;
                                        while (kp < cc.len and depth_s > 0) : (kp += 1) {
                                            if (cc[kp] == '(') depth_s += 1;
                                            if (cc[kp] == ')') depth_s -= 1;
                                            if (depth_s == 1 and cc[kp] == ',') break;
                                        }
                                        if (kp < cc.len and cc[kp] == ',') {
                                            kp += 1;
                                            while (kp < cc.len and cc[kp] == ' ') kp += 1;
                                            matchAndRecord(cc, kp, vn, alloc_sites.items, &provenance_buf, &prov_count);
                                        }
                                    }
                                    continue;
                                }
                                // Pattern 3: VAR[EXPR] = FACTORY( or VAR[EXPR] = new CTOR(
                                // Look for '] = ' followed by factory name
                                if (cc[sp] == ']' and sp + 3 < cc.len) {
                                    var eq = sp + 1;
                                    while (eq < cc.len and cc[eq] == ' ') eq += 1;
                                    if (eq < cc.len and cc[eq] == '=' and eq + 1 < cc.len and cc[eq + 1] != '=') {
                                        eq += 1;
                                        while (eq < cc.len and cc[eq] == ' ') eq += 1;
                                        // Find the variable name before '['
                                        var bk = sp;
                                        // Walk back past the bracket expression
                                        var bdepth: u32 = 1;
                                        if (bk > 0) bk -= 1; // skip ']'
                                        while (bk > 0 and bdepth > 0) : (bk -= 1) {
                                            if (cc[bk] == ']') bdepth += 1;
                                            if (cc[bk] == '[') bdepth -= 1;
                                        }
                                        // bk now points at '[', variable name is before it
                                        if (bk < cc.len and cc[bk] == '[') {
                                            const ve2 = bk;
                                            var vs2 = ve2;
                                            while (vs2 > 0 and isIdentChar(cc[vs2 - 1])) vs2 -= 1;
                                            if (vs2 < ve2) {
                                                const vn = cc[vs2..ve2];
                                                matchAndRecord(cc, eq, vn, alloc_sites.items, &provenance_buf, &prov_count);
                                            }
                                        }
                                    }
                                }
                            }
                        }
                        if (prov_count > 0) {
                            std.debug.print("[soa] Provenance: {d} containers from {d} alloc sites\n", .{ prov_count, alloc_sites.items.len });
                        }
                        // === Inline object literal provenance (runs independently of alloc sites) ===
                        // Scan source for VAR.push({ key: val, ... }) patterns and create synthetic alloc sites.
                        // This runs even when bytecode analysis found no named factory functions.
                        {
                            var isp: usize = 0;
                            while (isp + 7 < cc.len) : (isp += 1) {
                                if (!std.mem.startsWith(u8, cc[isp..], ".push(")) continue;
                                // Check for '{' after .push(
                                var lp2 = isp + 6;
                                while (lp2 < cc.len and cc[lp2] == ' ') lp2 += 1;
                                if (lp2 >= cc.len or cc[lp2] != '{') continue;
                                // Extract variable name before .push(
                                const ve2 = isp;
                                var vs2 = ve2;
                                while (vs2 > 0 and isIdentChar(cc[vs2 - 1])) vs2 -= 1;
                                if (vs2 >= ve2) continue;
                                const vn2 = cc[vs2..ve2];
                                // Skip if already has provenance for this variable
                                var already = false;
                                for (provenance_buf[0..prov_count]) |p| {
                                    if (std.mem.eql(u8, p.var_name, vn2)) { already = true; break; }
                                }
                                if (already) continue;
                                // Extract field names from { key: val, key: val, ... }
                                var il_field_names: [16][]const u8 = .{""} ** 16;
                                var il_fc: u8 = 0;
                                var il_fp = lp2 + 1;
                                var il_depth: u32 = 1;
                                while (il_fp < cc.len and il_depth > 0) {
                                    if (cc[il_fp] == '{') { il_depth += 1; il_fp += 1; continue; }
                                    if (cc[il_fp] == '}') { il_depth -= 1; il_fp += 1; continue; }
                                    if (il_depth > 1) { il_fp += 1; continue; }
                                    while (il_fp < cc.len and (cc[il_fp] == ' ' or cc[il_fp] == '\n' or cc[il_fp] == '\r')) il_fp += 1;
                                    if (il_fp >= cc.len or cc[il_fp] == '}') continue;
                                    const il_fks = il_fp;
                                    while (il_fp < cc.len and isIdentChar(cc[il_fp])) il_fp += 1;
                                    if (il_fp > il_fks and il_fp < cc.len) {
                                        var il_cp = il_fp;
                                        while (il_cp < cc.len and cc[il_cp] == ' ') il_cp += 1;
                                        if (il_cp < cc.len and cc[il_cp] == ':' and (il_cp + 1 >= cc.len or cc[il_cp + 1] != ':')) {
                                            if (il_fc < 16) {
                                                il_field_names[il_fc] = cc[il_fks..il_fp];
                                                il_fc += 1;
                                            }
                                            il_fp = il_cp + 1;
                                            var il_vd: u32 = 0;
                                            while (il_fp < cc.len) {
                                                if (cc[il_fp] == '(' or cc[il_fp] == '[' or cc[il_fp] == '{') { il_vd += 1; il_fp += 1; continue; }
                                                if (cc[il_fp] == ')' or cc[il_fp] == ']') { if (il_vd > 0) il_vd -= 1; il_fp += 1; continue; }
                                                if (cc[il_fp] == '}') { if (il_vd > 0) { il_vd -= 1; il_fp += 1; continue; } break; }
                                                if (cc[il_fp] == ',' and il_vd == 0) { il_fp += 1; break; }
                                                il_fp += 1;
                                            }
                                            continue;
                                        }
                                    }
                                    while (il_fp < cc.len and cc[il_fp] != ',' and cc[il_fp] != '}') il_fp += 1;
                                    if (il_fp < cc.len and cc[il_fp] == ',') il_fp += 1;
                                }
                                if (il_fc >= 3 and prov_count < provenance_buf.len) {
                                    // Find existing __inline_ alloc site with matching fields (created by pre-scan)
                                    var existing_si: ?usize = null;
                                    for (alloc_sites.items, 0..) |site, si| {
                                        if (!std.mem.startsWith(u8, site.name, "__inline_")) continue;
                                        if (site.field_count != il_fc) continue;
                                        var same = true;
                                        for (0..il_fc) |fi| {
                                            if (!std.mem.eql(u8, site.field_names[fi], il_field_names[fi])) { same = false; break; }
                                        }
                                        if (same) { existing_si = si; break; }
                                    }
                                    const si = existing_si orelse blk: {
                                        // No pre-scan match — create new (shouldn't happen normally)
                                        const syn_name = std.fmt.allocPrint(allocator, "__inline_{d}", .{alloc_sites.items.len}) catch continue;
                                        var syn_site = AllocSite{
                                            .name = syn_name,
                                            .line_num = 0,
                                            .field_names = .{""} ** 16,
                                            .field_count = il_fc,
                                        };
                                        for (0..il_fc) |fi| {
                                            syn_site.field_names[fi] = allocator.dupe(u8, il_field_names[fi]) catch "";
                                        }
                                        alloc_sites.append(allocator, syn_site) catch continue;
                                        break :blk alloc_sites.items.len - 1;
                                    };
                                    provenance_buf[prov_count] = .{
                                        .var_name = vn2,
                                        .site_idx = si,
                                        .decl_end = 0,
                                        .raw_index = false,
                                    };
                                    prov_count += 1;
                                    std.debug.print("[soa] Inline literal provenance: {s} → site {d}\n", .{ vn2, si });
                                }
                            }
                        }
                        // Find `VAR = []` declarations for each provenance entry (index alignment)
                        {
                            var pi: usize = 0;
                            while (pi < prov_count) : (pi += 1) {
                                const vn = provenance_buf[pi].var_name;
                                var dp: usize = 0;
                                while (dp + vn.len + 3 < cc.len) : (dp += 1) {
                                    if (dp > 0 and isIdentChar(cc[dp - 1])) continue;
                                    if (!std.mem.startsWith(u8, cc[dp..], vn)) continue;
                                    var ep = dp + vn.len;
                                    while (ep < cc.len and cc[ep] == ' ') ep += 1;
                                    if (ep >= cc.len or cc[ep] != '=') continue;
                                    // Skip if '=' is part of '==' or '=>'
                                    if (ep + 1 < cc.len and (cc[ep + 1] == '=' or cc[ep + 1] == '>')) continue;
                                    ep += 1;
                                    while (ep < cc.len and cc[ep] == ' ') ep += 1;
                                    if (ep + 1 < cc.len and cc[ep] == '[' and cc[ep + 1] == ']') {
                                        // Verify statement context: check for '(' or '??' before VAR
                                        // Skip if inside an expression like `x ?? (VAR = [])`
                                        var ctx = dp;
                                        while (ctx > 0 and cc[ctx - 1] == ' ') ctx -= 1;
                                        if (ctx > 0 and cc[ctx - 1] == '(') continue;
                                        provenance_buf[pi].decl_end = ep + 2;
                                        std.debug.print("[soa] decl_end: '{s}' = [] at pos {d}\n", .{ vn, ep + 2 });
                                        break;
                                    }
                                }
                            }
                        }
                        // Check raw index eligibility: every VAR[expr] must be followed by .field
                        {
                            var pi: usize = 0;
                            while (pi < prov_count) : (pi += 1) {
                                if (provenance_buf[pi].decl_end == 0) continue;
                                provenance_buf[pi].raw_index = true; // assume eligible
                                const vn = provenance_buf[pi].var_name;
                                const site = alloc_sites.items[provenance_buf[pi].site_idx];
                                var sp2: usize = 0;
                                while (sp2 + vn.len + 1 < cc.len) : (sp2 += 1) {
                                    if (sp2 > 0 and isIdentChar(cc[sp2 - 1])) continue;
                                    if (!std.mem.startsWith(u8, cc[sp2..], vn)) continue;
                                    const after = sp2 + vn.len;
                                    if (after >= cc.len or cc[after] != '[') continue;
                                    // Found VAR[ — find matching ]
                                    var depth2: u32 = 1;
                                    var k2 = after + 1;
                                    while (k2 < cc.len and depth2 > 0) : (k2 += 1) {
                                        if (cc[k2] == '[') depth2 += 1;
                                        if (cc[k2] == ']') depth2 -= 1;
                                    }
                                    if (k2 >= cc.len or cc[k2] != '.') {
                                        // Before rejecting, check if this VAR[expr] is the RHS
                                        // of `const/var/let ALIAS = VAR[expr]` — alias pattern.
                                        // If so, the field accesses happen through the alias,
                                        // not directly after the bracket.
                                        var is_alias_rhs = false;
                                        {
                                            // Scan backwards from VAR[ to find `= ` then identifier then keyword
                                            var bk2 = sp2;
                                            while (bk2 > 0 and cc[bk2 - 1] == ' ') bk2 -= 1;
                                            if (bk2 > 0 and cc[bk2 - 1] == '=') {
                                                // Check it's not == or =>
                                                if (bk2 >= 2 and (cc[bk2 - 2] == '=' or cc[bk2 - 2] == '!' or cc[bk2 - 2] == '<' or cc[bk2 - 2] == '>')) {
                                                    // comparison operator — not alias
                                                } else {
                                                    bk2 -= 1; // past '='
                                                    while (bk2 > 0 and cc[bk2 - 1] == ' ') bk2 -= 1;
                                                    // Should be at end of identifier
                                                    const id_e = bk2;
                                                    while (bk2 > 0 and isIdentChar(cc[bk2 - 1])) bk2 -= 1;
                                                    if (bk2 < id_e) {
                                                        // Found identifier — check for keyword before it
                                                        var kw_e = bk2;
                                                        while (kw_e > 0 and cc[kw_e - 1] == ' ') kw_e -= 1;
                                                        if (kw_e >= 4 and std.mem.eql(u8, cc[kw_e - 4 .. kw_e], "let ")) {
                                                            is_alias_rhs = true;
                                                        } else if (kw_e >= 4 and std.mem.eql(u8, cc[kw_e - 4 .. kw_e], "var ")) {
                                                            is_alias_rhs = true;
                                                        } else if (kw_e >= 6 and std.mem.eql(u8, cc[kw_e - 6 .. kw_e], "const ")) {
                                                            is_alias_rhs = true;
                                                        }
                                                    }
                                                }
                                            }
                                        }
                                        if (!is_alias_rhs) {
                                            provenance_buf[pi].raw_index = false;
                                            break;
                                        }
                                        // Alias RHS — skip this occurrence (field access validated separately)
                                        continue;
                                    }
                                    const fstart2 = k2 + 1;
                                    var fend2 = fstart2;
                                    while (fend2 < cc.len and isIdentChar(cc[fend2])) fend2 += 1;
                                    if (fend2 == fstart2) {
                                        provenance_buf[pi].raw_index = false;
                                        break;
                                    }
                                    const fname2 = cc[fstart2..fend2];
                                    var is_soa = false;
                                    for (site.field_names[0..site.field_count]) |sf| {
                                        if (std.mem.eql(u8, sf, fname2)) { is_soa = true; break; }
                                    }
                                    if (!is_soa) {
                                        provenance_buf[pi].raw_index = false;
                                        break;
                                    }
                                }
                            }
                        }
                        // === Factory-level SOA (expanded) ===
                        // For factories with ≥5 total call sites (regardless of container),
                        // activate SOA directly. This handles the dominant pattern in real code:
                        //   var x = createType(flags, name);  // → SOA pool allocation
                        //   ... x.flags ...                   // → __soa_N_flags[x.__idx]
                        // No container provenance needed — every object from the factory
                        // goes into the SOA pool, and field reads are rewritten universally.
                        if (prov_count == 0 and alloc_sites.items.len > 0) {
                            const MIN_FACTORY_CALLS: usize = 5;
                            for (alloc_sites.items, 0..) |site, si| {
                                if (site.field_count < 3 or site.name.len == 0) continue;
                                // Count total call sites in source: `factoryName(`
                                var call_count: usize = 0;
                                var scan: usize = 0;
                                while (scan + site.name.len + 1 < cc.len) : (scan += 1) {
                                    if (scan > 0 and isIdentChar(cc[scan - 1])) continue;
                                    if (!std.mem.startsWith(u8, cc[scan..], site.name)) continue;
                                    const after_name = scan + site.name.len;
                                    if (after_name < cc.len and cc[after_name] == '(') {
                                        call_count += 1;
                                        if (call_count >= MIN_FACTORY_CALLS) break;
                                    }
                                }
                                if (call_count >= MIN_FACTORY_CALLS and prov_count < provenance_buf.len) {
                                    // Create a synthetic provenance entry with a special marker name
                                    // "__factory_N" indicates this is factory-level SOA, not container SOA
                                    const marker = std.fmt.allocPrint(allocator, "__factory_{d}", .{si}) catch continue;
                                    provenance_buf[prov_count] = .{
                                        .var_name = marker,
                                        .site_idx = si,
                                        .decl_end = 0,
                                        .raw_index = false,
                                    };
                                    prov_count += 1;
                                    std.debug.print("[soa] Factory-level SOA: {s} ({d} calls, {d} fields)\n", .{
                                        site.name, call_count, site.field_count,
                                    });
                                }
                            }
                        }

                        const provenance = provenance_buf[0..prov_count];

                        // === Local variable alias detection ===
                        // Pre-scan for `const/var/let X = PROV_VAR[expr]` patterns.
                        // When ALL uses of X are `.field` reads (safe SOA fields), we can rewrite
                        // `X.field` → `__col_field[__PROV_soa_base + (expr)]` — raw column access.
                        // This captures the dominant real-world pattern:
                        //   for (let i = 0; i < arr.length; i++) {
                        //     const node = arr[i];    // ← alias assignment
                        //     if (node.kind === 3)    // ← alias read → __col_kind[base + i]
                        //       sum += node.symbolId; // ← alias read → __col_symbolId[base + i]
                        //   }
                        const AliasEntry = struct {
                            alias_name: []const u8, // local variable name (e.g. "node")
                            prov_idx: usize, // index into provenance[]
                            idx_expr: []const u8, // index expression between [] (e.g. "i")
                            assign_pos: usize, // position of the assignment in source
                            scope_start: usize, // position of enclosing '{' (for scope-aware matching)
                            scope_end: usize, // position of matching '}' (for scope-aware matching)
                        };
                        var alias_buf: [128]AliasEntry = undefined;
                        var alias_count: usize = 0;
                        // Build alias name→index map for O(1) lookup in char_loop
                        var alias_map = std.StringHashMapUnmanaged(usize){};
                        defer alias_map.deinit(allocator);
                        if (prov_count > 0) {
                            // Scan for: const/var/let IDENT = PROV_VAR[expr]
                            // where PROV_VAR has decl_end > 0 (index-aligned)
                            const keywords = [_][]const u8{ "const ", "var ", "let " };
                            var ap: usize = 0;
                            while (ap + 10 < cc.len) : (ap += 1) {
                                // Must be at a word boundary
                                if (ap > 0 and isIdentChar(cc[ap - 1])) continue;
                                // Match keyword
                                var kw_len: usize = 0;
                                for (keywords) |kw| {
                                    if (ap + kw.len < cc.len and std.mem.startsWith(u8, cc[ap..], kw)) {
                                        kw_len = kw.len;
                                        break;
                                    }
                                }
                                if (kw_len == 0) continue;
                                // Extract identifier after keyword
                                var id_start = ap + kw_len;
                                while (id_start < cc.len and cc[id_start] == ' ') id_start += 1;
                                var id_end = id_start;
                                while (id_end < cc.len and isIdentChar(cc[id_end])) id_end += 1;
                                if (id_end == id_start) continue;
                                const alias_name = cc[id_start..id_end];
                                // Skip if alias name starts with __ (internal variable)
                                if (alias_name.len >= 2 and alias_name[0] == '_' and alias_name[1] == '_') continue;
                                // Must be followed by ` = ` (not `==` or `=>`)
                                var eq_pos = id_end;
                                while (eq_pos < cc.len and cc[eq_pos] == ' ') eq_pos += 1;
                                if (eq_pos >= cc.len or cc[eq_pos] != '=') continue;
                                if (eq_pos + 1 < cc.len and (cc[eq_pos + 1] == '=' or cc[eq_pos + 1] == '>')) continue;
                                eq_pos += 1;
                                while (eq_pos < cc.len and cc[eq_pos] == ' ') eq_pos += 1;
                                // Now match PROV_VAR[expr]
                                var rhs_end = eq_pos;
                                while (rhs_end < cc.len and isIdentChar(cc[rhs_end])) rhs_end += 1;
                                if (rhs_end == eq_pos or rhs_end >= cc.len or cc[rhs_end] != '[') continue;
                                const rhs_name = cc[eq_pos..rhs_end];
                                // Match against provenance entries with decl_end > 0
                                var matched_pi: ?usize = null;
                                for (provenance, 0..) |prov, pi| {
                                    if (prov.decl_end == 0) continue;
                                    if (std.mem.eql(u8, prov.var_name, rhs_name)) {
                                        matched_pi = pi;
                                        break;
                                    }
                                }
                                if (matched_pi == null) continue;
                                const pi = matched_pi.?;
                                // Extract index expression between [ and ]
                                var bracket_depth: u32 = 1;
                                var bk = rhs_end + 1;
                                while (bk < cc.len and bracket_depth > 0) : (bk += 1) {
                                    if (cc[bk] == '[') bracket_depth += 1;
                                    if (cc[bk] == ']') bracket_depth -= 1;
                                }
                                // bk is past the ']'
                                if (bk == 0 or bk > cc.len) continue;
                                const idx_expr = cc[rhs_end + 1 .. bk - 1];
                                if (idx_expr.len == 0) continue;
                                // Check the rest of the statement: must end with ; or newline (not `.field` after])
                                var after_bracket = bk;
                                while (after_bracket < cc.len and cc[after_bracket] == ' ') after_bracket += 1;
                                if (after_bracket < cc.len and cc[after_bracket] == '.') continue; // e.g. `const x = arr[i].field` — not an alias
                                // === Scope-aware safety check ===
                                // Find the enclosing block: scan backwards from alias assignment to find
                                // the opening '{', then forward to find matching '}'. Only check uses
                                // within this scope — same name in other functions is a different variable.
                                var scope_start = ap;
                                var scope_end = cc.len;
                                {
                                    // Find enclosing '{' by counting brace depth backwards
                                    var brace_depth_back: i32 = 0;
                                    var scan_back = ap;
                                    while (scan_back > 0) {
                                        scan_back -= 1;
                                        if (cc[scan_back] == '}') brace_depth_back += 1;
                                        if (cc[scan_back] == '{') {
                                            if (brace_depth_back == 0) {
                                                scope_start = scan_back;
                                                break;
                                            }
                                            brace_depth_back -= 1;
                                        }
                                    }
                                    // Find matching '}' by counting brace depth forward from scope_start
                                    var brace_depth_fwd: u32 = 0;
                                    var scan_fwd = scope_start;
                                    while (scan_fwd < cc.len) : (scan_fwd += 1) {
                                        if (cc[scan_fwd] == '{') brace_depth_fwd += 1;
                                        if (cc[scan_fwd] == '}') {
                                            brace_depth_fwd -= 1;
                                            if (brace_depth_fwd == 0) {
                                                scope_end = scan_fwd;
                                                break;
                                            }
                                        }
                                    }
                                }
                                // Safety check: verify all uses of alias_name within scope are `.field` reads
                                const site = alloc_sites.items[provenance[pi].site_idx];
                                var safe = true;
                                var check_pos = bk; // start after the alias assignment
                                var in_string: u8 = 0; // 0 = not in string, '"'/'\'' = in that string
                                while (check_pos + alias_name.len < scope_end) : (check_pos += 1) {
                                    // Track string literals to avoid matching inside them
                                    if (in_string != 0) {
                                        if (cc[check_pos] == in_string and (check_pos == 0 or cc[check_pos - 1] != '\\')) {
                                            in_string = 0;
                                        }
                                        continue;
                                    }
                                    if (cc[check_pos] == '"' or cc[check_pos] == '\'') {
                                        in_string = cc[check_pos];
                                        continue;
                                    }
                                    if (check_pos > 0 and isIdentChar(cc[check_pos - 1])) continue;
                                    if (!std.mem.startsWith(u8, cc[check_pos..], alias_name)) continue;
                                    const after_alias = check_pos + alias_name.len;
                                    if (after_alias >= cc.len) continue;
                                    // Check what follows the alias name
                                    if (isIdentChar(cc[after_alias])) continue; // part of longer name (e.g. nodeName)
                                    if (cc[after_alias] == '.') {
                                        // Field access: alias.field — check it's a safe SOA read
                                        const fs2 = after_alias + 1;
                                        var fe2 = fs2;
                                        while (fe2 < cc.len and isIdentChar(cc[fe2])) fe2 += 1;
                                        if (fe2 == fs2) { safe = false; break; } // bare dot (syntax error)
                                        const fn2 = cc[fs2..fe2];
                                        // Reject method calls: alias.method(
                                        if (fe2 < cc.len and cc[fe2] == '(') { safe = false; break; }
                                        // Reject field assignment: alias.field = value
                                        var eq2 = fe2;
                                        while (eq2 < cc.len and cc[eq2] == ' ') eq2 += 1;
                                        if (eq2 < cc.len and cc[eq2] == '=' and (eq2 + 1 >= cc.len or cc[eq2 + 1] != '=')) { safe = false; break; }
                                        // Reject compound assignment: alias.field += / |= / etc
                                        if (eq2 + 1 < cc.len and cc[eq2 + 1] == '=' and (cc[eq2] == '+' or cc[eq2] == '-' or cc[eq2] == '|' or cc[eq2] == '&' or cc[eq2] == '*')) { safe = false; break; }
                                        // Reject increment/decrement: alias.field++ / alias.field--
                                        if (eq2 + 1 < cc.len and ((cc[eq2] == '+' and cc[eq2 + 1] == '+') or (cc[eq2] == '-' and cc[eq2 + 1] == '-'))) { safe = false; break; }
                                        // Verify field is a known SOA field
                                        var is_soa_field = false;
                                        for (site.field_names[0..site.field_count]) |sf| {
                                            if (std.mem.eql(u8, sf, fn2)) { is_soa_field = true; break; }
                                        }
                                        if (!is_soa_field) { safe = false; break; }
                                        // This use is safe — skip past it
                                        check_pos = fe2;
                                        continue;
                                    } else if (cc[after_alias] == '=' and (after_alias + 1 >= cc.len or cc[after_alias + 1] != '=')) {
                                        // Reassignment: alias = ... — NOT safe (changes what the name refers to)
                                        safe = false;
                                        break;
                                    } else if (cc[after_alias] == '[') {
                                        // Subscript: alias[expr] — not a safe .field pattern
                                        safe = false;
                                        break;
                                    } else if (cc[after_alias] == '(') {
                                        // Function call on alias: alias() — not safe
                                        safe = false;
                                        break;
                                    } else if (cc[after_alias] == ' ' or cc[after_alias] == ';' or cc[after_alias] == '\n' or
                                        cc[after_alias] == ')' or cc[after_alias] == ',' or cc[after_alias] == ']' or
                                        cc[after_alias] == ':' or cc[after_alias] == '?')
                                    {
                                        // Bare reference: might be comparison, ternary, or passing to function
                                        var sp3 = after_alias;
                                        while (sp3 < cc.len and cc[sp3] == ' ') sp3 += 1;
                                        // Safe: comparison operators (alias === x, alias !== x, etc.)
                                        if (sp3 < cc.len and (cc[sp3] == '=' or cc[sp3] == '!') and
                                            sp3 + 1 < cc.len and cc[sp3 + 1] == '=')
                                        {
                                            continue; // alias === x or alias !== x
                                        }
                                        if (sp3 < cc.len and (cc[sp3] == '<' or cc[sp3] == '>')) continue;
                                        // Safe: alias after operator in expression (... + alias, etc.)
                                        if (sp3 < cc.len and (cc[sp3] == ')' or cc[sp3] == ';' or cc[sp3] == '\n' or cc[sp3] == ']' or cc[sp3] == '}')) {
                                            // End of expression — check what's before: is it in a comparison context?
                                            // Conservative: reject bare references that could be passed by value
                                            // Exception: `; alias ;` or `} alias` aren't real patterns
                                            // Check if alias is preceded by comparison: `=== alias`
                                            var bp = check_pos;
                                            while (bp > 0 and cc[bp - 1] == ' ') bp -= 1;
                                            if (bp >= 2 and cc[bp - 1] == '=' and cc[bp - 2] == '=') continue;
                                            if (bp >= 3 and cc[bp - 1] == '=' and cc[bp - 2] == '=' and cc[bp - 3] == '=') continue;
                                            if (bp >= 2 and cc[bp - 1] == '=' and cc[bp - 2] == '!') continue;
                                            safe = false;
                                            break;
                                        }
                                        // Safe: ternary `alias ?` — tests truthiness, doesn't need object itself
                                        if (sp3 < cc.len and cc[sp3] == '?') continue;
                                        // Anything else: `return alias`, `fn(alias)`, `arr.push(alias)` — NOT safe
                                        safe = false;
                                        break;
                                    }
                                    // Other characters (&&, ||, etc.) — alias used as boolean, check next
                                    if (after_alias + 1 < cc.len and
                                        ((cc[after_alias] == '&' and cc[after_alias + 1] == '&') or
                                        (cc[after_alias] == '|' and cc[after_alias + 1] == '|')))
                                    {
                                        // Boolean operators: `alias && x`, `alias || x` — tests truthiness, safe
                                        continue;
                                    }
                                    // Other: reject to be conservative
                                    safe = false;
                                    break;
                                }
                                if (!safe) continue;
                                // Allow same alias name in different scopes (different scope_start means different variable)
                                var dup = false;
                                for (alias_buf[0..alias_count]) |existing| {
                                    if (std.mem.eql(u8, existing.alias_name, alias_name) and existing.assign_pos == ap) { dup = true; break; }
                                }
                                if (dup) continue;
                                if (alias_count < alias_buf.len) {
                                    alias_buf[alias_count] = .{
                                        .alias_name = alias_name,
                                        .prov_idx = pi,
                                        .idx_expr = idx_expr,
                                        .assign_pos = ap,
                                        .scope_start = scope_start,
                                        .scope_end = scope_end,
                                    };
                                    alias_map.put(allocator, alias_name, alias_count) catch {};
                                    alias_count += 1;
                                }
                            }
                            if (alias_count > 0) {
                                std.debug.print("[soa] Detected {d} local variable aliases:\n", .{alias_count});
                                for (alias_buf[0..alias_count]) |a| {
                                    std.debug.print("[soa]   {s} = {s}[{s}] (prov={s})\n", .{
                                        a.alias_name, provenance[a.prov_idx].var_name, a.idx_expr, provenance[a.prov_idx].var_name,
                                    });
                                }
                            }
                        }
                        // NOTE: aliases slice created after for...of detection (which adds entries)

                        // === for...of SOA iteration detection ===
                        // Detect: for (const/let/var IDENT of PROV_VAR) { IDENT.field... }
                        // Create alias entries so IDENT.field → __col_field[__PROV_soa_base + __of_N]
                        // and inject a counter variable at the for statement.
                        const ForOfEntry = struct {
                            iter_name: []const u8, // iterator variable (e.g. "item")
                            prov_idx: usize, // index into provenance[]
                            counter_name: []const u8, // generated counter name (e.g. "__of_0")
                            for_pos: usize, // position of 'for' keyword in source
                            of_pos: usize, // position of 'of' keyword in source
                            body_start: usize, // position of '{' opening loop body
                            body_end: usize, // position of '}' closing loop body
                        };
                        var forof_buf: [64]ForOfEntry = undefined;
                        var forof_count: usize = 0;
                        if (prov_count > 0) {
                            var fop: usize = 0;
                            while (fop + 10 < cc.len) : (fop += 1) {
                                if (fop > 0 and isIdentChar(cc[fop - 1])) continue;
                                if (!std.mem.startsWith(u8, cc[fop..], "for")) continue;
                                if (fop + 3 >= cc.len or isIdentChar(cc[fop + 3])) continue; // forX — not for
                                // Skip whitespace and find '('
                                var p = fop + 3;
                                while (p < cc.len and (cc[p] == ' ' or cc[p] == '\n')) p += 1;
                                if (p >= cc.len or cc[p] != '(') continue;
                                p += 1;
                                while (p < cc.len and cc[p] == ' ') p += 1;
                                // Check for const/let/var keyword
                                var kw_len2: usize = 0;
                                if (p + 6 < cc.len and std.mem.startsWith(u8, cc[p..], "const ")) kw_len2 = 6
                                else if (p + 4 < cc.len and std.mem.startsWith(u8, cc[p..], "let ")) kw_len2 = 4
                                else if (p + 4 < cc.len and std.mem.startsWith(u8, cc[p..], "var ")) kw_len2 = 4;
                                if (kw_len2 == 0) continue;
                                p += kw_len2;
                                while (p < cc.len and cc[p] == ' ') p += 1;
                                // Extract iterator variable name
                                const iter_start = p;
                                while (p < cc.len and isIdentChar(cc[p])) p += 1;
                                if (p == iter_start) continue;
                                const iter_name = cc[iter_start..p];
                                // Skip whitespace and check for ' of '
                                while (p < cc.len and cc[p] == ' ') p += 1;
                                if (p + 2 >= cc.len or cc[p] != 'o' or cc[p + 1] != 'f' or (p + 2 < cc.len and isIdentChar(cc[p + 2]))) continue;
                                const of_pos = p;
                                p += 2;
                                while (p < cc.len and cc[p] == ' ') p += 1;
                                // Extract array name (the provenance container)
                                const arr_start = p;
                                while (p < cc.len and isIdentChar(cc[p])) p += 1;
                                if (p == arr_start) continue;
                                const arr_name = cc[arr_start..p];
                                // Skip to ')' then '{'
                                while (p < cc.len and cc[p] != ')') p += 1;
                                if (p >= cc.len) continue;
                                p += 1; // skip ')'
                                while (p < cc.len and (cc[p] == ' ' or cc[p] == '\n')) p += 1;
                                if (p >= cc.len or cc[p] != '{') continue;
                                const body_start = p;
                                // Find matching '}'
                                const body_end = skipJsFunctionBody(cc, p);
                                // Match array name against provenance containers
                                var matched_pi2: ?usize = null;
                                for (provenance, 0..) |prov, pi| {
                                    if (prov.decl_end == 0) continue;
                                    if (std.mem.eql(u8, prov.var_name, arr_name)) {
                                        matched_pi2 = pi;
                                        break;
                                    }
                                }
                                if (matched_pi2 == null) continue;
                                // Safety check: all uses of iter_name within loop body are .field reads
                                const pi2 = matched_pi2.?;
                                const site2 = alloc_sites.items[provenance[pi2].site_idx];
                                var fo_safe = true;
                                var check_p = body_start + 1;
                                while (check_p + iter_name.len < body_end) : (check_p += 1) {
                                    if (check_p > 0 and isIdentChar(cc[check_p - 1])) continue;
                                    if (!std.mem.startsWith(u8, cc[check_p..], iter_name)) continue;
                                    const after_it = check_p + iter_name.len;
                                    if (after_it >= cc.len) continue;
                                    if (isIdentChar(cc[after_it])) continue; // part of longer name
                                    if (cc[after_it] == '.') {
                                        // Field access — verify it's a known SOA field and not a method
                                        const fs3 = after_it + 1;
                                        var fe3 = fs3;
                                        while (fe3 < cc.len and isIdentChar(cc[fe3])) fe3 += 1;
                                        if (fe3 == fs3) { fo_safe = false; break; }
                                        if (fe3 < cc.len and cc[fe3] == '(') { fo_safe = false; break; } // method call
                                        const fn3 = cc[fs3..fe3];
                                        // Check assignment
                                        var eq3 = fe3;
                                        while (eq3 < cc.len and cc[eq3] == ' ') eq3 += 1;
                                        if (eq3 < cc.len and cc[eq3] == '=' and (eq3 + 1 >= cc.len or cc[eq3 + 1] != '=')) { fo_safe = false; break; }
                                        // Verify SOA field
                                        var fo_field_match = false;
                                        for (site2.field_names[0..site2.field_count]) |sf| {
                                            if (std.mem.eql(u8, sf, fn3)) { fo_field_match = true; break; }
                                        }
                                        if (!fo_field_match) { fo_safe = false; break; }
                                        check_p = fe3;
                                    } else if (cc[after_it] == ' ' or cc[after_it] == ')' or cc[after_it] == ';' or cc[after_it] == ',') {
                                        // Bare reference in comparison context is OK
                                        var sp3 = after_it;
                                        while (sp3 < cc.len and cc[sp3] == ' ') sp3 += 1;
                                        if (sp3 < cc.len and (cc[sp3] == '=' or cc[sp3] == '!') and sp3 + 1 < cc.len and cc[sp3 + 1] == '=') continue;
                                        if (sp3 < cc.len and (cc[sp3] == ')' or cc[sp3] == ';' or cc[sp3] == ',')) {
                                            var bp3 = check_p;
                                            while (bp3 > 0 and cc[bp3 - 1] == ' ') bp3 -= 1;
                                            if (bp3 >= 2 and cc[bp3 - 1] == '=' and cc[bp3 - 2] == '=') continue;
                                            fo_safe = false; break;
                                        }
                                        fo_safe = false; break;
                                    } else if (cc[after_it] == '[' or cc[after_it] == '(') {
                                        fo_safe = false; break;
                                    } else {
                                        // &&, ||, ternary etc.
                                        if (after_it + 1 < cc.len and ((cc[after_it] == '&' and cc[after_it + 1] == '&') or (cc[after_it] == '|' and cc[after_it + 1] == '|'))) continue;
                                        if (cc[after_it] == '?' or cc[after_it] == ':') continue;
                                    }
                                }
                                if (!fo_safe) continue;
                                // Scope check: __CONTAINER_soa_base must be accessible from this for...of.
                                // If decl_end is outside the enclosing function, skip.
                                {
                                    const prov_fo = provenance[pi2];
                                    if (prov_fo.decl_end > 0) {
                                        // Find enclosing function of the for loop
                                        var fn_start: usize = fop;
                                        var fn_depth: i32 = 0;
                                        while (fn_start > 0) {
                                            fn_start -= 1;
                                            if (cc[fn_start] == '}') fn_depth += 1;
                                            if (cc[fn_start] == '{') {
                                                if (fn_depth == 0) break;
                                                fn_depth -= 1;
                                            }
                                        }
                                        // If decl_end is before the enclosing function start, skip
                                        if (prov_fo.decl_end < fn_start or prov_fo.decl_end > body_end) continue;
                                    }
                                }
                                if (forof_count < forof_buf.len) {
                                    const counter_name = std.fmt.allocPrint(allocator, "__of_{d}", .{forof_count}) catch continue;
                                    forof_buf[forof_count] = .{
                                        .iter_name = iter_name,
                                        .prov_idx = pi2,
                                        .counter_name = counter_name,
                                        .for_pos = fop,
                                        .of_pos = of_pos,
                                        .body_start = body_start,
                                        .body_end = body_end,
                                    };
                                    // Also create an alias entry for the iterator variable
                                    if (alias_count < alias_buf.len) {
                                        alias_buf[alias_count] = .{
                                            .alias_name = iter_name,
                                            .prov_idx = pi2,
                                            .idx_expr = counter_name,
                                            .assign_pos = body_start,
                                            .scope_start = body_start,
                                            .scope_end = body_end,
                                        };
                                        alias_map.put(allocator, iter_name, alias_count) catch {};
                                        alias_count += 1;
                                    }
                                    forof_count += 1;
                                    std.debug.print("[soa] for...of SOA: for ({s} of {s}) → counter {s}\n", .{
                                        iter_name, arr_name, counter_name,
                                    });
                                }
                            }
                        }
                        const forof_entries = forof_buf[0..forof_count];
                        // Create aliases slice AFTER for...of detection (which may add entries)
                        const aliases = alias_buf[0..alias_count];

                        // === Inter-procedural parameter provenance ===
                        // Detect call sites `funcName(PROV_VAR, ...)` where PROV_VAR is a provenance
                        // container. Find the matching `function funcName(param, ...)` definition and
                        // create synthetic provenance entries so that `param[expr].field` inside the
                        // function body gets rewritten to `__col_field[__PROV_soa_base + (expr)]`.
                        const ParamProv = struct {
                            param_name: []const u8, // function parameter name
                            prov_idx: usize, // index into provenance[] (which container was passed)
                            func_body_start: usize, // position of '{' opening function body
                            func_body_end: usize, // position of '}' closing function body
                        };
                        var param_prov_buf: [128]ParamProv = undefined;
                        var param_prov_count: usize = 0;
                        if (prov_count > 0) {
                            // Step 1: Find call sites where provenance containers are passed as arguments
                            // Pattern: funcName(PROV_VAR, ...) or funcName(..., PROV_VAR, ...)
                            const CallSiteInfo = struct {
                                func_name: []const u8,
                                arg_pos: usize, // 0-indexed argument position
                                prov_idx: usize, // which provenance entry
                            };
                            var call_sites_buf: [128]CallSiteInfo = undefined;
                            var call_sites_count: usize = 0;

                            var cs: usize = 0;
                            while (cs + 2 < cc.len) : (cs += 1) {
                                if (cc[cs] != '(') continue;
                                // Find function name before '('
                                const fn_end = cs;
                                if (fn_end == 0 or !isIdentChar(cc[fn_end - 1])) continue;
                                var fn_start = fn_end;
                                while (fn_start > 0 and isIdentChar(cc[fn_start - 1])) fn_start -= 1;
                                const func_name = cc[fn_start..fn_end];
                                // Skip keywords and known non-function names
                                if (std.mem.eql(u8, func_name, "if") or std.mem.eql(u8, func_name, "for") or
                                    std.mem.eql(u8, func_name, "while") or std.mem.eql(u8, func_name, "switch") or
                                    std.mem.eql(u8, func_name, "return") or std.mem.eql(u8, func_name, "function") or
                                    std.mem.eql(u8, func_name, "typeof") or std.mem.eql(u8, func_name, "new") or
                                    std.mem.eql(u8, func_name, "push") or std.mem.eql(u8, func_name, "set") or
                                    std.mem.eql(u8, func_name, "get") or std.mem.eql(u8, func_name, "call") or
                                    std.mem.eql(u8, func_name, "apply") or std.mem.eql(u8, func_name, "bind") or
                                    std.mem.eql(u8, func_name, "catch") or std.mem.eql(u8, func_name, "require")) continue;
                                // Skip method calls: something.funcName(
                                if (fn_start > 0 and cc[fn_start - 1] == '.') continue;
                                // Parse arguments: find each comma-separated arg
                                var arg_start = cs + 1;
                                while (arg_start < cc.len and cc[arg_start] == ' ') arg_start += 1;
                                var arg_pos: usize = 0;
                                var depth_cs: u32 = 1;
                                var ap2 = arg_start;
                                while (ap2 < cc.len and depth_cs > 0) {
                                    if (cc[ap2] == '(' or cc[ap2] == '[') {
                                        depth_cs += 1;
                                        ap2 += 1;
                                        continue;
                                    }
                                    if (cc[ap2] == ')' or cc[ap2] == ']') {
                                        if (depth_cs == 1 and cc[ap2] == ')') {
                                            // End of arguments — check final arg
                                            var ae2 = ap2;
                                            while (ae2 > arg_start and cc[ae2 - 1] == ' ') ae2 -= 1;
                                            const arg_text = cc[arg_start..ae2];
                                            // Check if arg is a provenance container name
                                            for (provenance, 0..) |prov, pi| {
                                                if (prov.decl_end == 0) continue;
                                                if (std.mem.eql(u8, arg_text, prov.var_name)) {
                                                    if (call_sites_count < call_sites_buf.len) {
                                                        // Deduplicate: same func+arg_pos+prov
                                                        var dup = false;
                                                        for (call_sites_buf[0..call_sites_count]) |existing| {
                                                            if (std.mem.eql(u8, existing.func_name, func_name) and
                                                                existing.arg_pos == arg_pos and existing.prov_idx == pi)
                                                            {
                                                                dup = true;
                                                                break;
                                                            }
                                                        }
                                                        if (!dup) {
                                                            call_sites_buf[call_sites_count] = .{
                                                                .func_name = func_name,
                                                                .arg_pos = arg_pos,
                                                                .prov_idx = pi,
                                                            };
                                                            call_sites_count += 1;
                                                        }
                                                    }
                                                    break;
                                                }
                                            }
                                            break;
                                        }
                                        depth_cs -= 1;
                                        ap2 += 1;
                                        continue;
                                    }
                                    if (depth_cs == 1 and cc[ap2] == ',') {
                                        // End of one argument
                                        var ae2 = ap2;
                                        while (ae2 > arg_start and cc[ae2 - 1] == ' ') ae2 -= 1;
                                        const arg_text = cc[arg_start..ae2];
                                        for (provenance, 0..) |prov, pi| {
                                            if (prov.decl_end == 0) continue;
                                            if (std.mem.eql(u8, arg_text, prov.var_name)) {
                                                if (call_sites_count < call_sites_buf.len) {
                                                    var dup = false;
                                                    for (call_sites_buf[0..call_sites_count]) |existing| {
                                                        if (std.mem.eql(u8, existing.func_name, func_name) and
                                                            existing.arg_pos == arg_pos and existing.prov_idx == pi)
                                                        {
                                                            dup = true;
                                                            break;
                                                        }
                                                    }
                                                    if (!dup) {
                                                        call_sites_buf[call_sites_count] = .{
                                                            .func_name = func_name,
                                                            .arg_pos = arg_pos,
                                                            .prov_idx = pi,
                                                        };
                                                        call_sites_count += 1;
                                                    }
                                                }
                                                break;
                                            }
                                        }
                                        // Move to next argument
                                        arg_pos += 1;
                                        ap2 += 1;
                                        while (ap2 < cc.len and cc[ap2] == ' ') ap2 += 1;
                                        arg_start = ap2;
                                        continue;
                                    }
                                    ap2 += 1;
                                }
                            }

                            // Step 2: For each call site, find the function definition and extract parameter name
                            for (call_sites_buf[0..call_sites_count]) |csi| {
                                // Search for `function funcName(` in source
                                var fs: usize = 0;
                                while (fs + 9 + csi.func_name.len < cc.len) : (fs += 1) {
                                    if (!std.mem.startsWith(u8, cc[fs..], "function ")) continue;
                                    var ns = fs + 9;
                                    while (ns < cc.len and cc[ns] == ' ') ns += 1;
                                    if (ns + csi.func_name.len >= cc.len) continue;
                                    if (!std.mem.startsWith(u8, cc[ns..], csi.func_name)) continue;
                                    const after_name = ns + csi.func_name.len;
                                    // Skip whitespace to '('
                                    var paren = after_name;
                                    while (paren < cc.len and cc[paren] == ' ') paren += 1;
                                    if (paren >= cc.len or cc[paren] != '(') continue;
                                    // Verify word boundary (not funcNameExtra)
                                    if (after_name < cc.len and isIdentChar(cc[after_name])) continue;
                                    // Extract parameter at arg_pos
                                    var param_start = paren + 1;
                                    while (param_start < cc.len and cc[param_start] == ' ') param_start += 1;
                                    var curr_arg: usize = 0;
                                    var pp = param_start;
                                    var pd: u32 = 1;
                                    while (pp < cc.len and pd > 0) {
                                        if (cc[pp] == '(') { pd += 1; pp += 1; continue; }
                                        if (cc[pp] == ')') {
                                            if (pd == 1) {
                                                // End of params — extract current arg if matching
                                                if (curr_arg == csi.arg_pos) {
                                                    var pe = pp;
                                                    while (pe > param_start and cc[pe - 1] == ' ') pe -= 1;
                                                    const param_name = cc[param_start..pe];
                                                    if (param_name.len > 0 and !std.mem.eql(u8, param_name, csi.func_name)) {
                                                        // Find function body: skip to '{' and find matching '}'
                                                        var body_open = pp + 1;
                                                        while (body_open < cc.len and cc[body_open] != '{') body_open += 1;
                                                        if (body_open < cc.len) {
                                                            var bd: u32 = 1;
                                                            var body_close = body_open + 1;
                                                            while (body_close < cc.len and bd > 0) : (body_close += 1) {
                                                                if (cc[body_close] == '{') bd += 1;
                                                                if (cc[body_close] == '}') bd -= 1;
                                                            }
                                                            if (param_prov_count < param_prov_buf.len) {
                                                                // Deduplicate
                                                                var dup = false;
                                                                for (param_prov_buf[0..param_prov_count]) |existing| {
                                                                    if (std.mem.eql(u8, existing.param_name, param_name) and
                                                                        existing.func_body_start == body_open)
                                                                    {
                                                                        dup = true;
                                                                        break;
                                                                    }
                                                                }
                                                                if (!dup) {
                                                                    param_prov_buf[param_prov_count] = .{
                                                                        .param_name = param_name,
                                                                        .prov_idx = csi.prov_idx,
                                                                        .func_body_start = body_open,
                                                                        .func_body_end = body_close,
                                                                    };
                                                                    param_prov_count += 1;
                                                                }
                                                            }
                                                        }
                                                    }
                                                }
                                                break;
                                            }
                                            pd -= 1;
                                            pp += 1;
                                            continue;
                                        }
                                        if (pd == 1 and cc[pp] == ',') {
                                            // Check if current arg matches before moving to next
                                            if (curr_arg == csi.arg_pos) {
                                                var pe = pp;
                                                while (pe > param_start and cc[pe - 1] == ' ') pe -= 1;
                                                const param_name = cc[param_start..pe];
                                                if (param_name.len > 0 and !std.mem.eql(u8, param_name, csi.func_name)) {
                                                    // Find function body
                                                    // First find the closing ')' of params
                                                    var close_paren = pp + 1;
                                                    var pd2: u32 = 1;
                                                    while (close_paren < cc.len and pd2 > 0) : (close_paren += 1) {
                                                        if (cc[close_paren] == '(') pd2 += 1;
                                                        if (cc[close_paren] == ')') pd2 -= 1;
                                                    }
                                                    var body_open = close_paren;
                                                    while (body_open < cc.len and cc[body_open] != '{') body_open += 1;
                                                    if (body_open < cc.len) {
                                                        var bd: u32 = 1;
                                                        var body_close = body_open + 1;
                                                        while (body_close < cc.len and bd > 0) : (body_close += 1) {
                                                            if (cc[body_close] == '{') bd += 1;
                                                            if (cc[body_close] == '}') bd -= 1;
                                                        }
                                                        if (param_prov_count < param_prov_buf.len) {
                                                            var dup = false;
                                                            for (param_prov_buf[0..param_prov_count]) |existing| {
                                                                if (std.mem.eql(u8, existing.param_name, param_name) and
                                                                    existing.func_body_start == body_open)
                                                                {
                                                                    dup = true;
                                                                    break;
                                                                }
                                                            }
                                                            if (!dup) {
                                                                param_prov_buf[param_prov_count] = .{
                                                                    .param_name = param_name,
                                                                    .prov_idx = csi.prov_idx,
                                                                    .func_body_start = body_open,
                                                                    .func_body_end = body_close,
                                                                };
                                                                param_prov_count += 1;
                                                            }
                                                        }
                                                    }
                                                }
                                                break; // found the match, done with this call site
                                            }
                                            curr_arg += 1;
                                            pp += 1;
                                            while (pp < cc.len and cc[pp] == ' ') pp += 1;
                                            param_start = pp;
                                            continue;
                                        }
                                        pp += 1;
                                    }
                                    break; // found the function definition
                                }
                            }

                            if (param_prov_count > 0) {
                                std.debug.print("[soa] Detected {d} inter-procedural parameter aliases:\n", .{param_prov_count});
                                for (param_prov_buf[0..param_prov_count]) |pp2| {
                                    std.debug.print("[soa]   param '{s}' ← {s} (scope {d}..{d})\n", .{
                                        pp2.param_name, provenance[pp2.prov_idx].var_name,
                                        pp2.func_body_start, pp2.func_body_end,
                                    });
                                }
                            }
                        }
                        const param_provs = param_prov_buf[0..param_prov_count];

                        // Safety: disable soa_base for provenance entries that have inter-procedural
                        // aliases. When a container is passed to other functions, the soa_base
                        // variable defined at the declaration site is NOT in scope in the callees.
                        // Disabling decl_end prevents index-aligned rewrites that would reference
                        // an undefined soa_base. The provenance still works for non-indexed access.
                        // soa_base now stored as VAR.__sb (property on array), not scoped variable.
                        // No need to disable for inter-procedural leaks — .__sb travels with the array.

                        const rs_rewrite_count: u32 = 0;

                        // Pre-pass: build exact rewrite positions
                        const RewriteInfo = struct { dot_pos: u32, end_pos: u32, field: []const u8 };
                        var pre_pass_rewrites = std.AutoHashMap(u32, RewriteInfo).init(allocator);
                        defer pre_pass_rewrites.deinit();
                        {
                            var pp_line: u32 = 1;
                            var pp_pos: usize = 0;
                            while (pp_pos < cc.len) : (pp_pos += 1) {
                                if (cc[pp_pos] == '\n') { pp_line += 1; continue; }
                                // Check if this line has patches and we're at a dot
                                if (cc[pp_pos] != '.' or !patch_lines.contains(pp_line)) continue;
                                if (pp_pos == 0 or (!isIdentChar(cc[pp_pos - 1]) and cc[pp_pos - 1] != ')' and cc[pp_pos - 1] != ']')) continue;
                                // Read field after dot
                                const fs = pp_pos + 1;
                                var fe = fs;
                                while (fe < cc.len and isIdentChar(cc[fe])) fe += 1;
                                if (fe == fs) continue;
                                const field = cc[fs..fe];
                                var fh: u64 = 0;
                                for (field) |c| fh = fh *% 31 + c;
                                if (!patch_set.contains(pp_line *% 1000003 + fh)) continue;
                                // Skip assignments and method calls
                                var eq = fe;
                                while (eq < cc.len and cc[eq] == ' ') eq += 1;
                                if (eq < cc.len and cc[eq] == '(') continue; // method call
                                if (eq < cc.len and cc[eq] == '=' and (eq + 1 >= cc.len or cc[eq + 1] != '=')) continue;
                                if (eq + 1 < cc.len and cc[eq + 1] == '=' and (cc[eq] == '+' or cc[eq] == '-' or cc[eq] == '|' or cc[eq] == '&')) continue;
                                if (eq + 1 < cc.len and ((cc[eq] == '+' and cc[eq + 1] == '+') or (cc[eq] == '-' and cc[eq + 1] == '-'))) continue;
                                // Scan backwards to find expression start
                                var expr_start = pp_pos;
                                while (expr_start > 0) {
                                    const prev = cc[expr_start - 1];
                                    if (isIdentChar(prev) or prev == '.') {
                                        expr_start -= 1;
                                    } else if (prev == ')' or prev == ']') {
                                        // Skip matched parens/brackets backwards
                                        var depth_c: u32 = 1;
                                        const close = prev;
                                        const open: u8 = if (close == ')') '(' else '[';
                                        expr_start -= 1;
                                        while (expr_start > 0 and depth_c > 0) {
                                            expr_start -= 1;
                                            if (cc[expr_start] == close) depth_c += 1;
                                            if (cc[expr_start] == open) depth_c -= 1;
                                        }
                                    } else break;
                                }
                                // Store: at expr_start, insert __rd( ... )
                                pre_pass_rewrites.put(@intCast(expr_start), .{
                                    .dot_pos = @intCast(pp_pos),
                                    .end_pos = @intCast(fe),
                                    .field = field,
                                }) catch { w_errs += 1; };
                            }
                            std.debug.print("[soa] Pre-pass: {d} rewrite positions identified\n", .{pre_pass_rewrites.count()});
                        }

                        // Build ident-char table for fast word-boundary detection
                        var ident_char = [_]bool{false} ** 256;
                        for ("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_$") |c| ident_char[c] = true;

                        // Precompute valid alloc sites and build name→index map for O(1) lookup
                        var valid_alloc_sites = std.StringHashMapUnmanaged(usize){};
                        defer valid_alloc_sites.deinit(allocator);
                        for (alloc_sites.items, 0..) |site, si| {
                            var valid = true;
                            for (site.field_names[0..site.field_count]) |fname| {
                                if (fname.len == 0) { valid = false; break; }
                                for (fname) |c| {
                                    if (!isIdentChar(c)) { valid = false; break; }
                                }
                                if (!valid) break;
                            }
                            if (valid and site.name.len > 0) {
                                valid_alloc_sites.put(allocator, site.name, si) catch {};
                            }
                        }
                        // Build wasm func name→index map for O(1) lookup
                        var wasm_name_map = std.StringHashMapUnmanaged(usize){};
                        defer wasm_name_map.deinit(allocator);
                        for (wasm_func_list.items, 0..) |wf_entry, wfi| {
                            if (!wf_entry.is_anon and wf_entry.name.len > 0) {
                                wasm_name_map.put(allocator, wf_entry.name, wfi) catch {};
                            }
                        }
                        // Pass 1: Pre-scan to identify which SOA columns have active reads.
                        // Only factory columns with reads should be written — writing unused
                        // columns can change object lifetimes (strong refs prevent GC).
                        var active_cols = std.StringHashMapUnmanaged(void){};
                        defer active_cols.deinit(allocator);
                        for (provenance) |prov| {
                            if (prov.decl_end == 0) continue;
                            const site = alloc_sites.items[prov.site_idx];
                            // Scan for VAR[expr].field patterns
                            var sp: usize = 0;
                            while (sp + prov.var_name.len + 2 < cc.len) : (sp += 1) {
                                if (sp > 0 and isIdentChar(cc[sp - 1])) continue;
                                if (!std.mem.startsWith(u8, cc[sp..], prov.var_name)) continue;
                                const av = sp + prov.var_name.len;
                                if (av >= cc.len or cc[av] != '[') continue;
                                // Find ']'
                                var d: u32 = 1;
                                var j = av + 1;
                                while (j < cc.len and d > 0) : (j += 1) {
                                    if (cc[j] == '[') d += 1;
                                    if (cc[j] == ']') d -= 1;
                                }
                                if (j >= cc.len or cc[j] != '.') continue;
                                const fs = j + 1;
                                var fe = fs;
                                while (fe < cc.len and isIdentChar(cc[fe])) fe += 1;
                                if (fe == fs) continue;
                                const fn2 = cc[fs..fe];
                                // Check if it's a known SOA field
                                for (site.field_names[0..site.field_count]) |sf| {
                                    if (std.mem.eql(u8, sf, fn2)) {
                                        active_cols.put(allocator, sf, {}) catch {};
                                        break;
                                    }
                                }
                            }
                        }
                        // Also check inter-procedural and alias patterns
                        for (param_provs) |pp| {
                            const prov = provenance[pp.prov_idx];
                            const site = alloc_sites.items[prov.site_idx];
                            var sp: usize = pp.func_body_start;
                            while (sp + pp.param_name.len + 2 < pp.func_body_end) : (sp += 1) {
                                if (sp > 0 and isIdentChar(cc[sp - 1])) continue;
                                if (!std.mem.startsWith(u8, cc[sp..], pp.param_name)) continue;
                                const av = sp + pp.param_name.len;
                                if (av >= cc.len or cc[av] != '[') continue;
                                var d: u32 = 1;
                                var j = av + 1;
                                while (j < cc.len and d > 0) : (j += 1) {
                                    if (cc[j] == '[') d += 1;
                                    if (cc[j] == ']') d -= 1;
                                }
                                if (j >= cc.len or cc[j] != '.') continue;
                                const fs = j + 1;
                                var fe = fs;
                                while (fe < cc.len and isIdentChar(cc[fe])) fe += 1;
                                if (fe == fs) continue;
                                const fn2 = cc[fs..fe];
                                for (site.field_names[0..site.field_count]) |sf| {
                                    if (std.mem.eql(u8, sf, fn2)) {
                                        active_cols.put(allocator, sf, {}) catch {};
                                        break;
                                    }
                                }
                            }
                        }
                        if (active_cols.count() > 0) {
                            std.debug.print("[soa] Pass 1: {d} active columns with reads\n", .{active_cols.count()});
                        }

                        // Build sorted arrays of special positions for O(1) next-position scan
                        // instead of per-byte HashMap lookups
                        var special_positions = std.ArrayListUnmanaged(u32){};
                        defer special_positions.deinit(allocator);
                        {
                            var rw_iter = pre_pass_rewrites.iterator();
                            while (rw_iter.next()) |entry| {
                                special_positions.append(allocator, entry.key_ptr.*) catch {};
                            }
                        }
                        // Also build a set of provenance decl_end positions for O(1) lookup
                        var decl_end_set = std.AutoHashMap(usize, usize).init(allocator);
                        defer decl_end_set.deinit();
                        for (provenance, 0..) |prov, pi| {
                            if (prov.decl_end > 0) {
                                decl_end_set.put(prov.decl_end, pi) catch {};
                                special_positions.append(allocator, @intCast(prov.decl_end)) catch {};
                            }
                        }
                        // Emit module-scope soa_base variable declarations
                        // These are `var` (not const) so they're hoisted to module scope
                        // and accessible from any function that receives the container.
                        for (provenance, 0..) |prov, pi| {
                            if (prov.decl_end > 0) {
                                w.print("var __sb{d};\n", .{pi}) catch { w_errs += 1; };
                            }
                        }

                        // Add for...of positions for counter injection
                        var forof_start_set = std.AutoHashMap(usize, usize).init(allocator);
                        defer forof_start_set.deinit();
                        var forof_end_set = std.AutoHashMap(usize, usize).init(allocator);
                        defer forof_end_set.deinit();
                        for (forof_entries, 0..) |fo, fi| {
                            forof_start_set.put(fo.for_pos, fi) catch {};
                            // body_end is the position AFTER the closing '}', so inject before it
                            if (fo.body_end > 0) forof_end_set.put(fo.body_end - 1, fi) catch {};
                            special_positions.append(allocator, @intCast(fo.for_pos)) catch {};
                            if (fo.body_end > 0) special_positions.append(allocator, @intCast(fo.body_end - 1)) catch {};
                        }
                        std.sort.insertion(u32, special_positions.items, {}, std.sort.asc(u32));
                        var special_idx: usize = 0; // cursor into sorted special_positions

                        var src_pos: usize = 0;
                        std.debug.print("[soa] Starting char_loop over {d} bytes...\n", .{cc.len});
                        const loop_start_ts = std.time.milliTimestamp();
                        var slow_path_count: u64 = 0;
                        var fast_path_count: u64 = 0;
                        char_loop: while (src_pos < cc.len) {
                            // Fast path: bulk-copy until we hit a word boundary or special position
                            // A "word boundary" is where the previous char is NOT an ident char
                            // but the current one IS (or current is '{' for arrow matches).
                            // Advance special_idx past positions we've already passed
                            while (special_idx < special_positions.items.len and special_positions.items[special_idx] < src_pos) special_idx += 1;
                            const at_special = special_idx < special_positions.items.len and special_positions.items[special_idx] == src_pos;
                            const at_word_start = (src_pos == 0 or !ident_char[cc[src_pos - 1]]) and (ident_char[cc[src_pos]] or cc[src_pos] == '{');
                            if (!at_word_start and !at_special) {
                                const chunk_start = src_pos;
                                const next_special: usize = if (special_idx < special_positions.items.len) special_positions.items[special_idx] else cc.len;
                                src_pos += 1;
                                while (src_pos < cc.len and src_pos < next_special) {
                                    const ws = (src_pos == 0 or !ident_char[cc[src_pos - 1]]) and (ident_char[cc[src_pos]] or cc[src_pos] == '{');
                                    if (ws) break;
                                    src_pos += 1;
                                }
                                w.writeAll(cc[chunk_start..src_pos]) catch { w_errs += 1; };
                                fast_path_count += 1;
                                continue :char_loop;
                            }
                            slow_path_count += 1;
                            if (slow_path_count % 100000 == 0) std.debug.print("[soa] slow_path={d} fast_path={d} src_pos={d}/{d}\n", .{ slow_path_count, fast_path_count, src_pos, cc.len });

                            // (line tracking removed — pre_pass_rewrites has exact byte positions)

                            // __rd() pre-pass rewrites disabled: runtime type check
                            // (o.__idx !== undefined) always hits fallback for non-SOA objects,
                            // adding pure overhead. Factories still write SOA columns for batch ops.
                            // === SOA base capture ===
                            // At `VAR = []` declaration, emit base offset as property on the array.
                            // Using `VAR.__sb` (soa_base) instead of `const __VAR_soa_base` so it
                            // survives across function boundaries when passed as a parameter.
                            if (decl_end_set.get(src_pos)) |pi| {
                                const prov = provenance[pi];
                                // Assign module-scope soa_base variable (declared in header)
                                w.print(";__sb{d} = __col_idx", .{pi}) catch { w_errs += 1; };
                                if (prov.raw_index) {
                                    w.print(";{s}.__soa={d}", .{ prov.var_name, prov.site_idx }) catch { w_errs += 1; };
                                }
                            }
                            // === for...of counter injection ===
                            // At for_pos: emit `let __of_N = 0;` before the `for`
                            if (forof_start_set.get(src_pos)) |fi| {
                                const fo = forof_entries[fi];
                                w.print("let {s} = 0;", .{fo.counter_name}) catch { w_errs += 1; };
                            }
                            // At body_end-1 (just before '}'): emit `__of_N++;`
                            if (forof_end_set.get(src_pos)) |fi| {
                                const fo = forof_entries[fi];
                                w.print("{s}++;", .{fo.counter_name}) catch { w_errs += 1; };
                            }
                            // === Batch struct detection ===
                            // Pattern: for (V = 0; V < ARR.length; V++) { ACC = (ACC + FN(ARR[V], ...) | 0; }
                            // where FN is a batch-eligible struct function.
                            // Replace: ACC = (ACC + __batch_FN(ARR, ...)) | 0;
                            if (src_pos + 4 < cc.len and std.mem.startsWith(u8, cc[src_pos..], "for ") or
                                (src_pos + 4 < cc.len and std.mem.startsWith(u8, cc[src_pos..], "for(")))
                            {
                                if (detectBatchLoop(cc, src_pos, wasm_func_list.items)) |batch| {
                                    // Write everything before the loop
                                    // The loop is already in the output stream at src_pos.
                                    // Write the batch replacement and skip past the original loop.
                                    w.print("{s} = ({s} + __batch_{d}({s}", .{
                                        batch.acc_name, batch.acc_name, batch.func_idx, batch.arr_name,
                                    }) catch { w_errs += 1; };
                                    // Write scalar args
                                    if (batch.scalar_args.len > 0) {
                                        w.print(", {s}", .{batch.scalar_args}) catch { w_errs += 1; };
                                    }
                                    w.writeAll(")) | 0;\n") catch { w_errs += 1; };
                                    src_pos = batch.loop_end;
                                    continue;
                                }
                            }

                            // === Push-loop pool materialization ===
                            // Detect: for (...) { ARR.push({ fields... }); }
                            // Append after loop: materialize WASM pool from JS array
                            // so subsequent __batch_* calls hit the identity cache.
                            // SKIP when the array has inline literal SOA provenance — let char_loop
                            // handle the push rewrite character by character instead.
                            if (src_pos + 4 < cc.len and
                                (std.mem.startsWith(u8, cc[src_pos..], "for ") or
                                std.mem.startsWith(u8, cc[src_pos..], "for(")))
                            {
                                if (detectPushLoop(cc, src_pos, wasm_func_list.items)) |push| {
                                    // Check if this array has inline SOA provenance — if so, skip push-loop
                                    // materialization so the char_loop can rewrite pushes to column writes
                                    var has_inline_prov = false;
                                    for (provenance) |prov2| {
                                        if (!std.mem.eql(u8, prov2.var_name, push.arr_name)) continue;
                                        const ps = alloc_sites.items[prov2.site_idx];
                                        if (std.mem.startsWith(u8, ps.name, "__inline_")) { has_inline_prov = true; break; }
                                    }
                                    if (!has_inline_prov) {
                                    // Write the original push loop unchanged
                                    w.writeAll(cc[src_pos..push.loop_end]) catch { w_errs += 1; };
                                    // Append pool materialization for each matched struct function
                                    w.writeAll("\n") catch { w_errs += 1; };
                                    for (wasm_func_list.items, 0..) |wfe, wfi| {
                                        if (wfe.struct_args == 0) continue;
                                        // Match fields
                                        var match_sai: u32 = 0;
                                        var match_fc: u8 = 0;
                                        while (match_sai < 8) : (match_sai += 1) {
                                            if (wfe.struct_args & (@as(u8, 1) << @intCast(match_sai)) != 0) {
                                                match_fc = wfe.struct_field_counts[match_sai];
                                                break;
                                            }
                                        }
                                        if (match_fc == 0) continue;
                                        // Check if ANY of this func's fields appear in the push body
                                        var fields_match = false;
                                        for (wfe.struct_field_names[match_sai][0..match_fc]) |fname| {
                                            if (fname.len > 0 and push.has_field(fname)) {
                                                fields_match = true;
                                                break;
                                            }
                                        }
                                        if (!fields_match) continue;
                                        const s = @as(u32, match_fc);
                                        // Emit pool materialization
                                        w.print("__bp_{d}_sp = __wasmStackSave(); ", .{wfi}) catch { w_errs += 1; };
                                        w.print("__bp_{d}_pool = __wasmStackAlloc({s}.length * {d}); ", .{ wfi, push.arr_name, s * 4 }) catch { w_errs += 1; };
                                        w.writeAll("{\n") catch { w_errs += 1; };
                                        w.print("  const __b = __bp_{d}_pool >> 2;\n", .{wfi}) catch { w_errs += 1; };
                                        w.print("  for (let __i = 0; __i < {s}.length; __i++) {{\n", .{push.arr_name}) catch { w_errs += 1; };
                                        for (wfe.struct_field_names[match_sai][0..match_fc], 0..) |fname, fi| {
                                            if (fname.len == 0) continue;
                                            if (s == 1) {
                                                w.print("    __m[__b + __i] = {s}[__i].{s};\n", .{ push.arr_name, fname }) catch { w_errs += 1; };
                                            } else {
                                                w.print("    __m[__b + __i * {d} + {d}] = {s}[__i].{s};\n", .{ s, fi, push.arr_name, fname }) catch { w_errs += 1; };
                                            }
                                        }
                                        w.writeAll("  }\n}\n") catch { w_errs += 1; };
                                        w.print("__bp_{d}_arr = {s};\n", .{ wfi, push.arr_name }) catch { w_errs += 1; };
                                    }
                                    src_pos = push.loop_end;
                                    continue;
                                    }
                                }
                            }

                            // === Inline object literal push → SOA column write ===
                            // Detect: PROV_VAR.push({ key1: expr1, key2: expr2, ... })
                            // Rewrite: __col_key1[__col_idx] = expr1; __col_key2[__col_idx] = expr2; PROV_VAR.push(__col_idx++)
                            if (prov_count > 0 and src_pos + 6 < cc.len and isIdentChar(cc[src_pos]) and
                                (src_pos == 0 or !isIdentChar(cc[src_pos - 1])))
                            inline_push: {
                                // Read identifier
                                var ie = src_pos;
                                while (ie < cc.len and ident_char[cc[ie]]) ie += 1;
                                if (ie >= cc.len or !std.mem.startsWith(u8, cc[ie..], ".push(")) break :inline_push;
                                const push_var = cc[src_pos..ie];
                                // Check if this variable is a provenance container with an inline alloc site
                                var matched_prov: ?usize = null;
                                for (provenance, 0..) |prov, pi| {
                                    if (!std.mem.eql(u8, prov.var_name, push_var)) continue;
                                    // Only match inline sites (synthetic names start with __inline_)
                                    const site2 = alloc_sites.items[prov.site_idx];
                                    if (!std.mem.startsWith(u8, site2.name, "__inline_")) continue;
                                    matched_prov = pi;
                                    break;
                                }
                                if (matched_prov == null) break :inline_push;
                                const prov = provenance[matched_prov.?];
                                // Inline push rewrite replaces objects with integers in the array.
                                // This is ONLY safe when ALL reads go through SOA columns.
                                // raw_index is true when every VAR[expr] is followed by .field.
                                // Without this guarantee, some reads would get integers instead of objects.
                                if (!prov.raw_index) break :inline_push;
                                const site = alloc_sites.items[prov.site_idx];
                                // Find the '{' after .push(
                                var obj_start = ie + 6; // skip ".push("
                                while (obj_start < cc.len and cc[obj_start] == ' ') obj_start += 1;
                                if (obj_start >= cc.len or cc[obj_start] != '{') break :inline_push;
                                // Parse the object literal: extract value expressions for each field
                                var field_vals: [16][]const u8 = .{""} ** 16;
                                var fv_count: u8 = 0;
                                var fv_pos = obj_start + 1;
                                var fv_depth: u32 = 1;
                                while (fv_pos < cc.len and fv_depth > 0 and fv_count < site.field_count) {
                                    if (cc[fv_pos] == '{') { fv_depth += 1; fv_pos += 1; continue; }
                                    if (cc[fv_pos] == '}') { fv_depth -= 1; if (fv_depth == 0) break; fv_pos += 1; continue; }
                                    if (fv_depth > 1) { fv_pos += 1; continue; }
                                    // Skip whitespace
                                    while (fv_pos < cc.len and (cc[fv_pos] == ' ' or cc[fv_pos] == '\n' or cc[fv_pos] == '\r')) fv_pos += 1;
                                    if (fv_pos >= cc.len or cc[fv_pos] == '}') break;
                                    // Read field name
                                    const fk_start = fv_pos;
                                    while (fv_pos < cc.len and isIdentChar(cc[fv_pos])) fv_pos += 1;
                                    if (fv_pos <= fk_start) { fv_pos += 1; continue; }
                                    const fk_name = cc[fk_start..fv_pos];
                                    // Skip to ':'
                                    while (fv_pos < cc.len and cc[fv_pos] == ' ') fv_pos += 1;
                                    if (fv_pos >= cc.len or cc[fv_pos] != ':') break;
                                    fv_pos += 1; // skip ':'
                                    while (fv_pos < cc.len and cc[fv_pos] == ' ') fv_pos += 1;
                                    // Read value expression (until ',' or '}' at depth 0)
                                    const val_start = fv_pos;
                                    var val_depth: u32 = 0;
                                    while (fv_pos < cc.len) {
                                        if (cc[fv_pos] == '(' or cc[fv_pos] == '[' or cc[fv_pos] == '{') { val_depth += 1; fv_pos += 1; continue; }
                                        if (cc[fv_pos] == ')' or cc[fv_pos] == ']') {
                                            if (val_depth > 0) val_depth -= 1;
                                            fv_pos += 1;
                                            continue;
                                        }
                                        if (cc[fv_pos] == '}') {
                                            if (val_depth > 0) { val_depth -= 1; fv_pos += 1; continue; }
                                            break;
                                        }
                                        if (cc[fv_pos] == ',' and val_depth == 0) { fv_pos += 1; break; }
                                        fv_pos += 1;
                                    }
                                    // Trim trailing whitespace from value
                                    var val_end = fv_pos;
                                    if (val_end > val_start and (cc[val_end - 1] == ',' or cc[val_end - 1] == ' ' or cc[val_end - 1] == '\n')) val_end -= 1;
                                    while (val_end > val_start and (cc[val_end - 1] == ' ' or cc[val_end - 1] == '\n')) val_end -= 1;
                                    // Find which field index this key maps to
                                    for (site.field_names[0..site.field_count], 0..) |sf, fi| {
                                        if (std.mem.eql(u8, sf, fk_name)) {
                                            field_vals[fi] = cc[val_start..val_end];
                                            fv_count += 1;
                                            break;
                                        }
                                    }
                                }
                                // All fields must have values
                                if (fv_count < site.field_count) break :inline_push;
                                // Find the closing ')' of .push(...)
                                while (fv_pos < cc.len and cc[fv_pos] == '}') fv_pos += 1; // skip '}'
                                while (fv_pos < cc.len and cc[fv_pos] == ' ') fv_pos += 1;
                                if (fv_pos >= cc.len or cc[fv_pos] != ')') break :inline_push;
                                fv_pos += 1; // skip ')'
                                // Emit column writes + index push
                                for (site.field_names[0..site.field_count], 0..) |fname, fi| {
                                    w.print("__col_{s}[__col_idx] = {s}; ", .{ fname, field_vals[fi] }) catch { w_errs += 1; };
                                }
                                w.print("{s}.push(__col_idx++)", .{push_var}) catch { w_errs += 1; };
                                src_pos = fv_pos;
                                continue :char_loop;
                            }

                            // === SOA Transform: match factory functions and rewrite body ===
                            if (valid_alloc_sites.count() > 0 and std.mem.startsWith(u8, cc[src_pos..], "function ") and
                                (src_pos == 0 or !isIdentChar(cc[src_pos - 1])))
                            alloc_match: {
                                // Extract function name after "function " for O(1) lookup
                                const after_kw = src_pos + 9;
                                var name_end = after_kw;
                                while (name_end < cc.len and ident_char[cc[name_end]]) name_end += 1;
                                const func_name = cc[after_kw..name_end];
                                if (func_name.len == 0 or name_end >= cc.len or cc[name_end] != '(') break :alloc_match;
                                const site_idx = valid_alloc_sites.get(func_name) orelse break :alloc_match;
                                const site = alloc_sites.items[site_idx];
                                const after_name = name_end;

                                    // Found an alloc site. Skip params (track parens) then find body '{'.
                                    var scan = after_name;
                                    if (scan < cc.len and cc[scan] == '(') {
                                        var paren_depth: u32 = 1;
                                        scan += 1;
                                        while (scan < cc.len and paren_depth > 0) : (scan += 1) {
                                            if (cc[scan] == '(') paren_depth += 1 else if (cc[scan] == ')') paren_depth -= 1;
                                        }
                                    }
                                    while (scan < cc.len and cc[scan] != '{') scan += 1;
                                    if (scan >= cc.len) break :alloc_match;

                                    const first_param = after_name + 1;
                                    const is_destructured = first_param < cc.len and cc[first_param] == '{';

                                    // === Validate params BEFORE emitting anything ===
                                    var param_names: [16][]const u8 = .{""} ** 16;

                                    if (is_destructured) {
                                        // Destructured: verify all alloc fields appear as bindings
                                        var close_paren = first_param;
                                        var pd: u32 = 0;
                                        while (close_paren < cc.len) : (close_paren += 1) {
                                            if (cc[close_paren] == '(') pd += 1 else if (cc[close_paren] == ')') {
                                                if (pd == 0) break;
                                                pd -= 1;
                                            }
                                        }
                                        const param_text = cc[first_param..@min(close_paren, cc.len)];
                                        var all_found = true;
                                        for (site.field_names[0..site.field_count]) |fname| {
                                            if (fname.len == 0) { all_found = false; break; }
                                            var found = false;
                                            var sp: usize = 0;
                                            while (sp + fname.len <= param_text.len) : (sp += 1) {
                                                if (std.mem.eql(u8, param_text[sp .. sp + fname.len], fname)) {
                                                    const before_ok = sp == 0 or !isIdentChar(param_text[sp - 1]);
                                                    const after_ok = sp + fname.len >= param_text.len or !isIdentChar(param_text[sp + fname.len]);
                                                    if (before_ok and after_ok) { found = true; break; }
                                                }
                                            }
                                            if (!found) { all_found = false; break; }
                                        }
                                        if (!all_found) break :alloc_match;
                                        // Use field names directly as binding names
                                        for (site.field_names[0..site.field_count], 0..) |fname, fi| {
                                            param_names[fi] = fname;
                                        }
                                    } else {
                                        // Positional: extract all param names, then use arg_indices to map
                                        var all_params: [16][]const u8 = .{""} ** 16;
                                        var total_params: usize = 0;
                                        var ps = after_name + 1;
                                        var params_valid = true;
                                        // Extract up to 16 params
                                        while (total_params < 16 and ps < scan) {
                                            while (ps < scan and (cc[ps] == ' ' or cc[ps] == '\n')) ps += 1;
                                            if (ps >= scan or cc[ps] == ')') break;
                                            var pe2 = ps;
                                            while (pe2 < scan and cc[pe2] != ',' and cc[pe2] != ')') pe2 += 1;
                                            var pe3 = pe2;
                                            while (pe3 > ps and (cc[pe3 - 1] == ' ' or cc[pe3 - 1] == '\n')) pe3 -= 1;
                                            if (pe3 <= ps) { params_valid = false; break; }
                                            const ptext = cc[ps..pe3];
                                            for (ptext) |c| {
                                                if (!isIdentChar(c)) { params_valid = false; break; }
                                            }
                                            if (!params_valid) break;
                                            all_params[total_params] = ptext;
                                            total_params += 1;
                                            ps = if (pe2 < scan and cc[pe2] == ',') pe2 + 1 else pe2;
                                        }
                                        if (!params_valid) break :alloc_match;
                                        // Map fields via arg_indices
                                        for (site.arg_indices[0..site.field_count], 0..) |ai, fi| {
                                            if (ai >= total_params) { params_valid = false; break; }
                                            param_names[fi] = all_params[ai];
                                        }
                                        if (!params_valid) break :alloc_match;
                                    }

                                    // === All validation passed — now emit ===
                                    if (site.is_constructor) {
                                        // Skip constructors — write original function unchanged and skip past body
                                        const body_end2 = skipJsFunctionBody(cc, scan);
                                        w.writeAll(cc[src_pos..body_end2]) catch { w_errs += 1; };
                                        src_pos = body_end2;
                                        continue :char_loop;
                                    }

                                    // Skip SOA for "init-block" factories: if >80% of calls
                                    // are in a contiguous region (e.g., Diagnostics = {diag(...), ...}),
                                    // the objects are static singletons — V8 handles these optimally.
                                    {
                                        var total_calls: usize = 0;
                                        var first_call: usize = cc.len;
                                        var last_call: usize = 0;
                                        var cs: usize = 0;
                                        while (cs + func_name.len + 1 < cc.len) : (cs += 1) {
                                            if (cs > 0 and ident_char[cc[cs - 1]]) continue;
                                            if (!std.mem.startsWith(u8, cc[cs..], func_name)) continue;
                                            if (cs + func_name.len < cc.len and cc[cs + func_name.len] == '(') {
                                                total_calls += 1;
                                                if (cs < first_call) first_call = cs;
                                                if (cs > last_call) last_call = cs;
                                            }
                                        }
                                        if (total_calls >= 50) {
                                            const span = if (last_call > first_call) last_call - first_call else 1;
                                            const density = (total_calls * 100) / (span / 100 + 1);
                                            // High density = many calls in small region = init block
                                            if (density > 5) {
                                                std.debug.print("[soa] Skip init-block factory: {s} ({d} calls in {d} bytes, density={d})\n", .{
                                                    func_name, total_calls, span, density,
                                                });
                                                const body_end3 = skipJsFunctionBody(cc, scan);
                                                w.writeAll(cc[src_pos..body_end3]) catch { w_errs += 1; };
                                                src_pos = body_end3;
                                                continue :char_loop;
                                            }
                                        }
                                    }

                                    // Check if this factory has provenance (container SOA)
                                    var has_provenance = false;
                                    for (provenance) |prov| {
                                        if (prov.site_idx == site_idx) { has_provenance = true; break; }
                                    }

                                    if (has_provenance) {
                                        // Check: does ANY field from this factory have active reads?
                                        // If not, skip factory rewrite entirely (avoids write overhead).
                                        var any_active = false;
                                        for (site.field_names[0..site.field_count]) |fname| {
                                            if (active_cols.contains(fname)) { any_active = true; break; }
                                        }
                                        if (!any_active) {
                                            // No active reads — skip factory rewrite, emit original body
                                            const body_skip = skipJsFunctionBody(cc, scan);
                                            w.writeAll(cc[src_pos..body_skip]) catch { w_errs += 1; };
                                            src_pos = body_skip;
                                            continue :char_loop;
                                        }
                                        // Container SOA: write to columns, return original object
                                        w.writeAll(cc[src_pos .. scan + 1]) catch { w_errs += 1; };
                                        w.writeAll("\n") catch { w_errs += 1; };
                                        w.writeAll("  const __idx = __col_idx++;\n") catch { w_errs += 1; };
                                        for (site.field_names[0..site.field_count], 0..) |fname, fi| {
                                            if (!active_cols.contains(fname)) continue;
                                            w.print("  __col_{s}[__idx] = {s};\n", .{ fname, param_names[fi] }) catch { w_errs += 1; };
                                        }
                                        w.writeAll("  return {") catch { w_errs += 1; };
                                        for (site.field_names[0..site.field_count], 0..) |fname, fi| {
                                            if (fi > 0) w.writeAll(", ") catch { w_errs += 1; };
                                            w.print("{s}: {s}", .{ fname, param_names[fi] }) catch { w_errs += 1; };
                                        }
                                        w.writeAll("};\n}\n") catch { w_errs += 1; };
                                    } else {
                                        // No container provenance — skip factory rewrite to avoid overhead
                                        std.debug.print("[soa] Skip no-provenance factory: {s}\n", .{func_name});
                                        const body_end4 = skipJsFunctionBody(cc, scan);
                                        w.writeAll(cc[src_pos..body_end4]) catch { w_errs += 1; };
                                        src_pos = body_end4;
                                        continue :char_loop;
                                    }

                                    // Skip the original function body
                                    const body_end = skipJsFunctionBody(cc, scan);
                                    src_pos = body_end;
                                    continue :char_loop;
                            }

                            // Try to match `function NAME(` using O(1) name lookup
                            var matched_func: ?WasmFunc = null;
                            var matched_func_idx: usize = 0;
                            var match_end: usize = 0;
                            var is_arrow_match = false;
                            if (std.mem.startsWith(u8, cc[src_pos..], "function ") and
                                (src_pos == 0 or !ident_char[cc[src_pos - 1]]))
                            {
                                const after_kw = src_pos + 9;
                                var ne = after_kw;
                                while (ne < cc.len and ident_char[cc[ne]]) ne += 1;
                                if (ne > after_kw and ne < cc.len and cc[ne] == '(') {
                                    if (wasm_name_map.get(cc[after_kw..ne])) |wfi| {
                                        matched_func = wasm_func_list.items[wfi];
                                        matched_func_idx = wfi;
                                        match_end = ne;
                                    }
                                }
                            }

                            // Check for arrow function replacement at this position
                            if (matched_func == null) {
                                for (arrow_replacements[0..arrow_count]) |ar| {
                                    if (ar.brace_pos == @as(u32, @intCast(src_pos))) {
                                        matched_func = wasm_func_list.items[ar.func_idx];
                                        matched_func_idx = ar.func_idx;
                                        is_arrow_match = true;
                                        // For arrows, match_end points to just before '(' in the params
                                        // We need to find the '(' before ') => {'
                                        // Scan backwards from brace_pos to find params
                                        var scan_back = src_pos; // at '{'
                                        if (scan_back >= 4) scan_back -= 4; // skip ' => '
                                        // Now find the matching '('
                                        while (scan_back > 0 and cc[scan_back] != '(') scan_back -= 1;
                                        match_end = scan_back; // points to '('
                                        break;
                                    }
                                }
                            }

                            // Check for shorthand method syntax: `NAME(` using O(1) lookup
                            if (matched_func == null and ident_char[cc[src_pos]] and
                                (src_pos == 0 or !ident_char[cc[src_pos - 1]]))
                            {
                                // Extract identifier at src_pos
                                var ne2 = src_pos;
                                while (ne2 < cc.len and ident_char[cc[ne2]]) ne2 += 1;
                                if (ne2 > src_pos and ne2 < cc.len and cc[ne2] == '(') {
                                    if (wasm_name_map.get(cc[src_pos..ne2])) |wfi| {
                                        const wf_entry = wasm_func_list.items[wfi];
                                        if (!wf_entry.is_anon) {
                                            // Verify this is a method DEFINITION, not a function CALL
                                            var paren_depth: i32 = 1;
                                            var scan_fwd = ne2 + 1;
                                            while (scan_fwd < cc.len and paren_depth > 0) : (scan_fwd += 1) {
                                                if (cc[scan_fwd] == '(') paren_depth += 1;
                                                if (cc[scan_fwd] == ')') paren_depth -= 1;
                                            }
                                            while (scan_fwd < cc.len and (cc[scan_fwd] == ' ' or cc[scan_fwd] == '\n' or cc[scan_fwd] == '\r' or cc[scan_fwd] == '\t')) scan_fwd += 1;
                                            if (scan_fwd < cc.len and cc[scan_fwd] == '{') {
                                                // Exclude `function ` prefix (already handled above)
                                                if (!(src_pos >= 9 and std.mem.eql(u8, cc[src_pos - 9 .. src_pos], "function "))) {
                                                    // Exclude `=> {`
                                                    if (!(src_pos >= 4 and std.mem.eql(u8, cc[src_pos - 4 .. src_pos], "=> {"))) {
                                                        matched_func = wf_entry;
                                                        matched_func_idx = wfi;
                                                        match_end = ne2;
                                                    }
                                                }
                                            }
                                        }
                                    }
                                }
                            }

                            if (matched_func) |mf| {
                                // Decide: trampoline to WASM or keep as JS?
                                const calls_wasm = if (!mf.is_recursive)
                                    bodyCallsWasmFunc(cc, match_end, mf.name, wasm_func_list.items)
                                else
                                    false;
                                if (!shouldTrampolineToWasm(mf, calls_wasm)) {
                                    w.writeAll(cc[src_pos .. src_pos + 1]) catch { w_errs += 1; };
                                    src_pos += 1;
                                    continue;
                                }

                                // Found a WASM function declaration. Find opening brace.
                                const brace_pos = if (is_arrow_match)
                                    src_pos // for arrow functions, src_pos IS the brace
                                else
                                    std.mem.indexOfPos(u8, cc, match_end, "{") orelse {
                                        // No brace found — write char and advance
                                        w.writeAll(cc[src_pos .. src_pos + 1]) catch { w_errs += 1; };
                                        src_pos += 1;
                                        continue;
                                    };

                                // Write everything up to and including the opening brace
                                w.writeAll(cc[src_pos .. brace_pos + 1]) catch { w_errs += 1; };

                                const scan = skipJsFunctionBody(cc, brace_pos);

                                // Parse param names from original source
                                const params_start = match_end + 1; // after '('
                                const params_end = std.mem.indexOfPos(u8, cc, params_start, ")") orelse params_start;
                                const params_str = cc[params_start..params_end];

                                // Split param names for individual access
                                var param_names: [24][]const u8 = undefined;
                                var param_count: u32 = 0;
                                {
                                    var ppos: usize = 0;
                                    while (ppos < params_str.len and param_count < 24) {
                                        // Skip whitespace
                                        while (ppos < params_str.len and (params_str[ppos] == ' ' or params_str[ppos] == '\t')) ppos += 1;
                                        const pstart = ppos;
                                        while (ppos < params_str.len and params_str[ppos] != ',' and params_str[ppos] != ' ' and params_str[ppos] != '=') ppos += 1;
                                        if (ppos > pstart) {
                                            param_names[param_count] = params_str[pstart..ppos];
                                            param_count += 1;
                                        }
                                        // Skip default value: = expression until comma
                                        if (ppos < params_str.len and params_str[ppos] == '=') {
                                            while (ppos < params_str.len and params_str[ppos] != ',') ppos += 1;
                                        }
                                        // Skip comma
                                        if (ppos < params_str.len and params_str[ppos] == ',') ppos += 1;
                                    }
                                }

                                if (mf.array_of_struct_args != 0) {
                                    // Array-of-struct args — materialize JS array of objects
                                    // into flat WASM pool, pass (pool_ptr, count, ...scalars)
                                    w.writeAll("\n") catch { w_errs += 1; };
                                    w.writeAll("  const __sp0 = __wasmStackSave();\n") catch { w_errs += 1; };
                                    var si: u32 = 0;
                                    while (si < param_count) : (si += 1) {
                                        if (mf.array_of_struct_args & (@as(u8, 1) << @intCast(si)) != 0) {
                                            const fc = mf.struct_field_counts[si];
                                            if (fc == 0) continue;
                                            const stride = @as(u32, fc);
                                            // Allocate pool: arr.length * field_count * 4 bytes
                                            w.print("  const __n{d} = {s}.length;\n", .{ si, param_names[si] }) catch { w_errs += 1; };
                                            w.print("  const __p{d} = __wasmStackAlloc(__n{d} * {d});\n", .{ si, si, stride * 4 }) catch { w_errs += 1; };
                                            w.print("  const __b{d} = __p{d} >> 2;\n", .{ si, si }) catch { w_errs += 1; };
                                            // Copy loop: write each element's fields
                                            w.print("  for (let __i = 0; __i < __n{d}; __i++) {{\n", .{si}) catch { w_errs += 1; };
                                            for (mf.struct_field_names[si][0..fc], 0..) |field_name, fi| {
                                                if (field_name.len == 0) continue;
                                                if (stride == 1) {
                                                    w.print("    __m[__b{d} + __i] = {s}[__i].{s};\n", .{ si, param_names[si], field_name }) catch { w_errs += 1; };
                                                } else {
                                                    w.print("    __m[__b{d} + __i * {d} + {d}] = {s}[__i].{s};\n", .{ si, stride, fi, param_names[si], field_name }) catch { w_errs += 1; };
                                                }
                                            }
                                            w.writeAll("  }\n") catch { w_errs += 1; };
                                        }
                                    }
                                    // Call WASM: AOS args become pool_ptr, scalars pass through
                                    w.print("  const __r = __wasm.exports.{s}(", .{mf.name}) catch { w_errs += 1; };
                                    {
                                        var first_arg = true;
                                        si = 0;
                                        while (si < param_count) : (si += 1) {
                                            if (!first_arg) w.writeAll(", ") catch { w_errs += 1; };
                                            if (mf.array_of_struct_args & (@as(u8, 1) << @intCast(si)) != 0) {
                                                w.print("__p{d}", .{si}) catch { w_errs += 1; };
                                            } else {
                                                w.writeAll(param_names[si]) catch { w_errs += 1; };
                                            }
                                            first_arg = false;
                                        }
                                    }
                                    w.writeAll(");\n") catch { w_errs += 1; };
                                    w.writeAll("  __wasmStackRestore(__sp0);\n  return __r;\n}") catch { w_errs += 1; };
                                } else if (mf.struct_args != 0 and mf.array_args == 0) {
                                    // Struct args — flatten JS object fields into WASM linear memory
                                    w.writeAll("\n") catch { w_errs += 1; };
                                    w.writeAll("  const __sp0 = __wasmStackSave();\n") catch { w_errs += 1; };
                                    // Allocate + write fields for each struct arg
                                    var si: u32 = 0;
                                    while (si < param_count) : (si += 1) {
                                        if (mf.struct_args & (@as(u8, 1) << @intCast(si)) != 0) {
                                            const fc = mf.struct_field_counts[si];
                                            if (fc == 0) continue;
                                            // Allocate: fieldCount * 4 bytes (i32 per field)
                                            w.print("  const __p{d} = __wasmStackAlloc({d});\n", .{ si, @as(u32, fc) * 4 }) catch { w_errs += 1; };
                                            // Write each field: __m[(__pN >> 2) + offset] = param.fieldName
                                            for (mf.struct_field_names[si][0..fc], 0..) |field_name, fi| {
                                                if (field_name.len == 0) continue;
                                                if (fi == 0) {
                                                    w.print("  __m[__p{d} >> 2] = {s}.{s};\n", .{ si, param_names[si], field_name }) catch { w_errs += 1; };
                                                } else {
                                                    w.print("  __m[(__p{d} >> 2) + {d}] = {s}.{s};\n", .{ si, fi, param_names[si], field_name }) catch { w_errs += 1; };
                                                }
                                            }
                                        }
                                    }
                                    // Call WASM: struct args become pointers, scalars pass through
                                    w.print("  const __r = __wasm.exports.{s}(", .{mf.name}) catch { w_errs += 1; };
                                    {
                                        var first_arg = true;
                                        si = 0;
                                        while (si < param_count) : (si += 1) {
                                            if (!first_arg) w.writeAll(", ") catch { w_errs += 1; };
                                            if (mf.struct_args & (@as(u8, 1) << @intCast(si)) != 0) {
                                                w.print("__p{d}", .{si}) catch { w_errs += 1; };
                                            } else {
                                                w.writeAll(param_names[si]) catch { w_errs += 1; };
                                            }
                                            first_arg = false;
                                        }
                                    }
                                    w.writeAll(");\n") catch { w_errs += 1; };
                                    w.writeAll("  __wasmStackRestore(__sp0);\n  return __r;\n}") catch { w_errs += 1; };
                                } else if (mf.array_args == 0 and mf.struct_args == 0) {
                                    // No array or struct args — simple scalar trampoline
                                    w.print(" return __wasm.exports.{s}({s}); }}", .{ mf.name, params_str }) catch { w_errs += 1; };
                                } else {
                                    // Array args — copy to/from WASM linear memory
                                    w.writeAll("\n") catch { w_errs += 1; };

                                    // SOA fast path for array-arg functions:
                                    // If elements are SOA handles, data is already in WASM memory.
                                    // Find matching SOA field by looking at batch functions with struct_fields.
                                    if (alloc_sites.items.len > 0 and mf.struct_args == 0) {
                                        // Find a batch function with a single struct field matching an SOA site
                                        var soa_matched = false;
                                        for (wasm_func_list.items) |bf| {
                                            if (bf.struct_args == 0) continue;
                                            // Find first struct arg
                                            var bai: u32 = 0;
                                            while (bai < 8) : (bai += 1) {
                                                if (bf.struct_args & (@as(u8, 1) << @intCast(bai)) != 0) break;
                                            }
                                            if (bai >= 8) continue;
                                            if (bf.struct_field_counts[bai] != 1) continue; // only single-field for zero-copy
                                            const bfield = bf.struct_field_names[bai][0];
                                            if (bfield.len == 0) continue;
                                            // Match against SOA sites
                                            for (alloc_sites.items, 0..) |site, si| {
                                                for (site.field_names[0..site.field_count]) |sf| {
                                                    if (std.mem.eql(u8, sf, bfield)) {
                                                        // Use batch function (flat access) not original (gather/indirect).
                                                        // readKind_batch reads flat i32 values = SOA layout.
                                                        // sumKinds reads pointers-to-structs = AOS with indirection.
                                                        w.print("  if ({s}.__soa === {d}) return __wasm.exports.{s}_batch(__soa_{d}_{s}.byteOffset, {s}.length);\n", .{
                                                            param_names[0], si, bf.name, si, bfield, param_names[0],
                                                        }) catch { w_errs += 1; };
                                                        soa_matched = true;
                                                        break;
                                                    }
                                                }
                                                if (soa_matched) break;
                                            }
                                            if (soa_matched) break;
                                        }
                                    }

                                    // i32 tier: Int32Array (4 bytes/elem), byte offset = idx << 2
                                    // f64 tier: Float64Array (8 bytes/elem), byte offset = idx << 3
                                    // Float64Array lets copy-back handle type conversion (e.g.
                                    // Uint8ClampedArray.set(Float64Array) rounds+clamps correctly).
                                    const mem_view: []const u8 = if (mf.is_f64) "__m_f64" else "__m";
                                    const alloc_shift: []const u8 = if (mf.is_f64) " << 3" else " << 2";
                                    const elem_shift: []const u8 = if (mf.is_f64) " >> 3" else " >> 2";

                                    // Zero-copy fast path: if ALL array args are backed by WASM memory
                                    // (allocated via __wasmArray), skip copy entirely.
                                    // arr.buffer === __wbuf means the TypedArray is a view into WASM linear memory.
                                    // Zero-copy condition: all array args backed by WASM memory
                                    w.writeAll("  if (") catch { w_errs += 1; };
                                    {
                                        var first_zc = true;
                                        var zi: u32 = 0;
                                        while (zi < param_count) : (zi += 1) {
                                            if (mf.array_args & (@as(u8, 1) << @intCast(zi)) != 0) {
                                                if (!first_zc) w.writeAll(" && ") catch { w_errs += 1; };
                                                w.print("{s}.buffer === __wbuf", .{param_names[zi]}) catch { w_errs += 1; };
                                                first_zc = false;
                                            }
                                        }
                                    }
                                    w.print(") return __wasm.exports.{s}(", .{mf.name}) catch { w_errs += 1; };
                                    {
                                        var first_arg = true;
                                        var zi: u32 = 0;
                                        while (zi < param_count) : (zi += 1) {
                                            if (!first_arg) w.writeAll(", ") catch { w_errs += 1; };
                                            if (mf.array_args & (@as(u8, 1) << @intCast(zi)) != 0) {
                                                w.print("{s}.byteOffset", .{param_names[zi]}) catch { w_errs += 1; };
                                            } else {
                                                w.writeAll(param_names[zi]) catch { w_errs += 1; };
                                            }
                                            first_arg = false;
                                        }
                                        zi = 0;
                                        while (zi < param_count) : (zi += 1) {
                                            if (mf.length_args & (@as(u8, 1) << @intCast(zi)) != 0) {
                                                w.print(", {s}.length", .{param_names[zi]}) catch { w_errs += 1; };
                                            }
                                        }
                                    }
                                    w.writeAll(");\n") catch { w_errs += 1; };

                                    // Stack-scoped copy: save SP, allocate, copy, call, restore.
                                    // Identity cache: skip copy when same array ref on repeated calls.
                                    const read_only_args = mf.array_args & ~mf.mutated_args;
                                    // Format function index for __last_fn tracking
                                    var mfi_buf: [16]u8 = undefined;
                                    const mfi_str = std.fmt.bufPrint(&mfi_buf, "{d}", .{matched_func_idx}) catch "0";
                                    w.writeAll("  const __sp0 = __wasmStackSave();\n") catch { w_errs += 1; };
                                    var i: u32 = 0;
                                    while (i < param_count) : (i += 1) {
                                        if (mf.array_args & (@as(u8, 1) << @intCast(i)) != 0) {
                                            var idx_buf: [8]u8 = undefined;
                                            const idx_str = std.fmt.bufPrint(&idx_buf, "{d}", .{i}) catch "0";
                                            // Stack-allocate: __pN = __wasmStackAlloc(arr.length * bytes_per_elem)
                                            w.print("  const __p{s} = __wasmStackAlloc({s}.length{s});\n", .{ idx_str, param_names[i], alloc_shift }) catch { w_errs += 1; };

                                            if (read_only_args & (@as(u8, 1) << @intCast(i)) != 0) {
                                                // Read-only: cache by reference identity. Skip copy when
                                                // the same array reference is passed on repeated calls.
                                                // Trade-off: if user mutates array elements between calls
                                                // with the same ref, WASM memory will have stale data.
                                                // This is acceptable for the common loop pattern:
                                                //   for (i=0; i<N; i++) result = fn(arr);
                                                var cid_buf: [32]u8 = undefined;
                                                const cid = std.fmt.bufPrint(&cid_buf, "{d}_{d}", .{ matched_func_idx, i }) catch "0_0";
                                                const pn = param_names[i];
                                                // if (__last_fn !== N || arr.length < 128 || arr !== __cN_M) { copy; cache; }
                                                // Small arrays (< 128 = 512 bytes) always re-copy — too cheap to
                                                // risk stale data from same-ref mutation (e.g. crypto_verify_32).
                                                // Large arrays use identity cache for performance.
                                                w.print("  if (__last_fn !== {s} || {s}.length < 128 || {s} !== __c{s}) {{ {s}.set({s}, __p{s}{s}); __c{s} = {s}; }}\n", .{
                                                    mfi_str, pn, pn, cid, mem_view, pn, idx_str, elem_shift, cid, pn,
                                                }) catch { w_errs += 1; };
                                            } else if (mf.read_array_args & (@as(u8, 1) << @intCast(i)) != 0) {
                                                // Mutated + read: must copy in (function reads then writes)
                                                w.print("  {s}.set({s}, __p{s}{s});\n", .{ mem_view, param_names[i], idx_str, elem_shift }) catch { w_errs += 1; };
                                            } else {
                                                // Write-only: skip copy-in (data will be overwritten)
                                            }

                                        }
                                    }
                                    // Track which function last wrote to WASM memory (cross-function cache safety)
                                    if (any_cacheable_args) {
                                        w.print("  __last_fn = {s};\n", .{mfi_str}) catch { w_errs += 1; };
                                    }
                                    // Call WASM — __pN is already a byte offset from stack allocator
                                    w.print("  const __r = __wasm.exports.{s}(", .{mf.name}) catch { w_errs += 1; };
                                    i = 0;
                                    while (i < param_count) : (i += 1) {
                                        if (i > 0) w.writeAll(", ") catch { w_errs += 1; };
                                        if (mf.array_args & (@as(u8, 1) << @intCast(i)) != 0) {
                                            w.print("__p{d}", .{i}) catch { w_errs += 1; };
                                        } else {
                                            w.writeAll(param_names[i]) catch { w_errs += 1; };
                                        }
                                    }
                                    // Append .length for array args that use get_length
                                    i = 0;
                                    while (i < param_count) : (i += 1) {
                                        if (mf.length_args & (@as(u8, 1) << @intCast(i)) != 0) {
                                            w.print(", {s}.length", .{param_names[i]}) catch { w_errs += 1; };
                                        }
                                    }
                                    w.writeAll(");\n") catch { w_errs += 1; };
                                    // Copy modified arrays back from WASM memory (only mutated ones)
                                    i = 0;
                                    while (i < param_count) : (i += 1) {
                                        if (mf.mutated_args & (@as(u8, 1) << @intCast(i)) != 0) {
                                            const pn = param_names[i];
                                            // TypedArray fast path + plain Array fallback
                                            w.print("  if ({s}.set) {s}.set({s}.subarray(__p{d}{s}, (__p{d}{s}) + {s}.length));\n", .{
                                                pn, pn, mem_view, i, elem_shift, i, elem_shift, pn,
                                            }) catch { w_errs += 1; };
                                            w.print("  else for (let __i = 0; __i < {s}.length; __i++) {s}[__i] = {s}[(__p{d}{s}) + __i];\n", .{
                                                pn, pn, mem_view, i, elem_shift,
                                            }) catch { w_errs += 1; };
                                        }
                                    }
                                    w.writeAll("  __wasmStackRestore(__sp0);\n  return __r;\n}") catch { w_errs += 1; };
                                }

                                // src_pos = scan (past the closing brace we already consumed)
                                src_pos = scan;
                            } else {
                                // === SOA read-site transform ===
                                // Match `VAR[expr].field` where VAR is in provenance map
                                // Transform to `__soa_N_field[VAR[expr].__idx]`
                                var prov_matched = false;
                                if (provenance.len > 0 and isIdentChar(cc[src_pos]) and
                                    (src_pos == 0 or !isIdentChar(cc[src_pos - 1])))
                                {
                                    for (provenance, 0..) |prov, pi_r| {
                                        if (src_pos + prov.var_name.len + 1 >= cc.len) continue;
                                        if (!std.mem.startsWith(u8, cc[src_pos..], prov.var_name)) continue;
                                        const after_var = src_pos + prov.var_name.len;
                                        if (cc[after_var] != '[') continue;
                                        // Ensure word boundary (not part of longer identifier)
                                        if (after_var < cc.len and isIdentChar(cc[after_var])) {
                                            // cc[after_var] == '[' which is not ident, so this is fine
                                        }
                                        // Find matching ']' (handle nested brackets)
                                        var depth: u32 = 1;
                                        var k = after_var + 1;
                                        while (k < cc.len and depth > 0) : (k += 1) {
                                            if (cc[k] == '[') depth += 1;
                                            if (cc[k] == ']') depth -= 1;
                                        }
                                        // k is now past the ']'
                                        if (k >= cc.len or cc[k] != '.') continue;
                                        // Read field name after '.'
                                        const fstart = k + 1;
                                        var fend = fstart;
                                        while (fend < cc.len and isIdentChar(cc[fend])) fend += 1;
                                        if (fend == fstart) continue;
                                        const fname = cc[fstart..fend];
                                        // Skip .length, .push, .indexOf etc — only match SOA fields
                                        const site = alloc_sites.items[prov.site_idx];
                                        var field_match = false;
                                        for (site.field_names[0..site.field_count]) |sf| {
                                            if (std.mem.eql(u8, sf, fname)) { field_match = true; break; }
                                        }
                                        if (!field_match) continue;
                                        const idx_expr = cc[after_var + 1 .. k - 1]; // content between [ ]
                                        // Try to recursively transform idx_expr if it contains VAR[inner].field
                                        var inner_transformed = false;
                                        if (prov.decl_end > 0) {
                                            for (provenance) |ip| {
                                                if (ip.decl_end == 0) continue;
                                                if (!std.mem.startsWith(u8, idx_expr, ip.var_name)) continue;
                                                const ia = ip.var_name.len;
                                                if (ia >= idx_expr.len or idx_expr[ia] != '[') continue;
                                                // Find matching ] in idx_expr
                                                var d3: u32 = 1;
                                                var k3 = ia + 1;
                                                while (k3 < idx_expr.len and d3 > 0) : (k3 += 1) {
                                                    if (idx_expr[k3] == '[') d3 += 1;
                                                    if (idx_expr[k3] == ']') d3 -= 1;
                                                }
                                                if (k3 >= idx_expr.len or idx_expr[k3] != '.') continue;
                                                const ifs = k3 + 1;
                                                var ife = ifs;
                                                while (ife < idx_expr.len and isIdentChar(idx_expr[ife])) ife += 1;
                                                if (ife == ifs) continue;
                                                const ifn = idx_expr[ifs..ife];
                                                // Verify it's an SOA field
                                                const isite = alloc_sites.items[ip.site_idx];
                                                var if_match = false;
                                                for (isite.field_names[0..isite.field_count]) |sf| {
                                                    if (std.mem.eql(u8, sf, ifn)) { if_match = true; break; }
                                                }
                                                if (!if_match) continue;
                                                const inner_idx = idx_expr[ia + 1 .. k3 - 1];
                                                // Emit: __soa_N_field[__VAR_base + (__soa_M_innerfield[__IVAR_base + (inner_idx)])]
                                                // Find inner provenance index
                                                var ip_idx: usize = 0;
                                                for (provenance, 0..) |pp, ppi| {
                                                    if (std.mem.eql(u8, pp.var_name, ip.var_name)) { ip_idx = ppi; break; }
                                                }
                                                w.print("__col_{s}[__sb{d} + (__col_{s}[__sb{d} + ({s})])]", .{
                                                    fname, pi_r,
                                                    ifn, ip_idx, inner_idx,
                                                }) catch { w_errs += 1; };
                                                inner_transformed = true;
                                                break;
                                            }
                                        }
                                        if (!inner_transformed) {
                                            if (prov.decl_end > 0) {
                                                // Index-aligned: VAR[expr].field → __soa_N_field[__VAR_soa_base + (expr)]
                                                w.print("__col_{s}[__sb{d} + ({s})]", .{
                                                    fname, pi_r, idx_expr,
                                                }) catch { w_errs += 1; };
                                            } else {
                                                // SOA column read via __idx: VAR[expr].field → __soa_N_field[VAR[expr].__idx]
                                                w.print("__col_{s}[{s}[{s}].__idx]", .{
                                                    fname, prov.var_name, idx_expr,
                                                }) catch { w_errs += 1; };
                                            }
                                        }
                                        src_pos = fend;
                                        prov_matched = true;
                                        break;
                                    }
                                }
                                if (prov_matched) continue :char_loop;
                                // === Inter-procedural parameter provenance ===
                                // Match `PARAM[expr].field` where PARAM is a function parameter
                                // that receives a provenance container at the call site.
                                // Rewrite to `__col_field[__CONTAINER_soa_base + (expr)]`
                                if (param_provs.len > 0 and isIdentChar(cc[src_pos]) and
                                    (src_pos == 0 or !isIdentChar(cc[src_pos - 1])))
                                {
                                    for (param_provs) |pp| {
                                        // Scope check: src_pos must be within the function body
                                        if (src_pos < pp.func_body_start or src_pos >= pp.func_body_end) continue;
                                        // Scope check: __CONTAINER_soa_base must be defined.
                                        // It's defined at decl_end. If decl_end is outside this function body,
                                        // the soa_base variable won't be in scope → skip.
                                        const prov_pp_check = provenance[pp.prov_idx];
                                        if (prov_pp_check.decl_end > 0 and
                                            (prov_pp_check.decl_end < pp.func_body_start or prov_pp_check.decl_end >= pp.func_body_end))
                                            continue;
                                        if (src_pos + pp.param_name.len + 1 >= cc.len) continue;
                                        if (!std.mem.startsWith(u8, cc[src_pos..], pp.param_name)) continue;
                                        const after_param = src_pos + pp.param_name.len;
                                        if (after_param >= cc.len or cc[after_param] != '[') continue;
                                        // Verify word boundary
                                        if (after_param < cc.len and isIdentChar(cc[after_param]) and cc[after_param] != '[') continue;
                                        // Find matching ']'
                                        var depth_pp: u32 = 1;
                                        var k_pp = after_param + 1;
                                        while (k_pp < cc.len and depth_pp > 0) : (k_pp += 1) {
                                            if (cc[k_pp] == '[') depth_pp += 1;
                                            if (cc[k_pp] == ']') depth_pp -= 1;
                                        }
                                        // Must be followed by '.field'
                                        if (k_pp >= cc.len or cc[k_pp] != '.') continue;
                                        const fstart_pp = k_pp + 1;
                                        var fend_pp = fstart_pp;
                                        while (fend_pp < cc.len and isIdentChar(cc[fend_pp])) fend_pp += 1;
                                        if (fend_pp == fstart_pp) continue;
                                        const fname_pp = cc[fstart_pp..fend_pp];
                                        // Skip method calls and assignments
                                        if (fend_pp < cc.len and cc[fend_pp] == '(') continue;
                                        var eq_pp = fend_pp;
                                        while (eq_pp < cc.len and cc[eq_pp] == ' ') eq_pp += 1;
                                        if (eq_pp < cc.len and cc[eq_pp] == '=' and (eq_pp + 1 >= cc.len or cc[eq_pp + 1] != '=')) continue;
                                        if (eq_pp + 1 < cc.len and cc[eq_pp + 1] == '=' and (cc[eq_pp] == '+' or cc[eq_pp] == '-' or cc[eq_pp] == '|' or cc[eq_pp] == '&')) continue;
                                        // Verify it's a known SOA field
                                        const prov_pp = provenance[pp.prov_idx];
                                        const site_pp = alloc_sites.items[prov_pp.site_idx];
                                        var field_match_pp = false;
                                        for (site_pp.field_names[0..site_pp.field_count]) |sf| {
                                            if (std.mem.eql(u8, sf, fname_pp)) { field_match_pp = true; break; }
                                        }
                                        if (!field_match_pp) continue;
                                        // Emit: __col_field[__CONTAINER_soa_base + (expr)]
                                        // Use container name (module-scope var) not parameter name.
                                        const idx_expr_pp = cc[after_param + 1 .. k_pp - 1];
                                        w.print("__col_{s}[__sb{d} + ({s})]", .{
                                            fname_pp, pp.prov_idx, idx_expr_pp,
                                        }) catch { w_errs += 1; };
                                        src_pos = fend_pp;
                                        prov_matched = true;
                                        break;
                                    }
                                }
                                if (prov_matched) continue :char_loop;
                                // === Local variable alias read-site transform ===
                                // Match `ALIAS.field` where ALIAS was detected as `const ALIAS = PROV_VAR[expr]`
                                // Transform to `__col_field[__PROV_soa_base + (expr)]`
                                // Scope-aware: only match aliases whose scope contains src_pos
                                if (aliases.len > 0 and isIdentChar(cc[src_pos]) and
                                    (src_pos == 0 or !isIdentChar(cc[src_pos - 1])))
                                {
                                    // Extract identifier at src_pos
                                    var ae = src_pos;
                                    while (ae < cc.len and ident_char[cc[ae]]) ae += 1;
                                    if (ae > src_pos and ae < cc.len and cc[ae] == '.') {
                                        const candidate = cc[src_pos..ae];
                                        // Search all aliases with this name (may exist in multiple scopes)
                                        // Use linear scan since alias count is small (typically <128)
                                        for (aliases) |alias| {
                                            if (!std.mem.eql(u8, alias.alias_name, candidate)) continue;
                                            // Scope check: src_pos must be within alias's scope
                                            if (src_pos < alias.assign_pos or src_pos > alias.scope_end) continue;
                                            const prov = provenance[alias.prov_idx];
                                            // Read field name after '.'
                                            const afs = ae + 1;
                                            var afe = afs;
                                            while (afe < cc.len and ident_char[cc[afe]]) afe += 1;
                                            if (afe <= afs) continue;
                                            const afname = cc[afs..afe];
                                            // Skip method calls: alias.method(
                                            if (afe < cc.len and cc[afe] == '(') continue;
                                            // Verify it's a known SOA field
                                            const asite = alloc_sites.items[prov.site_idx];
                                            var af_match = false;
                                            for (asite.field_names[0..asite.field_count]) |sf| {
                                                if (std.mem.eql(u8, sf, afname)) { af_match = true; break; }
                                            }
                                            if (!af_match) continue;
                                            // Skip assignment: alias.field = (not a read)
                                            var aeq = afe;
                                            while (aeq < cc.len and cc[aeq] == ' ') aeq += 1;
                                            if (aeq < cc.len and cc[aeq] == '=' and (aeq + 1 >= cc.len or cc[aeq + 1] != '=')) continue;
                                            // Skip compound assignment: alias.field += / |= / etc
                                            if (aeq + 1 < cc.len and cc[aeq + 1] == '=' and (cc[aeq] == '+' or cc[aeq] == '-' or cc[aeq] == '|' or cc[aeq] == '&' or cc[aeq] == '*')) continue;
                                            // Emit: __col_field[PROV.__sb + (idx_expr)]
                                            w.print("__col_{s}[__sb{d} + ({s})]", .{
                                                afname, alias.prov_idx, alias.idx_expr,
                                            }) catch { w_errs += 1; };
                                            src_pos = afe;
                                            prov_matched = true;
                                            break;
                                        }
                                        if (prov_matched) continue :char_loop;
                                    }
                                }
                                // No match — copy one character
                                w.writeAll(cc[src_pos .. src_pos + 1]) catch { w_errs += 1; };
                                src_pos += 1;
                                if (src_pos % 50000 == 0) w.flush() catch { w_errs += 1; };
                            }
                        }
                        const loop_end_ts = std.time.milliTimestamp();
                        std.debug.print("[soa] char_loop done in {d}ms (slow={d} fast={d})\n", .{ loop_end_ts - loop_start_ts, slow_path_count, fast_path_count });
                        std.debug.print("[soa] Patch: {d} field reads rewritten (from {d} patches), src_pos={d}/{d}\n", .{ rs_rewrite_count, patches.items.len, src_pos, cc.len });
                        w.flush() catch |err| {
                            std.debug.print("[build] FLUSH ERROR: {}\n", .{err});
                        };
                    } else {
                        w.writeAll("// ERROR: Original source not available\n") catch { w_errs += 1; };
                    }

                    if (w_errs > 0) std.debug.print("[build] WARNING: {d} write errors during worker generation — output may be truncated\n", .{w_errs});
                    has_worker_files = true;
                    std.debug.print("[build] Module: {s}\n", .{mp_out});
                } else |_| {}
                } // module_path
            }

            // Generate config.capnp
            if (config_path) |cp| {
                if (std.fs.cwd().createFile(cp, .{})) |cf| {
                    defer cf.close();
                    var wn_buf: [256]u8 = undefined;
                    const wn = std.fmt.bufPrint(&wn_buf, "{s}-worker.js", .{output_base}) catch "worker.mjs";
                    var cf_buf: [4096]u8 = undefined;
                    var cf_state = cf.writer(&cf_buf);
                    const cw = &cf_state.interface;
                    defer cw.flush() catch |err| std.debug.print("[build] config flush: {}\n", .{err});
                    cw.print(
                        \\using Workerd = import "/workerd/workerd.capnp";
                        \\
                        \\const config :Workerd.Config = (
                        \\  services = [
                        \\    (name = "main", worker = .worker),
                        \\  ],
                        \\  sockets = [
                        \\    (name = "http", address = "*:8787", http = (), service = "main"),
                        \\  ],
                        \\);
                        \\
                        \\const worker :Workerd.Worker = (
                        \\  modules = [
                        \\    (name = "entrypoint", esModule = embed "{s}"),
                        \\    (name = "{s}", wasm = embed "{s}"),
                        \\  ],
                        \\  compatibilityDate = "2024-09-23",
                        \\  compatibilityFlags = ["nodejs_compat"],
                        \\);
                        \\
                    , .{ wn, wasm_filename, wasm_filename }) catch |err| std.debug.print("[build] config write: {}\n", .{err});
                    std.debug.print("[build] Config: {s}\n", .{cp});
                } else |_| {}
            }
        }
    }

    // Step 7: Build WASM static (with host imports for edgebox daemon AOT)
    // Skip when wasm_only — worker path only needs standalone.wasm + worker.mjs (generated in step 6f)
    if (!options.binary_only and !options.wasm_only) {
        std.debug.print("[build] Building WASM static with embedded bytecode...\n", .{});
        const wasm_result = if (source_dir_arg.len > 0)
            try runCommand(allocator, &.{
                "zig", "build", "-j4", "--prefix", out_prefix, "--cache-dir", zig_cache_path, "wasm-static", "-Doptimize=ReleaseSmall", source_dir_arg, bytecode_arg, cache_prefix_arg,
            })
        else
            try runCommand(allocator, &.{
                "zig", "build", "-j4", "--prefix", out_prefix, "--cache-dir", zig_cache_path, "wasm-static", "-Doptimize=ReleaseSmall", bytecode_arg, cache_prefix_arg,
            });
        defer {
            if (wasm_result.stdout) |s| allocator.free(s);
            if (wasm_result.stderr) |s| allocator.free(s);
        }

        const wasm_failed = switch (wasm_result.term) {
            .Exited => |code| code != 0,
            .Signal => true,
            .Stopped, .Unknown => true,
        };
        if (wasm_failed) {
            std.debug.print("[warn] WASM static build failed\n", .{});
            if (wasm_result.stderr) |err| {
                std.debug.print("{s}\n", .{err});
            }
        } else {
            // Copy from build output (wasm-static produces edgebox-static.wasm)
            var wasm_static_path_buf: [4096]u8 = undefined;
            const wasm_static_path = std.fmt.bufPrint(&wasm_static_path_buf, "{s}/bin/edgebox-static.wasm", .{out_prefix}) catch "zig-out/bin/edgebox-static.wasm";
            std.fs.cwd().copyFile(wasm_static_path, std.fs.cwd(), wasm_path, .{}) catch |err| {
                std.debug.print("[warn] Failed to copy WASM: {}\n", .{err});
            };
            if (std.fs.cwd().statFile(wasm_path)) |stat| {
                const size_mb = @as(f64, @floatFromInt(stat.size)) / 1024.0 / 1024.0;
                std.debug.print("[build] WASM: {s} ({d:.1}MB)\n", .{ wasm_path, size_mb });
            } else |_| {}
        }
    }

    // Generate binary path (no extension) — needed for summary even if skipped
    var binary_path_buf: [4096]u8 = undefined;
    const binary_path = std.fmt.bufPrint(&binary_path_buf, "{s}/{s}", .{ output_dir, output_base }) catch {
        std.debug.print("[error] Output path too long: {s}/{s}\n", .{ output_dir, output_base });
        std.process.exit(1);
    };

    // Step 7b: Build native binary using native (raw bytecode via @embedFile)
    // This avoids OOM from parsing 321MB C hex arrays by embedding bytecode directly
    // Uses the SAME frozen_module.zig as WASM, but embeds bytecode via linker
    if (!options.wasm_only) {
        std.debug.print("[build] Building native binary with embedded bytecode (native)...\n", .{});
        const native_result = if (source_dir_arg.len > 0)
            try runCommand(allocator, &.{
                "zig", "build", "-j4", "--prefix", out_prefix, "--cache-dir", zig_cache_path, "native", optimize_arg, frozen_optimize_arg, frozen_thin_optimize_arg, source_dir_arg, bytecode_arg, allocator_arg, cache_prefix_arg,
            })
        else
            try runCommand(allocator, &.{
                "zig", "build", "-j4", "--prefix", out_prefix, "--cache-dir", zig_cache_path, "native", optimize_arg, frozen_optimize_arg, frozen_thin_optimize_arg, bytecode_arg, allocator_arg, cache_prefix_arg,
            });
        defer {
            if (native_result.stdout) |s| allocator.free(s);
            if (native_result.stderr) |s| allocator.free(s);
        }

        const native_failed = switch (native_result.term) {
            .Exited => |code| code != 0,
            .Signal => true,
            .Stopped, .Unknown => true,
        };
        if (native_failed) {
            std.debug.print("[warn] Native-embed build failed (WASM/AOT still usable)\n", .{});
            if (native_result.stderr) |err| {
                std.debug.print("{s}\n", .{err});
            }
        } else {
            // Copy from build output with output name based on input
            // build.zig derives output name from source_dir basename (e.g., _tsc.js -> _tsc)
            var native_path_buf: [4096]u8 = undefined;
            const native_path = std.fmt.bufPrint(&native_path_buf, "{s}/bin/{s}", .{ out_prefix, output_base }) catch "zig-out/bin/edgebox-native";
            std.fs.cwd().copyFile(native_path, std.fs.cwd(), binary_path, .{}) catch |err| {
                std.debug.print("[warn] Failed to copy binary: {}\n", .{err});
            };
            if (std.fs.cwd().statFile(binary_path)) |stat| {
                const size_mb = @as(f64, @floatFromInt(stat.size)) / 1024.0 / 1024.0;
                std.debug.print("[build] Binary: {s} ({d:.1}MB)\n", .{ binary_path, size_mb });
            } else |_| {}
        }
    }

    // Worker-only summary (default path)
    if (options.wasm_only) {
        std.debug.print("\n[build] === Worker Build Complete ===\n\n", .{});
        std.debug.print("Files created:\n", .{});
        if (has_standalone_wasm) {
            const sw_path2 = std.fmt.bufPrint(&standalone_wasm_path_buf, "{s}/{s}-standalone.wasm", .{ output_dir, output_base }) catch "";
            if (std.fs.cwd().statFile(sw_path2)) |stat| {
                std.debug.print("  {s}  ({d} bytes)\n", .{ sw_path2, stat.size });
            } else |_| {
                std.debug.print("  {s}\n", .{sw_path2});
            }
        }
        if (has_worker_files) {
            var wp_summary: [4096]u8 = undefined;
            var cp_summary: [4096]u8 = undefined;
            const wp_s = std.fmt.bufPrint(&wp_summary, "{s}/{s}-worker.js", .{ output_dir, output_base }) catch "";
            const cp_s = std.fmt.bufPrint(&cp_summary, "{s}/{s}-config.capnp", .{ output_dir, output_base }) catch "";
            std.debug.print("  {s}  (V8 + WASM AOT)\n", .{wp_s});
            std.debug.print("  {s}  (workerd config)\n", .{cp_s});
        }
        std.debug.print("\nTo run:\n", .{});
        if (has_worker_files) {
            var wp_run: [4096]u8 = undefined;
            var cp_run: [4096]u8 = undefined;
            const wp_r = std.fmt.bufPrint(&wp_run, "{s}/{s}-worker.js", .{ output_dir, output_base }) catch "";
            const cp_r = std.fmt.bufPrint(&cp_run, "{s}/{s}-config.capnp", .{ output_dir, output_base }) catch "";
            std.debug.print("  node {s}  # Node.js (V8 JIT + WASM AOT)\n", .{wp_r});
            std.debug.print("  npx workerd serve {s}  # Cloudflare Workers\n", .{cp_r});
        }
        std.debug.print("\n", .{});
        return;
    }

    // Steps 8-10: Strip, wasm-opt, AOT (only for edgebox daemon, skip for worker-only)
    if (!options.binary_only) {
        // Step 8: Strip debug sections (reduces size significantly)
        std.debug.print("[build] Stripping debug sections...\n", .{});
        stripWasmDebug(allocator, wasm_path, stripped_path) catch |err| {
            std.debug.print("[warn] Debug strip failed: {}\n", .{err});
        };
        std.fs.cwd().deleteFile(wasm_path) catch |err| std.debug.print("[build] deleteFile: {}\n", .{err});
        std.fs.cwd().rename(stripped_path, wasm_path) catch |err| std.debug.print("[build] rename: {}\n", .{err});

        // Step 9: Optimize WASM with Binaryen (wasm-opt)
        std.debug.print("[build] Running wasm-opt (Binaryen)...\n", .{});
        optimizeWasm(allocator, wasm_path) catch |err| {
            std.debug.print("[warn] wasm-opt failed: {}\n", .{err});
        };

        // Step 10: AOT compile (SIMD enabled to match WASM build)
        // Note: Wizer is skipped for AOT - native code initializes fast enough
        std.debug.print("[build] AOT compiling with WAMR...\n", .{});
        const aot_compiler = @import("aot_compiler.zig");
        var aot_skipped = false;
        aot_compiler.compileWasmToAot(allocator, wasm_path, aot_path, true) catch |err| {
            if (err == error.WasmTooLarge) {
                aot_skipped = true;
                std.debug.print("[build] AOT skipped (WASM too large)\n", .{});
            } else {
                std.debug.print("[warn] AOT compilation failed: {}\n", .{err});
            }
        };

        if (!aot_skipped) {
            if (std.fs.cwd().statFile(aot_path)) |stat| {
                const size_mb = @as(f64, @floatFromInt(stat.size)) / 1024.0 / 1024.0;
                std.debug.print("[build] AOT: {s} ({d:.1}MB)\n", .{ aot_path, size_mb });
            } else |_| {
                std.debug.print("[warn] AOT file not created\n", .{});
            }
        }

        // Summary for full build (wasm_only already returned above)
        std.debug.print("\n[build] === Static Build Complete ===\n\n", .{});
        std.debug.print("Files created:\n", .{});
        std.debug.print("  {s}     - Native binary (QuickJS + frozen)\n", .{binary_path});
        std.debug.print("  {s}   - WASM with embedded bytecode\n", .{wasm_path});
        std.debug.print("  {s}  - AOT native module\n", .{aot_path});
        if (has_standalone_wasm) {
            const sw_path = std.fmt.bufPrint(&standalone_wasm_path_buf, "{s}/{s}-standalone.wasm", .{ output_dir, output_base }) catch "";
            std.debug.print("  {s}  - Standalone WASM (pure functions, for workerd/V8)\n", .{sw_path});
        }
        std.debug.print("\n", .{});
        std.debug.print("To run:\n", .{});
        std.debug.print("  ./{s}            # Binary (fastest startup)\n", .{binary_path});
        std.debug.print("  edgebox {s}   # WASM (sandboxed)\n", .{wasm_path});
        std.debug.print("  edgebox {s}  # AOT (sandboxed, fast)\n\n", .{aot_path});
    } else {
        // Summary for binary-only build
        std.debug.print("\n[build] === Build Complete ===\n\n", .{});
        std.debug.print("Files created:\n", .{});
        if (has_worker_files) {
            var wp_buf4: [4096]u8 = undefined;
            const wp2 = std.fmt.bufPrint(&wp_buf4, "{s}/{s}-worker.js", .{ output_dir, output_base }) catch "";
            std.debug.print("  {s}  - Worker module (V8 + WASM)\n", .{wp2});
        }
        if (has_standalone_wasm) {
            const sw_path = std.fmt.bufPrint(&standalone_wasm_path_buf, "{s}/{s}-standalone.wasm", .{ output_dir, output_base }) catch "";
            std.debug.print("  {s}  - Standalone WASM (AOT numeric kernels)\n", .{sw_path});
        }
        std.debug.print("  {s}  - Native binary (QuickJS + frozen)\n", .{binary_path});
        std.debug.print("\nTo run:\n", .{});
        if (has_worker_files) {
            var wp_buf5: [4096]u8 = undefined;
            const wp3 = std.fmt.bufPrint(&wp_buf5, "{s}/{s}-worker.js", .{ output_dir, output_base }) catch "";
            std.debug.print("  node {s}  # V8 + WASM (fastest)\n", .{wp3});
        }
        std.debug.print("  ./{s}\n\n", .{binary_path});
    }
}


/// Optimize WASM file using Binaryen (wasm-opt C API)
fn optimizeWasm(allocator: std.mem.Allocator, wasm_path: []const u8) !void {
    // Read WASM file
    const input_file = try std.fs.cwd().openFile(wasm_path, .{});
    defer input_file.close();
    const input = try input_file.readToEndAlloc(allocator, 100 * 1024 * 1024);
    defer allocator.free(input);

    const original_size = input.len;

    // Optimize with Oz (aggressive size optimization)
    const result = try wasm_opt.optimize(allocator, input, .Oz);
    defer allocator.free(result.binary);

    // Write back
    try std.fs.cwd().writeFile(.{ .sub_path = wasm_path, .data = result.binary });

    const saved = if (original_size > result.optimized_size) original_size - result.optimized_size else 0;
    const percent = if (original_size > 0) @as(f64, @floatFromInt(saved)) / @as(f64, @floatFromInt(original_size)) * 100 else 0;
    std.debug.print("[build] wasm-opt: {d}KB -> {d}KB (saved {d:.1}%)\n", .{
        original_size / 1024,
        result.optimized_size / 1024,
        percent,
    });
}

/// Strip debug sections from WASM file (pure Zig, no external tools)
/// Removes all custom sections starting with ".debug" or "name"
fn stripWasmDebug(allocator: std.mem.Allocator, input_path: []const u8, output_path: []const u8) !void {
    const file = try std.fs.cwd().openFile(input_path, .{});
    defer file.close();

    const data = try file.readToEndAlloc(allocator, 100 * 1024 * 1024); // 100MB max
    defer allocator.free(data);

    const original_size = data.len;

    // WASM magic + version = 8 bytes
    if (data.len < 8 or !std.mem.eql(u8, data[0..4], "\x00asm")) {
        return error.InvalidWasm;
    }

    var output = std.ArrayListUnmanaged(u8){};
    defer output.deinit(allocator);

    // Copy header
    try output.appendSlice(allocator, data[0..8]);

    var pos: usize = 8;
    while (pos < data.len) {
        const section_id = data[pos];
        pos += 1;

        // Read section size (LEB128)
        var section_size: u32 = 0;
        var shift: u5 = 0;
        while (true) {
            if (pos >= data.len) break;
            const byte = data[pos];
            pos += 1;
            section_size |= @as(u32, byte & 0x7f) << shift;
            if (byte & 0x80 == 0) break;
            shift +|= 7;
        }

        const section_start = pos;
        // Bounds check to prevent integer overflow and out-of-bounds access
        if (section_size > data.len or pos > data.len - section_size) {
            return error.MalformedWasm;
        }
        const section_end = pos + section_size;

        // Custom section (id 0) - check if debug
        if (section_id == 0 and section_size > 0) {
            // Read name length (LEB128)
            var name_len: u32 = 0;
            var name_shift: u5 = 0;
            var name_pos = section_start;
            while (name_pos < section_end) {
                const byte = data[name_pos];
                name_pos += 1;
                name_len |= @as(u32, byte & 0x7f) << name_shift;
                if (byte & 0x80 == 0) break;
                name_shift +|= 7;
            }

            if (name_pos + name_len <= section_end) {
                const name = data[name_pos .. name_pos + name_len];
                // Skip debug sections
                if (std.mem.startsWith(u8, name, ".debug") or
                    std.mem.eql(u8, name, "name") or
                    std.mem.startsWith(u8, name, "sourceMappingURL"))
                {
                    pos = section_end;
                    continue; // Skip this section
                }
            }
        }

        // Keep this section - write section id
        try output.append(allocator, section_id);

        // Write section size (LEB128)
        var size = section_size;
        while (true) {
            const byte: u8 = @truncate(size & 0x7f);
            size >>= 7;
            if (size == 0) {
                try output.append(allocator, byte);
                break;
            } else {
                try output.append(allocator, byte | 0x80);
            }
        }

        // Write section data
        try output.appendSlice(allocator, data[section_start..section_end]);
        pos = section_end;
    }

    // Write output
    const out_file = try std.fs.cwd().createFile(output_path, .{});
    defer out_file.close();
    try out_file.writeAll(output.items);

    const new_size = output.items.len;
    const saved = original_size - new_size;
    const saved_kb = @as(f64, @floatFromInt(saved)) / 1024.0;
    const new_kb = @as(f64, @floatFromInt(new_size)) / 1024.0;
    std.debug.print("[build] Stripped debug: {d:.1}KB -> {d:.1}KB (saved {d:.1}KB)\n", .{
        @as(f64, @floatFromInt(original_size)) / 1024.0,
        new_kb,
        saved_kb,
    });
}


fn findEntryPoint(app_dir: []const u8, buf: *[4096]u8) ![]const u8 {
    // First, check for .edgebox.json with npm field
    var config_buf: [4096]u8 = undefined;
    const config_path = std.fmt.bufPrint(&config_buf, "{s}/.edgebox.json", .{app_dir}) catch null;
    if (config_path) |cp| {
        if (std.fs.cwd().openFile(cp, .{})) |file| {
            defer file.close();
            var json_buf: [8192]u8 = undefined;
            const json_len = file.readAll(&json_buf) catch 0;
            if (json_len > 0) {
                const json_str = json_buf[0..json_len];
                // Simple JSON parse for "npm" field
                if (std.mem.indexOf(u8, json_str, "\"npm\"")) |npm_idx| {
                    // Find the value after "npm":
                    var i = npm_idx + 5; // skip "npm"
                    while (i < json_len and json_str[i] != '"') : (i += 1) {}
                    if (i < json_len) {
                        i += 1; // skip opening quote
                        const start = i;
                        while (i < json_len and json_str[i] != '"') : (i += 1) {}
                        if (i > start) {
                            const npm_pkg = json_str[start..i];
                            // Resolve npm package entry point
                            if (resolveNpmEntry(app_dir, npm_pkg, buf)) |entry| {
                                return entry;
                            } else |_| {}
                        }
                    }
                }
            }
        } else |_| {}
    }

    // Fall back to standard entry points
    const entries = [_][]const u8{ "index.js", "main.js", "app.js" };
    for (entries) |entry| {
        const path = std.fmt.bufPrint(buf, "{s}/{s}", .{ app_dir, entry }) catch continue;
        if (std.fs.cwd().access(path, .{})) |_| {
            return path;
        } else |_| {}
    }
    return error.NotFound;
}

fn resolveNpmEntry(app_dir: []const u8, npm_pkg: []const u8, buf: *[4096]u8) ![]const u8 {
    // Look for node_modules/{pkg}/package.json
    var pkg_json_buf: [4096]u8 = undefined;
    const pkg_json_path = std.fmt.bufPrint(&pkg_json_buf, "{s}/node_modules/{s}/package.json", .{ app_dir, npm_pkg }) catch return error.NotFound;

    const file = std.fs.cwd().openFile(pkg_json_path, .{}) catch return error.NotFound;
    defer file.close();

    var json_buf: [16384]u8 = undefined;
    const json_len = file.readAll(&json_buf) catch return error.NotFound;
    if (json_len == 0) return error.NotFound;

    const json_str = json_buf[0..json_len];

    // Try to find "bin" field first (for CLI packages)
    if (std.mem.indexOf(u8, json_str, "\"bin\"")) |bin_idx| {
        // Look for the first value in bin object or string
        var i = bin_idx + 5; // skip "bin"
        // Skip whitespace and colon
        while (i < json_len and (json_str[i] == ' ' or json_str[i] == ':' or json_str[i] == '\n' or json_str[i] == '\t')) : (i += 1) {}
        if (i < json_len) {
            if (json_str[i] == '"') {
                // bin is a string directly
                i += 1;
                const start = i;
                while (i < json_len and json_str[i] != '"') : (i += 1) {}
                if (i > start) {
                    const entry_file = json_str[start..i];
                    return std.fmt.bufPrint(buf, "{s}/node_modules/{s}/{s}", .{ app_dir, npm_pkg, entry_file }) catch error.NotFound;
                }
            } else if (json_str[i] == '{') {
                // bin is an object, find first value
                i += 1;
                while (i < json_len and json_str[i] != '"') : (i += 1) {}
                if (i < json_len) {
                    i += 1;
                    // Skip key
                    while (i < json_len and json_str[i] != '"') : (i += 1) {}
                    i += 1; // skip closing quote of key
                    // Skip : and whitespace
                    while (i < json_len and (json_str[i] == ' ' or json_str[i] == ':' or json_str[i] == '\n' or json_str[i] == '\t')) : (i += 1) {}
                    if (i < json_len and json_str[i] == '"') {
                        i += 1;
                        const start = i;
                        while (i < json_len and json_str[i] != '"') : (i += 1) {}
                        if (i > start) {
                            const entry_file = json_str[start..i];
                            return std.fmt.bufPrint(buf, "{s}/node_modules/{s}/{s}", .{ app_dir, npm_pkg, entry_file }) catch error.NotFound;
                        }
                    }
                }
            }
        }
    }

    // Try "main" field
    if (std.mem.indexOf(u8, json_str, "\"main\"")) |main_idx| {
        var i = main_idx + 6; // skip "main"
        while (i < json_len and json_str[i] != '"') : (i += 1) {}
        if (i < json_len) {
            i += 1;
            const start = i;
            while (i < json_len and json_str[i] != '"') : (i += 1) {}
            if (i > start) {
                const entry_file = json_str[start..i];
                return std.fmt.bufPrint(buf, "{s}/node_modules/{s}/{s}", .{ app_dir, npm_pkg, entry_file }) catch error.NotFound;
            }
        }
    }

    // Default to index.js
    const default_path = std.fmt.bufPrint(buf, "{s}/node_modules/{s}/index.js", .{ app_dir, npm_pkg }) catch return error.NotFound;
    if (std.fs.cwd().access(default_path, .{})) |_| {
        return default_path;
    } else |_| {}

    return error.NotFound;
}

fn prependPolyfills(allocator: std.mem.Allocator, polyfills_path: []const u8, bundle_path: []const u8) !void {
    // Read polyfills
    const polyfills = try std.fs.cwd().readFileAlloc(allocator, polyfills_path, 1024 * 1024);
    defer allocator.free(polyfills);

    // Read bundle (50MB max for large npm packages like claude-code)
    const bundle = try std.fs.cwd().readFileAlloc(allocator, bundle_path, 50 * 1024 * 1024);
    defer allocator.free(bundle);

    // Write combined, stripping shebang lines
    const file = try std.fs.cwd().createFile(bundle_path, .{});
    defer file.close();

    try file.writeAll(polyfills);
    try file.writeAll(";\n");

    // Wrap user code in IIFE so functions become closure functions and get frozen.
    // Without this, top-level functions run through the QuickJS interpreter (slow).
    try file.writeAll("(function() {\n");

    // Write bundle, skipping shebang lines (e.g., #!/usr/bin/env node)
    // These cause qjsc to fail with "invalid first character of private name"
    try writeBundleWithoutShebangs(file, bundle);

    // Close the IIFE
    try file.writeAll("\n})();\n");
}

/// Prepend runtime module without IIFE wrapping
/// Used for runtime/ modules that set up globals at top level
fn prependRuntimeModule(allocator: std.mem.Allocator, module_path: []const u8, bundle_path: []const u8) !void {
    // Read module
    const module_content = try std.fs.cwd().readFileAlloc(allocator, module_path, 1024 * 1024);
    defer allocator.free(module_content);

    // Read existing bundle
    const bundle = try std.fs.cwd().readFileAlloc(allocator, bundle_path, 50 * 1024 * 1024);
    defer allocator.free(bundle);

    // Write combined (no IIFE wrapping for runtime modules)
    const file = try std.fs.cwd().createFile(bundle_path, .{});
    defer file.close();

    try file.writeAll(module_content);
    try file.writeAll("\n");
    try file.writeAll(bundle);
}

fn writeBundleWithoutShebangs(file: std.fs.File, content: []const u8) !void {
    var remaining = content;

    // Skip leading shebang if present
    if (remaining.len >= 2 and remaining[0] == '#' and remaining[1] == '!') {
        if (std.mem.indexOf(u8, remaining, "\n")) |newline| {
            remaining = remaining[newline + 1 ..];
        }
    }

    // Write content in chunks, skipping any embedded shebang lines
    while (remaining.len > 0) {
        // Look for embedded shebang (can occur after minification)
        if (std.mem.indexOf(u8, remaining, "\n#!")) |idx| {
            // Write content before the shebang
            try file.writeAll(remaining[0 .. idx + 1]); // Include the newline

            // Skip the shebang line
            const after_shebang = remaining[idx + 1 ..];
            if (std.mem.indexOf(u8, after_shebang, "\n")) |newline| {
                remaining = after_shebang[newline + 1 ..];
            } else {
                // Shebang at end of file, we're done
                break;
            }
        } else {
            // No more shebangs, write rest of content
            try file.writeAll(remaining);
            break;
        }
    }
}

/// Apply trace injection patterns using native sed with a single script file
/// This reduces 25 subprocess calls to just 1, saving 5-10 seconds
fn applyTracePatterns(allocator: std.mem.Allocator, bundle_path: []const u8) !void {
    // Create temp sed script file
    const script_path = "/tmp/edgebox_trace_patterns.sed";
    const script_file = try std.fs.cwd().createFile(script_path, .{});
    defer std.fs.cwd().deleteFile(script_path) catch {};
    // Note: script_file is closed explicitly before running sed

    // Write all sed patterns to script (one per line)
    const patterns =
        \\s/^Dp7();$/if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('before_Dp7'); Dp7().then(function(){ if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('Dp7_resolved'); }).catch(function(e){ print('[ENTRY ERROR] ' + e); });/g
        \\s/^var __create = Object.create;$/if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('user_code_start'); var __create = Object.create;/g
        \\s/^var import_node_module = require("node:module");$/var import_node_module = require("node:module"); if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('after_node_module_require');/g
        \\s/^var import_fs = require("fs");$/var import_fs = require("fs"); if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('after_fs_require');/g
        \\s/^var import_promises2 = require("node:timers\/promises");$/var import_promises2 = require("node:timers\/promises"); if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('after_timers_require');/g
        \\s/^var import_node_http = require("node:http");$/var import_node_http = require("node:http"); if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('after_http_require');/g
        \\s/^var UA = import_node_module.createRequire(\([^)]*\));$/if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('before_createRequire'); var UA = import_node_module.createRequire(\1); if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('after_createRequire');/g
        \\s/^function v9(A) {$/if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('line_4752'); function v9(A) {/g
        \\s/^var TF1 = z((cR0)/if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('line_10k'); var TF1 = z((cR0)/g
        \\s/^var EU = z((dN3/if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('line_50k'); var EU = z((dN3/g
        \\s/^var Dk1 = z((sy3/if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('line_75k'); var Dk1 = z((sy3/g
        \\s/^var rr1 = z((VoB)/if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('line_150k'); var rr1 = z((VoB)/g
        \\s/^var XJ0 = z((tl2)/if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('line_250k'); var XJ0 = z((tl2)/g
        \\s/^var Nz9 = O(() => {$/if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('line_290k'); var Nz9 = O(() => {/g
        \\s/^var rq9 = O(() => {$/if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('line_300k'); var rq9 = O(() => {/g
        \\s/^var BN9 = O(() => {$/if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('line_300k_227'); var BN9 = O(() => {/g
        \\s/^var bN9 = O(() => {$/if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('line_301k'); var bN9 = O(() => {/g
        \\s/^var WL9 = O(() => {$/if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('line_302k'); var WL9 = O(() => {/g
        \\s/^var CL9 = O(() => {$/if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('line_303k'); var CL9 = O(() => {/g
        \\s/^yr();$/if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('after_CL9_before_yr'); yr();/g
        \\s/^dD();$/if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('dD_start'); dD(); if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('dD_done');/g
        \\s/^var kB = O(() => {$/if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('kB_define'); var kB = O(() => { if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('kB_enter');/g
        \\s/^  IQ = qq;$/  if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('kB_before_IQ'); IQ = qq; if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('kB_end');/g
        \\s/^var vs = new YW1()/if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('before_commander'); var vs = new YW1()/g
        \\s/v9("cli_entry")/if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('cli_entry_v9'); v9("cli_entry")/g
        \\
    ;
    try script_file.writeAll(patterns);
    script_file.close();  // Close before running sed so it can read the file

    // Run sed ONCE with the script file
    // Note: macOS sed requires -i '' (empty string), Linux sed requires -i (no argument)
    // Use -i.bak which works on both, then delete the backup
    const result = try runCommand(allocator, &.{
        "sed", "-i.bak", "-f", script_path, bundle_path,
    });
    // Clean up backup file created by sed -i.bak
    const backup_path = try std.fmt.allocPrint(allocator, "{s}.bak", .{bundle_path});
    defer allocator.free(backup_path);
    std.fs.cwd().deleteFile(backup_path) catch {};
    defer {
        if (result.stdout) |s| allocator.free(s);
        if (result.stderr) |s| allocator.free(s);
    }

    if (result.term.Exited != 0) {
        std.debug.print("[error] sed trace injection failed\n", .{});
        if (result.stderr) |s| std.debug.print("{s}\n", .{s});
        return error.SedFailed;
    }
}

const CommandResult = struct {
    term: std.process.Child.Term,
    stdout: ?[]u8,
    stderr: ?[]u8,
};

fn runCommand(allocator: std.mem.Allocator, argv: []const []const u8) !CommandResult {
    var child = std.process.Child.init(argv, allocator);
    child.stdout_behavior = .Pipe;
    child.stderr_behavior = .Pipe;

    try child.spawn();

    // Read stdout and stderr in Zig 0.15 style
    const stdout = if (child.stdout) |f| f.readToEndAlloc(allocator, 10 * 1024 * 1024) catch null else null;
    const stderr = if (child.stderr) |f| f.readToEndAlloc(allocator, 10 * 1024 * 1024) catch null else null;

    const term = try child.wait();

    return .{
        .term = term,
        .stdout = stdout,
        .stderr = stderr,
    };
}

