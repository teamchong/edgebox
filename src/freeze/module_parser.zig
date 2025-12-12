const std = @import("std");
const opcodes = @import("opcodes.zig");
const bytecode_parser = @import("bytecode_parser.zig");

/// BC_VERSION from QuickJS
/// This MUST match the version in vendor/quickjs-ng/quickjs.c
/// If QuickJS updates, check: grep '#define BC_VERSION' vendor/quickjs-ng/quickjs.c
pub const BC_VERSION: u8 = 21;

/// Supported BC_VERSION range (for forward compatibility)
pub const BC_VERSION_MIN: u8 = 21;
pub const BC_VERSION_MAX: u8 = 21;

/// Tags from BCTagEnum
pub const BCTag = enum(u8) {
    invalid = 0,
    null = 1,
    undefined = 2,
    false_ = 3,
    true_ = 4,
    int32 = 5,
    float64 = 6,
    string = 7,
    object = 8,
    array = 9,
    big_int = 10,
    template_object = 11,
    function_bytecode = 12,
    module = 13,
    typed_array = 14,
    array_buffer = 15,
    shared_array_buffer = 16,
    regexp = 17,
    date = 18,
    object_value = 19,
    object_reference = 20,
    map = 21,
    set = 22,
    symbol = 23,
};

/// Parsed function metadata from bytecode
pub const FunctionInfo = struct {
    name_atom: u32,
    arg_count: u32,
    var_count: u32,
    defined_arg_count: u32,
    stack_size: u32,
    closure_var_count: u32,
    cpool_count: u32,
    bytecode_len: u32,
    bytecode_offset: usize, // Offset into the raw data where bytecode starts
    bytecode: []const u8, // Slice of actual bytecode

    // Flags
    has_prototype: bool,
    has_simple_parameter_list: bool,
    is_derived_class_constructor: bool,
    need_home_object: bool,
    func_kind: u2,
    new_target_allowed: bool,
    super_call_allowed: bool,
    super_allowed: bool,
    arguments_allowed: bool,
    backtrace_barrier: bool,
    is_strict_mode: bool,
    has_debug: bool,
};

/// Module parser for QuickJS serialized bytecode
pub const ModuleParser = struct {
    data: []const u8,
    pos: usize,
    atoms: std.ArrayListUnmanaged(u32),
    atom_strings: std.ArrayListUnmanaged([]const u8), // Actual string values
    functions: std.ArrayListUnmanaged(FunctionInfo),
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator, data: []const u8) ModuleParser {
        return .{
            .data = data,
            .pos = 0,
            .atoms = .{},
            .atom_strings = .{},
            .functions = .{},
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *ModuleParser) void {
        self.atoms.deinit(self.allocator);
        for (self.atom_strings.items) |s| {
            self.allocator.free(s);
        }
        self.atom_strings.deinit(self.allocator);
        self.functions.deinit(self.allocator);
    }

    /// Get atom string by index
    pub fn getAtomString(self: *const ModuleParser, atom_idx: u32) ?[]const u8 {
        if (atom_idx < self.atom_strings.items.len) {
            return self.atom_strings.items[atom_idx];
        }
        return null;
    }

    fn readU8(self: *ModuleParser) ?u8 {
        if (self.pos >= self.data.len) return null;
        const v = self.data[self.pos];
        self.pos += 1;
        return v;
    }

    fn readU16(self: *ModuleParser) ?u16 {
        if (self.pos + 2 > self.data.len) return null;
        const v = std.mem.readInt(u16, self.data[self.pos..][0..2], .little);
        self.pos += 2;
        return v;
    }

    fn readU32(self: *ModuleParser) ?u32 {
        if (self.pos + 4 > self.data.len) return null;
        const v = std.mem.readInt(u32, self.data[self.pos..][0..4], .little);
        self.pos += 4;
        return v;
    }

    /// Read LEB128 encoded unsigned integer
    fn readLeb128(self: *ModuleParser) ?u32 {
        var result: u32 = 0;
        var shift: u5 = 0;
        while (true) {
            const byte = self.readU8() orelse return null;
            result |= @as(u32, byte & 0x7F) << shift;
            if ((byte & 0x80) == 0) break;
            shift +%= 7;
            if (shift > 28) return null; // Overflow protection
        }
        return result;
    }

    /// Skip bytes
    fn skip(self: *ModuleParser, n: usize) bool {
        if (self.pos + n > self.data.len) return false;
        self.pos += n;
        return true;
    }

    /// Read atom reference (index into atom table)
    fn readAtom(self: *ModuleParser) ?u32 {
        return self.readLeb128();
    }

    /// Parse the module header and atom table
    pub fn parseHeader(self: *ModuleParser) !void {
        // Check version
        const version = self.readU8() orelse return error.UnexpectedEof;
        if (version < BC_VERSION_MIN or version > BC_VERSION_MAX) {
            std.debug.print(
                \\Error: Unsupported bytecode version {d}
                \\  Supported range: {d}-{d}
                \\  This usually means QuickJS was updated.
                \\  Fix: Update BC_VERSION in src/freeze/module_parser.zig
                \\  Check: grep '#define BC_VERSION' vendor/quickjs-ng/quickjs.c
                \\
            , .{ version, BC_VERSION_MIN, BC_VERSION_MAX });
            return error.InvalidVersion;
        }

        // Read atom count
        const atom_count = self.readLeb128() orelse return error.UnexpectedEof;
        std.debug.print("Atom count: {d}\n", .{atom_count});

        // Read atoms
        var i: u32 = 0;
        while (i < atom_count) : (i += 1) {
            const atom_type = self.readU8() orelse return error.UnexpectedEof;
            if (atom_type == 0) {
                // Const atom - read as u32
                const atom_val = self.readU32() orelse return error.UnexpectedEof;
                try self.atoms.append(self.allocator, atom_val);
                try self.atom_strings.append(self.allocator, ""); // Empty for const atoms
            } else {
                // String atom - atom_type is JS_ATOM_TYPE_* (1 = string, etc.)
                // Read LEB128: bottom bit = is_wide_char, rest = length
                const len_encoded = self.readLeb128() orelse return error.UnexpectedEof;
                const is_wide = (len_encoded & 1) != 0;
                const str_len = len_encoded >> 1;
                const byte_len: usize = if (is_wide) str_len * 2 else str_len;

                // Read the actual string data
                if (self.pos + byte_len > self.data.len) return error.UnexpectedEof;
                const str_data = self.data[self.pos .. self.pos + byte_len];
                self.pos += byte_len;

                // Store the string (only non-wide for now, wide chars are rare)
                if (!is_wide and str_len > 0) {
                    const str_copy = try self.allocator.dupe(u8, str_data);
                    try self.atom_strings.append(self.allocator, str_copy);
                } else {
                    try self.atom_strings.append(self.allocator, ""); // Wide or empty
                }
                try self.atoms.append(self.allocator, i);
            }
        }
    }

    /// Parse a function bytecode tag
    fn parseFunctionBytecode(self: *ModuleParser) !FunctionInfo {
        // Read flags (u16)
        const flags = self.readU16() orelse return error.UnexpectedEof;
        var idx: u4 = 0;

        const has_prototype = ((flags >> idx) & 1) != 0;
        idx += 1;
        const has_simple_parameter_list = ((flags >> idx) & 1) != 0;
        idx += 1;
        const is_derived_class_constructor = ((flags >> idx) & 1) != 0;
        idx += 1;
        const need_home_object = ((flags >> idx) & 1) != 0;
        idx += 1;
        const func_kind: u2 = @truncate((flags >> idx) & 3);
        idx += 2;
        const new_target_allowed = ((flags >> idx) & 1) != 0;
        idx += 1;
        const super_call_allowed = ((flags >> idx) & 1) != 0;
        idx += 1;
        const super_allowed = ((flags >> idx) & 1) != 0;
        idx += 1;
        const arguments_allowed = ((flags >> idx) & 1) != 0;
        idx += 1;
        const backtrace_barrier = ((flags >> idx) & 1) != 0;
        idx += 1;
        const has_debug = ((flags >> idx) & 1) != 0;

        // Read is_strict_mode (u8)
        const is_strict_mode = (self.readU8() orelse return error.UnexpectedEof) != 0;

        // Read func_name atom
        const name_atom = self.readAtom() orelse return error.UnexpectedEof;

        // Read counts (all LEB128)
        const arg_count = self.readLeb128() orelse return error.UnexpectedEof;
        const var_count = self.readLeb128() orelse return error.UnexpectedEof;
        const defined_arg_count = self.readLeb128() orelse return error.UnexpectedEof;
        const stack_size = self.readLeb128() orelse return error.UnexpectedEof;
        const closure_var_count = self.readLeb128() orelse return error.UnexpectedEof;
        const cpool_count = self.readLeb128() orelse return error.UnexpectedEof;
        const bytecode_len = self.readLeb128() orelse return error.UnexpectedEof;

        std.debug.print("Function: name_atom={d} args={d} vars={d} stack={d} closure={d} cpool={d} bc_len={d}\n", .{
            name_atom,
            arg_count,
            var_count,
            stack_size,
            closure_var_count,
            cpool_count,
            bytecode_len,
        });

        // Read vardefs count and skip them
        const vardefs_count = self.readLeb128() orelse return error.UnexpectedEof;
        var v: u32 = 0;
        while (v < vardefs_count) : (v += 1) {
            _ = self.readAtom() orelse return error.UnexpectedEof; // var_name
            _ = self.readLeb128() orelse return error.UnexpectedEof; // scope_level
            _ = self.readLeb128() orelse return error.UnexpectedEof; // scope_next + 1
            _ = self.readU8() orelse return error.UnexpectedEof; // flags
        }

        // Skip closure vars
        var c: u32 = 0;
        while (c < closure_var_count) : (c += 1) {
            _ = self.readAtom() orelse return error.UnexpectedEof; // var_name
            _ = self.readLeb128() orelse return error.UnexpectedEof; // var_idx
            _ = self.readU8() orelse return error.UnexpectedEof; // flags
        }

        // Read constant pool (recursive - may contain nested functions)
        var p: u32 = 0;
        while (p < cpool_count) : (p += 1) {
            try self.parseValue();
        }

        // NOW we're at the actual bytecode!
        const bytecode_offset = self.pos;
        if (self.pos + bytecode_len > self.data.len) return error.UnexpectedEof;
        const bytecode = self.data[self.pos .. self.pos + bytecode_len];
        self.pos += bytecode_len;

        // Skip debug info if present
        if (has_debug) {
            _ = self.readAtom(); // filename
            _ = self.readLeb128(); // line_num
            _ = self.readLeb128(); // col_num
            const pc2line_len = self.readLeb128() orelse return error.UnexpectedEof;
            if (!self.skip(pc2line_len)) return error.UnexpectedEof;
            const source_len = self.readLeb128() orelse return error.UnexpectedEof;
            if (!self.skip(source_len)) return error.UnexpectedEof;
        }

        return FunctionInfo{
            .name_atom = name_atom,
            .arg_count = arg_count,
            .var_count = var_count,
            .defined_arg_count = defined_arg_count,
            .stack_size = stack_size,
            .closure_var_count = closure_var_count,
            .cpool_count = cpool_count,
            .bytecode_len = bytecode_len,
            .bytecode_offset = bytecode_offset,
            .bytecode = bytecode,
            .has_prototype = has_prototype,
            .has_simple_parameter_list = has_simple_parameter_list,
            .is_derived_class_constructor = is_derived_class_constructor,
            .need_home_object = need_home_object,
            .func_kind = func_kind,
            .new_target_allowed = new_target_allowed,
            .super_call_allowed = super_call_allowed,
            .super_allowed = super_allowed,
            .arguments_allowed = arguments_allowed,
            .backtrace_barrier = backtrace_barrier,
            .is_strict_mode = is_strict_mode,
            .has_debug = has_debug,
        };
    }

    /// Parse a value (recursive for nested structures)
    fn parseValue(self: *ModuleParser) error{ UnexpectedEof, OutOfMemory, InvalidFormat }!void {
        const tag_byte = self.readU8() orelse return error.UnexpectedEof;
        const tag: BCTag = @enumFromInt(tag_byte);

        switch (tag) {
            .null, .undefined, .false_, .true_ => {},
            .int32 => {
                _ = self.readLeb128();
            },
            .float64 => {
                if (!self.skip(8)) return error.UnexpectedEof;
            },
            .string => {
                // String: LEB128 length + data + null terminator
                const len = self.readLeb128() orelse return error.UnexpectedEof;
                if (!self.skip(len)) return error.UnexpectedEof;
            },
            .function_bytecode => {
                const func = try self.parseFunctionBytecode();
                try self.functions.append(self.allocator, func);
            },
            .object => {
                // Object: pairs of (atom, value) until null atom
                while (true) {
                    const atom = self.readAtom() orelse return error.UnexpectedEof;
                    if (atom == 0) break; // JS_ATOM_NULL
                    try self.parseValue();
                }
            },
            .array => {
                const len = self.readLeb128() orelse return error.UnexpectedEof;
                var i: u32 = 0;
                while (i < len) : (i += 1) {
                    try self.parseValue();
                }
            },
            .big_int => {
                // BigInt: sign byte + LEB128 length + digits
                _ = self.readU8() orelse return error.UnexpectedEof; // sign
                const len = self.readLeb128() orelse return error.UnexpectedEof;
                if (!self.skip(len * 4)) return error.UnexpectedEof; // limbs are u32
            },
            .regexp => {
                // RegExp: bytecode_len + bytecode + pattern string
                const bc_len = self.readLeb128() orelse return error.UnexpectedEof;
                if (!self.skip(bc_len)) return error.UnexpectedEof;
                const pat_len = self.readLeb128() orelse return error.UnexpectedEof;
                if (!self.skip(pat_len)) return error.UnexpectedEof;
            },
            .date => {
                // Date: float64 timestamp
                if (!self.skip(8)) return error.UnexpectedEof;
            },
            .object_reference => {
                // Object reference: LEB128 index
                _ = self.readLeb128() orelse return error.UnexpectedEof;
            },
            .template_object => {
                // Template object: raw strings array + cooked strings array
                const raw_len = self.readLeb128() orelse return error.UnexpectedEof;
                var i: u32 = 0;
                while (i < raw_len) : (i += 1) {
                    try self.parseValue(); // raw string
                }
                i = 0;
                while (i < raw_len) : (i += 1) {
                    try self.parseValue(); // cooked string
                }
            },
            .typed_array, .array_buffer, .shared_array_buffer => {
                // TypedArray/ArrayBuffer: type tag + length + data
                if (tag == .typed_array) {
                    _ = self.readU8() orelse return error.UnexpectedEof; // array type
                }
                const len = self.readLeb128() orelse return error.UnexpectedEof;
                if (!self.skip(len)) return error.UnexpectedEof;
            },
            .map, .set => {
                // Map/Set: LEB128 count + pairs/values
                const count = self.readLeb128() orelse return error.UnexpectedEof;
                var i: u32 = 0;
                const items_per_entry: u32 = if (tag == .map) 2 else 1;
                while (i < count * items_per_entry) : (i += 1) {
                    try self.parseValue();
                }
            },
            .symbol => {
                // Symbol: atom for description
                _ = self.readAtom() orelse return error.UnexpectedEof;
            },
            .module => {
                // Module structure from quickjs.c JS_ReadModule:
                // - module_name (atom)
                // - req_module_entries_count (LEB128)
                //   - for each: module_name (atom)
                // - export_entries_count (LEB128)
                //   - for each: export_type (u8), var_idx or (req_module_idx + local_name), export_name
                // - star_export_entries_count (LEB128)
                //   - for each: req_module_idx (LEB128)
                // - import_entries_count (LEB128)
                //   - for each: var_idx, import_name, req_module_idx
                // - module_func (function_bytecode)

                _ = self.readAtom() orelse return error.UnexpectedEof; // module_name

                // req_module_entries
                const req_count = self.readLeb128() orelse return error.UnexpectedEof;
                var i: u32 = 0;
                while (i < req_count) : (i += 1) {
                    _ = self.readAtom() orelse return error.UnexpectedEof; // module_name
                }

                // export_entries
                const export_count = self.readLeb128() orelse return error.UnexpectedEof;
                i = 0;
                while (i < export_count) : (i += 1) {
                    const export_type = self.readU8() orelse return error.UnexpectedEof;
                    if (export_type == 0) {
                        // JS_EXPORT_TYPE_LOCAL
                        _ = self.readLeb128() orelse return error.UnexpectedEof; // var_idx
                    } else {
                        // JS_EXPORT_TYPE_INDIRECT
                        _ = self.readLeb128() orelse return error.UnexpectedEof; // req_module_idx
                        _ = self.readAtom() orelse return error.UnexpectedEof; // local_name
                    }
                    _ = self.readAtom() orelse return error.UnexpectedEof; // export_name
                }

                // star_export_entries
                const star_count = self.readLeb128() orelse return error.UnexpectedEof;
                i = 0;
                while (i < star_count) : (i += 1) {
                    _ = self.readLeb128() orelse return error.UnexpectedEof; // req_module_idx
                }

                // import_entries
                const import_count = self.readLeb128() orelse return error.UnexpectedEof;
                i = 0;
                while (i < import_count) : (i += 1) {
                    _ = self.readLeb128() orelse return error.UnexpectedEof; // var_idx
                    _ = self.readAtom() orelse return error.UnexpectedEof; // import_name
                    _ = self.readLeb128() orelse return error.UnexpectedEof; // req_module_idx
                }

                // has_tla (top-level await flag)
                _ = self.readU8() orelse return error.UnexpectedEof;

                // module_func - the main function of the module
                try self.parseValue();
            },
            .object_value => {
                // Object value (for structured clone): class_id + value
                _ = self.readLeb128() orelse return error.UnexpectedEof; // class_id
                try self.parseValue();
            },
            else => {
                // Unknown tag - we can't safely skip it without knowing its size
                std.debug.print("Unknown tag {d} at pos {d}\n", .{ tag_byte, self.pos - 1 });
                // Return a sentinel error so caller can handle gracefully
                return error.InvalidFormat;
            },
        }
    }

    /// Parse the entire module and extract all functions
    /// Stops gracefully when hitting unknown structures (polyfills, regex, etc)
    pub fn parse(self: *ModuleParser) !void {
        try self.parseHeader();

        // Parse values until we hit something we don't understand
        // This is fine - functions are usually serialized early
        while (self.pos < self.data.len) {
            self.parseValue() catch |err| {
                if (err == error.InvalidFormat) {
                    // Hit unknown tag - stop parsing but keep what we found
                    std.debug.print("Stopped parsing at unknown structure (pos={d}/{d})\n", .{ self.pos, self.data.len });
                    break;
                }
                return err;
            };
        }

        std.debug.print("Found {d} functions\n", .{self.functions.items.len});
    }

    /// Get bytecode for a specific function by index
    pub fn getFunctionBytecode(self: *ModuleParser, index: usize) ?[]const u8 {
        if (index >= self.functions.items.len) return null;
        return self.functions.items[index].bytecode;
    }

    /// Get function info by index
    pub fn getFunctionInfo(self: *ModuleParser, index: usize) ?FunctionInfo {
        if (index >= self.functions.items.len) return null;
        return self.functions.items[index];
    }
};

/// Parse a C array format bytecode (from qjsc -e output)
pub fn parseCArrayBytecode(allocator: std.mem.Allocator, source: []const u8) ![]u8 {
    var result = std.ArrayListUnmanaged(u8){};
    errdefer result.deinit(allocator);

    var i: usize = 0;
    while (i < source.len) {
        // Skip whitespace and non-hex characters
        while (i < source.len and (source[i] == ' ' or source[i] == '\n' or source[i] == '\r' or source[i] == '\t' or source[i] == ',')) {
            i += 1;
        }

        // Look for 0x prefix
        if (i + 3 < source.len and source[i] == '0' and source[i + 1] == 'x') {
            i += 2;
            // Read hex digits
            var val: u8 = 0;
            var digits: usize = 0;
            while (i < source.len and digits < 2) {
                const c = source[i];
                if (c >= '0' and c <= '9') {
                    val = val * 16 + (c - '0');
                    digits += 1;
                    i += 1;
                } else if (c >= 'a' and c <= 'f') {
                    val = val * 16 + (c - 'a' + 10);
                    digits += 1;
                    i += 1;
                } else if (c >= 'A' and c <= 'F') {
                    val = val * 16 + (c - 'A' + 10);
                    digits += 1;
                    i += 1;
                } else {
                    break;
                }
            }
            if (digits > 0) {
                try result.append(allocator, val);
            }
        } else {
            i += 1;
        }
    }

    return result.toOwnedSlice(allocator);
}

test "parse C array bytecode" {
    const allocator = std.testing.allocator;
    const source = "0x01, 0x02, 0xff, 0x00";
    const result = try parseCArrayBytecode(allocator, source);
    defer allocator.free(result);

    try std.testing.expectEqual(@as(usize, 4), result.len);
    try std.testing.expectEqual(@as(u8, 0x01), result[0]);
    try std.testing.expectEqual(@as(u8, 0x02), result[1]);
    try std.testing.expectEqual(@as(u8, 0xff), result[2]);
    try std.testing.expectEqual(@as(u8, 0x00), result[3]);
}
