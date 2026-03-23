// WasmGC Binary Builder
// Generates a .wasm module with GC types (arrays, structs) that V8 TurboFan
// can inline into JS at the callsite. No WASM call overhead.
//
// V8 source study: WasmIntoJsInliner::TryInlining only supports:
//   struct.get/set, array.get/set/len, ref.cast, local.get, drop, end
//
// This builder emits modules using ONLY those instructions.
// The resulting functions are fully inlinable by TurboFan.

const std = @import("std");

// WasmGC opcodes (0xfb prefix)
const GC_PREFIX: u8 = 0xfb;
const STRUCT_NEW_DEFAULT: u8 = 0x02;
const STRUCT_GET: u8 = 0x03;
const STRUCT_SET: u8 = 0x06;
const ARRAY_NEW_DEFAULT: u8 = 0x1b;
const ARRAY_GET: u8 = 0x13;
const ARRAY_SET: u8 = 0x16;
const ARRAY_LEN: u8 = 0x17;

// Standard WASM opcodes
const OP_END: u8 = 0x0b;
const OP_LOCAL_GET: u8 = 0x20;
const OP_DROP: u8 = 0x1a;

// Value types
const I32: u8 = 0x7f;
const REF: u8 = 0x63; // ref (non-nullable)
const REFNULL: u8 = 0x64; // ref null

// Type constructors
const ARRAY_TYPE: u8 = 0x5e;
const STRUCT_TYPE: u8 = 0x5f;
const SUB_FINAL: u8 = 0x4f; // sub final (no subtypes)
const FUNC_TYPE: u8 = 0x60;

// Section IDs
const SEC_TYPE: u8 = 1;
const SEC_FUNCTION: u8 = 3;
const SEC_EXPORT: u8 = 7;
const SEC_CODE: u8 = 10;

// Mutability
const MUT: u8 = 0x01;
const IMMUT: u8 = 0x00;

const Emitter = struct {
    buf: std.ArrayListUnmanaged(u8),
    alloc: std.mem.Allocator,

    fn init(allocator: std.mem.Allocator) Emitter {
        return .{ .buf = .{}, .alloc = allocator };
    }

    fn deinit(self: *Emitter) void {
        self.buf.deinit(self.alloc);
    }

    fn emit(self: *Emitter, byte: u8) !void {
        try self.buf.append(self.alloc, byte);
    }

    fn emitSlice(self: *Emitter, bytes: []const u8) !void {
        try self.buf.appendSlice(self.alloc, bytes);
    }

    // LEB128 unsigned encoding
    fn emitU32(self: *Emitter, value: u32) !void {
        var v = value;
        while (true) {
            const byte: u8 = @intCast(v & 0x7f);
            v >>= 7;
            if (v == 0) {
                try self.emit(byte);
                break;
            }
            try self.emit(byte | 0x80);
        }
    }

    // LEB128 signed encoding
    fn emitI32(self: *Emitter, value: i32) !void {
        var v = value;
        while (true) {
            const byte: u8 = @intCast(@as(u32, @bitCast(v)) & 0x7f);
            v >>= 7;
            if ((v == 0 and byte & 0x40 == 0) or (v == -1 and byte & 0x40 != 0)) {
                try self.emit(byte);
                break;
            }
            try self.emit(byte | 0x80);
        }
    }

    fn emitString(self: *Emitter, s: []const u8) !void {
        try self.emitU32(@intCast(s.len));
        try self.emitSlice(s);
    }

    fn result(self: *Emitter) []const u8 {
        return self.buf.items;
    }
};

/// Build a WasmGC module for type flag storage.
///
/// Module exports:
///   newFlags(size: i32) -> ref $flags_array
///   getFlag(arr: ref $flags_array, idx: i32) -> i32
///   setFlag(arr: ref $flags_array, idx: i32, val: i32)
///   arrayLen(arr: ref $flags_array) -> i32
///
/// V8 TurboFan inlines getFlag/setFlag/arrayLen at JS callsite.
pub fn buildTypeFlagsModule(allocator: std.mem.Allocator) ![]const u8 {
    var e = Emitter.init(allocator);
    defer e.deinit();

    // WASM magic + version
    try e.emitSlice(&[_]u8{ 0x00, 0x61, 0x73, 0x6d }); // \0asm
    try e.emitSlice(&[_]u8{ 0x01, 0x00, 0x00, 0x00 }); // version 1

    // === Type Section ===
    // Type 0: (array (mut i32)) — the flags array
    // Type 1: func (i32) -> (ref 0) — newFlags
    // Type 2: func (ref 0, i32) -> (i32) — getFlag
    // Type 3: func (ref 0, i32, i32) -> () — setFlag
    // Type 4: func (ref 0) -> (i32) — arrayLen
    {
        var sec = Emitter.init(allocator);
        defer sec.deinit();

        try sec.emitU32(5); // 5 types

        // Type 0: (sub final [] (array (mut i32)))
        try sec.emit(SUB_FINAL);
        try sec.emitU32(0); // 0 supertypes
        try sec.emit(ARRAY_TYPE);
        try sec.emit(MUT); // mutable
        try sec.emit(I32); // element type: i32

        // Type 1: func (i32) -> (ref 0)
        try sec.emit(FUNC_TYPE);
        try sec.emitU32(1); // 1 param
        try sec.emit(I32);
        try sec.emitU32(1); // 1 result
        try sec.emit(REF);
        try sec.emitU32(0); // ref type index 0

        // Type 2: func (ref 0, i32) -> (i32)
        try sec.emit(FUNC_TYPE);
        try sec.emitU32(2); // 2 params
        try sec.emit(REF);
        try sec.emitU32(0);
        try sec.emit(I32);
        try sec.emitU32(1); // 1 result
        try sec.emit(I32);

        // Type 3: func (ref 0, i32, i32) -> ()
        try sec.emit(FUNC_TYPE);
        try sec.emitU32(3); // 3 params
        try sec.emit(REF);
        try sec.emitU32(0);
        try sec.emit(I32);
        try sec.emit(I32);
        try sec.emitU32(0); // 0 results

        // Type 4: func (ref 0) -> (i32)
        try sec.emit(FUNC_TYPE);
        try sec.emitU32(1); // 1 param
        try sec.emit(REF);
        try sec.emitU32(0);
        try sec.emitU32(1); // 1 result
        try sec.emit(I32);

        try e.emit(SEC_TYPE);
        try e.emitU32(@intCast(sec.result().len));
        try e.emitSlice(sec.result());
    }

    // === Function Section ===
    {
        var sec = Emitter.init(allocator);
        defer sec.deinit();
        try sec.emitU32(4); // 4 functions
        try sec.emitU32(1); // func 0: type 1 (newFlags)
        try sec.emitU32(2); // func 1: type 2 (getFlag)
        try sec.emitU32(3); // func 2: type 3 (setFlag)
        try sec.emitU32(4); // func 3: type 4 (arrayLen)

        try e.emit(SEC_FUNCTION);
        try e.emitU32(@intCast(sec.result().len));
        try e.emitSlice(sec.result());
    }

    // === Export Section ===
    {
        var sec = Emitter.init(allocator);
        defer sec.deinit();
        try sec.emitU32(4); // 4 exports

        try sec.emitString("newFlags");
        try sec.emit(0x00); // func export
        try sec.emitU32(0); // func index 0

        try sec.emitString("getFlag");
        try sec.emit(0x00);
        try sec.emitU32(1);

        try sec.emitString("setFlag");
        try sec.emit(0x00);
        try sec.emitU32(2);

        try sec.emitString("arrayLen");
        try sec.emit(0x00);
        try sec.emitU32(3);

        try e.emit(SEC_EXPORT);
        try e.emitU32(@intCast(sec.result().len));
        try e.emitSlice(sec.result());
    }

    // === Code Section ===
    {
        var sec = Emitter.init(allocator);
        defer sec.deinit();
        try sec.emitU32(4); // 4 function bodies

        // func 0: newFlags(size: i32) -> ref $flags_array
        // Body: (array.new_default $flags_array (local.get 0))
        {
            var body = Emitter.init(allocator);
            defer body.deinit();
            try body.emitU32(0); // 0 locals
            try body.emit(OP_LOCAL_GET);
            try body.emitU32(0); // param 0 (size)
            try body.emit(GC_PREFIX);
            try body.emit(ARRAY_NEW_DEFAULT);
            try body.emitU32(0); // type index 0
            try body.emit(OP_END);

            try sec.emitU32(@intCast(body.result().len));
            try sec.emitSlice(body.result());
        }

        // func 1: getFlag(arr: ref, idx: i32) -> i32
        // Body: (array.get $flags_array (local.get 0) (local.get 1))
        // THIS IS WHAT V8 TURBOFAN INLINES
        {
            var body = Emitter.init(allocator);
            defer body.deinit();
            try body.emitU32(0); // 0 locals
            try body.emit(OP_LOCAL_GET);
            try body.emitU32(0); // param 0 (arr)
            try body.emit(OP_LOCAL_GET);
            try body.emitU32(1); // param 1 (idx)
            try body.emit(GC_PREFIX);
            try body.emit(ARRAY_GET);
            try body.emitU32(0); // type index 0
            try body.emit(OP_END);

            try sec.emitU32(@intCast(body.result().len));
            try sec.emitSlice(body.result());
        }

        // func 2: setFlag(arr: ref, idx: i32, val: i32)
        // Body: (array.set $flags_array (local.get 0) (local.get 1) (local.get 2))
        {
            var body = Emitter.init(allocator);
            defer body.deinit();
            try body.emitU32(0); // 0 locals
            try body.emit(OP_LOCAL_GET);
            try body.emitU32(0);
            try body.emit(OP_LOCAL_GET);
            try body.emitU32(1);
            try body.emit(OP_LOCAL_GET);
            try body.emitU32(2);
            try body.emit(GC_PREFIX);
            try body.emit(ARRAY_SET);
            try body.emitU32(0);
            try body.emit(OP_END);

            try sec.emitU32(@intCast(body.result().len));
            try sec.emitSlice(body.result());
        }

        // func 3: arrayLen(arr: ref) -> i32
        // Body: (array.len (local.get 0))
        {
            var body = Emitter.init(allocator);
            defer body.deinit();
            try body.emitU32(0); // 0 locals
            try body.emit(OP_LOCAL_GET);
            try body.emitU32(0);
            try body.emit(GC_PREFIX);
            try body.emit(ARRAY_LEN);
            try body.emit(OP_END);

            try sec.emitU32(@intCast(body.result().len));
            try sec.emitSlice(body.result());
        }

        try e.emit(SEC_CODE);
        try e.emitU32(@intCast(sec.result().len));
        try e.emitSlice(sec.result());
    }

    return try allocator.dupe(u8, e.result());
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const wasm = try buildTypeFlagsModule(allocator);
    defer allocator.free(wasm);

    // Write to file
    const file = try std.fs.cwd().createFile("src/tsc-recipe/type_flags_gc.wasm", .{});
    defer file.close();
    try file.writeAll(wasm);

    _ = std.posix.write(2, "WasmGC module built\n") catch {};
    var size_buf: [64]u8 = undefined;
    const msg = std.fmt.bufPrint(&size_buf, "Size: {d} bytes\n", .{wasm.len}) catch "?\n";
    _ = std.posix.write(2, msg) catch {};
}
