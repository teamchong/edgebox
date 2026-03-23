// WasmGC Binary Builder
//
// Generates .wasm modules with GC types that V8 TurboFan inlines into JS.
// V8 source: WasmIntoJsInliner supports ONLY:
//   struct.get/set, array.get/set/len, ref.cast, local.get, drop, end
//
// This builder is the foundation for:
//   1. Type flag arrays (TSC recipe — type checking)
//   2. Auto SOA structs (frozen object layouts — struct operations)
//   3. Any future WASM kernel that needs TurboFan inlining
//
// Usage:
//   zig run wasmgc_builder.zig -- type_flags    → type_flags_gc.wasm
//   zig run wasmgc_builder.zig -- soa           → soa_gc.wasm
//   zig run wasmgc_builder.zig -- all           → both

const std = @import("std");

// ── WASM Binary Encoding ──

const WasmWriter = struct {
    data: std.ArrayListUnmanaged(u8) = .{},
    alloc: std.mem.Allocator,

    fn init(a: std.mem.Allocator) WasmWriter {
        return .{ .alloc = a };
    }
    fn deinit(self: *WasmWriter) void {
        self.data.deinit(self.alloc);
    }

    fn byte(self: *WasmWriter, b: u8) !void {
        try self.data.append(self.alloc, b);
    }
    fn bytes(self: *WasmWriter, bs: []const u8) !void {
        try self.data.appendSlice(self.alloc, bs);
    }
    fn u32leb(self: *WasmWriter, value: u32) !void {
        var v = value;
        while (true) {
            const b: u8 = @intCast(v & 0x7f);
            v >>= 7;
            if (v == 0) { try self.byte(b); break; }
            try self.byte(b | 0x80);
        }
    }
    fn i32leb(self: *WasmWriter, value: i32) !void {
        var v = value;
        while (true) {
            const b: u8 = @intCast(@as(u32, @bitCast(v)) & 0x7f);
            v >>= 7;
            if ((v == 0 and b & 0x40 == 0) or (v == -1 and b & 0x40 != 0)) {
                try self.byte(b); break;
            }
            try self.byte(b | 0x80);
        }
    }
    fn str(self: *WasmWriter, s: []const u8) !void {
        try self.u32leb(@intCast(s.len));
        try self.bytes(s);
    }
    fn result(self: *WasmWriter) []const u8 {
        return self.data.items;
    }

    // Write a section: id + length-prefixed content
    fn section(self: *WasmWriter, id: u8, content: []const u8) !void {
        try self.byte(id);
        try self.u32leb(@intCast(content.len));
        try self.bytes(content);
    }
};

// ── WASM Constants ──
const MAGIC = [_]u8{ 0x00, 0x61, 0x73, 0x6d };
const VERSION = [_]u8{ 0x01, 0x00, 0x00, 0x00 };

// Section IDs
const SEC_TYPE: u8 = 1;
const SEC_FUNC: u8 = 3;
const SEC_EXPORT: u8 = 7;
const SEC_CODE: u8 = 10;

// Type constructors (WasmGC)
const ARRAY: u8 = 0x5e;
const STRUCT: u8 = 0x5f;
const FUNC: u8 = 0x60;
const SUB_FINAL: u8 = 0x4f;

// Value types
const I32: u8 = 0x7f;
const I64: u8 = 0x7e;
const F64: u8 = 0x7c;
const REF: u8 = 0x63;
const REFNULL: u8 = 0x64;

// Mutability
const MUT: u8 = 0x01;

// Opcodes
const LOCAL_GET: u8 = 0x20;
const END: u8 = 0x0b;
const GC: u8 = 0xfb;
const GC_STRUCT_NEW_DEFAULT: u8 = 0x02;
const GC_STRUCT_GET: u8 = 0x03;
const GC_STRUCT_SET: u8 = 0x06;
const GC_ARRAY_NEW_DEFAULT: u8 = 0x1b;
const GC_ARRAY_GET: u8 = 0x13;
const GC_ARRAY_SET: u8 = 0x16;
const GC_ARRAY_LEN: u8 = 0x17;

// ── Module Builders ──

/// Build type_flags_gc.wasm — GC array for type flag storage.
/// Exports: newFlags, getFlag, setFlag, arrayLen
/// All function bodies use ONLY V8-inlinable instructions.
pub fn buildTypeFlagsModule(alloc: std.mem.Allocator) ![]const u8 {
    // Use wasm-tools to parse WAT (most reliable for WasmGC encoding)
    // The WAT is small enough to embed directly
    const wat =
        \\(module
        \\  (type $flags (array (mut i32)))
        \\  (func (export "newFlags") (param $size i32) (result (ref $flags))
        \\    (array.new_default $flags (local.get $size)))
        \\  (func (export "getFlag") (param $arr (ref null $flags)) (param $idx i32) (result i32)
        \\    (array.get $flags (local.get $arr) (local.get $idx)))
        \\  (func (export "setFlag") (param $arr (ref null $flags)) (param $idx i32) (param $val i32)
        \\    (array.set $flags (local.get $arr) (local.get $idx) (local.get $val)))
        \\  (func (export "arrayLen") (param $arr (ref null $flags)) (result i32)
        \\    (array.len (local.get $arr))))
    ;
    return try compileWat(alloc, wat, "type_flags_gc.wasm");
}

/// Build soa_gc.wasm — GC structs for auto SOA object layouts.
/// Each "shape" is a struct type with typed fields.
/// V8 TurboFan inlines struct.get/set at JS callsite.
///
/// For now: a generic 8-field struct (covers most JS object shapes).
/// Future: recipe generates per-shape struct types dynamically.
pub fn buildSoaModule(alloc: std.mem.Allocator) ![]const u8 {
    // Generic SOA struct: 8 fields (f64 for JS number compatibility)
    // Plus an i32 array for variable-length data
    const wat =
        \\(module
        \\  ;; Fixed-layout struct for common JS objects (up to 8 numeric fields)
        \\  (type $obj8 (struct
        \\    (field $f0 (mut f64))
        \\    (field $f1 (mut f64))
        \\    (field $f2 (mut f64))
        \\    (field $f3 (mut f64))
        \\    (field $f4 (mut f64))
        \\    (field $f5 (mut f64))
        \\    (field $f6 (mut f64))
        \\    (field $f7 (mut f64))))
        \\
        \\  ;; Variable-length i32 array (for flags, indices, etc.)
        \\  (type $i32arr (array (mut i32)))
        \\
        \\  ;; SOA column: array of f64 (one field across all objects)
        \\  (type $f64col (array (mut f64)))
        \\
        \\  ;; Create struct — ALL fields inlinable by TurboFan
        \\  (func (export "newObj8") (result (ref $obj8))
        \\    (struct.new_default $obj8))
        \\
        \\  ;; Field accessors — struct.get/set are V8-inlinable
        \\  (func (export "getF0") (param (ref null $obj8)) (result f64)
        \\    (struct.get $obj8 $f0 (local.get 0)))
        \\  (func (export "setF0") (param (ref null $obj8)) (param f64)
        \\    (struct.set $obj8 $f0 (local.get 0) (local.get 1)))
        \\  (func (export "getF1") (param (ref null $obj8)) (result f64)
        \\    (struct.get $obj8 $f1 (local.get 0)))
        \\  (func (export "setF1") (param (ref null $obj8)) (param f64)
        \\    (struct.set $obj8 $f1 (local.get 0) (local.get 1)))
        \\  (func (export "getF2") (param (ref null $obj8)) (result f64)
        \\    (struct.get $obj8 $f2 (local.get 0)))
        \\  (func (export "setF2") (param (ref null $obj8)) (param f64)
        \\    (struct.set $obj8 $f2 (local.get 0) (local.get 1)))
        \\  (func (export "getF3") (param (ref null $obj8)) (result f64)
        \\    (struct.get $obj8 $f3 (local.get 0)))
        \\  (func (export "setF3") (param (ref null $obj8)) (param f64)
        \\    (struct.set $obj8 $f3 (local.get 0) (local.get 1)))
        \\
        \\  ;; SOA column operations
        \\  (func (export "newCol") (param i32) (result (ref $f64col))
        \\    (array.new_default $f64col (local.get 0)))
        \\  (func (export "getCol") (param (ref null $f64col)) (param i32) (result f64)
        \\    (array.get $f64col (local.get 0) (local.get 1)))
        \\  (func (export "setCol") (param (ref null $f64col)) (param i32) (param f64)
        \\    (array.set $f64col (local.get 0) (local.get 1) (local.get 2)))
        \\  (func (export "colLen") (param (ref null $f64col)) (result i32)
        \\    (array.len (local.get 0)))
        \\
        \\  ;; i32 array (for type flags, indices)
        \\  (func (export "newI32") (param i32) (result (ref $i32arr))
        \\    (array.new_default $i32arr (local.get 0)))
        \\  (func (export "getI32") (param (ref null $i32arr)) (param i32) (result i32)
        \\    (array.get $i32arr (local.get 0) (local.get 1)))
        \\  (func (export "setI32") (param (ref null $i32arr)) (param i32) (param i32)
        \\    (array.set $i32arr (local.get 0) (local.get 1) (local.get 2)))
        \\  (func (export "i32Len") (param (ref null $i32arr)) (result i32)
        \\    (array.len (local.get 0))))
    ;
    return try compileWat(alloc, wat, "soa_gc.wasm");
}

/// Compile WAT text to WASM binary using wasm-tools
fn compileWat(alloc: std.mem.Allocator, wat: []const u8, output_name: []const u8) ![]const u8 {
    // Write WAT to temp file
    const tmp_wat = "/tmp/_edgebox_gc.wat";
    const tmp_wasm = "/tmp/_edgebox_gc.wasm";
    {
        const f = try std.fs.cwd().createFile(tmp_wat, .{});
        defer f.close();
        try f.writeAll(wat);
    }

    // Run wasm-tools parse
    var child = std.process.Child.init(
        &.{ "wasm-tools", "parse", tmp_wat, "-o", tmp_wasm },
        alloc,
    );
    child.stderr_behavior = .Inherit;
    const term = try child.spawnAndWait();
    if (term.Exited != 0) {
        _ = std.posix.write(2, "FATAL: wasm-tools parse failed\n") catch {};
        return error.WasmToolsFailed;
    }

    // Read the compiled WASM
    const wasm = try std.fs.cwd().readFileAlloc(alloc, tmp_wasm, 1024 * 1024);

    // Copy to output
    const output_path = try std.fmt.allocPrint(alloc, "src/tsc-recipe/{s}", .{output_name});
    defer alloc.free(output_path);
    {
        const f = try std.fs.cwd().createFile(output_path, .{});
        defer f.close();
        try f.writeAll(wasm);
    }

    var msg_buf: [128]u8 = undefined;
    const msg = std.fmt.bufPrint(&msg_buf, "Built {s}: {d} bytes\n", .{ output_name, wasm.len }) catch "built\n";
    _ = std.posix.write(2, msg) catch {};

    return wasm;
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const alloc = gpa.allocator();

    const args = try std.process.argsAlloc(alloc);
    defer std.process.argsFree(alloc, args);

    const target = if (args.len > 1) args[1] else "all";

    if (std.mem.eql(u8, target, "type_flags") or std.mem.eql(u8, target, "all")) {
        const wasm = try buildTypeFlagsModule(alloc);
        defer alloc.free(wasm);
    }

    if (std.mem.eql(u8, target, "soa") or std.mem.eql(u8, target, "all")) {
        const wasm = try buildSoaModule(alloc);
        defer alloc.free(wasm);
    }
}
