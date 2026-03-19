// v8.zig — Zig wrapper for V8 embedding via librusty_v8.a
//
// Provides a safe Zig API over V8's extern "C" binding functions.
// Most functions call directly into librusty_v8.a. A small C++ bridge
// (v8_bridge.cpp) handles operations requiring C++ struct layouts.
//
// Usage:
//   const v8 = @import("v8.zig");
//   v8.initPlatform();
//   const isolate = v8.Isolate.create();
//   defer isolate.dispose();
//   // ... eval JS code ...

const std = @import("std");

// ============================================================
// Opaque V8 types — pointers from Zig's perspective
// ============================================================
const Platform = opaque {};
const Isolate = opaque {};
const Context = opaque {};
const Value = opaque {};
const Script = opaque {};
const String = opaque {};
const Object = opaque {};
const ObjectTemplate = opaque {};
const FunctionTemplate = opaque {};
const MicrotaskQueue = opaque {};
const Data = opaque {};
const Message = opaque {};
const FunctionCallbackInfo = opaque {};
const Name = opaque {};
const Module = opaque {};

// ============================================================
// In-place constructed types (stack-allocated, known sizes)
// ============================================================

/// HandleScope — RAII scope for V8 handles.
/// Size: sizeof(size_t) * 3 = 24 bytes on 64-bit.
pub const HandleScope = struct {
    buf: [3 * @sizeOf(usize)]u8 align(@alignOf(usize)) = undefined,

    pub fn init(isolate: *Isolate) HandleScope {
        var hs = HandleScope{};
        c.v8__HandleScope__CONSTRUCT(&hs.buf, isolate);
        return hs;
    }

    pub fn deinit(self: *HandleScope) void {
        c.v8__HandleScope__DESTRUCT(&self.buf);
    }
};

/// TryCatch — exception catcher for V8 operations.
/// Size: sizeof(size_t) * 6 = 48 bytes on 64-bit.
pub const TryCatch = struct {
    buf: [6 * @sizeOf(usize)]u8 align(@alignOf(usize)) = undefined,

    pub fn init(isolate: *Isolate) TryCatch {
        var tc = TryCatch{};
        c.v8__TryCatch__CONSTRUCT(&tc.buf, isolate);
        return tc;
    }

    pub fn deinit(self: *TryCatch) void {
        c.v8__TryCatch__DESTRUCT(&self.buf);
    }

    pub fn hasCaught(self: *const TryCatch) bool {
        return c.v8__TryCatch__HasCaught(&self.buf);
    }

    pub fn exception(self: *const TryCatch) ?*const Value {
        return c.v8__TryCatch__Exception(&self.buf);
    }

    pub fn stackTrace(self: *const TryCatch, context: *const Context) ?*const Value {
        return c.v8__TryCatch__StackTrace(&self.buf, context);
    }

    pub fn message(self: *const TryCatch) ?*const Message {
        return c.v8__TryCatch__Message(&self.buf);
    }
};

/// ScriptOrigin — metadata about a script's source location.
/// Size varies by platform, queried from C++ bridge at comptime.
pub const ScriptOrigin = struct {
    // ScriptOrigin is ~40-56 bytes depending on platform.
    // We use the maximum possible size (64 bytes) for safety.
    // The actual size is checked at runtime in init().
    buf: [max_size]u8 align(@alignOf(usize)) = undefined,

    const max_size = 64;

    pub fn init(resource_name: *const Value) ScriptOrigin {
        var so = ScriptOrigin{};
        const actual_size = bridge.edgebox_v8_script_origin_sizeof();
        std.debug.assert(actual_size <= max_size);
        c.v8__ScriptOrigin__CONSTRUCT(
            &so.buf,
            resource_name,
            0, // line offset
            0, // column offset
            false, // shared cross origin
            -1, // script id
            null, // source map url
            false, // opaque
            false, // wasm
            false, // module
            null, // host defined options
        );
        return so;
    }
};

// ============================================================
// V8 Platform + Engine Lifecycle
// ============================================================

/// Initialize V8 platform and engine. Call once at startup.
pub fn initPlatform() !*Platform {
    const platform = c.v8__Platform__NewDefaultPlatform(0, false) orelse
        return error.V8PlatformFailed;
    c.v8__V8__InitializePlatform(platform);
    c.v8__V8__Initialize();
    return platform;
}

/// Shut down V8 engine and platform.
pub fn disposePlatform() void {
    _ = c.v8__V8__Dispose();
    c.v8__V8__DisposePlatform();
}

/// Get V8 version string.
pub fn getVersion() []const u8 {
    return std.mem.span(c.v8__V8__GetVersion());
}

// ============================================================
// Isolate API
// ============================================================

pub const IsolateApi = struct {
    /// Create a new Isolate with default ArrayBuffer allocator.
    /// Uses C++ bridge to handle CreateParams struct layout.
    pub fn create() *Isolate {
        return bridge.edgebox_v8_create_isolate();
    }

    pub fn enter(isolate: *Isolate) void {
        c.v8__Isolate__Enter(isolate);
    }

    pub fn exit(isolate: *Isolate) void {
        c.v8__Isolate__Exit(isolate);
    }

    pub fn dispose(isolate: *Isolate) void {
        c.v8__Isolate__Dispose(isolate);
    }

    pub fn getCurrent() ?*Isolate {
        return c.v8__Isolate__GetCurrent();
    }

    pub fn throwException(isolate: *Isolate, exception_val: *const Value) *const Value {
        return c.v8__Isolate__ThrowException(isolate, exception_val);
    }
};

// ============================================================
// Context API
// ============================================================

pub const ContextApi = struct {
    /// Create a new Context.
    pub fn create(isolate: *Isolate) *const Context {
        return c.v8__Context__New(isolate, null, null, null);
    }

    pub fn enter(ctx: *const Context) void {
        c.v8__Context__Enter(ctx);
    }

    pub fn exit(ctx: *const Context) void {
        c.v8__Context__Exit(ctx);
    }

    pub fn global(ctx: *const Context) *const Object {
        return c.v8__Context__Global(ctx);
    }
};

// ============================================================
// String API
// ============================================================

pub const StringApi = struct {
    /// Create a V8 string from a Zig slice.
    pub fn fromUtf8(isolate: *Isolate, data: []const u8) ?*const String {
        return c.v8__String__NewFromUtf8(
            isolate,
            data.ptr,
            0, // kNormal
            @intCast(data.len),
        );
    }

    /// Get UTF-8 length of a V8 string.
    pub fn utf8Length(str: *const String, isolate: *Isolate) i32 {
        return c.v8__String__Utf8Length(str, isolate);
    }

    /// Write V8 string to a Zig buffer. Returns bytes written.
    pub fn writeUtf8(str: *const String, isolate: *Isolate, buf: []u8) usize {
        return c.v8__String__WriteUtf8_v2(
            str,
            isolate,
            buf.ptr,
            buf.len,
            0, // flags
            null, // processed_characters
        );
    }

    /// Read a V8 string into a Zig-allocated buffer.
    pub fn toSlice(str: *const String, isolate: *Isolate, allocator: std.mem.Allocator) ![]u8 {
        const len: usize = @intCast(utf8Length(str, isolate));
        const buf = try allocator.alloc(u8, len + 1); // +1 for potential null
        const written = writeUtf8(str, isolate, buf);
        return buf[0..written];
    }
};

// ============================================================
// Script API
// ============================================================

pub const ScriptApi = struct {
    /// Compile a script from source string + origin.
    pub fn compile(context: *const Context, source: *const String, origin: *const ScriptOrigin) ?*const Script {
        return c.v8__Script__Compile(context, source, &origin.buf);
    }

    /// Run a compiled script. Returns null on exception.
    pub fn run(script: *const Script, context: *const Context) ?*const Value {
        return c.v8__Script__Run(script, context);
    }
};

// ============================================================
// Value API
// ============================================================

pub const ValueApi = struct {
    pub fn isString(val: *const Value) bool {
        return c.v8__Value__IsString(val);
    }

    pub fn isInt32(val: *const Value) bool {
        return c.v8__Value__IsInt32(val);
    }

    pub fn isNumber(val: *const Value) bool {
        return c.v8__Value__IsNumber(val);
    }

    pub fn isUndefined(val: *const Value) bool {
        return c.v8__Value__IsUndefined(val);
    }

    pub fn isNull(val: *const Value) bool {
        return c.v8__Value__IsNull(val);
    }

    pub fn isBoolean(val: *const Value) bool {
        return c.v8__Value__IsBoolean(val);
    }

    pub fn toString(val: *const Value, context: *const Context) ?*const String {
        return c.v8__Value__ToString(val, context);
    }

    /// Get Int32 value. Returns null if conversion fails.
    pub fn int32Value(val: *const Value, context: *const Context) ?i32 {
        var result: MaybeInt32 = .{ .has_value = 0, .value = 0 };
        c.v8__Value__Int32Value(val, context, &result);
        if (result.has_value != 0) return result.value;
        return null;
    }

    /// Get Number (f64) value.
    pub fn numberValue(val: *const Value, context: *const Context) ?f64 {
        var result: MaybeF64 = .{ .has_value = 0, .value = 0 };
        c.v8__Value__NumberValue(val, context, &result);
        if (result.has_value != 0) return result.value;
        return null;
    }

    pub fn booleanValue(val: *const Value, isolate: *Isolate) bool {
        return c.v8__Value__BooleanValue(val, isolate);
    }
};

// ============================================================
// FunctionTemplate API
// ============================================================

pub const FunctionCallback = *const fn (*const FunctionCallbackInfo) callconv(.c) void;

pub const FunctionTemplateApi = struct {
    pub fn create(
        isolate: *Isolate,
        callback: ?FunctionCallback,
    ) ?*const FunctionTemplate {
        return c.v8__FunctionTemplate__New(isolate, callback, null, null, 0, 0, null);
    }
};

// ============================================================
// Object API
// ============================================================

pub const ObjectApi = struct {
    pub fn set(obj: *const Object, context: *const Context, key: *const Value, value: *const Value) bool {
        return c.v8__Object__Set(obj, context, key, value) != 0;
    }

    pub fn get(obj: *const Object, context: *const Context, key: *const Value) ?*const Value {
        return c.v8__Object__Get(obj, context, key);
    }
};

// ============================================================
// High-level eval helper
// ============================================================

pub const EvalError = error{
    StringCreationFailed,
    CompilationFailed,
    ExecutionFailed,
};

/// Evaluate a JavaScript string and return the result value.
pub fn eval(isolate: *Isolate, context: *const Context, code: []const u8, filename: []const u8) EvalError!*const Value {
    const source = StringApi.fromUtf8(isolate, code) orelse
        return error.StringCreationFailed;
    const name = StringApi.fromUtf8(isolate, filename) orelse
        return error.StringCreationFailed;

    var origin = ScriptOrigin.init(@ptrCast(name));

    const script = ScriptApi.compile(context, source, &origin) orelse
        return error.CompilationFailed;

    return ScriptApi.run(script, context) orelse
        return error.ExecutionFailed;
}

// ============================================================
// Internal: Maybe types for V8 out-params
// ============================================================

const MaybeInt32 = extern struct {
    has_value: u8,
    value: i32,
};

const MaybeF64 = extern struct {
    has_value: u8,
    value: f64,
};

// ============================================================
// Extern declarations — binding.cc functions from librusty_v8.a
// ============================================================

const c = struct {
    // Platform + Engine
    extern fn v8__V8__GetVersion() [*:0]const u8;
    extern fn v8__Platform__NewDefaultPlatform(thread_pool_size: c_int, idle_task_support: bool) ?*Platform;
    extern fn v8__V8__InitializePlatform(platform: *Platform) void;
    extern fn v8__V8__Initialize() void;
    extern fn v8__V8__Dispose() bool;
    extern fn v8__V8__DisposePlatform() void;

    // Isolate
    extern fn v8__Isolate__Enter(isolate: *Isolate) void;
    extern fn v8__Isolate__Exit(isolate: *Isolate) void;
    extern fn v8__Isolate__Dispose(isolate: *Isolate) void;
    extern fn v8__Isolate__GetCurrent() ?*Isolate;
    extern fn v8__Isolate__ThrowException(isolate: *Isolate, exception_val: *const Value) *const Value;

    // HandleScope (in-place construction into caller buffer)
    extern fn v8__HandleScope__CONSTRUCT(buf: *[3 * @sizeOf(usize)]u8, isolate: *Isolate) void;
    extern fn v8__HandleScope__DESTRUCT(buf: *[3 * @sizeOf(usize)]u8) void;

    // TryCatch (in-place construction)
    extern fn v8__TryCatch__CONSTRUCT(buf: *[6 * @sizeOf(usize)]u8, isolate: *Isolate) void;
    extern fn v8__TryCatch__DESTRUCT(buf: *[6 * @sizeOf(usize)]u8) void;
    extern fn v8__TryCatch__HasCaught(buf: *const [6 * @sizeOf(usize)]u8) bool;
    extern fn v8__TryCatch__Exception(buf: *const [6 * @sizeOf(usize)]u8) ?*const Value;
    extern fn v8__TryCatch__StackTrace(buf: *const [6 * @sizeOf(usize)]u8, context: *const Context) ?*const Value;
    extern fn v8__TryCatch__Message(buf: *const [6 * @sizeOf(usize)]u8) ?*const Message;

    // Context
    extern fn v8__Context__New(
        isolate: *Isolate,
        templ: ?*const ObjectTemplate,
        global_object: ?*const Value,
        microtask_queue: ?*MicrotaskQueue,
    ) *const Context;
    extern fn v8__Context__Enter(ctx: *const Context) void;
    extern fn v8__Context__Exit(ctx: *const Context) void;
    extern fn v8__Context__Global(ctx: *const Context) *const Object;

    // String
    extern fn v8__String__NewFromUtf8(isolate: *Isolate, data: [*]const u8, new_type: c_int, length: c_int) ?*const String;
    extern fn v8__String__Utf8Length(str: *const String, isolate: *Isolate) c_int;
    extern fn v8__String__WriteUtf8_v2(str: *const String, isolate: *Isolate, buf: [*]u8, capacity: usize, flags: c_int, processed: ?*usize) usize;

    // ScriptOrigin (in-place construction)
    extern fn v8__ScriptOrigin__CONSTRUCT(
        buf: *[ScriptOrigin.max_size]u8,
        resource_name: *const Value,
        resource_line_offset: c_int,
        resource_column_offset: c_int,
        resource_is_shared_cross_origin: bool,
        script_id: c_int,
        source_map_url: ?*const Value,
        resource_is_opaque: bool,
        is_wasm: bool,
        is_module: bool,
        host_defined_options: ?*const Data,
    ) void;

    // Script
    extern fn v8__Script__Compile(context: *const Context, source: *const String, origin: *const [ScriptOrigin.max_size]u8) ?*const Script;
    extern fn v8__Script__Run(script: *const Script, context: *const Context) ?*const Value;

    // Value type checks
    extern fn v8__Value__IsString(val: *const Value) bool;
    extern fn v8__Value__IsInt32(val: *const Value) bool;
    extern fn v8__Value__IsNumber(val: *const Value) bool;
    extern fn v8__Value__IsUndefined(val: *const Value) bool;
    extern fn v8__Value__IsNull(val: *const Value) bool;
    extern fn v8__Value__IsBoolean(val: *const Value) bool;

    // Value conversions
    extern fn v8__Value__ToString(val: *const Value, context: *const Context) ?*const String;
    extern fn v8__Value__Int32Value(val: *const Value, context: *const Context, out: *MaybeInt32) void;
    extern fn v8__Value__NumberValue(val: *const Value, context: *const Context, out: *MaybeF64) void;
    extern fn v8__Value__BooleanValue(val: *const Value, isolate: *Isolate) bool;

    // FunctionTemplate
    extern fn v8__FunctionTemplate__New(
        isolate: *Isolate,
        callback: ?FunctionCallback,
        data_val: ?*const Value,
        signature: ?*const anyopaque,
        length: c_int,
        constructor_behavior: c_int,
        side_effect_type: ?*const anyopaque,
    ) ?*const FunctionTemplate;

    // Object
    extern fn v8__Object__Set(obj: *const Object, context: *const Context, key: *const Value, value: *const Value) u8;
    extern fn v8__Object__Get(obj: *const Object, context: *const Context, key: *const Value) ?*const Value;
};

// Functions from v8_bridge.cpp
const bridge = struct {
    extern fn edgebox_v8_create_isolate() *Isolate;
    extern fn edgebox_v8_script_origin_sizeof() usize;
    extern fn edgebox_v8_create_params_sizeof() usize;
};
