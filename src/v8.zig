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
pub const Isolate = opaque {};
pub const Context = opaque {};
pub const Value = opaque {};
const Script = opaque {};
pub const String = opaque {};
pub const Object = opaque {};
const ObjectTemplate = opaque {};
pub const FunctionTemplate = opaque {};
const MicrotaskQueue = opaque {};
const Data = opaque {};
const Message = opaque {};
pub const FunctionCallbackInfo = opaque {};
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
// SharedArrayBuffer API (zero-copy cross-isolate memory)
// ============================================================

fn noopDeleter(_: ?*anyopaque, _: usize, _: ?*anyopaque) callconv(.c) void {}

pub const SharedArrayBufferApi = struct {
    /// Create a SharedArrayBuffer backed by existing Zig memory (zero-copy).
    /// The memory is NOT owned by V8 — Zig manages its lifetime.
    /// Multiple isolates can create views into the same buffer.
    pub fn fromExternalMemory(isolate: *Isolate, data: [*]u8, byte_length: usize) ?*const Value {
        const backing_store = c.v8__SharedArrayBuffer__NewBackingStore__with_data(
            @ptrCast(data), byte_length,
            &noopDeleter, null,
        ) orelse return null;
        return c.v8__SharedArrayBuffer__New__with_backing_store(isolate, backing_store);
    }

    /// Create a new SharedArrayBuffer with V8-allocated memory.
    pub fn create(isolate: *Isolate, byte_length: usize) ?*const Value {
        return c.v8__SharedArrayBuffer__New__with_byte_length(isolate, byte_length);
    }

    pub fn byteLength(sab: *const Value) usize {
        return c.v8__SharedArrayBuffer__ByteLength(sab);
    }
};

// ============================================================
// FunctionCallbackInfo + ReturnValue API
// ============================================================

pub const FunctionCallback = *const fn (*const FunctionCallbackInfo) callconv(.c) void;

/// ReturnValue wraps the internal value slot pointer from V8.
/// GetReturnValue returns uintptr_t* (the slot pointer).
/// ReturnValue__Set expects ReturnValue* (pointer TO a struct containing the slot).
/// So we store the slot in a struct and pass &self to Set.
pub const ReturnValue = struct {
    slot: *anyopaque,

    pub fn set(self: *ReturnValue, value: *const Value) void {
        c.v8__ReturnValue__Value__Set(self, value);
    }

    pub fn setInt32(self: *ReturnValue, val: i32) void {
        c.v8__ReturnValue__Value__Set__Int32(self, val);
    }

    pub fn setUndefined(self: *ReturnValue) void {
        c.v8__ReturnValue__Value__SetUndefined(self);
    }

    pub fn setNull(self: *ReturnValue) void {
        c.v8__ReturnValue__Value__SetNull(self);
    }
};

pub const CallbackInfoApi = struct {
    pub fn getIsolate(info: *const FunctionCallbackInfo) *Isolate {
        return c.v8__FunctionCallbackInfo__GetIsolate(info);
    }

    pub fn length(info: *const FunctionCallbackInfo) i32 {
        return c.v8__FunctionCallbackInfo__Length(info);
    }

    pub fn get(info: *const FunctionCallbackInfo, index: i32) ?*const Value {
        return c.v8__FunctionCallbackInfo__Get(info, index);
    }

    pub fn data(info: *const FunctionCallbackInfo) ?*const Value {
        return c.v8__FunctionCallbackInfo__Data(info);
    }

    pub fn this(info: *const FunctionCallbackInfo) *const Object {
        return c.v8__FunctionCallbackInfo__This(info);
    }

    /// Returns a ReturnValue. The caller must store it on the stack
    /// and use its methods to set the return value.
    pub fn getReturnValue(info: *const FunctionCallbackInfo) ReturnValue {
        return .{ .slot = c.v8__FunctionCallbackInfo__GetReturnValue(info) };
    }
};

// ReturnValueApi remains for backward compat but methods are now on ReturnValue itself
pub const ReturnValueApi = struct {
    pub fn set(rv: *ReturnValue, value: *const Value) void {
        rv.set(value);
    }

    pub fn setInt32(rv: *ReturnValue, val: i32) void {
        rv.setInt32(val);
    }

    pub fn setUndefined(rv: *ReturnValue) void {
        rv.setUndefined();
    }

    pub fn setNull(rv: *ReturnValue) void {
        rv.setNull();
    }
};

// ============================================================
// FunctionTemplate API
// ============================================================

pub const Function = opaque {};

pub const FunctionTemplateApi = struct {
    pub fn create(
        isolate: *Isolate,
        callback: ?FunctionCallback,
    ) ?*const FunctionTemplate {
        return c.v8__FunctionTemplate__New(isolate, callback, null, null, 0, 0, 0, null, 0);
    }

    pub fn getFunction(tmpl: *const FunctionTemplate, context: *const Context) ?*const Function {
        return c.v8__FunctionTemplate__GetFunction(tmpl, context);
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
// ScriptCompiler API — code caching
// ============================================================

const UnboundScript = opaque {};
pub const CachedData = opaque {};

/// ScriptCompiler::Source — in-place constructed, ~104 bytes on 64-bit.
pub const CompilerSource = struct {
    buf: [max_size]u8 align(@alignOf(usize)) = undefined,

    // Source is ~104 bytes on 64-bit (see binding.cc static_asserts)
    const max_size = 128; // generous upper bound

    /// Construct Source in-place. Must be called on a stable (non-moved) pointer.
    /// Do NOT assign the result of this to a new variable — call on a var directly.
    pub fn initInPlace(self: *CompilerSource, source: *const String, origin: *const ScriptOrigin, cached_data: ?*CachedData) void {
        const actual_size = bridge.edgebox_v8_source_sizeof();
        std.debug.assert(actual_size <= max_size);
        c.v8__ScriptCompiler__Source__CONSTRUCT(&self.buf, source, &origin.buf, cached_data);
    }

    pub fn deinit(self: *CompilerSource) void {
        c.v8__ScriptCompiler__Source__DESTRUCT(&self.buf);
    }

    pub fn getCachedData(self: *const CompilerSource) ?*const CachedData {
        return c.v8__ScriptCompiler__Source__GetCachedData(&self.buf);
    }
};

/// CachedData memory layout (64-bit):
///   offset 0: data pointer (8 bytes)
///   offset 8: length (4 bytes)
///   offset 12: rejected (4 bytes)
///   offset 16: buffer_policy (4 bytes + 4 padding)
const CachedDataLayout = extern struct {
    data: [*]const u8,
    length: i32,
    rejected: i32,
    buffer_policy: i32,
    _pad: i32 = 0,
};

pub const ScriptCompilerApi = struct {
    // CompileOptions
    pub const kNoCompileOptions: c_int = 0;
    pub const kConsumeCodeCache: c_int = 1;
    pub const kEagerCompile: c_int = 2;

    // NoCacheReason
    pub const kNoCacheNoReason: c_int = 0;

    /// Create CachedData from raw bytes (for loading from disk).
    pub fn createCachedData(data: [*]const u8, length: i32) *CachedData {
        return c.v8__ScriptCompiler__CachedData__NEW(data, length);
    }

    /// Delete CachedData.
    pub fn deleteCachedData(cd: *CachedData) void {
        c.v8__ScriptCompiler__CachedData__DELETE(cd);
    }

    /// Compile with options (code cache consume/produce).
    pub fn compile(context: *const Context, source: *CompilerSource, options: c_int) ?*const Script {
        return c.v8__ScriptCompiler__Compile(context, &source.buf, options, kNoCacheNoReason);
    }

    /// Get CachedData raw bytes.
    pub fn getCachedDataBytes(cd: *const CachedData) struct { data: [*]const u8, length: i32, rejected: bool } {
        const layout: *const CachedDataLayout = @ptrCast(@alignCast(cd));
        return .{
            .data = layout.data,
            .length = layout.length,
            .rejected = layout.rejected != 0,
        };
    }
};

pub const UnboundScriptApi = struct {
    /// Create code cache from an UnboundScript.
    pub fn createCodeCache(us: *const UnboundScript) *CachedData {
        return c.v8__UnboundScript__CreateCodeCache(us);
    }

    /// Bind an UnboundScript to the current context, producing a Script.
    pub fn bindToCurrentContext(us: *const UnboundScript) *const Script {
        return c.v8__UnboundScript__BindToCurrentContext(us);
    }
};

pub const ScriptExtApi = struct {
    /// Get the UnboundScript from a compiled Script (for creating code cache).
    pub fn getUnboundScript(script: *const Script) *const UnboundScript {
        return c.v8__Script__GetUnboundScript(script);
    }
};

// ============================================================
// Snapshot API — create and load V8 heap snapshots
// ============================================================

/// StartupData — V8 snapshot blob (data pointer + size).
/// Layout: { const char* data; int raw_size; } + padding.
pub const StartupData = extern struct {
    data: ?[*]const u8,
    raw_size: i32,
    _pad: i32 = 0,
};

/// SnapshotCreator — creates V8 heap snapshots.
/// Size: sizeof(size_t) = 8 bytes on 64-bit (just a pointer to impl).
pub const SnapshotCreator = struct {
    buf: [max_size]u8 align(@alignOf(usize)) = undefined,

    const max_size = @sizeOf(usize); // 8 bytes, just a pointer

    /// Create a SnapshotCreator with external references.
    /// external_refs is a null-terminated array of function pointers.
    /// The SnapshotCreator owns its Isolate — do NOT dispose it separately.
    pub fn init(external_refs: ?[*]const usize) SnapshotCreator {
        var sc = SnapshotCreator{};
        const actual_size = bridge.edgebox_v8_snapshot_creator_sizeof();
        std.debug.assert(actual_size <= max_size);
        bridge.edgebox_v8_snapshot_creator_new(&sc.buf, external_refs);
        return sc;
    }

    pub fn deinit(self: *SnapshotCreator) void {
        c.v8__SnapshotCreator__DESTRUCT(@ptrCast(&self.buf));
    }

    /// Get the Isolate owned by this SnapshotCreator.
    pub fn getIsolate(self: *const SnapshotCreator) *Isolate {
        return c.v8__SnapshotCreator__GetIsolate(@ptrCast(&self.buf));
    }

    /// Set the default context to be included in the snapshot.
    /// Must be called before createBlob().
    pub fn setDefaultContext(self: *SnapshotCreator, context: *const Context) void {
        c.v8__SnapshotCreator__SetDefaultContext(@ptrCast(&self.buf), context);
    }

    /// Create the snapshot blob. Returns StartupData with the serialized heap.
    /// The caller must free the data with StartupData.delete().
    /// function_code_handling: 0 = kClear, 1 = kKeep
    pub fn createBlob(self: *SnapshotCreator, function_code_handling: c_int) StartupData {
        return c.v8__SnapshotCreator__CreateBlob(@ptrCast(&self.buf), function_code_handling);
    }

    /// FunctionCodeHandling enum values
    pub const kClear: c_int = 0;
    pub const kKeep: c_int = 1;
};

pub const SnapshotApi = struct {
    /// Create an Isolate from a snapshot blob with external references.
    /// The snapshot data must remain valid during this call (V8 copies it).
    pub fn createIsolateFromSnapshot(
        snapshot_data: [*]const u8,
        snapshot_len: i32,
        external_refs: ?[*]const usize,
    ) *Isolate {
        return bridge.edgebox_v8_create_isolate_from_snapshot(
            snapshot_data,
            snapshot_len,
            external_refs,
        );
    }

    /// Check if StartupData is valid.
    pub fn isValid(startup: *const StartupData) bool {
        return c.v8__StartupData__IsValid(startup);
    }

    /// Delete StartupData's data buffer (allocated by V8 with new[]).
    pub fn deleteData(data: [*]const u8) void {
        c.v8__StartupData__data__DELETE(data);
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

    // FunctionCallbackInfo
    extern fn v8__FunctionCallbackInfo__GetIsolate(info: *const FunctionCallbackInfo) *Isolate;
    extern fn v8__FunctionCallbackInfo__Length(info: *const FunctionCallbackInfo) c_int;
    extern fn v8__FunctionCallbackInfo__Get(info: *const FunctionCallbackInfo, index: c_int) ?*const Value;
    extern fn v8__FunctionCallbackInfo__Data(info: *const FunctionCallbackInfo) ?*const Value;
    extern fn v8__FunctionCallbackInfo__This(info: *const FunctionCallbackInfo) *const Object;
    extern fn v8__FunctionCallbackInfo__GetReturnValue(info: *const FunctionCallbackInfo) *anyopaque;

    // ReturnValue
    extern fn v8__ReturnValue__Value__Set(rv: *ReturnValue, value: *const Value) void;
    extern fn v8__ReturnValue__Value__Set__Int32(rv: *ReturnValue, val: i32) void;
    extern fn v8__ReturnValue__Value__SetUndefined(rv: *ReturnValue) void;
    extern fn v8__ReturnValue__Value__SetNull(rv: *ReturnValue) void;

    // FunctionTemplate
    // 9 params: isolate, callback, data, signature, length, constructor_behavior,
    //           side_effect_type, c_functions, c_functions_len
    extern fn v8__FunctionTemplate__New(
        isolate: *Isolate,
        callback: ?FunctionCallback,
        data_val: ?*const Value,
        signature: ?*const anyopaque,
        length: c_int,
        constructor_behavior: c_int,
        side_effect_type: c_int,
        c_functions: ?*const anyopaque,
        c_functions_len: usize,
    ) ?*const FunctionTemplate;
    extern fn v8__FunctionTemplate__GetFunction(tmpl: *const FunctionTemplate, context: *const Context) ?*const Function;

    // Object
    extern fn v8__Object__Set(obj: *const Object, context: *const Context, key: *const Value, value: *const Value) u8;
    extern fn v8__Object__Get(obj: *const Object, context: *const Context, key: *const Value) ?*const Value;

    // SharedArrayBuffer (zero-copy cross-isolate memory)
    extern fn v8__SharedArrayBuffer__New__with_byte_length(isolate: *Isolate, byte_length: usize) ?*const Value;
    extern fn v8__SharedArrayBuffer__NewBackingStore__with_data(
        data: ?*anyopaque, byte_length: usize,
        deleter: ?*const fn (?*anyopaque, usize, ?*anyopaque) callconv(.c) void,
        deleter_data: ?*anyopaque,
    ) ?*anyopaque; // returns BackingStore*
    extern fn v8__SharedArrayBuffer__New__with_backing_store(isolate: *Isolate, backing_store: *anyopaque) ?*const Value;
    extern fn v8__SharedArrayBuffer__ByteLength(sab: *const Value) usize;

    // ScriptCompiler
    extern fn v8__ScriptCompiler__Source__CONSTRUCT(buf: *[CompilerSource.max_size]u8, source: *const String, origin: *const [ScriptOrigin.max_size]u8, cached_data: ?*CachedData) void;
    extern fn v8__ScriptCompiler__Source__DESTRUCT(buf: *[CompilerSource.max_size]u8) void;
    extern fn v8__ScriptCompiler__Source__GetCachedData(buf: *const [CompilerSource.max_size]u8) ?*const CachedData;
    extern fn v8__ScriptCompiler__CachedData__NEW(data: [*]const u8, length: c_int) *CachedData;
    extern fn v8__ScriptCompiler__CachedData__DELETE(cd: *CachedData) void;
    extern fn v8__ScriptCompiler__Compile(context: *const Context, source: *[CompilerSource.max_size]u8, options: c_int, no_cache_reason: c_int) ?*const Script;

    // UnboundScript / code cache
    extern fn v8__Script__GetUnboundScript(script: *const Script) *const UnboundScript;
    extern fn v8__UnboundScript__CreateCodeCache(us: *const UnboundScript) *CachedData;
    extern fn v8__UnboundScript__BindToCurrentContext(us: *const UnboundScript) *const Script;

    // SnapshotCreator
    extern fn v8__SnapshotCreator__DESTRUCT(self: *anyopaque) void;
    extern fn v8__SnapshotCreator__GetIsolate(self: *const anyopaque) *Isolate;
    extern fn v8__SnapshotCreator__SetDefaultContext(self: *anyopaque, context: *const Context) void;
    extern fn v8__SnapshotCreator__CreateBlob(self: *anyopaque, function_code_handling: c_int) StartupData;

    // StartupData
    extern fn v8__StartupData__IsValid(self: *const StartupData) bool;
    extern fn v8__StartupData__data__DELETE(data: [*]const u8) void;
};

// Functions from v8_bridge.cpp
const bridge = struct {
    extern fn edgebox_v8_create_isolate() *Isolate;
    extern fn edgebox_v8_create_isolate_from_snapshot(
        snapshot_data: [*]const u8,
        snapshot_len: i32,
        external_refs: ?[*]const usize,
    ) *Isolate;
    extern fn edgebox_v8_snapshot_creator_new(
        buf: *[SnapshotCreator.max_size]u8,
        external_refs: ?[*]const usize,
    ) void;
    extern fn edgebox_v8_script_origin_sizeof() usize;
    extern fn edgebox_v8_create_params_sizeof() usize;
    extern fn edgebox_v8_source_sizeof() usize;
    extern fn edgebox_v8_snapshot_creator_sizeof() usize;
};
