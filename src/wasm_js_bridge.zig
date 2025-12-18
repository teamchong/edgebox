//! JS ↔ WASM Value Conversion Bridge
//!
//! Converts between QuickJS JSValue and WASM values for zero-overhead calls.

const std = @import("std");

// Import QuickJS C API (defined in wasm_main_static.zig)
const qjs = @cImport({
    @cInclude("quickjs.h");
});

// Import WAMR C API
const wamr = @cImport({
    @cInclude("wasm_export.h");
});

/// Convert JSValue to WASM value
pub fn jsValueToWasmVal(ctx: *qjs.JSContext, js_val: qjs.JSValue) wamr.wasm_val_t {
    var wasm_val: wamr.wasm_val_t = undefined;

    const tag = qjs.JS_VALUE_GET_TAG(js_val);

    switch (tag) {
        qjs.JS_TAG_INT => {
            // Int32
            wasm_val.kind = wamr.WASM_I32;
            wasm_val.of.i32 = qjs.JS_VALUE_GET_INT(js_val);
        },
        qjs.JS_TAG_BOOL => {
            // Boolean -> i32 (0 or 1)
            wasm_val.kind = wamr.WASM_I32;
            wasm_val.of.i32 = if (qjs.JS_VALUE_GET_BOOL(js_val)) 1 else 0;
        },
        qjs.JS_TAG_FLOAT64 => {
            // Float64
            wasm_val.kind = wamr.WASM_F64;
            var f64_val: f64 = 0;
            _ = qjs.JS_ToFloat64(ctx, &f64_val, js_val);
            wasm_val.of.f64 = f64_val;
        },
        qjs.JS_TAG_NULL, qjs.JS_TAG_UNDEFINED => {
            // null/undefined -> i32(0)
            wasm_val.kind = wamr.WASM_I32;
            wasm_val.of.i32 = 0;
        },
        else => {
            // Try to convert to number
            var f64_val: f64 = 0;
            if (qjs.JS_ToFloat64(ctx, &f64_val, js_val) == 0) {
                // Check if integer
                if (f64_val == @floor(f64_val) and f64_val >= -2147483648.0 and f64_val <= 2147483647.0) {
                    wasm_val.kind = wamr.WASM_I32;
                    wasm_val.of.i32 = @intFromFloat(f64_val);
                } else {
                    wasm_val.kind = wamr.WASM_F64;
                    wasm_val.of.f64 = f64_val;
                }
            } else {
                // Conversion failed, default to 0
                wasm_val.kind = wamr.WASM_I32;
                wasm_val.of.i32 = 0;
            }
        },
    }

    return wasm_val;
}

/// Convert WASM value to JSValue
pub fn wasmValToJSValue(ctx: *qjs.JSContext, wasm_val: wamr.wasm_val_t) qjs.JSValue {
    return switch (wasm_val.kind) {
        wamr.WASM_I32 => qjs.JS_NewInt32(ctx, wasm_val.of.i32),
        wamr.WASM_I64 => blk: {
            // i64 -> number (may lose precision for large values)
            const f64_val: f64 = @floatFromInt(wasm_val.of.i64);
            break :blk qjs.JS_NewFloat64(ctx, f64_val);
        },
        wamr.WASM_F32 => qjs.JS_NewFloat64(ctx, @as(f64, wasm_val.of.f32)),
        wamr.WASM_F64 => qjs.JS_NewFloat64(ctx, wasm_val.of.f64),
        else => qjs.JS_UNDEFINED, // Unknown type
    };
}

/// Get WASM value kind name for debugging
pub fn wasmValKindName(kind: u8) []const u8 {
    return switch (kind) {
        wamr.WASM_I32 => "i32",
        wamr.WASM_I64 => "i64",
        wamr.WASM_F32 => "f32",
        wamr.WASM_F64 => "f64",
        else => "unknown",
    };
}
