/// Native string_decoder module - QuickJS C functions
/// Handles multi-byte character boundaries when streaming text
const std = @import("std");
const quickjs = @import("../quickjs_core.zig");
const qjs = quickjs.c;

/// Get expected byte count for UTF-8 sequence starting with this byte
inline fn utf8SeqLen(byte: u8) u3 {
    if (byte & 0x80 == 0) return 1; // ASCII
    if (byte & 0xE0 == 0xC0) return 2; // 110xxxxx
    if (byte & 0xF0 == 0xE0) return 3; // 1110xxxx
    if (byte & 0xF8 == 0xF0) return 4; // 11110xxx
    return 1; // Invalid, treat as 1
}

/// Helper to get raw bytes from a TypedArray/ArrayBuffer
fn getBufferBytes(ctx: ?*qjs.JSContext, val: qjs.JSValue) ?[]const u8 {
    var offset: usize = undefined;
    var byte_len: usize = undefined;
    var bytes_per_element: usize = undefined;
    const array_buf = qjs.JS_GetTypedArrayBuffer(ctx, val, &offset, &byte_len, &bytes_per_element);

    if (!qjs.JS_IsException(array_buf)) {
        var size: usize = undefined;
        const ptr = qjs.JS_GetArrayBuffer(ctx, &size, array_buf);
        qjs.JS_FreeValue(ctx, array_buf);
        if (ptr != null and byte_len > 0) {
            return (ptr + offset)[0..byte_len];
        }
    } else {
        const exc = qjs.JS_GetException(ctx);
        qjs.JS_FreeValue(ctx, exc);
    }

    var ab_size: usize = undefined;
    const ab_ptr = qjs.JS_GetArrayBuffer(ctx, &ab_size, val);
    if (ab_ptr != null and ab_size > 0) {
        return ab_ptr[0..ab_size];
    } else {
        const exc = qjs.JS_GetException(ctx);
        qjs.JS_FreeValue(ctx, exc);
    }
    return null;
}

/// StringDecoder.write(buffer) - decode buffer, buffering incomplete sequences
fn stringDecoderWrite(ctx: ?*qjs.JSContext, this: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) return qjs.JS_NewStringLen(ctx, "", 0);

    // Get input buffer bytes
    const input_bytes = getBufferBytes(ctx, argv[0]) orelse {
        return qjs.JS_NewStringLen(ctx, "", 0);
    };

    return processInput(ctx, this, input_bytes.ptr, input_bytes.len);
}

fn processInput(ctx: ?*qjs.JSContext, this: qjs.JSValue, input_ptr: [*]const u8, input_size: usize) qjs.JSValue {
    if (input_size == 0) return qjs.JS_NewStringLen(ctx, "", 0);

    // Get pending bytes from this object
    var pending_buf: [4]u8 = undefined;
    var pending_len: usize = 0;

    const pending_prop = qjs.JS_GetPropertyStr(ctx, this, "_pending");
    if (!qjs.JS_IsUndefined(pending_prop) and !qjs.JS_IsNull(pending_prop)) {
        var prop_size: usize = 0;
        const prop_ptr = qjs.JS_GetArrayBuffer(ctx, &prop_size, pending_prop);
        if (prop_ptr != null and prop_size <= 4) {
            pending_len = prop_size;
            @memcpy(pending_buf[0..pending_len], prop_ptr[0..prop_size]);
        }
    }
    qjs.JS_FreeValue(ctx, pending_prop);

    // Combine pending + input
    const total_len = pending_len + input_size;

    // Use stack buffer for small inputs, otherwise allocate
    var stack_buf: [1024]u8 = undefined;
    var combined: []u8 = undefined;
    var allocated = false;

    if (total_len <= stack_buf.len) {
        combined = stack_buf[0..total_len];
    } else {
        const alloc_buf = std.heap.c_allocator.alloc(u8, total_len) catch {
            return qjs.JS_NewStringLen(ctx, "", 0);
        };
        combined = alloc_buf;
        allocated = true;
    }
    defer if (allocated) std.heap.c_allocator.free(combined);

    // Copy pending + input
    if (pending_len > 0) {
        @memcpy(combined[0..pending_len], pending_buf[0..pending_len]);
    }
    @memcpy(combined[pending_len..], input_ptr[0..input_size]);

    // Find last complete UTF-8 character boundary
    var complete_end: usize = total_len;

    // Scan backwards from end to find incomplete sequence
    if (total_len > 0) {
        var i: usize = total_len;
        while (i > 0) {
            i -= 1;
            const byte = combined[i];

            // Check if this is a sequence start byte
            if (byte & 0x80 == 0) {
                // ASCII - complete
                break;
            } else if (byte & 0xC0 == 0xC0) {
                // Start of multi-byte sequence
                const seq_len = utf8SeqLen(byte);
                const bytes_available = total_len - i;

                if (bytes_available < seq_len) {
                    // Incomplete sequence
                    complete_end = i;
                }
                break;
            }
            // Continuation byte (10xxxxxx) - keep scanning
        }
    }

    // Save incomplete bytes to pending
    const new_pending_len = total_len - complete_end;
    if (new_pending_len > 0 and new_pending_len <= 4) {
        // Create new pending buffer
        const new_pending = qjs.JS_NewArrayBufferCopy(ctx, combined.ptr + complete_end, new_pending_len);
        _ = qjs.JS_SetPropertyStr(ctx, this, "_pending", new_pending);
    } else {
        // Clear pending
        _ = qjs.JS_SetPropertyStr(ctx, this, "_pending", quickjs.jsUndefined());
    }

    // Return complete characters
    if (complete_end == 0) {
        return qjs.JS_NewStringLen(ctx, "", 0);
    }
    return qjs.JS_NewStringLen(ctx, combined.ptr, complete_end);
}

/// StringDecoder.end(buffer) - flush remaining buffer
fn stringDecoderEnd(ctx: ?*qjs.JSContext, this: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    var result_parts: [2]qjs.JSValue = undefined;
    var num_parts: usize = 0;

    // If buffer provided, write it first
    if (argc >= 1 and !qjs.JS_IsUndefined(argv[0]) and !qjs.JS_IsNull(argv[0])) {
        result_parts[num_parts] = stringDecoderWrite(ctx, this, argc, argv);
        num_parts += 1;
    }

    // Get any remaining pending bytes
    const pending_prop = qjs.JS_GetPropertyStr(ctx, this, "_pending");
    if (!qjs.JS_IsUndefined(pending_prop) and !qjs.JS_IsNull(pending_prop)) {
        var prop_size: usize = 0;
        const prop_ptr = qjs.JS_GetArrayBuffer(ctx, &prop_size, pending_prop);
        if (prop_ptr != null and prop_size > 0) {
            // Return replacement character for incomplete sequence
            result_parts[num_parts] = qjs.JS_NewStringLen(ctx, "\xEF\xBF\xBD", 3);
            num_parts += 1;
        }
    }
    qjs.JS_FreeValue(ctx, pending_prop);

    // Clear pending
    _ = qjs.JS_SetPropertyStr(ctx, this, "_pending", quickjs.jsUndefined());

    // Concatenate results
    if (num_parts == 0) {
        return qjs.JS_NewStringLen(ctx, "", 0);
    } else if (num_parts == 1) {
        return result_parts[0];
    } else {
        // Concatenate two strings
        const str1 = qjs.JS_ToCString(ctx, result_parts[0]);
        const str2 = qjs.JS_ToCString(ctx, result_parts[1]);
        defer qjs.JS_FreeCString(ctx, str1);
        defer qjs.JS_FreeCString(ctx, str2);
        qjs.JS_FreeValue(ctx, result_parts[0]);
        qjs.JS_FreeValue(ctx, result_parts[1]);

        if (str1 == null) return result_parts[1];
        if (str2 == null) return result_parts[0];

        const len1 = std.mem.len(str1);
        const len2 = std.mem.len(str2);
        const buf = std.heap.c_allocator.alloc(u8, len1 + len2) catch {
            return qjs.JS_NewStringLen(ctx, "", 0);
        };
        defer std.heap.c_allocator.free(buf);

        @memcpy(buf[0..len1], str1[0..len1]);
        @memcpy(buf[len1..], str2[0..len2]);
        return qjs.JS_NewStringLen(ctx, buf.ptr, len1 + len2);
    }
}

/// Create StringDecoder class
fn createStringDecoderClass(ctx: *qjs.JSContext) qjs.JSValue {
    const class_code =
        \\(function() {
        \\    class StringDecoder {
        \\        constructor(encoding) {
        \\            this.encoding = (encoding || 'utf8').toLowerCase();
        \\            this._pending = undefined;
        \\        }
        \\        write(buf) {
        \\            if (!buf || buf.length === 0) return '';
        \\            return globalThis._string_decoder_write.call(this, buf);
        \\        }
        \\        end(buf) {
        \\            return globalThis._string_decoder_end.call(this, buf);
        \\        }
        \\    }
        \\    return StringDecoder;
        \\})()
    ;

    const result = qjs.JS_Eval(ctx, class_code.ptr, class_code.len, "<string_decoder>", qjs.JS_EVAL_TYPE_GLOBAL);
    return result;
}

/// Register string_decoder module
pub fn register(ctx: *qjs.JSContext) void {
    const global = qjs.JS_GetGlobalObject(ctx);
    defer qjs.JS_FreeValue(ctx, global);

    // Register helper functions on global
    _ = qjs.JS_SetPropertyStr(ctx, global, "_string_decoder_write", qjs.JS_NewCFunction(ctx, stringDecoderWrite, "_string_decoder_write", 1));
    _ = qjs.JS_SetPropertyStr(ctx, global, "_string_decoder_end", qjs.JS_NewCFunction(ctx, stringDecoderEnd, "_string_decoder_end", 1));

    // Create string_decoder module object
    const sd_obj = qjs.JS_NewObject(ctx);

    // Add StringDecoder class
    const sd_class = createStringDecoderClass(ctx);
    if (!qjs.JS_IsException(sd_class)) {
        _ = qjs.JS_SetPropertyStr(ctx, sd_obj, "StringDecoder", sd_class);
    }

    // Set in _modules for require('string_decoder')
    const modules_val = qjs.JS_GetPropertyStr(ctx, global, "_modules");
    if (!qjs.JS_IsUndefined(modules_val)) {
        _ = qjs.JS_SetPropertyStr(ctx, modules_val, "string_decoder", qjs.JS_DupValue(ctx, sd_obj));
        _ = qjs.JS_SetPropertyStr(ctx, modules_val, "node:string_decoder", sd_obj);
        qjs.JS_FreeValue(ctx, modules_val);
    } else {
        qjs.JS_FreeValue(ctx, sd_obj);
    }
}
