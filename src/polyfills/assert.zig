/// Native assert module - QuickJS C functions
/// Provides Node.js-compatible assertion functions
const std = @import("std");
const builtin = @import("builtin");
const quickjs = @import("../quickjs_core.zig");
const qjs = quickjs.c;
const util_polyfill = @import("util.zig");

/// Throw an AssertionError with message
fn throwAssertionError(ctx: ?*qjs.JSContext, message: ?[*:0]const u8, actual: qjs.JSValue, expected: qjs.JSValue, operator: [*:0]const u8) qjs.JSValue {
    // Create Error object
    const err = qjs.JS_NewError(ctx);

    // Set name
    _ = qjs.JS_SetPropertyStr(ctx, err, "name", qjs.JS_NewString(ctx, "AssertionError"));

    // Set message
    if (message != null) {
        _ = qjs.JS_SetPropertyStr(ctx, err, "message", qjs.JS_NewString(ctx, message.?));
    } else {
        // Generate default message
        var msg_buf: [256]u8 = undefined;
        const msg_slice = std.fmt.bufPrint(&msg_buf, "Expected values to satisfy {s}", .{std.mem.span(operator)}) catch &[_]u8{};
        _ = qjs.JS_SetPropertyStr(ctx, err, "message", qjs.JS_NewStringLen(ctx, msg_slice.ptr, msg_slice.len));
    }

    // Set actual, expected, operator properties
    _ = qjs.JS_SetPropertyStr(ctx, err, "actual", qjs.JS_DupValue(ctx, actual));
    _ = qjs.JS_SetPropertyStr(ctx, err, "expected", qjs.JS_DupValue(ctx, expected));
    _ = qjs.JS_SetPropertyStr(ctx, err, "operator", qjs.JS_NewString(ctx, operator));

    return qjs.JS_Throw(ctx, err);
}

/// assert(value, message) / assert.ok(value, message)
fn assertOk(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) return quickjs.jsUndefined();

    const truthy = qjs.JS_ToBool(ctx, argv[0]);
    if (truthy != 1) {
        const message = if (argc > 1) qjs.JS_ToCString(ctx, argv[1]) else null;
        defer if (message != null) qjs.JS_FreeCString(ctx, message);
        return throwAssertionError(ctx, message, argv[0], quickjs.jsTrue(), "ok");
    }

    return quickjs.jsUndefined();
}

/// assert.strictEqual(actual, expected, message)
fn assertStrictEqual(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 2) return qjs.JS_ThrowTypeError(ctx, "strictEqual requires 2 arguments");

    // Use JS strict equality check (returns bool)
    const equal = qjs.JS_IsStrictEqual(ctx, argv[0], argv[1]);
    if (!equal) {
        const message = if (argc > 2) qjs.JS_ToCString(ctx, argv[2]) else null;
        defer if (message != null) qjs.JS_FreeCString(ctx, message);
        return throwAssertionError(ctx, message, argv[0], argv[1], "strictEqual");
    }

    return quickjs.jsUndefined();
}

/// assert.notStrictEqual(actual, expected, message)
fn assertNotStrictEqual(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 2) return qjs.JS_ThrowTypeError(ctx, "notStrictEqual requires 2 arguments");

    const equal = qjs.JS_IsStrictEqual(ctx, argv[0], argv[1]);
    if (equal) {
        const message = if (argc > 2) qjs.JS_ToCString(ctx, argv[2]) else null;
        defer if (message != null) qjs.JS_FreeCString(ctx, message);
        return throwAssertionError(ctx, message, argv[0], argv[1], "notStrictEqual");
    }

    return quickjs.jsUndefined();
}

/// assert.equal(actual, expected, message) - loose equality (==)
fn assertEqual(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 2) return qjs.JS_ThrowTypeError(ctx, "equal requires 2 arguments");

    // Use loose equality (==) via JS_IsEqual (returns 1 for true, 0 for false, -1 for exception)
    const equal = qjs.JS_IsEqual(ctx, argv[0], argv[1]);
    if (equal != 1) {
        const message = if (argc > 2) qjs.JS_ToCString(ctx, argv[2]) else null;
        defer if (message != null) qjs.JS_FreeCString(ctx, message);
        return throwAssertionError(ctx, message, argv[0], argv[1], "==");
    }

    return quickjs.jsUndefined();
}

/// assert.notEqual(actual, expected, message) - loose inequality (!=)
fn assertNotEqual(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 2) return qjs.JS_ThrowTypeError(ctx, "notEqual requires 2 arguments");

    const equal = qjs.JS_IsEqual(ctx, argv[0], argv[1]);
    if (equal == 1) {
        const message = if (argc > 2) qjs.JS_ToCString(ctx, argv[2]) else null;
        defer if (message != null) qjs.JS_FreeCString(ctx, message);
        return throwAssertionError(ctx, message, argv[0], argv[1], "!=");
    }

    return quickjs.jsUndefined();
}

/// assert.deepStrictEqual(actual, expected, message)
fn assertDeepStrictEqual(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 2) return qjs.JS_ThrowTypeError(ctx, "deepStrictEqual requires 2 arguments");

    // Call util.isDeepStrictEqual
    const equal = util_polyfill.isDeepStrictEqualInternal(ctx, argv[0], argv[1]);
    if (!equal) {
        const message = if (argc > 2) qjs.JS_ToCString(ctx, argv[2]) else null;
        defer if (message != null) qjs.JS_FreeCString(ctx, message);
        return throwAssertionError(ctx, message, argv[0], argv[1], "deepStrictEqual");
    }

    return quickjs.jsUndefined();
}

/// assert.notDeepStrictEqual(actual, expected, message)
fn assertNotDeepStrictEqual(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 2) return qjs.JS_ThrowTypeError(ctx, "notDeepStrictEqual requires 2 arguments");

    const equal = util_polyfill.isDeepStrictEqualInternal(ctx, argv[0], argv[1]);
    if (equal) {
        const message = if (argc > 2) qjs.JS_ToCString(ctx, argv[2]) else null;
        defer if (message != null) qjs.JS_FreeCString(ctx, message);
        return throwAssertionError(ctx, message, argv[0], argv[1], "notDeepStrictEqual");
    }

    return quickjs.jsUndefined();
}

/// assert.fail(message) - Always throw AssertionError
fn assertFail(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    const message = if (argc > 0) qjs.JS_ToCString(ctx, argv[0]) else null;
    defer if (message != null) qjs.JS_FreeCString(ctx, message);
    return throwAssertionError(ctx, message, quickjs.jsUndefined(), quickjs.jsUndefined(), "fail");
}

/// assert.ifError(value) - Throw if value is truthy
fn assertIfError(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) return quickjs.jsUndefined();

    // Check if value is null or undefined (these are OK)
    if (qjs.JS_IsNull(argv[0]) or qjs.JS_IsUndefined(argv[0])) {
        return quickjs.jsUndefined();
    }

    // Value is truthy - throw it if it's an Error, otherwise create AssertionError
    const is_error = qjs.JS_IsError(argv[0]);
    if (is_error) {
        return qjs.JS_Throw(ctx, qjs.JS_DupValue(ctx, argv[0]));
    }

    return throwAssertionError(ctx, "Got unwanted exception", argv[0], quickjs.jsNull(), "ifError");
}

/// assert.throws(fn, error, message) - Assert that function throws
fn assertThrows(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) return qjs.JS_ThrowTypeError(ctx, "throws requires a function");

    if (!qjs.JS_IsFunction(ctx, argv[0])) {
        return qjs.JS_ThrowTypeError(ctx, "First argument must be a function");
    }

    // Call the function
    const result = qjs.JS_Call(ctx, argv[0], quickjs.jsUndefined(), 0, null);

    // Check if it threw
    if (qjs.JS_IsException(result)) {
        // Get the exception
        const exc = qjs.JS_GetException(ctx);
        defer qjs.JS_FreeValue(ctx, exc);

        // If error pattern provided (argc > 1), validate it
        if (argc > 1 and !qjs.JS_IsUndefined(argv[1]) and !qjs.JS_IsNull(argv[1])) {
            // Check if error matches expected (RegExp, class, or validation function)
            if (qjs.JS_IsFunction(ctx, argv[1])) {
                // Call validator function
                var args = [1]qjs.JSValue{exc};
                const valid = qjs.JS_Call(ctx, argv[1], quickjs.jsUndefined(), 1, &args);
                defer qjs.JS_FreeValue(ctx, valid);

                if (qjs.JS_ToBool(ctx, valid) != 1) {
                    const message = if (argc > 2) qjs.JS_ToCString(ctx, argv[2]) else null;
                    defer if (message != null) qjs.JS_FreeCString(ctx, message);
                    return throwAssertionError(ctx, message orelse "Expected function to throw matching error", exc, argv[1], "throws");
                }
            }
            // For other types (RegExp, class), we'd need more complex checking
            // For now, just accept any error if validation function not provided
        }

        return quickjs.jsUndefined();
    }

    qjs.JS_FreeValue(ctx, result);

    // Function didn't throw
    const message = if (argc > 1 and qjs.JS_IsString(argv[1])) qjs.JS_ToCString(ctx, argv[1]) else if (argc > 2) qjs.JS_ToCString(ctx, argv[2]) else null;
    defer if (message != null) qjs.JS_FreeCString(ctx, message);
    return throwAssertionError(ctx, message orelse "Expected function to throw", quickjs.jsUndefined(), quickjs.jsUndefined(), "throws");
}

/// assert.doesNotThrow(fn, error, message) - Assert that function does not throw
fn assertDoesNotThrow(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) return qjs.JS_ThrowTypeError(ctx, "doesNotThrow requires a function");

    if (!qjs.JS_IsFunction(ctx, argv[0])) {
        return qjs.JS_ThrowTypeError(ctx, "First argument must be a function");
    }

    // Call the function
    const result = qjs.JS_Call(ctx, argv[0], quickjs.jsUndefined(), 0, null);

    // Check if it threw
    if (qjs.JS_IsException(result)) {
        const exc = qjs.JS_GetException(ctx);
        defer qjs.JS_FreeValue(ctx, exc);

        const message = if (argc > 1 and qjs.JS_IsString(argv[1])) qjs.JS_ToCString(ctx, argv[1]) else if (argc > 2) qjs.JS_ToCString(ctx, argv[2]) else null;
        defer if (message != null) qjs.JS_FreeCString(ctx, message);
        return throwAssertionError(ctx, message orelse "Got unwanted exception", exc, quickjs.jsUndefined(), "doesNotThrow");
    }

    qjs.JS_FreeValue(ctx, result);
    return quickjs.jsUndefined();
}

/// assert.match(string, regexp, message) - Assert string matches regexp
fn assertMatch(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 2) return qjs.JS_ThrowTypeError(ctx, "match requires 2 arguments");

    // Call regexp.test(string) using JS
    const test_func = qjs.JS_GetPropertyStr(ctx, argv[1], "test");
    if (qjs.JS_IsUndefined(test_func) or !qjs.JS_IsFunction(ctx, test_func)) {
        qjs.JS_FreeValue(ctx, test_func);
        return qjs.JS_ThrowTypeError(ctx, "Second argument must be a RegExp");
    }
    defer qjs.JS_FreeValue(ctx, test_func);

    var args = [1]qjs.JSValue{argv[0]};
    const result = qjs.JS_Call(ctx, test_func, argv[1], 1, &args);
    defer qjs.JS_FreeValue(ctx, result);

    if (qjs.JS_ToBool(ctx, result) != 1) {
        const message = if (argc > 2) qjs.JS_ToCString(ctx, argv[2]) else null;
        defer if (message != null) qjs.JS_FreeCString(ctx, message);
        return throwAssertionError(ctx, message, argv[0], argv[1], "match");
    }

    return quickjs.jsUndefined();
}

/// assert.doesNotMatch(string, regexp, message) - Assert string does not match regexp
fn assertDoesNotMatch(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 2) return qjs.JS_ThrowTypeError(ctx, "doesNotMatch requires 2 arguments");

    const test_func = qjs.JS_GetPropertyStr(ctx, argv[1], "test");
    if (qjs.JS_IsUndefined(test_func) or !qjs.JS_IsFunction(ctx, test_func)) {
        qjs.JS_FreeValue(ctx, test_func);
        return qjs.JS_ThrowTypeError(ctx, "Second argument must be a RegExp");
    }
    defer qjs.JS_FreeValue(ctx, test_func);

    var args = [1]qjs.JSValue{argv[0]};
    const result = qjs.JS_Call(ctx, test_func, argv[1], 1, &args);
    defer qjs.JS_FreeValue(ctx, result);

    if (qjs.JS_ToBool(ctx, result) == 1) {
        const message = if (argc > 2) qjs.JS_ToCString(ctx, argv[2]) else null;
        defer if (message != null) qjs.JS_FreeCString(ctx, message);
        return throwAssertionError(ctx, message, argv[0], argv[1], "doesNotMatch");
    }

    return quickjs.jsUndefined();
}

/// Register assert module
pub fn register(ctx: *qjs.JSContext) void {
    const global = qjs.JS_GetGlobalObject(ctx);
    defer qjs.JS_FreeValue(ctx, global);

    // Create assert function (same as assert.ok)
    const assert_func = qjs.JS_NewCFunction(ctx, assertOk, "assert", 2);

    // Add methods as properties of the function
    _ = qjs.JS_SetPropertyStr(ctx, assert_func, "ok", qjs.JS_NewCFunction(ctx, assertOk, "ok", 2));
    _ = qjs.JS_SetPropertyStr(ctx, assert_func, "strictEqual", qjs.JS_NewCFunction(ctx, assertStrictEqual, "strictEqual", 3));
    _ = qjs.JS_SetPropertyStr(ctx, assert_func, "notStrictEqual", qjs.JS_NewCFunction(ctx, assertNotStrictEqual, "notStrictEqual", 3));
    _ = qjs.JS_SetPropertyStr(ctx, assert_func, "equal", qjs.JS_NewCFunction(ctx, assertEqual, "equal", 3));
    _ = qjs.JS_SetPropertyStr(ctx, assert_func, "notEqual", qjs.JS_NewCFunction(ctx, assertNotEqual, "notEqual", 3));
    _ = qjs.JS_SetPropertyStr(ctx, assert_func, "deepEqual", qjs.JS_NewCFunction(ctx, assertDeepStrictEqual, "deepEqual", 3));
    _ = qjs.JS_SetPropertyStr(ctx, assert_func, "deepStrictEqual", qjs.JS_NewCFunction(ctx, assertDeepStrictEqual, "deepStrictEqual", 3));
    _ = qjs.JS_SetPropertyStr(ctx, assert_func, "notDeepEqual", qjs.JS_NewCFunction(ctx, assertNotDeepStrictEqual, "notDeepEqual", 3));
    _ = qjs.JS_SetPropertyStr(ctx, assert_func, "notDeepStrictEqual", qjs.JS_NewCFunction(ctx, assertNotDeepStrictEqual, "notDeepStrictEqual", 3));
    _ = qjs.JS_SetPropertyStr(ctx, assert_func, "fail", qjs.JS_NewCFunction(ctx, assertFail, "fail", 1));
    _ = qjs.JS_SetPropertyStr(ctx, assert_func, "ifError", qjs.JS_NewCFunction(ctx, assertIfError, "ifError", 1));
    _ = qjs.JS_SetPropertyStr(ctx, assert_func, "throws", qjs.JS_NewCFunction(ctx, assertThrows, "throws", 3));
    _ = qjs.JS_SetPropertyStr(ctx, assert_func, "doesNotThrow", qjs.JS_NewCFunction(ctx, assertDoesNotThrow, "doesNotThrow", 3));
    _ = qjs.JS_SetPropertyStr(ctx, assert_func, "match", qjs.JS_NewCFunction(ctx, assertMatch, "match", 3));
    _ = qjs.JS_SetPropertyStr(ctx, assert_func, "doesNotMatch", qjs.JS_NewCFunction(ctx, assertDoesNotMatch, "doesNotMatch", 3));

    // Create AssertionError class
    const assertion_error_code =
        \\(function() {
        \\    class AssertionError extends Error {
        \\        constructor(options) {
        \\            super(options?.message || 'Assertion failed');
        \\            this.name = 'AssertionError';
        \\            this.code = 'ERR_ASSERTION';
        \\            this.actual = options?.actual;
        \\            this.expected = options?.expected;
        \\            this.operator = options?.operator;
        \\        }
        \\    }
        \\    return AssertionError;
        \\})()
    ;

    const assertion_error_class = qjs.JS_Eval(ctx, assertion_error_code.ptr, assertion_error_code.len, "<assert>", qjs.JS_EVAL_TYPE_GLOBAL);
    if (!qjs.JS_IsException(assertion_error_class)) {
        _ = qjs.JS_SetPropertyStr(ctx, assert_func, "AssertionError", assertion_error_class);
    }

    // Add async methods (rejects/doesNotReject) - need JS for Promise handling
    const async_methods_code =
        \\(function(assert) {
        \\    class AssertionError extends Error {
        \\        constructor(options) {
        \\            super(options?.message || 'Assertion failed');
        \\            this.name = 'AssertionError';
        \\            this.code = 'ERR_ASSERTION';
        \\            this.actual = options?.actual;
        \\            this.expected = options?.expected;
        \\            this.operator = options?.operator;
        \\        }
        \\    }
        \\
        \\    function matchError(actual, expected) {
        \\        if (expected === undefined) return true;
        \\        if (expected instanceof RegExp) {
        \\            return expected.test(actual.message || String(actual));
        \\        }
        \\        if (typeof expected === 'function') {
        \\            if (expected.prototype !== undefined && actual instanceof expected) {
        \\                return true;
        \\            }
        \\            return expected(actual) === true;
        \\        }
        \\        if (typeof expected === 'object' && expected !== null) {
        \\            for (const key of Object.keys(expected)) {
        \\                if (actual[key] !== expected[key]) return false;
        \\            }
        \\            return true;
        \\        }
        \\        return false;
        \\    }
        \\
        \\    assert.rejects = async function rejects(promiseOrFn, error, message) {
        \\        let promise;
        \\        if (typeof promiseOrFn === 'function') {
        \\            try {
        \\                promise = promiseOrFn();
        \\            } catch (e) {
        \\                promise = Promise.reject(e);
        \\            }
        \\        } else {
        \\            promise = promiseOrFn;
        \\        }
        \\
        \\        try {
        \\            await promise;
        \\        } catch (actual) {
        \\            if (typeof error === 'string') {
        \\                message = error;
        \\                error = undefined;
        \\            }
        \\            if (error !== undefined && !matchError(actual, error)) {
        \\                throw new AssertionError({
        \\                    message: message || 'Promise rejected with unexpected error',
        \\                    actual: actual,
        \\                    expected: error,
        \\                    operator: 'rejects'
        \\                });
        \\            }
        \\            return;
        \\        }
        \\
        \\        throw new AssertionError({
        \\            message: message || 'Missing expected rejection',
        \\            operator: 'rejects'
        \\        });
        \\    };
        \\
        \\    assert.doesNotReject = async function doesNotReject(promiseOrFn, error, message) {
        \\        let promise;
        \\        if (typeof promiseOrFn === 'function') {
        \\            try {
        \\                promise = promiseOrFn();
        \\            } catch (e) {
        \\                promise = Promise.reject(e);
        \\            }
        \\        } else {
        \\            promise = promiseOrFn;
        \\        }
        \\
        \\        try {
        \\            await promise;
        \\        } catch (actual) {
        \\            if (typeof error === 'string') {
        \\                message = error;
        \\                error = undefined;
        \\            }
        \\            if (error === undefined || matchError(actual, error)) {
        \\                throw new AssertionError({
        \\                    message: message || 'Got unwanted rejection',
        \\                    actual: actual,
        \\                    operator: 'doesNotReject'
        \\                });
        \\            }
        \\        }
        \\    };
        \\})
    ;

    const async_setup_fn = qjs.JS_Eval(ctx, async_methods_code.ptr, async_methods_code.len, "<assert-async>", qjs.JS_EVAL_TYPE_GLOBAL);
    if (!qjs.JS_IsException(async_setup_fn)) {
        var setup_args = [1]qjs.JSValue{assert_func};
        const setup_result = qjs.JS_Call(ctx, async_setup_fn, quickjs.jsUndefined(), 1, &setup_args);
        qjs.JS_FreeValue(ctx, setup_result);
        qjs.JS_FreeValue(ctx, async_setup_fn);
    }

    // Create assert.strict (same as assert with strict methods only)
    const strict_obj = qjs.JS_NewObject(ctx);
    _ = qjs.JS_SetPropertyStr(ctx, strict_obj, "ok", qjs.JS_NewCFunction(ctx, assertOk, "ok", 2));
    _ = qjs.JS_SetPropertyStr(ctx, strict_obj, "equal", qjs.JS_NewCFunction(ctx, assertStrictEqual, "equal", 3));
    _ = qjs.JS_SetPropertyStr(ctx, strict_obj, "notEqual", qjs.JS_NewCFunction(ctx, assertNotStrictEqual, "notEqual", 3));
    _ = qjs.JS_SetPropertyStr(ctx, strict_obj, "deepEqual", qjs.JS_NewCFunction(ctx, assertDeepStrictEqual, "deepEqual", 3));
    _ = qjs.JS_SetPropertyStr(ctx, strict_obj, "notDeepEqual", qjs.JS_NewCFunction(ctx, assertNotDeepStrictEqual, "notDeepEqual", 3));
    // DupValue because SetPropertyStr consumes, and we need to use strict_obj again below
    _ = qjs.JS_SetPropertyStr(ctx, assert_func, "strict", qjs.JS_DupValue(ctx, strict_obj));

    // Set in _modules for require('assert')
    const modules_val = qjs.JS_GetPropertyStr(ctx, global, "_modules");
    if (!qjs.JS_IsUndefined(modules_val)) {
        _ = qjs.JS_SetPropertyStr(ctx, modules_val, "assert", qjs.JS_DupValue(ctx, assert_func));
        _ = qjs.JS_SetPropertyStr(ctx, modules_val, "node:assert", qjs.JS_DupValue(ctx, assert_func));
        _ = qjs.JS_SetPropertyStr(ctx, modules_val, "assert/strict", qjs.JS_DupValue(ctx, strict_obj));
        // Last use - SetPropertyStr consumes the ref
        _ = qjs.JS_SetPropertyStr(ctx, modules_val, "node:assert/strict", strict_obj);
        qjs.JS_FreeValue(ctx, modules_val);
    } else {
        // modules_val is undefined, so we need to free strict_obj ourselves
        qjs.JS_FreeValue(ctx, strict_obj);
    }
    qjs.JS_FreeValue(ctx, assert_func);
}
