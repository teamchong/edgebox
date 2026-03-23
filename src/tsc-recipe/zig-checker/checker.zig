// Zig TypeScript Type Checker
// Validates parsed declarations. Detects type errors that don't require
// cross-file resolution (literal mismatches, missing return types, etc.)
// Compiled to WASM. Each error maps to a TSC error code.

const parser = @import("parser.zig");

pub const ErrorKind = enum(u16) {
    // TS2322: Type 'X' is not assignable to type 'Y'
    type_not_assignable = 2322,
    // TS2355: A function whose declared type is not 'void' or 'any' must return a value
    missing_return = 2355,
};

pub const Diagnostic = struct {
    code: u16, // TSC error code
    start: u32, // position in source
    len: u16,
    msg_kind: u8, // 1=numToStr, 2=strToNum, 3=boolToNum, 4=boolToStr, etc.
};

const MAX_DIAGS = 4096;
var diags: [MAX_DIAGS]Diagnostic = undefined;
var diag_count: u32 = 0;

fn addDiag(code: u16, start: u32, len: u16, msg_kind: u8) void {
    if (diag_count < MAX_DIAGS) {
        diags[diag_count] = .{ .code = code, .start = start, .len = len, .msg_kind = msg_kind };
        diag_count += 1;
    }
}

// Type compatibility matrix for literals → annotations
// Returns 0 if compatible, msg_kind if error
fn checkLiteralAssignable(init_kind: u8, annotation_kind: u8) u8 {
    // init_kind: 20=numLit, 21=strLit, 22=true, 23=false, 9=null, 10=undefined
    // annotation_kind: 1=number, 2=string, 3=boolean, 4=any, 5=unknown, 7=void, 11=custom

    // any/unknown accept everything
    if (annotation_kind == 4 or annotation_kind == 5) return 0;

    return switch (init_kind) {
        20 => switch (annotation_kind) { // number literal
            1 => 0, // number → number OK
            2 => 2, // number → string ERROR
            3 => 3, // number → boolean ERROR
            else => 0, // custom/union/etc → can't determine
        },
        21 => switch (annotation_kind) { // string literal
            1 => 1, // string → number ERROR
            2 => 0, // string → string OK
            3 => 4, // string → boolean ERROR
            else => 0,
        },
        22, 23 => switch (annotation_kind) { // true/false
            1 => 5, // boolean → number ERROR
            2 => 6, // boolean → string ERROR
            3 => 0, // boolean → boolean OK
            else => 0,
        },
        9 => switch (annotation_kind) { // null
            1, 2, 3 => 7, // null → primitive ERROR (strictNullChecks)
            else => 0,
        },
        10 => switch (annotation_kind) { // undefined
            1, 2, 3 => 8, // undefined → primitive ERROR (strictNullChecks)
            else => 0,
        },
        else => 0, // can't determine
    };
}

/// Parse and check a source file in one call.
export fn parseAndCheck(src_ptr: [*]const u8, src_len: u32) u32 {
    _ = parser.doParse(src_ptr, src_len);
    return check();
}

/// Check all parsed declarations for type errors.
fn check() u32 {
    diag_count = 0;
    const nc = parser.nodeCount();

    var i: u32 = 0;
    while (i < nc) : (i += 1) {
        const kind = parser.nodeKind(i);

        // Variable declaration with type annotation AND literal initializer
        if (kind == 1) { // var_decl
            const type_kind = parser.nodeTypeKind(i);
            const init_kind = parser.nodeInitTypeKind(i);

            // Both annotation and literal initializer present
            if (type_kind > 0 and type_kind <= 10 and init_kind >= 20) {
                const msg = checkLiteralAssignable(init_kind, type_kind);
                if (msg > 0) {
                    addDiag(2322, parser.nodeNameStart(i), parser.nodeNameLen(i), msg);
                }
            }
        }
    }
    return diag_count;
}

export fn getDiagCount() u32 {
    return diag_count;
}

export fn getDiagCode(idx: u32) u16 {
    if (idx >= diag_count) return 0;
    return diags[idx].code;
}

export fn getDiagStart(idx: u32) u32 {
    if (idx >= diag_count) return 0;
    return diags[idx].start;
}

export fn getDiagLen(idx: u32) u16 {
    if (idx >= diag_count) return 0;
    return diags[idx].len;
}

export fn getDiagMsgKind(idx: u32) u8 {
    if (idx >= diag_count) return 0;
    return diags[idx].msg_kind;
}
