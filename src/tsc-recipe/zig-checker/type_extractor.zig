// Zig Type Annotation Extractor
// Reads token stream from tokenizer, extracts type annotations.
// Output: flat array of (name_start, name_len, type_kind, decl_kind)
// Compiled to WASM, runs in recipe.

const tokenizer = @import("tokenizer.zig");
const Token = tokenizer.Token;
const TokenKind = tokenizer.TokenKind;

pub const TypeKind = enum(u8) {
    t_number = 1,
    t_string = 2,
    t_boolean = 3,
    t_any = 4,
    t_unknown = 5,
    t_never = 6,
    t_void = 7,
    t_object = 8,
    t_null = 9,
    t_undefined = 10,
    t_custom = 11, // user-defined type (identifier)
    t_union = 12, // A | B
    t_array = 13, // T[]
    t_literal_number = 20,
    t_literal_string = 21,
    t_literal_true = 22,
    t_literal_false = 23,
};

pub const DeclKind = enum(u8) {
    var_const = 1,
    var_let = 2,
    var_var = 3,
    param = 4,
    return_type = 5,
    property = 6,
    interface_prop = 7,
};

pub const TypeAnnotation = struct {
    name_start: u32, // position of variable/param name in source
    name_len: u16,
    type_kind: TypeKind,
    decl_kind: DeclKind,
    type_start: u32, // position of type annotation in source
    type_len: u16,
};

const MAX_ANNOTATIONS = 16384;
var annotations: [MAX_ANNOTATIONS]TypeAnnotation = undefined;
var annotation_count: u32 = 0;

fn getToken(idx: u32) Token {
    return .{
        .kind = @enumFromInt(tokenizer.getWasmTokenKind(idx)),
        .start = tokenizer.getWasmTokenStart(idx),
        .len = tokenizer.getWasmTokenLen(idx),
    };
}

fn tokenCount() u32 {
    return tokenizer.getWasmTokenCount();
}

fn addAnnotation(name_start: u32, name_len: u16, type_kind: TypeKind, decl_kind: DeclKind, type_start: u32, type_len: u16) void {
    if (annotation_count < MAX_ANNOTATIONS) {
        annotations[annotation_count] = .{
            .name_start = name_start,
            .name_len = name_len,
            .type_kind = type_kind,
            .decl_kind = decl_kind,
            .type_start = type_start,
            .type_len = type_len,
        };
        annotation_count += 1;
    }
}

fn tokenKindToTypeKind(kind: TokenKind) ?TypeKind {
    return switch (kind) {
        .kw_number => .t_number,
        .kw_string => .t_string,
        .kw_boolean => .t_boolean,
        .kw_any => .t_any,
        .kw_unknown => .t_unknown,
        .kw_never => .t_never,
        .kw_void => .t_void,
        .kw_object => .t_object,
        .kw_null => .t_null,
        .kw_undefined => .t_undefined,
        .identifier => .t_custom,
        else => null,
    };
}

/// Extract type annotations from token stream.
/// Must call tokenizer.tokenize() first.
export fn extractTypes() u32 {
    annotation_count = 0;
    const tc = tokenCount();
    var i: u32 = 0;

    while (i < tc) {
        const tok = getToken(i);

        // const/let/var NAME : TYPE
        if (tok.kind == .kw_const or tok.kind == .kw_let or tok.kind == .kw_var) {
            const decl_kind: DeclKind = switch (tok.kind) {
                .kw_const => .var_const,
                .kw_let => .var_let,
                else => .var_var,
            };
            if (i + 1 < tc) {
                const name_tok = getToken(i + 1);
                if (name_tok.kind == .identifier) {
                    // Look for : after name
                    if (i + 2 < tc and getToken(i + 2).kind == .colon) {
                        if (i + 3 < tc) {
                            const type_tok = getToken(i + 3);
                            if (tokenKindToTypeKind(type_tok.kind)) |tk| {
                                addAnnotation(name_tok.start, name_tok.len, tk, decl_kind, type_tok.start, type_tok.len);
                                i += 4;
                                continue;
                            }
                        }
                    }
                }
            }
        }

        // function NAME ( PARAM : TYPE, ... ) : RETURN_TYPE
        if (tok.kind == .kw_function and i + 1 < tc) {
            const name_tok = getToken(i + 1);
            if (name_tok.kind == .identifier and i + 2 < tc and getToken(i + 2).kind == .open_paren) {
                // Scan parameters
                var j = i + 3;
                while (j < tc and getToken(j).kind != .close_paren) {
                    const param_tok = getToken(j);
                    if (param_tok.kind == .identifier and j + 1 < tc and getToken(j + 1).kind == .colon and j + 2 < tc) {
                        const ptype_tok = getToken(j + 2);
                        if (tokenKindToTypeKind(ptype_tok.kind)) |tk| {
                            addAnnotation(param_tok.start, param_tok.len, tk, .param, ptype_tok.start, ptype_tok.len);
                        }
                    }
                    j += 1;
                }
                // Check for return type: ) : TYPE
                if (j < tc and getToken(j).kind == .close_paren and j + 1 < tc and getToken(j + 1).kind == .colon and j + 2 < tc) {
                    const ret_tok = getToken(j + 2);
                    if (tokenKindToTypeKind(ret_tok.kind)) |tk| {
                        addAnnotation(name_tok.start, name_tok.len, tk, .return_type, ret_tok.start, ret_tok.len);
                    }
                }
                i = j + 1;
                continue;
            }
        }

        // interface NAME { PROP : TYPE; ... }
        if (tok.kind == .kw_interface and i + 1 < tc) {
            const iface_tok = getToken(i + 1);
            if (iface_tok.kind == .identifier) {
                var j = i + 2;
                // Skip to opening brace
                while (j < tc and getToken(j).kind != .open_brace) : (j += 1) {}
                j += 1; // skip {
                // Scan properties
                while (j < tc and getToken(j).kind != .close_brace) {
                    const prop_tok = getToken(j);
                    // PROP : TYPE or PROP? : TYPE
                    if (prop_tok.kind == .identifier) {
                        var k = j + 1;
                        if (k < tc and getToken(k).kind == .question) k += 1; // optional
                        if (k < tc and getToken(k).kind == .colon and k + 1 < tc) {
                            const ptype_tok = getToken(k + 1);
                            if (tokenKindToTypeKind(ptype_tok.kind)) |tk| {
                                addAnnotation(prop_tok.start, prop_tok.len, tk, .interface_prop, ptype_tok.start, ptype_tok.len);
                            }
                        }
                    }
                    j += 1;
                }
                i = j + 1;
                continue;
            }
        }

        i += 1;
    }
    return annotation_count;
}

export fn getAnnotationCount() u32 {
    return annotation_count;
}

export fn getAnnotationNameStart(idx: u32) u32 {
    if (idx >= annotation_count) return 0;
    return annotations[idx].name_start;
}

export fn getAnnotationNameLen(idx: u32) u16 {
    if (idx >= annotation_count) return 0;
    return annotations[idx].name_len;
}

export fn getAnnotationTypeKind(idx: u32) u8 {
    if (idx >= annotation_count) return 0;
    return @intFromEnum(annotations[idx].type_kind);
}

export fn getAnnotationDeclKind(idx: u32) u8 {
    if (idx >= annotation_count) return 0;
    return @intFromEnum(annotations[idx].decl_kind);
}
