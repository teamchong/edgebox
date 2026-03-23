// Zig TypeScript Parser (Declaration-level)
// Parses token stream into declaration nodes for type checking.
// Handles: variable declarations, function signatures, interface definitions,
//          type aliases, import declarations.
// Does NOT handle: expressions, control flow, JSX (delegated to TSC).

const tokenizer = @import("tokenizer.zig");
const TK = tokenizer.TokenKind;

// ── AST Node Types ──

pub const NodeKind = enum(u8) {
    var_decl = 1, // const/let/var name: Type = init
    func_decl = 2, // function name(params): ReturnType
    param_decl = 3, // name: Type (inside function)
    interface_decl = 4, // interface Name { ... }
    interface_prop = 5, // propName: Type (inside interface)
    type_alias = 6, // type Name = Type
    import_decl = 7, // import { ... } from '...'
    export_decl = 8, // export { ... } or export default
};

pub const TypeRef = struct {
    kind: u8, // TypeKind from type_extractor or 0 for complex
    start: u32, // position in source
    len: u16,
};

pub const Node = struct {
    kind: NodeKind,
    name_start: u32,
    name_len: u16,
    type_ref: TypeRef, // declared type annotation (if any)
    init_type: TypeRef, // inferred type from initializer (if literal)
    parent: u16, // index of parent node (for interface members)
    flags: u8, // 1=optional, 2=readonly, 4=exported
};

// ── Node Storage ──

const MAX_NODES = 32768;
var nodes: [MAX_NODES]Node = undefined;
var node_count: u32 = 0;

fn addNode(kind: NodeKind, name_start: u32, name_len: u16, type_ref: TypeRef, init_type: TypeRef, parent: u16, flags: u8) u32 {
    if (node_count >= MAX_NODES) return node_count;
    const idx = node_count;
    nodes[idx] = .{
        .kind = kind,
        .name_start = name_start,
        .name_len = name_len,
        .type_ref = type_ref,
        .init_type = init_type,
        .parent = parent,
        .flags = flags,
    };
    node_count += 1;
    return idx;
}

// ── Parser State ──

var tpos: u32 = 0;
var tc: u32 = 0;

fn peek() TK {
    if (tpos >= tc) return .eof;
    return @enumFromInt(tokenizer.getWasmTokenKind(tpos));
}

fn peekAt(offset: u32) TK {
    if (tpos + offset >= tc) return .eof;
    return @enumFromInt(tokenizer.getWasmTokenKind(tpos + offset));
}

fn advance() void {
    if (tpos < tc) tpos += 1;
}

fn tokenStart() u32 {
    return tokenizer.getWasmTokenStart(tpos);
}

fn tokenLen() u16 {
    return tokenizer.getWasmTokenLen(tpos);
}

fn expect(kind: TK) bool {
    if (peek() == kind) {
        advance();
        return true;
    }
    return false;
}

const NO_TYPE = TypeRef{ .kind = 0, .start = 0, .len = 0 };

fn parseTypeAnnotation() TypeRef {
    // : Type
    if (peek() != .colon) return NO_TYPE;
    advance(); // skip :
    const start = tokenStart();
    const len = tokenLen();
    const kind: u8 = switch (peek()) {
        .kw_number => 1,
        .kw_string => 2,
        .kw_boolean => 3,
        .kw_any => 4,
        .kw_unknown => 5,
        .kw_never => 6,
        .kw_void => 7,
        .kw_object => 8,
        .kw_null => 9,
        .kw_undefined => 10,
        .identifier => 11, // custom type
        else => 0,
    };
    if (kind > 0) {
        advance();
        // Check for array: Type[]
        if (peek() == .open_bracket and peekAt(1) == .close_bracket) {
            advance();
            advance();
            return TypeRef{ .kind = 13, .start = start, .len = @intCast(tokenStart() + tokenLen() - start) };
        }
        // Check for union: Type | Type
        if (peek() == .pipe) {
            // Skip union members
            while (peek() == .pipe) {
                advance(); // |
                advance(); // type
            }
            return TypeRef{ .kind = 12, .start = start, .len = @intCast(tokenStart() - start) };
        }
        return TypeRef{ .kind = kind, .start = start, .len = len };
    }
    return NO_TYPE;
}

fn inferLiteralType() TypeRef {
    const start = tokenStart();
    const len = tokenLen();
    return switch (peek()) {
        .number_literal => blk: {
            advance();
            break :blk TypeRef{ .kind = 20, .start = start, .len = len };
        },
        .string_literal => blk: {
            advance();
            break :blk TypeRef{ .kind = 21, .start = start, .len = len };
        },
        .kw_true => blk: {
            advance();
            break :blk TypeRef{ .kind = 22, .start = start, .len = len };
        },
        .kw_false => blk: {
            advance();
            break :blk TypeRef{ .kind = 23, .start = start, .len = len };
        },
        .kw_null => blk: {
            advance();
            break :blk TypeRef{ .kind = 9, .start = start, .len = len };
        },
        .kw_undefined => blk: {
            advance();
            break :blk TypeRef{ .kind = 10, .start = start, .len = len };
        },
        else => NO_TYPE,
    };
}

// ── Parse Declarations ──

fn parseVarDecl(flags: u8) void {
    advance(); // const/let/var
    if (peek() != .identifier) return;
    const name_start = tokenStart();
    const name_len = tokenLen();
    advance(); // name

    const type_ref = parseTypeAnnotation();
    var init_type = NO_TYPE;

    // Check for = initializer
    if (peek() == .equals) {
        advance(); // =
        init_type = inferLiteralType();
    }

    _ = addNode(.var_decl, name_start, name_len, type_ref, init_type, 0, flags);
}

fn parseFuncDecl(flags: u8) void {
    advance(); // function
    if (peek() != .identifier) return;
    const name_start = tokenStart();
    const name_len = tokenLen();
    advance(); // name

    if (!expect(.open_paren)) return;

    const func_idx = addNode(.func_decl, name_start, name_len, NO_TYPE, NO_TYPE, 0, flags);

    // Parse parameters
    while (peek() != .close_paren and peek() != .eof) {
        if (peek() == .identifier) {
            const pname_start = tokenStart();
            const pname_len = tokenLen();
            advance(); // param name
            var pflags: u8 = 0;
            if (peek() == .question) {
                pflags |= 1; // optional
                advance();
            }
            const ptype = parseTypeAnnotation();
            _ = addNode(.param_decl, pname_start, pname_len, ptype, NO_TYPE, @intCast(func_idx), pflags);
        }
        if (peek() == .comma) advance();
        if (peek() != .close_paren and peek() != .identifier) advance(); // skip unknown
    }
    if (expect(.close_paren)) {
        // Return type
        const ret_type = parseTypeAnnotation();
        if (ret_type.kind > 0) {
            nodes[func_idx].type_ref = ret_type;
        }
    }
}

fn parseInterfaceDecl(flags: u8) void {
    advance(); // interface
    if (peek() != .identifier) return;
    const name_start = tokenStart();
    const name_len = tokenLen();
    advance(); // name

    // Skip extends
    if (peek() == .kw_extends) {
        advance();
        while (peek() == .identifier or peek() == .comma) advance();
    }

    if (!expect(.open_brace)) return;

    const iface_idx = addNode(.interface_decl, name_start, name_len, NO_TYPE, NO_TYPE, 0, flags);

    // Parse properties
    while (peek() != .close_brace and peek() != .eof) {
        if (peek() == .kw_readonly) advance();
        if (peek() == .identifier) {
            const pname_start = tokenStart();
            const pname_len = tokenLen();
            advance();
            var pflags: u8 = 0;
            if (peek() == .question) {
                pflags |= 1;
                advance();
            }
            const ptype = parseTypeAnnotation();
            _ = addNode(.interface_prop, pname_start, pname_len, ptype, NO_TYPE, @intCast(iface_idx), pflags);
        }
        // Skip to next property (;, ,, or newline boundary)
        while (peek() != .semicolon and peek() != .comma and peek() != .close_brace and peek() != .eof) advance();
        if (peek() == .semicolon or peek() == .comma) advance();
    }
    if (peek() == .close_brace) advance();
}

fn parseTypeAlias(flags: u8) void {
    advance(); // type
    if (peek() != .identifier) return;
    const name_start = tokenStart();
    const name_len = tokenLen();
    advance(); // name

    // Skip generic params <T, U>
    if (peek() == .less_than) {
        var depth: u32 = 1;
        advance();
        while (depth > 0 and peek() != .eof) {
            if (peek() == .less_than) depth += 1;
            if (peek() == .greater_than) depth -= 1;
            advance();
        }
    }

    if (!expect(.equals)) return;
    const type_ref = parseTypeAnnotation();
    // For type alias, the "type annotation" IS the equals value
    // Reuse type_ref field
    _ = addNode(.type_alias, name_start, name_len, if (type_ref.kind > 0) type_ref else blk: {
        // Complex type — record position
        const s = tokenStart();
        // Skip to semicolon
        while (peek() != .semicolon and peek() != .eof) advance();
        break :blk TypeRef{ .kind = 0, .start = s, .len = @intCast(tokenStart() - s) };
    }, NO_TYPE, 0, flags);
}

fn parseImportDecl() void {
    advance(); // import
    const start = tokenStart();
    // Skip to 'from'
    while (peek() != .kw_from and peek() != .eof and peek() != .semicolon) advance();
    if (peek() == .kw_from) {
        advance(); // from
        if (peek() == .string_literal) {
            const mod_start = tokenStart();
            const mod_len = tokenLen();
            advance();
            _ = addNode(.import_decl, mod_start, mod_len, NO_TYPE, NO_TYPE, 0, 0);
            return;
        }
    }
    _ = start;
}

// ── WASM Exports ──

export fn parse(src_ptr: [*]const u8, src_len: u32) u32 {
    // Tokenize first
    _ = tokenizer.doTokenize(src_ptr, src_len);
    tc = tokenizer.getWasmTokenCount();
    tpos = 0;
    node_count = 0;

    while (tpos < tc and peek() != .eof) {
        var flags: u8 = 0;
        if (peek() == .kw_export) {
            flags |= 4;
            advance();
            if (peek() == .kw_function or peek() == .kw_interface or peek() == .kw_type or
                peek() == .kw_const or peek() == .kw_let or peek() == .kw_var) {
                // continue to parse the declaration
            } else {
                advance(); // skip export default / export { }
                continue;
            }
        }

        switch (peek()) {
            .kw_const, .kw_let, .kw_var => parseVarDecl(flags),
            .kw_function, .kw_async => {
                if (peek() == .kw_async) advance();
                if (peek() == .kw_function) parseFuncDecl(flags) else advance();
            },
            .kw_interface => parseInterfaceDecl(flags),
            .kw_type => parseTypeAlias(flags),
            .kw_import => parseImportDecl(),
            else => advance(),
        }
    }
    return node_count;
}

export fn getNodeCount() u32 {
    return node_count;
}

export fn getNodeKind(idx: u32) u8 {
    if (idx >= node_count) return 0;
    return @intFromEnum(nodes[idx].kind);
}

export fn getNodeNameStart(idx: u32) u32 {
    if (idx >= node_count) return 0;
    return nodes[idx].name_start;
}

export fn getNodeNameLen(idx: u32) u16 {
    if (idx >= node_count) return 0;
    return nodes[idx].name_len;
}

export fn getNodeTypeKind(idx: u32) u8 {
    if (idx >= node_count) return 0;
    return nodes[idx].type_ref.kind;
}

export fn getNodeInitTypeKind(idx: u32) u8 {
    if (idx >= node_count) return 0;
    return nodes[idx].init_type.kind;
}

export fn getNodeFlags(idx: u32) u8 {
    if (idx >= node_count) return 0;
    return nodes[idx].flags;
}

export fn getNodeParent(idx: u32) u16 {
    if (idx >= node_count) return 0;
    return nodes[idx].parent;
}
