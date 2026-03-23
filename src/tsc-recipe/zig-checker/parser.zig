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
    const result = parseTypeExpr();
    if (result.kind > 0) return result;
    // Complex type we can't parse — skip to next delimiter but mark as "has type"
    var depth: u32 = 0;
    while (peek() != .eof) {
        // Stop at: , ) ; { = (at top level)
        if (depth == 0) {
            if (peek() == .comma or peek() == .close_paren or peek() == .semicolon or peek() == .open_brace or peek() == .equals) break;
        }
        if (peek() == .open_paren or peek() == .open_bracket or peek() == .less_than) depth += 1;
        if (peek() == .close_paren or peek() == .close_bracket or peek() == .greater_than) {
            if (depth > 0) depth -= 1 else break;
        }
        advance();
    }
    return TypeRef{ .kind = 11, .start = start, .len = @intCast(if (tokenStart() > start) tokenStart() - start else 1) };
}

/// Parse a type expression. Handles:
/// - Built-in types: number, string, boolean, any, unknown, never, void, object
/// - Identifiers: Foo, tls.ConnectionOptions
/// - Generic types: Promise<T>, Map<K, V>
/// - Array types: T[], Array<T>
/// - Union types: A | B | C
/// - Function types: (a: T) => R
/// - Parenthesized: (Type)
/// - Literal types: null, undefined
fn parseTypeExpr() TypeRef {
    const start = tokenStart();
    const start_pos = tpos; // track position to detect non-advancing

    // Parse primary type
    var kind: u8 = 0;
    switch (peek()) {
        .kw_number => { kind = 1; advance(); },
        .kw_string => { kind = 2; advance(); },
        .kw_boolean => { kind = 3; advance(); },
        .kw_any => { kind = 4; advance(); },
        .kw_unknown => { kind = 5; advance(); },
        .kw_never => { kind = 6; advance(); },
        .kw_void => { kind = 7; advance(); },
        .kw_object => { kind = 8; advance(); },
        .kw_null => { kind = 9; advance(); },
        .kw_undefined => { kind = 10; advance(); },
        .identifier => { kind = 11; advance(); },
        .kw_typeof => {
            kind = 11;
            advance(); // typeof
            if (peek() == .identifier) advance(); // name
            while (peek() == .dot and peekAt(1) == .identifier) { advance(); advance(); }
        },
        .open_paren => {
            kind = 11;
            skipBalanced(.open_paren, .close_paren);
            if (peek() == .arrow) { advance(); _ = parseTypeExpr(); }
        },
        .open_brace => { kind = 11; skipBalanced(.open_brace, .close_brace); },
        .open_bracket => { kind = 11; skipBalanced(.open_bracket, .close_bracket); },
        .kw_new => {
            kind = 11;
            advance();
            if (peek() == .open_paren) skipBalanced(.open_paren, .close_paren);
            if (peek() == .arrow) { advance(); _ = parseTypeExpr(); }
        },
        .string_literal => { kind = 21; advance(); }, // string literal type
        .number_literal => { kind = 20; advance(); }, // number literal type
        .kw_true => { kind = 22; advance(); },
        .kw_false => { kind = 23; advance(); },
        else => {},
    }

    if (kind == 0) return NO_TYPE;

    // Handle dotted types: a.b.c
    while (peek() == .dot and peekAt(1) == .identifier) { advance(); advance(); }

    // Handle generic types: Type<T, U>
    if (peek() == .less_than) skipBalanced(.less_than, .greater_than);

    // Handle array suffix: Type[] or Type[][]
    while (peek() == .open_bracket and peekAt(1) == .close_bracket) { advance(); advance(); kind = 13; }

    // Handle union: Type | Type
    if (peek() == .pipe) {
        while (peek() == .pipe) {
            advance(); // |
            const inner = parseTypeExpr();
            if (inner.kind == 0) break; // prevent infinite loop
        }
        kind = 12;
    }

    // Handle intersection: Type & Type
    if (peek() == .ampersand) {
        while (peek() == .ampersand) {
            advance(); // &
            const inner = parseTypeExpr();
            if (inner.kind == 0) break;
        }
    }

    // Safety: if we didn't advance at all, force advance to prevent infinite loop
    if (tpos == start_pos) advance();

    const end_pos = tokenStart();
    return TypeRef{ .kind = kind, .start = start, .len = @intCast(if (end_pos > start) end_pos - start else 1) };
}

/// Skip balanced pairs: (), {}, [], <>
fn skipBalanced(open: TK, close: TK) void {
    if (peek() != open) return;
    var depth: u32 = 1;
    advance();
    while (depth > 0 and peek() != .eof) {
        if (peek() == open) depth += 1;
        if (peek() == close) depth -= 1;
        advance();
    }
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

    // Handle destructuring: const { a, b } = ... or const [a, b] = ...
    if (peek() == .open_brace or peek() == .open_bracket) {
        if (peek() == .open_brace) skipBalanced(.open_brace, .close_brace)
        else skipBalanced(.open_bracket, .close_bracket);
        // Skip type annotation and initializer
        _ = parseTypeAnnotation();
        if (peek() == .equals) { advance(); while (peek() != .semicolon and peek() != .eof) advance(); }
        return;
    }

    if (peek() != .identifier) return;
    const name_start = tokenStart();
    const name_len = tokenLen();
    advance(); // name

    const type_ref = parseTypeAnnotation();
    var init_type = NO_TYPE;

    // Check for = initializer
    if (peek() == .equals) {
        advance(); // =

        // Arrow function: const f = (params) => ... or const f = async (params) => ...
        if (peek() == .kw_async and (peekAt(1) == .open_paren or peekAt(1) == .identifier)) {
            advance(); // async
        }
        if (peek() == .open_paren) {
            // Could be arrow function: (params) => body
            // Save position to backtrack if not arrow
            const saved = tpos;
            parseParamList(name_start, name_len, flags);
            if (peek() == .arrow) {
                advance(); // =>
                return; // arrow function with params parsed
            }
            // Not arrow — restore and treat as expression
            tpos = saved;
        } else if (peek() == .identifier and peekAt(1) == .arrow) {
            // Single-param arrow: const f = x => ...
            const pstart = tokenStart();
            const plen = tokenLen();
            advance(); // param name
            advance(); // =>
            const fidx = addNode(.func_decl, name_start, name_len, NO_TYPE, NO_TYPE, 0, flags);
            _ = addNode(.param_decl, pstart, plen, NO_TYPE, NO_TYPE, @intCast(fidx), 0);
            return;
        } else if (peek() == .kw_function) {
            // Function expression: const f = function(params) { ... }
            parseFuncDecl(flags);
            return;
        }

        init_type = inferLiteralType();
    }

    _ = addNode(.var_decl, name_start, name_len, type_ref, init_type, 0, flags);
}

/// Parse a function-like parameter list starting at '('.
/// Works for: function decl, arrow function, method, callback.
fn parseParamList(parent_name_start: u32, parent_name_len: u16, flags: u8) void {
    if (!expect(.open_paren)) return;
    const func_idx = addNode(.func_decl, parent_name_start, parent_name_len, NO_TYPE, NO_TYPE, 0, flags);

    // Parse parameters
    while (peek() != .close_paren and peek() != .eof) {
        const param_prev = tpos;
        // Skip modifiers: readonly, public, private, protected
        if (peek() == .kw_readonly) advance();

        if (peek() == .identifier) {
            const pname_start = tokenStart();
            const pname_len = tokenLen();
            advance(); // param name
            var pflags: u8 = 0;
            if (peek() == .question) {
                pflags |= 1; // optional
                advance();
            }
            var ptype = parseTypeAnnotation();

            // Default value: param = value → infers type from default
            var init = NO_TYPE;
            if (peek() == .equals) {
                advance(); // =
                init = inferLiteralType();
                // Default value provides implicit type
                if (ptype.kind == 0 and init.kind > 0) {
                    // Infer type from default: number literal → number, etc.
                    ptype.kind = switch (init.kind) {
                        20 => 1, // numLit → number
                        21 => 2, // strLit → string
                        22, 23 => 3, // true/false → boolean
                        else => 0,
                    };
                    pflags |= 8; // has inferred type from default
                }
                // Skip rest of default expression
                while (peek() != .comma and peek() != .close_paren and peek() != .eof) advance();
            }

            _ = addNode(.param_decl, pname_start, pname_len, ptype, init, @intCast(func_idx), pflags);
        } else if (peek() == .dot and peekAt(1) == .dot and peekAt(2) == .dot) {
            // Rest parameter: ...args
            advance(); advance(); advance(); // ...
            if (peek() == .identifier) {
                const pname_start = tokenStart();
                const pname_len = tokenLen();
                advance();
                const ptype = parseTypeAnnotation();
                _ = addNode(.param_decl, pname_start, pname_len, ptype, NO_TYPE, @intCast(func_idx), 0);
            }
        }
        if (peek() == .comma) advance();
        // Skip destructuring patterns { a, b } or [ a, b ]
        if (peek() == .open_brace) skipBalanced(.open_brace, .close_brace);
        if (peek() == .open_bracket) skipBalanced(.open_bracket, .close_bracket);
        // Safety: must advance to prevent infinite loop
        if (tpos == param_prev) advance();
    }
    if (expect(.close_paren)) {
        // Return type
        const ret_type = parseTypeAnnotation();
        if (ret_type.kind > 0) {
            nodes[func_idx].type_ref = ret_type;
        }
    }
}

fn parseFuncDecl(flags: u8) void {
    advance(); // function
    if (peek() != .identifier) {
        // Anonymous function — still parse params
        if (peek() == .open_paren) parseParamList(tokenStart(), 0, flags);
        return;
    }
    const name_start = tokenStart();
    const name_len = tokenLen();
    advance(); // name
    // Skip generic params <T>
    if (peek() == .less_than) skipBalanced(.less_than, .greater_than);
    parseParamList(name_start, name_len, flags);
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

// ── Public API (for use by checker.zig) ──

pub fn nodeCount() u32 { return node_count; }
pub fn nodeKind(idx: u32) u8 { if (idx >= node_count) return 0; return @intFromEnum(nodes[idx].kind); }
pub fn nodeNameStart(idx: u32) u32 { if (idx >= node_count) return 0; return nodes[idx].name_start; }
pub fn nodeNameLen(idx: u32) u16 { if (idx >= node_count) return 0; return nodes[idx].name_len; }
pub fn nodeTypeKind(idx: u32) u8 { if (idx >= node_count) return 0; return nodes[idx].type_ref.kind; }
pub fn nodeInitTypeKind(idx: u32) u8 { if (idx >= node_count) return 0; return nodes[idx].init_type.kind; }
pub fn nodeFlags(idx: u32) u8 { if (idx >= node_count) return 0; return nodes[idx].flags; }
pub fn nodeParent(idx: u32) u16 { if (idx >= node_count) return 0; return nodes[idx].parent; }

pub fn doParse(src_ptr: [*]const u8, src_len: u32) u32 {
    _ = tokenizer.doTokenize(src_ptr, src_len);
    tc = tokenizer.getWasmTokenCount();
    tpos = 0;
    node_count = 0;
    while (tpos < tc and peek() != .eof) {
        const prev = tpos;
        var flags: u8 = 0;
        if (peek() == .kw_export) {
            flags |= 4;
            advance();
            if (peek() == .kw_function or peek() == .kw_interface or peek() == .kw_type or
                peek() == .kw_const or peek() == .kw_let or peek() == .kw_var) {} else { advance(); continue; }
        }
        switch (peek()) {
            .kw_const, .kw_let, .kw_var => parseVarDecl(flags),
            .kw_function => parseFuncDecl(flags),
            .kw_async => {
                advance(); // async
                if (peek() == .kw_function) parseFuncDecl(flags) else advance();
            },
            .kw_interface => parseInterfaceDecl(flags),
            .kw_type => parseTypeAlias(flags),
            .kw_import => parseImportDecl(),
            .kw_class => {
                advance(); // class
                if (peek() == .identifier) advance(); // name
                // Skip extends/implements
                while (peek() == .kw_extends or peek() == .kw_implements) {
                    advance();
                    while (peek() == .identifier or peek() == .dot or peek() == .comma) advance();
                }
                if (peek() == .less_than) skipBalanced(.less_than, .greater_than);
                if (peek() != .open_brace) continue;
                advance(); // {
                // Parse class body: methods + properties
                while (peek() != .close_brace and peek() != .eof) {
                    const class_prev = tpos;
                    // Skip modifiers: public, private, protected, static, abstract, override, readonly, async
                    while (peek() == .kw_readonly or peek() == .kw_async or peek() == .identifier) {
                        // Check if this identifier is followed by ( or : → it's the member name
                        if (peek() == .identifier and (peekAt(1) == .open_paren or peekAt(1) == .colon or peekAt(1) == .question or peekAt(1) == .less_than)) break;
                        advance(); // skip modifier
                    }
                    if (peek() == .identifier and (peekAt(1) == .open_paren or (peekAt(1) == .less_than))) {
                        // Method: name(...) or name<T>(...)
                        const mstart = tokenStart();
                        const mlen = tokenLen();
                        advance();
                        if (peek() == .less_than) skipBalanced(.less_than, .greater_than);
                        if (peek() == .open_paren) parseParamList(mstart, mlen, 0);
                        // Skip method body
                        if (peek() == .open_brace) skipBalanced(.open_brace, .close_brace);
                    } else if (peek() == .identifier and (peekAt(1) == .colon or peekAt(1) == .question or peekAt(1) == .equals or peekAt(1) == .semicolon)) {
                        // Property: name: Type or name = value
                        advance(); // skip property (handled like var if needed)
                    }
                    if (tpos == class_prev) advance(); // safety
                }
                if (peek() == .close_brace) advance();
            },
            .identifier => advance(), // skip — method detection is inside class parser
            else => advance(),
        }
        if (tpos == prev) advance(); // safety guard
    }
    return node_count;
}

// ── WASM Exports ──

export fn parse(src_ptr: [*]const u8, src_len: u32) u32 {
    // Tokenize first
    _ = tokenizer.doTokenize(src_ptr, src_len);
    tc = tokenizer.getWasmTokenCount();
    tpos = 0;
    node_count = 0;

    while (tpos < tc and peek() != .eof) {
        const prev_pos = tpos; // guard: must advance
        var flags: u8 = 0;
        if (peek() == .kw_export) {
            flags |= 4;
            advance();
            if (peek() == .kw_function or peek() == .kw_interface or peek() == .kw_type or
                peek() == .kw_const or peek() == .kw_let or peek() == .kw_var) {
                // continue to parse the declaration
            } else {
                advance();
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
        // Safety: if no progress, force advance to prevent infinite loop
        if (tpos == prev_pos) advance();
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
