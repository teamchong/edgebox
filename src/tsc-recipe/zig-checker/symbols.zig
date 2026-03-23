// Zig Symbol Table for TypeScript
// Maps declared names to their types within a scope.
// Supports: block scoping (const/let), function scoping (var),
//           interface declarations, type aliases, imports.
// Used by the checker to resolve names and verify types.

const parser = @import("parser.zig");

pub const SymbolKind = enum(u8) {
    variable = 1, // const, let, var
    function = 2,
    parameter = 3,
    interface = 4,
    type_alias = 5,
    import = 6,
    interface_property = 7,
};

pub const Symbol = struct {
    name_start: u32,
    name_len: u16,
    kind: SymbolKind,
    type_kind: u8, // TypeRef.kind from parser
    flags: u8, // 1=optional, 2=readonly, 4=exported, 8=has_type_annotation
    scope: u16, // scope index
    node_idx: u32, // index into parser's node array
};

// Scopes for block-level tracking
pub const Scope = struct {
    parent: u16, // parent scope index (0 = global)
    start_symbol: u32, // first symbol in this scope
    end_symbol: u32, // one past last symbol
};

const MAX_SYMBOLS = 32768;
const MAX_SCOPES = 1024;

var symbols: [MAX_SYMBOLS]Symbol = undefined;
var symbol_count: u32 = 0;
var scopes: [MAX_SCOPES]Scope = undefined;
var scope_count: u16 = 0;
var current_scope: u16 = 0;

// Source text for name comparison
var src_text: []const u8 = "";

fn pushScope() u16 {
    if (scope_count >= MAX_SCOPES) return current_scope;
    const idx = scope_count;
    scopes[idx] = .{
        .parent = current_scope,
        .start_symbol = symbol_count,
        .end_symbol = symbol_count,
    };
    scope_count += 1;
    current_scope = idx;
    return idx;
}

fn popScope() void {
    scopes[current_scope].end_symbol = symbol_count;
    current_scope = scopes[current_scope].parent;
}

fn addSymbol(name_start: u32, name_len: u16, kind: SymbolKind, type_kind: u8, flags: u8, node_idx: u32) u32 {
    if (symbol_count >= MAX_SYMBOLS) return symbol_count;
    const idx = symbol_count;
    symbols[idx] = .{
        .name_start = name_start,
        .name_len = name_len,
        .kind = kind,
        .type_kind = type_kind,
        .flags = flags,
        .scope = current_scope,
        .node_idx = node_idx,
    };
    symbol_count += 1;
    return idx;
}

/// Look up a name in current scope chain.
/// Returns symbol index or MAX_SYMBOLS if not found.
pub fn lookup(name_start: u32, name_len: u16) u32 {
    const name = src_text[name_start .. name_start + name_len];
    var scope = current_scope;
    while (true) {
        // Search this scope's symbols
        var i = scopes[scope].start_symbol;
        while (i < symbol_count and i < scopes[scope].end_symbol) : (i += 1) {
            const sym = symbols[i];
            if (sym.name_len == name_len) {
                const sym_name = src_text[sym.name_start .. sym.name_start + sym.name_len];
                if (namesEqual(name, sym_name)) return i;
            }
        }
        // Move to parent scope
        if (scope == 0) break;
        scope = scopes[scope].parent;
    }
    return MAX_SYMBOLS; // not found
}

fn namesEqual(a: []const u8, b: []const u8) bool {
    if (a.len != b.len) return false;
    for (a, b) |ca, cb| {
        if (ca != cb) return false;
    }
    return true;
}

/// Build symbol table from parser output.
/// Call parser.doParse() first.
pub fn build(src: []const u8) void {
    src_text = src;
    symbol_count = 0;
    scope_count = 0;
    current_scope = 0;

    // Create global scope
    _ = pushScope();

    const nc = parser.nodeCount();
    var i: u32 = 0;
    while (i < nc) : (i += 1) {
        const kind = parser.nodeKind(i);
        const name_start = parser.nodeNameStart(i);
        const name_len = parser.nodeNameLen(i);
        const type_kind = parser.nodeTypeKind(i);
        const pflags = parser.nodeFlags(i);

        var flags: u8 = pflags;
        if (type_kind > 0) flags |= 8; // has_type_annotation

        switch (kind) {
            1 => { // var_decl
                _ = addSymbol(name_start, name_len, .variable, type_kind, flags, i);
            },
            2 => { // func_decl
                _ = addSymbol(name_start, name_len, .function, type_kind, flags, i);
            },
            3 => { // param_decl
                _ = addSymbol(name_start, name_len, .parameter, type_kind, flags, i);
            },
            4 => { // interface_decl
                _ = addSymbol(name_start, name_len, .interface, 0, flags, i);
            },
            5 => { // interface_prop
                _ = addSymbol(name_start, name_len, .interface_property, type_kind, flags, i);
            },
            6 => { // type_alias
                _ = addSymbol(name_start, name_len, .type_alias, type_kind, flags, i);
            },
            7 => { // import_decl
                _ = addSymbol(name_start, name_len, .import, 0, flags, i);
            },
            else => {},
        }
    }
    scopes[0].end_symbol = symbol_count;
}

// ── Queries ──

pub fn symbolCount() u32 {
    return symbol_count;
}

pub fn getSymbol(idx: u32) ?Symbol {
    if (idx >= symbol_count) return null;
    return symbols[idx];
}

/// Count parameters without type annotations (for TS7006)
pub fn countUntypedParams() u32 {
    var count: u32 = 0;
    var i: u32 = 0;
    while (i < symbol_count) : (i += 1) {
        if (symbols[i].kind == .parameter and symbols[i].flags & 8 == 0) {
            count += 1;
        }
    }
    return count;
}

// ── WASM Exports ──

export fn buildSymbols(src_ptr: [*]const u8, src_len: u32) u32 {
    build(src_ptr[0..src_len]);
    return symbol_count;
}

export fn getSymbolCount() u32 {
    return symbol_count;
}

export fn getSymbolKind(idx: u32) u8 {
    if (idx >= symbol_count) return 0;
    return @intFromEnum(symbols[idx].kind);
}

export fn getSymbolNameStart(idx: u32) u32 {
    if (idx >= symbol_count) return 0;
    return symbols[idx].name_start;
}

export fn getSymbolNameLen(idx: u32) u16 {
    if (idx >= symbol_count) return 0;
    return symbols[idx].name_len;
}

export fn getSymbolTypeKind(idx: u32) u8 {
    if (idx >= symbol_count) return 0;
    return symbols[idx].type_kind;
}

export fn getSymbolFlags(idx: u32) u8 {
    if (idx >= symbol_count) return 0;
    return symbols[idx].flags;
}

export fn getUntypedParamCount() u32 {
    return countUntypedParams();
}
