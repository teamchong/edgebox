// Zig TypeScript Tokenizer
// Scans UTF-8 bytes into tokens. Compiled to WASM for V8 TurboFan inlining.
// Handles: keywords, identifiers, literals, operators, punctuation.
// Does NOT handle: JSX, template literals (delegated to TSC for those).

pub const TokenKind = enum(u8) {
    // Literals
    number_literal = 1,
    string_literal = 2,
    identifier = 3,
    // Keywords
    kw_const = 10,
    kw_let = 11,
    kw_var = 12,
    kw_function = 13,
    kw_return = 14,
    kw_if = 15,
    kw_else = 16,
    kw_for = 17,
    kw_while = 18,
    kw_import = 19,
    kw_export = 20,
    kw_from = 21,
    kw_type = 22,
    kw_interface = 23,
    kw_class = 24,
    kw_extends = 25,
    kw_implements = 26,
    kw_new = 27,
    kw_this = 28,
    kw_true = 29,
    kw_false = 30,
    kw_null = 31,
    kw_undefined = 32,
    kw_void = 33,
    kw_typeof = 34,
    kw_as = 35,
    kw_readonly = 36,
    kw_async = 37,
    kw_await = 38,
    // Type keywords
    kw_number = 40,
    kw_string = 41,
    kw_boolean = 42,
    kw_any = 43,
    kw_unknown = 44,
    kw_never = 45,
    kw_object = 46,
    // Punctuation
    open_paren = 50,
    close_paren = 51,
    open_brace = 52,
    close_brace = 53,
    open_bracket = 54,
    close_bracket = 55,
    semicolon = 56,
    comma = 57,
    dot = 58,
    colon = 59,
    question = 60,
    exclamation = 61,
    // Operators
    equals = 70,
    arrow = 71, // =>
    pipe = 72, // |
    ampersand = 73, // &
    less_than = 74,
    greater_than = 75,
    plus = 76,
    minus = 77,
    star = 78,
    slash = 79,
    // Special
    eof = 0,
    unknown = 255,
};

pub const Token = struct {
    kind: TokenKind,
    start: u32,
    len: u16,
};

// Token buffer — flat array for WASM export
const MAX_TOKENS = 65536;
var tokens: [MAX_TOKENS]Token = undefined;
var token_count: u32 = 0;

// Source text pointer (set by caller)
var src: []const u8 = "";
var pos: u32 = 0;

fn isAlpha(c: u8) bool {
    return (c >= 'a' and c <= 'z') or (c >= 'A' and c <= 'Z') or c == '_' or c == '$';
}

fn isDigit(c: u8) bool {
    return c >= '0' and c <= '9';
}

fn isAlphaNum(c: u8) bool {
    return isAlpha(c) or isDigit(c);
}

fn matchKeyword(word: []const u8) TokenKind {
    // Most common keywords first
    return switch (word.len) {
        2 => if (eql(word, "if")) .kw_if
            else if (eql(word, "as")) .kw_as
            else .identifier,
        3 => if (eql(word, "let")) .kw_let
            else if (eql(word, "var")) .kw_var
            else if (eql(word, "for")) .kw_for
            else if (eql(word, "new")) .kw_new
            else if (eql(word, "any")) .kw_any
            else .identifier,
        4 => if (eql(word, "else")) .kw_else
            else if (eql(word, "from")) .kw_from
            else if (eql(word, "type")) .kw_type
            else if (eql(word, "this")) .kw_this
            else if (eql(word, "true")) .kw_true
            else if (eql(word, "null")) .kw_null
            else if (eql(word, "void")) .kw_void
            else .identifier,
        5 => if (eql(word, "const")) .kw_const
            else if (eql(word, "while")) .kw_while
            else if (eql(word, "class")) .kw_class
            else if (eql(word, "false")) .kw_false
            else if (eql(word, "async")) .kw_async
            else if (eql(word, "await")) .kw_await
            else if (eql(word, "never")) .kw_never
            else .identifier,
        6 => if (eql(word, "import")) .kw_import
            else if (eql(word, "export")) .kw_export
            else if (eql(word, "return")) .kw_return
            else if (eql(word, "typeof")) .kw_typeof
            else if (eql(word, "string")) .kw_string
            else if (eql(word, "number")) .kw_number
            else if (eql(word, "object")) .kw_object
            else .identifier,
        7 => if (eql(word, "boolean")) .kw_boolean
            else if (eql(word, "unknown")) .kw_unknown
            else if (eql(word, "extends")) .kw_extends
            else .identifier,
        8 => if (eql(word, "function")) .kw_function
            else if (eql(word, "readonly")) .kw_readonly
            else .identifier,
        9 => if (eql(word, "interface")) .kw_interface
            else if (eql(word, "undefined")) .kw_undefined
            else .identifier,
        10 => if (eql(word, "implements")) .kw_implements else .identifier,
        else => .identifier,
    };
}

fn eql(a: []const u8, b: []const u8) bool {
    if (a.len != b.len) return false;
    for (a, b) |ca, cb| {
        if (ca != cb) return false;
    }
    return true;
}

fn addToken(kind: TokenKind, start: u32, len: u16) void {
    if (token_count < MAX_TOKENS) {
        tokens[token_count] = .{ .kind = kind, .start = start, .len = len };
        token_count += 1;
    }
}

fn skipWhitespace() void {
    while (pos < src.len) {
        const c = src[pos];
        if (c == ' ' or c == '\t' or c == '\n' or c == '\r') {
            pos += 1;
        } else if (c == '/' and pos + 1 < src.len) {
            if (src[pos + 1] == '/') {
                // Line comment
                pos += 2;
                while (pos < src.len and src[pos] != '\n') : (pos += 1) {}
            } else if (src[pos + 1] == '*') {
                // Block comment
                pos += 2;
                while (pos + 1 < src.len) : (pos += 1) {
                    if (src[pos] == '*' and src[pos + 1] == '/') { pos += 2; break; }
                }
            } else break;
        } else break;
    }
}

fn scanToken() void {
    skipWhitespace();
    if (pos >= src.len) {
        addToken(.eof, pos, 0);
        return;
    }

    const start = pos;
    const c = src[pos];

    // Identifier or keyword
    if (isAlpha(c)) {
        pos += 1;
        while (pos < src.len and isAlphaNum(src[pos])) : (pos += 1) {}
        const word = src[start..pos];
        addToken(matchKeyword(word), start, @intCast(pos - start));
        return;
    }

    // Number
    if (isDigit(c)) {
        pos += 1;
        while (pos < src.len and (isDigit(src[pos]) or src[pos] == '.')) : (pos += 1) {}
        addToken(.number_literal, start, @intCast(pos - start));
        return;
    }

    // String
    if (c == '\'' or c == '"') {
        pos += 1;
        while (pos < src.len and src[pos] != c) {
            if (src[pos] == '\\') pos += 1; // skip escape
            pos += 1;
        }
        if (pos < src.len) pos += 1; // closing quote
        addToken(.string_literal, start, @intCast(pos - start));
        return;
    }

    // Punctuation
    pos += 1;
    switch (c) {
        '(' => addToken(.open_paren, start, 1),
        ')' => addToken(.close_paren, start, 1),
        '{' => addToken(.open_brace, start, 1),
        '}' => addToken(.close_brace, start, 1),
        '[' => addToken(.open_bracket, start, 1),
        ']' => addToken(.close_bracket, start, 1),
        ';' => addToken(.semicolon, start, 1),
        ',' => addToken(.comma, start, 1),
        '.' => addToken(.dot, start, 1),
        ':' => addToken(.colon, start, 1),
        '?' => addToken(.question, start, 1),
        '!' => addToken(.exclamation, start, 1),
        '|' => addToken(.pipe, start, 1),
        '&' => addToken(.ampersand, start, 1),
        '<' => addToken(.less_than, start, 1),
        '>' => addToken(.greater_than, start, 1),
        '+' => addToken(.plus, start, 1),
        '-' => addToken(.minus, start, 1),
        '*' => addToken(.star, start, 1),
        '/' => addToken(.slash, start, 1),
        '=' => {
            if (pos < src.len and src[pos] == '>') {
                pos += 1;
                addToken(.arrow, start, 2);
            } else {
                addToken(.equals, start, 1);
            }
        },
        else => addToken(.unknown, start, 1),
    }
}

// ── Public API (for use by other Zig modules) ──

pub fn getWasmTokenKind(idx: u32) u8 {
    if (idx >= token_count) return 0;
    return @intFromEnum(tokens[idx].kind);
}

pub fn getWasmTokenStart(idx: u32) u32 {
    if (idx >= token_count) return 0;
    return tokens[idx].start;
}

pub fn getWasmTokenLen(idx: u32) u16 {
    if (idx >= token_count) return 0;
    return tokens[idx].len;
}

pub fn getWasmTokenCount() u32 {
    return token_count;
}

pub fn doTokenize(src_ptr: [*]const u8, src_len: u32) u32 {
    src = src_ptr[0..src_len];
    pos = 0;
    token_count = 0;
    while (pos < src.len and token_count < MAX_TOKENS - 1) {
        scanToken();
        if (tokens[token_count - 1].kind == .eof) break;
    }
    return token_count;
}

// ── WASM Exports ──

export fn tokenize(src_ptr: [*]const u8, src_len: u32) u32 {
    src = src_ptr[0..src_len];
    pos = 0;
    token_count = 0;

    while (pos < src.len and token_count < MAX_TOKENS - 1) {
        scanToken();
        if (tokens[token_count - 1].kind == .eof) break;
    }
    return token_count;
}

export fn getTokenKind(idx: u32) u8 {
    if (idx >= token_count) return 0;
    return @intFromEnum(tokens[idx].kind);
}

export fn getTokenStart(idx: u32) u32 {
    if (idx >= token_count) return 0;
    return tokens[idx].start;
}

export fn getTokenLen(idx: u32) u16 {
    if (idx >= token_count) return 0;
    return tokens[idx].len;
}

export fn getTokenCount() u32 {
    return token_count;
}
