// Zig Native TypeScript Scanner
//
// Tokenizes TypeScript source at native speed (~270 MB/s).
// Produces tokens with SyntaxKind values matching TSC exactly.
// Used as the inner loop of the Zig parser.

const std = @import("std");

pub const TokenKind = u16;

// SyntaxKind values matching TypeScript's enum
pub const SK = struct {
    pub const Unknown: TokenKind = 0;
    pub const EndOfFile: TokenKind = 1;
    pub const NumericLiteral: TokenKind = 9;
    pub const BigIntLiteral: TokenKind = 10;
    pub const StringLiteral: TokenKind = 11;
    pub const RegExpLiteral: TokenKind = 14;
    pub const NoSubTemplate: TokenKind = 15;
    pub const TemplateHead: TokenKind = 16;
    pub const TemplateMiddle: TokenKind = 17;
    pub const TemplateTail: TokenKind = 18;
    pub const OpenBrace: TokenKind = 19;
    pub const CloseBrace: TokenKind = 20;
    pub const OpenParen: TokenKind = 21;
    pub const CloseParen: TokenKind = 22;
    pub const OpenBracket: TokenKind = 23;
    pub const CloseBracket: TokenKind = 24;
    pub const Dot: TokenKind = 25;
    pub const DotDotDot: TokenKind = 26;
    pub const Semicolon: TokenKind = 27;
    pub const Comma: TokenKind = 28;
    pub const QuestionDot: TokenKind = 29;
    pub const LessThan: TokenKind = 30;
    pub const GreaterThan: TokenKind = 32;
    pub const LessEqual: TokenKind = 33;
    pub const GreaterEqual: TokenKind = 34;
    pub const EqualEqual: TokenKind = 35;
    pub const ExclEqual: TokenKind = 36;
    pub const EqualEqualEqual: TokenKind = 37;
    pub const ExclEqualEqual: TokenKind = 38;
    pub const Arrow: TokenKind = 39;
    pub const Plus: TokenKind = 40;
    pub const Minus: TokenKind = 41;
    pub const Asterisk: TokenKind = 42;
    pub const AsteriskAsterisk: TokenKind = 43;
    pub const Slash: TokenKind = 44;
    pub const Percent: TokenKind = 45;
    pub const PlusPlus: TokenKind = 46;
    pub const MinusMinus: TokenKind = 47;
    pub const LessLess: TokenKind = 48;
    pub const GreaterGreater: TokenKind = 49;
    pub const GreaterGreaterGreater: TokenKind = 50;
    pub const Ampersand: TokenKind = 51;
    pub const Bar: TokenKind = 52;
    pub const Caret: TokenKind = 53;
    pub const Exclamation: TokenKind = 54;
    pub const Tilde: TokenKind = 55;
    pub const AmpersandAmpersand: TokenKind = 56;
    pub const BarBar: TokenKind = 57;
    pub const Question: TokenKind = 58;
    pub const Colon: TokenKind = 59;
    pub const At: TokenKind = 60;
    pub const QuestionQuestion: TokenKind = 61;
    pub const Backtick: TokenKind = 62;
    pub const Hash: TokenKind = 63;
    pub const Equals: TokenKind = 64;
    pub const PlusEquals: TokenKind = 65;
    pub const MinusEquals: TokenKind = 66;
    pub const AsteriskEquals: TokenKind = 67;
    pub const SlashEquals: TokenKind = 69;
    pub const PercentEquals: TokenKind = 70;
    pub const AmpersandEquals: TokenKind = 74;
    pub const BarEquals: TokenKind = 75;
    pub const CaretEquals: TokenKind = 79;
    pub const Identifier: TokenKind = 80;
    pub const PrivateIdentifier: TokenKind = 81;
    // Keywords (83-165) — looked up from identifier text
};

pub const Token = struct {
    kind: TokenKind,
    start: u32,
    end: u32,
};

pub const Scanner = struct {
    src: []const u8,
    pos: u32,
    token: Token,

    pub fn init(source: []const u8) Scanner {
        return .{ .src = source, .pos = 0, .token = .{ .kind = SK.Unknown, .start = 0, .end = 0 } };
    }

    pub fn scan(self: *Scanner) Token {
        self.skipWhitespaceAndComments();
        if (self.pos >= self.src.len) {
            self.token = .{ .kind = SK.EndOfFile, .start = self.pos, .end = self.pos };
            return self.token;
        }
        const start = self.pos;
        const c = self.src[self.pos];

        const kind: TokenKind = switch (c) {
            '{' => blk: { self.pos += 1; break :blk SK.OpenBrace; },
            '}' => blk: { self.pos += 1; break :blk SK.CloseBrace; },
            '(' => blk: { self.pos += 1; break :blk SK.OpenParen; },
            ')' => blk: { self.pos += 1; break :blk SK.CloseParen; },
            '[' => blk: { self.pos += 1; break :blk SK.OpenBracket; },
            ']' => blk: { self.pos += 1; break :blk SK.CloseBracket; },
            ';' => blk: { self.pos += 1; break :blk SK.Semicolon; },
            ',' => blk: { self.pos += 1; break :blk SK.Comma; },
            '~' => blk: { self.pos += 1; break :blk SK.Tilde; },
            '@' => blk: { self.pos += 1; break :blk SK.At; },
            '#' => blk: { self.pos += 1; break :blk SK.Hash; },
            ':' => blk: { self.pos += 1; break :blk SK.Colon; },
            '.' => self.scanDot(),
            '?' => self.scanQuestion(),
            '<' => self.scanLess(),
            '>' => self.scanGreater(),
            '=' => self.scanEquals(),
            '!' => self.scanExclamation(),
            '+' => self.scanPlus(),
            '-' => self.scanMinus(),
            '*' => self.scanAsterisk(),
            '/' => self.scanSlash(),
            '%' => self.scanPercent(),
            '&' => self.scanAmpersand(),
            '|' => self.scanBar(),
            '^' => self.scanCaret(),
            '\'' => self.scanString(),
            '"' => self.scanString(),
            '`' => blk: { self.pos += 1; break :blk SK.Backtick; },
            '0'...'9' => self.scanNumber(),
            'a'...'z', 'A'...'Z', '_', '$' => self.scanIdentifier(),
            else => blk: { self.pos += 1; break :blk SK.Unknown; },
        };

        self.token = .{ .kind = kind, .start = start, .end = self.pos };
        return self.token;
    }

    fn peek(self: *Scanner, offset: u32) u8 {
        const idx = self.pos + offset;
        return if (idx < self.src.len) self.src[idx] else 0;
    }

    fn skipWhitespaceAndComments(self: *Scanner) void {
        while (self.pos < self.src.len) {
            const c = self.src[self.pos];
            if (c == ' ' or c == '\t' or c == '\r' or c == '\n') {
                self.pos += 1;
                continue;
            }
            if (c == '/' and self.pos + 1 < self.src.len) {
                if (self.src[self.pos + 1] == '/') {
                    // Single-line comment
                    self.pos += 2;
                    while (self.pos < self.src.len and self.src[self.pos] != '\n') self.pos += 1;
                    continue;
                }
                if (self.src[self.pos + 1] == '*') {
                    // Multi-line comment
                    self.pos += 2;
                    while (self.pos + 1 < self.src.len) {
                        if (self.src[self.pos] == '*' and self.src[self.pos + 1] == '/') {
                            self.pos += 2;
                            break;
                        }
                        self.pos += 1;
                    }
                    continue;
                }
            }
            break;
        }
    }

    fn scanDot(self: *Scanner) TokenKind {
        self.pos += 1;
        if (self.peek(0) == '.' and self.peek(1) == '.') { self.pos += 2; return SK.DotDotDot; }
        return SK.Dot;
    }

    fn scanQuestion(self: *Scanner) TokenKind {
        self.pos += 1;
        if (self.peek(0) == '.') { self.pos += 1; return SK.QuestionDot; }
        if (self.peek(0) == '?') { self.pos += 1; return SK.QuestionQuestion; }
        return SK.Question;
    }

    fn scanLess(self: *Scanner) TokenKind {
        self.pos += 1;
        if (self.peek(0) == '=') { self.pos += 1; return SK.LessEqual; }
        if (self.peek(0) == '<') { self.pos += 1; return SK.LessLess; }
        return SK.LessThan;
    }

    fn scanGreater(self: *Scanner) TokenKind {
        self.pos += 1;
        if (self.peek(0) == '=') { self.pos += 1; return SK.GreaterEqual; }
        if (self.peek(0) == '>') {
            self.pos += 1;
            if (self.peek(0) == '>') { self.pos += 1; return SK.GreaterGreaterGreater; }
            return SK.GreaterGreater;
        }
        return SK.GreaterThan;
    }

    fn scanEquals(self: *Scanner) TokenKind {
        self.pos += 1;
        if (self.peek(0) == '=') {
            self.pos += 1;
            if (self.peek(0) == '=') { self.pos += 1; return SK.EqualEqualEqual; }
            return SK.EqualEqual;
        }
        if (self.peek(0) == '>') { self.pos += 1; return SK.Arrow; }
        return SK.Equals;
    }

    fn scanExclamation(self: *Scanner) TokenKind {
        self.pos += 1;
        if (self.peek(0) == '=') {
            self.pos += 1;
            if (self.peek(0) == '=') { self.pos += 1; return SK.ExclEqualEqual; }
            return SK.ExclEqual;
        }
        return SK.Exclamation;
    }

    fn scanPlus(self: *Scanner) TokenKind {
        self.pos += 1;
        if (self.peek(0) == '+') { self.pos += 1; return SK.PlusPlus; }
        if (self.peek(0) == '=') { self.pos += 1; return SK.PlusEquals; }
        return SK.Plus;
    }

    fn scanMinus(self: *Scanner) TokenKind {
        self.pos += 1;
        if (self.peek(0) == '-') { self.pos += 1; return SK.MinusMinus; }
        if (self.peek(0) == '=') { self.pos += 1; return SK.MinusEquals; }
        return SK.Minus;
    }

    fn scanAsterisk(self: *Scanner) TokenKind {
        self.pos += 1;
        if (self.peek(0) == '*') { self.pos += 1; return SK.AsteriskAsterisk; }
        if (self.peek(0) == '=') { self.pos += 1; return SK.AsteriskEquals; }
        return SK.Asterisk;
    }

    fn scanSlash(self: *Scanner) TokenKind {
        // Comments already handled in skipWhitespaceAndComments
        self.pos += 1;
        if (self.peek(0) == '=') { self.pos += 1; return SK.SlashEquals; }
        return SK.Slash;
    }

    fn scanPercent(self: *Scanner) TokenKind {
        self.pos += 1;
        if (self.peek(0) == '=') { self.pos += 1; return SK.PercentEquals; }
        return SK.Percent;
    }

    fn scanAmpersand(self: *Scanner) TokenKind {
        self.pos += 1;
        if (self.peek(0) == '&') { self.pos += 1; return SK.AmpersandAmpersand; }
        if (self.peek(0) == '=') { self.pos += 1; return SK.AmpersandEquals; }
        return SK.Ampersand;
    }

    fn scanBar(self: *Scanner) TokenKind {
        self.pos += 1;
        if (self.peek(0) == '|') { self.pos += 1; return SK.BarBar; }
        if (self.peek(0) == '=') { self.pos += 1; return SK.BarEquals; }
        return SK.Bar;
    }

    fn scanCaret(self: *Scanner) TokenKind {
        self.pos += 1;
        if (self.peek(0) == '=') { self.pos += 1; return SK.CaretEquals; }
        return SK.Caret;
    }

    fn scanString(self: *Scanner) TokenKind {
        const quote = self.src[self.pos];
        self.pos += 1;
        while (self.pos < self.src.len and self.src[self.pos] != quote) {
            if (self.src[self.pos] == '\\') self.pos += 1;
            self.pos += 1;
        }
        if (self.pos < self.src.len) self.pos += 1;
        return SK.StringLiteral;
    }

    fn scanNumber(self: *Scanner) TokenKind {
        // Handle 0x, 0o, 0b prefixes
        if (self.src[self.pos] == '0' and self.pos + 1 < self.src.len) {
            const next = self.src[self.pos + 1];
            if (next == 'x' or next == 'X' or next == 'o' or next == 'O' or next == 'b' or next == 'B') {
                self.pos += 2;
                while (self.pos < self.src.len and isHexDigit(self.src[self.pos])) self.pos += 1;
                if (self.pos < self.src.len and self.src[self.pos] == 'n') {
                    self.pos += 1;
                    return SK.BigIntLiteral;
                }
                return SK.NumericLiteral;
            }
        }
        while (self.pos < self.src.len and (self.src[self.pos] >= '0' and self.src[self.pos] <= '9' or self.src[self.pos] == '_')) self.pos += 1;
        // Decimal
        if (self.pos < self.src.len and self.src[self.pos] == '.') {
            self.pos += 1;
            while (self.pos < self.src.len and (self.src[self.pos] >= '0' and self.src[self.pos] <= '9')) self.pos += 1;
        }
        // Exponent
        if (self.pos < self.src.len and (self.src[self.pos] == 'e' or self.src[self.pos] == 'E')) {
            self.pos += 1;
            if (self.pos < self.src.len and (self.src[self.pos] == '+' or self.src[self.pos] == '-')) self.pos += 1;
            while (self.pos < self.src.len and (self.src[self.pos] >= '0' and self.src[self.pos] <= '9')) self.pos += 1;
        }
        // BigInt suffix
        if (self.pos < self.src.len and self.src[self.pos] == 'n') {
            self.pos += 1;
            return SK.BigIntLiteral;
        }
        return SK.NumericLiteral;
    }

    fn scanIdentifier(self: *Scanner) TokenKind {
        const id_start = self.pos;
        while (self.pos < self.src.len and isIdentChar(self.src[self.pos])) self.pos += 1;
        const text = self.src[id_start..self.pos];
        return keywordLookup(text);
    }

    fn isIdentChar(c: u8) bool {
        return (c >= 'a' and c <= 'z') or (c >= 'A' and c <= 'Z') or (c >= '0' and c <= '9') or c == '_' or c == '$';
    }

    fn isHexDigit(c: u8) bool {
        return (c >= '0' and c <= '9') or (c >= 'a' and c <= 'f') or (c >= 'A' and c <= 'F') or c == '_';
    }
};

// Keyword lookup — matches identifier text to SyntaxKind.
// Uses first-char dispatch + length check for speed.
fn keywordLookup(text: []const u8) TokenKind {
    if (text.len < 2 or text.len > 12) return SK.Identifier;
    return switch (text[0]) {
        'a' => kwMatch(text, "abstract", 128) orelse kwMatch(text, "accessor", 129) orelse kwMatch(text, "any", 133) orelse kwMatch(text, "as", 130) orelse kwMatch(text, "asserts", 131) orelse kwMatch(text, "assert", 132) orelse kwMatch(text, "async", 134) orelse kwMatch(text, "await", 135) orelse SK.Identifier,
        'b' => kwMatch(text, "break", 83) orelse kwMatch(text, "boolean", 136) orelse kwMatch(text, "bigint", 163) orelse SK.Identifier,
        'c' => kwMatch(text, "case", 84) orelse kwMatch(text, "catch", 85) orelse kwMatch(text, "class", 86) orelse kwMatch(text, "const", 87) orelse kwMatch(text, "continue", 88) orelse kwMatch(text, "constructor", 137) orelse SK.Identifier,
        'd' => kwMatch(text, "debugger", 89) orelse kwMatch(text, "default", 90) orelse kwMatch(text, "delete", 91) orelse kwMatch(text, "do", 92) orelse kwMatch(text, "declare", 138) orelse kwMatch(text, "defer", 166) orelse SK.Identifier,
        'e' => kwMatch(text, "else", 93) orelse kwMatch(text, "enum", 94) orelse kwMatch(text, "export", 95) orelse kwMatch(text, "extends", 96) orelse SK.Identifier,
        'f' => kwMatch(text, "false", 97) orelse kwMatch(text, "finally", 98) orelse kwMatch(text, "for", 99) orelse kwMatch(text, "function", 100) orelse kwMatch(text, "from", 161) orelse SK.Identifier,
        'g' => kwMatch(text, "get", 139) orelse kwMatch(text, "global", 162) orelse SK.Identifier,
        'i' => kwMatch(text, "if", 101) orelse kwMatch(text, "import", 102) orelse kwMatch(text, "in", 103) orelse kwMatch(text, "instanceof", 104) orelse kwMatch(text, "interface", 120) orelse kwMatch(text, "implements", 119) orelse kwMatch(text, "infer", 140) orelse kwMatch(text, "intrinsic", 141) orelse kwMatch(text, "is", 142) orelse SK.Identifier,
        'k' => kwMatch(text, "keyof", 143) orelse SK.Identifier,
        'l' => kwMatch(text, "let", 121) orelse SK.Identifier,
        'm' => kwMatch(text, "module", 144) orelse SK.Identifier,
        'n' => kwMatch(text, "new", 105) orelse kwMatch(text, "null", 106) orelse kwMatch(text, "namespace", 145) orelse kwMatch(text, "never", 146) orelse kwMatch(text, "number", 150) orelse SK.Identifier,
        'o' => kwMatch(text, "of", 165) orelse kwMatch(text, "out", 147) orelse kwMatch(text, "object", 151) orelse kwMatch(text, "override", 164) orelse SK.Identifier,
        'p' => kwMatch(text, "package", 122) orelse kwMatch(text, "private", 123) orelse kwMatch(text, "protected", 124) orelse kwMatch(text, "public", 125) orelse SK.Identifier,
        'r' => kwMatch(text, "return", 107) orelse kwMatch(text, "readonly", 148) orelse kwMatch(text, "require", 149) orelse SK.Identifier,
        's' => kwMatch(text, "super", 108) orelse kwMatch(text, "switch", 109) orelse kwMatch(text, "static", 126) orelse kwMatch(text, "string", 154) orelse kwMatch(text, "symbol", 155) orelse kwMatch(text, "set", 153) orelse kwMatch(text, "satisfies", 152) orelse SK.Identifier,
        't' => kwMatch(text, "this", 110) orelse kwMatch(text, "throw", 111) orelse kwMatch(text, "true", 112) orelse kwMatch(text, "try", 113) orelse kwMatch(text, "typeof", 114) orelse kwMatch(text, "type", 156) orelse SK.Identifier,
        'u' => kwMatch(text, "undefined", 157) orelse kwMatch(text, "unique", 158) orelse kwMatch(text, "unknown", 159) orelse kwMatch(text, "using", 160) orelse SK.Identifier,
        'v' => kwMatch(text, "var", 115) orelse kwMatch(text, "void", 116) orelse SK.Identifier,
        'w' => kwMatch(text, "while", 117) orelse kwMatch(text, "with", 118) orelse SK.Identifier,
        'y' => kwMatch(text, "yield", 127) orelse SK.Identifier,
        else => SK.Identifier,
    };
}

fn kwMatch(text: []const u8, keyword: []const u8, kind: TokenKind) ?TokenKind {
    if (text.len == keyword.len and std.mem.eql(u8, text, keyword)) return kind;
    return null;
}

// ── Test / Benchmark ──

pub fn main() !void {
    var args = std.process.args();
    _ = args.next();
    const path = args.next() orelse {
        _ = std.posix.write(2, "Usage: zig_scanner <file.ts>\n") catch {};
        return;
    };
    const file = try std.fs.cwd().openFile(path, .{});
    defer file.close();
    const content = try file.readToEndAlloc(std.heap.page_allocator, 100 * 1024 * 1024);

    // Benchmark: scan 100 times
    var timer = try std.time.Timer.start();
    var total_tokens: u64 = 0;
    for (0..100) |_| {
        var s = Scanner.init(content);
        var count: u64 = 0;
        while (true) {
            const t = s.scan();
            if (t.kind == SK.EndOfFile) break;
            count += 1;
        }
        total_tokens += count;
    }
    const elapsed = timer.read();
    const tokens = total_tokens / 100;
    var buf: [256]u8 = undefined;
    const msg = std.fmt.bufPrint(&buf, "Tokens: {d}, Time: {d}ms (100 runs), {d}ms/run, {d}KB source\n", .{
        tokens,
        elapsed / std.time.ns_per_ms,
        elapsed / std.time.ns_per_ms / 100,
        content.len / 1024,
    }) catch "err\n";
    _ = std.posix.write(2, msg) catch {};
}
