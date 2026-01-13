//! Native SIMD Scanner for TypeScript
//!
//! High-performance tokenizer using SIMD for:
//!   - Whitespace detection (16 bytes at once)
//!   - Identifier character detection
//!   - Numeric literal detection
//!   - String delimiter detection
//!
//! Architecture:
//!   - Single-pass tokenization
//!   - Pre-allocated token buffer (no per-token allocation)
//!   - Compact token representation (8 bytes per token)
//!   - SIMD-based character classification
//!
//! Performance targets:
//!   - 10x faster than JavaScript scanner
//!   - Process ~1GB/s of source code

const std = @import("std");

/// Compact token representation (8 bytes)
/// Uses bit packing for cache efficiency
pub const Token = packed struct {
    /// SyntaxKind (0-255 covers all TypeScript tokens)
    kind: u8,
    /// TokenFlags (newline, unterminated, etc.)
    flags: u8,
    /// Start position in source (max 16M chars)
    start: u24,
    /// Token length (max 64K chars per token)
    length: u16,
};

/// Common TypeScript SyntaxKind values
/// Subset needed for fast-path transpilation
pub const SyntaxKind = enum(u8) {
    unknown = 0,
    end_of_file = 1,

    // Trivia
    whitespace = 5,
    newline = 6,
    single_line_comment = 7,
    multi_line_comment = 8,

    // Literals
    numeric_literal = 9,
    string_literal = 10,
    template_head = 11,
    template_middle = 12,
    template_tail = 13,
    no_substitution_template = 14,

    // Identifiers and keywords
    identifier = 80,
    keyword_const = 85,
    keyword_let = 86,
    keyword_var = 87,
    keyword_function = 97,
    keyword_class = 98,
    keyword_interface = 99,
    keyword_type = 100,
    keyword_enum = 101,
    keyword_import = 102,
    keyword_export = 103,
    keyword_async = 104,
    keyword_await = 105,

    // Punctuation
    open_brace = 19,
    close_brace = 20,
    open_paren = 21,
    close_paren = 22,
    open_bracket = 23,
    close_bracket = 24,
    dot = 25,
    dot_dot_dot = 26,
    semicolon = 27,
    comma = 28,
    colon = 59,
    less_than = 30,
    greater_than = 31,
    equals = 64,
    exclamation = 54,
    question = 58,
    plus = 40,
    minus = 41,
    asterisk = 42,
    slash = 44,
    ampersand = 51,
    bar = 52,
    at = 60,
    arrow = 39,
};

/// Token flags
pub const TokenFlags = packed struct {
    /// Preceded by newline
    preceded_by_newline: bool = false,
    /// Unterminated string/template
    unterminated: bool = false,
    /// Has extended unicode escape
    extended_escape: bool = false,
    /// Contains escape sequence
    contains_escape: bool = false,
    /// Reserved
    _reserved: u4 = 0,
};

/// SIMD vector type for character classification
const Vec16u8 = @Vector(16, u8);

/// SIMD character class masks
const whitespace_chars: Vec16u8 = .{ ' ', '\t', '\r', '\n', ' ', '\t', '\r', '\n', ' ', '\t', '\r', '\n', ' ', '\t', '\r', '\n' };

/// Native SIMD scanner
pub const NativeScanner = struct {
    const Self = @This();

    /// Source text
    source: []const u8,

    /// Current position
    pos: u32,

    /// Token buffer (pre-allocated)
    tokens: []Token,

    /// Number of tokens scanned
    token_count: u32,

    /// Capacity of token buffer
    capacity: u32,

    /// Allocator for growing token buffer
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator, source: []const u8) !Self {
        // Pre-allocate for ~1 token per 5 characters (typical density)
        const estimated_tokens: u32 = @intCast(@max(1024, source.len / 5));

        return .{
            .source = source,
            .pos = 0,
            .tokens = try allocator.alloc(Token, estimated_tokens),
            .token_count = 0,
            .capacity = estimated_tokens,
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *Self) void {
        self.allocator.free(self.tokens);
    }

    /// SIMD whitespace skip - scans 16 bytes at once
    pub fn simdSkipWhitespace(self: *Self) void {
        while (self.pos + 16 <= self.source.len) {
            // Load 16 bytes
            const chunk: Vec16u8 = self.source[self.pos..][0..16].*;

            // Check each whitespace character
            const is_space = chunk == @as(Vec16u8, @splat(' '));
            const is_tab = chunk == @as(Vec16u8, @splat('\t'));
            const is_cr = chunk == @as(Vec16u8, @splat('\r'));
            const is_lf = chunk == @as(Vec16u8, @splat('\n'));

            // Combine masks
            const is_ws = @select(bool, is_space, @as(@Vector(16, bool), @splat(true)), is_tab);
            const is_ws2 = @select(bool, is_ws, @as(@Vector(16, bool), @splat(true)), is_cr);
            const is_whitespace = @select(bool, is_ws2, @as(@Vector(16, bool), @splat(true)), is_lf);

            // Find first non-whitespace
            inline for (0..16) |i| {
                if (!is_whitespace[i]) {
                    self.pos += @intCast(i);
                    return;
                }
            }

            // All 16 chars are whitespace
            self.pos += 16;
        }

        // Handle remainder (scalar)
        while (self.pos < self.source.len) {
            const c = self.source[self.pos];
            if (c != ' ' and c != '\t' and c != '\r' and c != '\n') {
                break;
            }
            self.pos += 1;
        }
    }

    /// SIMD identifier scan - finds end of identifier
    pub fn simdScanIdentifier(self: *Self, start: u32) u32 {
        var pos = start;

        // SIMD scan while we have 16+ chars
        while (pos + 16 <= self.source.len) {
            const chunk: Vec16u8 = self.source[pos..][0..16].*;

            // Check identifier characters: a-z, A-Z, 0-9, _, $
            // Using range checks
            const lower_a: Vec16u8 = @splat('a');
            const lower_z: Vec16u8 = @splat('z');
            const upper_a: Vec16u8 = @splat('A');
            const upper_z: Vec16u8 = @splat('Z');
            const digit_0: Vec16u8 = @splat('0');
            const digit_9: Vec16u8 = @splat('9');
            const underscore: Vec16u8 = @splat('_');
            const dollar: Vec16u8 = @splat('$');

            const is_lower = (chunk >= lower_a) & (chunk <= lower_z);
            const is_upper = (chunk >= upper_a) & (chunk <= upper_z);
            const is_digit = (chunk >= digit_0) & (chunk <= digit_9);
            const is_underscore = chunk == underscore;
            const is_dollar = chunk == dollar;

            // Combine: any of these is valid identifier char
            const is_ident = @select(bool, is_lower, @as(@Vector(16, bool), @splat(true)), is_upper);
            const is_ident2 = @select(bool, is_ident, @as(@Vector(16, bool), @splat(true)), is_digit);
            const is_ident3 = @select(bool, is_ident2, @as(@Vector(16, bool), @splat(true)), is_underscore);
            const is_identifier = @select(bool, is_ident3, @as(@Vector(16, bool), @splat(true)), is_dollar);

            // Find first non-identifier character
            inline for (0..16) |i| {
                if (!is_identifier[i]) {
                    return pos + @as(u32, @intCast(i));
                }
            }

            pos += 16;
        }

        // Scalar remainder
        while (pos < self.source.len) {
            const c = self.source[pos];
            if (!isIdentifierChar(c)) {
                break;
            }
            pos += 1;
        }

        return pos;
    }

    /// Scan a single token
    pub fn scanToken(self: *Self) ?Token {
        // Skip whitespace using SIMD
        const ws_start = self.pos;
        self.simdSkipWhitespace();

        // Record whitespace token if any
        if (self.pos > ws_start) {
            // Check if contains newline
            var flags = TokenFlags{};
            for (self.source[ws_start..self.pos]) |c| {
                if (c == '\n') {
                    flags.preceded_by_newline = true;
                    break;
                }
            }
            _ = flags;
        }

        if (self.pos >= self.source.len) {
            return Token{
                .kind = @intFromEnum(SyntaxKind.end_of_file),
                .flags = 0,
                .start = @intCast(@min(self.pos, 0xFFFFFF)),
                .length = 0,
            };
        }

        const start = self.pos;
        const c = self.source[self.pos];

        // Single-character tokens
        switch (c) {
            '{' => {
                self.pos += 1;
                return Token{ .kind = @intFromEnum(SyntaxKind.open_brace), .flags = 0, .start = @intCast(start), .length = 1 };
            },
            '}' => {
                self.pos += 1;
                return Token{ .kind = @intFromEnum(SyntaxKind.close_brace), .flags = 0, .start = @intCast(start), .length = 1 };
            },
            '(' => {
                self.pos += 1;
                return Token{ .kind = @intFromEnum(SyntaxKind.open_paren), .flags = 0, .start = @intCast(start), .length = 1 };
            },
            ')' => {
                self.pos += 1;
                return Token{ .kind = @intFromEnum(SyntaxKind.close_paren), .flags = 0, .start = @intCast(start), .length = 1 };
            },
            '[' => {
                self.pos += 1;
                return Token{ .kind = @intFromEnum(SyntaxKind.open_bracket), .flags = 0, .start = @intCast(start), .length = 1 };
            },
            ']' => {
                self.pos += 1;
                return Token{ .kind = @intFromEnum(SyntaxKind.close_bracket), .flags = 0, .start = @intCast(start), .length = 1 };
            },
            ';' => {
                self.pos += 1;
                return Token{ .kind = @intFromEnum(SyntaxKind.semicolon), .flags = 0, .start = @intCast(start), .length = 1 };
            },
            ',' => {
                self.pos += 1;
                return Token{ .kind = @intFromEnum(SyntaxKind.comma), .flags = 0, .start = @intCast(start), .length = 1 };
            },
            ':' => {
                self.pos += 1;
                return Token{ .kind = @intFromEnum(SyntaxKind.colon), .flags = 0, .start = @intCast(start), .length = 1 };
            },
            '?' => {
                self.pos += 1;
                return Token{ .kind = @intFromEnum(SyntaxKind.question), .flags = 0, .start = @intCast(start), .length = 1 };
            },
            '@' => {
                self.pos += 1;
                return Token{ .kind = @intFromEnum(SyntaxKind.at), .flags = 0, .start = @intCast(start), .length = 1 };
            },
            '<' => {
                self.pos += 1;
                return Token{ .kind = @intFromEnum(SyntaxKind.less_than), .flags = 0, .start = @intCast(start), .length = 1 };
            },
            '>' => {
                self.pos += 1;
                return Token{ .kind = @intFromEnum(SyntaxKind.greater_than), .flags = 0, .start = @intCast(start), .length = 1 };
            },
            '+' => {
                self.pos += 1;
                return Token{ .kind = @intFromEnum(SyntaxKind.plus), .flags = 0, .start = @intCast(start), .length = 1 };
            },
            '*' => {
                self.pos += 1;
                return Token{ .kind = @intFromEnum(SyntaxKind.asterisk), .flags = 0, .start = @intCast(start), .length = 1 };
            },
            '&' => {
                self.pos += 1;
                return Token{ .kind = @intFromEnum(SyntaxKind.ampersand), .flags = 0, .start = @intCast(start), .length = 1 };
            },
            '|' => {
                self.pos += 1;
                return Token{ .kind = @intFromEnum(SyntaxKind.bar), .flags = 0, .start = @intCast(start), .length = 1 };
            },
            '!' => {
                self.pos += 1;
                return Token{ .kind = @intFromEnum(SyntaxKind.exclamation), .flags = 0, .start = @intCast(start), .length = 1 };
            },

            // Multi-character tokens
            '.' => {
                if (self.pos + 2 < self.source.len and
                    self.source[self.pos + 1] == '.' and
                    self.source[self.pos + 2] == '.')
                {
                    self.pos += 3;
                    return Token{ .kind = @intFromEnum(SyntaxKind.dot_dot_dot), .flags = 0, .start = @intCast(start), .length = 3 };
                }
                self.pos += 1;
                return Token{ .kind = @intFromEnum(SyntaxKind.dot), .flags = 0, .start = @intCast(start), .length = 1 };
            },

            '=' => {
                if (self.pos + 1 < self.source.len and self.source[self.pos + 1] == '>') {
                    self.pos += 2;
                    return Token{ .kind = @intFromEnum(SyntaxKind.arrow), .flags = 0, .start = @intCast(start), .length = 2 };
                }
                self.pos += 1;
                return Token{ .kind = @intFromEnum(SyntaxKind.equals), .flags = 0, .start = @intCast(start), .length = 1 };
            },

            '-' => {
                self.pos += 1;
                return Token{ .kind = @intFromEnum(SyntaxKind.minus), .flags = 0, .start = @intCast(start), .length = 1 };
            },

            '/' => {
                // Check for comment
                if (self.pos + 1 < self.source.len) {
                    if (self.source[self.pos + 1] == '/') {
                        // Single-line comment
                        self.pos += 2;
                        while (self.pos < self.source.len and self.source[self.pos] != '\n') {
                            self.pos += 1;
                        }
                        return Token{
                            .kind = @intFromEnum(SyntaxKind.single_line_comment),
                            .flags = 0,
                            .start = @intCast(start),
                            .length = @intCast(self.pos - start),
                        };
                    }
                    if (self.source[self.pos + 1] == '*') {
                        // Multi-line comment
                        self.pos += 2;
                        while (self.pos + 1 < self.source.len) {
                            if (self.source[self.pos] == '*' and self.source[self.pos + 1] == '/') {
                                self.pos += 2;
                                break;
                            }
                            self.pos += 1;
                        }
                        return Token{
                            .kind = @intFromEnum(SyntaxKind.multi_line_comment),
                            .flags = 0,
                            .start = @intCast(start),
                            .length = @intCast(self.pos - start),
                        };
                    }
                }
                self.pos += 1;
                return Token{ .kind = @intFromEnum(SyntaxKind.slash), .flags = 0, .start = @intCast(start), .length = 1 };
            },

            // String literals
            '"', '\'' => {
                const quote = c;
                self.pos += 1;
                while (self.pos < self.source.len) {
                    const sc = self.source[self.pos];
                    if (sc == quote) {
                        self.pos += 1;
                        break;
                    }
                    if (sc == '\\' and self.pos + 1 < self.source.len) {
                        self.pos += 2; // Skip escaped char
                    } else {
                        self.pos += 1;
                    }
                }
                return Token{
                    .kind = @intFromEnum(SyntaxKind.string_literal),
                    .flags = 0,
                    .start = @intCast(start),
                    .length = @intCast(self.pos - start),
                };
            },

            // Template literal
            '`' => {
                self.pos += 1;
                while (self.pos < self.source.len) {
                    const tc = self.source[self.pos];
                    if (tc == '`') {
                        self.pos += 1;
                        break;
                    }
                    if (tc == '\\' and self.pos + 1 < self.source.len) {
                        self.pos += 2;
                    } else if (tc == '$' and self.pos + 1 < self.source.len and self.source[self.pos + 1] == '{') {
                        // Template substitution - for now treat as template head
                        self.pos += 2;
                        return Token{
                            .kind = @intFromEnum(SyntaxKind.template_head),
                            .flags = 0,
                            .start = @intCast(start),
                            .length = @intCast(self.pos - start),
                        };
                    } else {
                        self.pos += 1;
                    }
                }
                return Token{
                    .kind = @intFromEnum(SyntaxKind.no_substitution_template),
                    .flags = 0,
                    .start = @intCast(start),
                    .length = @intCast(self.pos - start),
                };
            },

            // Numbers
            '0'...'9' => {
                while (self.pos < self.source.len and isDigitOrDot(self.source[self.pos])) {
                    self.pos += 1;
                }
                return Token{
                    .kind = @intFromEnum(SyntaxKind.numeric_literal),
                    .flags = 0,
                    .start = @intCast(start),
                    .length = @intCast(self.pos - start),
                };
            },

            // Identifiers and keywords
            'a'...'z', 'A'...'Z', '_', '$' => {
                const end = self.simdScanIdentifier(self.pos);
                const len = end - self.pos;
                self.pos = end;

                // Check for keywords
                const text = self.source[start..end];
                const kind = classifyKeyword(text);

                return Token{
                    .kind = kind,
                    .flags = 0,
                    .start = @intCast(start),
                    .length = @intCast(len),
                };
            },

            else => {
                self.pos += 1;
                return Token{ .kind = @intFromEnum(SyntaxKind.unknown), .flags = 0, .start = @intCast(start), .length = 1 };
            },
        }
    }

    /// Scan all tokens in one pass
    pub fn scanAll(self: *Self) !void {
        while (true) {
            const token = self.scanToken() orelse break;

            // Grow buffer if needed
            if (self.token_count >= self.capacity) {
                const new_capacity = self.capacity * 2;
                self.tokens = try self.allocator.realloc(self.tokens, new_capacity);
                self.capacity = new_capacity;
            }

            self.tokens[self.token_count] = token;
            self.token_count += 1;

            if (token.kind == @intFromEnum(SyntaxKind.end_of_file)) {
                break;
            }
        }
    }

    /// Get token text from source
    pub fn getTokenText(self: *const Self, token: Token) []const u8 {
        return self.source[token.start..][0..token.length];
    }
};

// ============================================================================
// Helper functions
// ============================================================================

inline fn isIdentifierChar(c: u8) bool {
    return (c >= 'a' and c <= 'z') or
        (c >= 'A' and c <= 'Z') or
        (c >= '0' and c <= '9') or
        c == '_' or c == '$';
}

inline fn isDigitOrDot(c: u8) bool {
    return (c >= '0' and c <= '9') or c == '.' or c == 'e' or c == 'E' or c == 'x' or c == 'X' or
        (c >= 'a' and c <= 'f') or (c >= 'A' and c <= 'F');
}

fn classifyKeyword(text: []const u8) u8 {
    // Fast keyword lookup using string comparison
    // Sorted by frequency for early exit
    if (std.mem.eql(u8, text, "const")) return @intFromEnum(SyntaxKind.keyword_const);
    if (std.mem.eql(u8, text, "let")) return @intFromEnum(SyntaxKind.keyword_let);
    if (std.mem.eql(u8, text, "var")) return @intFromEnum(SyntaxKind.keyword_var);
    if (std.mem.eql(u8, text, "function")) return @intFromEnum(SyntaxKind.keyword_function);
    if (std.mem.eql(u8, text, "class")) return @intFromEnum(SyntaxKind.keyword_class);
    if (std.mem.eql(u8, text, "interface")) return @intFromEnum(SyntaxKind.keyword_interface);
    if (std.mem.eql(u8, text, "type")) return @intFromEnum(SyntaxKind.keyword_type);
    if (std.mem.eql(u8, text, "enum")) return @intFromEnum(SyntaxKind.keyword_enum);
    if (std.mem.eql(u8, text, "import")) return @intFromEnum(SyntaxKind.keyword_import);
    if (std.mem.eql(u8, text, "export")) return @intFromEnum(SyntaxKind.keyword_export);
    if (std.mem.eql(u8, text, "async")) return @intFromEnum(SyntaxKind.keyword_async);
    if (std.mem.eql(u8, text, "await")) return @intFromEnum(SyntaxKind.keyword_await);

    return @intFromEnum(SyntaxKind.identifier);
}

// ============================================================================
// Tests
// ============================================================================

test "native scanner basic tokens" {
    const allocator = std.testing.allocator;
    var scanner = try NativeScanner.init(allocator, "const x = 1;");
    defer scanner.deinit();

    try scanner.scanAll();

    try std.testing.expect(scanner.token_count >= 5);

    // First token should be 'const' keyword
    try std.testing.expectEqual(@intFromEnum(SyntaxKind.keyword_const), scanner.tokens[0].kind);

    // Second token should be identifier 'x'
    try std.testing.expectEqual(@intFromEnum(SyntaxKind.identifier), scanner.tokens[1].kind);
    try std.testing.expectEqualStrings("x", scanner.getTokenText(scanner.tokens[1]));
}

test "native scanner SIMD whitespace skip" {
    const allocator = std.testing.allocator;
    // 20 spaces followed by 'x'
    var scanner = try NativeScanner.init(allocator, "                    x");
    defer scanner.deinit();

    const token = scanner.scanToken().?;

    try std.testing.expectEqual(@intFromEnum(SyntaxKind.identifier), token.kind);
    try std.testing.expectEqual(@as(u24, 20), token.start);
}

test "native scanner string literals" {
    const allocator = std.testing.allocator;
    var scanner = try NativeScanner.init(allocator, "\"hello\" 'world'");
    defer scanner.deinit();

    try scanner.scanAll();

    try std.testing.expect(scanner.token_count >= 2);
    try std.testing.expectEqual(@intFromEnum(SyntaxKind.string_literal), scanner.tokens[0].kind);
    try std.testing.expectEqual(@intFromEnum(SyntaxKind.string_literal), scanner.tokens[1].kind);
}

test "native scanner comments" {
    const allocator = std.testing.allocator;
    var scanner = try NativeScanner.init(allocator, "// comment\nx /* block */ y");
    defer scanner.deinit();

    try scanner.scanAll();

    // Should have: single-line comment, x, multi-line comment, y, EOF
    try std.testing.expect(scanner.token_count >= 4);
    try std.testing.expectEqual(@intFromEnum(SyntaxKind.single_line_comment), scanner.tokens[0].kind);
}

test "native scanner arrow function" {
    const allocator = std.testing.allocator;
    var scanner = try NativeScanner.init(allocator, "() => x");
    defer scanner.deinit();

    try scanner.scanAll();

    // Should have: (, ), =>, x, EOF
    try std.testing.expect(scanner.token_count >= 4);
    try std.testing.expectEqual(@intFromEnum(SyntaxKind.arrow), scanner.tokens[2].kind);
}

test "native scanner type annotation" {
    const allocator = std.testing.allocator;
    var scanner = try NativeScanner.init(allocator, "const x: number = 1;");
    defer scanner.deinit();

    try scanner.scanAll();

    // const, x, :, number, =, 1, ;, EOF
    try std.testing.expect(scanner.token_count >= 7);
    try std.testing.expectEqual(@intFromEnum(SyntaxKind.colon), scanner.tokens[2].kind);
    try std.testing.expectEqual(@intFromEnum(SyntaxKind.identifier), scanner.tokens[3].kind);
    try std.testing.expectEqualStrings("number", scanner.getTokenText(scanner.tokens[3]));
}
