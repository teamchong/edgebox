//! Native TypeScript Transpiler - High-Performance Token-Based
//!
//! This module provides a native Zig implementation of TypeScript
//! transformations using SIMD-accelerated tokenization.
//!
//! Supported transformations:
//!   - const/let/var declarations with type annotations
//!   - Function declarations with parameter/return types
//!   - Arrow functions with type annotations
//!   - Interface declarations (removed entirely)
//!   - Type alias declarations (removed entirely)
//!   - Import/export statements (pass through)
//!   - Class declarations with type annotations
//!   - Generic type parameters
//!
//! For unsupported cases, returns null and caller falls back to full TypeScript.

const std = @import("std");
const native_scanner = @import("native_scanner.zig");
const Token = native_scanner.Token;
const SyntaxKind = native_scanner.SyntaxKind;
const NativeScanner = native_scanner.NativeScanner;

/// Result of transpilation
pub const TranspileResult = struct {
    output: []const u8,
    allocator: std.mem.Allocator,

    pub fn deinit(self: *TranspileResult) void {
        self.allocator.free(self.output);
    }
};

/// Token-based TypeScript transpiler
pub const Transpiler = struct {
    const Self = @This();

    allocator: std.mem.Allocator,
    source: []const u8,
    tokens: []Token,
    token_count: u32,
    pos: u32, // Current token position
    output: std.ArrayListUnmanaged(u8),
    last_emit_end: u32, // End position of last emitted source range

    pub fn init(allocator: std.mem.Allocator, source: []const u8) !Self {
        var scanner = try NativeScanner.init(allocator, source);
        errdefer scanner.deinit();

        try scanner.scanAll();

        return .{
            .allocator = allocator,
            .source = source,
            .tokens = scanner.tokens,
            .token_count = scanner.token_count,
            .pos = 0,
            .output = .{},
            .last_emit_end = 0,
        };
    }

    pub fn deinit(self: *Self) void {
        self.allocator.free(self.tokens);
        self.output.deinit(self.allocator);
    }

    /// Get current token
    inline fn current(self: *const Self) Token {
        if (self.pos >= self.token_count) {
            return Token{ .kind = @intFromEnum(SyntaxKind.end_of_file), .flags = 0, .start = @intCast(self.source.len), .length = 0 };
        }
        return self.tokens[self.pos];
    }

    /// Get token at offset from current
    inline fn peek(self: *const Self, offset: u32) Token {
        const idx = self.pos + offset;
        if (idx >= self.token_count) {
            return Token{ .kind = @intFromEnum(SyntaxKind.end_of_file), .flags = 0, .start = @intCast(self.source.len), .length = 0 };
        }
        return self.tokens[idx];
    }

    /// Advance to next token
    inline fn advance(self: *Self) void {
        if (self.pos < self.token_count) {
            self.pos += 1;
        }
    }

    /// Check if current token is of given kind
    inline fn check(self: *const Self, kind: SyntaxKind) bool {
        return self.current().kind == @intFromEnum(kind);
    }

    /// Get token text
    inline fn getText(self: *const Self, token: Token) []const u8 {
        return self.source[token.start..][0..token.length];
    }

    /// Emit source range directly (zero-copy for unchanged code)
    fn emitRange(self: *Self, start: u32, end: u32) !void {
        if (end > start) {
            try self.output.appendSlice(self.allocator, self.source[start..end]);
        }
        self.last_emit_end = end;
    }

    /// Emit string literal
    fn emit(self: *Self, s: []const u8) !void {
        try self.output.appendSlice(self.allocator, s);
    }

    /// Skip whitespace/comment tokens
    fn skipTrivia(self: *Self) void {
        while (self.pos < self.token_count) {
            const kind = self.current().kind;
            if (kind == @intFromEnum(SyntaxKind.whitespace) or
                kind == @intFromEnum(SyntaxKind.newline) or
                kind == @intFromEnum(SyntaxKind.single_line_comment) or
                kind == @intFromEnum(SyntaxKind.multi_line_comment))
            {
                self.advance();
            } else {
                break;
            }
        }
    }

    /// Transpile entire source
    pub fn transpile(self: *Self) !?[]const u8 {
        while (!self.check(.end_of_file)) {
            // Try to handle each top-level construct
            if (!try self.handleTopLevel()) {
                // Unsupported construct - return null
                return null;
            }
        }

        // Emit any remaining source
        try self.emitRange(self.last_emit_end, @intCast(self.source.len));

        return try self.output.toOwnedSlice(self.allocator);
    }

    /// Handle a top-level statement
    fn handleTopLevel(self: *Self) !bool {
        const tok = self.current();
        const kind = tok.kind;

        // Interface declaration - skip entirely
        if (kind == @intFromEnum(SyntaxKind.keyword_interface)) {
            return self.skipInterfaceDeclaration();
        }

        // Type alias - skip entirely
        if (kind == @intFromEnum(SyntaxKind.keyword_type)) {
            return self.skipTypeAlias();
        }

        // Const/let/var declarations
        if (kind == @intFromEnum(SyntaxKind.keyword_const) or
            kind == @intFromEnum(SyntaxKind.keyword_let) or
            kind == @intFromEnum(SyntaxKind.keyword_var))
        {
            return self.handleVariableDeclaration();
        }

        // Function declaration
        if (kind == @intFromEnum(SyntaxKind.keyword_function)) {
            return self.handleFunctionDeclaration();
        }

        // Async function
        if (kind == @intFromEnum(SyntaxKind.keyword_async)) {
            return self.handleAsyncFunction();
        }

        // Export statement
        if (kind == @intFromEnum(SyntaxKind.keyword_export)) {
            return self.handleExport();
        }

        // Import statement
        if (kind == @intFromEnum(SyntaxKind.keyword_import)) {
            return self.handleImport();
        }

        // Class declaration
        if (kind == @intFromEnum(SyntaxKind.keyword_class)) {
            return self.handleClassDeclaration();
        }

        // Enum - not supported yet, fallback
        if (kind == @intFromEnum(SyntaxKind.keyword_enum)) {
            return false;
        }

        // For any other token, just advance and emit
        self.advance();
        return true;
    }

    /// Skip interface declaration entirely
    fn skipInterfaceDeclaration(self: *Self) !bool {
        // Emit everything before 'interface'
        const interface_start = self.current().start;
        try self.emitRange(self.last_emit_end, interface_start);

        // Skip: interface Name { ... }
        self.advance(); // 'interface'

        // Skip name
        if (self.check(.identifier)) {
            self.advance();
        }

        // Skip generic parameters <T, U>
        if (self.check(.less_than)) {
            try self.skipGenericParams();
        }

        // Skip extends clause
        while (self.pos < self.token_count) {
            const kind = self.current().kind;
            if (kind == @intFromEnum(SyntaxKind.open_brace)) break;
            self.advance();
        }

        // Skip body { ... }
        if (!try self.skipBraceBlock()) return false;

        // Update last_emit_end to skip the entire interface
        self.last_emit_end = self.current().start;

        return true;
    }

    /// Skip type alias entirely
    fn skipTypeAlias(self: *Self) !bool {
        // Emit everything before 'type'
        const type_start = self.current().start;
        try self.emitRange(self.last_emit_end, type_start);

        // Skip: type Name = ...;
        self.advance(); // 'type'

        // Skip until semicolon or end of statement
        var brace_depth: u32 = 0;
        var angle_depth: u32 = 0;
        while (self.pos < self.token_count) {
            const kind = self.current().kind;

            if (kind == @intFromEnum(SyntaxKind.open_brace)) brace_depth += 1;
            if (kind == @intFromEnum(SyntaxKind.close_brace)) {
                if (brace_depth > 0) brace_depth -= 1;
            }
            if (kind == @intFromEnum(SyntaxKind.less_than)) angle_depth += 1;
            if (kind == @intFromEnum(SyntaxKind.greater_than)) {
                if (angle_depth > 0) angle_depth -= 1;
            }

            self.advance();

            if (kind == @intFromEnum(SyntaxKind.semicolon) and brace_depth == 0 and angle_depth == 0) {
                break;
            }
        }

        // Update last_emit_end to skip the entire type alias
        self.last_emit_end = self.current().start;

        return true;
    }

    /// Handle variable declaration with possible type annotation
    fn handleVariableDeclaration(self: *Self) !bool {
        // Emit everything up to and including the keyword
        const keyword_token = self.current();
        const keyword_end: u32 = keyword_token.start + keyword_token.length;
        try self.emitRange(self.last_emit_end, keyword_end);
        self.advance();

        // Process variable name(s)
        while (!self.check(.end_of_file)) {
            // Skip to identifier
            while (!self.check(.identifier) and !self.check(.end_of_file) and
                !self.check(.semicolon))
            {
                const tok = self.current();
                try self.emitRange(self.last_emit_end, tok.start + tok.length);
                self.advance();
            }

            if (self.check(.identifier)) {
                // Emit the identifier
                const id_tok = self.current();
                try self.emitRange(self.last_emit_end, id_tok.start + id_tok.length);
                self.advance();

                // Check for type annotation
                if (self.check(.colon)) {
                    // Skip the type annotation
                    if (!try self.skipTypeAnnotation()) return false;
                }
            }

            // Check what comes next
            if (self.check(.equals)) {
                // Has initializer - emit = and handle the expression
                const eq_tok = self.current();
                try self.emitRange(self.last_emit_end, eq_tok.start + eq_tok.length);
                self.advance();

                // Handle the expression (might contain arrow function)
                if (!try self.handleExpression()) return false;
            }

            if (self.check(.comma)) {
                // Multiple declarations
                const comma_tok = self.current();
                try self.emitRange(self.last_emit_end, comma_tok.start + comma_tok.length);
                self.advance();
                continue;
            }

            if (self.check(.semicolon)) {
                const semi_tok = self.current();
                try self.emitRange(self.last_emit_end, semi_tok.start + semi_tok.length);
                self.advance();
                break;
            }

            // End of statement (no semicolon)
            break;
        }

        return true;
    }

    /// Handle expression (for initializers, might contain arrow functions)
    fn handleExpression(self: *Self) !bool {
        var paren_depth: u32 = 0;
        var brace_depth: u32 = 0;
        var bracket_depth: u32 = 0;
        var just_closed_paren = false;

        while (!self.check(.end_of_file)) {
            const tok = self.current();
            const kind = tok.kind;

            // Track nesting
            if (kind == @intFromEnum(SyntaxKind.open_paren)) {
                paren_depth += 1;
                just_closed_paren = false;
            }
            if (kind == @intFromEnum(SyntaxKind.close_paren)) {
                if (paren_depth > 0) paren_depth -= 1;
                just_closed_paren = true;
                try self.emitRange(self.last_emit_end, tok.start + tok.length);
                self.advance();
                continue;
            }
            if (kind == @intFromEnum(SyntaxKind.open_brace)) {
                brace_depth += 1;
                just_closed_paren = false;
            }
            if (kind == @intFromEnum(SyntaxKind.close_brace)) {
                if (brace_depth > 0) brace_depth -= 1;
                just_closed_paren = false;
            }
            if (kind == @intFromEnum(SyntaxKind.open_bracket)) {
                bracket_depth += 1;
                just_closed_paren = false;
            }
            if (kind == @intFromEnum(SyntaxKind.close_bracket)) {
                if (bracket_depth > 0) bracket_depth -= 1;
                just_closed_paren = false;
            }

            // Check for arrow function: ) =>
            // We need to handle type annotations in parameters
            if (kind == @intFromEnum(SyntaxKind.arrow)) {
                // Emit up to arrow
                try self.emitRange(self.last_emit_end, tok.start + tok.length);
                self.advance();
                just_closed_paren = false;
                continue;
            }

            // Check for type annotation after parameter name in arrow function
            if (kind == @intFromEnum(SyntaxKind.colon) and paren_depth > 0) {
                // This might be a type annotation in function parameters
                // Skip it
                if (!try self.skipTypeAnnotation()) return false;
                just_closed_paren = false;
                continue;
            }

            // Check for return type annotation after closing paren
            // e.g., (x: number): number => x
            if (kind == @intFromEnum(SyntaxKind.colon) and just_closed_paren) {
                // Return type annotation - skip it
                if (!try self.skipTypeAnnotation()) return false;
                just_closed_paren = false;
                continue;
            }

            // End of expression at top level
            if (paren_depth == 0 and brace_depth == 0 and bracket_depth == 0) {
                if (kind == @intFromEnum(SyntaxKind.semicolon) or
                    kind == @intFromEnum(SyntaxKind.comma))
                {
                    break;
                }
            }

            // Emit and advance
            try self.emitRange(self.last_emit_end, tok.start + tok.length);
            self.advance();
            just_closed_paren = false;
        }

        return true;
    }

    /// Skip type annotation (after colon)
    fn skipTypeAnnotation(self: *Self) !bool {
        // Record position before colon
        const colon_start = self.current().start;
        try self.emitRange(self.last_emit_end, colon_start);

        self.advance(); // Skip ':'

        // Skip the type expression
        var angle_depth: u32 = 0;
        var paren_depth: u32 = 0;
        var bracket_depth: u32 = 0;

        while (!self.check(.end_of_file)) {
            const kind = self.current().kind;

            // Track nesting
            if (kind == @intFromEnum(SyntaxKind.less_than)) angle_depth += 1;
            if (kind == @intFromEnum(SyntaxKind.greater_than)) {
                if (angle_depth > 0) {
                    angle_depth -= 1;
                    self.advance();
                    continue;
                }
            }
            if (kind == @intFromEnum(SyntaxKind.open_paren)) paren_depth += 1;
            if (kind == @intFromEnum(SyntaxKind.close_paren)) {
                if (paren_depth > 0) {
                    paren_depth -= 1;
                    self.advance();
                    continue;
                }
                // End of type - we're at a close paren that's not ours
                break;
            }
            if (kind == @intFromEnum(SyntaxKind.open_bracket)) bracket_depth += 1;
            if (kind == @intFromEnum(SyntaxKind.close_bracket)) {
                if (bracket_depth > 0) {
                    bracket_depth -= 1;
                    self.advance();
                    continue;
                }
            }

            // Inside nested brackets, consume everything
            if (angle_depth > 0 or paren_depth > 0) {
                self.advance();
                continue;
            }

            // Type can include: identifier, |, &, [], etc.
            if (kind == @intFromEnum(SyntaxKind.identifier) or
                kind == @intFromEnum(SyntaxKind.bar) or
                kind == @intFromEnum(SyntaxKind.ampersand) or
                kind == @intFromEnum(SyntaxKind.open_bracket) or
                kind == @intFromEnum(SyntaxKind.close_bracket) or
                kind == @intFromEnum(SyntaxKind.dot) or
                kind == @intFromEnum(SyntaxKind.question))
            {
                self.advance();
                continue;
            }

            // End of type
            break;
        }

        // Update last_emit_end to skip the type annotation
        self.last_emit_end = self.current().start;

        // Emit a space to replace the removed type annotation for readability
        const next_kind = self.current().kind;
        if (next_kind == @intFromEnum(SyntaxKind.equals) or
            next_kind == @intFromEnum(SyntaxKind.open_brace) or
            next_kind == @intFromEnum(SyntaxKind.arrow))
        {
            try self.emit(" ");
        }

        return true;
    }

    /// Handle function declaration
    fn handleFunctionDeclaration(self: *Self) !bool {
        // Emit 'function'
        const func_tok = self.current();
        try self.emitRange(self.last_emit_end, func_tok.start + func_tok.length);
        self.advance();

        // Emit function name if present
        if (self.check(.identifier)) {
            const name_tok = self.current();
            try self.emitRange(self.last_emit_end, name_tok.start + name_tok.length);
            self.advance();
        }

        // Skip generic type parameters <T>
        if (self.check(.less_than)) {
            try self.skipGenericParams();
        }

        // Handle parameters
        if (!try self.handleFunctionParams()) return false;

        // Skip return type
        if (self.check(.colon)) {
            if (!try self.skipTypeAnnotation()) return false;
        }

        // Handle function body
        if (self.check(.open_brace)) {
            if (!try self.handleBraceBlock()) return false;
        }

        return true;
    }

    /// Handle async function
    fn handleAsyncFunction(self: *Self) !bool {
        // Emit 'async'
        const async_tok = self.current();
        try self.emitRange(self.last_emit_end, async_tok.start + async_tok.length);
        self.advance();

        // Check for function keyword
        if (self.check(.keyword_function)) {
            return self.handleFunctionDeclaration();
        }

        // Could be async arrow function - just continue
        return true;
    }

    /// Handle function parameters with type annotations
    fn handleFunctionParams(self: *Self) !bool {
        if (!self.check(.open_paren)) return true;

        // Emit '('
        const open_tok = self.current();
        try self.emitRange(self.last_emit_end, open_tok.start + open_tok.length);
        self.advance();

        while (!self.check(.close_paren) and !self.check(.end_of_file)) {
            // Handle destructuring, rest params, etc.
            const tok = self.current();
            const kind = tok.kind;

            // Rest parameter ...
            if (kind == @intFromEnum(SyntaxKind.dot_dot_dot)) {
                try self.emitRange(self.last_emit_end, tok.start + tok.length);
                self.advance();
                continue;
            }

            // Parameter name
            if (kind == @intFromEnum(SyntaxKind.identifier)) {
                try self.emitRange(self.last_emit_end, tok.start + tok.length);
                self.advance();

                // Optional parameter ?
                if (self.check(.question)) {
                    // Skip the ? - it's TypeScript-only
                    const q_start = self.current().start;
                    try self.emitRange(self.last_emit_end, q_start);
                    self.advance();
                    self.last_emit_end = self.current().start;
                }

                // Type annotation
                if (self.check(.colon)) {
                    if (!try self.skipTypeAnnotation()) return false;
                }

                // Default value
                if (self.check(.equals)) {
                    const eq_tok = self.current();
                    try self.emitRange(self.last_emit_end, eq_tok.start + eq_tok.length);
                    self.advance();

                    // Handle default value expression
                    if (!try self.handleParamDefaultValue()) return false;
                }

                continue;
            }

            // Comma between params
            if (kind == @intFromEnum(SyntaxKind.comma)) {
                try self.emitRange(self.last_emit_end, tok.start + tok.length);
                self.advance();
                continue;
            }

            // Destructuring { } or [ ]
            if (kind == @intFromEnum(SyntaxKind.open_brace) or
                kind == @intFromEnum(SyntaxKind.open_bracket))
            {
                if (!try self.handleDestructuringParam()) return false;
                continue;
            }

            // Other - just emit
            try self.emitRange(self.last_emit_end, tok.start + tok.length);
            self.advance();
        }

        // Emit ')'
        if (self.check(.close_paren)) {
            const close_tok = self.current();
            try self.emitRange(self.last_emit_end, close_tok.start + close_tok.length);
            self.advance();
        }

        return true;
    }

    /// Handle parameter default value
    fn handleParamDefaultValue(self: *Self) !bool {
        var paren_depth: u32 = 0;
        var brace_depth: u32 = 0;
        var bracket_depth: u32 = 0;

        while (!self.check(.end_of_file)) {
            const tok = self.current();
            const kind = tok.kind;

            if (kind == @intFromEnum(SyntaxKind.open_paren)) paren_depth += 1;
            if (kind == @intFromEnum(SyntaxKind.close_paren)) {
                if (paren_depth > 0) {
                    paren_depth -= 1;
                } else {
                    break; // End of params
                }
            }
            if (kind == @intFromEnum(SyntaxKind.open_brace)) brace_depth += 1;
            if (kind == @intFromEnum(SyntaxKind.close_brace)) {
                if (brace_depth > 0) brace_depth -= 1;
            }
            if (kind == @intFromEnum(SyntaxKind.open_bracket)) bracket_depth += 1;
            if (kind == @intFromEnum(SyntaxKind.close_bracket)) {
                if (bracket_depth > 0) bracket_depth -= 1;
            }

            // End at comma (next param) at top level
            if (kind == @intFromEnum(SyntaxKind.comma) and
                paren_depth == 0 and brace_depth == 0 and bracket_depth == 0)
            {
                break;
            }

            try self.emitRange(self.last_emit_end, tok.start + tok.length);
            self.advance();
        }

        return true;
    }

    /// Handle destructuring parameter
    fn handleDestructuringParam(self: *Self) !bool {
        const open_kind = self.current().kind;
        const close_kind: u8 = if (open_kind == @intFromEnum(SyntaxKind.open_brace))
            @intFromEnum(SyntaxKind.close_brace)
        else
            @intFromEnum(SyntaxKind.close_bracket);

        var depth: u32 = 0;

        while (!self.check(.end_of_file)) {
            const tok = self.current();
            const kind = tok.kind;

            if (kind == open_kind) depth += 1;
            if (kind == close_kind) {
                depth -= 1;
                try self.emitRange(self.last_emit_end, tok.start + tok.length);
                self.advance();
                if (depth == 0) break;
                continue;
            }

            // Type annotation inside destructuring - skip
            if (kind == @intFromEnum(SyntaxKind.colon)) {
                // Check if this is type annotation or property shorthand
                // In destructuring, : can be renaming ({ a: b }) or type ({ a }: Type)
                // Only skip if followed by type-like thing
                const next = self.peek(1);
                if (next.kind == @intFromEnum(SyntaxKind.identifier)) {
                    const next_next = self.peek(2);
                    if (next_next.kind == @intFromEnum(SyntaxKind.comma) or
                        next_next.kind == close_kind or
                        next_next.kind == @intFromEnum(SyntaxKind.equals))
                    {
                        // This is renaming { a: b }
                        try self.emitRange(self.last_emit_end, tok.start + tok.length);
                        self.advance();
                        continue;
                    }
                }
                // Otherwise it's a type annotation
                if (!try self.skipTypeAnnotation()) return false;
                continue;
            }

            try self.emitRange(self.last_emit_end, tok.start + tok.length);
            self.advance();
        }

        // Check for type annotation after destructuring
        if (self.check(.colon)) {
            if (!try self.skipTypeAnnotation()) return false;
        }

        return true;
    }

    /// Handle export statement
    fn handleExport(self: *Self) !bool {
        // Emit 'export'
        const export_tok = self.current();
        try self.emitRange(self.last_emit_end, export_tok.start + export_tok.length);
        self.advance();

        // Check what follows
        const kind = self.current().kind;

        // export default
        if (kind == @intFromEnum(SyntaxKind.identifier)) {
            const text = self.getText(self.current());
            if (std.mem.eql(u8, text, "default")) {
                const default_tok = self.current();
                try self.emitRange(self.last_emit_end, default_tok.start + default_tok.length);
                self.advance();
            }
        }

        // export interface -> skip
        if (self.check(.keyword_interface)) {
            return self.skipInterfaceDeclaration();
        }

        // export type -> skip
        if (self.check(.keyword_type)) {
            return self.skipTypeAlias();
        }

        // export function/class/const/etc.
        const next_kind = self.current().kind;

        if (next_kind == @intFromEnum(SyntaxKind.keyword_const) or
            next_kind == @intFromEnum(SyntaxKind.keyword_let) or
            next_kind == @intFromEnum(SyntaxKind.keyword_var))
        {
            return self.handleVariableDeclaration();
        }

        if (next_kind == @intFromEnum(SyntaxKind.keyword_function)) {
            return self.handleFunctionDeclaration();
        }

        if (next_kind == @intFromEnum(SyntaxKind.keyword_async)) {
            return self.handleAsyncFunction();
        }

        if (next_kind == @intFromEnum(SyntaxKind.keyword_class)) {
            return self.handleClassDeclaration();
        }

        // Unknown export - advance and continue
        return true;
    }

    /// Handle import statement - pass through (might need to skip type imports)
    fn handleImport(self: *Self) !bool {
        const import_start = self.current().start;

        // Look ahead to see if this is `import type`
        self.advance(); // 'import'

        if (self.check(.keyword_type)) {
            // import type { ... } -> skip entirely
            try self.emitRange(self.last_emit_end, import_start);

            // Skip to end of statement
            while (!self.check(.semicolon) and !self.check(.end_of_file)) {
                self.advance();
            }
            if (self.check(.semicolon)) {
                self.advance();
            }
            self.last_emit_end = self.current().start;
            return true;
        }

        // Regular import - emit normally
        // We already advanced past 'import', so emit from import_start
        try self.emitRange(self.last_emit_end, import_start);

        // Continue until semicolon
        while (!self.check(.semicolon) and !self.check(.end_of_file)) {
            const tok = self.current();
            try self.emitRange(self.last_emit_end, tok.start + tok.length);
            self.advance();
        }

        if (self.check(.semicolon)) {
            const semi_tok = self.current();
            try self.emitRange(self.last_emit_end, semi_tok.start + semi_tok.length);
            self.advance();
        }

        return true;
    }

    /// Handle class declaration
    fn handleClassDeclaration(self: *Self) !bool {
        // Emit 'class'
        const class_tok = self.current();
        try self.emitRange(self.last_emit_end, class_tok.start + class_tok.length);
        self.advance();

        // Emit class name
        if (self.check(.identifier)) {
            const name_tok = self.current();
            try self.emitRange(self.last_emit_end, name_tok.start + name_tok.length);
            self.advance();
        }

        // Skip generic type parameters <T>
        if (self.check(.less_than)) {
            try self.skipGenericParams();
        }

        // Handle extends/implements
        while (!self.check(.open_brace) and !self.check(.end_of_file)) {
            const tok = self.current();
            const kind = tok.kind;

            // Skip 'implements' clause entirely
            if (kind == @intFromEnum(SyntaxKind.identifier)) {
                const text = self.getText(tok);
                if (std.mem.eql(u8, text, "implements")) {
                    try self.emitRange(self.last_emit_end, tok.start);
                    // Skip implements and the types
                    while (!self.check(.open_brace) and !self.check(.end_of_file)) {
                        self.advance();
                    }
                    self.last_emit_end = self.current().start;
                    break;
                }
            }

            try self.emitRange(self.last_emit_end, tok.start + tok.length);
            self.advance();
        }

        // Handle class body
        if (self.check(.open_brace)) {
            if (!try self.handleClassBody()) return false;
        }

        return true;
    }

    /// Handle class body
    fn handleClassBody(self: *Self) !bool {
        // Emit '{'
        const open_tok = self.current();
        try self.emitRange(self.last_emit_end, open_tok.start + open_tok.length);
        self.advance();

        while (!self.check(.close_brace) and !self.check(.end_of_file)) {
            // Handle class members
            if (!try self.handleClassMember()) return false;
        }

        // Emit '}'
        if (self.check(.close_brace)) {
            const close_tok = self.current();
            try self.emitRange(self.last_emit_end, close_tok.start + close_tok.length);
            self.advance();
        }

        return true;
    }

    /// Handle class member
    fn handleClassMember(self: *Self) !bool {
        const tok = self.current();
        const kind = tok.kind;

        // Skip modifiers: public, private, protected, readonly, static, abstract
        const text = self.getText(tok);
        if (std.mem.eql(u8, text, "public") or
            std.mem.eql(u8, text, "private") or
            std.mem.eql(u8, text, "protected") or
            std.mem.eql(u8, text, "readonly") or
            std.mem.eql(u8, text, "abstract"))
        {
            // Skip TypeScript-only modifiers
            try self.emitRange(self.last_emit_end, tok.start);
            self.advance();
            self.last_emit_end = self.current().start;
            return true;
        }

        // static is valid JS
        if (std.mem.eql(u8, text, "static")) {
            try self.emitRange(self.last_emit_end, tok.start + tok.length);
            self.advance();
            return true;
        }

        // Constructor or method
        if (kind == @intFromEnum(SyntaxKind.identifier) or
            std.mem.eql(u8, text, "constructor") or
            std.mem.eql(u8, text, "get") or
            std.mem.eql(u8, text, "set"))
        {
            // Emit method name
            try self.emitRange(self.last_emit_end, tok.start + tok.length);
            self.advance();

            // Skip generic params
            if (self.check(.less_than)) {
                try self.skipGenericParams();
            }

            // Handle parameters
            if (self.check(.open_paren)) {
                if (!try self.handleFunctionParams()) return false;
            }

            // Skip return type
            if (self.check(.colon)) {
                if (!try self.skipTypeAnnotation()) return false;
            }

            // Handle method body
            if (self.check(.open_brace)) {
                if (!try self.handleBraceBlock()) return false;
            }

            // Property with semicolon
            if (self.check(.semicolon)) {
                const semi_tok = self.current();
                try self.emitRange(self.last_emit_end, semi_tok.start + semi_tok.length);
                self.advance();
            }

            return true;
        }

        // Other - just emit
        try self.emitRange(self.last_emit_end, tok.start + tok.length);
        self.advance();
        return true;
    }

    /// Skip generic type parameters <T, U extends Foo>
    fn skipGenericParams(self: *Self) !void {
        const start = self.current().start;
        try self.emitRange(self.last_emit_end, start);

        var depth: u32 = 0;
        while (!self.check(.end_of_file)) {
            const kind = self.current().kind;

            if (kind == @intFromEnum(SyntaxKind.less_than)) depth += 1;
            if (kind == @intFromEnum(SyntaxKind.greater_than)) {
                depth -= 1;
                self.advance();
                if (depth == 0) break;
                continue;
            }
            self.advance();
        }

        self.last_emit_end = self.current().start;
    }

    /// Skip brace block (for interfaces)
    fn skipBraceBlock(self: *Self) !bool {
        if (!self.check(.open_brace)) return true;

        var depth: u32 = 0;
        while (!self.check(.end_of_file)) {
            const kind = self.current().kind;

            if (kind == @intFromEnum(SyntaxKind.open_brace)) depth += 1;
            if (kind == @intFromEnum(SyntaxKind.close_brace)) {
                depth -= 1;
                self.advance();
                if (depth == 0) break;
                continue;
            }
            self.advance();
        }

        return true;
    }

    /// Handle brace block (emit content, recursively handle nested)
    fn handleBraceBlock(self: *Self) !bool {
        if (!self.check(.open_brace)) return true;

        // Emit '{'
        const open_tok = self.current();
        try self.emitRange(self.last_emit_end, open_tok.start + open_tok.length);
        self.advance();

        var depth: u32 = 1;
        while (depth > 0 and !self.check(.end_of_file)) {
            const tok = self.current();
            const kind = tok.kind;

            if (kind == @intFromEnum(SyntaxKind.open_brace)) depth += 1;
            if (kind == @intFromEnum(SyntaxKind.close_brace)) depth -= 1;

            // Handle nested type annotations in blocks
            if (kind == @intFromEnum(SyntaxKind.colon)) {
                // Check if this is a type annotation context
                // (after identifier, before = or ; or , or ))
                if (!try self.maybeSkipTypeAnnotation()) {
                    try self.emitRange(self.last_emit_end, tok.start + tok.length);
                    self.advance();
                }
                continue;
            }

            try self.emitRange(self.last_emit_end, tok.start + tok.length);
            self.advance();
        }

        return true;
    }

    /// Maybe skip type annotation (only if in right context)
    fn maybeSkipTypeAnnotation(self: *Self) !bool {
        // Look back to see if previous non-trivia token was identifier or )
        if (self.pos == 0) return false;

        var prev_idx = self.pos - 1;
        while (prev_idx > 0) {
            const prev_kind = self.tokens[prev_idx].kind;
            if (prev_kind != @intFromEnum(SyntaxKind.whitespace) and
                prev_kind != @intFromEnum(SyntaxKind.newline))
            {
                break;
            }
            prev_idx -= 1;
        }

        const prev_kind = self.tokens[prev_idx].kind;
        if (prev_kind != @intFromEnum(SyntaxKind.identifier) and
            prev_kind != @intFromEnum(SyntaxKind.close_paren) and
            prev_kind != @intFromEnum(SyntaxKind.question))
        {
            // Not a type annotation context (e.g., object literal { a: 1 })
            return false;
        }

        // Look ahead to see what follows the type
        // Save position
        const saved_pos = self.pos;
        const saved_emit = self.last_emit_end;

        // Skip the type
        const colon_start = self.current().start;
        self.advance(); // ':'

        var angle_depth: u32 = 0;
        var paren_depth: u32 = 0;
        while (!self.check(.end_of_file)) {
            const kind = self.current().kind;

            if (kind == @intFromEnum(SyntaxKind.less_than)) angle_depth += 1;
            if (kind == @intFromEnum(SyntaxKind.greater_than)) {
                if (angle_depth > 0) {
                    angle_depth -= 1;
                    self.advance();
                    continue;
                }
            }
            if (kind == @intFromEnum(SyntaxKind.open_paren)) paren_depth += 1;
            if (kind == @intFromEnum(SyntaxKind.close_paren)) {
                if (paren_depth > 0) {
                    paren_depth -= 1;
                    self.advance();
                    continue;
                }
                break;
            }

            if (angle_depth > 0 or paren_depth > 0) {
                self.advance();
                continue;
            }

            if (kind == @intFromEnum(SyntaxKind.identifier) or
                kind == @intFromEnum(SyntaxKind.bar) or
                kind == @intFromEnum(SyntaxKind.ampersand) or
                kind == @intFromEnum(SyntaxKind.open_bracket) or
                kind == @intFromEnum(SyntaxKind.close_bracket) or
                kind == @intFromEnum(SyntaxKind.dot) or
                kind == @intFromEnum(SyntaxKind.question))
            {
                self.advance();
                continue;
            }

            break;
        }

        // Check what follows
        const after_kind = self.current().kind;
        if (after_kind == @intFromEnum(SyntaxKind.equals) or
            after_kind == @intFromEnum(SyntaxKind.semicolon) or
            after_kind == @intFromEnum(SyntaxKind.comma) or
            after_kind == @intFromEnum(SyntaxKind.close_paren) or
            after_kind == @intFromEnum(SyntaxKind.open_brace) or
            after_kind == @intFromEnum(SyntaxKind.arrow))
        {
            // This is a type annotation - commit the skip
            try self.output.appendSlice(self.allocator, self.source[saved_emit..colon_start]);
            self.last_emit_end = self.current().start;
            return true;
        }

        // Not a type annotation - restore
        self.pos = saved_pos;
        self.last_emit_end = saved_emit;
        return false;
    }
};

/// Fast transpile TypeScript to JavaScript
/// Returns null if source cannot be handled by native transpiler
pub fn fastTranspile(allocator: std.mem.Allocator, source: []const u8) !?TranspileResult {
    var transpiler = try Transpiler.init(allocator, source);
    errdefer transpiler.deinit();

    const output = try transpiler.transpile();
    if (output) |out| {
        // Transfer ownership - don't free tokens yet
        const tokens = transpiler.tokens;
        transpiler.tokens = &.{};
        allocator.free(tokens);

        return TranspileResult{
            .output = out,
            .allocator = allocator,
        };
    }

    return null;
}

/// Arena-optimized transpile - uses arena allocator for zero-cost allocation
/// The arena is reset after transpilation, all intermediate allocations are freed at once
pub fn arenaTranspile(arena: *std.heap.ArenaAllocator, source: []const u8) !?[]const u8 {
    const allocator = arena.allocator();

    var transpiler = try Transpiler.init(allocator, source);
    // No need for errdefer - arena will clean up everything

    const output = try transpiler.transpile();
    return output;
}

/// Pre-allocated buffer transpiler for maximum performance
/// Uses a fixed buffer with no allocations for small files
pub const BufferTranspiler = struct {
    const Self = @This();

    /// Maximum source size supported by buffer transpiler
    pub const MAX_SOURCE_SIZE = 64 * 1024; // 64KB
    /// Maximum token count
    pub const MAX_TOKENS = 16 * 1024; // 16K tokens

    token_buf: [MAX_TOKENS]Token,
    output_buf: [MAX_SOURCE_SIZE]u8,

    pub fn init() Self {
        return .{
            .token_buf = undefined,
            .output_buf = undefined,
        };
    }

    /// Transpile using pre-allocated buffers
    /// Returns output slice or null if source is too large or unsupported
    pub fn transpile(self: *Self, source: []const u8) ?[]const u8 {
        if (source.len > MAX_SOURCE_SIZE) return null;

        // Use fixed buffer allocator for zero-allocation scanning
        var fba = std.heap.FixedBufferAllocator.init(&self.output_buf);
        const allocator = fba.allocator();

        // Scan tokens into fixed buffer
        var scanner = NativeScanner{
            .source = source,
            .pos = 0,
            .tokens = &self.token_buf,
            .token_count = 0,
            .capacity = MAX_TOKENS,
            .allocator = allocator,
        };

        scanner.scanAll() catch return null;

        // Create transpiler with scanned tokens
        var transpiler = Transpiler{
            .allocator = allocator,
            .source = source,
            .tokens = &self.token_buf,
            .token_count = scanner.token_count,
            .pos = 0,
            .output = .{},
            .last_emit_end = 0,
        };

        const output = transpiler.transpile() catch return null;
        return output;
    }
};

/// Thread-local buffer transpiler for parallel processing
threadlocal var tl_buffer_transpiler: ?BufferTranspiler = null;

/// Get or create thread-local buffer transpiler
pub fn getThreadLocalTranspiler() *BufferTranspiler {
    if (tl_buffer_transpiler == null) {
        tl_buffer_transpiler = BufferTranspiler.init();
    }
    return &tl_buffer_transpiler.?;
}

/// Parallel transpile multiple files
/// Returns list of (input_index, output) pairs
pub fn parallelTranspile(
    allocator: std.mem.Allocator,
    sources: []const []const u8,
) ![]struct { index: usize, output: []const u8 } {
    const Result = struct { index: usize, output: []const u8 };
    var results = std.ArrayList(Result).init(allocator);
    errdefer results.deinit();

    // For now, process sequentially but use arena for each file
    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();

    for (sources, 0..) |source, i| {
        if (try arenaTranspile(&arena, source)) |output| {
            // Copy output to results allocator
            const output_copy = try allocator.dupe(u8, output);
            try results.append(.{ .index = i, .output = output_copy });
        }
        // Reset arena for next file
        _ = arena.reset(.retain_capacity);
    }

    return try results.toOwnedSlice();
}

/// Legacy simple check (for backwards compatibility)
pub fn canFastTranspile(source: []const u8) bool {
    _ = source;
    // Now we handle most TypeScript constructs
    return true;
}

// ============================================================================
// Tests
// ============================================================================

test "simple const to var" {
    const allocator = std.testing.allocator;
    var result = (try fastTranspile(allocator, "const x = 1;")).?;
    defer result.deinit();
    try std.testing.expectEqualStrings("const x = 1;", result.output);
}

test "const with type annotation" {
    const allocator = std.testing.allocator;
    var result = (try fastTranspile(allocator, "const x: number = 1;")).?;
    defer result.deinit();
    try std.testing.expectEqualStrings("const x = 1;", result.output);
}

test "interface removal" {
    const allocator = std.testing.allocator;
    var result = (try fastTranspile(allocator, "interface Foo { x: number; }")).?;
    defer result.deinit();
    try std.testing.expectEqualStrings("", result.output);
}

test "type alias removal" {
    const allocator = std.testing.allocator;
    var result = (try fastTranspile(allocator, "type Foo = string | number;")).?;
    defer result.deinit();
    try std.testing.expectEqualStrings("", result.output);
}

test "function with types" {
    const allocator = std.testing.allocator;
    var result = (try fastTranspile(allocator, "function add(a: number, b: number): number { return a + b; }")).?;
    defer result.deinit();
    try std.testing.expectEqualStrings("function add(a, b) { return a + b; }", result.output);
}

test "arrow function" {
    const allocator = std.testing.allocator;
    var result = (try fastTranspile(allocator, "const f = (x: number): number => x * 2;")).?;
    defer result.deinit();
    try std.testing.expectEqualStrings("const f = (x) => x * 2;", result.output);
}

test "import type removal" {
    const allocator = std.testing.allocator;
    var result = (try fastTranspile(allocator, "import type { Foo } from './foo';")).?;
    defer result.deinit();
    try std.testing.expectEqualStrings("", result.output);
}

test "regular import passthrough" {
    const allocator = std.testing.allocator;
    var result = (try fastTranspile(allocator, "import { foo } from './foo';")).?;
    defer result.deinit();
    try std.testing.expectEqualStrings("import { foo } from './foo';", result.output);
}

test "class with types" {
    const allocator = std.testing.allocator;
    var result = (try fastTranspile(allocator, "class Foo { x: number; constructor(x: number) { this.x = x; } }")).?;
    defer result.deinit();
    // Access modifiers and type annotations removed
    try std.testing.expect(std.mem.indexOf(u8, result.output, ": number") == null);
}

test "generic type parameters" {
    const allocator = std.testing.allocator;
    var result = (try fastTranspile(allocator, "function identity<T>(x: T): T { return x; }")).?;
    defer result.deinit();
    try std.testing.expectEqualStrings("function identity(x) { return x; }", result.output);
}

test "export interface removal" {
    const allocator = std.testing.allocator;
    var result = (try fastTranspile(allocator, "export interface Foo { x: number; }")).?;
    defer result.deinit();
    try std.testing.expectEqualStrings("export ", result.output);
}
