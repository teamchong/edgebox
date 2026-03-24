// Zig Native TypeScript Parser — Core
//
// Recursive descent parser producing a flat binary AST.
// Each node is 20 bytes (kind, flags, start, end, parent, first_child, next_sibling).
// The flat AST is then materialized into V8 Node objects lazily.

const std = @import("std");
const Scanner = @import("zig_scanner.zig").Scanner;
const SK = @import("zig_scanner.zig").SK;
const TokenKind = @import("zig_scanner.zig").TokenKind;

// Node kinds that are AST nodes (not tokens)
// SyntaxKind values — MUST match TSC 5.9 exactly (verified via ts.SyntaxKind)
pub const NK = struct {
    pub const SourceFile: u16 = 308;
    pub const Block: u16 = 242;
    pub const VariableStatement: u16 = 244;
    pub const VariableDeclarationList: u16 = 262;
    pub const VariableDeclaration: u16 = 261;
    pub const ExpressionStatement: u16 = 245;
    pub const IfStatement: u16 = 246;
    pub const ReturnStatement: u16 = 254;
    pub const FunctionDeclaration: u16 = 263;
    pub const ClassDeclaration: u16 = 264;
    pub const InterfaceDeclaration: u16 = 265;
    pub const TypeAliasDeclaration: u16 = 266;
    pub const EnumDeclaration: u16 = 267;
    pub const ImportDeclaration: u16 = 273;
    pub const ExportDeclaration: u16 = 279;
    pub const ExportAssignment: u16 = 278;
    // Expressions
    pub const Identifier: u16 = 80;
    pub const StringLiteral: u16 = 11;
    pub const NumericLiteral: u16 = 9;
    pub const CallExpression: u16 = 214;
    pub const PropertyAccessExpression: u16 = 212;
    pub const ElementAccessExpression: u16 = 213;
    pub const BinaryExpression: u16 = 227;
    pub const ArrowFunction: u16 = 220;
    pub const ObjectLiteralExpression: u16 = 211;
    pub const ArrayLiteralExpression: u16 = 210;
    pub const ParenthesizedExpression: u16 = 218;
    pub const PrefixUnaryExpression: u16 = 225;
    pub const TemplateExpression: u16 = 229;
    // Types
    pub const TypeReference: u16 = 184;
    pub const UnionType: u16 = 193;
    pub const ArrayType: u16 = 189;
    pub const LiteralType: u16 = 202;
    pub const FunctionType: u16 = 185;
    pub const Parameter: u16 = 170;
    pub const PropertySignature: u16 = 172;
};

// Flat AST node — 24 bytes, cache-friendly, zero GC.
// MUST be extern struct to guarantee field order (Zig reorders non-extern structs!)
pub const FlatNode = extern struct {
    kind: u16,
    flags: u16,
    start: u32,
    end: u32,
    parent: u32,
    first_child: u32,
    next_sibling: u32,
};

pub const AST = struct {
    nodes: std.ArrayListUnmanaged(FlatNode),
    alloc: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator) AST {
        return .{ .nodes = .{}, .alloc = allocator };
    }

    pub fn deinit(self: *AST) void {
        self.nodes.deinit(self.alloc);
    }

    pub fn addNode(self: *AST, kind: u16, start: u32, end: u32, parent: u32) !u32 {
        const idx: u32 = @intCast(self.nodes.items.len);
        try self.nodes.append(self.alloc, .{
            .kind = kind,
            .flags = 0,
            .start = start,
            .end = end,
            .parent = parent,
            .first_child = 0xFFFFFFFF, // no children
            .next_sibling = 0xFFFFFFFF, // no sibling
        });
        return idx;
    }

    pub fn addChild(self: *AST, parent: u32, child: u32) void {
        self.nodes.items[child].parent = parent;
        var node = &self.nodes.items[parent];
        if (node.first_child == 0xFFFFFFFF) {
            node.first_child = child;
        } else {
            // Find last child and append
            var last = node.first_child;
            while (self.nodes.items[last].next_sibling != 0xFFFFFFFF) {
                last = self.nodes.items[last].next_sibling;
            }
            self.nodes.items[last].next_sibling = child;
        }
    }

    pub fn setEnd(self: *AST, idx: u32, end: u32) void {
        self.nodes.items[idx].end = end;
    }
};

pub const Parser = struct {
    scanner: Scanner,
    ast: AST,
    current_token: TokenKind,

    pub fn init(source: []const u8, allocator: std.mem.Allocator) Parser {
        var p = Parser{
            .scanner = Scanner.init(source),
            .ast = AST.init(allocator),
            .current_token = SK.Unknown,
        };
        p.nextToken();
        return p;
    }

    pub fn deinit(self: *Parser) void {
        self.ast.deinit();
    }

    fn nextToken(self: *Parser) void {
        const tok = self.scanner.scan();
        self.current_token = tok.kind;
    }

    fn expect(self: *Parser, kind: TokenKind) void {
        if (self.current_token == kind) {
            self.nextToken();
        }
        // On mismatch: skip (error recovery)
    }

    fn pos(self: *Parser) u32 {
        return self.scanner.token.start;
    }

    fn endPos(self: *Parser) u32 {
        return self.scanner.token.end;
    }

    // ── Top level ──

    const ParseError = error{OutOfMemory};

    pub fn parseSourceFile(self: *Parser) ParseError!u32 {
        const start = self.pos();
        const sf = try self.ast.addNode(NK.SourceFile, start, 0, 0xFFFFFFFF);

        while (self.current_token != SK.EndOfFile) {
            const prev_pos = self.scanner.pos;
            const stmt = self.parseStatement() catch {
                // Error recovery: skip token
                self.nextToken();
                continue;
            };
            self.ast.addChild(sf, stmt);
            // Safety: if no progress was made, force advance to prevent infinite loop
            if (self.scanner.pos == prev_pos) self.nextToken();
        }

        self.ast.setEnd(sf, self.pos());
        return sf;
    }

    // ── Statements ──

    fn parseStatement(self: *Parser) ParseError!u32 {
        return switch (self.current_token) {
            SK.OpenBrace => self.parseBlock(),
            87 => self.parseVariableStatement(), // const
            115 => self.parseVariableStatement(), // var
            121 => self.parseVariableStatement(), // let
            100 => self.parseFunctionDeclaration(), // function
            86 => self.parseClassDeclaration(), // class
            120 => self.parseInterfaceDeclaration(), // interface
            156 => self.parseTypeAliasDeclaration(), // type
            94 => self.parseEnumDeclaration(), // enum
            102 => self.parseImportDeclaration(), // import
            95 => self.parseExportDeclaration(), // export
            101 => self.parseIfStatement(), // if
            107 => self.parseReturnStatement(), // return
            99 => self.parseForStatement(), // for
            117 => self.parseWhileStatement(), // while
            92 => self.parseDoWhileStatement(), // do
            109 => self.parseSwitchStatement(), // switch
            113 => self.parseTryCatchStatement(), // try
            111 => self.parseThrowStatement(), // throw
            88 => blk: { self.nextToken(); if (self.current_token == SK.Semicolon) self.nextToken(); break :blk try self.ast.addNode(252, self.pos(), self.endPos(), 0); }, // continue
            83 => blk: { self.nextToken(); if (self.current_token == SK.Semicolon) self.nextToken(); break :blk try self.ast.addNode(253, self.pos(), self.endPos(), 0); }, // break
            89 => blk: { self.nextToken(); if (self.current_token == SK.Semicolon) self.nextToken(); break :blk try self.ast.addNode(260, self.pos(), self.endPos(), 0); }, // debugger
            128, 134, 138 => blk: { // abstract, async, declare — modifiers before declaration
                self.nextToken();
                break :blk try self.parseStatement();
            },
            else => self.parseExpressionStatement(),
        };
    }

    fn parseBlock(self: *Parser) ParseError!u32 {
        const start = self.pos();
        const block = try self.ast.addNode(NK.Block, start, 0, 0);
        self.expect(SK.OpenBrace);
        while (self.current_token != SK.CloseBrace and self.current_token != SK.EndOfFile) {
            const stmt = self.parseStatement() catch { self.nextToken(); continue; };
            self.ast.addChild(block, stmt);
        }
        self.expect(SK.CloseBrace);
        self.ast.setEnd(block, self.endPos());
        return block;
    }

    fn parseVariableStatement(self: *Parser) ParseError!u32 {
        const start = self.pos();
        const stmt = try self.ast.addNode(NK.VariableStatement, start, 0, 0);
        // Set NodeFlags: Const=2, Let=1, Var=0 (TSC uses these on VariableDeclarationList)
        const kw = self.current_token;
        if (kw == 87) self.ast.nodes.items[stmt].flags = 2 // const → NodeFlags.Const
        else if (kw == 121) self.ast.nodes.items[stmt].flags = 1; // let → NodeFlags.Let
        self.nextToken(); // skip const/var/let
        // Parse declaration list
        const decl = try self.parseVariableDeclaration();
        self.ast.addChild(stmt, decl);
        while (self.current_token == SK.Comma) {
            self.nextToken();
            const d2 = try self.parseVariableDeclaration();
            self.ast.addChild(stmt, d2);
        }
        if (self.current_token == SK.Semicolon) self.nextToken();
        self.ast.setEnd(stmt, self.endPos());
        return stmt;
    }

    fn parseVariableDeclaration(self: *Parser) ParseError!u32 {
        const start = self.pos();
        const decl = try self.ast.addNode(NK.VariableDeclaration, start, 0, 0);
        // Name (can be identifier or destructuring pattern)
        if (self.current_token == SK.Identifier) {
            const name = try self.ast.addNode(NK.Identifier, self.pos(), self.endPos(), 0);
            self.ast.addChild(decl, name);
            self.nextToken();
        } else if (self.current_token == SK.OpenBrace) {
            self.skipBraced(); // object destructuring (simplified)
        } else if (self.current_token == SK.OpenBracket) {
            self.skipBracketed(); // array destructuring (simplified)
        }
        // Optional type annotation — creates real AST node
        if (self.current_token == SK.Colon) {
            self.nextToken();
            const type_node = try self.parseTypeNode();
            self.ast.addChild(decl, type_node);
        }
        // Optional initializer
        if (self.current_token == SK.Equals) {
            self.nextToken();
            const init_expr = try self.parseExpression();
            self.ast.addChild(decl, init_expr);
        }
        self.ast.setEnd(decl, self.endPos());
        return decl;
    }

    fn parseExpressionStatement(self: *Parser) ParseError!u32 {
        const start = self.pos();
        const stmt = try self.ast.addNode(NK.ExpressionStatement, start, 0, 0);
        const expr = try self.parseExpression();
        self.ast.addChild(stmt, expr);
        if (self.current_token == SK.Semicolon) self.nextToken();
        self.ast.setEnd(stmt, self.endPos());
        return stmt;
    }

    fn parseIfStatement(self: *Parser) ParseError!u32 {
        const start = self.pos();
        const stmt = try self.ast.addNode(NK.IfStatement, start, 0, 0);
        self.nextToken(); // skip 'if'
        self.expect(SK.OpenParen);
        const cond = try self.parseExpression();
        self.ast.addChild(stmt, cond);
        self.expect(SK.CloseParen);
        const then_stmt = try self.parseStatement();
        self.ast.addChild(stmt, then_stmt);
        if (self.current_token == 93) { // else
            self.nextToken();
            const else_stmt = try self.parseStatement();
            self.ast.addChild(stmt, else_stmt);
        }
        self.ast.setEnd(stmt, self.endPos());
        return stmt;
    }

    fn parseReturnStatement(self: *Parser) ParseError!u32 {
        const start = self.pos();
        const stmt = try self.ast.addNode(NK.ReturnStatement, start, 0, 0);
        self.nextToken(); // skip 'return'
        if (self.current_token != SK.Semicolon and self.current_token != SK.CloseBrace and self.current_token != SK.EndOfFile) {
            const expr = try self.parseExpression();
            self.ast.addChild(stmt, expr);
        }
        if (self.current_token == SK.Semicolon) self.nextToken();
        self.ast.setEnd(stmt, self.endPos());
        return stmt;
    }

    fn parseForStatement(self: *Parser) ParseError!u32 {
        const start = self.pos();
        const stmt = try self.ast.addNode(249, start, 0, 0); // ForStatement
        self.nextToken(); // skip 'for'
        self.skipParenthesized();
        const body = try self.parseStatement();
        self.ast.addChild(stmt, body);
        self.ast.setEnd(stmt, self.endPos());
        return stmt;
    }

    fn parseWhileStatement(self: *Parser) ParseError!u32 {
        const start = self.pos();
        const stmt = try self.ast.addNode(248, start, 0, 0); // WhileStatement
        self.nextToken(); // skip 'while'
        self.expect(SK.OpenParen);
        const cond = try self.parseExpression();
        self.ast.addChild(stmt, cond);
        self.expect(SK.CloseParen);
        const body = try self.parseStatement();
        self.ast.addChild(stmt, body);
        self.ast.setEnd(stmt, self.endPos());
        return stmt;
    }

    fn parseDoWhileStatement(self: *Parser) ParseError!u32 {
        const start = self.pos();
        const stmt = try self.ast.addNode(247, start, 0, 0); // DoStatement
        self.nextToken(); // skip 'do'
        const body = try self.parseStatement();
        self.ast.addChild(stmt, body);
        if (self.current_token == 117) { // while
            self.nextToken();
            self.skipParenthesized();
        }
        if (self.current_token == SK.Semicolon) self.nextToken();
        self.ast.setEnd(stmt, self.endPos());
        return stmt;
    }

    fn parseSwitchStatement(self: *Parser) ParseError!u32 {
        const start = self.pos();
        const stmt = try self.ast.addNode(256, start, 0, 0); // SwitchStatement
        self.nextToken(); // skip 'switch'
        self.skipParenthesized();
        if (self.current_token == SK.OpenBrace) {
            self.skipBraced(); // simplified: skip switch body
        }
        self.ast.setEnd(stmt, self.endPos());
        return stmt;
    }

    fn parseTryCatchStatement(self: *Parser) ParseError!u32 {
        const start = self.pos();
        const stmt = try self.ast.addNode(259, start, 0, 0); // TryStatement
        self.nextToken(); // skip 'try'
        if (self.current_token == SK.OpenBrace) {
            const body = try self.parseBlock();
            self.ast.addChild(stmt, body);
        }
        if (self.current_token == 85) { // catch
            self.nextToken();
            if (self.current_token == SK.OpenParen) self.skipParenthesized();
            if (self.current_token == SK.OpenBrace) {
                const catchBody = try self.parseBlock();
                self.ast.addChild(stmt, catchBody);
            }
        }
        if (self.current_token == 98) { // finally
            self.nextToken();
            if (self.current_token == SK.OpenBrace) {
                const finallyBody = try self.parseBlock();
                self.ast.addChild(stmt, finallyBody);
            }
        }
        self.ast.setEnd(stmt, self.endPos());
        return stmt;
    }

    fn parseThrowStatement(self: *Parser) ParseError!u32 {
        const start = self.pos();
        const stmt = try self.ast.addNode(258, start, 0, 0); // ThrowStatement
        self.nextToken(); // skip 'throw'
        const expr = try self.parseExpression();
        self.ast.addChild(stmt, expr);
        if (self.current_token == SK.Semicolon) self.nextToken();
        self.ast.setEnd(stmt, self.endPos());
        return stmt;
    }

    fn parseFunctionDeclaration(self: *Parser) ParseError!u32 {
        const start = self.pos();
        const decl = try self.ast.addNode(NK.FunctionDeclaration, start, 0, 0);
        // Skip asterisk for generators
        if (self.current_token == 42) self.nextToken(); // *
        self.nextToken(); // skip 'function'
        // Name (can be identifier or contextual keyword like get/set/number/string)
        if (self.current_token == SK.Identifier or
            (self.current_token >= 133 and self.current_token <= 165)) {
            const name = try self.ast.addNode(NK.Identifier, self.pos(), self.endPos(), 0);
            self.ast.addChild(decl, name);
            self.nextToken();
        }
        // Type parameters <T>
        if (self.current_token == SK.LessThan) self.skipAngleBracketed();
        // Parameters
        try self.parseParameterList(decl);
        // Return type
        if (self.current_token == SK.Colon) {
            self.nextToken();
            const ret_type = try self.parseTypeNode();
            self.ast.addChild(decl, ret_type);
        }
        // Body
        if (self.current_token == SK.OpenBrace) {
            const body = try self.parseBlock();
            self.ast.addChild(decl, body);
        }
        self.ast.setEnd(decl, self.endPos());
        return decl;
    }

    fn parseParameterList(self: *Parser, parent: u32) ParseError!void {
        if (self.current_token != SK.OpenParen) return;
        self.nextToken(); // skip (
        while (self.current_token != SK.CloseParen and self.current_token != SK.EndOfFile) {
            const loop_start_pos = self.scanner.pos;
            // Skip modifiers (public, private, readonly, etc.)
            while (self.current_token == 123 or self.current_token == 124 or
                   self.current_token == 125 or self.current_token == 148 or
                   self.current_token == 126) self.nextToken();
            // Rest parameter
            if (self.current_token == SK.DotDotDot) self.nextToken();
            const param_start = self.pos();
            const param = try self.ast.addNode(NK.Parameter, param_start, 0, 0);
            // Name (identifier, `this`, or contextual keywords like `number`, `string`, etc.)
            // TypeScript allows type keywords (133-165) as parameter names.
            if (self.current_token == SK.Identifier or self.current_token == 110 or
                (self.current_token >= 133 and self.current_token <= 165)) {
                const name = try self.ast.addNode(NK.Identifier, self.pos(), self.endPos(), 0);
                self.ast.addChild(param, name);
                self.nextToken();
            } else if (self.current_token == SK.OpenBrace) {
                self.skipBraced();
            } else if (self.current_token == SK.OpenBracket) {
                self.skipBracketed();
            }
            // Optional marker ?
            if (self.current_token == SK.Question) self.nextToken();
            // Type annotation
            if (self.current_token == SK.Colon) {
                self.nextToken();
                const type_node = try self.parseTypeNode();
                self.ast.addChild(param, type_node);
            }
            // Default value
            if (self.current_token == SK.Equals) {
                self.nextToken();
                const default_val = try self.parseAssignmentExpression();
                self.ast.addChild(param, default_val);
            }
            self.ast.setEnd(param, self.endPos());
            self.ast.addChild(parent, param);
            if (self.current_token == SK.Comma) self.nextToken();
            // Safety: if we didn't consume any tokens, force advance to prevent infinite loop
            if (self.scanner.pos == loop_start_pos) self.nextToken();
        }
        if (self.current_token == SK.CloseParen) self.nextToken();
    }

    fn parseClassDeclaration(self: *Parser) ParseError!u32 {
        const start = self.pos();
        const decl = try self.ast.addNode(NK.ClassDeclaration, start, 0, 0);
        self.nextToken(); // skip 'class'
        if (self.current_token == SK.Identifier) {
            const name = try self.ast.addNode(NK.Identifier, self.pos(), self.endPos(), 0);
            self.ast.addChild(decl, name);
            self.nextToken();
        }
        // Skip extends/implements/type params
        while (self.current_token != SK.OpenBrace and self.current_token != SK.EndOfFile) self.nextToken();
        // Skip class body entirely — don't parse as Block (causes checker hang).
        // Bridge sets members = [] (empty). TSC will find no members on the class.
        if (self.current_token == SK.OpenBrace) self.skipBraced();
        self.ast.setEnd(decl, self.endPos());
        return decl;
    }

    fn parseInterfaceDeclaration(self: *Parser) ParseError!u32 {
        const start = self.pos();
        const decl = try self.ast.addNode(NK.InterfaceDeclaration, start, 0, 0);
        self.nextToken(); // skip 'interface'
        if (self.current_token == SK.Identifier) {
            const name = try self.ast.addNode(NK.Identifier, self.pos(), self.endPos(), 0);
            self.ast.addChild(decl, name);
            self.nextToken();
        }
        // Skip type parameters and extends clause
        if (self.current_token == SK.LessThan) self.skipAngleBracketed();
        while (self.current_token != SK.OpenBrace and self.current_token != SK.EndOfFile) self.nextToken();
        // Parse interface body: property/method signatures
        if (self.current_token == SK.OpenBrace) {
            self.nextToken(); // skip {
            while (self.current_token != SK.CloseBrace and self.current_token != SK.EndOfFile) {
                const member_start = self.pos();
                // Skip modifiers (readonly, static, etc.)
                if (self.current_token == 148) self.nextToken(); // readonly
                // Property or method signature
                if (self.current_token == SK.Identifier or self.current_token == SK.StringLiteral or
                    self.current_token == SK.OpenBracket) // computed/index signature
                {
                    const sig = try self.ast.addNode(NK.PropertySignature, member_start, 0, 0);
                    if (self.current_token == SK.OpenBracket) {
                        self.skipBracketed(); // index signature [key: string]
                    } else {
                        const mname = try self.ast.addNode(NK.Identifier, self.pos(), self.endPos(), 0);
                        self.ast.addChild(sig, mname);
                        self.nextToken();
                    }
                    // Optional ?
                    if (self.current_token == SK.Question) self.nextToken();
                    // Type annotation
                    if (self.current_token == SK.Colon) {
                        self.nextToken();
                        const mtype = try self.parseTypeNode();
                        self.ast.addChild(sig, mtype);
                    }
                    // Method signature: name(...): Type
                    if (self.current_token == SK.OpenParen) {
                        self.skipParenthesized();
                        if (self.current_token == SK.Colon) {
                            self.nextToken();
                            const rtype = try self.parseTypeNode();
                            self.ast.addChild(sig, rtype);
                        }
                    }
                    self.ast.setEnd(sig, self.endPos());
                    self.ast.addChild(decl, sig);
                    if (self.current_token == SK.Semicolon or self.current_token == SK.Comma) self.nextToken();
                } else {
                    // Skip unknown member
                    self.nextToken();
                }
            }
            if (self.current_token == SK.CloseBrace) self.nextToken();
        }
        self.ast.setEnd(decl, self.endPos());
        return decl;
    }

    fn parseTypeAliasDeclaration(self: *Parser) ParseError!u32 {
        const start = self.pos();
        const decl = try self.ast.addNode(NK.TypeAliasDeclaration, start, 0, 0);
        self.nextToken(); // skip 'type'
        if (self.current_token == SK.Identifier) {
            const name = try self.ast.addNode(NK.Identifier, self.pos(), self.endPos(), 0);
            self.ast.addChild(decl, name);
            self.nextToken();
        }
        // Skip to semicolon or next statement
        while (self.current_token != SK.Semicolon and self.current_token != SK.EndOfFile) {
            if (self.current_token == SK.OpenBrace) { self.skipBraced(); continue; }
            self.nextToken();
        }
        if (self.current_token == SK.Semicolon) self.nextToken();
        self.ast.setEnd(decl, self.endPos());
        return decl;
    }

    fn parseEnumDeclaration(self: *Parser) ParseError!u32 {
        const start = self.pos();
        const decl = try self.ast.addNode(NK.EnumDeclaration, start, 0, 0);
        self.nextToken(); // skip 'enum'
        if (self.current_token == SK.Identifier) self.nextToken();
        if (self.current_token == SK.OpenBrace) self.skipBraced();
        self.ast.setEnd(decl, self.endPos());
        return decl;
    }

    fn parseImportDeclaration(self: *Parser) ParseError!u32 {
        const start = self.pos();
        const decl = try self.ast.addNode(NK.ImportDeclaration, start, 0, 0);
        self.nextToken(); // skip 'import'
        // import 'module' (side-effect import)
        if (self.current_token == SK.StringLiteral) {
            const spec = try self.ast.addNode(NK.StringLiteral, self.pos(), self.endPos(), 0);
            self.ast.addChild(decl, spec);
            self.nextToken();
            if (self.current_token == SK.Semicolon) self.nextToken();
            self.ast.setEnd(decl, self.endPos());
            return decl;
        }
        // import type ...
        if (self.current_token == 156) self.nextToken(); // skip 'type'
        // import { ... } from '...' OR import X from '...' OR import * as X from '...'
        if (self.current_token == SK.OpenBrace) {
            self.skipBraced();
        } else if (self.current_token == SK.Asterisk) {
            self.nextToken(); // *
            if (self.current_token == 130) self.nextToken(); // as
            if (self.current_token == SK.Identifier) self.nextToken(); // name
        } else if (self.current_token == SK.Identifier) {
            self.nextToken(); // default import name
            if (self.current_token == SK.Comma) {
                self.nextToken();
                if (self.current_token == SK.OpenBrace) self.skipBraced();
                if (self.current_token == SK.Asterisk) {
                    self.nextToken();
                    if (self.current_token == 130) self.nextToken();
                    if (self.current_token == SK.Identifier) self.nextToken();
                }
            }
        }
        // from 'module-specifier'
        if (self.current_token == 161) self.nextToken(); // from
        if (self.current_token == SK.StringLiteral) {
            const spec = try self.ast.addNode(NK.StringLiteral, self.pos(), self.endPos(), 0);
            self.ast.addChild(decl, spec);
            self.nextToken();
        }
        if (self.current_token == SK.Semicolon) self.nextToken();
        self.ast.setEnd(decl, self.endPos());
        return decl;
    }

    fn parseExportDeclaration(self: *Parser) ParseError!u32 {
        const start = self.pos();
        self.nextToken(); // skip 'export'
        // 'export default', 'export class', 'export function', etc.
        return switch (self.current_token) {
            86 => self.parseClassDeclaration(), // class
            100 => self.parseFunctionDeclaration(), // function
            87, 115, 121 => self.parseVariableStatement(), // const/var/let
            120 => self.parseInterfaceDeclaration(), // interface
            156 => self.parseTypeAliasDeclaration(), // type
            94 => self.parseEnumDeclaration(), // enum
            else => blk: {
                // export { ... } or export default ...
                const decl = try self.ast.addNode(NK.ExportDeclaration, start, 0, 0);
                while (self.current_token != SK.Semicolon and self.current_token != SK.EndOfFile) {
                    if (self.current_token == SK.OpenBrace) { self.skipBraced(); continue; }
                    self.nextToken();
                }
                if (self.current_token == SK.Semicolon) self.nextToken();
                self.ast.setEnd(decl, self.endPos());
                break :blk decl;
            },
        };
    }

    // ── Expressions (Pratt parser with operator precedence) ──

    fn parseExpression(self: *Parser) ParseError!u32 {
        return self.parseAssignmentExpression();
    }

    fn parseAssignmentExpression(self: *Parser) ParseError!u32 {
        var left = try self.parseTernaryExpression();
        // Assignment operators: =, +=, -=, etc.
        if (isAssignmentOp(self.current_token)) {
            const start = self.ast.nodes.items[left].start;
            const bin = try self.ast.addNode(NK.BinaryExpression, start, 0, 0);
            self.ast.addChild(bin, left);
            self.nextToken(); // skip operator
            const right = try self.parseAssignmentExpression(); // right-associative
            self.ast.addChild(bin, right);
            self.ast.setEnd(bin, self.ast.nodes.items[right].end);
            left = bin;
        }
        return left;
    }

    fn parseTernaryExpression(self: *Parser) ParseError!u32 {
        const left = try self.parseBinaryExpression(0);
        if (self.current_token == SK.Question) {
            self.nextToken();
            _ = try self.parseAssignmentExpression(); // consequent (skip for now)
            if (self.current_token == SK.Colon) self.nextToken();
            _ = try self.parseAssignmentExpression(); // alternate
        }
        return left;
    }

    fn parseBinaryExpression(self: *Parser, min_prec: u8) ParseError!u32 {
        var left = try self.parseUnaryExpression();
        while (true) {
            const prec = binaryPrecedence(self.current_token);
            if (prec == 0 or prec <= min_prec) break;
            const start = self.ast.nodes.items[left].start;
            const bin = try self.ast.addNode(NK.BinaryExpression, start, 0, 0);
            self.ast.addChild(bin, left);
            self.nextToken(); // skip operator
            const right = try self.parseBinaryExpression(prec); // left-associative
            self.ast.addChild(bin, right);
            self.ast.setEnd(bin, self.ast.nodes.items[right].end);
            left = bin;
        }
        return left;
    }

    fn parseUnaryExpression(self: *Parser) ParseError!u32 {
        // Prefix: !, ~, -, +, typeof, void, delete, ++, --, await
        if (isUnaryPrefix(self.current_token)) {
            const start = self.pos();
            const node = try self.ast.addNode(225, start, 0, 0); // PrefixUnaryExpression
            self.nextToken();
            const operand = try self.parseUnaryExpression();
            self.ast.addChild(node, operand);
            self.ast.setEnd(node, self.ast.nodes.items[operand].end);
            return node;
        }
        return self.parsePostfixPrimary();
    }

    fn parsePostfixPrimary(self: *Parser) ParseError!u32 {
        var expr = try self.parsePrimaryExpression();
        expr = try self.parsePostfixExpression(expr);
        // Postfix ++/--
        if (self.current_token == SK.PlusPlus or self.current_token == SK.MinusMinus) {
            self.nextToken();
        }
        // 'as' type assertion
        if (self.current_token == 130) { // as
            self.nextToken();
            self.skipTypeAnnotation();
        }
        // Non-null assertion !
        if (self.current_token == SK.Exclamation) {
            self.nextToken();
        }
        return expr;
    }

    fn isAssignmentOp(kind: TokenKind) bool {
        return kind >= 64 and kind <= 79; // EqualsToken through CaretEqualsToken
    }

    fn isUnaryPrefix(kind: TokenKind) bool {
        return kind == SK.Exclamation or kind == SK.Tilde or kind == SK.Minus or
               kind == SK.Plus or kind == 114 or kind == 116 or kind == 91 or // typeof, void, delete
               kind == SK.PlusPlus or kind == SK.MinusMinus or kind == 135; // await
    }

    fn binaryPrecedence(kind: TokenKind) u8 {
        return switch (kind) {
            SK.BarBar => 1,
            SK.AmpersandAmpersand => 2,
            SK.Bar => 3,
            SK.Caret => 4,
            SK.Ampersand => 5,
            SK.EqualEqual, SK.ExclEqual, SK.EqualEqualEqual, SK.ExclEqualEqual => 6,
            SK.LessThan, SK.GreaterThan, SK.LessEqual, SK.GreaterEqual, 104, 103 => 7, // instanceof, in
            SK.LessLess, SK.GreaterGreater, SK.GreaterGreaterGreater => 8,
            SK.Plus, SK.Minus => 9,
            SK.Asterisk, SK.Slash, SK.Percent => 10,
            SK.AsteriskAsterisk => 11,
            61 => 1, // ??
            else => 0,
        };
    }

    fn parsePrimaryExpression(self: *Parser) ParseError!u32 {
        const start = self.pos();
        return switch (self.current_token) {
            SK.Identifier, 110, 108 => blk: { // identifier, this, super
                const node = try self.ast.addNode(NK.Identifier, start, self.endPos(), 0);
                self.nextToken();
                // Arrow function: identifier => expr
                if (self.current_token == SK.Arrow) {
                    self.nextToken();
                    const arrow = try self.ast.addNode(NK.ArrowFunction, start, 0, 0);
                    self.ast.addChild(arrow, node); // param
                    if (self.current_token == SK.OpenBrace) {
                        const body = try self.parseBlock();
                        self.ast.addChild(arrow, body);
                    } else {
                        const body = try self.parseAssignmentExpression();
                        self.ast.addChild(arrow, body);
                    }
                    self.ast.setEnd(arrow, self.endPos());
                    break :blk arrow;
                }
                break :blk node;
            },
            SK.StringLiteral => blk: {
                const node = try self.ast.addNode(NK.StringLiteral, start, self.endPos(), 0);
                self.nextToken();
                break :blk node;
            },
            SK.NumericLiteral, 10 => blk: { // number, bigint
                const node = try self.ast.addNode(NK.NumericLiteral, start, self.endPos(), 0);
                self.nextToken();
                break :blk node;
            },
            97 => blk: { // false
                const node = try self.ast.addNode(97, start, self.endPos(), 0);
                self.nextToken();
                break :blk node;
            },
            112 => blk: { // true
                const node = try self.ast.addNode(112, start, self.endPos(), 0);
                self.nextToken();
                break :blk node;
            },
            106 => blk: { // null
                const node = try self.ast.addNode(106, start, self.endPos(), 0);
                self.nextToken();
                break :blk node;
            },
            SK.NoSubTemplate, SK.TemplateHead => blk: {
                // Template literal
                const node = try self.ast.addNode(229, start, 0, 0); // TemplateExpression
                if (self.current_token == SK.NoSubTemplate) {
                    self.nextToken();
                    self.ast.setEnd(node, self.endPos());
                    break :blk node;
                }
                // TemplateHead — has expressions
                self.nextToken();
                while (self.current_token != SK.EndOfFile) {
                    _ = try self.parseExpression(); // expression in ${}
                    if (self.current_token == SK.CloseBrace) {
                        // Rescan as template middle/tail
                        const tmpl = self.scanner.scanTemplateMiddleOrTail();
                        self.current_token = tmpl.kind;
                        if (tmpl.kind == SK.TemplateTail) { self.nextToken(); break; }
                        self.nextToken(); // skip TemplateMiddle
                    } else break;
                }
                self.ast.setEnd(node, self.endPos());
                break :blk node;
            },
            SK.OpenParen => blk: {
                // Could be: (expr), arrow (params) => body
                const node = try self.ast.addNode(NK.ParenthesizedExpression, start, 0, 0);
                self.nextToken();
                if (self.current_token != SK.CloseParen) {
                    const inner = try self.parseExpression();
                    self.ast.addChild(node, inner);
                }
                // Consume remaining comma-separated items (for arrow params)
                while (self.current_token == SK.Comma) {
                    self.nextToken();
                    if (self.current_token != SK.CloseParen) {
                        _ = try self.parseExpression();
                    }
                }
                self.expect(SK.CloseParen);
                self.ast.setEnd(node, self.endPos());
                // Arrow function: (...) => body
                if (self.current_token == SK.Colon) { self.nextToken(); self.skipTypeAnnotation(); }
                if (self.current_token == SK.Arrow) {
                    self.nextToken();
                    const arrow = try self.ast.addNode(NK.ArrowFunction, start, 0, 0);
                    if (self.current_token == SK.OpenBrace) {
                        const body = try self.parseBlock();
                        self.ast.addChild(arrow, body);
                    } else {
                        const body = try self.parseAssignmentExpression();
                        self.ast.addChild(arrow, body);
                    }
                    self.ast.setEnd(arrow, self.endPos());
                    return arrow;
                }
                break :blk node;
            },
            SK.OpenBrace => blk: {
                const node = try self.ast.addNode(NK.ObjectLiteralExpression, start, 0, 0);
                self.skipBraced();
                self.ast.setEnd(node, self.endPos());
                break :blk node;
            },
            SK.OpenBracket => blk: {
                const node = try self.ast.addNode(NK.ArrayLiteralExpression, start, 0, 0);
                self.skipBracketed();
                self.ast.setEnd(node, self.endPos());
                break :blk node;
            },
            100 => blk: { // function expression
                break :blk try self.parseFunctionDeclaration();
            },
            86 => blk: { // class expression
                break :blk try self.parseClassDeclaration();
            },
            105 => blk: { // new
                self.nextToken();
                var expr = try self.parsePrimaryExpression();
                expr = try self.parsePostfixExpression(expr);
                break :blk expr;
            },
            else => blk: {
                const node = try self.ast.addNode(NK.Identifier, start, self.endPos(), 0);
                self.nextToken();
                break :blk node;
            },
        };
    }

    fn parsePostfixExpression(self: *Parser, left: u32) error{OutOfMemory}!u32 {
        var result = left;
        while (true) {
            switch (self.current_token) {
                SK.Dot => {
                    self.nextToken();
                    // After '.', any identifier or keyword can be a property name
                    if (self.current_token == SK.Identifier or
                        (self.current_token >= 81 and self.current_token <= 165)) {
                        const prop = try self.ast.addNode(NK.PropertyAccessExpression, self.ast.nodes.items[result].start, self.endPos(), 0);
                        self.ast.addChild(prop, result);
                        self.nextToken();
                        result = prop;
                    }
                },
                SK.OpenParen => {
                    const call = try self.ast.addNode(NK.CallExpression, self.ast.nodes.items[result].start, 0, 0);
                    self.ast.addChild(call, result);
                    self.skipParenthesized();
                    self.ast.setEnd(call, self.endPos());
                    result = call;
                },
                SK.OpenBracket => {
                    const elem = try self.ast.addNode(NK.ElementAccessExpression, self.ast.nodes.items[result].start, 0, 0);
                    self.ast.addChild(elem, result);
                    self.skipBracketed();
                    self.ast.setEnd(elem, self.endPos());
                    result = elem;
                },
                else => break,
            }
        }
        return result;
    }

    // ── Helpers ──

    /// Parse a type annotation and return the AST node.
    /// Handles: keyword types, type references, union types, array types,
    /// function types, literal types, tuple types, mapped types.
    fn parseTypeNode(self: *Parser) ParseError!u32 {
        const left = try self.parsePrimaryType();
        // Union: T | U | V
        if (self.current_token == SK.Bar) {
            const start = self.ast.nodes.items[left].start;
            const union_node = try self.ast.addNode(NK.UnionType, start, 0, 0);
            self.ast.addChild(union_node, left);
            while (self.current_token == SK.Bar) {
                self.nextToken();
                const member = try self.parsePrimaryType();
                self.ast.addChild(union_node, member);
            }
            self.ast.setEnd(union_node, self.endPos());
            return union_node;
        }
        // Intersection: T & U
        if (self.current_token == SK.Ampersand) {
            const start = self.ast.nodes.items[left].start;
            const inter = try self.ast.addNode(194, start, 0, 0); // IntersectionType
            self.ast.addChild(inter, left);
            while (self.current_token == SK.Ampersand) {
                self.nextToken();
                const member = try self.parsePrimaryType();
                self.ast.addChild(inter, member);
            }
            self.ast.setEnd(inter, self.endPos());
            return inter;
        }
        return left;
    }

    fn parsePrimaryType(self: *Parser) ParseError!u32 {
        const start = self.pos();
        var base: u32 = switch (self.current_token) {
            // Keyword types: string, number, boolean, any, void, never, etc.
            133, 136, 146, 150, 151, 154, 155, 157, 159, 116 => blk: { // any,boolean,never,number,object,string,symbol,undefined,unknown,void
                const node = try self.ast.addNode(self.current_token, start, self.endPos(), 0);
                self.nextToken();
                break :blk node;
            },
            // typeof type
            114 => blk: { // typeof
                self.nextToken();
                const operand = try self.parsePrimaryType();
                _ = operand;
                break :blk try self.ast.addNode(187, start, self.endPos(), 0); // TypeQuery
            },
            // Literal types: true, false, null, string/number literals
            97, 112, 106 => blk: { // false, true, null
                const lit = try self.ast.addNode(NK.LiteralType, start, self.endPos(), 0);
                self.nextToken();
                break :blk lit;
            },
            SK.StringLiteral, SK.NumericLiteral => blk: {
                const lit = try self.ast.addNode(NK.LiteralType, start, self.endPos(), 0);
                self.nextToken();
                break :blk lit;
            },
            // Parenthesized or function type
            SK.OpenParen => blk: {
                const node = try self.ast.addNode(NK.FunctionType, start, 0, 0);
                self.skipParenthesized();
                if (self.current_token == SK.Arrow) {
                    self.nextToken();
                    const ret = try self.parseTypeNode();
                    self.ast.addChild(node, ret);
                }
                self.ast.setEnd(node, self.endPos());
                break :blk node;
            },
            // Object type: { ... }
            SK.OpenBrace => blk: {
                const node = try self.ast.addNode(188, start, 0, 0); // TypeLiteral
                self.skipBraced();
                self.ast.setEnd(node, self.endPos());
                break :blk node;
            },
            // Tuple type: [T, U]
            SK.OpenBracket => blk: {
                const node = try self.ast.addNode(190, start, 0, 0); // TupleType
                self.skipBracketed();
                self.ast.setEnd(node, self.endPos());
                break :blk node;
            },
            // Type reference: Foo, Foo<T>, Promise<string>
            SK.Identifier, 102 => blk: { // Identifier or import (for import("..."))
                const ref = try self.ast.addNode(NK.TypeReference, start, 0, 0);
                const name = try self.ast.addNode(NK.Identifier, start, self.endPos(), 0);
                self.ast.addChild(ref, name);
                self.nextToken();
                // Qualified: Foo.Bar
                while (self.current_token == SK.Dot) {
                    self.nextToken();
                    if (self.current_token == SK.Identifier) self.nextToken();
                }
                // Generic args: Foo<T, U>
                if (self.current_token == SK.LessThan) {
                    self.skipAngleBracketed();
                }
                self.ast.setEnd(ref, self.endPos());
                break :blk ref;
            },
            // keyof T
            143 => blk: { // keyof
                self.nextToken();
                const operand = try self.parsePrimaryType();
                _ = operand;
                break :blk try self.ast.addNode(199, start, self.endPos(), 0); // TypeOperator
            },
            // readonly type
            148 => blk: {
                self.nextToken();
                break :blk try self.parsePrimaryType();
            },
            // infer T
            140 => blk: {
                self.nextToken();
                if (self.current_token == SK.Identifier) self.nextToken();
                break :blk try self.ast.addNode(196, start, self.endPos(), 0); // InferType
            },
            else => blk: {
                // Unknown type — treat as identifier
                const node = try self.ast.addNode(NK.Identifier, start, self.endPos(), 0);
                self.nextToken();
                break :blk node;
            },
        };
        // Array type suffix: T[]
        while (self.current_token == SK.OpenBracket and self.pos() < self.scanner.src.len) {
            if (self.scanner.src[self.pos()] == ']') {
                const arr = try self.ast.addNode(NK.ArrayType, self.ast.nodes.items[base].start, 0, 0);
                self.ast.addChild(arr, base);
                self.nextToken(); // [
                self.nextToken(); // ]
                self.ast.setEnd(arr, self.endPos());
                base = arr;
            } else break;
        }
        return base;
    }

    fn skipAngleBracketed(self: *Parser) void {
        if (self.current_token != SK.LessThan) return;
        var depth: u32 = 1;
        self.nextToken();
        while (depth > 0 and self.current_token != SK.EndOfFile) {
            if (self.current_token == SK.LessThan) depth += 1;
            if (self.current_token == SK.GreaterThan) depth -= 1;
            self.nextToken();
        }
    }

    fn skipTypeAnnotation(self: *Parser) void {
        // Legacy: skip type without creating nodes (used where type nodes aren't needed)
        var depth: u32 = 0;
        while (self.current_token != SK.EndOfFile) {
            if (self.current_token == SK.LessThan) { depth += 1; self.nextToken(); continue; }
            if (self.current_token == SK.GreaterThan and depth > 0) { depth -= 1; self.nextToken(); continue; }
            if (depth == 0 and (self.current_token == SK.Equals or self.current_token == SK.Semicolon or self.current_token == SK.Comma or self.current_token == SK.CloseParen or self.current_token == SK.CloseBrace or self.current_token == SK.OpenBrace or self.current_token == SK.Arrow)) break;
            if (self.current_token == SK.OpenBrace) { self.skipBraced(); continue; }
            if (self.current_token == SK.OpenParen) { self.skipParenthesized(); continue; }
            self.nextToken();
        }
    }

    fn skipParenthesized(self: *Parser) void {
        if (self.current_token != SK.OpenParen) return;
        var depth: u32 = 1;
        self.nextToken();
        while (depth > 0 and self.current_token != SK.EndOfFile) {
            if (self.current_token == SK.OpenParen) depth += 1;
            if (self.current_token == SK.CloseParen) depth -= 1;
            self.nextToken();
        }
    }

    fn skipBraced(self: *Parser) void {
        if (self.current_token != SK.OpenBrace) return;
        var depth: u32 = 1;
        self.nextToken();
        while (depth > 0 and self.current_token != SK.EndOfFile) {
            if (self.current_token == SK.OpenBrace) depth += 1;
            if (self.current_token == SK.CloseBrace) depth -= 1;
            self.nextToken();
        }
    }

    fn skipBracketed(self: *Parser) void {
        if (self.current_token != SK.OpenBracket) return;
        var depth: u32 = 1;
        self.nextToken();
        while (depth > 0 and self.current_token != SK.EndOfFile) {
            if (self.current_token == SK.OpenBracket) depth += 1;
            if (self.current_token == SK.CloseBracket) depth -= 1;
            self.nextToken();
        }
    }
};

// ── C ABI Export: parse a file, return flat AST buffer ──
// Called from V8 via edgebox_parse_file callback.
// Returns pointer to flat AST nodes, sets out_count to number of nodes.
// Caller must NOT free — buffer lives in Zig's allocator (persists for program lifetime).

const c_alloc = std.heap.page_allocator;

// Mutex to serialize parse calls across V8 worker threads.
// page_allocator (mmap) is thread-safe, but Parser internals (ArrayList
// grow/realloc) under simultaneous pressure from 3+ threads causes hangs.
// Serializing is fine: total Zig parse time is ~32ms for all files.
var parse_mutex: std.Thread.Mutex = .{};

export fn edgebox_zig_parse(
    src_ptr: [*]const u8,
    src_len: c_int,
    out_nodes: *?[*]const u8,
    out_count: *c_int,
) c_int {
    parse_mutex.lock();
    defer parse_mutex.unlock();

    if (src_len <= 0) return 0;
    const source = src_ptr[0..@intCast(src_len)];

    var parser = Parser.init(source, c_alloc);
    _ = parser.parseSourceFile() catch return 0;

    const nodes_slice = parser.ast.nodes.items;
    if (nodes_slice.len == 0) return 0;

    // CRITICAL: copy nodes to a separate allocation that persists.
    // The Parser's ArrayList items may be reclaimed when parser goes out of scope.
    const copy = c_alloc.alloc(FlatNode, nodes_slice.len) catch return 0;
    @memcpy(copy, nodes_slice);

    out_nodes.* = @ptrCast(copy.ptr);
    out_count.* = @intCast(copy.len);
    return 1;
}

// ── Benchmark ──

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const alloc = gpa.allocator();

    var args = std.process.args();
    _ = args.next();
    const path = args.next() orelse {
        _ = std.posix.write(2, "Usage: zig_parser_core <file.ts>\n") catch {};
        return;
    };
    const file = try std.fs.cwd().openFile(path, .{});
    defer file.close();
    const content = try file.readToEndAlloc(alloc, 100 * 1024 * 1024);
    defer alloc.free(content);

    // Benchmark: parse 10 times
    var timer = try std.time.Timer.start();
    var total_nodes: u64 = 0;
    for (0..10) |_| {
        var parser = Parser.init(content, alloc);
        defer parser.deinit();
        _ = parser.parseSourceFile() catch 0;
        total_nodes += parser.ast.nodes.items.len;
    }
    const elapsed = timer.read();
    const nodes = total_nodes / 10;
    var buf: [256]u8 = undefined;
    const msg = std.fmt.bufPrint(&buf, "Nodes: {d}, Time: {d}ms (10 runs), {d}ms/run, {d}KB source\n", .{
        nodes,
        elapsed / std.time.ns_per_ms,
        elapsed / std.time.ns_per_ms / 10,
        content.len / 1024,
    }) catch "err\n";
    _ = std.posix.write(2, msg) catch {};
}
