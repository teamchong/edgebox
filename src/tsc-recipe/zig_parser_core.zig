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
pub const NK = struct {
    pub const SourceFile: u16 = 316;
    pub const Block: u16 = 241;
    pub const VariableStatement: u16 = 243;
    pub const VariableDeclarationList: u16 = 261;
    pub const VariableDeclaration: u16 = 260;
    pub const ExpressionStatement: u16 = 244;
    pub const IfStatement: u16 = 245;
    pub const ReturnStatement: u16 = 253;
    pub const FunctionDeclaration: u16 = 262;
    pub const ClassDeclaration: u16 = 263;
    pub const InterfaceDeclaration: u16 = 264;
    pub const TypeAliasDeclaration: u16 = 265;
    pub const EnumDeclaration: u16 = 266;
    pub const ImportDeclaration: u16 = 272;
    pub const ExportDeclaration: u16 = 278;
    pub const ExportAssignment: u16 = 277;
    // Expressions
    pub const Identifier: u16 = 80;
    pub const StringLiteral: u16 = 11;
    pub const NumericLiteral: u16 = 9;
    pub const CallExpression: u16 = 213;
    pub const PropertyAccessExpression: u16 = 211;
    pub const ElementAccessExpression: u16 = 212;
    pub const BinaryExpression: u16 = 226;
    pub const ArrowFunction: u16 = 219;
    pub const ObjectLiteralExpression: u16 = 210;
    pub const ArrayLiteralExpression: u16 = 209;
    pub const ParenthesizedExpression: u16 = 217;
    // Types
    pub const TypeReference: u16 = 183;
    pub const UnionType: u16 = 193;
    pub const ArrayType: u16 = 188;
    pub const LiteralType: u16 = 200;
    pub const FunctionType: u16 = 184;
    pub const Parameter: u16 = 169;
    pub const PropertySignature: u16 = 171;
};

// Flat AST node — 20 bytes, cache-friendly, zero GC
pub const FlatNode = struct {
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
            const stmt = self.parseStatement() catch {
                // Error recovery: skip token
                self.nextToken();
                continue;
            };
            self.ast.addChild(sf, stmt);
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
        // Name
        if (self.current_token == SK.Identifier) {
            const name = try self.ast.addNode(NK.Identifier, self.pos(), self.endPos(), 0);
            self.ast.addChild(decl, name);
            self.nextToken();
        }
        // Optional type annotation
        if (self.current_token == SK.Colon) {
            self.nextToken();
            self.skipTypeAnnotation();
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

    fn parseFunctionDeclaration(self: *Parser) ParseError!u32 {
        const start = self.pos();
        const decl = try self.ast.addNode(NK.FunctionDeclaration, start, 0, 0);
        self.nextToken(); // skip 'function'
        // Name
        if (self.current_token == SK.Identifier) {
            const name = try self.ast.addNode(NK.Identifier, self.pos(), self.endPos(), 0);
            self.ast.addChild(decl, name);
            self.nextToken();
        }
        // Skip params and body (simplified)
        self.skipParenthesized();
        if (self.current_token == SK.Colon) { self.nextToken(); self.skipTypeAnnotation(); }
        if (self.current_token == SK.OpenBrace) {
            const body = try self.parseBlock();
            self.ast.addChild(decl, body);
        }
        self.ast.setEnd(decl, self.endPos());
        return decl;
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
        if (self.current_token == SK.OpenBrace) {
            const body = try self.parseBlock();
            self.ast.addChild(decl, body);
        }
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
        while (self.current_token != SK.OpenBrace and self.current_token != SK.EndOfFile) self.nextToken();
        if (self.current_token == SK.OpenBrace) self.skipBraced();
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
        // Skip everything until semicolon/newline
        while (self.current_token != SK.Semicolon and self.current_token != SK.EndOfFile) {
            if (self.current_token == SK.OpenBrace) { self.skipBraced(); continue; }
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

    // ── Expressions (simplified) ──

    fn parseExpression(self: *Parser) ParseError!u32 {
        return self.parsePrimaryExpression();
    }

    fn parsePrimaryExpression(self: *Parser) ParseError!u32 {
        const start = self.pos();
        return switch (self.current_token) {
            SK.Identifier, 110, 108 => blk: { // identifier, this, super
                const node = try self.ast.addNode(NK.Identifier, start, self.endPos(), 0);
                self.nextToken();
                break :blk self.parsePostfixExpression(node);
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
            SK.OpenParen => blk: {
                const node = try self.ast.addNode(NK.ParenthesizedExpression, start, 0, 0);
                self.nextToken();
                const inner = try self.parseExpression();
                self.ast.addChild(node, inner);
                self.expect(SK.CloseParen);
                self.ast.setEnd(node, self.endPos());
                break :blk self.parsePostfixExpression(node);
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
            105 => blk: { // new
                self.nextToken();
                const expr = try self.parsePrimaryExpression();
                break :blk expr;
            },
            else => blk: {
                // Unknown expression — create identifier placeholder
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
                    if (self.current_token == SK.Identifier) {
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

    fn skipTypeAnnotation(self: *Parser) void {
        // Skip type until we hit something that ends the type context
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
