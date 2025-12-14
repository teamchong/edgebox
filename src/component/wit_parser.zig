/// Minimal WIT Parser for EdgeBox Component Model
/// Parses a subset of WIT syntax needed for timer interface
///
/// Based on: https://github.com/WebAssembly/component-model/blob/main/design/mvp/WIT.md

const std = @import("std");

/// Parsed WIT package
pub const Package = struct {
    name: []const u8,
    version: []const u8,
    interfaces: []Interface,
    types: []TypeDef,

    pub fn deinit(self: *Package, allocator: std.mem.Allocator) void {
        allocator.free(self.name);
        allocator.free(self.version);
        for (self.interfaces) |*iface| {
            iface.deinit(allocator);
        }
        allocator.free(self.interfaces);
        for (self.types) |*typedef| {
            typedef.deinit(allocator);
        }
        allocator.free(self.types);
    }
};

/// WIT interface definition
pub const Interface = struct {
    name: []const u8,
    functions: []Function,

    pub fn deinit(self: *Interface, allocator: std.mem.Allocator) void {
        allocator.free(self.name);
        for (self.functions) |*func| {
            func.deinit(allocator);
        }
        allocator.free(self.functions);
    }
};

/// Function definition
pub const Function = struct {
    name: []const u8,
    params: []Param,
    result: ?Type,

    pub fn deinit(self: *Function, allocator: std.mem.Allocator) void {
        allocator.free(self.name);
        for (self.params) |*param| {
            param.deinit(allocator);
        }
        allocator.free(self.params);
        if (self.result) |*result| {
            result.deinit(allocator);
        }
    }
};

/// Function parameter
pub const Param = struct {
    name: []const u8,
    type: Type,

    pub fn deinit(self: *Param, allocator: std.mem.Allocator) void {
        allocator.free(self.name);
        self.type.deinit(allocator);
    }
};

/// Type definition (type aliases)
pub const TypeDef = struct {
    name: []const u8,
    type: Type,

    pub fn deinit(self: *TypeDef, allocator: std.mem.Allocator) void {
        allocator.free(self.name);
        self.type.deinit(allocator);
    }
};

/// WIT type
pub const Type = union(enum) {
    u8,
    u16,
    u32,
    u64,
    s8,
    s16,
    s32,
    s64,
    f32,
    f64,
    bool,
    string,
    named: []const u8, // e.g., "timer-id"

    pub fn deinit(self: *Type, allocator: std.mem.Allocator) void {
        switch (self.*) {
            .named => |name| allocator.free(name),
            else => {},
        }
    }
};

/// Parser state
const Parser = struct {
    allocator: std.mem.Allocator,
    source: []const u8,
    pos: usize,

    fn init(allocator: std.mem.Allocator, source: []const u8) Parser {
        return .{
            .allocator = allocator,
            .source = source,
            .pos = 0,
        };
    }

    /// Parse WIT source into Package
    pub fn parse(self: *Parser) !Package {
        self.skipWhitespaceAndComments();

        // Parse package declaration
        const pkg_name = try self.parsePackage();
        errdefer self.allocator.free(pkg_name);

        const version = try self.parseVersion();
        errdefer self.allocator.free(version);

        self.skipWhitespaceAndComments();

        // Parse interfaces and types
        var interfaces: std.ArrayListUnmanaged(Interface) = .{};
        defer interfaces.deinit(self.allocator);
        errdefer {
            for (interfaces.items) |*iface| iface.deinit(self.allocator);
        }

        var types: std.ArrayListUnmanaged(TypeDef) = .{};
        defer types.deinit(self.allocator);
        errdefer {
            for (types.items) |*typedef| typedef.deinit(self.allocator);
        }

        while (self.pos < self.source.len) {
            self.skipWhitespaceAndComments();
            if (self.pos >= self.source.len) break;

            if (try self.tryParseKeyword("interface")) {
                const iface = try self.parseInterface();
                try interfaces.append(self.allocator, iface);
            } else if (try self.tryParseKeyword("type")) {
                const typedef = try self.parseTypeDef();
                try types.append(self.allocator, typedef);
            } else {
                return error.UnexpectedToken;
            }

            self.skipWhitespaceAndComments();
        }

        return Package{
            .name = pkg_name,
            .version = version,
            .interfaces = try interfaces.toOwnedSlice(self.allocator),
            .types = try types.toOwnedSlice(self.allocator),
        };
    }

    fn parsePackage(self: *Parser) ![]const u8 {
        try self.expectKeyword("package");
        self.skipWhitespace();

        const start = self.pos;
        // Parse until semicolon: "edgebox:runtime@1.0.0;"
        while (self.pos < self.source.len and self.source[self.pos] != ';') {
            self.pos += 1;
        }

        if (self.pos >= self.source.len) return error.UnexpectedEof;

        const pkg_line = self.source[start..self.pos];
        self.pos += 1; // skip semicolon

        // Extract just the name part before @
        var it = std.mem.splitScalar(u8, pkg_line, '@');
        const name_part = std.mem.trim(u8, it.first(), " \t\r\n");

        return try self.allocator.dupe(u8, name_part);
    }

    fn parseVersion(self: *Parser) ![]const u8 {
        // Already parsed in parsePackage, extract from source
        var it = std.mem.splitScalar(u8, self.source[0..self.pos], '@');
        _ = it.first(); // skip name
        const version_part = it.next() orelse return error.MissingVersion;

        var it2 = std.mem.splitScalar(u8, version_part, ';');
        const version = std.mem.trim(u8, it2.first(), " \t\r\n");

        return try self.allocator.dupe(u8, version);
    }

    fn parseInterface(self: *Parser) !Interface {
        self.skipWhitespace();
        const name = try self.parseIdentifier();
        errdefer self.allocator.free(name);

        self.skipWhitespace();
        try self.expectChar('{');

        var functions: std.ArrayListUnmanaged(Function) = .{};
        defer functions.deinit(self.allocator);
        errdefer {
            for (functions.items) |*func| func.deinit(self.allocator);
        }

        while (true) {
            self.skipWhitespaceAndComments();
            if (self.pos >= self.source.len) return error.UnexpectedEof;
            if (self.source[self.pos] == '}') {
                self.pos += 1;
                break;
            }

            const func = try self.parseFunction();
            try functions.append(self.allocator, func);
        }

        return Interface{
            .name = name,
            .functions = try functions.toOwnedSlice(self.allocator),
        };
    }

    fn parseFunction(self: *Parser) !Function {
        const name = try self.parseIdentifier();
        errdefer self.allocator.free(name);

        self.skipWhitespace();
        try self.expectChar(':');
        self.skipWhitespace();
        try self.expectKeyword("func");
        try self.expectChar('(');

        // Parse parameters
        var params: std.ArrayListUnmanaged(Param) = .{};
        defer params.deinit(self.allocator);
        errdefer {
            for (params.items) |*param| param.deinit(self.allocator);
        }

        while (true) {
            self.skipWhitespace();
            if (self.source[self.pos] == ')') {
                self.pos += 1;
                break;
            }

            if (params.items.len > 0) {
                try self.expectChar(',');
                self.skipWhitespace();
            }

            const param = try self.parseParam();
            try params.append(self.allocator, param);
        }

        self.skipWhitespace();

        // Parse optional result
        var result: ?Type = null;
        if (self.pos < self.source.len and self.source[self.pos] == '-') {
            try self.expectString("->");
            self.skipWhitespace();
            result = try self.parseType();
        }

        self.skipWhitespace();
        try self.expectChar(';');

        return Function{
            .name = name,
            .params = try params.toOwnedSlice(self.allocator),
            .result = result,
        };
    }

    fn parseParam(self: *Parser) !Param {
        const name = try self.parseIdentifier();
        errdefer self.allocator.free(name);

        self.skipWhitespace();
        try self.expectChar(':');
        self.skipWhitespace();

        const param_type = try self.parseType();

        return Param{
            .name = name,
            .type = param_type,
        };
    }

    fn parseTypeDef(self: *Parser) !TypeDef {
        self.skipWhitespace();
        const name = try self.parseIdentifier();
        errdefer self.allocator.free(name);

        self.skipWhitespace();
        try self.expectChar('=');
        self.skipWhitespace();

        const typedef_type = try self.parseType();

        self.skipWhitespace();
        try self.expectChar(';');

        return TypeDef{
            .name = name,
            .type = typedef_type,
        };
    }

    fn parseType(self: *Parser) !Type {
        const ident = try self.parseIdentifier();
        defer self.allocator.free(ident);

        // Check for primitive types
        if (std.mem.eql(u8, ident, "u8")) return Type.u8;
        if (std.mem.eql(u8, ident, "u16")) return Type.u16;
        if (std.mem.eql(u8, ident, "u32")) return Type.u32;
        if (std.mem.eql(u8, ident, "u64")) return Type.u64;
        if (std.mem.eql(u8, ident, "s8")) return Type.s8;
        if (std.mem.eql(u8, ident, "s16")) return Type.s16;
        if (std.mem.eql(u8, ident, "s32")) return Type.s32;
        if (std.mem.eql(u8, ident, "s64")) return Type.s64;
        if (std.mem.eql(u8, ident, "f32")) return Type.f32;
        if (std.mem.eql(u8, ident, "f64")) return Type.f64;
        if (std.mem.eql(u8, ident, "bool")) return Type.bool;
        if (std.mem.eql(u8, ident, "string")) return Type.string;

        // Named type (e.g., "timer-id")
        return Type{ .named = try self.allocator.dupe(u8, ident) };
    }

    fn parseIdentifier(self: *Parser) ![]const u8 {
        const start = self.pos;

        // First char: a-z, A-Z
        if (self.pos >= self.source.len or !isAlpha(self.source[self.pos])) {
            return error.ExpectedIdentifier;
        }
        self.pos += 1;

        // Rest: a-z, A-Z, 0-9, -, _
        while (self.pos < self.source.len and isIdentChar(self.source[self.pos])) {
            self.pos += 1;
        }

        const ident = self.source[start..self.pos];
        return try self.allocator.dupe(u8, ident);
    }

    fn expectKeyword(self: *Parser, keyword: []const u8) !void {
        self.skipWhitespace();
        if (!try self.tryParseKeyword(keyword)) {
            return error.ExpectedKeyword;
        }
    }

    fn tryParseKeyword(self: *Parser, keyword: []const u8) !bool {
        if (self.pos + keyword.len > self.source.len) return false;

        const slice = self.source[self.pos .. self.pos + keyword.len];
        if (!std.mem.eql(u8, slice, keyword)) return false;

        // Check not followed by identifier char
        if (self.pos + keyword.len < self.source.len and
            isIdentChar(self.source[self.pos + keyword.len]))
        {
            return false;
        }

        self.pos += keyword.len;
        return true;
    }

    fn expectChar(self: *Parser, char: u8) !void {
        if (self.pos >= self.source.len or self.source[self.pos] != char) {
            return error.ExpectedChar;
        }
        self.pos += 1;
    }

    fn expectString(self: *Parser, str: []const u8) !void {
        if (self.pos + str.len > self.source.len) return error.UnexpectedEof;
        if (!std.mem.eql(u8, self.source[self.pos .. self.pos + str.len], str)) {
            return error.ExpectedString;
        }
        self.pos += str.len;
    }

    fn skipWhitespace(self: *Parser) void {
        while (self.pos < self.source.len and isWhitespace(self.source[self.pos])) {
            self.pos += 1;
        }
    }

    fn skipWhitespaceAndComments(self: *Parser) void {
        while (self.pos < self.source.len) {
            if (isWhitespace(self.source[self.pos])) {
                self.pos += 1;
            } else if (self.pos + 2 <= self.source.len and
                std.mem.eql(u8, self.source[self.pos .. self.pos + 2], "//"))
            {
                // Skip line comment
                self.pos += 2;
                while (self.pos < self.source.len and self.source[self.pos] != '\n') {
                    self.pos += 1;
                }
            } else {
                break;
            }
        }
    }

    fn isAlpha(char: u8) bool {
        return (char >= 'a' and char <= 'z') or (char >= 'A' and char <= 'Z');
    }

    fn isIdentChar(char: u8) bool {
        return isAlpha(char) or (char >= '0' and char <= '9') or char == '-' or char == '_';
    }

    fn isWhitespace(char: u8) bool {
        return char == ' ' or char == '\t' or char == '\n' or char == '\r';
    }
};

/// Parse WIT source into Package
pub fn parse(allocator: std.mem.Allocator, source: []const u8) !Package {
    var parser = Parser.init(allocator, source);
    return try parser.parse();
}

// Tests
test "WIT Parser - parse timer interface" {
    // Read the WIT file instead of embedding it
    const file = try std.fs.cwd().openFile("wit/edgebox-timer.wit", .{});
    defer file.close();

    const wit_source = try file.readToEndAlloc(std.testing.allocator, 10 * 1024);
    defer std.testing.allocator.free(wit_source);

    var pkg = try parse(std.testing.allocator, wit_source);
    defer pkg.deinit(std.testing.allocator);

    // Check package
    try std.testing.expectEqualStrings("edgebox:runtime", pkg.name);
    try std.testing.expectEqualStrings("1.0.0", pkg.version);

    // Check interface
    try std.testing.expectEqual(@as(usize, 1), pkg.interfaces.len);
    const timer_iface = pkg.interfaces[0];
    try std.testing.expectEqualStrings("timer", timer_iface.name);

    // Check functions
    try std.testing.expectEqual(@as(usize, 4), timer_iface.functions.len);

    // Check set-timeout function
    const set_timeout = timer_iface.functions[0];
    try std.testing.expectEqualStrings("set-timeout", set_timeout.name);
    try std.testing.expectEqual(@as(usize, 2), set_timeout.params.len);

    try std.testing.expectEqualStrings("callback-id", set_timeout.params[0].name);
    try std.testing.expect(set_timeout.params[0].type == .u32);

    try std.testing.expectEqualStrings("delay-ms", set_timeout.params[1].name);
    try std.testing.expect(set_timeout.params[1].type == .u64);

    try std.testing.expect(set_timeout.result != null);
    try std.testing.expect(set_timeout.result.? == .named);
    try std.testing.expectEqualStrings("timer-id", set_timeout.result.?.named);

    // Check clear-timeout function
    const clear_timeout = timer_iface.functions[1];
    try std.testing.expectEqualStrings("clear-timeout", clear_timeout.name);
    try std.testing.expectEqual(@as(usize, 1), clear_timeout.params.len);
    try std.testing.expectEqualStrings("id", clear_timeout.params[0].name);

    // Check type definitions
    try std.testing.expectEqual(@as(usize, 1), pkg.types.len);
    const timer_id_type = pkg.types[0];
    try std.testing.expectEqualStrings("timer-id", timer_id_type.name);
    try std.testing.expect(timer_id_type.type == .u32);
}

test "WIT Parser - parse basic types" {
    const wit_source =
        \\package test:basic@1.0.0;
        \\
        \\interface types {
        \\  test-u8: func(val: u8) -> u8;
        \\  test-u16: func(val: u16) -> u16;
        \\  test-u32: func(val: u32) -> u32;
        \\  test-u64: func(val: u64) -> u64;
        \\  test-string: func(val: string) -> string;
        \\  test-bool: func(val: bool) -> bool;
        \\}
    ;

    var pkg = try parse(std.testing.allocator, wit_source);
    defer pkg.deinit(std.testing.allocator);

    try std.testing.expectEqualStrings("test:basic", pkg.name);
    try std.testing.expectEqual(@as(usize, 1), pkg.interfaces.len);

    const types_iface = pkg.interfaces[0];
    try std.testing.expectEqual(@as(usize, 6), types_iface.functions.len);

    // Check u8 function
    try std.testing.expect(types_iface.functions[0].params[0].type == .u8);
    try std.testing.expect(types_iface.functions[0].result.? == .u8);

    // Check string function
    try std.testing.expect(types_iface.functions[4].params[0].type == .string);
    try std.testing.expect(types_iface.functions[4].result.? == .string);

    // Check bool function
    try std.testing.expect(types_iface.functions[5].params[0].type == .bool);
    try std.testing.expect(types_iface.functions[5].result.? == .bool);
}
