// Shell Command Parser for Security Analysis
//
// This module provides shell command tokenization and security analysis
// to detect dangerous patterns like:
// - Redirects to sensitive files: echo foo > .env
// - Pipes to dangerous commands: cat secret | tee ~/.ssh/config
// - Command substitution: git reset $(cat malicious)
// - Variable expansion: echo $API_KEY > /tmp/leak
//
// Run tests with: zig test src/shell_parser.zig

const std = @import("std");

// ============================================================================
// Token Types
// ============================================================================

pub const TokenType = enum {
    word, // command/argument (may include quoted content)
    redirect_out, // >
    redirect_append, // >>
    redirect_in, // <
    redirect_fd, // 2>, &>, etc.
    pipe, // |
    semicolon, // ;
    and_op, // &&
    or_op, // ||
    subshell_start, // $( or (
    subshell_end, // )
    backtick, // `
    background, // &
    newline, // \n
};

pub const Token = struct {
    type: TokenType,
    value: []const u8,
    // For redirects, this is the fd number (0=stdin, 1=stdout, 2=stderr)
    fd: ?u8 = null,
};

// ============================================================================
// Security Analysis Results
// ============================================================================

pub const SecurityAnalysis = struct {
    allocator: std.mem.Allocator,

    // Files being redirected to (output redirects only)
    redirect_targets: std.ArrayListUnmanaged([]const u8) = .{},

    // Commands after pipe operators
    pipe_commands: std.ArrayListUnmanaged([]const u8) = .{},

    // Contains $() or backticks
    has_subshell: bool = false,

    // Ends with & (background)
    has_background: bool = false,

    // Contains ; or && or || (multiple commands)
    has_command_chain: bool = false,

    // Variable references found ($VAR, ${VAR})
    variables_used: std.ArrayListUnmanaged([]const u8) = .{},

    pub fn deinit(self: *SecurityAnalysis) void {
        self.redirect_targets.deinit(self.allocator);
        self.pipe_commands.deinit(self.allocator);
        self.variables_used.deinit(self.allocator);
    }
};

// ============================================================================
// Tokenizer State
// ============================================================================

const TokenizerState = enum {
    normal,
    in_single_quote,
    in_double_quote,
    in_escape,
    in_word,
};

// ============================================================================
// Shell Tokenizer
// ============================================================================

pub const ShellTokenizer = struct {
    input: []const u8,
    pos: usize = 0,
    allocator: std.mem.Allocator,
    tokens: std.ArrayListUnmanaged(Token) = .{},

    pub fn init(allocator: std.mem.Allocator, input: []const u8) ShellTokenizer {
        return .{
            .input = input,
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *ShellTokenizer) void {
        self.tokens.deinit(self.allocator);
    }

    /// Tokenize the entire input
    pub fn tokenize(self: *ShellTokenizer) ![]Token {
        while (self.pos < self.input.len) {
            try self.nextToken();
        }
        return self.tokens.items;
    }

    fn nextToken(self: *ShellTokenizer) !void {
        self.skipWhitespace();
        if (self.pos >= self.input.len) return;

        const c = self.input[self.pos];

        // Check for operators first
        if (try self.tryOperator()) return;

        // Check for subshell start
        if (c == '$' and self.pos + 1 < self.input.len and self.input[self.pos + 1] == '(') {
            try self.tokens.append(self.allocator, .{
                .type = .subshell_start,
                .value = self.input[self.pos .. self.pos + 2],
            });
            self.pos += 2;
            return;
        }

        // Check for backtick
        if (c == '`') {
            try self.tokens.append(self.allocator, .{
                .type = .backtick,
                .value = self.input[self.pos .. self.pos + 1],
            });
            self.pos += 1;
            return;
        }

        // Check for closing paren (subshell end)
        if (c == ')') {
            try self.tokens.append(self.allocator, .{
                .type = .subshell_end,
                .value = self.input[self.pos .. self.pos + 1],
            });
            self.pos += 1;
            return;
        }

        // Otherwise, it's a word (possibly with quotes)
        try self.parseWord();
    }

    fn tryOperator(self: *ShellTokenizer) !bool {
        if (self.pos >= self.input.len) return false;

        const remaining = self.input[self.pos..];

        // Check for fd redirect (e.g., 2>, &>)
        if (remaining.len >= 2) {
            // &> or &>> (redirect both stdout and stderr)
            if (remaining[0] == '&' and remaining[1] == '>') {
                const is_append = remaining.len >= 3 and remaining[2] == '>';
                const len: usize = if (is_append) 3 else 2;
                try self.tokens.append(self.allocator, .{
                    .type = if (is_append) .redirect_append else .redirect_out,
                    .value = remaining[0..len],
                    .fd = null, // Special: both fds
                });
                self.pos += len;
                return true;
            }

            // 2> or 2>> (redirect stderr)
            if (remaining[0] == '2' and remaining[1] == '>') {
                const is_append = remaining.len >= 3 and remaining[2] == '>';
                const len: usize = if (is_append) 3 else 2;
                try self.tokens.append(self.allocator, .{
                    .type = if (is_append) .redirect_append else .redirect_out,
                    .value = remaining[0..len],
                    .fd = 2,
                });
                self.pos += len;
                return true;
            }
        }

        // Two-character operators
        if (remaining.len >= 2) {
            const two = remaining[0..2];
            if (std.mem.eql(u8, two, ">>")) {
                try self.tokens.append(self.allocator, .{ .type = .redirect_append, .value = two, .fd = 1 });
                self.pos += 2;
                return true;
            }
            if (std.mem.eql(u8, two, "&&")) {
                try self.tokens.append(self.allocator, .{ .type = .and_op, .value = two });
                self.pos += 2;
                return true;
            }
            if (std.mem.eql(u8, two, "||")) {
                try self.tokens.append(self.allocator, .{ .type = .or_op, .value = two });
                self.pos += 2;
                return true;
            }
        }

        // Single-character operators
        const c = remaining[0];
        switch (c) {
            '>' => {
                try self.tokens.append(self.allocator, .{ .type = .redirect_out, .value = remaining[0..1], .fd = 1 });
                self.pos += 1;
                return true;
            },
            '<' => {
                try self.tokens.append(self.allocator, .{ .type = .redirect_in, .value = remaining[0..1], .fd = 0 });
                self.pos += 1;
                return true;
            },
            '|' => {
                try self.tokens.append(self.allocator, .{ .type = .pipe, .value = remaining[0..1] });
                self.pos += 1;
                return true;
            },
            ';' => {
                try self.tokens.append(self.allocator, .{ .type = .semicolon, .value = remaining[0..1] });
                self.pos += 1;
                return true;
            },
            '&' => {
                // Single & is background (already handled &> above)
                try self.tokens.append(self.allocator, .{ .type = .background, .value = remaining[0..1] });
                self.pos += 1;
                return true;
            },
            '\n' => {
                try self.tokens.append(self.allocator, .{ .type = .newline, .value = remaining[0..1] });
                self.pos += 1;
                return true;
            },
            else => return false,
        }
    }

    fn parseWord(self: *ShellTokenizer) !void {
        const start = self.pos;
        var state: TokenizerState = .normal;

        while (self.pos < self.input.len) {
            const c = self.input[self.pos];

            switch (state) {
                .normal => {
                    // Check for word-ending characters
                    if (isWordBreak(c)) break;

                    if (c == '\\') {
                        state = .in_escape;
                        self.pos += 1;
                    } else if (c == '\'') {
                        state = .in_single_quote;
                        self.pos += 1;
                    } else if (c == '"') {
                        state = .in_double_quote;
                        self.pos += 1;
                    } else {
                        self.pos += 1;
                    }
                },
                .in_escape => {
                    // Escaped character, always include
                    self.pos += 1;
                    state = .normal;
                },
                .in_single_quote => {
                    if (c == '\'') {
                        self.pos += 1;
                        state = .normal;
                    } else {
                        self.pos += 1;
                    }
                },
                .in_double_quote => {
                    if (c == '"') {
                        self.pos += 1;
                        state = .normal;
                    } else if (c == '\\') {
                        // In double quotes, backslash only escapes specific chars
                        self.pos += 1;
                        if (self.pos < self.input.len) {
                            self.pos += 1;
                        }
                    } else {
                        self.pos += 1;
                    }
                },
                .in_word => unreachable,
            }
        }

        if (self.pos > start) {
            try self.tokens.append(self.allocator, .{
                .type = .word,
                .value = self.input[start..self.pos],
            });
        }
    }

    fn skipWhitespace(self: *ShellTokenizer) void {
        while (self.pos < self.input.len) {
            const c = self.input[self.pos];
            if (c == ' ' or c == '\t') {
                self.pos += 1;
            } else {
                break;
            }
        }
    }

    fn isWordBreak(c: u8) bool {
        return switch (c) {
            ' ', '\t', '\n', '>', '<', '|', ';', '&', '(', ')', '`' => true,
            else => false,
        };
    }
};

// ============================================================================
// Security Analysis
// ============================================================================

/// Analyze a shell command for security-relevant patterns
pub fn analyzeForSecurity(allocator: std.mem.Allocator, cmd: []const u8) !SecurityAnalysis {
    var result = SecurityAnalysis{ .allocator = allocator };
    errdefer result.deinit();

    var tokenizer = ShellTokenizer.init(allocator, cmd);
    defer tokenizer.deinit();

    const tokens = try tokenizer.tokenize();

    var i: usize = 0;
    var in_subshell: usize = 0; // Nesting level
    var after_pipe = false;
    var pipe_cmd_start: ?usize = null;

    while (i < tokens.len) : (i += 1) {
        const token = tokens[i];

        switch (token.type) {
            .redirect_out, .redirect_append => {
                // Next token should be the target file
                if (i + 1 < tokens.len and tokens[i + 1].type == .word) {
                    const target = stripQuotes(tokens[i + 1].value);
                    try result.redirect_targets.append(allocator, target);
                    i += 1; // Skip the target
                }
            },
            .pipe => {
                // Save current pipe command before starting new one
                if (after_pipe and pipe_cmd_start != null) {
                    const cmd_slice = extractCommandSlice(cmd, tokens, pipe_cmd_start.?, i);
                    if (cmd_slice.len > 0) {
                        try result.pipe_commands.append(allocator, cmd_slice);
                    }
                }
                after_pipe = true;
                pipe_cmd_start = i + 1;
            },
            .subshell_start => {
                result.has_subshell = true;
                in_subshell += 1;
            },
            .subshell_end => {
                if (in_subshell > 0) in_subshell -= 1;
            },
            .backtick => {
                result.has_subshell = true;
            },
            .background => {
                result.has_background = true;
            },
            .semicolon, .and_op, .or_op => {
                result.has_command_chain = true;
                // End current pipe command if any
                if (after_pipe and pipe_cmd_start != null) {
                    const cmd_slice = extractCommandSlice(cmd, tokens, pipe_cmd_start.?, i);
                    if (cmd_slice.len > 0) {
                        try result.pipe_commands.append(allocator, cmd_slice);
                    }
                }
                after_pipe = false;
                pipe_cmd_start = null;
            },
            .word => {
                // Check for variable references in the word
                try extractVariables(allocator, token.value, &result.variables_used);
            },
            else => {},
        }
    }

    // Handle trailing pipe command
    if (after_pipe and pipe_cmd_start != null) {
        const cmd_slice = extractCommandSlice(cmd, tokens, pipe_cmd_start.?, tokens.len);
        if (cmd_slice.len > 0) {
            try result.pipe_commands.append(allocator, cmd_slice);
        }
    }

    return result;
}

/// Strip surrounding quotes from a value
fn stripQuotes(value: []const u8) []const u8 {
    if (value.len < 2) return value;

    const first = value[0];
    const last = value[value.len - 1];

    if ((first == '"' and last == '"') or (first == '\'' and last == '\'')) {
        return value[1 .. value.len - 1];
    }
    return value;
}

/// Extract the command text between two token indices
fn extractCommandSlice(original: []const u8, tokens: []Token, start_idx: usize, end_idx: usize) []const u8 {
    if (start_idx >= tokens.len or start_idx >= end_idx) return "";

    // Find the actual positions in the original string
    const first_token = tokens[start_idx];
    const first_pos = @intFromPtr(first_token.value.ptr) - @intFromPtr(original.ptr);

    var last_pos = first_pos + first_token.value.len;
    if (end_idx > start_idx and end_idx <= tokens.len) {
        const idx = if (end_idx == tokens.len) end_idx - 1 else end_idx - 1;
        if (idx < tokens.len) {
            const last_token = tokens[idx];
            last_pos = @intFromPtr(last_token.value.ptr) - @intFromPtr(original.ptr) + last_token.value.len;
        }
    }

    if (first_pos < original.len and last_pos <= original.len and first_pos < last_pos) {
        return std.mem.trim(u8, original[first_pos..last_pos], " \t");
    }
    return "";
}

/// Extract variable references from a token value
fn extractVariables(allocator: std.mem.Allocator, value: []const u8, vars: *std.ArrayListUnmanaged([]const u8)) !void {
    var i: usize = 0;
    while (i < value.len) {
        if (value[i] == '$') {
            i += 1;
            if (i >= value.len) break;

            // ${VAR} format
            if (value[i] == '{') {
                i += 1;
                const start = i;
                while (i < value.len and value[i] != '}') : (i += 1) {}
                if (i > start) {
                    try vars.append(allocator, value[start..i]);
                }
                if (i < value.len) i += 1; // Skip }
            }
            // $VAR format
            else if (isVarChar(value[i])) {
                const start = i;
                while (i < value.len and isVarChar(value[i])) : (i += 1) {}
                if (i > start) {
                    try vars.append(allocator, value[start..i]);
                }
            }
        } else {
            i += 1;
        }
    }
}

fn isVarChar(c: u8) bool {
    return std.ascii.isAlphanumeric(c) or c == '_';
}

// ============================================================================
// Convenience Functions for Security Checks
// ============================================================================

/// Check if a command has redirects to any of the given sensitive files
pub fn hasRedirectToSensitiveFile(analysis: *const SecurityAnalysis, sensitive_patterns: []const []const u8) bool {
    for (analysis.redirect_targets.items) |target| {
        for (sensitive_patterns) |pattern| {
            if (matchesSensitivePattern(target, pattern)) {
                return true;
            }
        }
    }
    return false;
}

/// Check if any piped command is potentially dangerous
pub fn hasDangerousPipeTarget(analysis: *const SecurityAnalysis, dangerous_commands: []const []const u8) bool {
    for (analysis.pipe_commands.items) |piped| {
        // Extract first word (command name) from piped command
        const cmd_name = extractFirstWord(piped);
        for (dangerous_commands) |dangerous| {
            if (std.mem.eql(u8, cmd_name, dangerous)) {
                return true;
            }
        }
    }
    return false;
}

/// Check if any sensitive variable is used
pub fn usesSensitiveVariable(analysis: *const SecurityAnalysis, sensitive_vars: []const []const u8) bool {
    for (analysis.variables_used.items) |var_name| {
        for (sensitive_vars) |sensitive| {
            if (std.mem.eql(u8, var_name, sensitive)) {
                return true;
            }
        }
    }
    return false;
}

fn matchesSensitivePattern(target: []const u8, pattern: []const u8) bool {
    // Handle glob patterns
    if (pattern.len > 0 and pattern[0] == '*') {
        // *.ext pattern
        const ext = pattern[1..];
        return std.mem.endsWith(u8, target, ext);
    }
    // Exact or substring match
    return std.mem.indexOf(u8, target, pattern) != null;
}

fn extractFirstWord(s: []const u8) []const u8 {
    const trimmed = std.mem.trim(u8, s, " \t");
    for (trimmed, 0..) |c, i| {
        if (c == ' ' or c == '\t') {
            return trimmed[0..i];
        }
    }
    return trimmed;
}

// ============================================================================
// Tests
// ============================================================================

test "tokenize simple command" {
    var tokenizer = ShellTokenizer.init(std.testing.allocator, "echo hello world");
    defer tokenizer.deinit();

    const tokens = try tokenizer.tokenize();
    try std.testing.expectEqual(@as(usize, 3), tokens.len);
    try std.testing.expectEqual(TokenType.word, tokens[0].type);
    try std.testing.expectEqualStrings("echo", tokens[0].value);
    try std.testing.expectEqualStrings("hello", tokens[1].value);
    try std.testing.expectEqualStrings("world", tokens[2].value);
}

test "tokenize redirect" {
    var tokenizer = ShellTokenizer.init(std.testing.allocator, "echo foo > file.txt");
    defer tokenizer.deinit();

    const tokens = try tokenizer.tokenize();
    try std.testing.expectEqual(@as(usize, 4), tokens.len);
    try std.testing.expectEqual(TokenType.redirect_out, tokens[2].type);
    try std.testing.expectEqualStrings("file.txt", tokens[3].value);
}

test "tokenize append redirect" {
    var tokenizer = ShellTokenizer.init(std.testing.allocator, "echo bar >> log.txt");
    defer tokenizer.deinit();

    const tokens = try tokenizer.tokenize();
    try std.testing.expectEqual(@as(usize, 4), tokens.len);
    try std.testing.expectEqual(TokenType.redirect_append, tokens[2].type);
}

test "tokenize pipe" {
    var tokenizer = ShellTokenizer.init(std.testing.allocator, "cat file | grep pattern");
    defer tokenizer.deinit();

    const tokens = try tokenizer.tokenize();
    try std.testing.expectEqual(@as(usize, 5), tokens.len);
    try std.testing.expectEqual(TokenType.pipe, tokens[2].type);
}

test "tokenize quoted string - double quotes" {
    var tokenizer = ShellTokenizer.init(std.testing.allocator, "echo \"hello world\"");
    defer tokenizer.deinit();

    const tokens = try tokenizer.tokenize();
    try std.testing.expectEqual(@as(usize, 2), tokens.len);
    try std.testing.expectEqualStrings("\"hello world\"", tokens[1].value);
}

test "tokenize quoted string - single quotes" {
    var tokenizer = ShellTokenizer.init(std.testing.allocator, "echo 'hello $world'");
    defer tokenizer.deinit();

    const tokens = try tokenizer.tokenize();
    try std.testing.expectEqual(@as(usize, 2), tokens.len);
    try std.testing.expectEqualStrings("'hello $world'", tokens[1].value);
}

test "tokenize escaped character" {
    var tokenizer = ShellTokenizer.init(std.testing.allocator, "echo hello\\ world");
    defer tokenizer.deinit();

    const tokens = try tokenizer.tokenize();
    try std.testing.expectEqual(@as(usize, 2), tokens.len);
    try std.testing.expectEqualStrings("hello\\ world", tokens[1].value);
}

test "tokenize subshell" {
    var tokenizer = ShellTokenizer.init(std.testing.allocator, "echo $(whoami)");
    defer tokenizer.deinit();

    const tokens = try tokenizer.tokenize();
    try std.testing.expectEqual(@as(usize, 4), tokens.len);
    try std.testing.expectEqual(TokenType.subshell_start, tokens[1].type);
    try std.testing.expectEqual(TokenType.word, tokens[2].type);
    try std.testing.expectEqual(TokenType.subshell_end, tokens[3].type);
}

test "tokenize backticks" {
    var tokenizer = ShellTokenizer.init(std.testing.allocator, "echo `date`");
    defer tokenizer.deinit();

    const tokens = try tokenizer.tokenize();
    try std.testing.expectEqual(@as(usize, 4), tokens.len);
    try std.testing.expectEqual(TokenType.backtick, tokens[1].type);
    try std.testing.expectEqual(TokenType.backtick, tokens[3].type);
}

test "tokenize command chain" {
    var tokenizer = ShellTokenizer.init(std.testing.allocator, "cmd1 && cmd2 || cmd3");
    defer tokenizer.deinit();

    const tokens = try tokenizer.tokenize();
    try std.testing.expectEqual(@as(usize, 5), tokens.len);
    try std.testing.expectEqual(TokenType.and_op, tokens[1].type);
    try std.testing.expectEqual(TokenType.or_op, tokens[3].type);
}

test "tokenize stderr redirect" {
    var tokenizer = ShellTokenizer.init(std.testing.allocator, "cmd 2> error.log");
    defer tokenizer.deinit();

    const tokens = try tokenizer.tokenize();
    try std.testing.expectEqual(@as(usize, 3), tokens.len);
    try std.testing.expectEqual(TokenType.redirect_out, tokens[1].type);
    try std.testing.expectEqual(@as(?u8, 2), tokens[1].fd);
}

test "analyze redirect to sensitive file" {
    var analysis = try analyzeForSecurity(std.testing.allocator, "echo secret > .env");
    defer analysis.deinit();

    try std.testing.expectEqual(@as(usize, 1), analysis.redirect_targets.items.len);
    try std.testing.expectEqualStrings(".env", analysis.redirect_targets.items[0]);
}

test "analyze has subshell" {
    var analysis = try analyzeForSecurity(std.testing.allocator, "echo $(whoami)");
    defer analysis.deinit();

    try std.testing.expect(analysis.has_subshell);
}

test "analyze has backtick subshell" {
    var analysis = try analyzeForSecurity(std.testing.allocator, "echo `date`");
    defer analysis.deinit();

    try std.testing.expect(analysis.has_subshell);
}

test "analyze pipe commands" {
    var analysis = try analyzeForSecurity(std.testing.allocator, "cat file | grep pattern | head");
    defer analysis.deinit();

    try std.testing.expectEqual(@as(usize, 2), analysis.pipe_commands.items.len);
}

test "analyze variable extraction" {
    var analysis = try analyzeForSecurity(std.testing.allocator, "echo $HOME ${API_KEY}");
    defer analysis.deinit();

    try std.testing.expectEqual(@as(usize, 2), analysis.variables_used.items.len);
}

test "analyze command chain" {
    var analysis = try analyzeForSecurity(std.testing.allocator, "cmd1 && cmd2");
    defer analysis.deinit();

    try std.testing.expect(analysis.has_command_chain);
}

test "analyze background" {
    var analysis = try analyzeForSecurity(std.testing.allocator, "cmd &");
    defer analysis.deinit();

    try std.testing.expect(analysis.has_background);
}

test "quoted redirect is not detected as operator" {
    var analysis = try analyzeForSecurity(std.testing.allocator, "echo \">\" test");
    defer analysis.deinit();

    // The > inside quotes should NOT be detected as redirect
    try std.testing.expectEqual(@as(usize, 0), analysis.redirect_targets.items.len);
}

test "hasRedirectToSensitiveFile" {
    var analysis = try analyzeForSecurity(std.testing.allocator, "echo foo > .env");
    defer analysis.deinit();

    const sensitive = &[_][]const u8{ ".env", "*.pem" };
    try std.testing.expect(hasRedirectToSensitiveFile(&analysis, sensitive));
}

test "hasDangerousPipeTarget" {
    var analysis = try analyzeForSecurity(std.testing.allocator, "cat file | tee output");
    defer analysis.deinit();

    const dangerous = &[_][]const u8{ "tee", "xargs" };
    try std.testing.expect(hasDangerousPipeTarget(&analysis, dangerous));
}

test "usesSensitiveVariable" {
    var analysis = try analyzeForSecurity(std.testing.allocator, "echo $API_KEY");
    defer analysis.deinit();

    const sensitive = &[_][]const u8{ "API_KEY", "AWS_SECRET" };
    try std.testing.expect(usesSensitiveVariable(&analysis, sensitive));
}
