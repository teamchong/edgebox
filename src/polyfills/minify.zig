/// JavaScript minifier - comptime-compatible for use with @embedFile
/// Removes comments, unnecessary whitespace, and shortens where safe
const std = @import("std");

/// Minify JavaScript source code
/// Safe transformations only - preserves semantics
pub fn minify(comptime source: []const u8) []const u8 {
    comptime {
        var result: [source.len]u8 = undefined;
        var out_len: usize = 0;
        var i: usize = 0;
        var in_string: u8 = 0; // 0 = not in string, '"' or '\'' = in that string
        var in_template: bool = false;
        var in_regex: bool = false;
        var last_char: u8 = 0;
        var last_non_ws: u8 = 0;

        while (i < source.len) {
            const c = source[i];

            // Handle string literals
            if (in_string != 0) {
                result[out_len] = c;
                out_len += 1;
                if (c == in_string and last_char != '\\') {
                    in_string = 0;
                }
                last_char = c;
                i += 1;
                continue;
            }

            // Handle template literals
            if (in_template) {
                result[out_len] = c;
                out_len += 1;
                if (c == '`' and last_char != '\\') {
                    in_template = false;
                }
                last_char = c;
                i += 1;
                continue;
            }

            // Handle regex literals (simple detection)
            if (in_regex) {
                result[out_len] = c;
                out_len += 1;
                if (c == '/' and last_char != '\\') {
                    in_regex = false;
                }
                last_char = c;
                i += 1;
                continue;
            }

            // Check for string/template start
            if (c == '"' or c == '\'') {
                in_string = c;
                result[out_len] = c;
                out_len += 1;
                last_char = c;
                last_non_ws = c;
                i += 1;
                continue;
            }

            if (c == '`') {
                in_template = true;
                result[out_len] = c;
                out_len += 1;
                last_char = c;
                last_non_ws = c;
                i += 1;
                continue;
            }

            // Check for single-line comment
            if (c == '/' and i + 1 < source.len and source[i + 1] == '/') {
                // Skip to end of line
                while (i < source.len and source[i] != '\n') {
                    i += 1;
                }
                continue;
            }

            // Check for multi-line comment
            if (c == '/' and i + 1 < source.len and source[i + 1] == '*') {
                i += 2;
                while (i + 1 < source.len) {
                    if (source[i] == '*' and source[i + 1] == '/') {
                        i += 2;
                        break;
                    }
                    i += 1;
                }
                continue;
            }

            // Check for regex (after operators or at line start)
            if (c == '/' and (last_non_ws == '=' or last_non_ws == '(' or
                last_non_ws == ',' or last_non_ws == ':' or
                last_non_ws == '[' or last_non_ws == '!' or
                last_non_ws == '&' or last_non_ws == '|' or
                last_non_ws == ';' or last_non_ws == '{' or
                last_non_ws == '}' or last_non_ws == '\n' or last_non_ws == 0))
            {
                // Could be regex, preserve it
                in_regex = true;
                result[out_len] = c;
                out_len += 1;
                last_char = c;
                last_non_ws = c;
                i += 1;
                continue;
            }

            // Handle whitespace
            if (c == ' ' or c == '\t') {
                // Keep single space only if needed between identifiers/keywords
                if (out_len > 0 and needsSpaceBetween(last_non_ws, peekNextNonWs(source, i + 1))) {
                    result[out_len] = ' ';
                    out_len += 1;
                }
                i += 1;
                continue;
            }

            // Handle newlines
            if (c == '\n' or c == '\r') {
                // Keep newline only if it could be a statement terminator
                // (after identifier, number, ), ], ++, --, or string)
                if (needsNewline(last_non_ws)) {
                    // But skip if next non-ws is an operator that continues the statement
                    const next = peekNextNonWs(source, i + 1);
                    if (!continuesStatement(next)) {
                        result[out_len] = '\n';
                        out_len += 1;
                        last_char = '\n';
                        last_non_ws = '\n';
                    }
                }
                i += 1;
                continue;
            }

            // Regular character
            result[out_len] = c;
            out_len += 1;
            last_char = c;
            last_non_ws = c;
            i += 1;
        }

        return result[0..out_len];
    }
}

fn needsSpaceBetween(prev: u8, next: u8) bool {
    // Space needed between two identifiers/keywords
    return isIdentChar(prev) and isIdentChar(next);
}

fn isIdentChar(c: u8) bool {
    return (c >= 'a' and c <= 'z') or (c >= 'A' and c <= 'Z') or
        (c >= '0' and c <= '9') or c == '_' or c == '$';
}

fn needsNewline(c: u8) bool {
    return isIdentChar(c) or c == ')' or c == ']' or c == '}' or
        c == '"' or c == '\'' or c == '`' or c == '+' or c == '-';
}

fn continuesStatement(c: u8) bool {
    return c == '.' or c == ',' or c == ':' or c == '?' or
        c == '+' or c == '-' or c == '*' or c == '/' or c == '%' or
        c == '&' or c == '|' or c == '^' or c == '<' or c == '>' or
        c == '=' or c == '!' or c == '(' or c == '[';
}

fn peekNextNonWs(source: []const u8, start: usize) u8 {
    var i = start;
    while (i < source.len) {
        const c = source[i];
        if (c != ' ' and c != '\t' and c != '\n' and c != '\r') {
            return c;
        }
        i += 1;
    }
    return 0;
}

test "minify removes comments" {
    const input = "var x = 1; // comment\nvar y = 2;";
    const output = comptime minify(input);
    try std.testing.expectEqualStrings("var x=1\nvar y=2", output);
}

test "minify preserves strings" {
    const input = "var x = \"hello // world\";";
    const output = comptime minify(input);
    try std.testing.expectEqualStrings("var x=\"hello // world\"", output);
}
