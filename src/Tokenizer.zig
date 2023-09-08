const Tokenizer = @This();

const std = @import("std");

pub const Token = struct {
    tag: Tag,
    loc: Location,

    pub const List = std.MultiArrayList(Token);
    pub const Slice = List.Slice;
    pub const Index = u32;

    pub const Location = struct {
        start: usize,
        end: usize,

        pub const Coords = struct {
            line: usize,
            column: usize,
            line_loc: Location,
        };

        pub fn getCoords(loc: Location, source: [:0]const u8) Coords {
            var coords: Coords = .{
                .line = 1,
                .column = 1,
                .line_loc = .{ .start = 0, .end = source.len },
            };

            for (source[0..loc.start], 0..) |ch, i| {
                if (ch == '\n') {
                    coords.line += 1;
                    coords.line_loc.start = i + 1;
                }
            }

            for (source[loc.end..], 0..) |ch, i| {
                if (ch == '\n') {
                    coords.line_loc.end = loc.end + i;
                    break;
                }
            }

            coords.column += loc.start - coords.line_loc.start;
            return coords;
        }
    };

    pub fn slice(token: Token, source: [:0]const u8) []const u8 {
        return source[token.loc.start..token.loc.end];
    }

    pub const Tag = enum {
        eof,
        invalid,

        ident,
        num,
        string_literal,

        l_paren,
        r_paren,
        l_brace,
        r_brace,
        l_bracket,
        r_bracket,
        semicolon,
        colon,
        comma,
        plus,
        minus,
        multiply,
        divide,
        mod,
        exponent,
        hash,
        not,
        equal,
        equal_equal,
        not_equal,
        angle_bracket_left,
        angle_bracket_left_equal,
        angle_bracket_right,
        angle_bracket_right_equal,
        dot,
        dot_dot,
        dot_dot_dot,

        keyword_and,
        keyword_break,
        keyword_do,
        keyword_else,
        keyword_elseif,
        keyword_end,
        keyword_false,
        keyword_for,
        keyword_function,
        keyword_if,
        keyword_in,
        keyword_nil,
        keyword_or,
        keyword_return,
        keyword_then,
        keyword_true,
        keyword_while,

        pub fn symbol(tag: Tag) []const u8 {
            return switch (tag) {
                .eof => "EOF",
                .invalid => "invalid bytes",

                .ident => "an identifier",
                .num => "a number",
                .string_literal => "a string literal",

                .l_paren => "(",
                .r_paren => ")",
                .l_brace => "{",
                .r_brace => "}",
                .l_bracket => "[",
                .r_bracket => "]",
                .semicolon => ";",
                .colon => ":",
                .comma => ",",
                .plus => "+",
                .minus => "-",
                .multiply => "*",
                .divide => "/",
                .mod => "%",
                .exponent => "^",
                .hash => "#",
                .not => "!",
                .equal => "=",
                .equal_equal => "==",
                .not_equal => "!=",
                .angle_bracket_left => "<",
                .angle_bracket_right => ">",
                .angle_bracket_left_equal => "<=",
                .angle_bracket_right_equal => ">=",
                .dot => ".",
                .dot_dot => "..",
                .dot_dot_dot => "...",

                else => @tagName(tag)["keyword_".len..],
            };
        }
    };

    pub const keywords = std.ComptimeStringMap(Tag, .{
        .{ "and", .keyword_and },
        .{ "break", .keyword_break },
        .{ "do", .keyword_do },
        .{ "else", .keyword_else },
        .{ "elseif", .keyword_elseif },
        .{ "end", .keyword_end },
        .{ "false", .keyword_false },
        .{ "for", .keyword_for },
        .{ "function", .keyword_function },
        .{ "if", .keyword_if },
        .{ "in", .keyword_in },
        .{ "nil", .keyword_nil },
        .{ "or", .keyword_or },
        .{ "return", .keyword_return },
        .{ "then", .keyword_then },
        .{ "true", .keyword_true },
        .{ "while", .keyword_while },
    });

    pub fn getKeyword(kword: []const u8) ?Tag {
        return keywords.get(kword);
    }
};

source: [:0]const u8,
index: usize = 0,

pub fn init(source: [:0]const u8) Tokenizer {
    return Tokenizer{ .source = source };
}

const State = enum {
    start,
    ident,
    equal,
    not,
    angle_bracket_left,
    angle_bracket_right,
    dot,
    dot_dot,
    hyphen,
    comment,
    num,
    num_decimal,
    num_exponent,
    num_exponent_symbol,
};

/// Parsing is based on https://www.lua.org/manual/5.1/manual.html
pub fn next(tokenizer: *Tokenizer) Token {
    var index: usize = tokenizer.index;
    var state: State = .start;
    var result = Token{
        .tag = .eof,
        .loc = .{
            .start = index,
            .end = undefined,
        },
    };

    // TODO: support for hexadecimal and some special cases
    while (true) : (index += 1) {
        const ch = tokenizer.source[index];
        switch (state) {
            .start => switch (ch) {
                0 => {
                    if (index != tokenizer.source.len) {
                        result.tag = .invalid;
                        index += 1;
                    }
                    break;
                },
                ' ', '\n', '\t', '\r' => result.loc.start = index + 1,
                'a'...'z', 'A'...'Z', '_' => state = .ident,
                '0'...'9' => {
                    state = .num;
                    result.tag = .num;
                },
                '=' => state = .equal,
                '!' => state = .not,
                '<' => state = .angle_bracket_left,
                '>' => state = .angle_bracket_right,
                '.' => state = .dot,
                '-' => state = .hyphen,
                '(' => {
                    result.tag = .l_paren;
                    index += 1;
                    break;
                },
                ')' => {
                    result.tag = .r_paren;
                    index += 1;
                    break;
                },
                '{' => {
                    result.tag = .l_brace;
                    index += 1;
                    break;
                },
                '}' => {
                    result.tag = .r_brace;
                    index += 1;
                    break;
                },
                '[' => {
                    result.tag = .l_bracket;
                    index += 1;
                    break;
                },
                ']' => {
                    result.tag = .r_bracket;
                    index += 1;
                    break;
                },
                ';' => {
                    result.tag = .semicolon;
                    index += 1;
                    break;
                },
                ':' => {
                    result.tag = .colon;
                    index += 1;
                    break;
                },
                ',' => {
                    result.tag = .comma;
                    index += 1;
                    break;
                },
                '+' => {
                    result.tag = .plus;
                    index += 1;
                    break;
                },
                '*' => {
                    result.tag = .multiply;
                    index += 1;
                    break;
                },
                '/' => {
                    result.tag = .divide;
                    index += 1;
                    break;
                },
                '%' => {
                    result.tag = .mod;
                    index += 1;
                    break;
                },
                '^' => {
                    result.tag = .exponent;
                    index += 1;
                    break;
                },
                '#' => {
                    result.tag = .hash;
                    index += 1;
                    break;
                },
                else => {
                    result.tag = .invalid;
                    index += 1;
                    break;
                },
            },
            .ident => switch (ch) {
                'a'...'z', 'A'...'Z', '0'...'9', '_' => {},
                else => {
                    result.tag = .ident;
                    if (Token.getKeyword(tokenizer.source[result.loc.start..index])) |tag| {
                        result.tag = tag;
                    }
                    break;
                },
            },
            .num => switch (ch) {
                '0'...'9' => {},
                '.' => state = .num_decimal,
                'e', 'E' => state = .num_exponent,
                else => break,
            },
            .num_decimal => switch (ch) {
                '0'...'9' => {},
                'e', 'E' => state = .num_exponent,
                else => break,
            },
            .num_exponent => switch (ch) {
                '0'...'9' => {},
                '-', '+' => state = .num_exponent_symbol,
                else => break,
            },
            .num_exponent_symbol => switch (ch) {
                '0'...'9' => {},
                else => break,
            },
            .equal => switch (ch) {
                '=' => {
                    result.tag = .equal_equal;
                    index += 1;
                    break;
                },
                else => {
                    result.tag = .equal;
                    break;
                },
            },
            .not => switch (ch) {
                '=' => {
                    result.tag = .not_equal;
                    index += 1;
                    break;
                },
                else => {
                    result.tag = .not;
                    break;
                },
            },
            .angle_bracket_left => switch (ch) {
                '=' => {
                    result.tag = .angle_bracket_left_equal;
                    index += 1;
                    break;
                },
                else => {
                    result.tag = .angle_bracket_left;
                    break;
                },
            },
            .angle_bracket_right => switch (ch) {
                '=' => {
                    result.tag = .angle_bracket_right_equal;
                    index += 1;
                    break;
                },
                else => {
                    result.tag = .angle_bracket_right;
                    break;
                },
            },
            .dot => switch (ch) {
                '.' => state = .dot_dot,
                else => {
                    result.tag = .dot;
                    break;
                },
            },
            .dot_dot => switch (ch) {
                '.' => {
                    result.tag = .dot_dot_dot;
                    break;
                },
                else => {
                    result.tag = .dot_dot;
                    break;
                },
            },
            .hyphen => switch (ch) {
                '-' => state = .comment,
                else => {
                    result.tag = .minus;
                    break;
                },
            },
            .comment => switch (ch) {
                0 => {
                    if (index != tokenizer.source.len) {
                        result.tag = .invalid;
                        index += 1;
                    }
                    break;
                },
                '\n' => {
                    state = .start;
                    result.loc.start = index + 1;
                },
                else => {},
            },
        }
    }

    result.loc.end = index;
    tokenizer.index = index;
    return result;
}

fn testTokenizer(source: [:0]const u8, tags: []const Token.Tag) !void {
    var tokenizer = Tokenizer.init(source);
    for (tags) |expected_tag| {
        const token = tokenizer.next();
        try std.testing.expectEqual(expected_tag, token.tag);
    }
}

fn testIdentifier(source: [:0]const u8, idents: []const []const u8) !void {
    var tokenizer = Tokenizer.init(source);
    for (idents) |ident| {
        const token = tokenizer.next();
        try std.testing.expectEqualStrings(ident, token.slice(source));
    }
}

test "keywords" {
    try testTokenizer(
        "and while",
        &.{ .keyword_and, .keyword_while },
    );
}

test "ident" {
    try testTokenizer(
        "hello_world _VERSION iD0nt",
        &.{ .ident, .ident, .ident },
    );

    try testIdentifier(
        "hello_world _VERSION iD0nt",
        &.{ "hello_world", "_VERSION", "iD0nt" },
    );
}

test "number" {
    try testTokenizer(
        "10 2.15 76e12 56e-4 54e+3 3.14e-12",
        &[_]Token.Tag{.num} ** 6,
    );
}

test "symbols" {
    try testTokenizer(
        "( ) { } [ ] ; : , + - * / % ^ # ! = == != < <= > >= . .. ...",
        &.{
            .l_paren,
            .r_paren,
            .l_brace,
            .r_brace,
            .l_bracket,
            .r_bracket,
            .semicolon,
            .colon,
            .comma,
            .plus,
            .minus,
            .multiply,
            .divide,
            .mod,
            .exponent,
            .hash,
            .not,
            .equal,
            .equal_equal,
            .not_equal,
            .angle_bracket_left,
            .angle_bracket_left_equal,
            .angle_bracket_right,
            .angle_bracket_right_equal,
            .dot,
            .dot_dot,
            .dot_dot_dot,
        },
    );
}
