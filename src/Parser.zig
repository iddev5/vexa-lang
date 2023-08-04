const Parser = @This();

const std = @import("std");
const Tokenizer = @import("Tokenizer.zig");
const Token = Tokenizer.Token;
const Ast = @import("Ast.zig");
const Node = Ast.Node;

allocator: std.mem.Allocator,
source: [:0]const u8,
tokens: Token.List,
nodes: Node.List = .{},
tok_index: Token.Index = 0,

pub fn parse(parser: *Parser) !void {
    _ = try parser.parseChunk();
}

fn addNode(parser: *Parser, node: Node) !Node.Index {
    const res = @as(Node.Index, @intCast(parser.nodes.len));
    try parser.nodes.append(parser.allocator, node);
    return res;
}

fn parseChunk(parser: *Parser) !Node.Index {
    var chunk = try parser.addNode(.{
        .tag = .chunk,
        .main_token = 0,
    });

    const stmt = try parser.parseStatement();
    parser.nodes.items(.lhs)[chunk] = stmt;

    return chunk;
}

fn parseStatement(parser: *Parser) !Node.Index {
    switch (parser.tokens.items(.tag)[parser.tok_index]) {
        .keyword_if,
        .keyword_do,
        .keyword_while,
        .keyword_for,
        .keyword_local,
        => unreachable,
        else => return try parser.parseExprOrStmt(),
    }
    unreachable;
}

fn parseExprOrStmt(parser: *Parser) !Node.Index {
    const tags = parser.tokens.items(.tag);

    const main_token = parser.tok_index;
    var expr = try parser.parsePrimaryExpr();

    if (tags[parser.tok_index] == .equal) {
        parser.tok_index += 1;
        expr = try parser.addNode(.{
            .tag = .assignment,
            .main_token = main_token,
            .lhs = expr,
            .rhs = try parser.parseBinaryExpr(0), // TODO: should be explist
        });
    }

    return expr;
}

fn parsePrimaryExpr(parser: *Parser) !Node.Index {
    return try parser.parsePrefixExpr();
}

fn parsePrefixExpr(parser: *Parser) !Node.Index {
    const tags = parser.tokens.items(.tag);
    switch (tags[parser.tok_index]) {
        .ident => {
            parser.tok_index += 1;
            return try parser.addNode(.{
                .tag = .identifier,
                .main_token = parser.tok_index - 1,
            });
        },
        else => unreachable,
    }
    unreachable;
}

const OperatorInfo = struct {
    prec: i8,
    assoc: Assoc = .left,

    const Assoc = enum { left, right };
};

const operator_table = std.enums.directEnumArrayDefault(Token.Tag, OperatorInfo, .{ .prec = -1 }, 0, .{
    .keyword_or = .{ .prec = 10 },

    .keyword_and = .{ .prec = 20 },

    .angle_bracket_left = .{ .prec = 30 },
    .angle_bracket_right = .{ .prec = 30 },
    .angle_bracket_left_equal = .{ .prec = 30 },
    .angle_bracket_right_equal = .{ .prec = 30 },
    .tilde_equal = .{ .prec = 30 },
    .equal_equal = .{ .prec = 30 },

    .dot_dot = .{ .prec = 40, .assoc = .right },

    .plus = .{ .prec = 50 },
    .minus = .{ .prec = 50 },

    .multiply = .{ .prec = 60 },
    .divide = .{ .prec = 60 },
    .mod = .{ .prec = 60 },

    // TODO: exponent has higher prec than unaries
    .exponent = .{ .prec = 70, .assoc = .right },
});

fn parseBinaryExpr(parser: *Parser, min_precedence: i32) !Node.Index {
    var node = try parser.parseUnaryExpr();

    // TODO: associativity
    while (parser.tok_index < parser.tokens.len - 1) {
        const operator_token = parser.tok_index;
        const operator = parser.tokens.items(.tag)[operator_token];
        const info = operator_table[@as(usize, @intCast(@intFromEnum(operator)))];

        if (info.prec < min_precedence) {
            break;
        }

        parser.tok_index += 1;
        const rhs = try parser.parseBinaryExpr(info.prec + 1);
        if (rhs == Node.invalid) {
            // error
        }

        node = try parser.addNode(.{
            .tag = .binary_expression,
            .main_token = operator_token,
            .lhs = node,
            .rhs = rhs,
        });
    }

    return node;
}

fn parseUnaryExpr(parser: *Parser) !Node.Index {
    switch (parser.tokens.items(.tag)[parser.tok_index]) {
        .minus, .hash, .keyword_not => {},
        else => return parser.parseSimpleExpr(),
    }

    parser.tok_index += 1;

    return parser.addNode(.{
        .tag = .unary_expression,
        .main_token = parser.tok_index,
        .lhs = try parser.parseUnaryExpr(),
    });
}

fn parseSimpleExpr(parser: *Parser) !Node.Index {
    switch (parser.tokens.items(.tag)[parser.tok_index]) {
        .num, .keyword_false, .keyword_true, .keyword_nil => {
            parser.tok_index += 1;
            return try parser.addNode(.{
                .tag = .literal,
                .main_token = parser.tok_index - 1,
            });
        },
        .keyword_function => unreachable,
        else => unreachable,
    }
    unreachable;
}
