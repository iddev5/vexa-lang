const Parser = @This();

const std = @import("std");
const Tokenizer = @import("Tokenizer.zig");
const Token = Tokenizer.Token;
const Ast = @import("Ast.zig");
const Node = Ast.Node;
const Diagnostics = @import("Diagnostics.zig");

allocator: std.mem.Allocator,
source: [:0]const u8,
tokens: Token.List,
diag: Diagnostics = undefined,
nodes: Node.List = .{},
extras: std.ArrayListUnmanaged(Node.Index) = .{},
tok_index: Token.Index = 0,

pub const Error = std.mem.Allocator.Error || error{ParsingFailed};

fn emitErrorLoc(
    parser: *Parser,
    loc: Token.Location,
    comptime tag: Diagnostics.ErrorTag,
    args: anytype,
) !void {
    parser.diag = try Diagnostics.emitError(parser.allocator, loc, tag, args);
}

fn emitError(
    parser: *Parser,
    comptime tag: Diagnostics.ErrorTag,
    args: anytype,
) !void {
    try parser.emitErrorLoc(parser.tokens.items(.loc)[parser.tok_index], tag, args);
}

pub fn parse(parser: *Parser) !void {
    _ = try parser.parseChunk();
}

fn addNode(parser: *Parser, node: Node) !Node.Index {
    const res = @as(Node.Index, @intCast(parser.nodes.len));
    try parser.nodes.append(parser.allocator, node);
    return res;
}

fn parseChunk(parser: *Parser) Error!Node.Index {
    var chunk = try parser.addNode(.{
        .tag = .chunk,
        .main_token = 0,
    });

    const tags = parser.tokens.items(.tag);

    // TODO: allocate less
    var stmt_list: std.ArrayListUnmanaged(Node.Index) = .{};
    defer stmt_list.deinit(parser.allocator);

    var i: u32 = 0;
    while (!blockFollow(tags[parser.tok_index])) : (i += 1) {
        const stmt = try parser.parseStatement();
        try stmt_list.append(parser.allocator, stmt);

        if (tags[parser.tok_index] == .semicolon)
            parser.tok_index += 1;
    }

    try parser.extras.appendSlice(parser.allocator, stmt_list.items);

    parser.nodes.items(.lhs)[chunk] = @as(u32, @intCast(parser.extras.items.len)) - i;
    parser.nodes.items(.rhs)[chunk] = @intCast(parser.extras.items.len);

    return chunk;
}

// TODO: move to lexer?
fn blockFollow(tag: Token.Tag) bool {
    switch (tag) {
        .keyword_else,
        .keyword_elseif,
        .keyword_end,
        .keyword_until,
        .eof,
        => return true,
        else => {},
    }

    return false;
}

fn parseStatement(parser: *Parser) !Node.Index {
    const tags = parser.tokens.items(.tag);
    switch (tags[parser.tok_index]) {
        .keyword_if => return try parser.parseIfStatement(),
        .keyword_return => return try parser.parseRetStatement(),
        .keyword_while,
        .keyword_for,
        .keyword_repeat,
        .keyword_function,
        => unreachable,
        .keyword_break => {
            parser.tok_index += 1;
            return try parser.addNode(.{
                .tag = .break_statement,
                .main_token = 0,
            });
        },
        .keyword_do => {
            parser.tok_index += 1;
            const chunk = try parser.parseChunk();
            try parser.expectToken(.keyword_end);
            return parser.addNode(.{
                .tag = .do_statement,
                .main_token = 0,
                .lhs = chunk,
            });
        },
        .keyword_local => {
            parser.tok_index += 1;
            if (tags[parser.tok_index] == .keyword_function)
                unreachable;

            return try parser.parseLocalStmt();
        },
        else => return try parser.parseExprOrStmt(),
    }
    unreachable;
}

fn parseIfStatement(parser: *Parser) !Node.Index {
    const if_tree = try parser.parseIfTree();
    try parser.expectToken(.keyword_end);
    return if_tree;
}

fn parseIfTree(parser: *Parser) !Node.Index {
    const tags = parser.tokens.items(.tag);

    // Ignore the 'IF' or 'ELSEIF' keyword
    parser.tok_index += 1;
    const if_cont = try parser.parseCondValue();

    const else_cont = switch (tags[parser.tok_index]) {
        .keyword_elseif => try parser.parseIfTree(),
        .keyword_else => blk: {
            parser.tok_index += 1;
            break :blk try parser.parseChunk();
        },
        else => Node.invalid,
    };

    return try parser.addNode(.{
        .tag = .if_statement,
        .main_token = undefined,
        .lhs = if_cont,
        .rhs = else_cont,
    });
}

fn parseCondValue(parser: *Parser) !Node.Index {
    const main_token = parser.tok_index - 1;

    const cond = try parser.parseBinaryExpr(0); // TODO: is this correct?
    try parser.expectToken(.keyword_then);
    const chunk = try parser.parseChunk();

    return try parser.addNode(.{
        .tag = .cond_value,
        .main_token = main_token,
        .lhs = cond,
        .rhs = chunk,
    });
}

fn parseRetStatement(parser: *Parser) !Node.Index {
    const tags = parser.tokens.items(.tag);

    const return_tok = parser.tok_index;
    var ret_val: Node.Index = Node.invalid;
    parser.tok_index += 1;

    if (tags[parser.tok_index] != .semicolon or !blockFollow(tags[parser.tok_index])) {
        // TODO: should be explist
        ret_val = try parser.parseBinaryExpr(0);
    }

    return parser.addNode(.{
        .tag = .return_statement,
        .main_token = return_tok,
        .lhs = ret_val,
    });
}

fn parseLocalStmt(parser: *Parser) !Node.Index {
    const tags = parser.tokens.items(.tag);

    const local_token = parser.tok_index - 1;
    const ident = try parser.expectIdent();
    const value = if (tags[parser.tok_index] == .equal) blk: {
        parser.tok_index += 1;
        break :blk try parser.parseBinaryExpr(0);
    } else Node.invalid;

    return try parser.addNode(.{
        .tag = .assignment,
        .main_token = local_token,
        .lhs = ident,
        .rhs = value,
    });
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
        .ident => return try parser.parseIdent(),
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
            try parser.emitError(.expected_token, .{ "expression", "eof" });
            return error.ParsingFailed;
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
        else => return Node.invalid,
    }
    unreachable;
}

fn parseIdent(parser: *Parser) !Node.Index {
    parser.tok_index += 1;
    return parser.addNode(.{
        .tag = .identifier,
        .main_token = parser.tok_index - 1,
    });
}

fn expectIdent(parser: *Parser) !Node.Index {
    const tags = parser.tokens.items(.tag);
    if (tags[parser.tok_index] != .ident) {
        try parser.emitError(.expected_token, .{ "identifier", "null" });
        return error.ParsingFailed;
    }

    return try parser.parseIdent();
}

fn expectToken(parser: *Parser, tag: Token.Tag) !void {
    if (parser.tokens.items(.tag)[parser.tok_index] != tag) {
        try parser.emitError(.expected_token, .{ "this", "that" });
        return error.ParsingFailed;
    }

    parser.tok_index += 1;
}
