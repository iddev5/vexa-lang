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

    parser.nodes.items(.lhs)[chunk] = try parser.parseStatement();

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
    return try parser.parseSimpleExpr();
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
