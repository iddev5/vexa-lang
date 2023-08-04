const Ast = @This();

const std = @import("std");
const Parser = @import("Parser.zig");
const Tokenizer = @import("Tokenizer.zig");
const Token = Tokenizer.Token;

source: [:0]const u8,
tokens: Token.Slice,
nodes: Node.Slice,

pub fn parse(allocator: std.mem.Allocator, source: [:0]const u8) !Ast {
    var tokens: Token.List = .{};
    errdefer tokens.deinit(allocator);

    var tokenizer = Tokenizer.init(source);
    while (true) {
        const token = tokenizer.next();
        try tokens.append(allocator, token);
        if (token.tag == .eof) break;
    }

    var parser = Parser{
        .allocator = allocator,
        .source = source,
        .tokens = tokens,
    };

    try parser.parse();

    return .{
        .source = source,
        .tokens = tokens.toOwnedSlice(),
        .nodes = parser.nodes.toOwnedSlice(),
    };
}

pub const Node = struct {
    tag: Tag,
    main_token: Token.Index,
    lhs: Token.Index = invalid,
    rhs: Token.Index = invalid,

    pub const List = std.MultiArrayList(Node);
    pub const Slice = List.Slice;
    pub const Index = u32;

    pub const invalid = std.math.maxInt(Index);

    pub const Tag = enum {
        chunk,
        assignment,
        literal,
        identifier,
        unary_expression,
        binary_expression,
    };
};

fn printNode(tree: *Ast, writer: anytype, node_idx: Node.Index, indent: u8) !void {
    if (node_idx == Node.invalid)
        return;

    const node = tree.nodes.get(node_idx);
    _ = try writer.writeByteNTimes(' ', indent * 4);
    try writer.print("- {s}\n", .{@tagName(node.tag)});
    try tree.printNode(writer, node.lhs, indent + 1);
    try tree.printNode(writer, node.rhs, indent + 1);
}

pub fn printAst(tree: *Ast, writer: anytype) !void {
    try tree.printNode(writer, 0, 0);
}
