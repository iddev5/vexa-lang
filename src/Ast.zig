const Ast = @This();

const std = @import("std");
const Parser = @import("Parser.zig");
const Tokenizer = @import("Tokenizer.zig");
const Token = Tokenizer.Token;

allocator: std.mem.Allocator,
source: [:0]const u8,
tokens: Token.Slice,
nodes: Node.Slice,

pub fn deinit(ast: *Ast) void {
    ast.tokens.deinit(ast.allocator);
    ast.nodes.deinit(ast.allocator);
}

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
        .allocator = allocator,
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

pub fn printNode(tree: *Ast, writer: anytype, node_idx: Node.Index, indent: u8) !void {
    if (node_idx == Node.invalid)
        return;

    const node = tree.nodes.get(node_idx);
    _ = try writer.writeByteNTimes(' ', indent * 2);
    try writer.print("{s}\n", .{@tagName(node.tag)});
    try tree.printNode(writer, node.lhs, indent + 1);
    try tree.printNode(writer, node.rhs, indent + 1);
}

pub fn printTree(tree: *Ast, writer: anytype) !void {
    try tree.printNode(writer, 0, 0);
}

fn testParser(expected_ast_dump: []const u8, source: [:0]const u8) !void {
    var tree = try Ast.parse(std.testing.allocator, source);
    defer tree.deinit();
    var buf: [1024]u8 = undefined;
    var stream = std.io.fixedBufferStream(buf[0..]);
    try tree.printTree(stream.writer());
    try std.testing.expectEqualStrings(expected_ast_dump, stream.getWritten());
}

test "assignment" {
    try testParser(
        \\chunk
        \\  assignment
        \\    identifier
        \\    literal
        \\
    ,
        \\hello = 12
    );

    try testParser(
        \\chunk
        \\  assignment
        \\    identifier
        \\    binary_expression
        \\      binary_expression
        \\        unary_expression
        \\          literal
        \\        literal
        \\      binary_expression
        \\        literal
        \\        literal
        \\
    ,
        \\hello = -12 * 54 + 25 % 4
    );
}
