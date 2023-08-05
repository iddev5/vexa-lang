const Ast = @This();

const std = @import("std");
const Parser = @import("Parser.zig");
const Tokenizer = @import("Tokenizer.zig");
const Token = Tokenizer.Token;
const Diagnostics = @import("Diagnostics.zig");

allocator: std.mem.Allocator,
source: [:0]const u8,
tokens: Token.Slice,
nodes: Node.Slice,
extras: []const Node.Index,
diag: Diagnostics,

pub fn deinit(ast: *Ast) void {
    ast.tokens.deinit(ast.allocator);
    ast.nodes.deinit(ast.allocator);
    ast.allocator.free(ast.extras);
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
        .extras = try parser.extras.toOwnedSlice(allocator),
        .diag = parser.diag,
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
        return_statement,
        break_statement,
        do_statement,
        if_statement,
        cond_value,
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

    switch (node.tag) {
        .chunk => {
            var i = node.lhs;
            while (i < node.rhs) : (i += 1) {
                const child = tree.extras[i];
                try tree.printNode(writer, child, indent + 1);
            }
        },
        else => {
            try tree.printNode(writer, node.lhs, indent + 1);
            try tree.printNode(writer, node.rhs, indent + 1);
        },
    }
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

test "chunk" {
    try testParser(
        \\chunk
        \\  assignment
        \\    identifier
        \\    literal
        \\  assignment
        \\    identifier
        \\    literal
        \\
    ,
        \\local x = 12
        \\h = 13
    );

    try testParser(
        \\chunk
        \\  if_statement
        \\    cond_value
        \\      literal
        \\      chunk
        \\        assignment
        \\          identifier
        \\          literal
        \\  if_statement
        \\    cond_value
        \\      literal
        \\      chunk
        \\        assignment
        \\          identifier
        \\          literal
        \\
    ,
        \\if nil then
        \\  x = false
        \\end
        \\if true then
        \\ i = 12
        \\end
    );
}

test "if_statement" {
    try testParser(
        \\chunk
        \\  if_statement
        \\    cond_value
        \\      literal
        \\      chunk
        \\        assignment
        \\          identifier
        \\          literal
        \\
    ,
        \\if true then
        \\  local h = 15
        \\end
    );

    try testParser(
        \\chunk
        \\  if_statement
        \\    cond_value
        \\      literal
        \\      chunk
        \\        assignment
        \\          identifier
        \\          literal
        \\    chunk
        \\      assignment
        \\        identifier
        \\        literal
        \\
    ,
        \\if true then
        \\  local h = 15
        \\else
        \\  local t = 45
        \\end
    );

    try testParser(
        \\chunk
        \\  if_statement
        \\    cond_value
        \\      literal
        \\      chunk
        \\        assignment
        \\          identifier
        \\          literal
        \\    if_statement
        \\      cond_value
        \\        literal
        \\        chunk
        \\          assignment
        \\            identifier
        \\            literal
        \\      chunk
        \\        assignment
        \\          identifier
        \\          literal
        \\
    ,
        \\if true then
        \\  local h = 15
        \\elseif 56 then
        \\  i = 34
        \\else
        \\  local t = 45
        \\end
    );
}

test "return_statement" {
    try testParser(
        \\chunk
        \\  return_statement
        \\
    ,
        \\return
    );

    try testParser(
        \\chunk
        \\  return_statement
        \\    literal
        \\
    ,
        \\return 12
    );
}

test "do_statement" {
    try testParser(
        \\chunk
        \\  do_statement
        \\    chunk
        \\      assignment
        \\        identifier
        \\        literal
        \\      if_statement
        \\        cond_value
        \\          literal
        \\          chunk
        \\            assignment
        \\              identifier
        \\              literal
        \\
    ,
        \\do
        \\  local l = 12
        \\  if true then
        \\    test = false
        \\  end
        \\end
    );
}

test "break_statement" {
    try testParser(
        \\chunk
        \\  break_statement
        \\
    ,
        \\break
    );
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

test "local assignment" {
    try testParser(
        \\chunk
        \\  assignment
        \\    identifier
        \\
    ,
        \\local ident
    );

    try testParser(
        \\chunk
        \\  assignment
        \\    identifier
        \\    binary_expression
        \\      literal
        \\      unary_expression
        \\        literal
        \\
    ,
        \\local ident = 23 * -34
    );
}
