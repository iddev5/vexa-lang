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

pub fn deinit(ast: *Ast) void {
    ast.tokens.deinit(ast.allocator);
    ast.nodes.deinit(ast.allocator);
    ast.allocator.free(ast.extras);
}

pub fn parse(
    allocator: std.mem.Allocator,
    source: [:0]const u8,
    diagnostics: ?*Diagnostics,
) !Ast {
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
        .diag = diagnostics,
    };
    defer parser.deinit();

    try parser.parse();

    return .{
        .allocator = allocator,
        .source = source,
        .tokens = tokens.toOwnedSlice(),
        .nodes = parser.nodes.toOwnedSlice(),
        .extras = try parser.extras.toOwnedSlice(allocator),
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
        declaration,
        assignment,
        literal,
        identifier,
        identifier_list,
        return_statement,
        break_statement,
        do_statement,

        /// Main token is undefined
        /// Lhs is cond_value i.e pair of condition and a chunk
        /// Rhs is else block
        if_statement,

        /// Main token is undefined
        /// Lhs is condition
        /// Rhs is chunk (block)
        while_statement,

        /// Main token is undefined
        /// Lhs is function_param
        /// Rhs is return type
        function_type,

        /// Main token is undefined
        /// Lhs is symbol name (identifier)
        /// Rhs is type name
        function_param,

        /// Main token is undefined
        /// Lhs is function_type
        /// Rhs is chunk (block)
        function_defn,

        cond_value,
        expression_list,
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
        .chunk, .expression_list, .identifier_list => {
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
    var tree = try Ast.parse(std.testing.allocator, source, null);
    defer tree.deinit();
    var buf: [1024]u8 = undefined;
    var stream = std.io.fixedBufferStream(buf[0..]);
    try tree.printTree(stream.writer());
    try std.testing.expectEqualStrings(expected_ast_dump, stream.getWritten());
}

fn testParserError(source: [:0]const u8, expected_error: []const u8) !void {
    var diag: Diagnostics = .{ .allocator = std.testing.allocator, .source = source };
    defer diag.deinit();
    var tree = Ast.parse(std.testing.allocator, source, &diag) catch |err| switch (err) {
        error.ParsingFailed => {
            try std.testing.expectEqualStrings(expected_error, diag.errors.items[0].message);
            return;
        },
        else => |e| return e,
    };
    defer tree.deinit();
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
        \\x = 12
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
        \\  h = 15
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
        \\  h = 15
        \\else
        \\  t = 45
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
        \\  h = 15
        \\elseif 56 then
        \\  i = 34
        \\else
        \\  t = 45
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

    try testParser(
        \\chunk
        \\  return_statement
        \\    expression_list
        \\      literal
        \\      identifier
        \\
    ,
        \\return 12, hi
    );
}

test "while_statement" {
    try testParser(
        \\chunk
        \\  while_statement
        \\    literal
        \\    chunk
        \\      break_statement
        \\
    ,
        \\while true do
        \\    break
        \\end
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
        \\  l = 12
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

test "declaration" {
    try testParser(
        \\chunk
        \\  declaration
        \\    identifier
        \\    literal
        \\
    ,
        \\hello := 23
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

test "multi assignment" {
    try testParser(
        \\chunk
        \\  assignment
        \\    identifier
        \\    expression_list
        \\      literal
        \\      identifier
        \\      literal
        \\
    ,
        \\hello = 19, hi, true
    );

    try testParser(
        \\chunk
        \\  assignment
        \\    expression_list
        \\      identifier
        \\      identifier
        \\    expression_list
        \\      literal
        \\      identifier
        \\      literal
        \\
    ,
        \\hello, hii = 19, hi, true
    );
}

test "function" {
    try testParser(
        \\chunk
        \\  declaration
        \\    identifier
        \\    function_defn
        \\      function_type
        \\        function_param
        \\          identifier
        \\          identifier
        \\        identifier
        \\      chunk
        \\        break_statement
        \\
    ,
        \\function hello(i: float) float
        \\    break
        \\end
    );

    try testParser(
        \\chunk
        \\  declaration
        \\    identifier
        \\    function_defn
        \\      function_type
        \\        function_param
        \\          identifier
        \\          identifier
        \\        identifier
        \\      chunk
        \\        declaration
        \\          identifier
        \\          function_defn
        \\            function_type
        \\              function_param
        \\                identifier
        \\                identifier
        \\              identifier
        \\            chunk
        \\              return_statement
        \\                literal
        \\        return_statement
        \\          literal
        \\
    ,
        \\function abc(i: float) float
        \\    function def(j: float) float
        \\        return 10
        \\    end
        \\    return 20
        \\end
    );
}

test "assignment error" {
    try testParserError(
        \\x =
    , "expected 'expression', found 'EOF'");

    try testParserError(
        \\x = while
    , "expected 'expression', found 'while'");

    try testParserError(
        \\x =
    , "expected 'expression', found 'EOF'");

    try testParserError(
        \\x = if
    , "expected 'expression', found 'if'");
}
