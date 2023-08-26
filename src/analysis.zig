const std = @import("std");
const Air = @import("Air.zig");
const Inst = Air.Inst;
const Ast = @import("Ast.zig");
const Node = Ast.Node;

pub fn gen(tree: *Ast) !Air {
    const allocator = tree.allocator;
    var anl = Analyzer{
        .tree = tree,
        .allocator = allocator,
    };

    const start_inst = try anl.genChunk(0);

    var funcs: std.ArrayListUnmanaged(Air.FuncBlock) = .{};
    try funcs.append(allocator, .{
        .instructions = try anl.instructions.toOwnedSlice(allocator),
        .start_inst = start_inst,
        .params = &.{},
        .result = &.{},
    });

    return .{
        .allocator = allocator,
        .functions = try funcs.toOwnedSlice(allocator),
    };
}

const Analyzer = struct {
    tree: *Ast,
    allocator: std.mem.Allocator,
    instructions: std.ArrayListUnmanaged(Inst) = .{},

    const Error = std.mem.Allocator.Error;

    fn genChunk(anl: *Analyzer, node: Node.Index) !Inst.Index {
        const node_val = anl.tree.nodes.get(node);
        var i = node_val.lhs;
        while (i < node_val.rhs) : (i += 1) {
            const stmt = anl.tree.extras[i];
            // TODO: multiple statements
            return try anl.genStatement(stmt);
        }

        unreachable;
    }

    fn genStatement(anl: *Analyzer, node: Node.Index) !Inst.Index {
        const tags = anl.tree.nodes.items(.tag);
        switch (tags[node]) {
            .assignment => return try anl.genAssignment(node),
            else => {},
        }
        unreachable;
    }

    fn genAssignment(anl: *Analyzer, node: Node.Index) !Inst.Index {
        const node_val = anl.tree.nodes.get(node);
        return try anl.genExpression(node_val.rhs);
    }

    fn genExpression(anl: *Analyzer, node: Node.Index) Error!Inst.Index {
        const tags = anl.tree.nodes.items(.tag);
        switch (tags[node]) {
            .binary_expression => return try anl.genBinaryExpr(node),
            .literal => return try anl.genLiteral(node),
            else => {},
        }
        unreachable;
    }

    fn genBinaryExpr(anl: *Analyzer, node: Node.Index) !Inst.Index {
        const node_val = anl.tree.nodes.get(node);
        const lhs = try anl.genExpression(node_val.lhs);
        const rhs = try anl.genExpression(node_val.rhs);

        return anl.addInst(switch (anl.tree.tokens.items(.tag)[node_val.main_token]) {
            .plus => .{ .add = .{ .lhs = lhs, .rhs = rhs } },
            .minus => .{ .sub = .{ .lhs = lhs, .rhs = rhs } },
            .multiply => .{ .mul = .{ .lhs = lhs, .rhs = rhs } },
            .divide => .{ .div = .{ .lhs = lhs, .rhs = rhs } },
            else => unreachable,
        });
    }

    fn genLiteral(anl: *Analyzer, node: Node.Index) !Inst.Index {
        const node_val = anl.tree.nodes.get(node);
        const token_val = anl.tree.tokens.get(node_val.main_token);

        switch (token_val.tag) {
            .keyword_true, .keyword_false, .keyword_nil => {},
            else => {
                const number = std.fmt.parseFloat(f64, token_val.slice(anl.tree.source)) catch unreachable;
                return anl.addInst(.{ .float = number });
            },
        }
        unreachable;
    }

    fn addInst(anl: *Analyzer, inst: Inst) !Inst.Index {
        try anl.instructions.append(anl.allocator, inst);
        return @intCast(anl.instructions.items.len - 1);
    }
};

fn printAir(air: *Air, writer: anytype) !void {
    for (air.instructions) |inst| {
        try writer.print("{s}\n", .{@tagName(inst)});
    }
}

fn testAir(expected_ir_dump: []const u8, source: [:0]const u8) !void {
    var tree = try Ast.parse(std.testing.allocator, source, null);
    defer tree.deinit();

    var air = try gen(&tree);
    defer air.deinit();

    var buf: [1024]u8 = undefined;
    var stream = std.io.fixedBufferStream(buf[0..]);
    try printAir(&air, stream.writer());
    try std.testing.expectEqualStrings(expected_ir_dump, stream.getWritten());
}

test "binary op" {
    try testAir(
        \\float
        \\float
        \\float
        \\mul
        \\add
        \\
    ,
        \\local x = 10 + 20 * 2;
    );
}
