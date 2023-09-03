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

    return .{
        .start_inst = try anl.genChunk(0),
        .instructions = try anl.instructions.toOwnedSlice(allocator),
        .allocator = allocator,
    };
}

const Analyzer = struct {
    tree: *Ast,
    allocator: std.mem.Allocator,
    instructions: std.ArrayListUnmanaged(Inst) = .{},
    locals: std.StringArrayHashMapUnmanaged(Air.ValueType) = .{},

    const Error = std.mem.Allocator.Error;

    fn genChunk(anl: *Analyzer, node: Node.Index) !Inst.Index {
        const node_val = anl.tree.nodes.get(node);
        const extras = anl.tree.extras;
        const first_inst = try anl.genStatement(extras[node_val.lhs]);

        var i = node_val.lhs + 1;
        while (i < node_val.rhs) : (i += 1) {
            _ = try anl.genStatement(extras[i]);
        }

        return first_inst;
    }

    fn genStatement(anl: *Analyzer, node: Node.Index) !Inst.Index {
        const tags = anl.tree.nodes.items(.tag);
        switch (tags[node]) {
            .assignment => return try anl.genAssignment(node),
            .return_statement => return try anl.genReturn(node),
            else => {},
        }
        unreachable;
    }

    fn genAssignment(anl: *Analyzer, node: Node.Index) !Inst.Index {
        const node_val = anl.tree.nodes.get(node);
        const ident = anl.tree.tokens.get(node_val.lhs).slice(anl.tree.source);
        const value = try anl.genExpression(node_val.rhs);

        try anl.locals.put(anl.allocator, ident, anl.getType(value));

        return try anl.addInst(.{ .local_set = .{
            .index = @as(u16, @intCast(anl.locals.count())) - 1,
            .value = value,
        } });
    }

    fn genReturn(anl: *Analyzer, node: Node.Index) !Inst.Index {
        const lhs_idx = anl.tree.nodes.items(.lhs)[node];
        const lhs = anl.tree.nodes.get(lhs_idx);

        const value_inst = try anl.genExpression(lhs_idx);
        return switch (lhs.tag) {
            .expression_list => unreachable, // TODO: not supported right now
            else => try anl.addInst(.{ .ret = .{
                .result_ty = anl.getType(value_inst),
                .value = value_inst,
            } }),
        };
    }

    fn getType(anl: *Analyzer, node: Node.Index) Air.ValueType {
        const inst = anl.instructions.items[node];
        return switch (inst) {
            .nop => .void,
            .float => .float,
            .bool => .bool,
            inline else => |field| {
                if (@hasField(@TypeOf(field), "result_ty")) {
                    return @field(field, "result_ty");
                }

                unreachable;
            },
        };
    }

    fn genExpression(anl: *Analyzer, node: Node.Index) Error!Inst.Index {
        const tags = anl.tree.nodes.items(.tag);
        switch (tags[node]) {
            .binary_expression => return try anl.genBinaryExpr(node),
            .unary_expression => return try anl.genUnaryExpr(node),
            .literal => return try anl.genLiteral(node),
            else => return try anl.addInst(.{ .nop = {} }),
        }
        unreachable;
    }

    fn genBinaryExpr(anl: *Analyzer, node: Node.Index) !Inst.Index {
        const tags = anl.tree.tokens.items(.tag);
        const node_val = anl.tree.nodes.get(node);
        const op_tag = tags[node_val.main_token];
        const lhs = try anl.genExpression(node_val.lhs);
        const rhs = try anl.genExpression(node_val.rhs);
        const lhs_type = anl.getType(lhs);
        const rhs_type = anl.getType(rhs);

        var valid_op = false;
        switch (op_tag) {
            .plus,
            .minus,
            .multiply,
            .divide,
            .angle_bracket_left,
            .angle_bracket_left_equal,
            .angle_bracket_right,
            .angle_bracket_right_equal,
            => valid_op = lhs_type == .float and rhs_type == .float,
            .equal_equal,
            .tilde_equal,
            => valid_op = true, // Applicable on all types
            else => unreachable,
        }

        if (!valid_op) {
            unreachable; // TODO: emit error
        }

        var result_ty = lhs_type;
        switch (op_tag) {
            .equal_equal,
            .tilde_equal,
            .angle_bracket_left,
            .angle_bracket_left_equal,
            .angle_bracket_right,
            .angle_bracket_right_equal,
            => result_ty = .bool,
            else => {},
        }

        const payload = Air.Inst.BinaryOp{
            .result_ty = result_ty,
            .lhs = lhs,
            .rhs = rhs,
        };

        return anl.addInst(switch (op_tag) {
            .plus => .{ .add = payload },
            .minus => .{ .sub = payload },
            .multiply => .{ .mul = payload },
            .divide => .{ .div = payload },
            .equal_equal => .{ .equal = payload },
            .tilde_equal => .{ .not_equal = payload },
            .angle_bracket_left => .{ .less_than = payload },
            .angle_bracket_left_equal => .{ .less_equal = payload },
            .angle_bracket_right => .{ .greater_than = payload },
            .angle_bracket_right_equal => .{ .greater_equal = payload },
            else => unreachable,
        });
    }

    fn genUnaryExpr(anl: *Analyzer, node: Node.Index) !Inst.Index {
        const node_val = anl.tree.nodes.get(node);
        const inst = try anl.genExpression(node_val.lhs);

        return anl.addInst(switch (anl.tree.tokens.items(.tag)[node_val.main_token]) {
            .minus => .{ .negate = .{
                .result_ty = anl.getType(inst),
                .inst = inst,
            } },
            else => unreachable,
        });
    }

    fn genLiteral(anl: *Analyzer, node: Node.Index) !Inst.Index {
        const node_val = anl.tree.nodes.get(node);
        const token_val = anl.tree.tokens.get(node_val.main_token);

        switch (token_val.tag) {
            .keyword_nil => {},
            .keyword_true, .keyword_false => return anl.addInst(.{ .bool = token_val.tag == .keyword_true }),
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

pub fn printAir(air: *Air, writer: anytype) !void {
    for (air.functions[0].instructions) |inst| {
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
