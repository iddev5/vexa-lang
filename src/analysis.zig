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

    // Create a new scope
    var scope = try Scope.init(anl.allocator, .func, null);
    defer scope.deinit(allocator);
    const start_inst = try anl.genChunk(0, scope);

    return .{
        .allocator = allocator,
        .start_inst = start_inst,
        .instructions = try anl.instructions.toOwnedSlice(allocator),
        .globals = anl.globals.entries.items(.value),
        .locals = scope.locals.entries.items(.value),
    };
}

const Scope = struct {
    ty: Type,
    parent: ?*Scope,
    locals: std.StringArrayHashMapUnmanaged(Air.ValueType) = .{},

    const Type = enum { func, other };

    pub fn init(allocator: std.mem.Allocator, ty: Type, parent: ?*Scope) !*Scope {
        var scope = try allocator.create(Scope);
        scope.* = .{ .ty = ty, .parent = parent };
        return scope;
    }

    pub fn deinit(scope: *Scope, allocator: std.mem.Allocator) void {
        allocator.destroy(scope);
    }

    pub fn findNearest(scope: *Scope, ty: Type) ?*Scope {
        return if (scope.ty == ty) scope else if (scope.parent) |par| par.findNearest(ty) else null;
    }
};

const Analyzer = struct {
    tree: *Ast,
    allocator: std.mem.Allocator,
    instructions: std.ArrayListUnmanaged(Inst) = .{},
    globals: std.StringArrayHashMapUnmanaged(Air.ValueType) = .{},
    current_scope: ?*Scope = null,

    const Error = std.mem.Allocator.Error;

    fn genChunk(anl: *Analyzer, node: Node.Index, scope: *Scope) anyerror!Inst.Index {
        const node_val = anl.tree.nodes.get(node);
        const extras = anl.tree.extras;

        anl.current_scope = scope;
        defer anl.current_scope = anl.current_scope.?.parent;

        const first_inst = try anl.genStatement(extras[node_val.lhs]);

        var i = node_val.lhs + 1;
        while (i < node_val.rhs) : (i += 1) {
            _ = try anl.genStatement(extras[i]);
        }

        return first_inst;
    }

    fn genBlock(anl: *Analyzer, node: Node.Index) !Inst.Index {
        const scope = try Scope.init(anl.allocator, .other, anl.current_scope);
        defer scope.deinit(anl.allocator);

        const block = try anl.addInst(.{ .block = .{
            .start_inst = 0,
            .inst_len = 0,
        } });

        const start_inst: u32 = @intCast(anl.instructions.items.len);
        _ = try anl.genChunk(node, scope);

        var block_inst = anl.instructions.items[block].block;
        block_inst.start_inst = start_inst;
        block_inst.inst_len = @intCast(anl.instructions.items.len - start_inst);

        anl.instructions.items[block].block = block_inst;
        return block;
    }

    fn genStatement(anl: *Analyzer, node: Node.Index) !Inst.Index {
        const tags = anl.tree.nodes.items(.tag);
        switch (tags[node]) {
            .assignment => return try anl.genAssignment(node),
            .return_statement => return try anl.genReturn(node),
            .do_statement => return try anl.genDoStat(node),
            .if_statement => return try anl.genIfStat(node),
            else => {},
        }
        unreachable;
    }

    fn genAssignment(anl: *Analyzer, node: Node.Index) !Inst.Index {
        const node_val = anl.tree.nodes.get(node);
        const ident = anl.tree.tokens.get(node_val.lhs).slice(anl.tree.source);
        const value = try anl.genExpression(node_val.rhs);

        if (anl.current_scope.?.parent == null) {
            var result = try anl.globals.getOrPut(anl.allocator, ident);
            if (result.found_existing) {
                // error
            }

            result.value_ptr.* = anl.getType(value);
            const index = @as(u16, @intCast(anl.globals.count())) - 1;
            return try anl.addInst(.{ .global_set = .{
                .index = index,
                .value = value,
            } });
        }

        if (anl.current_scope.?.locals.get(ident) != null) {
            // error
        }

        // Find nearest function scope
        var scope = anl.current_scope.?.findNearest(.func) orelse unreachable; // error

        try scope.locals.putNoClobber(anl.allocator, ident, anl.getType(value));
        const index = @as(u16, @intCast(scope.locals.count())) - 1;
        const payload: Air.Inst.SetValue = .{ .index = index, .value = value };

        return try anl.addInst(.{ .local_set = payload });
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

    fn genDoStat(anl: *Analyzer, node: Node.Index) !Inst.Index {
        const lhs = anl.tree.nodes.items(.lhs)[node];
        return try anl.addInst(.{ .block_do = try anl.genBlock(lhs) });
    }

    fn genIfStat(anl: *Analyzer, node: Node.Index) !Inst.Index {
        const node_val = anl.tree.nodes.get(node);
        const pair_val = anl.tree.nodes.get(node_val.lhs);

        return try anl.addInst(.{ .cond = .{
            .cond = try anl.genExpression(pair_val.lhs),
            .result = try anl.genBlock(pair_val.rhs),
        } });
    }

    fn getType(anl: *Analyzer, node: Node.Index) Air.ValueType {
        const inst = anl.instructions.items[node];
        return switch (inst) {
            .nop => .void,
            .float => .float,
            .bool => .bool,
            inline else => |field| {
                switch (@typeInfo(@TypeOf(field))) {
                    .Struct => if (@hasField(@TypeOf(field), "result_ty")) {
                        return @field(field, "result_ty");
                    },
                    else => {},
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
