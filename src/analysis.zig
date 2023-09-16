const std = @import("std");
const Air = @import("Air.zig");
const Inst = Air.Inst;
const Ast = @import("Ast.zig");
const Node = Ast.Node;
const Diagnostics = @import("Diagnostics.zig");
const Token = @import("Tokenizer.zig").Token;

pub fn gen(tree: *Ast, diag: ?*Diagnostics) !Air {
    const allocator = tree.allocator;
    var anl = Analyzer{
        .tree = tree,
        .allocator = allocator,
        .diag = diag,
    };
    defer anl.deinit();

    // Create a new scope
    var scope = try Scope.init(anl.allocator, .func, null);
    defer scope.deinit(allocator);
    const start_inst = try anl.genChunk(0, scope);

    const main_fn_type = try anl.addInst(.{ .func_type = .{
        .params = &.{},
        .result = &.{},
    } });

    return .{
        .allocator = allocator,
        .start_inst = start_inst,
        .instructions = try anl.instructions.toOwnedSlice(allocator),
        .globals = try allocator.dupe(Air.ValueType, anl.globals.entries.items(.value)),
        .locals = try scope.types.toOwnedSlice(allocator),
        .main_fn_type = main_fn_type,
    };
}

const Symbol = struct {
    id: u16,
    ty: Air.ValueType,
    global: bool,
};

const Scope = struct {
    ty: Type,
    parent: ?*Scope,
    locals: std.StringArrayHashMapUnmanaged(usize) = .{},
    types: std.ArrayListUnmanaged(Air.ValueType) = .{},

    const Type = enum { func, other };

    pub fn init(allocator: std.mem.Allocator, ty: Type, parent: ?*Scope) !*Scope {
        var scope = try allocator.create(Scope);
        scope.* = .{ .ty = ty, .parent = parent };
        return scope;
    }

    pub fn deinit(scope: *Scope, allocator: std.mem.Allocator) void {
        scope.locals.deinit(allocator);
        scope.types.deinit(allocator);
        allocator.destroy(scope);
    }

    pub fn findNearest(scope: *Scope, ty: Type) ?*Scope {
        return if (scope.ty == ty) scope else if (scope.parent) |par| par.findNearest(ty) else null;
    }
};

const Analyzer = struct {
    tree: *Ast,
    allocator: std.mem.Allocator,
    diag: ?*Diagnostics,
    instructions: std.ArrayListUnmanaged(Inst) = .{},
    globals: std.StringArrayHashMapUnmanaged(Air.ValueType) = .{},
    current_scope: ?*Scope = null,

    const Error = std.mem.Allocator.Error || error{AnalysisFailed};

    fn deinit(anl: *Analyzer) void {
        anl.instructions.deinit(anl.allocator);
        anl.globals.deinit(anl.allocator);
        if (anl.current_scope) |scope| {
            scope.deinit(anl.allocator);
        }
    }

    fn emitError(anl: *Analyzer, loc: Token.Location, comptime tag: Diagnostics.ErrorTag, args: anytype) !void {
        if (anl.diag) |diag|
            try diag.emitError(loc, tag, args);
    }

    fn genChunk(anl: *Analyzer, node: Node.Index, scope: *Scope) Error!Inst.Index {
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

    fn genBlock(anl: *Analyzer, node: Node.Index) Error!Inst.Index {
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

    fn genStatement(anl: *Analyzer, node: Node.Index) Error!Inst.Index {
        const tags = anl.tree.nodes.items(.tag);
        switch (tags[node]) {
            .chunk => return try anl.genBlock(node),
            .declaration => return try anl.genDeclaration(node),
            .assignment => return try anl.genAssignment(node),
            .return_statement => return try anl.genReturn(node),
            .do_statement => return try anl.genDoStat(node),
            .if_statement => return try anl.genIfStat(node),
            .while_statement => return try anl.genWhileStat(node),
            .break_statement => return try anl.addInst(.{ .br = {} }),
            else => {
                const main_token = anl.tree.nodes.items(.main_token)[node];
                try anl.emitError(anl.tree.tokens.items(.loc)[main_token], .invalid_stmt, .{});
                return error.AnalysisFailed;
            },
        }
        unreachable;
    }

    fn getSymbol(anl: *Analyzer, scope: *Scope, symbol: []const u8) ?Symbol {
        var result = Symbol{ .id = 0, .ty = .void, .global = false };

        var check_scope = scope;
        while (true) {
            if (check_scope.locals.get(symbol)) |index| {
                const nearest_func = check_scope.findNearest(.func).?;
                result.id = @intCast(index);
                result.ty = nearest_func.types.items[index];
                return result;
            }

            check_scope = check_scope.parent orelse break;
        }

        result.id = @intCast(anl.globals.getIndex(symbol) orelse return null);
        result.ty = anl.globals.get(symbol) orelse unreachable;
        result.global = true;

        return result;
    }

    fn GenAssignCommon(comptime func: anytype) fn (*Analyzer, Node.Index) Error!Inst.Index {
        return struct {
            fn f(anl: *Analyzer, node: Node.Index) Error!Inst.Index {
                const node_val = anl.tree.nodes.get(node);
                const ident_list = anl.tree.nodes.get(node_val.lhs);

                if (ident_list.tag == .expression_list) {
                    const locs = anl.tree.tokens.items(.loc);
                    const num_ident = ident_list.rhs - ident_list.lhs;
                    const expr_list = anl.tree.nodes.get(node_val.rhs);
                    const num_expr = expr_list.rhs - expr_list.lhs;

                    if (expr_list.tag != .expression_list) {
                        // TODO: it could also be an iterable but those havent yet been implemented
                        unreachable;
                    }

                    if (num_ident > num_expr) {
                        try anl.emitError(locs[ident_list.main_token], .unpack_error, .{ num_ident, num_expr });
                        return error.AnalysisFailed;
                    }

                    const main_tokens = anl.tree.nodes.items(.main_token);
                    var first_decl: ?Inst.Index = 0;

                    var i: usize = 0;
                    while (i < num_ident) : (i += 1) {
                        const ident = anl.tree.extras[ident_list.lhs + i];
                        const expr = anl.tree.extras[expr_list.lhs + i];

                        const decl = try func(anl, main_tokens[ident], expr);
                        first_decl = first_decl orelse decl;
                    }
                    return first_decl.?;
                }

                return try func(anl, ident_list.main_token, node_val.rhs);
            }
        }.f;
    }

    const genDeclaration = GenAssignCommon(makeDeclaration);
    const genAssignment = GenAssignCommon(makeAssignment);

    fn makeDeclaration(anl: *Analyzer, ident: Token.Index, value_node: Inst.Index) !Inst.Index {
        const locs = anl.tree.tokens.items(.loc);
        const slice = anl.tree.tokens.get(ident).slice(anl.tree.source);
        const value = try anl.genExpression(value_node);

        if (anl.current_scope.?.parent == null) {
            var result = try anl.globals.getOrPut(anl.allocator, slice);
            if (result.found_existing) {
                try anl.emitError(locs[ident], .redecl_global, .{slice});
            }

            // If its a function, just return the index and exit early
            if (anl.instructions.items[value] == .func) {
                return value;
            }

            result.value_ptr.* = anl.getType(value);
            const index = @as(u16, @intCast(anl.globals.count())) - 1;
            return try anl.addInst(.{ .global_set = .{
                .index = index,
                .value = value,
            } });
        }

        // Check if the symbol already exists
        if (anl.getSymbol(anl.current_scope.?, slice) != null) {
            try anl.emitError(locs[ident], .redecl_local, .{slice});
        }

        // Find nearest function scope
        var scope = anl.current_scope.?.findNearest(.func) orelse unreachable;
        try scope.types.append(anl.allocator, anl.getType(value));

        const index = @as(u16, @intCast(scope.types.items.len)) - 1;
        try anl.current_scope.?.locals.putNoClobber(anl.allocator, slice, index);
        const payload: Air.Inst.SetValue = .{ .index = index, .value = value };

        return try anl.addInst(.{ .local_set = payload });
    }

    fn makeAssignment(anl: *Analyzer, ident: Token.Index, value_node: Inst.Index) !Inst.Index {
        const locs = anl.tree.tokens.items(.loc);
        const slice = anl.tree.tokens.get(ident).slice(anl.tree.source);
        const value = try anl.genExpression(value_node);

        const symbol = anl.getSymbol(anl.current_scope.?, slice) orelse {
            try anl.emitError(locs[ident], .undecl_ident, .{slice});
            return error.AnalysisFailed;
        };

        const val_ty = anl.getType(value);
        if (symbol.ty != val_ty) {
            const main_token = anl.tree.nodes.items(.main_token)[value_node];
            try anl.emitError(locs[main_token], .expected_ty, .{ @tagName(symbol.ty), @tagName(val_ty) });
        }

        const payload: Air.Inst.SetValue = .{ .index = symbol.id, .value = value };

        if (symbol.global)
            return try anl.addInst(.{ .global_set = payload });

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

    fn genDoStat(anl: *Analyzer, node: Node.Index) Error!Inst.Index {
        const lhs = anl.tree.nodes.items(.lhs)[node];
        return try anl.addInst(.{ .block_do = try anl.genBlock(lhs) });
    }

    fn genIfStat(anl: *Analyzer, node: Node.Index) !Inst.Index {
        const node_val = anl.tree.nodes.get(node);
        const pair_val = anl.tree.nodes.get(node_val.lhs);

        const cond = try anl.addInst(.{ .cond = .{ .cond = 0, .result = 0, .else_blk = 0 } });
        var stat = anl.instructions.items[cond];

        stat.cond.cond = try anl.genExpression(pair_val.lhs);
        stat.cond.result = try anl.genBlock(pair_val.rhs);
        stat.cond.else_blk = if (node_val.rhs != Node.invalid) blk: {
            const tags = anl.tree.nodes.items(.tag);
            var stmt: ?Inst.Index = null;
            if (tags[node_val.rhs] == .if_statement) {
                stmt = try anl.addInst(.{ .stmt = {} });
            }

            const block = try anl.genStatement(node_val.rhs);
            break :blk stmt orelse block;
        } else try anl.addInst(.{ .nop = {} });

        anl.instructions.items[cond] = stat;

        return cond;
    }

    fn genWhileStat(anl: *Analyzer, node: Node.Index) !Inst.Index {
        const node_val = anl.tree.nodes.get(node);

        return try anl.addInst(.{ .loop = .{
            .cond = try anl.genExpression(node_val.lhs),
            .block = try anl.genBlock(node_val.rhs),
        } });
    }

    fn getType(anl: *Analyzer, node: Node.Index) Air.ValueType {
        const inst = anl.instructions.items[node];
        return switch (inst) {
            .nop => .void,
            .float => .float,
            .bool => .bool,
            .func => .func,
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
            .function_defn => return try anl.genFunction(node),
            .binary_expression => return try anl.genBinaryExpr(node),
            .unary_expression => return try anl.genUnaryExpr(node),
            .literal => return try anl.genLiteral(node),
            .identifier => return try anl.genIdentifier(node),
            else => return try anl.addInst(.{ .nop = {} }),
        }
        unreachable;
    }

    fn genFunction(anl: *Analyzer, node: Node.Index) !Inst.Index {
        const node_val = anl.tree.nodes.get(node);
        const fn_type = try anl.genFunctionType(node_val.rhs);

        var scope = try Scope.init(anl.allocator, .func, anl.current_scope.?);
        defer scope.deinit(anl.allocator);
        const start_inst = try anl.genChunk(node_val.rhs, scope);

        return try anl.addInst(.{ .func = .{
            .fn_type = fn_type,
            .start_inst = start_inst,
            .inst_len = @intCast(anl.instructions.items.len - start_inst),
            .locals = try scope.types.toOwnedSlice(anl.allocator),
        } });
    }

    fn genFunctionType(anl: *Analyzer, node: Node.Index) !Inst.Index {
        _ = node;
        // const node_val = anl.tree.nodes.get(node);
        // const main_tokens = anl.tree.nodes.items(.main_token);
        // const tokens = anl.tree.tokens;
        // TODO: need a better type parsing system
        // const param = tokens.get(main_tokens[node_val.lhs]).slice(anl.tree.source);
        // const result = tokens.get(main_tokens[node_val.rhs]).slice(anl.tree.source);

        return try anl.addInst(.{ .func_type = .{
            .params = &.{.float},
            .result = &.{.float},
        } });
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
            .not_equal,
            => valid_op = lhs_type == rhs_type, // Applicable on all values of same type
            else => unreachable,
        }

        if (!valid_op) {
            try anl.emitError(
                anl.tree.tokens.items(.loc)[node_val.main_token],
                .invalid_bin_op,
                .{ op_tag.symbol(), @tagName(lhs_type), @tagName(rhs_type) },
            );
        }

        var result_ty = lhs_type;
        switch (op_tag) {
            .equal_equal,
            .not_equal,
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
            .not_equal => .{ .not_equal = payload },
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

    fn genIdentifier(anl: *Analyzer, node: Node.Index) !Inst.Index {
        const locs = anl.tree.tokens.items(.loc);
        const node_main = anl.tree.nodes.items(.main_token)[node];
        const token_val = anl.tree.tokens.get(node_main);
        const ident = token_val.slice(anl.tree.source);

        const symbol = anl.getSymbol(anl.current_scope.?, ident) orelse {
            try anl.emitError(locs[node_main], .undecl_ident, .{ident});
            return error.AnalysisFailed;
        };

        return try anl.addInst(.{ .ident = .{
            .index = symbol.id,
            .result_ty = symbol.ty,
            .global = symbol.global,
        } });
    }

    fn addInst(anl: *Analyzer, inst: Inst) Error!Inst.Index {
        try anl.instructions.append(anl.allocator, inst);
        return @intCast(anl.instructions.items.len - 1);
    }
};

pub fn printAir(air: *Air, writer: anytype) !void {
    for (air.instructions) |inst| {
        try writer.print("{s}\n", .{@tagName(inst)});
    }
}

fn testAir(expected_ir_dump: []const u8, source: [:0]const u8) !void {
    var tree = try Ast.parse(std.testing.allocator, source, null);
    defer tree.deinit();

    var air = try gen(&tree, null);
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
        \\global_set
        \\
    ,
        \\x := 10 + 20 * 2;
    );
}
