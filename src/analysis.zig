const std = @import("std");
const Air = @import("Air.zig");
const Inst = Air.Inst;
const Ast = @import("Ast.zig");
const Node = Ast.Node;
const Diagnostics = @import("Diagnostics.zig");
const Token = @import("Tokenizer.zig").Token;

pub fn gen(tree: *Ast, diag: ?*Diagnostics) !Air {
    const allocator = tree.allocator;

    // Owned by analyzer
    var global = try Scope.init(allocator, .global, null);

    const main_fn_type: Air.Inst.FunctionType = .{
        .params = &.{},
        .result = &.{},
    };

    var anl = Analyzer{
        .tree = tree,
        .allocator = allocator,
        .diag = diag,
        .global_scope = global,
        .current_func = main_fn_type,
    };
    defer anl.deinit();

    // Create a new scope
    var scope = try Scope.init(anl.allocator, .func, null);
    defer scope.deinit(allocator);
    const start_inst = try anl.genChunk(0, scope);

    const main_fn_type_node = try anl.addInst(.{ .func_type = main_fn_type });

    return .{
        .allocator = allocator,
        .start_inst = start_inst,
        .instructions = try anl.instructions.toOwnedSlice(allocator),
        .globals = try global.types.toOwnedSlice(allocator),
        .locals = try scope.types.toOwnedSlice(allocator),
        .main_fn_type = main_fn_type_node,
    };
}

const Symbol = struct {
    id: u16,
    ty: Air.ValueType,
    scope: *Scope,

    const invalid = std.math.maxInt(u16);

    fn isGlobal(sym: Symbol) bool {
        return sym.scope.ty == .global;
    }
};

const Scope = struct {
    ty: Type,
    parent: ?*Scope,
    locals: std.StringArrayHashMapUnmanaged(usize) = .{},
    // Only used when ty == .func or .global
    types: std.ArrayListUnmanaged(Air.ValueType) = .{},

    const Type = enum { global, func, other };

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
        return if (scope.ty == ty or scope.ty == .global)
            scope
        else if (scope.parent) |par|
            par.findNearest(ty)
        else
            null;
    }
};

const Analyzer = struct {
    tree: *Ast,
    allocator: std.mem.Allocator,
    diag: ?*Diagnostics,
    instructions: std.ArrayListUnmanaged(Inst) = .{},
    current_scope: ?*Scope = null,
    global_scope: *Scope,
    current_func: Air.Inst.FunctionType,
    func_count: u32 = 1,

    const Error = std.mem.Allocator.Error || error{AnalysisFailed};

    fn deinit(anl: *Analyzer) void {
        anl.instructions.deinit(anl.allocator);
        anl.global_scope.deinit(anl.allocator);
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
        var check_scope = scope;
        while (true) {
            if (check_scope.locals.get(symbol)) |index| {
                // If the parent of checked scope doesnt exists, its the scope
                // of the main function, so look up the symbol type in the global
                // scope instead
                const nearest_func = if (check_scope.parent == null)
                    anl.global_scope
                else
                    check_scope.findNearest(.func).?;

                return .{
                    .id = @intCast(index),
                    .ty = if (index == Symbol.invalid) .void else nearest_func.types.items[index],
                    .scope = nearest_func,
                };
            }

            // If its global scope, we have already reached the end of chai
            // otherwise check in parent scope, if there's no parent scope
            // finally check in the global scope
            check_scope = if (check_scope.ty == .global)
                break
            else
                check_scope.parent orelse anl.global_scope;
        }

        return null;
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

        // Check if the symbol already exists
        if (anl.getSymbol(anl.current_scope.?, slice)) |sym| {
            if (sym.isGlobal())
                try anl.emitError(locs[ident], .redecl_global, .{slice})
            else
                try anl.emitError(locs[ident], .redecl_local, .{slice});
            return error.AnalysisFailed;
        }

        // Find nearest function scope
        var scope = if (anl.current_scope.?.parent == null)
            anl.global_scope
        else
            anl.current_scope.?.findNearest(.func) orelse unreachable;

        // Insert a garbage index
        // Function declarations are handled as declarations. This means that
        // the body is one whole inst and the name is another. Without inserting
        // a garbage index (i.e declaring the variable uninitialized), it would be
        // possible to redeclare a variable or function with the same name as current
        // function inside the current function.
        // There are other use cases for it too, like struct fields having pointer
        // to self, this needs to be handled somewhere else
        try anl.current_scope.?.locals.putNoClobber(anl.allocator, slice, Symbol.invalid);

        const value = try anl.genExpression(value_node);
        const value_ty = anl.getType(value);

        try scope.types.append(anl.allocator, value_ty);
        const index = @as(u16, @intCast(scope.types.items.len - anl.current_func.params.len)) - 1;

        const inst = anl.instructions.items[value];
        if (std.meta.activeTag(inst) == .func) {
            // For functions, get the index from the actual function instruction
            anl.current_scope.?.locals.putAssumeCapacity(slice, inst.func.id);
        } else if (value_ty == .func) {
            // For identifiers pointing to function, we need to get the
            // index held by the identifier instruction
            anl.current_scope.?.locals.putAssumeCapacity(slice, inst.ident.index);
        } else anl.current_scope.?.locals.putAssumeCapacity(slice, index);
        const payload: Air.Inst.SetValue = .{ .index = index, .value = value };

        if (scope.ty == .global)
            return try anl.addInst(.{ .global_set = payload });

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
        if (!symbol.ty.eql(val_ty)) {
            const main_token = anl.tree.nodes.items(.main_token)[value_node];
            try anl.emitError(locs[main_token], .expected_ty, .{ @tagName(symbol.ty), @tagName(val_ty) });
        }

        const payload: Air.Inst.SetValue = .{ .index = symbol.id, .value = value };

        if (symbol.isGlobal())
            return try anl.addInst(.{ .global_set = payload });

        return try anl.addInst(.{ .local_set = payload });
    }

    fn genReturn(anl: *Analyzer, node: Node.Index) !Inst.Index {
        const lhs_idx = anl.tree.nodes.items(.lhs)[node];
        const lhs = anl.tree.nodes.get(lhs_idx);

        const value_inst = try anl.genExpression(lhs_idx);
        const value_ty = anl.getType(value_inst);

        if (!value_ty.eql(anl.current_func.result[0])) {
            const locs = anl.tree.tokens.items(.loc);
            const main_token = anl.tree.nodes.items(.main_token)[lhs_idx];
            try anl.emitError(locs[main_token], .expected_ty, .{ @tagName(anl.current_func.result[0]), @tagName(value_ty) });
        }

        return switch (lhs.tag) {
            .expression_list => unreachable, // TODO: not supported right now
            else => try anl.addInst(.{ .ret = value_inst }),
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

    fn getType(anl: *Analyzer, node: Inst.Index) Air.ValueType {
        const inst = anl.instructions.items[node];
        return switch (inst) {
            .nop => .void,
            .float => .float,
            .bool => .bool,
            .func => |func| Air.ValueType{
                .func = anl.instructions.items[func.fn_type].func_type,
            },
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
            .function_call => return try anl.genCall(node),
            .binary_expression => return try anl.genBinaryExpr(node),
            .unary_expression => return try anl.genUnaryExpr(node),
            .literal => return try anl.genLiteral(node),
            .identifier => return try anl.genIdentifier(node),
            else => return try anl.addInst(.{ .nop = {} }),
        }
        unreachable;
    }

    const builtin_types = std.ComptimeStringMap(Air.ValueType, .{
        .{ "float", .float },
        .{ "bool", .bool },
        .{ "void", .void },
    });

    fn genFunction(anl: *Analyzer, node: Node.Index) !Inst.Index {
        const node_val = anl.tree.nodes.get(node);

        var scope = try Scope.init(anl.allocator, .func, anl.current_scope.?);
        defer scope.deinit(anl.allocator);

        const type_val = anl.tree.nodes.get(node_val.lhs);
        const main_tokens = anl.tree.nodes.items(.main_token);
        const lhss = anl.tree.nodes.items(.lhs);
        const rhss = anl.tree.nodes.items(.rhs);
        const tokens = anl.tree.tokens;

        // Process result type
        const result = tokens.get(main_tokens[type_val.rhs]).slice(anl.tree.source);
        const result_ty = builtin_types.get(result) orelse unreachable;
        try scope.types.append(anl.allocator, result_ty);
        const num_results = scope.types.items.len;

        // Process param name and type
        const params = anl.tree.nodes.get(type_val.lhs);
        if (params.tag == .expression_list) {
            var i = params.lhs;
            while (i < params.rhs) : (i += 1) {
                const param = anl.tree.extras[i];
                const param_name = tokens.get(main_tokens[lhss[param]]).slice(anl.tree.source);
                const param_type = tokens.get(main_tokens[rhss[param]]).slice(anl.tree.source);

                try scope.types.append(anl.allocator, builtin_types.get(param_type) orelse unreachable);
                try scope.locals.putNoClobber(anl.allocator, param_name, scope.types.items.len - num_results - 1);
            }
        } else {
            const param_name = tokens.get(main_tokens[params.lhs]).slice(anl.tree.source);
            const param_type = tokens.get(main_tokens[params.rhs]).slice(anl.tree.source);

            try scope.types.append(anl.allocator, builtin_types.get(param_type) orelse unreachable);
            try scope.locals.putNoClobber(anl.allocator, param_name, scope.types.items.len - num_results - 1);
        }

        const num_params_and_res = scope.types.items.len;

        // Create the function type
        const fn_type: Air.Inst.FunctionType = .{
            .params = scope.types.items[num_results..],
            .result = scope.types.items[0..num_results],
        };

        const fn_type_node = try anl.addInst(.{ .func_type = fn_type });

        // Store the current function type
        const current_fn = anl.current_func;
        anl.current_func = fn_type;
        defer anl.current_func = current_fn;

        const block = try anl.addInst(.{ .block = .{
            .start_inst = 0,
            .inst_len = 0,
        } });

        const start_inst: u32 = @intCast(anl.instructions.items.len);
        _ = try anl.genChunk(node_val.rhs, scope);

        var block_inst = anl.instructions.items[block].block;
        block_inst.start_inst = start_inst;
        block_inst.inst_len = @intCast(anl.instructions.items.len - start_inst);

        anl.instructions.items[block].block = block_inst;
        anl.func_count += 1;

        return try anl.addInst(.{ .func = .{
            .id = anl.func_count - 1,
            .fn_type = fn_type_node,
            .start_inst = block_inst.start_inst,
            .inst_len = block_inst.inst_len,
            .locals = (try scope.types.toOwnedSlice(anl.allocator))[num_params_and_res..],
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
            .params = &.{},
            .result = &.{},
        } });
    }

    fn genCall(anl: *Analyzer, node: Node.Index) !Inst.Index {
        const node_val = anl.tree.nodes.get(node);
        const main_tokens = anl.tree.nodes.items(.main_token);
        const ident = anl.tree.tokens.get(main_tokens[node_val.lhs]).slice(anl.tree.source);
        const args = anl.tree.nodes.get(node_val.rhs);

        const symbol = anl.getSymbol(anl.current_scope.?, ident) orelse {
            // TODO: error
            return error.AnalysisFailed;
        };

        var args_idx: u32 = undefined;
        var args_len: u32 = 0;

        if (args.tag == .expression_list) {
            args_idx = try anl.genExpression(anl.tree.extras[args.lhs]);
            var i = args.lhs + 1;
            while (i < args.rhs) : (i += 1) {
                _ = try anl.genExpression(anl.tree.extras[i]);
            }
            args_len = @intCast(anl.instructions.items.len - args_idx);
        } else {
            args_idx = try anl.genExpression(node_val.rhs);
            args_len = 1;
        }

        return try anl.addInst(.{
            .call = .{
                .index = symbol.id,
                // TODO: support multiple return values, in this case we need
                // tuple/list support as the function with more than one returns
                // will actually return a tuple which the user may break down
                .result_ty = symbol.ty.func.result[0],
                .args_idx = args_idx,
                .args_len = args_len,
            },
        });
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
            => valid_op = lhs_type.eql(rhs_type), // Applicable on all values of same type
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
            .global = symbol.isGlobal(),
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
        \\func_type
        \\
    ,
        \\x := 10 + 20 * 2;
    );
}
