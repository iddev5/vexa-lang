const Air = @This();

const std = @import("std");

allocator: std.mem.Allocator,
start_inst: u32,
instructions: []const Inst,
globals: []const ValueType,
locals: []const ValueType,
main_fn_type: Inst.Index,

pub const ValueType = union(enum) {
    void,
    bool,
    float,
    func: Inst.FunctionType,

    pub fn eql(ty: ValueType, other: ValueType) bool {
        if (std.meta.activeTag(ty) != std.meta.activeTag(other))
            return false;

        return switch (ty) {
            .func => blk: {
                if (ty.func.params.len != other.func.params.len)
                    break :blk false;

                for (ty.func.params, 0..) |param, i| {
                    if (!param.eql(other.func.params[i]))
                        break :blk false;
                }

                break :blk true;
            },
            else => true,
        };
    }
};

pub fn deinit(air: *Air) void {
    air.allocator.free(air.instructions);
    air.allocator.free(air.globals);
    air.allocator.free(air.locals);
}

pub const InstType = std.meta.Tag(Inst);

pub const Inst = union(enum) {
    nop: void,
    ident: struct { index: u16, result_ty: ValueType, global: bool },
    bool: bool,
    float: f64,
    add: BinaryOp,
    sub: BinaryOp,
    mul: BinaryOp,
    div: BinaryOp,
    equal: BinaryOp,
    not_equal: BinaryOp,
    less_than: BinaryOp,
    greater_than: BinaryOp,
    less_equal: BinaryOp,
    greater_equal: BinaryOp,
    negate: UnaryOp,
    local_set: SetValue,
    global_set: SetValue,
    ret: struct {
        result_ty: ValueType,
        value: Index,
    },
    func_type: FunctionType,
    func: Function,
    cond: struct {
        cond: Index,
        result: Index,
        else_blk: Index,
    },
    loop: struct {
        cond: Index,
        block: Index,
    },
    call: struct {
        index: u16,
        result_ty: ValueType,
    },
    br: void,
    block: Block,
    block_do: Index,
    stmt: void,

    pub const Index = u32;

    pub const BinaryOp = struct {
        result_ty: ValueType,
        lhs: Inst.Index,
        rhs: Inst.Index,
    };

    pub const UnaryOp = struct {
        result_ty: ValueType,
        inst: Inst.Index,
    };

    pub const SetValue = struct {
        index: u16,
        value: Index,
    };

    pub const FunctionType = struct {
        params: []const ValueType,
        result: []const ValueType,
    };

    pub const Function = struct {
        fn_type: Inst.Index,
        start_inst: u32,
        inst_len: u32,
        locals: []const ValueType,
    };

    pub const Block = struct {
        start_inst: u32,
        inst_len: u32,
    };
};
