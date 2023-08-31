const Air = @This();

const std = @import("std");

allocator: std.mem.Allocator,
functions: []const FuncBlock,

pub const FuncBlock = struct {
    instructions: []const Inst,
    start_inst: usize,
    locals: []const ValueType,
    params: []const ValueType,
    result: []const ValueType,
};

pub const ValueType = enum {
    void,
    bool,
    float,
};

pub fn deinit(air: *Air) void {
    for (air.functions) |func|
        air.allocator.free(func.instructions);

    air.allocator.free(air.functions);
}

pub const InstType = std.meta.Tag(Inst);

pub const Inst = union(enum) {
    nop: void,
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
    local_set: struct {
        index: u16,
        value: Index,
    },
    ret: struct {
        result_ty: ValueType,
        value: Index,
    },

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
};
