const Air = @This();

const std = @import("std");

allocator: std.mem.Allocator,
start_inst: u32,
instructions: []const Inst,
globals: []const ValueType,

pub const ValueType = enum {
    void,
    bool,
    float,
};

pub fn deinit(air: *Air) void {
    air.allocator.free(air.instructions);
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
    local_set: SetValue,
    global_set: SetValue,
    ret: struct {
        result_ty: ValueType,
        value: Index,
    },
    func: Function,
    cond: struct {
        cond: Index,
        result: Index,
    },
    block: Block,

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

    pub const Function = struct {
        start_inst: u32,
        inst_len: u32,
        locals: []const ValueType,
        params: []const ValueType,
        result: []const ValueType,
    };

    pub const Block = struct {
        start_inst: u32,
        inst_len: u32,
    };
};
