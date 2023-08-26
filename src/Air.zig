const Air = @This();

const std = @import("std");

allocator: std.mem.Allocator,
functions: []const FuncBlock,

pub const FuncBlock = struct {
    instructions: []const Inst,
    start_inst: usize,
};

pub const ValueIndex = u32;

pub fn deinit(air: *Air) void {
    for (air.functions) |func|
        air.allocator.free(func.instructions);

    air.allocator.free(air.functions);
}

pub const InstType = enum {
    float,
    add,
    sub,
    mul,
    div,
};

pub const Inst = union(InstType) {
    float: f64,
    add: BinaryOp,
    sub: BinaryOp,
    mul: BinaryOp,
    div: BinaryOp,

    pub const Index = u32;

    pub const BinaryOp = struct {
        lhs: Inst.Index,
        rhs: Inst.Index,
    };
};
