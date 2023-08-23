const Air = @This();

const std = @import("std");

allocator: std.mem.Allocator,
instructions: []const Inst,
start_inst: usize,
values: []const f64,
strings: []const u8,

pub const ValueIndex = u32;

pub fn deinit(air: *Air) void {
    air.allocator.free(air.instructions);
    air.allocator.free(air.values);
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
