const Air = @This();

const std = @import("std");

allocator: std.mem.Allocator,
instructions: []const Inst,
values: []const f64,
strings: []const u8,

pub const ValueIndex = u32;

pub fn deinit(air: *Air) void {
    air.allocator.free(air.instructions);
    air.allocator.free(air.values);
}

pub const Inst = union(enum) {
    load_number: ValueIndex,
    binary_op: BinaryOp,

    pub const Index = u32;

    pub const BinaryOp = struct {
        op: OpType,
        lhs: Inst.Index,
        rhs: Inst.Index,
    };

    pub const OpType = enum {
        plus,
        minus,
        multiply,
        divide,
    };
};
