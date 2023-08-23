const WasmGen = @This();

const std = @import("std");
const Air = @import("Air.zig");

allocator: std.mem.Allocator,
// buffer: std.ArrayListUnmanaged(u8) = .{},
ir: *Air,

pub const Module = struct {
    code: []const u8,
};

pub fn emit(gen: *WasmGen, writer: anytype) !Module {
    try gen.airTopLevel(writer, gen.ir.start_inst);
    return .{
        .code = "",
        // .code = try gen.buffer.toOwnedSlice(gen.allocator),
    };
}

fn airTopLevel(gen: *WasmGen, writer: anytype, inst: usize) !void {
    switch (gen.ir.instructions[inst]) {
        .add, .mul => |op| try gen.airBinOp(writer, inst, op),
        else => unreachable,
    }
}

fn airBinOp(gen: *WasmGen, writer: anytype, inst: usize, op: Air.Inst.BinaryOp) !void {
    _ = inst;
    try gen.airExpr(writer, op.lhs);
    try gen.airExpr(writer, op.rhs);
    try writer.writeAll("f64.add\n");
}

fn airExpr(gen: *WasmGen, writer: anytype, inst: usize) anyerror!void {
    switch (gen.ir.instructions[inst]) {
        .float => |info| try writer.print("f64.const {d}\n", .{info}),
        .add, .mul => |info| try gen.airBinOp(writer, inst, info),
        else => {},
    }
}
