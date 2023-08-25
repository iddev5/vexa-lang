const WasmGen = @This();

const std = @import("std");
const leb = std.leb;
const Air = @import("Air.zig");

allocator: std.mem.Allocator,
ir: *Air,

pub const Module = struct {
    code: []const u8,
};

pub const Op = enum {
    @"const",
    add,
    sub,
    mul,
    div,
};

pub const OpCode = enum(u8) {
    f64_const = 0x44,
    f64_add = 0xa0,
    f64_sub = 0xa1,
    f64_mul = 0xa2,
    f64_div = 0xa3,
};

pub const ValType = enum {
    i32,
    i64,
    f32,
    f64,
};

const module_header = [_]u8{ 0x00, 0x61, 0x73, 0x6d };
const module_version = [_]u8{ 0x01, 0x00, 0x00, 0x00 };

pub fn emit(gen: *WasmGen, writer: anytype) !Module {
    try writer.writeAll(module_header[0..]);
    try writer.writeAll(module_version[0..]);

    try gen.airTopLevel(writer, gen.ir.start_inst);
    return .{
        .code = "",
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

    try writer.writeIntLittle(u8, @intFromEnum(buildOpcode(.add, .f64)));
}

fn airExpr(gen: *WasmGen, writer: anytype, inst: usize) anyerror!void {
    switch (gen.ir.instructions[inst]) {
        .float => |info| {
            try writer.writeIntLittle(u8, @intFromEnum(buildOpcode(.@"const", .f64)));

            const float = @as(u64, @bitCast(info));
            try writer.writeIntLittle(u32, @as(u32, @truncate(float)));
            try writer.writeIntLittle(u32, @as(u32, @truncate(float >> 32)));
        },
        .add, .mul => |info| try gen.airBinOp(writer, inst, info),
        else => {},
    }
}

fn buildOpcode(op: Op, ty: ValType) OpCode {
    return switch (op) {
        .@"const" => switch (ty) {
            .f64 => .f64_const,
            else => unreachable, // do when needed
        },
        .add => switch (ty) {
            .f64 => .f64_add,
            else => unreachable, // do when needed
        },
        .sub => switch (ty) {
            .f64 => .f64_sub,
            else => unreachable, // do when needed
        },
        .mul => switch (ty) {
            .f64 => .f64_mul,
            else => unreachable, // do when needed
        },
        .div => switch (ty) {
            .f64 => .f64_div,
            else => unreachable, // do when needed
        },
    };
}
