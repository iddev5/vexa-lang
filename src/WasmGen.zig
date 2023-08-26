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

    for (gen.ir.functions) |func| {
        try gen.airTopLevel(
            writer,
            func.instructions,
            func.start_inst,
        );
    }

    return .{
        .code = "",
    };
}

fn airTopLevel(gen: *WasmGen, writer: anytype, ir: []const Air.Inst, inst: usize) !void {
    switch (ir[inst]) {
        .add, .mul => |op| try gen.airBinOp(writer, ir, inst, op),
        else => unreachable,
    }
}

fn airBinOp(gen: *WasmGen, writer: anytype, ir: []const Air.Inst, inst: usize, op: Air.Inst.BinaryOp) !void {
    const inst_obj = ir[inst];
    try gen.airExpr(writer, ir, op.lhs);
    try gen.airExpr(writer, ir, op.rhs);

    try emitOpcode(writer, buildOpcode(std.meta.activeTag(inst_obj), .f64));
}

fn airExpr(gen: *WasmGen, writer: anytype, ir: []const Air.Inst, inst: usize) anyerror!void {
    switch (ir[inst]) {
        .float => |info| {
            try emitOpcode(writer, .f64_const);

            const float = @as(u64, @bitCast(info));
            try writer.writeIntLittle(u32, @as(u32, @truncate(float)));
            try writer.writeIntLittle(u32, @as(u32, @truncate(float >> 32)));
        },
        .add, .mul => |info| try gen.airBinOp(writer, ir, inst, info),
        else => {},
    }
}

fn emitOpcode(writer: anytype, op: OpCode) !void {
    try writer.writeByte(@intFromEnum(op));
}

fn buildOpcode(op: Air.InstType, ty: ValType) OpCode {
    return switch (op) {
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
        else => unreachable,
    };
}
