const WasmGen = @This();

const std = @import("std");
const leb = std.leb;
const Air = @import("Air.zig");

pub const Module = struct {
    code: []const u8,
};

pub const Opcode = enum(u8) {
    f64_const = 0x44,
    f64_add = 0xa0,
    f64_sub = 0xa1,
    f64_mul = 0xa2,
    f64_div = 0xa3,
    end = 0x0b,
};

pub const Section = struct {
    ty: Type,
    code: std.ArrayList(u8),

    pub fn init(ty: Type, allocator: std.mem.Allocator) Section {
        return .{ .ty = ty, .code = std.ArrayList(u8).init(allocator) };
    }

    pub fn deinit(sec: *Section) void {
        sec.code.deinit();
    }

    pub fn emit(sec: *const Section, w: anytype) !void {
        try leb.writeULEB128(w, @intFromEnum(sec.ty));
        try leb.writeULEB128(w, @as(u8, 0)); // TODO: size?
        try leb.writeULEB128(w, @as(u8, 1)); // TODO: num something
        try w.writeAll(sec.code.items);
    }

    pub const Type = enum(u8) {
        custom = 0,
        type = 1,
        import = 2,
        func = 3,
        table = 4,
        mem = 5,
        global = 6,
        start = 7,
        elem = 8,
        datacount = 9,
        code = 10,
        data = 11,
    };
};

pub const ValType = enum {
    i32,
    i64,
    f32,
    f64,
};

allocator: std.mem.Allocator,
ir: *Air,
sections: [12]Section = undefined,

const module_header = [_]u8{ 0x00, 0x61, 0x73, 0x6d };
const module_version = [_]u8{ 0x01, 0x00, 0x00, 0x00 };

pub fn emit(gen: *WasmGen, w: anytype) !Module {
    try w.writeAll(module_header[0..]);
    try w.writeAll(module_version[0..]);

    for (gen.ir.functions) |func| {
        const code_writer = gen.writer(.code);
        try leb.writeULEB128(code_writer, @as(u8, 0)); // func size
        try leb.writeULEB128(code_writer, @as(u8, 0)); // num locals

        try gen.airTopLevel(
            func.instructions,
            func.start_inst,
        );

        try emitOpcode(code_writer, .end);
    }

    // for (gen.sections) |sec|
    try gen.sections[0].emit(w);

    return .{
        .code = "",
    };
}

fn getSection(gen: *WasmGen, ty: Section.Type) *Section {
    for (&gen.sections) |*sec|
        if (sec.ty == ty)
            return sec;

    // TODO: number
    gen.sections[0] = Section.init(ty, gen.allocator);
    return &gen.sections[0];
}

fn writer(gen: *WasmGen, ty: Section.Type) std.ArrayList(u8).Writer {
    return gen.getSection(ty).code.writer();
}

fn airTopLevel(gen: *WasmGen, ir: []const Air.Inst, inst: usize) !void {
    switch (ir[inst]) {
        .add, .mul => |op| try gen.airBinOp(ir, inst, op),
        else => unreachable,
    }
}

fn airBinOp(gen: *WasmGen, ir: []const Air.Inst, inst: usize, op: Air.Inst.BinaryOp) !void {
    const inst_obj = ir[inst];
    try gen.airExpr(ir, op.lhs);
    try gen.airExpr(ir, op.rhs);

    const code_writer = gen.getSection(.code).code.writer();
    try emitOpcode(code_writer, buildOpcode(std.meta.activeTag(inst_obj), .f64));
}

fn airExpr(gen: *WasmGen, ir: []const Air.Inst, inst: usize) anyerror!void {
    const code_writer = gen.getSection(.code).code.writer();
    switch (ir[inst]) {
        .float => |info| {
            try emitOpcode(code_writer, .f64_const);

            const float = @as(u64, @bitCast(info));
            try code_writer.writeIntLittle(u32, @as(u32, @truncate(float)));
            try code_writer.writeIntLittle(u32, @as(u32, @truncate(float >> 32)));
        },
        .add, .mul => |info| try gen.airBinOp(ir, inst, info),
        else => {},
    }
}

fn emitOpcode(w: anytype, op: Opcode) !void {
    try w.writeByte(@intFromEnum(op));
}

fn buildOpcode(op: Air.InstType, ty: ValType) Opcode {
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
