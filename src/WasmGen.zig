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
num_sections: usize = 0,

const module_header = [_]u8{ 0x00, 0x61, 0x73, 0x6d };
const module_version = [_]u8{ 0x01, 0x00, 0x00, 0x00 };

fn section(gen: *WasmGen, ty: Section.Type) *Section {
    for (&gen.sections) |*sec|
        if (sec.ty == ty)
            return sec;

    gen.sections[gen.num_sections] = Section.init(ty, gen.allocator);
    gen.num_sections += 1;
    return &gen.sections[gen.num_sections - 1];
}

pub fn emit(gen: *WasmGen, w: anytype) !Module {
    try w.writeAll(module_header[0..]);
    try w.writeAll(module_version[0..]);

    for (gen.ir.functions) |func|
        try gen.emitFunc(func);

    for (gen.sections[0..gen.num_sections]) |sec|
        try sec.emit(w);

    return .{
        .code = "",
    };
}

fn emitFunc(gen: *WasmGen, func: Air.FuncBlock) !void {
    // TODO: approximate initial size
    var func_code = std.ArrayList(u8).init(gen.allocator);
    defer func_code.deinit();

    try gen.emitTopLevel(
        func_code.writer(),
        func.instructions,
        func.start_inst,
    );

    const code_writer = gen.section(.code).code.writer();
    try leb.writeULEB128(code_writer, @as(u32, @intCast(func_code.items.len))); // func size
    try leb.writeULEB128(code_writer, @as(u8, 0)); // num locals

    try code_writer.writeAll(func_code.items);

    try gen.emitOpcode(code_writer, .end);
}

fn emitTopLevel(gen: *WasmGen, writer: anytype, ir: []const Air.Inst, inst: usize) !void {
    switch (ir[inst]) {
        .add, .mul => |op| try gen.emitBinOp(writer, ir, inst, op),
        else => unreachable,
    }
}

fn emitBinOp(gen: *WasmGen, writer: anytype, ir: []const Air.Inst, inst: usize, op: Air.Inst.BinaryOp) !void {
    const inst_obj = ir[inst];
    try gen.emitExpr(writer, ir, op.lhs);
    try gen.emitExpr(writer, ir, op.rhs);

    try gen.emitOpcode(writer, buildOpcode(std.meta.activeTag(inst_obj), .f64));
}

fn emitExpr(gen: *WasmGen, writer: anytype, ir: []const Air.Inst, inst: usize) anyerror!void {
    switch (ir[inst]) {
        .float => |info| {
            try gen.emitOpcode(writer, .f64_const);

            const float = @as(u64, @bitCast(info));
            try writer.writeIntLittle(u32, @as(u32, @truncate(float)));
            try writer.writeIntLittle(u32, @as(u32, @truncate(float >> 32)));
        },
        .add, .mul => |info| try gen.emitBinOp(writer, ir, inst, info),
        else => {},
    }
}

fn emitOpcode(gen: *WasmGen, writer: anytype, op: Opcode) !void {
    _ = gen;
    try writer.writeByte(@intFromEnum(op));
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
