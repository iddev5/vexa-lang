const WasmGen = @This();

const std = @import("std");
const leb = std.leb;
const Air = @import("Air.zig");

pub const Module = struct {
    code: []const u8,
};

pub const Opcode = enum(u8) {
    local_set = 0x21,
    i32_const = 0x41,
    f64_const = 0x44,
    f64_neg = 0x9a,
    f64_add = 0xa0,
    f64_sub = 0xa1,
    f64_mul = 0xa2,
    f64_div = 0xa3,
    end = 0x0b,
};

pub const Section = struct {
    ty: Type,
    code: std.ArrayList(u8),
    count: u32 = 0,

    pub fn init(ty: Type, allocator: std.mem.Allocator) Section {
        return .{ .ty = ty, .code = std.ArrayList(u8).init(allocator) };
    }

    pub fn deinit(sec: *Section) void {
        sec.code.deinit();
    }

    pub fn emit(sec: *const Section, w: anytype) !void {
        // Section code
        try leb.writeULEB128(w, @intFromEnum(sec.ty));
        // Size of the section
        try leb.writeULEB128(w, @as(u32, @intCast(sec.code.items.len + 1)));
        // Num/count of elements
        try leb.writeULEB128(w, @as(u32, sec.count));
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

// TODO: combine all types
pub const ValType = enum(u8) {
    i32 = 0x7f,
    i64 = 0x7e,
    f32 = 0x7d,
    f64 = 0x7c,
    _,
};

pub const WasmType = enum(u8) {
    func = 0x60,
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

fn resolveValueType(gen: *WasmGen, ty: Air.ValueType) ValType {
    _ = gen;
    return switch (ty) {
        .bool => .i32,
        .float => .f64,
    };
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

    var func_code_writer = func_code.writer();

    try leb.writeULEB128(func_code_writer, @as(u32, @intCast(func.locals.len)));
    for (func.locals) |local| {
        try leb.writeULEB128(func_code_writer, @as(u8, 1));
        try leb.writeULEB128(func_code_writer, @intFromEnum(gen.resolveValueType(local)));
    }

    for (0..func.instructions.len) |inst_index|
        try gen.emitTopLevel(
            func_code_writer,
            func.instructions,
            inst_index,
        );

    // Function type
    var type_section = gen.section(.type);
    type_section.count += 1;

    const type_writer = type_section.code.writer();
    try leb.writeULEB128(type_writer, @intFromEnum(WasmType.func));

    try leb.writeULEB128(type_writer, @as(u32, @intCast(func.params.len)));
    for (func.params) |param|
        try type_writer.writeByte(@intFromEnum(gen.resolveValueType(param)));

    try leb.writeULEB128(type_writer, @as(u32, @intCast(func.result.len)));
    for (func.result) |result|
        try type_writer.writeByte(@intFromEnum(gen.resolveValueType(result)));

    // Function entry
    var func_section = gen.section(.func);
    func_section.count += 1;

    const func_writer = func_section.code.writer();
    try leb.writeULEB128(func_writer, func_section.count - 1);

    // Function code
    var code_section = gen.section(.code);
    code_section.count += 1;

    const code_writer = code_section.code.writer();
    // Emit function size the end opcode
    try leb.writeULEB128(code_writer, @as(u32, @intCast(func_code.items.len + 1)));

    try code_writer.writeAll(func_code.items);
    try gen.emitOpcode(code_writer, .end);
}

fn emitTopLevel(gen: *WasmGen, writer: anytype, ir: []const Air.Inst, inst: usize) !void {
    switch (ir[inst]) {
        .local_set => try gen.emitLocal(writer, ir, inst),
        else => {},
    }
}

fn emitLocal(gen: *WasmGen, writer: anytype, ir: []const Air.Inst, inst: usize) !void {
    const inst_obj = ir[inst];
    try gen.emitExpr(writer, ir, inst_obj.local_set.value);
    try gen.emitOpcode(writer, .local_set);
    try leb.writeULEB128(writer, inst_obj.local_set.index);
}

fn emitBinOp(gen: *WasmGen, writer: anytype, ir: []const Air.Inst, inst: usize, op: Air.Inst.BinaryOp) !void {
    const inst_obj = ir[inst];
    try gen.emitExpr(writer, ir, op.lhs);
    try gen.emitExpr(writer, ir, op.rhs);

    try gen.emitOpcode(writer, buildOpcode(std.meta.activeTag(inst_obj), .f64));
}

fn emitUnOp(gen: *WasmGen, writer: anytype, ir: []const Air.Inst, inst: usize, op: Air.Inst.UnaryOp) !void {
    const inst_obj = ir[inst];
    try gen.emitExpr(writer, ir, op.inst);

    try gen.emitOpcode(writer, buildOpcode(std.meta.activeTag(inst_obj), .f64));
}

fn emitExpr(gen: *WasmGen, writer: anytype, ir: []const Air.Inst, inst: usize) anyerror!void {
    switch (ir[inst]) {
        .bool => |info| {
            try gen.emitOpcode(writer, .i32_const);
            try writer.writeByte(@intFromBool(info));
        },
        .float => |info| {
            try gen.emitOpcode(writer, .f64_const);

            const float = @as(u64, @bitCast(info));
            try writer.writeIntLittle(u32, @as(u32, @truncate(float)));
            try writer.writeIntLittle(u32, @as(u32, @truncate(float >> 32)));
        },
        .add, .sub, .mul, .div => |info| try gen.emitBinOp(writer, ir, inst, info),
        .negate => |info| try gen.emitUnOp(writer, ir, inst, info),
        else => unreachable,
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
        .negate => switch (ty) {
            .f64 => .f64_neg,
            else => unreachable,
        },
        else => unreachable,
    };
}
