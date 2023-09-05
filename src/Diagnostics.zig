const Diagnostics = @This();

const std = @import("std");
const Token = @import("Tokenizer.zig").Token;

pub const ErrorTag = enum {
    // Parser errors
    expected_token,

    // Analysis errors
    invalid_bin_op,
    redecl_global,
    redecl_local,

    pub fn getMessage(tag: ErrorTag) []const u8 {
        return switch (tag) {
            .expected_token => "expected '{s}', found '{s}'",
            .invalid_bin_op => "invalid operator '{s}' between values of type '{s}' and '{s}'",
            .redecl_global => "redeclaration of global variable '{s}'",
            .redecl_local => "redeclaration of local variable '{s}'",
        };
    }
};

tag: ErrorTag,
loc: Token.Location,
message: []const u8,
source: [:0]const u8,

pub fn deinit(diag: *Diagnostics, allocator: std.mem.Allocator) void {
    allocator.free(diag.message);
}

pub fn emitError(
    diag: *Diagnostics,
    allocator: std.mem.Allocator,
    loc: Token.Location,
    comptime tag: ErrorTag,
    args: anytype,
) !void {
    const message = comptime tag.getMessage();
    diag.tag = tag;
    diag.loc = loc;
    diag.message = try std.fmt.allocPrint(allocator, message, args);
}

pub fn render(diag: *Diagnostics, writer: anytype) !void {
    const loc_coords = diag.loc.getCoords(diag.source);

    try writer.writeAll("error: ");
    try writer.print("{d}:{d} ", .{ loc_coords.line, loc_coords.column });
    try writer.writeAll(diag.message);
    try writer.writeByte('\n');

    // Print source code lines
    try writer.print("{d} | ", .{loc_coords.line});
    try writer.writeAll(diag.source[loc_coords.line_loc.start..loc_coords.line_loc.end]);
    try writer.writeByte('\n');

    const line_no_len = std.math.log10_int(loc_coords.line) + 3;
    try writer.writeByteNTimes(' ', line_no_len + loc_coords.column);
    try writer.writeByte('^');
    try writer.writeByteNTimes('~', diag.loc.end - diag.loc.start - 1);
    try writer.writeByte('\n');
}
