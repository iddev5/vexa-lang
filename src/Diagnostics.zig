const Diagnostics = @This();

const std = @import("std");
const Token = @import("Tokenizer.zig").Token;

pub const ErrorTag = enum {
    // Parser errors
    expected_token,
    invalid_lhs,

    // Analysis errors
    invalid_bin_op,
    redecl_global,
    redecl_local,
    undecl_ident,
    not_func,
    expected_ty,
    invalid_stmt,
    unpack_error,

    pub fn getMessage(tag: ErrorTag) []const u8 {
        return switch (tag) {
            .expected_token => "expected '{s}', found '{s}'",
            .invalid_lhs => "invalid left hand side to assignment",
            .invalid_bin_op => "invalid operator '{s}' between values of type '{s}' and '{s}'",
            .redecl_global => "redeclaration of global variable '{s}'",
            .redecl_local => "redeclaration of local variable '{s}'",
            .undecl_ident => "use of undeclared identifier '{s}'",
            .not_func => "variable '{s}' is not a function",
            .expected_ty => "expected value of type '{s}', found '{s}'",
            .invalid_stmt => "invalid statement found",
            .unpack_error => "not enough values to unpack, expected {d}, found {d}",
        };
    }
};

pub const Error = struct {
    tag: ErrorTag,
    loc: Token.Location,
    message: []const u8,
};

allocator: std.mem.Allocator,
source: [:0]const u8,
errors: std.ArrayListUnmanaged(Error) = .{},

pub fn deinit(diag: *Diagnostics) void {
    for (diag.errors.items) |err|
        diag.allocator.free(err.message);

    diag.errors.deinit(diag.allocator);
}

pub fn emitError(
    diag: *Diagnostics,
    loc: Token.Location,
    comptime tag: ErrorTag,
    args: anytype,
) !void {
    const message = comptime tag.getMessage();

    try diag.errors.append(diag.allocator, .{
        .tag = tag,
        .loc = loc,
        .message = try std.fmt.allocPrint(diag.allocator, message, args),
    });
}

pub fn render(diag: *Diagnostics, writer: anytype) !void {
    for (diag.errors.items) |err| {
        const loc_coords = err.loc.getCoords(diag.source);

        try writer.writeAll("error: ");
        try writer.print("{d}:{d} ", .{ loc_coords.line, loc_coords.column });
        try writer.writeAll(err.message);
        try writer.writeByte('\n');

        // Print source code lines
        try writer.print("{d} | ", .{loc_coords.line});
        try writer.writeAll(diag.source[loc_coords.line_loc.start..loc_coords.line_loc.end]);
        try writer.writeByte('\n');

        const line_no_len = std.math.log10_int(loc_coords.line) + 3;
        try writer.writeByteNTimes(' ', line_no_len + loc_coords.column);
        try writer.writeByte('^');
        try writer.writeByteNTimes('~', err.loc.end - err.loc.start -| 1);
        try writer.writeByte('\n');
    }
}
