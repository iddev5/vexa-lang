const Diagnostics = @This();

const std = @import("std");
const Token = @import("Tokenizer.zig").Token;

pub const ErrorTag = enum {
    expected_token,

    pub fn getMessage(tag: ErrorTag) []const u8 {
        return switch (tag) {
            .expected_token => "expected {s}, found {s}",
        };
    }
};

tag: ErrorTag,
loc: Token.Location,
message: []const u8,

pub fn emitError(
    allocator: std.mem.Allocator,
    loc: Token.Location,
    comptime tag: ErrorTag,
    args: anytype,
) !Diagnostics {
    const message = comptime tag.getMessage();
    return Diagnostics{
        .tag = tag,
        .loc = loc,
        .message = try std.fmt.allocPrint(allocator, message, args),
    };
}

pub fn render(diag: *Diagnostics, writer: anytype) !void {
    try writer.writeAll(diag.message);
    try writer.writeByte('\n');
}
