const std = @import("std");
const Ast = @import("Ast.zig");

pub fn main() !void {
    const allocator = std.heap.page_allocator;
    const stdout = std.io.getStdOut().writer();

    var tree = try Ast.parse(allocator,
        \\if true then
        \\  local x = 12
        \\elseif 12 then
        \\  hoo = 34
        \\elseif true == 35 then
        \\  local x = 12
        \\else
        \\  hi = 12
        \\end
    );
    defer tree.deinit();

    try tree.printTree(stdout);
}

comptime {
    _ = @import("Tokenizer.zig");
    _ = @import("Ast.zig");
}
