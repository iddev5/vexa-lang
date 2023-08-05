const std = @import("std");
const Ast = @import("Ast.zig");

pub fn main() !void {
    const allocator = std.heap.page_allocator;
    const stdout = std.io.getStdOut().writer();

    var tree = try Ast.parse(allocator,
        \\if true then
        \\  local h = 12
        \\  hmm = 20
        \\  an = 23
        \\end
        \\
        \\if false then
        \\  local hi = 12
        \\  ho = 23
        \\end
        \\
        \\local h = 12
        \\local o = 12
        \\
        \\if 1 then
        \\  i = 2
        \\end
    );
    defer tree.deinit();

    try tree.printTree(stdout);
}

comptime {
    _ = @import("Tokenizer.zig");
    _ = @import("Ast.zig");
}
