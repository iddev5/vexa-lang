const std = @import("std");
const Ast = @import("Ast.zig");
const analysis = @import("analysis.zig");
const WasmGen = @import("WasmGen.zig");

pub fn main() !void {
    const allocator = std.heap.page_allocator;
    const stdout = std.io.getStdOut().writer();

    var tree = try Ast.parse(allocator,
        \\local h = -1 + 2 * 3 / 4
        \\local i = true == true
        \\if true == true then
        \\    local x = 67 / 22
        \\end
        \\return 12
    , null);
    defer tree.deinit();

    var air = try analysis.gen(&tree);
    defer air.deinit();

    var gen = WasmGen{
        .allocator = allocator,
        .ir = &air,
    };

    _ = try gen.emit(stdout);
}

comptime {
    _ = @import("Tokenizer.zig");
    _ = @import("Ast.zig");
    _ = @import("analysis.zig");
}
