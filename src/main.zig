const std = @import("std");
const Ast = @import("Ast.zig");
const analysis = @import("analysis.zig");
const WasmGen = @import("WasmGen.zig");

pub fn main() !void {
    const allocator = std.heap.page_allocator;
    const stdout = std.io.getStdOut().writer();

    var tree = try Ast.parse(allocator,
        \\local h = -1 + 2 * 3 / 4 + false
        \\local i = true + false
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
