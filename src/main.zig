const std = @import("std");
const Ast = @import("Ast.zig");
const analysis = @import("analysis.zig");
const WasmGen = @import("WasmGen.zig");
const Diagnostics = @import("Diagnostics.zig");

pub fn main() !void {
    const allocator = std.heap.page_allocator;
    const stdout = std.io.getStdOut().writer();
    const stderr = std.io.getStdErr().writer();

    var diag: Diagnostics = undefined;
    // defer diag.deinit(allocator);

    var tree = Ast.parse(allocator,
        \\local h = -1 + 2 * 3 / 4
        \\local i = true == true
        \\if true == true then
        \\    local x = 67 / 22
        \\end
        \\if false then
        \\    return false
        \\end
        \\do
        \\    local j = true
        \\end
        \\return 12
    , &diag) catch |err| switch (err) {
        error.ParsingFailed => return try diag.render(stderr),
        else => |e| return e,
    };
    defer tree.deinit();

    var air = analysis.gen(&tree, &diag) catch |err| switch (err) {
        error.AnalysisFailed => return try diag.render(stderr),
        else => |e| return e,
    };
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
