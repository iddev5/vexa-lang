const std = @import("std");
const Ast = @import("Ast.zig");
const analysis = @import("analysis.zig");
const WasmGen = @import("WasmGen.zig");
const Diagnostics = @import("Diagnostics.zig");

pub fn main() !void {
    const allocator = std.heap.page_allocator;
    const stdout = std.io.getStdOut().writer();
    const stderr = std.io.getStdErr().writer();

    const source =
        \\hi := -1 + 2 * 3 / 4
        \\i := true == true
        \\if true == true then
        \\    hi = 67 / 22 + hi
        \\end
        \\if false then
        \\    return false
        \\elseif true then
        \\    return true
        \\else
        \\    m := 12
        \\    if true then
        \\        m = m * 2
        \\        return true
        \\    else
        \\        return false
        \\    end
        \\    return 10
        \\end
        \\do
        \\    j := true
        \\end
        \\return 12
        \\while true do
        \\    if i == false then
        \\        break
        \\    end
        \\    n := 12
        \\end
    ;

    var diag: Diagnostics = undefined;
    diag.source = source;
    // defer diag.deinit(allocator);

    var tree = Ast.parse(allocator, source, &diag) catch |err| switch (err) {
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
