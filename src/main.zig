const std = @import("std");
const Ast = @import("Ast.zig");
const analysis = @import("analysis.zig");
const WasmGen = @import("WasmGen.zig");
const Diagnostics = @import("Diagnostics.zig");

pub fn main() !void {
    const allocator = std.heap.page_allocator;
    const stdout = std.io.getStdOut().writer();
    const stderr = std.io.getStdErr().writer();

    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    if (args.len < 2) {
        try stderr.writeAll("File input not provided.\n");
        std.process.exit(1);
    }

    const source = try std.fs.cwd().readFileAllocOptions(allocator, args[1], std.math.maxInt(usize), null, @alignOf(u8), 0);
    defer allocator.free(source);

    var diag: Diagnostics = .{ .allocator = allocator, .source = source };
    defer diag.deinit();

    var tree = Ast.parse(allocator, source, &diag) catch |err| switch (err) {
        error.ParsingFailed => return try diag.render(stderr),
        else => |e| return e,
    };
    defer tree.deinit();

    var air = analysis.gen(&tree, &diag) catch |err| switch (err) {
        error.AnalysisFailed => return try diag.render(stderr),
        else => |e| return e,
    };
    if (diag.errors.items.len > 0)
        return try diag.render(stderr);

    defer air.deinit();

    var gen = WasmGen{ .allocator = allocator, .ir = &air };
    defer gen.deinit();

    _ = try gen.emit(stdout);
}

comptime {
    _ = @import("Tokenizer.zig");
    _ = @import("Ast.zig");
    _ = @import("analysis.zig");
    _ = @import("WasmGen.zig");
}
