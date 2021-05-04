const std = @import("std");

const p = std.debug.print;

const numbers = [_]u8{
    16,
    9,
    3,
    15,
    3,
    20,
    6,
    '{' - 64,

    20,
    8,
    5,
    14,
    21,
    13,
    2,
    5,
    18,
    19,
    13,
    1,
    19,
    15,
    14,

    '}' - 64,
};

pub fn main() !void {
    for (numbers) |n| {
        p("{c}", .{n + 64});
    }

    p("\n", .{});
}
