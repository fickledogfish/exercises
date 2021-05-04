const std = @import("std");

const p = std.debug.print;

const cipher = blk: {
    const file = @embedFile("../ciphertext");

    // Ignore the unnecessary bits of the file
    break :blk file["picoCTF{".len .. file.len - 1];
};

pub fn main() !void {
    var i: u8 = 0;
    while (i < 26) : (i += 1) {
        p("{}: picoCTF{{", .{i});
        for (cipher) |ch| {
            p("{c}", .{(ch - 'a' + i) % 26 + 'a'});
        }
        p("}}\n", .{});
    }

    // Solution is i = 24:
    //
    // picoCTF{crossingtherubicondjneoach}
}
