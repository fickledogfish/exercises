const std = @import("std");

const p = std.debug.print;

const cipher = "cvpbPGS{abg_gbb_onq_bs_n_ceboyrz}";

pub fn main() !void {
    p("original: {}\n", .{cipher});

    p("solved:   ", .{});
    for (cipher) |ch| {
        const padding: u8 = if (ch >= 'a' and ch <= 'z')
            'a'
        else if (ch >= 'A' and ch <= 'Z')
            'A'
        else {
            p("{c}", .{ch});
            continue;
        };

        p("{c}", .{(ch - padding + 13) % 26 + padding});
    }
    p("\n", .{});
}
