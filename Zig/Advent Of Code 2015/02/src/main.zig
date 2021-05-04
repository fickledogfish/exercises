// --- Day 2: I Was Told There Would Be No Math ---
//
// The elves are running low on wrapping paper, and so they need to submit an
// order for more. They have a list of the dimensions (length l, width w, and
// height h) of each present, and only want to order exactly as much as they
// need.
//
// Fortunately, every present is a box (a perfect right rectangular prism),
// which makes calculating the required wrapping paper for each gift a little
// easier: find the surface area of the box, which is 2*l*w + 2*w*h + 2*h*l.
// The elves also need a little extra paper for each present: the area of the
// smallest side.
//
// For example:
//
//     - A present with dimensions 2x3x4 requires 2*6 + 2*12 + 2*8 = 52 square
//       feet of wrapping paper plus 6 square feet of slack, for a total of 58
//       square feet.
//     - A present with dimensions 1x1x10 requires 2*1 + 2*10 + 2*10 = 42
//       square feet of wrapping paper plus 1 square foot of slack, for a total
//       of 43 square feet.
//
// All numbers in the elves' list are in feet. How many total square feet of
// wrapping paper should they order?
//
// --- Part Two ---
//
// The elves are also running low on ribbon. Ribbon is all the same width, so
// they only have to worry about the length they need to order, which they
// would again like to be exact.
//
// The ribbon required to wrap a present is the shortest distance around its
// sides, or the smallest perimeter of any one face. Each present also requires
// a bow made out of ribbon as well; the feet of ribbon required for the
// perfect bow is equal to the cubic feet of volume of the present. Don't ask
// how they tie the bow, though; they'll never tell.
//
// For example:
//
//     - A present with dimensions 2x3x4 requires 2+2+3+3 = 10 feet of ribbon
//       to wrap the present plus 2*3*4 = 24 feet of ribbon for the bow, for a
//       total of 34 feet.
//     - A present with dimensions 1x1x10 requires 1+1+1+1 = 4 feet of ribbon
//       to wrap the present plus 1*1*10 = 10 feet of ribbon for the bow, for a
//       total of 14 feet.
//
// How many total feet of ribbon should they order?

const std = @import("std");
const expectEq = std.testing.expectEqual;

const FILE = @embedFile("../input.txt");

const Dim = u32;

pub fn main() anyerror!void {
    var parser = Parser.init(FILE);
    var sum_wrapping: Dim = 0;
    var sum_ribbon: Dim = 0;

    while (true) {
        const line = parser.next() orelse break;
        const wrapping = present_wrapping(line.l, line.w, line.h);
        const ribbon = ribbon_len(line.l, line.w, line.h);

        sum_wrapping += wrapping;
        sum_ribbon += ribbon;
    }

    std.debug.warn("wrapping: {}\nribbon: {}\n", .{
        sum_wrapping,
        sum_ribbon,
    });
}

fn ribbon_len(l: Dim, w: Dim, h: Dim) Dim {
    const perims = [_]Dim{
        2 * (w + h),
        2 * (w + l),
        2 * (l + h),
    };

    const bow = l * w * h;

    var ribbon = perims[0];
    for (perims) |perim| {
        if (perim < ribbon) ribbon = perim;
    }

    return ribbon + bow;
}

test "Provided samples for ribbon" {
    expectEq(ribbon_len(2, 3, 4), 34);
    expectEq(ribbon_len(1, 1, 10), 14);
}

const Parser = struct {
    const Self = @This();

    const ParsedLine = struct {
        l: Dim,
        w: Dim,
        h: Dim,

        pub fn init(l: Dim, w: Dim, h: Dim) ParsedLine {
            return ParsedLine{
                .l = l,
                .w = w,
                .h = h,
            };
        }
    };

    start: usize,

    buffer: []const u8,

    pub fn init(buffer: []const u8) Self {
        return Self{
            .start = 0,
            .buffer = buffer,
        };
    }

    fn findChar(buffer: []const u8, start: usize, char: u8) ?usize {
        if (start >= buffer.len) return null;

        var end = start + 1;

        while (true) : (end += 1) {
            if (end == buffer.len or buffer[end] == char) break;
        }

        return end;
    }

    pub fn next(self: *Self) ?ParsedLine {
        const DIV_CHAR = 'x';

        const buffer = self.buffer;
        const start = self.start;
        const end = findChar(buffer, start, '\n') orelse return null;

        const fstDiv = findChar(
            self.buffer,
            start,
            DIV_CHAR,
        ) orelse unreachable;
        const sndDiv = findChar(
            self.buffer,
            fstDiv,
            DIV_CHAR,
        ) orelse unreachable;

        const lstr = self.buffer[start..fstDiv];
        const wstr = self.buffer[fstDiv + 1 .. sndDiv];
        const hstr = self.buffer[sndDiv + 1 .. end];

        const l = std.fmt.parseInt(Dim, lstr, 10) catch unreachable;
        const w = std.fmt.parseInt(Dim, wstr, 10) catch unreachable;
        const h = std.fmt.parseInt(Dim, hstr, 10) catch unreachable;

        self.start = end + 1;

        return ParsedLine.init(l, w, h);
    }
};

fn present_wrapping(w: Dim, h: Dim, l: Dim) Dim {
    const sides = [_]Dim{
        w * h,
        w * l,
        h * l,
    };

    var slack: Dim = sides[0];
    var ret: Dim = 0;

    for (sides) |side| {
        if (side < slack) slack = side;
        ret += side;
    }

    return 2 * ret + slack;
}

test "Provided samples for wrapping" {
    expectEq(present_wrapping(2, 3, 4), 58);
    expectEq(present_wrapping(1, 1, 10), 43);
}
