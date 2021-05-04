// --- Day 3: Perfectly Spherical Houses in a Vacuum ---
//
// Santa is delivering presents to an infinite two-dimensional grid of houses.
//
// He begins by delivering a present to the house at his starting location, and
// then an elf at the North Pole calls him via radio and tells him where to
// move next. Moves are always exactly one house to the north (^), south (v),
// east (>), or west (<). After each move, he delivers another present to the
// house at his new location.
//
// However, the elf back at the north pole has had a little too much eggnog,
// and so his directions are a little off, and Santa ends up visiting some
// houses more than once. How many houses receive at least one present?
//
// For example:
//
//     - > delivers presents to 2 houses: one at the starting location, and one
//       to the east.
//
//     - ^>v< delivers presents to 4 houses in a square, including twice to the
//       house at his starting/ending location.
//
//     - ^v^v^v^v^v delivers a bunch of presents to some very lucky children at
//       only 2 houses.
//
// --- Part Two ---
//
// The next year, to speed up the process, Santa creates a robot version of
// himself, Robo-Santa, to deliver presents with him.
//
// Santa and Robo-Santa start at the same location (delivering two presents to
// the same starting house), then take turns moving based on instructions from
// the elf, who is eggnoggedly reading from the same script as the previous
// year.
//
// This year, how many houses receive at least one present?
//
// For example:
//
//     - ^v delivers presents to 3 houses, because Santa goes north, and then
//       Robo-Santa goes south.
//
//     - ^>v< now delivers presents to 3 houses, and Santa and Robo-Santa end
//       up back where they started.
//
//     - ^v^v^v^v^v now delivers presents to 11 houses, with Santa going one
//       direction and Robo-Santa going the other.

const std = @import("std");
const Alloc = std.mem.Allocator;

const FILE = @embedFile("../input.txt");

pub fn main() anyerror!void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    const alloc = &arena.allocator;

    // Duplication to solve both parts at the same time.
    var norobo = Santa.init(alloc, false);
    defer norobo.deinit();

    var santa = Santa.init(alloc, true);
    defer santa.deinit();

    var i: usize = 0;
    while (true) : (i += 1) {
        if (FILE[i] == '\n') break;

        const direction = Direction.fromChar(FILE[i]) orelse unreachable;
        norobo.follow(direction);
        santa.follow(direction);
    }

    std.debug.warn("no robo: {}\nrobo: {}\n", .{
        norobo.delivered.items.len,
        santa.delivered.items.len,
    });
}

const Direction = enum {
    const Self = @This();

    up,
    down,
    left,
    right,

    pub fn fromChar(ch: u8) ?Self {
        return switch (ch) {
            '>' => .right,
            'v' => .down,
            '^' => .up,
            '<' => .left,

            else => null,
        };
    }
};

const Santa = struct {
    const Self = @This();

    const Pos = struct {
        row: i64,
        col: i64,
    };

    const Turn = enum {
        real,
        robo,
        norobo,
    };

    turn: Turn,
    realpos: Pos,
    robopos: Pos,
    delivered: std.ArrayList(Pos),

    pub fn init(alloc: *Alloc, robosanta: bool) Self {
        const real_start_pos = Pos{
            .row = 0,
            .col = 0,
        };

        const robo_start_pos = real_start_pos;

        var delivered = std.ArrayList(Pos).init(alloc);
        delivered.append(real_start_pos) catch unreachable;

        var turn: Turn = undefined;
        if (robosanta) {
            turn = .real;
        } else {
            turn = .norobo;
        }

        return Self{
            .turn = turn,
            .realpos = real_start_pos,
            .robopos = robo_start_pos,
            .delivered = delivered,
        };
    }

    pub fn deinit(self: *Self) void {
        self.delivered.deinit();
    }

    pub fn follow(self: *Self, dir: Direction) void {
        const pos = switch (self.turn) {
            .norobo, .real => &self.realpos,
            .robo => &self.robopos,
        };

        switch (dir) {
            .right => pos.col += 1,
            .left => pos.col -= 1,
            .up => pos.row -= 1,
            .down => pos.row += 1,
        }

        if (!self.deliveredTo(pos.*))
            self.delivered.append(pos.*) catch unreachable;

        self.flip();
    }

    fn deliveredTo(self: Self, to: Pos) bool {
        for (self.delivered.items) |pos| {
            if (to.row == pos.row and to.col == pos.col) return true;
        }

        return false;
    }

    fn flip(self: *Self) void {
        self.turn = switch (self.turn) {
            .real => .robo,
            .robo => .real,
            .norobo => .norobo,
        };
    }
};
