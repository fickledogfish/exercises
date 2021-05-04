// --- Day 1: Not Quite Lisp ---
//
// Santa was hoping for a white Christmas, but his weather machine's "snow"
// function is powered by stars, and he's fresh out! To save Christmas, he
// needs you to collect fifty stars by December 25th.
//
// Collect stars by helping Santa solve puzzles. Two puzzles will be made
// available on each day in the Advent calendar; the second puzzle is unlocked
// when you complete the first. Each puzzle grants one star. Good luck!
//
// Here's an easy puzzle to warm you up.
//
// Santa is trying to deliver presents in a large apartment building, but he
// can't find the right floor - the directions he got are a little confusing.
// He starts on the ground floor (floor 0) and then follows the instructions
// one character at a time.
//
// An opening parenthesis, (, means he should go up one floor, and a closing
// parenthesis, ), means he should go down one floor.
//
// The apartment building is very tall, and the basement is very deep; he will
// never find the top or bottom floors.
//
// For example:
//
//     - (()) and ()() both result in floor 0.
//     - ((( and (()(()( both result in floor 3.
//     - ))((((( also results in floor 3.
//     - ()) and ))( both result in floor -1 (the first basement level).
//     - ))) and )())()) both result in floor -3.
//
// To what floor do the instructions take Santa?
//
// --- Part Two ---
//
// Now, given the same instructions, find the position of the first character
// that causes him to enter the basement (floor -1). The first character in the
// instructions has position 1, the second character has position 2, and so on.
//
// For example:
//
//     - ) causes him to enter the basement at character position 1.
//     - ()()) causes him to enter the basement at character position 5.
//
// What is the position of the character that causes Santa to first enter the
// basement?

const std = @import("std");
const expectEq = std.testing.expectEqual;

const INSTRUCTIONS = @embedFile("../instructions.txt");

const FloorNum = i32;

pub fn main() void {
    var follower = Follower.init();
    follower.follow(INSTRUCTIONS[0..]);

    std.debug.warn("floor: {}\nbasement: {}\n", .{
        follower.curr_floor,
        follower.first_basement,
    });
}

const Follower = struct {
    const Self = @This();

    curr_floor: FloorNum,
    curr_pos: FloorNum,
    first_basement: ?FloorNum,

    pub fn init() Self {
        return Self{
            .curr_floor = 0,
            .curr_pos = 0,
            .first_basement = null,
        };
    }

    pub fn reset(self: *Self) void {
        self.curr_floor = 0;
        self.curr_pos = 0;
        self.first_basement = null;
    }

    fn followChar(self: *Self, char: u8) void {
        self.curr_pos += 1;

        switch (char) {
            '(' => self.curr_floor += 1,
            ')' => self.curr_floor -= 1,
            else => {},
        }

        if (self.first_basement == null and self.curr_floor == -1)
            self.first_basement = self.curr_pos;
    }

    pub fn follow(self: *Self, instructions: []const u8) void {
        var i: usize = 0;
        while (i < instructions.len) : (i += 1) {
            self.followChar(instructions[i]);
        }
    }
};

test "Provided samples" {
    var follower = Follower.init();

    follower.follow("(())");
    expectEq(follower.curr_floor, 0);
    follower.reset();
    follower.follow("()()");
    expectEq(follower.curr_floor, 0);

    follower.reset();
    follower.follow("(((");
    expectEq(follower.curr_floor, 3);
    follower.reset();
    follower.follow("(()(()(");
    expectEq(follower.curr_floor, 3);

    follower.reset();
    follower.follow("))(((((");
    expectEq(follower.curr_floor, 3);

    follower.reset();
    follower.follow("())");
    expectEq(follower.curr_floor, -1);
    follower.reset();
    follower.follow("))(");
    expectEq(follower.curr_floor, -1);

    follower.reset();
    follower.follow(")))");
    expectEq(follower.curr_floor, -3);
    follower.reset();
    follower.follow(")())())");
    expectEq(follower.curr_floor, -3);

    // Part two

    follower.reset();
    follower.follow(")");
    expectEq(follower.first_basement.?, 1);

    follower.reset();
    follower.follow("()())");
    expectEq(follower.first_basement.?, 5);
}

test "Solution" {
    var follower = Follower.init();
    follower.follow(INSTRUCTIONS[0..]);

    expectEq(follower.curr_floor, 232);
    expectEq(follower.first_basement, 1783);
}
