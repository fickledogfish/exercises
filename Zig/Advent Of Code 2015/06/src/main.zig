// --- Day 6: Probably a Fire Hazard ---
//
// Because your neighbors keep defeating you in the holiday house decorating
// contest year after year, you've decided to deploy one million lights in a
// 1000x1000 grid.
//
// Furthermore, because you've been especially nice this year, Santa has mailed
// you instructions on how to display the ideal lighting configuration.
//
// Lights in your grid are numbered from 0 to 999 in each direction; the lights
// at each corner are at 0,0, 0,999, 999,999, and 999,0. The instructions
// include whether to turn on, turn off, or toggle various inclusive ranges
// given as coordinate pairs. Each coordinate pair represents opposite corners
// of a rectangle, inclusive; a coordinate pair like 0,0 through 2,2 therefore
// refers to 9 lights in a 3x3 square. The lights all start turned off.
//
// To defeat your neighbors this year, all you have to do is set up your lights
// by doing the instructions Santa sent you in order.
//
// For example:
//
//     - turn on 0,0 through 999,999 would turn on (or leave on) every light.
//
//     - toggle 0,0 through 999,0 would toggle the first line of 1000 lights,
//       turning off the ones that were on, and turning on the ones that were
//       off.
//
//     - turn off 499,499 through 500,500 would turn off (or leave off) the
//       middle four lights.
//
// After following the instructions, how many lights are lit?

const std = @import("std");

const GRID_SIZE: usize = 1000 * 1000;

pub fn main() void {
    const light_grid = Grid(GRID_SIZE).init();

    std.debug.warn("{}\n", light_grid.countOn());
}

test "Provided examples for part one" {
    const assert = std.debug.assert;

    var grid = Grid(GRID_SIZE).init();

    grid.follow("turn on 0,0 through 999,999");
}

fn Grid(comptime size: usize) type {
    return struct {
        const Self = @This();

        grid: [size]bool,

        pub fn init() Self {
            var lights: [size]bool = undefined;
            resetGrid(&lights);

            return Self{
                .grid = lights,
            };
        }

        fn countOn(self: Self) usize {
            var on: usize = 0;
            for (self.grid) |cell| {
                if (cell) on += 1;
            }

            return on;
        }

        fn follow(self: *Self, instruction: []const u8) void {
            const startsWith = std.mem.startsWith;
        }
    };
}

test "Instruction parsing" {
    const assert = std.debug.assert;

    const instr1 = Instruction.parse("toggle 623,289 through 750,524");
    assert(instr1.type_ == .Toggle);
    assert(instr1.target.from_x == 623);
    assert(instr1.target.from_y == 289);
    assert(instr1.target.to_x == 750);
    assert(instr1.target.to_y == 524);
}

const Instruction = struct {
    const Self = @This();

    type_: InstructionType,
    target: InstructionTarget,

    fn parseType(line: []const u8) InstructionType {
        const startsWith = std.mem.startsWith;

        var tp: InstructionType = undefined;

        if (startsWith(u8, line, "turn on")) {
            tp = .TurnOn;
            //} else if(startsWith(u8, line, "turn off")) {
        } else if (startsWith(u8, line, "toggle")) {
            tp = .Toggle;
        }

        return tp;
    }

    pub fn parse(line: []const u8) Self {
        const tp = parseType(line);

        const target_start: usize = blk: {
            var start: usize = 0;

            switch (tp) {
                .TurnOn => start = 7,
                .Toggle => start = 6,
            }

            break :blk start;
        };

        const fx: usize = 0;
        const fy: usize = 0;
        const tx: usize = 0;
        const ty: usize = 0;
        const target = InstructionTarget.new(fx, fy, tx, ty);

        return Self{
            .type_ = tp,
            .target = target,
        };
    }
};

const InstructionType = enum {
    TurnOn,
    Toggle,
};

const InstructionTarget = struct {
    const Self = @This();

    from_x: usize,
    from_y: usize,
    to_x: usize,
    to_y: usize,

    pub fn new(fx: usize, fy: usize, tx: usize, ty: usize) Self {
        return Self{
            .from_x = fx,
            .from_y = fy,
            .to_x = tx,
            .to_y = ty,
        };
    }
};

fn resetGrid(grid: *[]bool) void {
    for (grid.*) |*cell| {
        cell.* = false;
    }
}
