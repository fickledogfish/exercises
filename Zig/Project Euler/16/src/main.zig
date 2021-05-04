//                              16: Power digit sum
//
// 2^15 = 32768 and the sum of its digits is 3 + 2 + 7 + 6 + 8 = 26.
//
// What is the sum of the digits of the number 2^1000?
//
// Answer: 1366

const std = @import("std");

pub fn main() anyerror!void {
    var num = BigInt.init(std.heap.direct_allocator, 2);
    defer num.deinit();

    {
        var itercount: usize = 0;
        while(itercount < 999) : (itercount += 1) {
            num.mul(2);
        }
    }

    var sum: u64 = 0;
    for(num.digits()) |el| {
        sum += el;
    }

    std.debug.warn("{}\n", sum);
}

const BigInt = struct {
    const Self  = @This();
    const Digit = u4;
    const List  = std.ArrayList(Digit);

    data: List,

    pub fn init(alloc: *std.mem.Allocator, start: Digit) Self {
        var data = List.init(alloc);
        data.append(start) catch unreachable;

        return Self {
            .data = data,
        };
    }

    pub fn deinit(self: *Self) void {
        self.data.deinit();
    }

    pub fn mul(self: *Self, n: u64) void {
        var carry: u64 = 0;

        var i: usize = 0;
        while(i < self.data.len) : (i += 1) {
            const prev_digit = @intCast(u64, self.data.at(i));
            const result = prev_digit * n + carry;
            carry = @divTrunc(result, 10);

            const new_digit = @mod(result, 10);

            self.data.set(i, @intCast(Digit, new_digit));
        }

        while(carry > 0) {
            const digit = @intCast(Digit, @mod(carry, 10));
            carry = @divTrunc(carry, 10);
            self.data.append(digit) catch unreachable;
        }
    }

    pub fn digits(self: *const Self) []Digit {
        return self.data.toSlice();
    }

    pub fn print(self: *const Self) void {
        var i = self.data.count() - 1;
        while(true) {
            std.debug.warn("{}", self.data.at(i));

            if(i == 0) break;
            i -= 1;
        }

        std.debug.warn("\n");
    }
};
