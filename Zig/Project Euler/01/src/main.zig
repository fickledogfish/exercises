//                            1: Multiples of 3 and 5
//
// If we list all the natural numbers below 10 that are multiples of 3 or 5, we
// get 3, 5, 6 and 9. The sum of these multiples is 23.
//
// Find the sum of all the multiples of 3 or 5 below 1000.
//
// Answer: 233168

const std = @import("std");
const AList = std.ArrayList(u64);

const CAP: i32 = 1000;

pub fn main() anyerror!void {
    var arena = std.heap.ArenaAllocator.init(std.heap.direct_allocator);
    defer arena.deinit();

    const nums = try list_multiples(&arena.allocator, CAP);

    var sum: u64 = 0;
    var i: usize = 0;
    while(i < nums.len) : (i += 1) {
        //std.debug.warn("{}: {}\n", i, nums.items[i]);
        sum += nums.items[i];
    }

    std.debug.warn("{}\n", sum);
}

fn list_multiples(alloc: *std.mem.Allocator, cap: u64) anyerror!AList {
    var nums = AList.init(alloc);

    var num: u64 = 1;
    while(num < cap) : (num += 1) {
        if(num % 3 == 0 or num % 5 == 0) {
            try nums.append(num);
        }
    }

    return nums;
}
