//                            3: Largest prime factor
//
//
// The prime factors of 13195 are 5, 7, 13 and 29.
//
// What is the largest prime factor of the number 600851475143?
//
// Answer:

const std = @import("std");

const NumT = u64;
const FactorList = std.ArrayList(NumT);

pub fn main() anyerror!void {
    const alloc = std.heap.direct_allocator;

    const num: NumT = 600851475143;

    const facts = prime_factors(alloc, num);
    defer facts.deinit();

    std.debug.warn("{{");
    for(facts.toSlice()) |fact, i| {
        std.debug.warn(" {}", fact);

        if(i < facts.len - 1)
            std.debug.warn(",");
    }
    std.debug.warn(" }}\n");
}

test "prime_factors(13195)" {
    const assert = std.debug.assert;

    const num: u64 = 13195;

    const facts = prime_factors(std.heap.direct_allocator, num);
    defer facts.deinit();

    const expectedFacts = [_]NumT { 1, 5, 7, 13, 29 };

    assert(facts.len == expectedFacts.len);
    for(facts.toSlice()) |el, idx| {
        assert(el == expectedFacts[idx]);
    }
}

fn prime_factors(alloc: *std.mem.Allocator, num: NumT) FactorList {
    const maxFactor = std.math.sqrt(num);

    var facts = FactorList.init(alloc);
    facts.append(1) catch unreachable;

    var n = num;

    if(@mod(n, 2) == 0) {
        facts.append(2) catch unreachable;
        n = reduce(n, 2);
    }

    var currFactor: NumT = 3;

    while(n > 1 and currFactor <= maxFactor) : (currFactor += 2) {
        if(@mod(n, currFactor) == 0) {
            facts.append(currFactor) catch unreachable;
            n = reduce(n, currFactor);
        }
    }

    if(n > 1) {
        facts.append(n) catch unreachable;
    }

    return facts;
}

fn reduce(num: NumT, factor: NumT) NumT {
    var n = num;

    while(@mod(n, factor) == 0) {
        n = @divTrunc(n, factor);
    }

    return n;
}
