//                          14: Longest Collatz sequence
//
// The following iterative sequence is defined for the set of positive integers:
//
//     n -> n/2    (n is even)
//     n -> 3n + 1 (n is odd)
//
// Using the rule above and starting with 13, we generate the following
// sequence:
//
//     13 → 40 → 20 → 10 → 5 → 16 → 8 → 4 → 2 → 1
//
// It can be seen that this sequence (starting at 13 and finishing at 1)
// contains 10 terms. Although it has not been proved yet (Collatz Problem), it
// is thought that all starting numbers finish at 1.
//
// Which starting number, under one million, produces the longest chain?
//
// NOTE: Once the chain starts the terms are allowed to go above one million.
//
// Answer: 837_799

const std = @import("std");

pub fn main() anyerror!void {
    const col = CollatzSeq(100000, usize).calculate();
    //col.print();

    std.debug.warn("{}\n", col.findMaxLenUnder(1000000));
}

fn CollatzSeq(comptime precalc_size: usize, comptime NumT: type) type {
    return struct {
        const Self = @This();

        const CollatzArrT = [precalc_size]NumT;

        const SeqLen = struct {
            num: NumT,
            len: NumT,
        };

        vals: CollatzArrT,

        pub fn calculate() Self {
            var vals: CollatzArrT = undefined;

            var i: usize = 0;
            while(i < vals.len) : (i += 1) {
                vals[i] = nextCollatz(i + 2);
            }

            return Self {
                .vals = vals
            };
        }

        pub fn findMaxLenUnder(self: *const Self, cap: NumT) SeqLen {
            if(cap < 2) return SeqLen {
                .num = cap,
                .len = 1,
            };

            var maxLenSoFar: NumT = 0;
            var maxNum: NumT = 0;

            var i: NumT = 2;
            while(i < cap) : (i += 1) {
                const newLen = self.len(i);

                if(newLen > maxLenSoFar) {
                    maxLenSoFar = newLen;
                    maxNum = i;
                }
            }

            return SeqLen {
                .num = maxNum,
                .len = maxLenSoFar,
            };
        }

        pub fn getNext(self: *const Self, prev: NumT) NumT {
            const idx = @intCast(usize, prev);

            if(idx == 0 or idx == 1) {
                return 1;
            } else {
                return self.vals[idx - 2];
            }
        }

        pub fn len(self: *const Self, num: NumT) NumT {
            var count: NumT = 1;
            var last = num;

            while(last != 1) : (count += 1) {
                var next: NumT = undefined;

                if(last < precalc_size) {
                    next = self.getNext(last);
                } else {
                    next = nextCollatz(last);
                }

                last = next;
            }

            return count;
        }

        pub fn print(self: *const Self) void {
            const printFn = std.debug.warn;

            printFn("{{");

            for(self.vals) |el, i| {
                if(i != 0)
                    printFn(",");

                printFn(" {}: {}\n", i, el);
            }

            printFn("}}\n");
        }

        fn nextCollatz(prev: NumT) NumT {
            if(@mod(prev, 2) == 0) {
                return @divTrunc(prev, 2);
            } else {
                return 3 * prev + 1;
            }
        }
    };
}
