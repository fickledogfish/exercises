// --- Day 5: Doesn't He Have Intern-Elves For This? ---
//
// Santa needs help figuring out which strings in his text file are naughty or
// nice.
//
// A nice string is one with all of the following properties:
//
//     - It contains at least three vowels (aeiou only), like aei, xazegov, or
//       aeiouaeiouaeiou.
//
//     - It contains at least one letter that appears twice in a row, like xx,
//       abcdde (dd), or aabbccdd (aa, bb, cc, or dd).
//
//     - It does not contain the strings ab, cd, pq, or xy, even if they are
//       part of one of the other requirements.
//
// For example:
//
//     - ugknbfddgicrmopn is nice because it has at least three vowels
//       (u...i...o...), a double letter (...dd...), and none of the disallowed
//       substrings.
//
//     - aaa is nice because it has at least three vowels and a double letter,
//       even though the letters used by different rules overlap.
//
//     - jchzalrnumimnmhp is naughty because it has no double letter.
//
//     - haegwjzuvuyypxyu is naughty because it contains the string xy.
//
//     - dvszwmarrgswjxmb is naughty because it contains only one vowel.
//
// How many strings are nice?
//
// --- Part Two ---
//
// Realizing the error of his ways, Santa has switched to a better model of
// determining whether a string is naughty or nice. None of the old rules
// apply, as they are all clearly ridiculous.
//
// Now, a nice string is one with all of the following properties:
//
//     - It contains a pair of any two letters that appears at least twice in
//       the string without overlapping, like xyxy (xy) or aabcdefgaa (aa), but
//       not like aaa (aa, but it overlaps).
//
//     - It contains at least one letter which repeats with exactly one letter
//       between them, like xyx, abcdefeghi (efe), or even aaa.
//
// For example:
//
//     - qjhvhtzxzqqjkmpb is nice because is has a pair that appears twice (qj)
//       and a letter that repeats with exactly one letter between them (zxz).
//
//     - xxyxx is nice because it has a pair that appears twice and a letter
//       that repeats with one between, even though the letters used by each
//       rule overlap.
//
//     - uurcxstgmygtbstg is naughty because it has a pair (tg) but no repeat
//       with a single letter between them.
//
//     - ieodomkazucvgmuy is naughty because it has a repeating letter with one
//       between (odo), but no pair that appears twice.
//
// How many strings are nice under these new rules?

const std = @import("std");

const FILE = @embedFile("../input.txt");
const LINE_LEN: usize = 17;

pub fn main() anyerror!void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    const alloc = &arena.allocator;

    // Part 1
    const p1 = part1Solution();
    std.debug.warn("Part 1: {}\n", .{p1});

    // Part 2
    const p2 = part2Solution();
    std.debug.warn("Part 2: {}\n", .{p2});
}

fn part2Solution() u32 {
    return mapSolutionToFile(isNiceForReal);
}

fn isNiceForReal(line: []const u8) bool {
    return hasNonOverlappingDoubles(line) and hasIntercalatedRepeat(line);
}

fn hasNonOverlappingDoubles(line: []const u8) bool {
    var i: usize = 0;
    while (i < line.len - 1) : (i += 1) {
        const crr = line[i];
        const nxt = line[i + 1];

        var j: usize = i + 2;
        while (j < line.len - 1) : (j += 1) {
            if (line[j] == crr and line[j + 1] == nxt) return true;
        }
    }

    return false;
}

fn hasIntercalatedRepeat(line: []const u8) bool {
    var i: usize = 0;
    while (i < line.len - 2) : (i += 1) {
        const c1 = line[i];
        const c2 = line[i + 1];
        const c3 = line[i + 2];

        if (c1 == c3) return true;
    }
    return false;
}

test "Provided examples for part two" {
    const assert = std.debug.assert;

    assert(hasNonOverlappingDoubles("xyxy"));
    assert(hasNonOverlappingDoubles("aabcdefgaa"));
    assert(!hasNonOverlappingDoubles("aaa"));

    assert(hasIntercalatedRepeat("xyx"));
    assert(hasIntercalatedRepeat("abcdefeghi"));
    assert(hasIntercalatedRepeat("aaa"));

    assert(isNiceForReal("qjhvhtzxzqqjkmpb"));
    assert(isNiceForReal("xxyxx"));
    assert(!isNiceForReal("uurcxstgmygtbstg"));
    assert(!isNiceForReal("ieodomkazucvgmuy"));
}

fn part1Solution() u32 {
    return mapSolutionToFile(isNice);
}

fn mapSolutionToFile(fun: fn ([]const u8) bool) u32 {
    var nice_strings: u32 = 0;

    var line_start: usize = 0;
    while (line_start < FILE.len) : (line_start += LINE_LEN) {
        var line_end = line_start + LINE_LEN - 1;

        if (fun(FILE[line_start..line_end])) nice_strings += 1;
    }

    return nice_strings;
}

fn isNice(line: []const u8) bool {
    const chrs = CharCount.count(line);

    return chrs.vowels > 2 and countDoubles(line) > 0 and
        !containsForbidden(line);
}

test "Provided examples for part one" {
    const assert = std.debug.assert;

    assert(isNice("ugknbfddgicrmopn"));
    assert(isNice("aaa"));
    assert(!isNice("jchzalrnumimnmhp"));
    assert(!isNice("haegwjzuvuyypxyu"));
    assert(!isNice("dvszwmarrgswjxmb"));
}

fn containsForbidden(line: []const u8) bool {
    const forbidden = [_][]const u8{ "ab", "cd", "pq", "xy" };

    var i: usize = 0;
    while (i < line.len - 1) : (i += 1) {
        const prv = line[i];
        const nxt = line[i + 1];

        for (forbidden) |fb| {
            if (prv == fb[0] and nxt == fb[1]) return true;
        }
    }

    return false;
}

fn countDoubles(line: []const u8) usize {
    var doubles: u16 = 0;

    var i: usize = 0;
    while (i < line.len - 1) : (i += 1) {
        const prv = line[i];
        const nxt = line[i + 1];

        if (prv == nxt) doubles += 1;
    }

    return doubles;
}

const CharCount = struct {
    const Self = @This();

    vowels: u16,
    consonants: u16,

    pub fn count(line: []const u8) Self {
        var vowels: u16 = 0;
        var consonants: u16 = 0;

        for (line) |chr| {
            if (isVowel(chr)) {
                vowels += 1;
            } else {
                consonants += 1;
            }
        }

        return Self{
            .vowels = vowels,
            .consonants = consonants,
        };
    }

    fn isVowel(chr: u8) bool {
        return switch (chr) {
            'a', 'e', 'i', 'o', 'u' => true,
            else => false,
        };
    }
};
