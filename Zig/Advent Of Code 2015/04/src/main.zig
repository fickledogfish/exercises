// --- Day 4: The Ideal Stocking Stuffer ---
//
// Santa needs help mining some AdventCoins (very similar to bitcoins) to use
// as gifts for all the economically forward-thinking little girls and boys.
//
// To do this, he needs to find MD5 hashes which, in hexadecimal, start with at
// least five zeroes. The input to the MD5 hash is some secret key (your puzzle
// input, given below) followed by a number in decimal. To mine AdventCoins,
// you must find Santa the lowest positive number (no leading zeroes: 1, 2, 3,
// ...) that produces such a hash.
//
// For example:
//
//     - If your secret key is abcdef, the answer is 609043, because the MD5
//       hash of abcdef609043 starts with five zeroes (000001dbbfa...), and it
//       is the lowest such number to do so.
//
//     - If your secret key is pqrstuv, the lowest number it combines with to
//       make an MD5 hash starting with five zeroes is 1048970; that is, the
//       MD5 hash of pqrstuv1048970 looks like 000006136ef....
//
// --- Part Two ---
//
// Now find one that starts with six zeroes.

const std = @import("std");
const md5 = std.crypto.hash.Md5.hash;
const Alloc = std.mem.Allocator;
const expectEq = std.testing.expectEqual;

const SECRET_KEY = "yzbqklnj";

const MD5_HASH_BUFFER_LEN = 16;

const Number = u32;

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    const alloc = &arena.allocator;

    const num1 = try findHash(5, alloc, SECRET_KEY);
    std.debug.warn("5 zeroes: {}\n", .{num1});

    const num2 = try findHash(6, alloc, SECRET_KEY);
    std.debug.warn("6 zeroes: {}\n", .{num2});
}

test "Provided examples" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    const alloc = &arena.allocator;

    const num1 = findHash(5, alloc, "abcdef") catch unreachable;
    expectEq(num1, 609043);

    const num2 = findHash(5, alloc, "pqrstuv") catch unreachable;
    expectEq(num2, 1048970);
}

fn findHash(
    comptime num_zeroes: usize,
    alloc: *Alloc,
    key: []const u8,
) !Number {
    var num: Number = 1;

    while (true) : (num += 1) {
        const strnum = try stringfyNum(alloc, num);
        defer alloc.free(strnum);

        const message = try alloc.alloc(u8, key.len + strnum.len);
        defer alloc.free(message);

        std.mem.copy(u8, message[0..key.len], key);
        std.mem.copy(u8, message[key.len..], strnum);

        var hashbuffer: [MD5_HASH_BUFFER_LEN]u8 = undefined;
        md5(message, &hashbuffer, .{});

        const hexedhash = try hex(alloc, hashbuffer[0..]);
        defer alloc.free(hexedhash);

        if (std.mem.startsWith(u8, hexedhash, "0" ** num_zeroes)) break;
    }

    return num;
}

fn stringfyNum(alloc: *Alloc, num: Number) ![]const u8 {
    var num_digits: usize = undefined;
    {
        const m = std.math;

        const num_f = @intToFloat(f32, num);
        const num_digits_f = m.floor(m.log10(num_f) + 1);
        num_digits = @floatToInt(usize, num_digits_f);
    }

    var str = try alloc.alloc(u8, num_digits);

    var head: u8 = @intCast(u8, num % 10);
    var tail: Number = num / 10;

    var i: usize = str.len - 1;
    while (true) : (i -= 1) {
        str[i] = fmtChar(false, head);

        head = @intCast(u8, tail % 10);
        tail = tail / 10;

        if (i == 0) break;
    }

    return str;
}

fn hex(alloc: *Alloc, hash: []const u8) ![]const u8 {
    var hexedhash = try alloc.alloc(u8, 2 * hash.len);

    var i: usize = 0;
    while (i < hash.len) : (i += 1) {
        const char = hash[i];
        const hexidx = 2 * i;

        hexedhash[hexidx] = fmtChar(false, char / 16);
        hexedhash[hexidx + 1] = fmtChar(false, @rem(char, 16));
    }

    return hexedhash;
}

fn fmtChar(uppercase: bool, char: u8) u8 {
    const startchar: u8 = 'A';

    return switch (char) {
        0...9 => '0' + char,
        else => startchar + char - 10,
    };
}
