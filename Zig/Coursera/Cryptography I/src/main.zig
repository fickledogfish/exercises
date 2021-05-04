const std = @import("std");
const Alloc = std.mem.Allocator;

const String      = []const u8;
const HexString   = []const u8;
const AsciiString = []const u8;

const CIPHER_FILE = "ciphers.txt";

pub fn main() anyerror!void {
    var arena = std.heap.ArenaAllocator.init(std.heap.direct_allocator);
    defer arena.deinit();

    const alloc = &arena.allocator;

    var ciphers = try CipherTexts.init(alloc, CIPHER_FILE, null);
    defer ciphers.deinit();

    // A: 0100 0001
    // Z: 0101 1010
    //
    // a: 0110 0001
    // z: 0111 1010
    //
    // 0010 0000
    // 01?? ???? XOR
    // ---------
    // 01?? ????
    //   ||----|
    //   |  |_ the character code
    //   |_ lower or uppercase
    //
    // A: 0100 0001
    //  : 0100 0000
    var r: usize = 0;
    var l: usize = 1;
    while(r < ciphers.ciphers.count() and l < ciphers.ciphers.count()) {
        std.debug.warn("{}, {}: ", r, l);
        const ASCII_RED   = "\x1b[31m";
        const ASCII_BLUE  = "\x1b[34m";
        const ASCII_RESET = "\x1b[0m";

        const c1 = ciphers.get(r);
        const c2 = ciphers.get(l);

        const minlen = if(c1.len < c2.len) c1.len else c2.len;

        var xored = try alloc.alloc(u8, minlen);
        defer alloc.free(xored);

        var i: usize = 0;
        while(i < minlen) : (i += 1) {
            xored[i] = c1[i] ^ c2[i];

            if(xored[i] >= 'A' and xored[i] <= 'Z') {
                std.debug.warn(ASCII_RED ++ "{c:0^2}" ++ ASCII_RESET, xored[i]);
            } else if(xored[i] >= 'a' and xored[i] <= 'z') {
                std.debug.warn(ASCII_BLUE ++ "{c:0^2}" ++ ASCII_RESET, xored[i]);
            } else {
                std.debug.warn("{x:0^2}", xored[i]);
            }
        }

        std.debug.warn("\n");

        r += 1;
        if(r == l) {
            r += 1;
        } else if(r >= ciphers.ciphers.count()) {
            r = 0;
            l += 1;
        }
    }

    // Exam question
    //const m1 = "attack at dawn";
    //std.debug.warn("{}: {x}\n", m1, m1);
    //const m2 = "attack at dusk";
    //std.debug.warn("{}: {x}\n", m2, m2);

    //const c1 = try parseLine(alloc, "6c73d5240a948c86981bc294814d");
    //defer alloc.free(c1);

    //var key = m1;
    //var c2 = m1;
    //var i: usize = 0;
    //while(i < m1.len) : (i += 1) {
    //    key[i] = m1[i] ^ c1[i];
    //    c2[i] = m2[i] ^ key[i];
    //}
    ////std.debug.warn("key: {}\n", key);
    //std.debug.warn("new mssg: {x}\n", c2);
    //std.debug.warn("{}\n", parseLine(alloc, "61747461636b206174206461776e"));
}

const CipherTexts = struct {
    const Self = @This();

    alloc:   *Alloc,
    target:  AsciiString,
    ciphers: std.ArrayList(AsciiString),

    pub fn init(
        alloc:            *Alloc,
        filename:         String,
        target_maybe_idx: ?usize
    ) !Self {
        var file = try CipherFile.init(alloc, CIPHER_FILE);
        defer file.deinit();

        var cipherlst = std.ArrayList(AsciiString).init(alloc);

        while(file.currLine() != null) : (file.next()) {
            const hexedcipher = file.currLine() orelse unreachable;

            const parsed_cipher = try parseLine(alloc, hexedcipher);
            defer alloc.free(parsed_cipher);

            try cipherlst.append(parsed_cipher);
        }

        const targetidx = target_maybe_idx orelse cipherlst.len - 1;
        const target = cipherlst.at(targetidx);
        const targetcpy = try alloc.alloc(u8, target.len);
        std.mem.copy(
            u8,
            targetcpy,
            target
        );

        _ = cipherlst.orderedRemove(targetidx);

        return Self {
            .alloc   = alloc,
            .target  = targetcpy,
            .ciphers = cipherlst,
        };
    }

    pub fn deinit(self: *Self) void {
        self.alloc.free(self.target);
        self.ciphers.deinit();
    }

    pub fn get(self: Self, idx: usize) AsciiString {
        return self.ciphers.at(idx);
    }

    // pub fn format(
        // value: Self,
        // comptime fmt: []const u8,
        // options: std.fmt.FormatOptions,
        // context: var,
        // comptime Errors: type,
        // output: fn (@typeOf(context), []const u8) Errors!void
    // ) Errors!void {
        // try output(context, value.target);
    // }
};

fn parseLine(alloc: *Alloc, line: HexString) !AsciiString {
    var start: usize = 0;
    var end: usize = 2;
    var idx: usize = 0;

    var parsed = try alloc.alloc(u8, @divTrunc(line.len, 2));

    while(start < line.len) : (idx += 1) {
        parsed[idx] = try std.fmt.parseInt(u8, line[start..end], 16);

        start = end;
        end += 2;
    }

    return parsed;
}

const CipherFile = struct {
    const Self = @This();

    alloc:    *Alloc,

    start:    usize,
    end:      usize,
    contents: String,

    pub fn init(alloc: *Alloc, filename: String) !Self {
        const fd = try std.fs.File.openRead(filename);
        defer fd.close();

        const stats = try fd.stat();

        var contents = try alloc.alloc(u8, stats.size);

        _ = try fd.read(contents);

        return Self {
            .alloc    = alloc,
            .start    = 0,
            .end      = findChar(contents, 0, '\n'),
            .contents = contents,
        };
    }

    pub fn next(self: *Self) void {
        self.start = self.end + 1;
        self.end = findChar(self.contents, self.start, '\n');
    }

    pub fn currLine(self: *Self) ?String {
        if(self.start == self.end and self.end == self.contents.len)
            return null;

        return self.contents[self.start..self.end];
    }

    pub fn deinit(self: *Self) void {
        self.alloc.free(self.contents);
    }

    fn findChar(buffer: String, start: usize, char: u8) usize {
        var curr = start;

        while(curr < buffer.len) : (curr += 1) {
            if(buffer[curr] == char) return curr;
        }

        return buffer.len;
    }
};
