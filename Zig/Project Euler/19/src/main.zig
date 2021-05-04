//                            19: Counting Sundays
//
// You are given the following information, but you may prefer to do some
// research for yourself.
// 
//     - 1 Jan 1900 was a Monday.
//     - Thirty days has September,
//       April, June and November.
//       All the rest have thirty-one,
//       Saving February alone,
//       Which has twenty-eight, rain or shine.
//       And on leap years, twenty-nine.
//     - A leap year occurs on any year evenly divisible by 4, but not on a
//       century unless it is divisible by 400.
// 
// How many Sundays fell on the first of the month during the twentieth century
// (1 Jan 1901 to 31 Dec 2000)?
//
// Answer: 171

const std = @import("std");

pub fn main() anyerror!void {
    var d = Day.init();
    var count: u32 = 0;

    while(d.yr < 2001) : (d.next()) {
        if(d.yr > 1900 and d.dy == 1 and d.wd == .Sunday)
            count += 1;
    }

    std.debug.warn("{}\n", count);
}

const Day = struct {
    const Self = @This();

    dy: u5,
    mn: Month,
    yr: u12,
    wd: WeekDay,

    pub fn init() Self {
        return Self {
            .dy = 1,
            .mn = .January,
            .yr = 1900,
            .wd = .Monday,
        };
    }

    pub fn next(self: *Self) void {
        const leap = isLeapYear(self.yr);

        var changeMonth = false;
        var changeYear = false;

        if(self.dy == self.mn.numDays(leap)) {
            self.dy = 1;
            changeMonth = true;
        } else {
            self.dy += 1;
        }

        self.wd = self.wd.next();

        if(changeMonth) {
            if(self.mn == .December)
                changeYear = true;

            self.mn = self.mn.next();
        }

        if(changeYear)
            self.yr += 1;
    }

    pub fn print(self: *const Self) void {
        std.debug.warn(
            "{d:4}-{d:0>2}-{d:0>2} ({})\n",
            self.yr,
            @enumToInt(self.mn) + 1,
            self.dy,
            self.wd,
        );
    }
};

const YearT = u16;

fn isLeapYear(y: YearT) bool {
    return @mod(y, 4) == 0 and !(@mod(y, 100) == 0 and @mod(y, 400) != 0);
}

test "leap years" {
    const assert = std.debug.assert;

    assert(isLeapYear(1960));
    assert(isLeapYear(1932));
}

const WeekDay = enum(u3) {
    Sunday,
    Monday,
    Tuesday,
    Wednesday,
    Thursday,
    Friday,
    Saturday,

    const Self = @This();

    pub fn next(self: Self) Self {
        return @intToEnum(
            Self,
            @mod(@enumToInt(self) + 1, @memberCount(Self))
        );
    }

    pub fn format(
        self: Self,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        context: var,
        comptime Errors: type,
        output: fn (@typeOf(context), []const u8) Errors!void
        ) Errors!void {
        const wd = switch(self) {
            .Sunday    => "Sunday",
            .Monday    => "Monday",
            .Tuesday   => "Tuesday",
            .Wednesday => "Wednesday",
            .Thursday  => "Thursday",
            .Friday    => "Friday",
            .Saturday  => "Saturday",
        };

        try std.fmt.formatText(wd, fmt, options, context, Errors, output);
    }
};

test "weekdays" {
    const assert = std.debug.assert;
    var wd = WeekDay.Sunday;

    wd = wd.next(); assert(wd == .Monday);
    wd = wd.next(); assert(wd == .Tuesday);
    wd = wd.next(); assert(wd == .Wednesday);
    wd = wd.next(); assert(wd == .Thursday);
    wd = wd.next(); assert(wd == .Friday);
    wd = wd.next(); assert(wd == .Saturday);
    wd = wd.next(); assert(wd == .Sunday);
}

const Month = enum(u4) {
    January,
    February,
    March,
    April,
    May,
    June,
    July,
    August,
    September,
    October,
    November,
    December,

    const Self = @This();

    pub fn next(self: Self) Self {
        return @intToEnum(
            Self,
            @mod(@enumToInt(self) + 1, @memberCount(Self))
        );
    }

    pub fn numDays(self: Self, isLeap: bool) u16 {
        if(isLeap and self == .February) {
            return 29;
        } else {
            return switch(self) {
                .January   => 31,
                .February  => 28,
                .March     => 31,
                .April     => 30,
                .May       => 31,
                .June      => 30,
                .July      => 31,
                .August    => 31,
                .September => 30,
                .October   => 31,
                .November  => 30,
                .December  => 31,
            };
        }
    }
};

test "next month" {
    const assert = std.debug.assert;

    var month = Month.January;

    month = month.next(); assert(month == .February);
    month = month.next(); assert(month == .March);
    month = month.next(); assert(month == .April);
    month = month.next(); assert(month == .May);
    month = month.next(); assert(month == .June);
    month = month.next(); assert(month == .July);
    month = month.next(); assert(month == .August);
    month = month.next(); assert(month == .September);
    month = month.next(); assert(month == .October);
    month = month.next(); assert(month == .November);
    month = month.next(); assert(month == .December);
    month = month.next(); assert(month == .January);
}

test "number of days in a month" {
    const assert = std.debug.assert;

    assert(Month.January.  numDays(false) == 31);
    assert(Month.February. numDays(true)  == 29);
    assert(Month.February. numDays(false) == 28);
    assert(Month.March.    numDays(false) == 31);
    assert(Month.April.    numDays(false) == 30);
    assert(Month.May.      numDays(false) == 31);
    assert(Month.June.     numDays(false) == 30);
    assert(Month.July.     numDays(false) == 31);
    assert(Month.August.   numDays(false) == 31);
    assert(Month.September.numDays(false) == 30);
    assert(Month.October.  numDays(false) == 31);
    assert(Month.November. numDays(false) == 30);
    assert(Month.December. numDays(false) == 31);
}
