const std = @import("std");

/// Prime number sieve. Can be configured with any desired time
/// at compile time. So that switching between them would be easy.
pub fn Sieve(
    comptime Type: type,
    comptime true_val: Type,
    comptime false_val: Type,
    sieve_size: comptime_int,
) type {
    return struct {
        const Self = @This();
        const field_size = sieve_size >> 1;

        field: [field_size]Type align(std.mem.page_size),

        pub fn init(field: [field_size]Type) Self {
            return .{
                .field = field,
            };
        }

        pub fn run(self: *Self) void {
            @setAlignStack(256);
            comptime const q = @floatToInt(usize, @sqrt(@intToFloat(f64, sieve_size)));
            var factor: usize = 3;

            while (factor <= q) : (factor += 2) {
                var num: usize = factor;
                factorSet: while (num < field_size) : (num += 2) {
                    if (self.field[num >> 1] == true_val) {
                        factor = num;
                        break :factorSet;
                    }
                }

                num = (factor * factor) >> 1;
                while (num < field_size) : (num += factor) {
                    self.field[num] = false_val;
                }
            }
        }

        pub fn primeCount(self: *Self) usize {
            var count: usize = 0;
            var idx: usize = 0;

            while (idx < field_size) : (idx += 1) {
                count += @boolToInt(self.field[idx] == true_val);
            }

            return count;
        }
    };
}

pub fn BitSieve( // slightly less performant but uses 8x less memory
    comptime sieve_size: comptime_int,
) type {
    return struct {
        const Self = @This();
        const field_size = sieve_size >> 1;
        const BitSet = std.StaticBitSet(field_size);

        field: BitSet align(std.mem.page_size),

        pub fn init() Self {
            return .{
                .field = BitSet.initEmpty(),
            };
        }

        pub fn run(self: *Self) void {
            @setAlignStack(256);
            comptime const q = @floatToInt(usize, @sqrt(@intToFloat(f64, sieve_size)));
            var factor: usize = 3;

            while (factor <= q) : (factor += 2) {
                var num: usize = factor;
                factorSet: while (num < field_size) : (num += 2) {
                    if (!self.field.isSet(num >> 1)) {
                        factor = num;
                        break :factorSet;
                    }
                }

                num = (factor * factor) >> 1;
                while (num < field_size) : (num += factor) {
                    self.field.set(num);
                }
            }
        }

        pub fn primeCount(self: *Self) usize {
            var count: usize = 0;
            var idx: usize = 0;

            while (idx < field_size) : (idx += 1) {
                count += @boolToInt(!self.field.isSet(idx));
            }

            return count;
        }
    };
}

/// Prime number sieve.
pub const ItyonemoSieve = struct {
    limit: usize,
    slice: []bool,

    const Self = @This();

    /// Initializes the sieve struct.  Please pass me a slice!
    /// The length of the slice should be half the limit.
    pub fn init(slice: []bool, limit: usize) Self {
        std.debug.assert(slice.len == limit >> 1);
        // set all odds to be prime, except 1, which is not prime
        slice[0] = false;
        std.mem.set(bool, slice[1..], true);
        return Self{
            .limit = limit,
            .slice = slice
        };
    }

    pub fn run(self: Self) void {
        var factor : u64 = 3;  // start factor at "3"
        // calculate the stopping point
        const q = @floatToInt(u64, @sqrt(@intToFloat(f64, self.limit)));
        const size = self.limit >> 1;

        while (factor <= q) : (factor += 2) {
            // search for the next factor to try, we can ignore composite numbers.
            var num: u64 = factor;
            factorSearch: while (num < size) : (num += 2) {
                if (self.slice[num >> 1]) {
                    factor = num;
                    break :factorSearch;
                }
            }

            // start filling out the sieve at factor * factor.
            var start:u64 = (factor * factor) >> 1;
            while (start < size) : (start += factor) {
                self.slice[start] = false;
            }
        }
    }

    pub fn primeCount(self: Self) u64 {
        var count: u64 = 1;  // start with 2, which we aren't counting using this fn
        var idx: u64 = 0;

        while (idx < (self.limit >> 1)) : (idx += 1) {
            count += @boolToInt(self.slice[idx]);
        }

        return count;
    }

    pub fn isPrime(self: Self, result: []bool) void {
        for (result) | *val, idx | {
            result[idx] = if (idx % 2 == 0) idx == 2 else self.slice[idx >> 1];
        }
    }
};

test "Test byte sieve" {
    const expected_results = .{
        .{             10,         4 },
        .{            100,        25 },
        .{          1_000,       168 },
        .{         10_000,      1229 },
        .{        100_000,      9592 },
        .{      1_000_000,     78498 },
        // Uncommenting the following tests make my compiler crash ¯\_(ツ)_/¯ Probably cause it's allocating huge memory sizes at compile time
        // .{     10_000_000,    664579 },
        // .{    100_000_000,   5761455 },
        // .{  1_000_000_000,  50847534 },
        // .{ 10_000_000_000, 455052511 },
    };

    inline for (expected_results) |result| {
        const size = result[0];
        const count = result[1];

        var field = [_]bool{true} ** (size >> 1);
        var sieve = Sieve(bool, true, false, size).init(field);

        sieve.run();
        std.testing.expectEqual(@as(usize, count), sieve.primeCount());
    }
}

test "Test bit sieve" {
    const expected_results = .{
        .{             10,         4 },
        .{            100,        25 },
        .{          1_000,       168 },
        .{         10_000,      1229 },
        .{        100_000,      9592 },
        .{      1_000_000,     78498 },
        .{     10_000_000,    664579 },
        // Uncommenting the following tests make my compiler crash ¯\_(ツ)_/¯ Probably cause it's allocating huge memory sizes at compile time
        // .{    100_000_000,   5761455 },
        // .{  1_000_000_000,  50847534 },
        // .{ 10_000_000_000, 455052511 },
    };

    inline for (expected_results) |result| {
        const size = result[0];
        const count = result[1];

        var sieve = BitSieve(size).init();

        sieve.run();
        std.testing.expectEqual(@as(usize, count), sieve.primeCount());
    }
}
