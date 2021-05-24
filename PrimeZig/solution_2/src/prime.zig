const std = @import("std");
const ArrayList = std.ArrayList;

/// Prime number sieve.
pub const Sieve = struct {
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

const expectEqual = std.testing.expectEqual;

fn test_primes(comptime limit: usize, expect_count: u64) !void {
    var buf: [limit >> 1]bool = undefined;
    var sieve = Sieve.init(buf[0..], limit);

    sieve.run();

    expectEqual(@as(u64, expect_count), sieve.primeCount());
}

test "count primes up to 10" {
    try test_primes(10, 4);
}

test "count primes up to 100" {
    try test_primes(100, 25);
}

test "count primes up to 1000" {
    try test_primes(1000, 168);
}

test "count primes up to 10000" {
    try test_primes(10_000, 1229);
}

test "count primes up to 100000" {
    try test_primes(100_000, 9592);
}

test "count primes up to 1000000" {
    try test_primes(1_000_000, 78_498);
}

test "the real deal function" {
    var buffer: [10]bool = undefined;
    var result: [20]bool = undefined;
    var sieve = Sieve.init(buffer[0..], 20);

    sieve.run();

    sieve.isPrime(result[0..]);

    var idx : usize = 0;
    while (idx < 20):(idx += 1) {
        
        var expected_result = switch (idx) {
            2, 3, 5, 7, 11, 13, 17, 19 => true,
            else => false
        };

        expectEqual(expected_result, result[idx]);
    }
}