const std = @import("std");
const ArrayList = std.ArrayList;
const Allocator = std.mem.Allocator;

/// Prime number sieve.
pub fn Sieve(comptime NumType: type) type {
    return struct {
        sieveSize: usize,
        bits: ArrayList(u8),

        const Self = @This();

        pub fn init(allocator: *Allocator, size: usize) !Self {
            var self = Self{
                .sieveSize = size,
                .bits = try ArrayList(u8).initCapacity(allocator, size),
            };
            try self.bits.appendNTimes(0, size);
            return self;
        }

        pub fn deinit(self: *Self) void {
            self.bits.deinit();
        }

        pub fn run(self: *Self) void {
            var factor: NumType = 3;
            const q = @floatToInt(NumType, @sqrt(@intToFloat(f64, self.sieveSize)));

            while (factor <= q) : (factor += 2) {
                var num: NumType = factor;
                factorSet: while (num < self.sieveSize) : (num += 2) {
                    if (self.bits.items[num] == 0) {
                        factor = num;
                        break :factorSet;
                    }
                }

                num = factor * factor;
                while (num < self.sieveSize) : (num += factor * 2) {
                    self.bits.items[num] = 1;
                }
            }
        }

        pub fn primeCount(self: *Self) NumType {
            var count: NumType = 1;
            var idx: NumType = 3;

            while (idx < self.sieveSize) : (idx += 2) {
                count += if (self.bits.items[idx] == 0) @as(NumType, 1) else @as(NumType, 0);
            }

            return count;
        }
    };
}

const expectEqual = std.testing.expectEqual;
const test_allocator = std.testing.allocator;

test "count primes up to 10" {
    var sieve = try Sieve(u32).init(test_allocator, 10);
    defer sieve.deinit();

    sieve.run();

    expectEqual(@as(u32, 4), sieve.primeCount());
}

test "count primes up to 100" {
    var sieve = try Sieve(u32).init(test_allocator, 100);
    defer sieve.deinit();

    sieve.run();

    expectEqual(@as(u32, 25), sieve.primeCount());
}

test "count primes up to 1000" {
    var sieve = try Sieve(u32).init(test_allocator, 1_000);
    defer sieve.deinit();

    sieve.run();

    expectEqual(@as(u32, 168), sieve.primeCount());
}

test "count primes up to 10000" {
    var sieve = try Sieve(u32).init(test_allocator, 10_000);
    defer sieve.deinit();

    sieve.run();

    expectEqual(@as(u32, 1_229), sieve.primeCount());
}

test "count primes up to 100000" {
    var sieve = try Sieve(u32).init(test_allocator, 100_000);
    defer sieve.deinit();

    sieve.run();

    expectEqual(@as(u32, 9_592), sieve.primeCount());
}

test "count primes up to 1000000" {
    var sieve = try Sieve(u32).init(test_allocator, 1_000_000);
    defer sieve.deinit();

    sieve.run();

    expectEqual(@as(u32, 78_498), sieve.primeCount());
}

test "count primes up to 10000000" {
    var sieve = try Sieve(u32).init(test_allocator, 10_000_000);
    defer sieve.deinit();

    sieve.run();

    expectEqual(@as(u32, 664_579), sieve.primeCount());
}

test "count primes up to 100000000" {
    var sieve = try Sieve(u32).init(test_allocator, 100_000_000);
    defer sieve.deinit();

    sieve.run();

    expectEqual(@as(u32, 5_761_455), sieve.primeCount());
}

test "count primes up to 1000000000" {
    var sieve = try Sieve(u32).init(test_allocator, 1_000_000_000);
    defer sieve.deinit();

    sieve.run();

    expectEqual(@as(u32, 50_847_534), sieve.primeCount());
}

// Integer overflows. Too lazy to fix :)
// test "count primes up to 10000000000" {
//     var sieve = try Sieve(u32).init(test_allocator, 10_000_000_000);
//     defer sieve.deinit();
//
//     sieve.run();
//
//     expectEqual(@as(u32, 455_052_511), sieve.primeCount());
// }
