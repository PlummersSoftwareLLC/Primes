//!////////////////////////////////////////////////////////////////////////////////
//! TESTS

const std = @import("std");
const sieves = @import("sieves.zig");
const runners = @import("runners.zig");
const BitSieve = sieves.BitSieve;
const IntSieve = sieves.IntSieve;
const SingleThreadedRunner = runners.SingleThreadedRunner;
const ParallelAmdahlRunner = runners.AmdahlRunner;
const ParallelGustafsonRunner = runners.GustafsonRunner;
const Wheel = @import("wheel.zig").Wheel;

const allocator = std.testing.allocator;

fn runSieve(comptime Runner: type, field_size: usize, expected_primes: usize, boosted: bool) !u64 {
    var passes: u64 = 0;
    var runner = try Runner.init(allocator, field_size, &passes);
    const initial_count: usize = field_size >> 1;
    defer runner.deinit();

    try runner.sieveInit();
    defer runner.sieveDeinit();

    if (!boosted) {
        try std.testing.expectEqual(initial_count, runner.sieve.primeCount());
    }

    runner.run();

    try std.testing.expectEqual(@as(usize, expected_primes), runner.sieve.primeCount());

    return passes;
}

const expected_results = .{
    .{ 10, 4 },
    .{ 100, 25 },
    .{ 1_000, 168 },
    .{ 10_000, 1_229 },
    .{ 100_000, 9_592 },
    .{ 1_000_000, 78_498 },
    .{ 10_000_000, 664_579 },
    .{ 100_000_000, 5_761_455 },
    // these last two take a while, so comment them out if you are developing.
    //    .{ 1_000_000_000, 50_847_534 },
    //    .{ 10_000_000_000, 455_052_511 },
};

test "Test single threaded" {
    inline for (expected_results) |result| {
        const field_size = result[0];
        const expected_primes = result[1];
        const Sieve = IntSieve(bool, .{});

        var passes = try runSieve(SingleThreadedRunner(Sieve, .{}), field_size, expected_primes, false);
        try std.testing.expectEqual(passes, 1);
    }
}

test "Test multithreaded-amdahl" {
    inline for (expected_results) |result| {
        const field_size = result[0];
        const expected_primes = result[1];
        const Sieve = IntSieve(bool, .{});

        var passes = try runSieve(ParallelAmdahlRunner(Sieve, .{}), field_size, expected_primes, false);
        try std.testing.expectEqual(passes, 1);
    }
}

test "Test mulithreaded-gustafson" {
    inline for (expected_results) |result| {
        const field_size = result[0];
        const expected_primes = result[1];
        const Sieve = IntSieve(bool, .{});

        _ = try runSieve(ParallelGustafsonRunner(Sieve, .{}), field_size, expected_primes, false);
    }
}

test "Single threaded with bitsieve/8" {
    inline for (expected_results) |result| {
        const field_size = result[0];
        const expected_primes = result[1];
        const Sieve = BitSieve(u8, .{});

        var passes = try runSieve(SingleThreadedRunner(Sieve, .{}), field_size, expected_primes, false);
        try std.testing.expectEqual(passes, 1);
    }
}

test "Single threaded with bitsieve/64" {
    inline for (expected_results) |result| {
        const field_size = result[0];
        const expected_primes = result[1];
        const Sieve = BitSieve(u64, .{});

        var passes = try runSieve(SingleThreadedRunner(Sieve, .{}), field_size, expected_primes, false);
        try std.testing.expectEqual(passes, 1);
    }
}

const pregens = [_]comptime_int{2, 3, 4, 5};
const OEIS_PRIMES = [_]usize{ 3, 5, 7, 11, 13, 17, 19 };

fn divisible_by(a: usize, b: usize) bool {
    return (a % b) == 0;
}

test "Int Sieve boost produces correct byte values" {
    inline for (expected_results) |result| {
        inline for (pregens) |pregen| {
            const field_size = result[0];
            const Sieve = IntSieve(u8, .{ .pregen = pregen });

            var sieve = try Sieve.init(allocator, field_size);
            defer sieve.deinit();

            _ = sieve.reset();

            for (sieve.field) |v, index| {
                var target = 2 * index + 1;
                var maybe_prime = true;
                for (OEIS_PRIMES[0..pregen]) |prime| {
                    maybe_prime = maybe_prime and !divisible_by(target, prime);
                    maybe_prime = maybe_prime or (prime == target);
                }

                if (maybe_prime) {
                    try std.testing.expectEqual(v, 1);
                } else {
                    try std.testing.expectEqual(v, 0);
                }
            }
        }
    }
}

const intsizes = [_]type{ u8, u16, u32, u64 };

fn bit_fetch(comptime int_t: type, slice: []int_t, target: usize) bool {
    const shift = switch (int_t) {
        u8 => 3,
        u16 => 4,
        u32 => 5,
        u64 => 6,
        else => unreachable,
    };
    const shift_t = switch (int_t) {
        u8 => u3,
        u16 => u4,
        u32 => u5,
        u64 => u6,
        else => unreachable,
    };

    const slice_mask: usize = (@as(usize, 1) << shift) - 1;

    const slice_index: usize = target >> shift;
    const slice_bit: shift_t = @truncate(shift_t, target & slice_mask);
    const bit_mask: int_t = @as(int_t, 1) << slice_bit;

    return (slice[slice_index] & bit_mask) != 0;
}

test "Bit Sieve boost produces correct bit values" {
    inline for (intsizes) |int_t| {
        inline for (expected_results) |result| {
            inline for (pregens) |pregen| {
                const field_size = result[0];
                const Sieve = BitSieve(int_t, field_size, .{ .pregen = pregen });

                var sieve = try Sieve.create(allocator);
                defer sieve.destroy();

                _ = sieve.reset();

                var index: usize = 0;
                while (index < (Sieve.size >> 1)) : (index += 1) {
                    var target = 2 * index + 1;
                    var shouldbe_prime = true;
                    for (OEIS_PRIMES[0..pregen]) |prime| {
                        shouldbe_prime = shouldbe_prime and !divisible_by(target, prime);
                        shouldbe_prime = shouldbe_prime or (prime == target);
                    }
                    var field_val = bit_fetch(int_t, sieve.field.*[0..], index);
                    std.testing.expectEqual(shouldbe_prime, field_val);
                }
            }
        }
    }
}

test "Single threaded Int Sieve with a boost from pregeneration" {
    inline for (expected_results) |result| {
        inline for (pregens) |pregen| {
            const field_size = result[0];
            const expected_primes = result[1];
            const Sieve = IntSieve(u8, .{ .pregen = pregen });

            var passes = try runSieve(SingleThreadedRunner(Sieve, .{}), field_size, expected_primes, true);
            try std.testing.expectEqual(passes, 1);
        }
    }
}

test "Single threaded Bit Sieve with a boost from pregeneration" {
    inline for (expected_results) |result| {
        inline for (pregens) |pregen| {
            const field_size = result[0];
            const expected_primes = result[1];
            const Sieve = BitSieve(u8, field_size, .{ .pregen = pregen });

            var passes = try runSieve(SingleThreadedRunner(Sieve, .{}), expected_primes, true);
            std.testing.expectEqual(@as(u64, 2), passes);
        }
    }
}
