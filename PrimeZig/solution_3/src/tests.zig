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

const allocator = std.testing.allocator;

fn runSieve(comptime Runner: type, comptime expected_primes: usize) !u64 {
    var runner = Runner{};
    try runner.init(allocator);
    var initial_count: usize = @TypeOf(runner.sieve).size >> 1;
    defer runner.deinit();
    var passes: u64 = 0;

    runner.reset();
    std.testing.expectEqual(@as(usize, initial_count), runner.sieve.primeCount());
    runner.run(&passes);
    std.testing.expectEqual(@as(usize, expected_primes), runner.sieve.primeCount());

    runner.reset();
    std.testing.expectEqual(@as(usize, initial_count), runner.sieve.primeCount());
    runner.run(&passes);
    std.testing.expectEqual(@as(usize, expected_primes), runner.sieve.primeCount());

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
        const count = result[0];
        const expected_primes = result[1];
        const Sieve = IntSieve(bool, count, .{});

        var passes = try runSieve(SingleThreadedRunner(Sieve, .{}), expected_primes);
        std.testing.expectEqual(@as(u64, 2), passes);
    }
}

test "Test multithreaded-amdahl" {
    inline for (expected_results) |result| {
        const count = result[0];
        const expected_primes = result[1];
        const Sieve = IntSieve(bool, count, .{});

        var passes = try runSieve(ParallelAmdahlRunner(Sieve, .{}), expected_primes);
        std.testing.expectEqual(@as(u64, 2), passes);
    }
}

test "Test mulithreaded-gustafson" {
    inline for (expected_results) |result| {
        const count = result[0];
        const expected_primes = result[1];
        const Sieve = IntSieve(bool, count, .{});

        _ = try runSieve(ParallelGustafsonRunner(Sieve, .{}), expected_primes);
    }
}

test "Single threaded with bitsieve/8" {
    inline for (expected_results) |result| {
        const count = result[0];
        const expected_primes = result[1];
        const Sieve = BitSieve(u8, count, .{});

        var passes = try runSieve(SingleThreadedRunner(Sieve, .{}), expected_primes);
        std.testing.expectEqual(@as(u64, 2), passes);
    }
}

test "Single threaded with bitsieve/64" {
    inline for (expected_results) |result| {
        const count = result[0];
        const expected_primes = result[1];
        const Sieve = BitSieve(u64, count, .{});

        var passes = try runSieve(SingleThreadedRunner(Sieve, .{}), expected_primes);
        std.testing.expectEqual(@as(u64, 2), passes);
    }
}

const big_results = .{
    .{ 10_000, 1_229 },
    .{ 100_000, 9_592 },
    .{ 1_000_000, 78_498 },
    .{ 10_000_000, 664_579 },
}

const pregens = .{2, 3, 4, 5};

test "Single threaded with a boost from pregeneration" {
    inline for (big_results) |result| {
        inline for (pregen) |pregen| {
            const count = result[0];
            const expected_primes = result[1];
            const Sieve = IntSieve(u64, count, .{.pregen =  pregen});
        }
    }
}
