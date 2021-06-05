//!////////////////////////////////////////////////////////////////////////////////
//! TESTS

const std = @import("std");
const Sieve = @import("sieve.zig").Sieve;
const SingleThreadedRunner = @import("sieve.zig").SingleThreadedRunner;
const ParallelAmdahlRunner = @import("parallel.zig").AmdahlRunner;
//const ParallelGustafsonRunner = parallel.GustafsonRunner;

const allocator = std.testing.allocator;

fn run_sieve(comptime Runner: type, comptime expected_primes: usize) !void {
    var runner = try Runner.init(allocator);
    var initial_count: usize = @TypeOf(runner.sieve).size >> 1;
    defer runner.deinit();

    runner.reset();
    std.testing.expectEqual(@as(usize, initial_count), runner.sieve.primeCount());
    runner.run();
    std.testing.expectEqual(@as(usize, expected_primes), runner.sieve.primeCount());

    runner.reset();
    std.testing.expectEqual(@as(usize, initial_count), runner.sieve.primeCount());
    runner.run();
    std.testing.expectEqual(@as(usize, expected_primes), runner.sieve.primeCount());
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
        const SieveType = Sieve(bool, count);

        try run_sieve(SingleThreadedRunner(SieveType), expected_primes);
    }
}

test "Test multithreaded-Amdahl" {
    inline for (expected_results) |result| {
        const count = result[0];
        const expected_primes = result[1];
        const SieveType = Sieve(bool, count);

        try run_sieve(ParallelAmdahlRunner(SieveType), expected_primes);
    }
}