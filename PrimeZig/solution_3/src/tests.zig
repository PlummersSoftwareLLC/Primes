//!////////////////////////////////////////////////////////////////////////////////
//! TESTS

const std = @import("std");
const Sieve = @import("sieve.zig").Sieve;
const SingleThreadedRunner = @import("sieve.zig").SingleThreadedRunner;

const allocator = std.testing.allocator;

fn run_sieve(comptime Runner: type, comptime expected_primes: usize) !void {
    var sieve = try Runner.init(allocator);

    sieve.reset();
    Runner.run(&sieve);
    std.testing.expectEqual(@as(usize, expected_primes), sieve.primeCount());
}

const expected_results = .{
    .{ 10, 4 },
    .{ 100, 25 },
    .{ 1_000, 168 },
    .{ 10_000, 1229 },
    .{ 100_000, 9592 },
    .{ 1_000_000, 78498 },
    .{ 10_000_000, 664579 },
};

test "Test byte sieve" {
    inline for (expected_results) |result| {
        const count = result[0];
        const expected_primes = result[1];
        const SieveType = Sieve(bool, count);

        try run_sieve(SingleThreadedRunner(SieveType), expected_primes);
    }
}