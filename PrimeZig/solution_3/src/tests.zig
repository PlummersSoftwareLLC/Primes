//!////////////////////////////////////////////////////////////////////////////////
//! TESTS

const std = @import("std");
const sieves = @import("sieves.zig");
const runners = @import("runners.zig");
const a = @import("alloc.zig");
const BitSieve = sieves.BitSieve;
const IntSieve = sieves.IntSieve;
const VecSieve = sieves.VecSieve;
const SingleThreadedRunner = runners.SingleThreadedRunner;
const ParallelAmdahlRunner = runners.AmdahlRunner;
const ParallelGustafsonRunner = runners.GustafsonRunner;

fn runSieve(comptime Runner: type, sieve_size: usize, expected_primes: usize, base: bool) !u64 {
    var passes: u64 = 0;
    var runner = try Runner.init(sieve_size, &passes);
    defer runner.deinit();

    const initial_count: usize = sieve_size / 2;

    try runner.sieveInit();
    defer runner.sieveDeinit();

    if (base) {
        try std.testing.expectEqual(initial_count, runner.sieve.primeCount());
    }

    runner.run();

    std.testing.expectEqual(@as(usize, expected_primes), runner.sieve.primeCount()) catch |err| {
        runner.sieve.dump();
        return err;
    };

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

test "single threaded, intsieve" {
    inline for (expected_results) |result| {
        const field_size = result[0];
        const expected_primes = result[1];
        const Sieve = IntSieve(.{});

        var passes = try runSieve(SingleThreadedRunner(Sieve, .{}), field_size, expected_primes, true);
        try std.testing.expectEqual(passes, 1);
    }
}

test "Single threaded with bitsieve" {
    inline for (expected_results) |result| {
        const field_size = result[0];
        const expected_primes = result[1];
        const Sieve = BitSieve(.{});

        var passes = try runSieve(SingleThreadedRunner(Sieve, .{}), field_size, expected_primes, true);
        try std.testing.expectEqual(passes, 1);
    }
}

test "Single threaded with inverted bitsieve" {
    inline for (expected_results) |result| {
        const field_size = result[0];
        const expected_primes = result[1];
        const Sieve = BitSieve(.{.PRIME = 1, .allocator = a.SAlloc(a.c_std_lib, .{})});

        var passes = try runSieve(SingleThreadedRunner(Sieve, .{}), field_size, expected_primes, true);
        try std.testing.expectEqual(passes, 1);
    }
}

test "Single threaded with unrolled bitsieve" {
    inline for (expected_results) |result| {
        const field_size = result[0];
        const expected_primes = result[1];
        const Sieve = BitSieve(.{.RunFactorChunk = u8, .unrolled = true});

        var passes = try runSieve(SingleThreadedRunner(Sieve, .{}), field_size, expected_primes, true);
        try std.testing.expectEqual(passes, 1);
    }
}

test "Single threaded with unrolled bitsieve, vector 2" {
    inline for (expected_results) |result| {
        const field_size = result[0];
        const expected_primes = result[1];
        const Sieve = BitSieve(.{.unrolled = true, .RunFactorChunk = u8, .max_vector = 2});

        var passes = try runSieve(SingleThreadedRunner(Sieve, .{}), field_size, expected_primes, true);
        try std.testing.expectEqual(passes, 1);
    }
}

test "Single threaded with unrolled bitsieve, vector 4" {
    inline for (expected_results) |result| {
        const field_size = result[0];
        const expected_primes = result[1];
        const Sieve = BitSieve(.{.unrolled = true, .RunFactorChunk = u8, .max_vector = 4});

        var passes = try runSieve(SingleThreadedRunner(Sieve, .{}), field_size, expected_primes, true);
        try std.testing.expectEqual(passes, 1);
    }
}

test "Single threaded with unrolled bitsieve, u64 vector" {
    inline for (expected_results) |result| {
        const field_size = result[0];
        const expected_primes = result[1];
        const Sieve = BitSieve(.{.unrolled = true, .RunFactorChunk = u64, .max_vector = 4});

        var passes = try runSieve(SingleThreadedRunner(Sieve, .{}), field_size, expected_primes, true);
        try std.testing.expectEqual(passes, 1);
    }
}

// multithreaded runs

test "Test multithreaded-amdahl" {
    inline for (expected_results) |result| {
        const field_size = result[0];
        const expected_primes = result[1];
        const Sieve = IntSieve(.{});

        var passes = try runSieve(ParallelAmdahlRunner(Sieve, .{}), field_size, expected_primes, true);
        try std.testing.expectEqual(passes, 1);
    }
}

test "Test multithreaded-gustafson" {
    inline for (expected_results) |result| {
        const field_size = result[0];
        const expected_primes = result[1];
        const Sieve = IntSieve(.{});

        _ = try runSieve(ParallelGustafsonRunner(Sieve, .{}), field_size, expected_primes, true);
    }
}

test "Test multithreaded-gustafson-bitsieve" {
    inline for (expected_results) |result| {
        const field_size = result[0];
        const expected_primes = result[1];
        const Sieve = BitSieve(.{});

        _ = try runSieve(ParallelGustafsonRunner(Sieve, .{}), field_size, expected_primes, true);
    }
}

test "Int sieve with wheels work" {
    inline for (expected_results) |result| {
        const field_size = result[0];
        const expected_primes = result[1];
        const Sieve = IntSieve(.{.wheel_primes = 2});

        var passes = try runSieve(SingleThreadedRunner(Sieve, .{}), field_size, expected_primes, false);
        try std.testing.expectEqual(passes, 1);
    }
}

test "Bit sieve with wheels work" {
    inline for (expected_results) |result| {
        const field_size = result[0];
        const expected_primes = result[1];
        const Sieve = BitSieve(.{.wheel_primes = 2});

        var passes = try runSieve(SingleThreadedRunner(Sieve, .{}), field_size, expected_primes, false);
        try std.testing.expectEqual(passes, 1);
    }
}

test "Vector Sieve works" {
    inline for (expected_results) |result| {
        const field_size = result[0];
        const expected_primes = result[1];
        const Sieve = VecSieve(.{.PRIME = 1, .allocator = a.SAlloc(a.c_std_lib, .{})});

        var passes = try runSieve(SingleThreadedRunner(Sieve, .{}), field_size, expected_primes, false);
        try std.testing.expectEqual(passes, 1);
    }
}
