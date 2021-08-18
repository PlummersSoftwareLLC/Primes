//!////////////////////////////////////////////////////////////////////////////////
//! TESTS

const std = @import("std");
const sieves = @import("sieves.zig");
const runners = @import("runners.zig");
const a = @import("alloc.zig");
const BitSieve = sieves.BitSieve;
const IntSieve = sieves.IntSieve;
const SingleThreadedRunner = runners.SingleThreadedRunner;
const ParallelAmdahlRunner = runners.AmdahlRunner;
const ParallelGustafsonRunner = runners.GustafsonRunner;
//const Wheel = @import("wheel.zig").Wheel;

const allocator = std.testing.allocator;

fn runSieve(comptime Runner: type, sieve_size: usize, expected_primes: usize) !u64 {
    var passes: u64 = 0;
    var runner = try Runner.init(sieve_size, &passes);
    defer runner.deinit();

    const initial_count: usize = sieve_size / 2;

    try runner.sieveInit();
    defer runner.sieveDeinit();

    try std.testing.expectEqual(initial_count, runner.sieve.primeCount());

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

test "single threaded, intsieve" {
    inline for (expected_results) |result| {
        const field_size = result[0];
        const expected_primes = result[1];
        const Sieve = IntSieve(.{});

        var passes = try runSieve(SingleThreadedRunner(Sieve, .{}), field_size, expected_primes);
        try std.testing.expectEqual(passes, 1);
    }
}

test "single threaded, inverted intsieve" {
    inline for (expected_results) |result| {
        const field_size = result[0];
        const expected_primes = result[1];
        const Sieve = IntSieve(.{.primeval = 1, .allocator = a.SAlloc(a.c_std_lib)});

        var passes = try runSieve(SingleThreadedRunner(Sieve, .{}), field_size, expected_primes);
        try std.testing.expectEqual(passes, 1);
    }
}

test "Single threaded with bitsieve" {
    std.debug.print("\n", .{});
    inline for (expected_results) |result| {
        const field_size = result[0];
        const expected_primes = result[1];
        const Sieve = BitSieve(.{});

        var passes = try runSieve(SingleThreadedRunner(Sieve, .{}), field_size, expected_primes);
        try std.testing.expectEqual(passes, 1);
    }
}

//test "Single threaded with inverted bitsieve" {
//    inline for (expected_results) |result| {
//        const field_size = result[0];
//        const expected_primes = result[1];
//        const Sieve = BitSieve(.{.primeval = 1, .allocator = a.SAlloc(a.c_std_lib)});
//
//        var passes = try runSieve(SingleThreadedRunner(Sieve, .{}), field_size, expected_primes);
//        try std.testing.expectEqual(passes, 1);
//    }
//}

test "Single threaded with unrolled bitsieve" {
    inline for (expected_results) |result| {
        const field_size = result[0];
        const expected_primes = result[1];
        const Sieve = BitSieve(.{.unrolled = true});

        var passes = try runSieve(SingleThreadedRunner(Sieve, .{}), field_size, expected_primes);
        try std.testing.expectEqual(passes, 1);
    }
}

// multithreaded runs

test "Test multithreaded-amdahl" {
    inline for (expected_results) |result| {
        const field_size = result[0];
        const expected_primes = result[1];
        const Sieve = IntSieve(.{});

        var passes = try runSieve(ParallelAmdahlRunner(Sieve, .{}), field_size, expected_primes);
        try std.testing.expectEqual(passes, 1);
    }
}

test "Test multithreaded-gustafson" {
    inline for (expected_results) |result| {
        const field_size = result[0];
        const expected_primes = result[1];
        const Sieve = IntSieve(.{});

        _ = try runSieve(ParallelGustafsonRunner(Sieve, .{}), field_size, expected_primes);
    }
}

test "Test multithreaded-gustafson-bitsieve" {
    inline for (expected_results) |result| {
        const field_size = result[0];
        const expected_primes = result[1];
        const Sieve = BitSieve(.{});

        _ = try runSieve(ParallelGustafsonRunner(Sieve, .{}), field_size, expected_primes);
    }
}

//const wheels = [_]comptime_int{2, 3, 4, 5};
//const OEIS_PRIMES = [_]usize{ 3, 5, 7, 11, 13, 17, 19 };
//
//fn divisible_by(a: usize, b: usize) bool {
//    return (a % b) == 0;
//}
//
//test "Int Sieve boost produces correct byte values" {
//    inline for (expected_results) |result| {
//        inline for (wheels) |wheel| {
//            const field_size = result[0];
//            const Sieve = IntSieve(u8, .{ .wheel = wheel });
//
//            var sieve = try Sieve.init(allocator, field_size);
//            defer sieve.deinit();
//
//            _ = sieve.reset();
//
//            for (sieve.field) |v, index| {
//                var target = 2 * index + 1;
//                var maybe_prime = true;
//                for (OEIS_PRIMES[0..wheel]) |prime| {
//                    maybe_prime = maybe_prime and !divisible_by(target, prime);
//                    maybe_prime = maybe_prime or (prime == target);
//                }
//
//                if (maybe_prime) {
//                    try std.testing.expectEqual(v, 1);
//                } else {
//                    try std.testing.expectEqual(v, 0);
//                }
//            }
//        }
//    }
//}
//
//const intsizes = [_]type{ u8, u16, u32, u64 };
//
//fn bit_fetch(comptime int_t: type, slice: []int_t, target: usize) bool {
//    const shift = switch (int_t) {
//        u8 => 3,
//        u16 => 4,
//        u32 => 5,
//        u64 => 6,
//        else => unreachable,
//    };
//    const shift_t = switch (int_t) {
//        u8 => u3,
//        u16 => u4,
//        u32 => u5,
//        u64 => u6,
//        else => unreachable,
//    };
//
//    const slice_mask: usize = (@as(usize, 1) << shift) - 1;
//
//    const slice_index: usize = target >> shift;
//    const slice_bit: shift_t = @truncate(shift_t, target & slice_mask);
//    const bit_mask: int_t = @as(int_t, 1) << slice_bit;
//
//    return (slice[slice_index] & bit_mask) != 0;
//}
//
//test "Single threaded Int Sieve with a boost from wheeleration" {
//    inline for (expected_results) |result| {
//        inline for (wheels) |wheel| {
//            const field_size = result[0];
//            const expected_primes = result[1];
//            const Sieve = IntSieve(u8, .{ .wheel = wheel });
//
//            var passes = try runSieve(SingleThreadedRunner(Sieve, .{}), field_size, expected_primes, true);
//            try std.testing.expectEqual(passes, 1);
//        }
//    }
//}
//
//test "Single threaded Bit Sieve with a boost from wheeleration" {
//    inline for (expected_results) |result| {
//        inline for (wheels) |wheel| {
//            const field_size = result[0];
//            const expected_primes = result[1];
//            const Sieve = BitSieve(u8, .{ .wheel = wheel });
//
//            var passes = try runSieve(SingleThreadedRunner(Sieve, .{}), field_size, expected_primes, true);
//            try std.testing.expectEqual(@as(u64, 1), passes);
//        }
//    }
//}

//test {
//    const print = std.debug.print;
//    print("\n", .{});
//    defer print("\n", .{});
//
//    const Sieve = BitSieve(u8, .{});
//    var gold_sieve = try Sieve.init(std.testing.allocator, 1_000_000);
//    defer gold_sieve.deinit();
//    var test_sieve = try FastSieve.init(std.testing.allocator, 1_000_000);
//    defer test_sieve.deinit();
//    var prime = gold_sieve.reset();
//    try std.testing.expectEqual(prime, test_sieve.reset());
//    while (true) {
//        gold_sieve.runFactor(prime);
//        test_sieve.runFactor(prime);
//        for (test_sieve.field[gold_sieve.field.len..]) |v| {
//            try std.testing.expectEqual(@as(u8, 0), v);
//        }
//        std.testing.expectEqualSlices(u8, gold_sieve.field, test_sieve.field[0..gold_sieve.field.len]) catch |err| {
//            const mismatch_index = for (gold_sieve.field) |v, i| {
//                if (v != test_sieve.field[i]) break i;
//            } else unreachable;
//            const print_start = if (mismatch_index < 8) 0 else mismatch_index - 8;
//            const gold_print_end = if (mismatch_index + 8 >= gold_sieve.field.len) gold_sieve.field.len else mismatch_index + 8;
//            const test_print_end = if (mismatch_index + 8 >= gold_sieve.field.len) test_sieve.field.len else mismatch_index + 8;
//            print("@{}:{} prime={}\n", .{print_start / 8, print_start % 8, prime});
//            printBits(gold_sieve.field, print_start * 8, gold_print_end * 8);
//            printBits(test_sieve.field, print_start * 8, test_print_end * 8);
//            return err;
//        };
//        const num_primes = gold_sieve.primeCount();
//        try std.testing.expectEqual(num_primes, test_sieve.primeCount());
//        const new_prime = gold_sieve.findNextFactor(prime);
//        if (new_prime >= 1_000_000) break;
//        try std.testing.expectEqual(new_prime, test_sieve.findNextFactor(prime));
//        prime = new_prime;
//    }
//}
