const std = @import("std");
const heap = std.heap;
const time = std.time;
const sieves = @import("sieves.zig");
const IntSieve = sieves.IntSieve;
const BitSieve = sieves.BitSieve;
const BitSieveClassic = sieves.BitSieveClassic;
const FastSieve = sieves.FastSieve;
const runners = @import("runners.zig");
const SingleThreadedRunner = runners.SingleThreadedRunner;
const ParallelAmdahlRunner = runners.AmdahlRunner;
const ParallelGustafsonRunner = runners.GustafsonRunner;
const allocators = @import("alloc.zig");
const VAlloc = allocators.VAlloc;
const CAlloc = allocators.CAlloc;
const SAlloc = allocators.SAlloc;
const c_std_lib = @import("alloc.zig").c_std_lib;

const SIZE = 1_000_000;

var scratchpad: [SIZE]u8 align(std.mem.page_size) = undefined;

pub fn main() anyerror!void {
    const run_for = 5; // Seconds

    // check for the --all flag.
    const args = try std.process.argsAlloc(std.heap.page_allocator);
    const all = (args.len == 2) and (std.mem.eql(u8, args[1], "--all"));

    comptime const specs = .{
        .{ SingleThreadedRunner, .{}, IntSieve, .{}},
        .{ SingleThreadedRunner, .{}, IntSieve, .{.wheel_primes = 6}},
//        .{ SingleThreadedRunner, .{}, IntSieve, .{.allocator = VAlloc(.{}), .T = u8, .primeval = 1}},
//        .{ SingleThreadedRunner, .{}, IntSieve, .{.allocator = VAlloc(.{}), .T = bool, .primeval = false}},
//        .{ SingleThreadedRunner, .{}, IntSieve, .{.allocator = VAlloc(.{}), .T = bool, .primeval = true}},
//        .{ ParallelAmdahlRunner, IntSieve, false, false, false },
//        .{ ParallelAmdahlRunner, IntSieve, true, false, false },
//        .{ ParallelGustafsonRunner, IntSieve, false, false, false },
//        .{ ParallelGustafsonRunner, IntSieve, true, false, false },
//      .{ SingleThreadedRunner, .{}, BitSieve, .{.FindFactorChunk = u32}}, // equivalent to "c solution"
        .{ SingleThreadedRunner, .{}, BitSieve, .{}},
        .{ SingleThreadedRunner, .{}, BitSieve, .{.wheel_primes = 2}},
//      .{ SingleThreadedRunner, .{}, BitSieve, .{.wheel_primes = 4}},
//      .{ SingleThreadedRunner, .{}, BitSieve, .{.wheel_primes = 6}},
//      .{ SingleThreadedRunner, .{}, BitSieve, .{.unrolled = true, .RunFactorChunk = u8}},
//      .{ SingleThreadedRunner, .{}, BitSieve, .{.unrolled = true, .RunFactorChunk = u64}},
//      .{ SingleThreadedRunner, .{}, BitSieve, .{.unrolled = true, .RunFactorChunk = u64, .half_extent = true}},
//      .{ SingleThreadedRunner, .{}, BitSieve, .{.unrolled = true, .RunFactorChunk = u64, .max_vector = 2}},
//      .{ SingleThreadedRunner, .{}, BitSieve, .{.unrolled = true, .RunFactorChunk = u64, .max_vector = 4}},
//      .{ SingleThreadedRunner, .{}, BitSieve, .{.unrolled = true, .RunFactorChunk = u64, .max_vector = 8}},
//      .{ SingleThreadedRunner, .{}, BitSieve, .{.unrolled = true, .RunFactorChunk = u64, .max_vector = 2, .half_extent = true}},
//      .{ SingleThreadedRunner, .{}, BitSieve, .{.unrolled = true, .RunFactorChunk = u64, .max_vector = 4, .half_extent = true}},
//      .{ SingleThreadedRunner, .{}, BitSieve, .{.unrolled = true, .RunFactorChunk = u64, .max_vector = 8, .half_extent = true}},
//    .{ SingleThreadedRunner, .{}, BitSieve, .{.FindFactorChunk = u8, .cached_masks = true}},
//      .{ SingleThreadedRunner, .{}, BitSieve, .{.primeval = 1, .allocator = VAlloc(.{})}},
//        .{ SingleThreadedRunner, BitSieve, false, false, true },
//    .{ ParallelGustafsonRunner, .{}, BitSieve, .{.unrolled = true, .RunFactorChunk = u64, .max_vector = 4}},
//        .{ ParallelGustafsonRunner, BitSieve, false, false, false },
//        .{ ParallelGustafsonRunner, BitSieve, false, false, true },
//        .{ ParallelGustafsonRunner, BitSieve, true, false, false },
//        .{ ParallelGustafsonRunner, BitSieve, true, false, true },
//        .{ SingleThreadedRunner, FastSieve, false, false, false },
//        .{ ParallelGustafsonRunner, FastSieve, false, false, false },
//        .{ SingleThreadedRunner, IntSieve, false, true, false },
//        .{ SingleThreadedRunner, BitSieve, false, true, false },
//        .{ ParallelGustafsonRunner, BitSieve, false, true, false },
//        .{ ParallelGustafsonRunner, BitSieve, true, true, false },
    };

    inline for (specs) |spec| {
        comptime const RunnerFn = spec[0];
        comptime const runner_opts = spec[1];
        comptime const SieveFn = spec[2];
        comptime const sieve_opts = spec[3];

        comptime const Sieve = SieveFn(sieve_opts);
        comptime const Runner = RunnerFn(Sieve, runner_opts);

        try runSieveTest(Runner, run_for, SIZE);
    }
}

fn runSieveTest(
    comptime Runner: type,
    run_for: comptime_int,
    sieve_size: usize,
) anyerror!void {
    @setAlignStack(256);
    const timer = try time.Timer.start();
    var passes: u64 = 0;

    var runner = try Runner.init(sieve_size, &passes);
    defer runner.deinit();

    while (timer.read() < run_for * time.ns_per_s) {
        try runner.sieveInit();
        defer runner.sieveDeinit();
        runner.run();
    }

    const elapsed = timer.read();

    var threads = try Runner.threads();

    try printResults(
        "ManDeJan&ityonemo&SpexGuy-zig-" ++ Runner.name,
        passes,
        elapsed,
        threads,
        Runner.algo,
        Runner.bits);
}

fn printResults(backing: []const u8, passes: usize, elapsed_ns: u64, threads: usize, algo: []const u8, bits: usize) !void {
    const elapsed = @intToFloat(f32, elapsed_ns) / @intToFloat(f32, time.ns_per_s);
    const stdout = std.io.getStdOut().writer();

    try stdout.print("{s};{};{d:.5};{};faithful=yes,algorithm={s},bits={}\n", .{ backing, passes, elapsed, threads, algo, bits });
}
