const std = @import("std");

// sieves
const sieves = @import("sieves.zig");
const IntSieve = sieves.IntSieve;
const BitSieve = sieves.BitSieve;
const VecSieve = sieves.VecSieve;

// runners
const runners = @import("runners.zig");
const SingleThreadedRunner = runners.SingleThreadedRunner;
const ParallelAmdahlRunner = runners.AmdahlRunner;
const ParallelGustafsonRunner = runners.GustafsonRunner;

// allocators
const allocators = @import("alloc.zig");
const c_std_lib = @import("alloc.zig").c_std_lib;
const VAlloc = allocators.VAlloc;
const CAlloc = allocators.CAlloc;
const SAlloc = allocators.SAlloc;

const SIZE = 1_000_000;

var scratchpad: [SIZE]u8 align(std.mem.page_size) = undefined;

const ARCH_BITS = std.builtin.target.cpu.arch.ptrBitWidth();
const ARCH_64 = ARCH_BITS == 64;
const ARCH_32 = ARCH_BITS == 32;

const SELECTED = true; // making this false lets us compile just one in debug without commenting out
const build_options = @import("build_options");
const IS_RPI4 = std.builtin.target.cpu.arch.isARM() and build_options.arm_is_rpi4;

pub fn main() anyerror!void {
    const run_for = 5; // Seconds

    // did we set -Dall?
    const ONLY_WHEN_ALL = build_options.all;

    const NonClearing = SAlloc(c_std_lib, .{.should_clear = false});

    comptime const specs = .{
        // single-threaded int runners
        .{ SingleThreadedRunner, .{}, IntSieve, .{}, SELECTED},
        .{ SingleThreadedRunner, .{}, IntSieve, .{.wheel_primes = 6}, SELECTED},
        // comparison against C runner.
        .{ SingleThreadedRunner, .{}, BitSieve, .{}, SELECTED}, // equivalent to C
        // best singlethreaded base runner
        .{ SingleThreadedRunner, .{}, BitSieve, .{.unrolled = .{.max_vector = 8}}, SELECTED},
        // ----   pessimizations on singlethreaded (base)
        // RunFactorChunk matters
        .{ SingleThreadedRunner, .{}, BitSieve, .{.unrolled = .{.max_vector = 16}, .RunFactorChunk = u32}, ONLY_WHEN_ALL and ARCH_64},
        .{ SingleThreadedRunner, .{}, BitSieve, .{.unrolled = .{.max_vector = 8}, .RunFactorChunk = u32}, ONLY_WHEN_ALL and ARCH_64},
        .{ SingleThreadedRunner, .{}, BitSieve, .{.unrolled = .{.max_vector = 16}, .RunFactorChunk = u8}, ONLY_WHEN_ALL},
        // FindFactorChunk matters
        .{ SingleThreadedRunner, .{}, BitSieve, .{.unrolled = .{.max_vector = 8}, .FindFactorChunk = u8}, ONLY_WHEN_ALL},
        .{ SingleThreadedRunner, .{}, BitSieve, .{.unrolled = .{.max_vector = 8}, .FindFactorChunk = u64}, ONLY_WHEN_ALL},
        // Allocator matters
        .{ SingleThreadedRunner, .{}, BitSieve, .{.unrolled = .{.max_vector = 8}, .allocator = SAlloc(c_std_lib, .{}), .note = "malloc"}, ONLY_WHEN_ALL},
        .{ SingleThreadedRunner, .{}, BitSieve, .{.unrolled = .{.max_vector = 8}, .allocator = VAlloc(.{}), .note = "custom-allocator"}, ONLY_WHEN_ALL},
        // Inverted is not generally better
        .{ SingleThreadedRunner, .{}, BitSieve, .{.unrolled = .{.max_vector = 8}, .allocator = SAlloc(c_std_lib, .{}), .PRIME = 1}, ONLY_WHEN_ALL},
        // "Advanced FindFactor doesn't help"
        .{ SingleThreadedRunner, .{}, BitSieve, .{.unrolled = .{.max_vector = 8}, .find_factor = .advanced}, ONLY_WHEN_ALL},
        .{ SingleThreadedRunner, .{}, BitSieve, .{.unrolled = .{.max_vector = 8}, .find_factor = .advanced, .FindFactorChunk = u32}, ONLY_WHEN_ALL and ARCH_64},
        .{ SingleThreadedRunner, .{}, BitSieve, .{.unrolled = .{.max_vector = 8}, .find_factor = .advanced, .FindFactorChunk = u8}, ONLY_WHEN_ALL},
        // vectorization
        .{ SingleThreadedRunner, .{}, BitSieve, .{.unrolled = .{.max_vector = 8, .half_extent = false}}, ONLY_WHEN_ALL},
        .{ SingleThreadedRunner, .{}, BitSieve, .{.unrolled = .{.max_vector = 4}}, ONLY_WHEN_ALL},
        .{ SingleThreadedRunner, .{}, BitSieve, .{.unrolled = .{.max_vector = 2}}, ONLY_WHEN_ALL},
        .{ SingleThreadedRunner, .{}, BitSieve, .{.unrolled = .{.max_vector = 1}}, ONLY_WHEN_ALL},
        // best wheel runner
        .{ SingleThreadedRunner, .{}, BitSieve, .{.unrolled = .{.max_vector = 8}, .wheel_primes = 5, .allocator = NonClearing}, SELECTED},
        .{ SingleThreadedRunner, .{}, BitSieve, .{.unrolled = .{.max_vector = 8}, .FindFactorChunk = u8, .wheel_primes = 5, .allocator = NonClearing, .PRIME = 1, .find_factor = .advanced}, SELECTED},
        // ----   pessimizations on singlethreaded (wheel)
        .{ SingleThreadedRunner, .{}, BitSieve, .{.unrolled = .{.max_vector = 8}, .wheel_primes = 4, .allocator = NonClearing}, ONLY_WHEN_ALL},
        // experimental vector sieve (typically less performant than unrolled bitsieve)
        .{ SingleThreadedRunner, .{}, VecSieve, .{.PRIME = 1, .allocator = SAlloc(c_std_lib, .{})}, SELECTED and !IS_RPI4},
        // multi-threaded, amdahl
        .{ ParallelAmdahlRunner, .{}, IntSieve, .{}, SELECTED and !IS_RPI4},
        // multi-threaded int runner
        .{ ParallelGustafsonRunner, .{}, IntSieve, .{}, SELECTED},
        .{ ParallelGustafsonRunner, .{}, IntSieve, .{.wheel_primes = 6}, SELECTED},
        // multi-threaded int regressions
        .{ ParallelGustafsonRunner, .{.no_ht = true}, IntSieve, .{.wheel_primes = 6, .note = "no-ht"}, ONLY_WHEN_ALL},
        // multi-threaded, base
        .{ ParallelGustafsonRunner, .{}, BitSieve, .{.unrolled = .{.max_vector = 8}}, SELECTED},
        // multi-threaded, wheel (which one is best may depend on the time of day)
        .{ ParallelGustafsonRunner, .{}, BitSieve, .{.unrolled = .{.max_vector = 8}, .FindFactorChunk = u32, .wheel_primes = 3, .allocator = NonClearing}, ONLY_WHEN_ALL},
        .{ ParallelGustafsonRunner, .{}, BitSieve, .{.unrolled = .{.max_vector = 8}, .FindFactorChunk = u32, .wheel_primes = 4, .allocator = NonClearing}, SELECTED},
        .{ ParallelGustafsonRunner, .{}, BitSieve, .{.unrolled = .{.max_vector = 8}, .FindFactorChunk = u32, .wheel_primes = 5, .allocator = NonClearing}, SELECTED},
        // multi-threaded base regressions
        .{ ParallelGustafsonRunner, .{.no_ht = true}, BitSieve, .{.unrolled = .{.max_vector = 8}, .note = "no-ht"}, ONLY_WHEN_ALL},
    };

    const args = try std.process.argsAlloc(std.heap.page_allocator);
    const count = if ((args.len == 3) and (std.mem.eql(u8, args[1], "-n"))) (std.fmt.parseInt(usize, args[2], 10) catch @panic("badarg")) else 1;

    var so_far: usize = 0;
    while (so_far < count) : (so_far += 1) {

    inline for (specs) |spec| {
        comptime const RunnerFn = spec[0];
        comptime const runner_opts = spec[1];
        comptime const SieveFn = spec[2];
        comptime const sieve_opts = spec[3];

        comptime const Sieve = SieveFn(sieve_opts);
        comptime const Runner = RunnerFn(Sieve, runner_opts);
        comptime const should_run = spec[4];

        if (should_run) {
            try runSieveTest(Runner, run_for, SIZE);
        }
    }
    }
}

fn runSieveTest(
    comptime Runner: type,
    run_for: comptime_int,
    sieve_size: usize,
) anyerror!void {
    @setAlignStack(256);

    try printName(Runner);

    const timer = try std.time.Timer.start();
    var passes: u64 = 0;

    var runner = try Runner.init(sieve_size, &passes);
    defer runner.deinit();

    while (timer.read() < run_for * std.time.ns_per_s) {
        try runner.sieveInit();
        defer runner.sieveDeinit();
        runner.run();
    }

    const elapsed = timer.read();

    var threads = try Runner.threads();

    try printResults(
        passes,
        elapsed,
        threads,
        Runner.algo,
        Runner.bits);
}

fn printName(comptime Runner: type) !void {
    const stdout = std.io.getStdOut().writer();
    try stdout.print("ManDeJan&ityonemo&SpexGuy-zig-{s}", .{Runner.name});
}

fn printResults(passes: usize, elapsed_ns: u64, threads: usize, algo: []const u8, bits: usize) !void {
    const elapsed = @intToFloat(f32, elapsed_ns) / @intToFloat(f32, std.time.ns_per_s);
    const stdout = std.io.getStdOut().writer();

    try stdout.print(";{};{d:.5};{};faithful=yes,algorithm={s},bits={}\n", .{ passes, elapsed, threads, algo, bits });
}
