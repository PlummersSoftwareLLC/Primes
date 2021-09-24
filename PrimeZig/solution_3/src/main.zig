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

const ARCH_64 = std.builtin.target.cpu.arch.ptrBitWidth() == 64;
const ARCH_32 = std.builtin.target.cpu.arch.ptrBitWidth() == 32;
// arm systems tend to have constrained memory so compile less stuff
const NOT_ARM = (!std.builtin.target.cpu.arch.isARM());

pub fn main() anyerror!void {
    const run_for = 5; // Seconds

    // did we set -Dall?
    const all = @import("build_options").all;

    const NonClearing = SAlloc(c_std_lib, .{.should_clear = false});

    comptime const specs = .{
//        // single-threaded.
//        .{ SingleThreadedRunner, .{}, IntSieve, .{}, true},
//        .{ SingleThreadedRunner, .{}, IntSieve, .{.wheel_primes = 6}, true},
//        .{ SingleThreadedRunner, .{}, BitSieve, .{.RunFactorChunk = u64}, false},
//        .{ SingleThreadedRunner, .{}, BitSieve, .{.RunFactorChunk = u32, .FindFactorChunk = u32}, ARCH_32 and NOT_ARM}, // equivalent to C
//        .{ SingleThreadedRunner, .{}, BitSieve, .{.RunFactorChunk = u64, .FindFactorChunk = u32}, ARCH_64 and NOT_ARM}, // equivalent to C
//        // best singlethreaded base runners
//        .{ SingleThreadedRunner, .{}, BitSieve, .{.unrolled = true, .RunFactorChunk = u32, .FindFactorChunk = u32}, ARCH_32},
        .{ SingleThreadedRunner, .{}, BitSieve, .{.unrolled = true, .RunFactorChunk = u64, .FindFactorChunk = u32, .max_vector = 8, .half_extent = true}, ARCH_64},
        .{ SingleThreadedRunner, .{}, BitSieve, .{.RunFactorChunk = u64, .FindFactorChunk = u32}, ARCH_64},
        // ----   pessimizations on singlethreadedness (base)
//        // RunFactorChunk matters
//        .{ SingleThreadedRunner, .{}, BitSieve, .{.unrolled = true, .RunFactorChunk = u32, .FindFactorChunk = u32}, false},
//        .{ SingleThreadedRunner, .{}, BitSieve, .{.unrolled = true, .RunFactorChunk = u8, .FindFactorChunk = u32}, false},
//        // vectorization
//        .{ SingleThreadedRunner, .{}, BitSieve, .{.unrolled = true, .RunFactorChunk = u64, .FindFactorChunk = u32, .max_vector = 2}, false},
//        .{ SingleThreadedRunner, .{}, BitSieve, .{.unrolled = true, .RunFactorChunk = u64, .FindFactorChunk = u32, .max_vector = 2, .half_extent = true}, false},
//        .{ SingleThreadedRunner, .{}, BitSieve, .{.unrolled = true, .RunFactorChunk = u64, .FindFactorChunk = u32, .max_vector = 4}, false},
//        .{ SingleThreadedRunner, .{}, BitSieve, .{.unrolled = true, .RunFactorChunk = u64, .FindFactorChunk = u32, .max_vector = 4, .half_extent = true}, false},
//        .{ SingleThreadedRunner, .{}, BitSieve, .{.unrolled = true, .RunFactorChunk = u64, .FindFactorChunk = u32, .max_vector = 8}, false},
//        .{ SingleThreadedRunner, .{}, BitSieve, .{.unrolled = true, .RunFactorChunk = u64, .FindFactorChunk = u32, .max_vector = 8, .half_extent = true}, ARCH_64}, // sometimes this could be competitive
//        .{ SingleThreadedRunner, .{}, BitSieve, .{.unrolled = true, .RunFactorChunk = u32, .FindFactorChunk = u32, .max_vector = 4}, false},
//        .{ SingleThreadedRunner, .{}, BitSieve, .{.unrolled = true, .RunFactorChunk = u32, .FindFactorChunk = u32, .max_vector = 4, .half_extent = true}, false},
//        .{ SingleThreadedRunner, .{}, BitSieve, .{.unrolled = true, .RunFactorChunk = u32, .FindFactorChunk = u32, .max_vector = 8}, false},
//        .{ SingleThreadedRunner, .{}, BitSieve, .{.unrolled = true, .RunFactorChunk = u32, .FindFactorChunk = u32, .max_vector = 8, .half_extent = true}, ARCH_32},
//        .{ SingleThreadedRunner, .{}, BitSieve, .{.unrolled = true, .RunFactorChunk = u32, .FindFactorChunk = u32, .max_vector = 16}, false},
//        .{ SingleThreadedRunner, .{}, BitSieve, .{.unrolled = true, .RunFactorChunk = u32, .FindFactorChunk = u32, .max_vector = 16, .half_extent = true}, false},
//        .{ SingleThreadedRunner, .{}, BitSieve, .{.unrolled = true, .RunFactorChunk = u8, .FindFactorChunk = u32, .max_vector = 8}, false},
//        .{ SingleThreadedRunner, .{}, BitSieve, .{.unrolled = true, .RunFactorChunk = u8, .FindFactorChunk = u32, .max_vector = 8, .half_extent = true}, false},
//        .{ SingleThreadedRunner, .{}, BitSieve, .{.unrolled = true, .RunFactorChunk = u8, .FindFactorChunk = u32, .max_vector = 16}, false},
//        .{ SingleThreadedRunner, .{}, BitSieve, .{.unrolled = true, .RunFactorChunk = u8, .FindFactorChunk = u32, .max_vector = 16, .half_extent = true}, false},
//        .{ SingleThreadedRunner, .{}, BitSieve, .{.unrolled = true, .RunFactorChunk = u8, .FindFactorChunk = u32, .max_vector = 64}, false},
//        .{ SingleThreadedRunner, .{}, BitSieve, .{.unrolled = true, .RunFactorChunk = u8, .FindFactorChunk = u32, .max_vector = 64, .half_extent = true}, false},
//        // best singlethreaded wheel runner (can be environment dependent):
//        .{ SingleThreadedRunner, .{}, BitSieve, .{.unrolled = true, .RunFactorChunk = u32, .FindFactorChunk = u32, .wheel_primes = 5, .allocator = NonClearing}, ARCH_32},
//        .{ SingleThreadedRunner, .{}, BitSieve, .{.unrolled = true, .RunFactorChunk = u64, .FindFactorChunk = u32, .wheel_primes = 5, .allocator = NonClearing}, ARCH_64},
//        .{ SingleThreadedRunner, .{}, BitSieve, .{.unrolled = true, .RunFactorChunk = u32, .FindFactorChunk = u8, .wheel_primes = 5, .allocator = NonClearing, .PRIME = 1, .find_factor = .advanced}, ARCH_32 and NOT_ARM},
//        .{ SingleThreadedRunner, .{}, BitSieve, .{.unrolled = true, .RunFactorChunk = u64, .FindFactorChunk = u8, .wheel_primes = 5, .allocator = NonClearing, .PRIME = 1, .find_factor = .advanced}, ARCH_64 and NOT_ARM},
//        // pessimizations on singlethreadedeness (wheel)
//        .{ SingleThreadedRunner, .{}, BitSieve, .{.unrolled = true, .RunFactorChunk = u64, .FindFactorChunk = u32, .wheel_primes = 5, .note = "-calloc"}, false},  // using calloc matters
//        .{ SingleThreadedRunner, .{}, BitSieve, .{.PRIME = 1, .unrolled = true, .RunFactorChunk = u64, .FindFactorChunk = u8, .wheel_primes = 5, .allocator = NonClearing}, false}, // inverted
//        .{ SingleThreadedRunner, .{}, BitSieve, .{.unrolled = true, .RunFactorChunk = u64, .FindFactorChunk = u8, .find_factor = .advanced, .wheel_primes = 5, .allocator = NonClearing}, false},
//        // experimental vector sieve (typically less performant than unrolled bitsieve)
//        .{ SingleThreadedRunner, .{}, VecSieve, .{.PRIME = 1, .allocator = SAlloc(c_std_lib, .{})}, NOT_ARM},
//        // multi-threaded
//        .{ ParallelAmdahlRunner, .{}, IntSieve, .{}, NOT_ARM},
//        .{ ParallelGustafsonRunner, .{}, IntSieve, .{}, true},
//        .{ ParallelGustafsonRunner, .{}, IntSieve, .{.wheel_primes = 6}, true},
//        .{ ParallelGustafsonRunner, .{}, BitSieve, .{.unrolled = true, .RunFactorChunk = u32, .FindFactorChunk = u32}, ARCH_32 and NOT_ARM},
//        .{ ParallelGustafsonRunner, .{}, BitSieve, .{.unrolled = true, .RunFactorChunk = u64, .FindFactorChunk = u32}, ARCH_64 and NOT_ARM},
//        .{ ParallelGustafsonRunner, .{}, BitSieve, .{.unrolled = true, .RunFactorChunk = u32, .FindFactorChunk = u8, .wheel_primes = 5, .allocator = NonClearing, .PRIME = 1, .find_factor = .advanced}, ARCH_32},
//        .{ ParallelGustafsonRunner, .{}, BitSieve, .{.unrolled = true, .RunFactorChunk = u64, .FindFactorChunk = u8, .wheel_primes = 5, .allocator = NonClearing, .PRIME = 1, .find_factor = .advanced}, ARCH_64},
//        // multi-threaded regressions
//        .{ ParallelGustafsonRunner, .{.no_ht = true}, BitSieve, .{.unrolled = true, .RunFactorChunk = u32, .FindFactorChunk = u32}, false},
//        .{ ParallelGustafsonRunner, .{.no_ht = true}, BitSieve, .{.unrolled = true, .RunFactorChunk = u64, .FindFactorChunk = u32}, false},
//        // in testing we find that the best performing of the following four is architecture-dependent
//        .{ ParallelGustafsonRunner, .{}, BitSieve, .{.unrolled = true, .RunFactorChunk = u32, .FindFactorChunk = u32, .wheel_primes = 2, .allocator = NonClearing}, ARCH_32 and NOT_ARM},
//        .{ ParallelGustafsonRunner, .{}, BitSieve, .{.unrolled = true, .RunFactorChunk = u32, .FindFactorChunk = u32, .wheel_primes = 3, .allocator = NonClearing}, ARCH_32 and NOT_ARM},
//        .{ ParallelGustafsonRunner, .{}, BitSieve, .{.unrolled = true, .RunFactorChunk = u32, .FindFactorChunk = u32, .wheel_primes = 4, .allocator = NonClearing}, ARCH_32 and NOT_ARM},
//        .{ ParallelGustafsonRunner, .{}, BitSieve, .{.unrolled = true, .RunFactorChunk = u32, .FindFactorChunk = u32, .wheel_primes = 5, .allocator = NonClearing}, ARCH_32 and NOT_ARM},
//        .{ ParallelGustafsonRunner, .{}, BitSieve, .{.unrolled = true, .RunFactorChunk = u64, .FindFactorChunk = u32, .wheel_primes = 2, .allocator = NonClearing}, ARCH_64 and NOT_ARM},
//        .{ ParallelGustafsonRunner, .{}, BitSieve, .{.unrolled = true, .RunFactorChunk = u64, .FindFactorChunk = u32, .wheel_primes = 3, .allocator = NonClearing}, ARCH_64 and NOT_ARM},
//        .{ ParallelGustafsonRunner, .{}, BitSieve, .{.unrolled = true, .RunFactorChunk = u64, .FindFactorChunk = u32, .wheel_primes = 4, .allocator = NonClearing}, ARCH_64 and NOT_ARM},
//        .{ ParallelGustafsonRunner, .{}, BitSieve, .{.unrolled = true, .RunFactorChunk = u64, .FindFactorChunk = u32, .wheel_primes = 5, .allocator = NonClearing}, ARCH_64 and NOT_ARM},
    };

    inline for (specs) |spec| {
        comptime const RunnerFn = spec[0];
        comptime const runner_opts = spec[1];
        comptime const SieveFn = spec[2];
        comptime const sieve_opts = spec[3];

        comptime const Sieve = SieveFn(sieve_opts);
        comptime const Runner = RunnerFn(Sieve, runner_opts);
        comptime const should_run = spec[4];

        if (all or should_run) {
            try runSieveTest(Runner, run_for, SIZE);
        }
    }
}

fn runSieveTest(
    comptime Runner: type,
    run_for: comptime_int,
    sieve_size: usize,
) align(std.mem.page_size) anyerror!void {
    @setAlignStack(256);
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
        "ManDeJan&ityonemo&SpexGuy-zig-" ++ Runner.name,
        passes,
        elapsed,
        threads,
        Runner.algo,
        Runner.bits);
}

fn printResults(backing: []const u8, passes: usize, elapsed_ns: u64, threads: usize, algo: []const u8, bits: usize) !void {
    const elapsed = @intToFloat(f32, elapsed_ns) / @intToFloat(f32, std.time.ns_per_s);
    const stdout = std.io.getStdOut().writer();

    try stdout.print("{s};{};{d:.5};{};faithful=yes,algorithm={s},bits={}\n", .{ backing, passes, elapsed, threads, algo, bits });
}
