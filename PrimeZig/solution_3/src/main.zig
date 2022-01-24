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


const build_options = @import("build_options");
const SELECTED = build_options.selected;     // false if -Ddebug
const ONLY_WHEN_ALL = build_options.all;     // did we set -Dall?
const IS_RPI4 = std.builtin.target.cpu.arch.isARM() and build_options.arm_is_rpi4;
const U64_LANES = if (IS_RPI4) 2 else 8;  // how many U64-lanes are available for SIMD?
const U8_LANES = U64_LANES * 8;

pub fn main() anyerror!void {
    const run_for = 5; // Seconds

    const NonClearing = SAlloc(c_std_lib, .{.should_clear = false});

    comptime const specs = .{
        // single-threaded int runners

        .{ SingleThreadedRunner, .{}, IntSieve, .{}, SELECTED, @src().line},
        .{ SingleThreadedRunner, .{}, IntSieve, .{.wheel_primes = 6}, SELECTED, @src().line},
        // comparison against C runner.
        .{ SingleThreadedRunner, .{}, BitSieve, .{}, SELECTED, @src().line}, // equivalent to C
        // best singlethreaded base runner
        .{ SingleThreadedRunner, .{}, BitSieve, .{.unrolled = .{.max_vector = U64_LANES}}, SELECTED, @src().line},
        // LUTs matter
        .{ SingleThreadedRunner, .{}, BitSieve, .{.unrolled = .{.max_vector = U64_LANES, .use_dense_LUT = false}}, ONLY_WHEN_ALL, @src().line},
        .{ SingleThreadedRunner, .{}, BitSieve, .{.unrolled = .{.max_vector = U64_LANES, .use_sparse_LUT = false}}, ONLY_WHEN_ALL, @src().line},
        // RunFactorChunk matters
        .{ SingleThreadedRunner, .{}, BitSieve, .{.unrolled = .{.max_vector = 16}, .RunFactorChunk = u32}, ONLY_WHEN_ALL and ARCH_64, @src().line},
        .{ SingleThreadedRunner, .{}, BitSieve, .{.unrolled = .{.max_vector = 8}, .RunFactorChunk = u32}, ONLY_WHEN_ALL and ARCH_64, @src().line},
        .{ SingleThreadedRunner, .{}, BitSieve, .{.unrolled = .{.max_vector = 16}, .RunFactorChunk = u8}, ONLY_WHEN_ALL, @src().line},
        // FindFactorChunk matters
        .{ SingleThreadedRunner, .{}, BitSieve, .{.unrolled = .{.max_vector = 8}, .FindFactorChunk = u8}, ONLY_WHEN_ALL, @src().line},
        .{ SingleThreadedRunner, .{}, BitSieve, .{.unrolled = .{.max_vector = 8}, .FindFactorChunk = u64}, ONLY_WHEN_ALL, @src().line},
        // Allocator matters
        .{ SingleThreadedRunner, .{}, BitSieve, .{.unrolled = .{.max_vector = 8}, .allocator = SAlloc(c_std_lib, .{}), .note = "malloc"}, ONLY_WHEN_ALL, @src().line},
        .{ SingleThreadedRunner, .{}, BitSieve, .{.unrolled = .{.max_vector = 8}, .allocator = VAlloc(.{}), .note = "custom-allocator"}, ONLY_WHEN_ALL, @src().line},
        // Inverted is not generally better
        .{ SingleThreadedRunner, .{}, BitSieve, .{.unrolled = .{.max_vector = 8}, .allocator = SAlloc(c_std_lib, .{}), .PRIME = 1}, ONLY_WHEN_ALL, @src().line},
        // "Advanced FindFactor doesn't help"
        .{ SingleThreadedRunner, .{}, BitSieve, .{.unrolled = .{.max_vector = 8}, .find_factor = .advanced}, ONLY_WHEN_ALL, @src().line},
        .{ SingleThreadedRunner, .{}, BitSieve, .{.unrolled = .{.max_vector = 8}, .find_factor = .advanced, .FindFactorChunk = u32}, ONLY_WHEN_ALL and ARCH_64, @src().line},
        .{ SingleThreadedRunner, .{}, BitSieve, .{.unrolled = .{.max_vector = 8}, .find_factor = .advanced, .FindFactorChunk = u8}, ONLY_WHEN_ALL, @src().line},
        // vectorization
        .{ SingleThreadedRunner, .{}, BitSieve, .{.unrolled = .{.max_vector = U64_LANES, .half_extent = false}}, ONLY_WHEN_ALL, @src().line},
        .{ SingleThreadedRunner, .{}, BitSieve, .{.unrolled = .{.max_vector = 4}}, ONLY_WHEN_ALL and !IS_RPI4, @src().line},
        .{ SingleThreadedRunner, .{}, BitSieve, .{.unrolled = .{.max_vector = 2}}, ONLY_WHEN_ALL and !IS_RPI4, @src().line},
        .{ SingleThreadedRunner, .{}, BitSieve, .{.unrolled = .{.max_vector = 1}}, ONLY_WHEN_ALL, @src().line},
        // best wheel runner
        .{ SingleThreadedRunner, .{}, BitSieve, .{.unrolled = .{.max_vector = 8}, .FindFactorChunk = u8, .wheel_primes = 5, .allocator = NonClearing, .PRIME = 1, .find_factor = .advanced, .wheel_copy_vector = U8_LANES}, SELECTED, @src().line},
        // ----   pessimizations on singlethreaded (wheel)
        .{ SingleThreadedRunner, .{}, BitSieve, .{.unrolled = .{.max_vector = 8}, .FindFactorChunk = u8, .wheel_primes = 5, .allocator = NonClearing, .PRIME = 1, .find_factor = .advanced}, ONLY_WHEN_ALL, @src().line},
        .{ SingleThreadedRunner, .{}, BitSieve, .{.unrolled = .{.max_vector = 8}, .wheel_primes = 5, .allocator = NonClearing}, ONLY_WHEN_ALL, @src().line},
        .{ SingleThreadedRunner, .{}, BitSieve, .{.unrolled = .{.max_vector = 8}, .wheel_primes = 4, .allocator = NonClearing}, ONLY_WHEN_ALL, @src().line},
        // experimental vector sieve (typically less performant than unrolled bitsieve)
        .{ SingleThreadedRunner, .{}, VecSieve, .{.PRIME = 1, .allocator = SAlloc(c_std_lib, .{})}, SELECTED and !IS_RPI4, @src().line},
        // multi-threaded, amdahl
        .{ ParallelAmdahlRunner, .{}, IntSieve, .{}, SELECTED and !IS_RPI4, @src().line},
        // multi-threadad, gustafson
        .{ ParallelGustafsonRunner, .{}, IntSieve, .{}, SELECTED, @src().line},
        .{ ParallelGustafsonRunner, .{}, IntSieve, .{.wheel_primes = 6}, SELECTED, @src().line},
        .{ ParallelGustafsonRunner, .{.no_ht = true}, IntSieve, .{.wheel_primes = 6, .note = "-no-ht"}, SELECTED and !IS_RPI4, @src().line},
        .{ ParallelGustafsonRunner, .{}, BitSieve, .{.unrolled = .{.max_vector = U64_LANES}}, SELECTED, @src().line},
        .{ ParallelGustafsonRunner, .{.no_ht = true}, BitSieve, .{.unrolled = .{.max_vector = 8}, .note = "-no-ht"}, SELECTED and !IS_RPI4, @src().line},
        .{ ParallelGustafsonRunner, .{}, BitSieve, .{.unrolled = .{.max_vector = U64_LANES}, .FindFactorChunk = u8, .wheel_primes = 4, .allocator = NonClearing, .wheel_copy_vector = U8_LANES, .PRIME = 1, .find_factor = .advanced}, SELECTED, @src().line},
        .{ ParallelGustafsonRunner, .{.no_ht = true}, BitSieve, .{.unrolled = .{.max_vector = U64_LANES}, .FindFactorChunk = u8, .wheel_primes = 4, .allocator = NonClearing, .wheel_copy_vector = U8_LANES, .PRIME = 1, .find_factor = .advanced, .note = "-no-ht"}, SELECTED and !IS_RPI4, @src().line},
        .{ ParallelGustafsonRunner, .{}, BitSieve, .{.unrolled = .{.max_vector = U64_LANES}, .FindFactorChunk = u8, .wheel_primes = 5, .allocator = NonClearing, .wheel_copy_vector = U8_LANES, .PRIME = 1, .find_factor = .advanced}, SELECTED, @src().line},
        .{ ParallelGustafsonRunner, .{.no_ht = true}, BitSieve, .{.unrolled = .{.max_vector = U64_LANES}, .FindFactorChunk = u8, .wheel_primes = 5, .allocator = NonClearing, .wheel_copy_vector = U8_LANES, .PRIME = 1, .find_factor = .advanced, .note = "-no-ht"}, SELECTED and !IS_RPI4, @src().line},
    };

    // serially run tests seem to be subjected to heavy throttling which bleeds over between
    // runs, at least on linux.  It seems the only way to prevent this is by running a
    const args = try std.process.argsAlloc(std.heap.page_allocator);
    const selected_run: ?usize = if ((args.len == 3) and (std.mem.eql(u8, args[1], "-l"))) (std.fmt.parseInt(usize, args[2], 10) catch @panic("badarg")) else null;

    inline for (specs) |spec| {
        comptime const RunnerFn = spec[0];
        comptime const runner_opts = spec[1];
        comptime const SieveFn = spec[2];
        comptime const sieve_opts = spec[3];
        comptime const Sieve = SieveFn(sieve_opts);
        comptime const Runner = RunnerFn(Sieve, runner_opts);
        comptime const should_run = spec[4];
        comptime const line = spec[5];

        if (should_run) {
            const selected = if (selected_run) | s_line | line == s_line else true;
            if (selected) {
                try runSieveTest(Runner, run_for, SIZE, line);
            }
        }
    }
}

fn runSieveTest(
    comptime Runner: type,
    run_for: comptime_int,
    sieve_size: usize,
    line: usize,
) anyerror!void {
    @setAlignStack(256);

    try printName(Runner, line);

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

fn printName(comptime Runner: type, line: usize) !void {
    const stdout = std.io.getStdOut().writer();
    try stdout.print("{}-ManDeJan&ityonemo&SpexGuy-zig-{s}", .{line, Runner.name});
}

fn printResults(passes: usize, elapsed_ns: u64, threads: usize, algo: []const u8, bits: usize) !void {
    const elapsed = @intToFloat(f32, elapsed_ns) / @intToFloat(f32, std.time.ns_per_s);
    const stdout = std.io.getStdOut().writer();

    try stdout.print(";{};{d:.5};{};faithful=yes,algorithm={s},bits={}\n", .{ passes, elapsed, threads, algo, bits });
}

// If you're going to use tracy, you need these callback hooks.
// note: stupid C++ mangling shenanigans
extern fn  @"__cxa_thread_atexit_impl@GLIBC_2.18"(fun: fn () callconv(.C) void, obj: *c_void, dso_symbol: *c_void) c_int;
export fn  __cxa_thread_atexit_impl(fun: fn () callconv(.C) void, obj: *c_void, dso_symbol: *c_void) c_int {
    return @"__cxa_thread_atexit_impl@GLIBC_2.18"(fun, obj, dso_symbol);
}
