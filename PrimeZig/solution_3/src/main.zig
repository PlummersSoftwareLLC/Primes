const std = @import("std");
const heap = std.heap;
const print = std.debug.print;
const time = std.time;
const sieves = @import("./sieves.zig");
const IntSieve = sieves.IntSieve;
const BitSieve = sieves.BitSieve;
const runners = @import("./runners.zig");
const SingleThreadedRunner = runners.SingleThreadedRunner;
const ParallelAmdahlRunner = runners.AmdahlRunner;
const ParallelGustafsonRunner = runners.GustafsonRunner;

const Allocator = @import("alloc.zig").SnappyAllocator;

const SIZE = 1_000_000;

var scratchpad: [SIZE]u8 align(std.mem.page_size) = undefined;

pub fn main() anyerror!void {
    @setEvalBranchQuota(100000);

    const run_for = 5; // Seconds
    var allocator = &Allocator(SIZE).init(std.heap.page_allocator, &scratchpad).allocator;

    // check for the --all flag.
    const all = (std.os.argv.len == 2) and (std.mem.eql(u8, std.mem.spanZ(std.os.argv[1]), "--all"));

    comptime const AllDataTypes = .{ bool, u1, u8, u16, u32, u64, usize };
    comptime const BitSieveDataTypes = .{ u8, u16, u32, u64 };

    comptime const specs = .{
        .{ SingleThreadedRunner, IntSieve, .{}, false },
        .{ ParallelAmdahlRunner, IntSieve, .{}, false },
        .{ ParallelGustafsonRunner, IntSieve, .{}, false },
        .{ ParallelAmdahlRunner, IntSieve, .{ .no_ht = true }, false },
        .{ ParallelGustafsonRunner, IntSieve, .{ .no_ht = true }, false },
        .{ SingleThreadedRunner, BitSieve, .{}, false },
        .{ ParallelGustafsonRunner, BitSieve, .{}, false },
        .{ ParallelGustafsonRunner, BitSieve, .{ .no_ht = true }, false },
        .{ SingleThreadedRunner, IntSieve, .{}, true },
        .{ SingleThreadedRunner, BitSieve, .{}, true },
        .{ ParallelGustafsonRunner, BitSieve, .{}, true },
        .{ ParallelGustafsonRunner, BitSieve, .{ .no_ht = true }, true },
    };

    // number of pregenerated primes in the wheel
    comptime const pregens = [_]comptime_int{ 2, 3, 4, 5 };

    inline for (specs) |spec| {
        comptime const RunnerFn = spec[0];
        comptime const SieveFn = spec[1];
        comptime const runner_opts = spec[2];
        comptime const wheel = spec[3];

        if (wheel) {
            comptime const DataTypes = if (SieveFn == IntSieve) .{u8} else BitSieveDataTypes;

            inline for (DataTypes) |Type| {
                comptime const typebits = if (SieveFn == IntSieve) @bitSizeOf(Type) else 1;
                inline for (pregens) |pregen| {
                    comptime const Sieve = SieveFn(Type, SIZE, .{ .pregen = pregen });
                    comptime const Runner = RunnerFn(Sieve, runner_opts);
                    comptime const selected = selected_runs(Runner);
                    if (all or selected) {
                        try runSieveTest(Runner, run_for, allocator, wheel, typebits);
                    }
                }
            }
        } else {
            comptime const DataTypes = if (SieveFn == IntSieve) AllDataTypes else BitSieveDataTypes;

            inline for (DataTypes) |Type| {
                comptime const typebits = if (SieveFn == IntSieve) @bitSizeOf(Type) else 1;
                comptime const Sieve = SieveFn(Type, SIZE, .{});
                comptime const Runner = RunnerFn(Sieve, runner_opts);
                comptime const selected = selected_runs(Runner);
                if (all or selected) {
                    try runSieveTest(Runner, run_for, allocator, wheel, typebits);
                }
            }
        }
    }
}

fn selected_runs(comptime Runner: type) bool {
    const selections = .{
        "single-sieve-bool",
        "single-sieve-u8",
        "parallel-amdahl-sieve-u8",
        "parallel-gustafson-sieve-u8",
        "parallel-amdahl-sieve-u8",
        "parallel-gustafson-sieve-u8",
        "single-bitSieve-u32",
        "single-bitSieve-u64",
        "parallel-gustafson-bitSieve-u32",
        "parallel-gustafson-bitSieve-u64",
        "single-sieve-u8-480of2310",
        "single-sieve-u8-5760of30030",
        "single-bitSieve-u8-480of2310",
        "single-bitSieve-u8-5760of30030",
        "single-bitSieve-u32-480of2310",
        "single-bitSieve-u32-5760of30030",
        "single-bitSieve-u64-480of2310",
        "single-bitSieve-u64-5760of30030",
        "parallel-gustafson-bitSieve-u8-480of2310",
        "parallel-gustafson-bitSieve-u8-5760of30030",
        "parallel-gustafson-bitSieve-u32-480of2310",
        "parallel-gustafson-bitSieve-u32-5760of30030",
        "parallel-gustafson-bitSieve-u64-480of2310",
        "parallel-gustafson-bitSieve-u64-5760of30030",
    };
    for (selections) |selection| {
        if (std.mem.eql(u8, selection, Runner.name)) return true;
    }
    return false;
}

fn runSieveTest(
    comptime Runner: type,
    run_for: comptime_int,
    allocator: *std.mem.Allocator,
    wheel: bool,
    bits: usize
) anyerror!void {
    const timer = try time.Timer.start();
    var passes: u64 = 0;
    var runner = Runner{};

    try runner.init(allocator);
    defer runner.deinit();

    while (timer.read() < run_for * time.ns_per_s) : (passes += 1) {
        runner.reset();
        runner.run(&passes);
    }

    const elapsed = timer.read();

    var threads = try Runner.threads();
    try printResults("ManDeJan&ityonemo-zig-" ++ Runner.name, passes, elapsed, threads, wheel, bits);
}

fn printResults(backing: []const u8, passes: usize, elapsed_ns: u64, threads: usize, wheel: bool, bits: usize) !void {
    const elapsed = @intToFloat(f32, elapsed_ns) / @intToFloat(f32, time.ns_per_s);
    const stdout = std.io.getStdOut().writer();
    const algo = if (wheel) "wheel" else "base";

    try stdout.print("{s};{};{d:.5};{};faithful=yes,algorithm={s},bits={}\n", .{
        backing, passes, elapsed, threads, algo, bits
    });
}
