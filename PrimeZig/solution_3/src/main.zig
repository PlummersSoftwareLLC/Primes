const std = @import("std");
const heap = std.heap;
const time = std.time;
const sieves = @import("./sieves.zig");
const IntSieve = sieves.IntSieve;
const BitSieve = sieves.BitSieve;
const runners = @import("./runners.zig");
const SingleThreadedRunner = runners.SingleThreadedRunner;
const ParallelAmdahlRunner = runners.AmdahlRunner;
const ParallelGustafsonRunner = runners.GustafsonRunner;

const Allocator = @import("alloc.zig").EnvironmentallyFriendlyBlockAllocator;

const SIZE = 1_000_000;

var scratchpad: [SIZE]u8 align(std.mem.page_size) = undefined;

pub fn main() anyerror!void {
    // exists so that we don't run out of compiler credits.  Zig compiler is stingier than AWS.
    @setEvalBranchQuota(100000);

    const run_for = 5; // Seconds
    var single_threaded_allocator = Allocator(.{ .single_threaded = true }).init(std.heap.page_allocator);
    defer single_threaded_allocator.deinit();
    var multithreaded_allocator = Allocator(.{}).init(std.heap.page_allocator);
    defer multithreaded_allocator.deinit();

    // check for the --all flag.
    const all = (std.os.argv.len == 2) and (std.mem.eql(u8, std.mem.spanZ(std.os.argv[1]), "--all"));

    comptime const AllDataTypes = .{ bool, u1, u8, u16, u32, u64, usize };
    comptime const BitSieveDataTypes = .{ u8, u16, u32, u64 };

    comptime const specs = .{
        .{ SingleThreadedRunner, IntSieve, false, false },
        .{ ParallelAmdahlRunner, IntSieve, false, false },
        .{ ParallelAmdahlRunner, IntSieve, true, false },
        .{ ParallelGustafsonRunner, IntSieve, false, false },
        .{ ParallelGustafsonRunner, IntSieve, true, false },
        .{ SingleThreadedRunner, BitSieve, false, false },
        .{ ParallelGustafsonRunner, BitSieve, false, false },
        .{ ParallelGustafsonRunner, BitSieve, true, false },
        .{ SingleThreadedRunner, IntSieve, false, true },
        .{ SingleThreadedRunner, BitSieve, false, true },
        .{ ParallelGustafsonRunner, BitSieve, false, true },
        .{ ParallelGustafsonRunner, BitSieve, true, true },

    };

    // number of pregenerated primes in the wheel
    comptime const pregens = [_]comptime_int{ 2, 3, 4, 5 };

    inline for (specs) |spec| {
        comptime const RunnerFn = spec[0];
        comptime const SieveFn = spec[1];
        comptime const no_ht_opt = spec[2];
        comptime const wheel = spec[3];

        comptime const runner_opts = if (no_ht_opt) .{.no_ht = true} else .{};

        var allocator = if (RunnerFn == SingleThreadedRunner) &single_threaded_allocator.allocator else &multithreaded_allocator.allocator;

        if (wheel) {
            comptime const DataTypes = if (SieveFn == IntSieve) .{u8} else BitSieveDataTypes;

            inline for (DataTypes) |Type| {
                comptime const typebits = if (SieveFn == IntSieve) 8 * @sizeOf(Type) else 1;
                inline for (pregens) |pregen| {
                    comptime const Sieve = SieveFn(Type, .{ .pregen = pregen });
                    comptime const Runner = RunnerFn(Sieve, runner_opts);
                    comptime const selected = selected_runs(Runner, no_ht_opt);
                    if (all or selected) {
                        try runSieveTest(Runner, run_for, allocator, wheel, typebits, SIZE);
                    }
                }
            }
        } else {
            comptime const DataTypes = if (SieveFn == IntSieve) AllDataTypes else BitSieveDataTypes;

            inline for (DataTypes) |Type| {
                comptime const typebits = if (SieveFn == IntSieve) 8 * @sizeOf(Type) else 1;
                comptime const Sieve = SieveFn(Type, .{});
                comptime const Runner = RunnerFn(Sieve, runner_opts);
                comptime const selected = selected_runs(Runner, no_ht_opt);
                if (all or selected) {
                    try runSieveTest(Runner, run_for, allocator, wheel, typebits, SIZE);
                }
            }
        }
    }
}


const widthstring = switch (std.builtin.target.cpu.arch.ptrBitWidth()) {
    32 => "u32",
    64 => "u64",
    else => unreachable
};

fn selected_runs(comptime Runner: type, comptime no_ht_opt: bool) bool {
    const selections = .{
        "single-sieve-bool",
        "single-sieve-u8",
        "parallel-amdahl-sieve-u8",
        "parallel-gustafson-sieve-u8",
        "parallel-amdahl-sieve-u8",
        "parallel-gustafson-sieve-u8",
        "single-bitSieve-u8",
        "parallel-gustafson-bitSieve-u8",
        "parallel-gustafson-bitSieve-" ++ widthstring,
        "single-sieve-u8-480of2310",
        "single-sieve-u8-5760of30030",
        "single-bitSieve-u8-480of2310",
        "single-bitSieve-u8-5760of30030",
        "single-bitSieve-" ++ widthstring ++ "-480of2310",
        "single-bitSieve-" ++ widthstring ++ "-5760of30030",
        "parallel-gustafson-bitSieve-u8-480of2310",
        "parallel-gustafson-bitSieve-u8-5760of30030",
        "parallel-gustafson-bitSieve-" ++ widthstring ++ "-480of2310",
        "parallel-gustafson-bitSieve-" ++ widthstring ++ "-5760of30030",
    };
    if (no_ht_opt) return false;
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
    bits: usize,
    sieve_size: usize,
) anyerror!void {
    @setAlignStack(256);
    const timer = try time.Timer.start();
    var passes: u64 = 0;

    var runner = try Runner.init(allocator, sieve_size, &passes);
    defer runner.deinit();

    while (timer.read() < run_for * time.ns_per_s) {
        try runner.sieveInit();
        defer runner.sieveDeinit();

        runner.run();
    }

    const elapsed = timer.read();

    var threads = try Runner.threads();
    try printResults("ManDeJan&ityonemo-zig-" ++ Runner.name, passes, elapsed, threads, wheel, bits);
}

fn printResults(backing: []const u8, passes: usize, elapsed_ns: u64, threads: usize, wheel: bool, bits: usize) !void {
    const elapsed = @intToFloat(f32, elapsed_ns) / @intToFloat(f32, time.ns_per_s);
    const stdout = std.io.getStdOut().writer();
    const algo = if (wheel) "wheel" else "base";

    try stdout.print("{s};{};{d:.5};{};faithful=yes,algorithm={s},bits={}\n", .{ backing, passes, elapsed, threads, algo, bits });
}
