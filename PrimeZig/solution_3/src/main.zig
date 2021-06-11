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
    const run_for = 5; // Seconds
    var allocator = &Allocator(SIZE).init(std.heap.page_allocator, &scratchpad).allocator;

    comptime const AllDataTypes = .{ bool, u1, u8, u16, u32, u64, usize };
    comptime const BitSieveDataTypes = .{u8, u16, u32, u64};

    comptime const specs = .{
        .{ SingleThreadedRunner, IntSieve, .{}, false},
        .{ ParallelAmdahlRunner, IntSieve, .{}, false},
        .{ ParallelGustafsonRunner, IntSieve, .{}, false},
        .{ ParallelAmdahlRunner, IntSieve, .{ .no_ht = true }, false},
        .{ ParallelGustafsonRunner, IntSieve, .{ .no_ht = true }, false},
        .{ SingleThreadedRunner, BitSieve, .{}, false},
        .{ ParallelGustafsonRunner, BitSieve, .{}, false},
        .{ ParallelGustafsonRunner, BitSieve, .{ .no_ht = true }, false},
        .{ SingleThreadedRunner, IntSieve, .{}, true},
        .{ SingleThreadedRunner, BitSieve, .{}, true},
        .{ ParallelGustafsonRunner, BitSieve, .{}, true},
        .{ ParallelGustafsonRunner, BitSieve, .{ .no_ht = true }, true},
    };

    comptime const pregens = [_]comptime_int{2, 3, 4, 5};

    inline for (specs) |spec| {
        comptime const RunnerFn = spec[0];
        comptime const SieveFn = spec[1];
        comptime const runner_opts = spec[2];
        comptime const use_pregen = spec[3];

        if (use_pregen) {
            comptime const DataTypes = if (SieveFn == IntSieve) .{u8} else BitSieveDataTypes;

            inline for (DataTypes) |Type| {
                inline for (pregens) | pregen | {
                    comptime const Sieve = SieveFn(Type, SIZE, .{.pregen = pregen});
                    comptime const Runner = RunnerFn(Sieve, runner_opts);
                    try runSieveTest(Runner, Runner.name, SIZE, run_for, allocator);
                }
            }
        } else {
            comptime const DataTypes = if (SieveFn == IntSieve) AllDataTypes else BitSieveDataTypes;

            inline for (DataTypes) |Type| {
                comptime const Sieve = SieveFn(Type, SIZE, .{});
                comptime const Runner = RunnerFn(Sieve, runner_opts);
                try runSieveTest(Runner, Runner.name, SIZE, run_for, allocator);
            }
        }
    }
}

fn runSieveTest(
    comptime Runner: type,
    comptime name: anytype,
    size: comptime_int,
    run_for: comptime_int,
    allocator: *std.mem.Allocator,
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

    try printResults("ManDeJan&ityonemo-zig-" ++ name, passes, elapsed, size);
}

fn printResults(backing: []const u8, passes: usize, elapsed_ns: u64, limit: usize) !void {
    const elapsed = @intToFloat(f32, elapsed_ns) / @intToFloat(f32, time.ns_per_s);
    const stdout = std.io.getStdOut().writer();
    try stdout.print("{s};{};{d:.5};1\n", .{
        backing, passes, elapsed,
    });
}
