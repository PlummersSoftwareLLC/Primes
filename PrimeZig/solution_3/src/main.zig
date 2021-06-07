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
        .{ SingleThreadedRunner, IntSieve, .{}, .{}},
        .{ ParallelAmdahlRunner, IntSieve, .{}, .{}},
        .{ ParallelGustafsonRunner, IntSieve, .{}, .{}},
        .{ ParallelAmdahlRunner, IntSieve, .{ .no_ht = true }, .{}},
        .{ ParallelGustafsonRunner, IntSieve, .{ .no_ht = true }, .{}},
        .{ SingleThreadedRunner, BitSieve, .{}, .{}},
        .{ ParallelGustafsonRunner, BitSieve, .{}, .{}},
        .{ ParallelGustafsonRunner, BitSieve, .{ .no_ht = true }, .{}},
    };

    inline for (specs) |spec| {
        comptime const RunnerFn = spec[0];
        comptime const SieveFn = spec[1];
        comptime const runner_opts = spec[2];
        comptime const sieve_opts = spec[3];

        comptime const DataTypes = switch (SieveFn) {
            IntSieve => AllDataTypes,
            BitSieve => BitSieveDataTypes,
            else => unreachable,
        };

        inline for (DataTypes) |Type| {
            comptime const Sieve = SieveFn(Type, SIZE, sieve_opts);
            comptime const Runner = RunnerFn(Sieve, runner_opts);
            try runSieveTest(Runner, Runner.name, SIZE, run_for, allocator);
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
