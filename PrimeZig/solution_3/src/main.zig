const std = @import("std");
const heap = std.heap;
const print = std.debug.print;
const time = std.time;
const Sieve = @import("./sieve.zig").Sieve;
const SingleThreadedRunner = @import("./sieve.zig").SingleThreadedRunner;
const ParallelAmdahlRunner = @import("./parallel.zig").AmdahlRunner;
const Allocator = @import("alloc.zig").SnappyAllocator;

const SIZE = 1_000_000;

var scratchpad: [SIZE]u8 align(std.mem.page_size) = undefined;

pub fn main() anyerror!void {
    const run_for = 5; // Seconds
    var allocator = &Allocator(SIZE).init(std.heap.page_allocator, &scratchpad).allocator;

    comptime const DataTypes = .{bool, u1, u8, u16, u32, u64, usize};
    comptime const Runners = .{SingleThreadedRunner, ParallelAmdahlRunner};
    comptime const names = .{"single", "amdahl"};

    inline for (Runners) | Runner, runner_index | {
        inline for (DataTypes) |Type| {
            comptime const SieveType = Sieve(Type, SIZE);
            comptime const name = names[runner_index] ++ "-" ++ @typeName(Type);
            try runSieveTest(Runner(SieveType), name, SIZE, run_for, allocator);
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
