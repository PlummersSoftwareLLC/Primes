const std = @import("std");
const heap = std.heap;
const print = std.debug.print;
const time = std.time;
const prime = @import("./prime.zig");

pub fn main() anyerror!void {
    const size = 1_000_000;
    const run_for = 5; // Seconds

    comptime const configs = .{
        .{ bool, false, true },
        // .{ u8, 0, 1 },
        // .{ u1, 0, 1 },
        // .{ usize, 0, 1 },
        // .{ bool, true, false },
        // .{ u1, 1, 0 },
        // .{ u8, 1, 0 },
        // .{ usize, 1, 0 },
        // .{ u16, 0, 1 },
        // .{ u32, 0, 1 },
        // .{ u64, 0, 1 },
    };

    inline for (configs) |run| {
        try runSieveTest(run[0], run[1], run[2], size, run_for);
    }

    inline for (configs) |run| {
        try runAmdahlTest(run[0], run[1], run[2], size, run_for);
    }

    inline for (configs) |run| {
        try runGustafsonTest(run[0], run[1], run[2], size, run_for);
    }
}

fn runSieveTest(
    comptime Type: type,
    comptime true_val: Type,
    comptime false_val: Type,
    size: comptime_int,
    run_for: comptime_int,
) anyerror!void {
    const timer = try time.Timer.start();
    var passes: u64 = 0;
    while (timer.read() < run_for * time.ns_per_s) : (passes += 1) {
        const field = [_]Type{true_val} ** (size >> 1);
        prime.Sieve(Type, true_val, false_val, size).init(field).run();
    }
    const elapsed = timer.read();

    try printResults("ManDeJan&ityonemo-zig-byte-sieve-type-" ++ @typeName(Type), passes, elapsed, size);
}

fn runAmdahlTest(
    comptime Type: type,
    comptime true_val: Type,
    comptime false_val: Type,
    size: comptime_int,
    run_for: comptime_int,
) anyerror!void {
    const timer = try time.Timer.start();
    var passes: u64 = 0;

    var sieve = prime.ParallelAmdahlSieve(Type, true_val, false_val, size){};
    try sieve.parallelInit();

    while (timer.read() < run_for * time.ns_per_s) : (passes += 1) {
        const field = [_]Type{true_val} ** (size >> 1);
        sieve.init(field).mainLoop();
    }

    sieve.parallelCleanup();

    const elapsed = timer.read();
    try printResults("ManDeJan&ityonemo-zig-amdahl-parallel-sieve", passes, elapsed, size);
}

fn runGustafsonTest(
    comptime Type: type,
    comptime true_val: Type,
    comptime false_val: Type,
    size: comptime_int,
    run_for: comptime_int,
) anyerror!void {
    var passes: u64 = 0;
    var finished: bool = false;

    try prime.ParallelGustafsonSieve(Type, true_val, false_val, size, run_for).parallelInit(&passes, &finished);
    var elapsed = try prime.ParallelGustafsonSieve(Type, true_val, false_val, size, run_for).mainRun(&passes, &finished);

    try printResults("ManDeJan&ityonemo-zig-gustafson-parallel-sieve", passes, elapsed, size);
}

fn printResults(backing: []const u8, passes: usize, elapsed_ns: u64, limit: usize) !void {
    const elapsed = @intToFloat(f32, elapsed_ns) / @intToFloat(f32, time.ns_per_s);
    const stdout = std.io.getStdOut().writer();
    try stdout.print("{s};{};{d:.5};1\n", .{
        backing, passes, elapsed,
    });
}
