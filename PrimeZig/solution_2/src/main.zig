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
        // .{ u1, 0, 1 },
        .{ u8, 0, 1 },
        // .{ usize, 0, 1 },
        .{ bool, true, false },
        // .{ u1, 1, 0 },
        .{ u8, 1, 0 },
        // .{ usize, 1, 0 },
        // .{ u16, 0, 1 },
        // .{ u32, 0, 1 },
        // .{ u64, 0, 1 },
    };

    try runSieveBitTest(size, run_for);
    try runItyonemoTest(size, run_for);
    inline for (configs) |run| {
        try runSieveTest(run[0], run[1], run[2], size, run_for);
    }
}

fn runItyonemoTest(
    comptime size: comptime_int,
    run_for: comptime_int,
) anyerror!void {
    var allocator = std.heap.page_allocator;

    const timer = try time.Timer.start();
    var passes: u64 = 0;

    var slice = try allocator.alloc(bool, size >> 1);
    defer allocator.free(slice);

    while (timer.read() < run_for * time.ns_per_s) : (passes += 1) {
        var sieve = prime.ItyonemoSieve.init(slice, size);
        sieve.run();
    }
    const elapsed = timer.read();

    printResults("Ityonemo Sieve: ", passes, elapsed, size);
}


fn runSieveTest(
    comptime Type: type,
    comptime trueVal: Type,
    comptime falseVal: Type,
    size: comptime_int,
    run_for: comptime_int,
) anyerror!void {
    const timer = try time.Timer.start();
    var passes: u64 = 0;
    while (timer.read() < run_for * time.ns_per_s) : (passes += 1) {
        const field = [_]Type{trueVal} ** (size >> 1);
        prime.Sieve(Type, trueVal, falseVal, size).init(field).run();
    }
    const elapsed = timer.read();

    printResults("Regular Sieve Type: " ++ @typeName(Type), passes, elapsed, size);
}

fn runSieveBitTest(comptime size: comptime_int, run_for: comptime_int) anyerror!void {
    // @compileLog(@typeInfo(std));
    const timer = try time.Timer.start();
    var passes: u64 = 0;
    while (timer.read() < run_for * time.ns_per_s) : (passes += 1) {
        prime.BitSieve(size).init().run();
    }
    const elapsed = timer.read();
    printResults("Bit set", passes, elapsed, size);
}

fn printResults(backing: []const u8, passes: usize, elapsed_ns: u64, limit: usize) void {
    const elapsed = @intToFloat(f32, elapsed_ns) / @intToFloat(f32, time.ns_per_s);
    print("{s:<26}, Passes: {:5}, Time: {d:.5}, Avg: {d:.5}, Limit: {}\n", .{
        backing, passes, elapsed, elapsed / @intToFloat(f32, passes), limit,
    });
}
