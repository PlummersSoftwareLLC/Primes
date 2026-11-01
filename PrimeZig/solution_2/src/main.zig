const std = @import("std");
const heap = std.heap;
const print = std.debug.print;
const time = std.time;
const prime = @import("./prime.zig");

pub fn main(init: std.process.Init) anyerror!void {
    const io = init.io;
    const size = 1_000_000;
    const run_for = 5; // Seconds

    const configs = .{
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

    try runSieveBitTest(io, size, run_for);
    inline for (configs) |run| {
        try runSieveTest(io, run[0], run[1], run[2], size, run_for);
    }
}

fn runSieveTest(
    io: std.Io,
    comptime Type: type,
    comptime trueVal: Type,
    comptime falseVal: Type,
    size: comptime_int,
    run_for: comptime_int,
) anyerror!void {
    const timer = std.Io.Clock.Timestamp.now(io, .awake);
    var passes: u64 = 0;
    while (timer.untilNow(io).raw.nanoseconds < run_for * time.ns_per_s) : (passes += 1) {
        const field = [_]Type{trueVal} ** (size >> 1);
        var sieve = prime.Sieve(Type, trueVal, falseVal, size).init(field);
        sieve.run();
    }
    const elapsed: u64 = @intCast(timer.untilNow(io).raw.nanoseconds);

    try printResults(io, "ManDeJan&ityonemo-zig-byte-sieve-type-" ++ @typeName(Type), passes, elapsed, 8 * @sizeOf(Type));
}

fn runNewSieveTest(
    io: std.Io,
    comptime Type: type,
    comptime trueVal: Type,
    comptime falseVal: Type,
    size: comptime_int,
    run_for: comptime_int,
) anyerror!void {
    const timer = std.Io.Clock.Timestamp.now(io, .awake);
    var passes: u64 = 0;
    while (timer.untilNow(io).raw.nanoseconds < run_for * time.ns_per_s) : (passes += 1) {
        const field = [_]Type{trueVal} ** (size >> 1);
        var sieve = prime.NewSieve(Type, trueVal, falseVal, size).init(field);
        sieve.run();
    }
    const elapsed: u64 = @intCast(timer.untilNow(io).raw.nanoseconds);

    try printResults(io, "ManDeJan&ityonemo-zig-newbyte-sieve-type-" ++ @typeName(Type), passes, elapsed, 8 * @sizeOf(Type));
}

fn runSieveBitTest(io: std.Io, comptime size: comptime_int, run_for: comptime_int) anyerror!void {
    // @compileLog(@typeInfo(std));
    const timer = std.Io.Clock.Timestamp.now(io, .awake);
    var passes: u64 = 0;
    while (timer.untilNow(io).raw.nanoseconds < run_for * time.ns_per_s) : (passes += 1) {
        var sieve = prime.BitSieve(size).init();
        sieve.run();
    }
    const elapsed: u64 = @intCast(timer.untilNow(io).raw.nanoseconds);
    try printResults(io, "ManDeJan&ityonemo-zig-bit-sieve", passes, elapsed, 1);
}

fn printResults(io: std.Io, backing: []const u8, passes: usize, elapsed_ns: u64, bit_size: usize) !void {
    const elapsed = @as(f32, @floatFromInt(elapsed_ns)) / @as(f32, @floatFromInt(time.ns_per_s));
    var buffer: [256]u8 = undefined;
    var stdout = std.Io.File.stdout().writerStreaming(io, &buffer);
    try stdout.interface.print("{s};{};{d:.5};1;faithful=no,algorithm=base,bits={}\n", .{
        backing, passes, elapsed, bit_size
    });
    try stdout.interface.flush();
}
