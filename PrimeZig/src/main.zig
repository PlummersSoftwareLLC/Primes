const std = @import("std");
const heap = std.heap;
const print = std.debug.print;
const time = std.time;
const prime = @import("./prime.zig");

pub fn main() anyerror!void {
    const size: u64 = 1_000_000;
    const run_for: u64 = 5; // Seconds

    comptime const configs = .{
        .{ u8, 0, 1 },
        .{ bool, true, false },
        .{ u1, 0, 1 },
        .{ u2, 0, 1 },
        .{ u4, 0, 1 },
        .{ u32, 0, 1 },
        .{ u64, 0, 1 },
    };

    inline for (configs) |run| {
        // Runs with ArenaAllocator that is backed by a page allocator.
        try runArenaArrayListBacked(run[0], run[1], run[2], size, run_for);

        // Runs on a FixedAllocator, that is backed by a buffer in stack.
        try runFixedArrayListBacked(run[0], run[1], run[2], size, run_for);

        // Runs backed by page alocator (pages allocated from OS directly).
        try runArrayListBacked(run[0], run[1], run[2], size, run_for);
    }
}

fn runArrayListBacked(
    comptime Type: type,
    comptime trueVal: Type,
    comptime falseVal: Type,
    size: u64,
    run_for: u64,
) anyerror!void {
    const timer = try time.Timer.start();

    var passes: u64 = 0;
    while (timer.read() < run_for * time.ns_per_s) : (passes += 1) {
        var field = try prime.ArrayListField(Type, trueVal).init(heap.page_allocator, size);
        prime.Sieve(Type, trueVal, falseVal).init(field.slice(), size).run();
        field.deinit();
    }

    printResults("PageAllocator+ArrayList(" ++ @typeName(Type) ++ ")", passes, timer.read(), size);
}

fn runArenaArrayListBacked(
    comptime Type: type,
    comptime trueVal: Type,
    comptime falseVal: Type,
    size: u64,
    run_for: u64,
) anyerror!void {
    const timer = try time.Timer.start();

    var allocator = heap.ArenaAllocator.init(heap.page_allocator);
    defer allocator.deinit();

    var passes: u64 = 0;
    while (timer.read() < run_for * time.ns_per_s) : (passes += 1) {
        var field = try prime.ArrayListField(Type, trueVal).init(&allocator.allocator, size);
        prime.Sieve(Type, trueVal, falseVal).init(field.slice(), size).run();
        field.deinit();
    }

    printResults("ArenaAllocator+ArrayList(" ++ @typeName(Type) ++ ")", passes, timer.read(), size);
}

fn runFixedArrayListBacked(
    comptime Type: type,
    comptime trueVal: Type,
    comptime falseVal: Type,
    comptime size: u64,
    run_for: u64,
) anyerror!void {
    const timer = try time.Timer.start();

    var buffer = [_]u8{0} ** (size * @sizeOf(Type));
    var allocator = heap.FixedBufferAllocator.init(buffer[0..]);

    var passes: u64 = 0;
    while (timer.read() < run_for * time.ns_per_s) : (passes += 1) {
        var field = try prime.ArrayListField(Type, trueVal).init(&allocator.allocator, size);
        prime.Sieve(Type, trueVal, falseVal).init(field.slice(), size).run();
        field.deinit();
    }

    printResults("FixedAllocator+ArrayList(" ++ @typeName(Type) ++ ")", passes, timer.read(), size);
}

fn printResults(backing: []const u8, passes: usize, elapsed_ns: u64, limit: usize) void {
    const elapsed = @intToFloat(f32, elapsed_ns) / @intToFloat(f32, time.ns_per_s);
    print("Backing: {}, Passes: {}, Time: {d:.5}, Avg: {d:.5}, Limit: {}\n", .{
        backing, passes, elapsed, elapsed / @intToFloat(f32, passes), limit,
    });
}
