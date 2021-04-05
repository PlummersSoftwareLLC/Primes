const std = @import("std");
const heap = std.heap;
const print = std.debug.print;
const time = std.time;
const prime = @import("./prime.zig");

pub fn main() anyerror!void {
    const size: u64 = 1_000_000;

    try runArrayListBacked(u8, 0, 1, size);
    try runArrayListBacked(bool, true, false, size);
    try runArrayListBacked(u1, 0, 1, size);
    try runArrayListBacked(u2, 0, 1, size);
    try runArrayListBacked(u4, 0, 1, size);
    try runArrayListBacked(u32, 0, 1, size);
    try runArrayListBacked(u64, 0, 1, size);

    try runFixedArrayListBacked(u8, 0, 1, size);
    try runFixedArrayListBacked(bool, true, false, size);
    try runFixedArrayListBacked(u1, 0, 1, size);
    try runFixedArrayListBacked(u2, 0, 1, size);
    try runFixedArrayListBacked(u4, 0, 1, size);
    try runFixedArrayListBacked(u32, 0, 1, size);
    try runFixedArrayListBacked(u64, 0, 1, size);
}

fn runArrayListBacked(
    comptime Type: type,
    comptime trueVal: Type,
    comptime falseVal: Type,
    size: u64,
) anyerror!void {
    const timer = try time.Timer.start();

    var allocator = heap.ArenaAllocator.init(heap.page_allocator);
    defer allocator.deinit();

    var passes: u64 = 0;
    while (timer.read() < 5 * time.ns_per_s) : (passes += 1) {
        var field = try prime.ArrayListField(Type, trueVal).init(&allocator.allocator, size);
        prime.Sieve(Type, trueVal, falseVal).init(field.slice(), size).run();
        field.deinit();
    }

    printResults("ArrayList(" ++ @typeName(Type) ++ ")", passes, timer.read(), size);
}

fn runFixedArrayListBacked(
    comptime Type: type,
    comptime trueVal: Type,
    comptime falseVal: Type,
    comptime size: u64,
) anyerror!void {
    const timer = try time.Timer.start();

    var buffer = [_]u8{0} ** (size * @sizeOf(Type));
    var allocator = heap.FixedBufferAllocator.init(buffer[0..]);

    var passes: u64 = 0;
    while (timer.read() < 5 * time.ns_per_s) : (passes += 1) {
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
