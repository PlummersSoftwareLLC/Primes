const std = @import("std");
const heap = std.heap;
const print = std.debug.print;
const time = std.time;
const prime = @import("./prime.zig");

pub fn main() anyerror!void {
    const timer = try time.Timer.start();
    const size: u64 = 1_000_000;

    var allocator = heap.ArenaAllocator.init(heap.page_allocator);
    defer allocator.deinit();

    var passes: u64 = 0;
    while (timer.read() < 5 * time.ns_per_s) : (passes += 1) {
        var field = try prime.ArrayListField(u8, 0).init(&allocator.allocator, size);
        prime.Sieve(u8, 0, 1).init(field.slice(), size).run();
        field.deinit();
    }

    const elapsed = @intToFloat(f32, timer.read()) / @intToFloat(f32, time.ns_per_s);
    print("Passes: {}, Time: {d:.5}, Avg: {d:.5}, Limit: {}\n", .{
        passes,
        elapsed,
        elapsed / @intToFloat(f32, passes),
        size,
    });
}
