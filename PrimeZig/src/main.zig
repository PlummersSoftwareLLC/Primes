const std = @import("std");
const heap = std.heap;
const print = std.debug.print;
const Timer = std.time.Timer;
const Sieve = @import("./prime.zig").Sieve;

pub fn main() anyerror!void {
    const timer = try Timer.start();
    const size: u64 = 1_000_000;

    var allocator = heap.ArenaAllocator.init(heap.page_allocator);
    defer allocator.deinit();

    var passes: u64 = 0;
    while (timer.read() < 5 * 1_000_000_000) : (passes += 1) {
        var sieve = try Sieve(u64).init(&allocator.allocator, size);
        sieve.run();
        sieve.deinit();
    }

    const elapsed = @intToFloat(f32, timer.read()) / 1_000_000_000.0;
    print("Passes: {}, Time: {d:.5}, Avg: {d:.5}, Limit: {}\n", .{
        passes,
        elapsed,
        elapsed / @intToFloat(f32, passes),
        size,
    });
}
