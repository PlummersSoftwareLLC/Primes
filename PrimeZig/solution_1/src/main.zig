const std = @import("std");
const heap = std.heap;
const print = std.debug.print;
const time = std.time;
const Sieve = @import("./prime.zig").Sieve;

pub fn main() anyerror!void {
    const timer = try time.Timer.start();
    const size: u64 = 1_000_000;

    var allocator = heap.ArenaAllocator.init(heap.page_allocator);
    defer allocator.deinit();

    var passes: u64 = 0;
    while (timer.read() < 5 * time.ns_per_s) : (passes += 1) {
        var sieve = try Sieve(u64).init(&allocator.allocator, size);
        sieve.run();
        sieve.deinit();
    }

    const elapsed = @intToFloat(f32, timer.read()) / @intToFloat(f32, time.ns_per_s);
    print("Passes: {}, Time: {d:.5}, Avg: {d:.5}, Limit: {}\n", .{
        passes,
        elapsed,
        elapsed / @intToFloat(f32, passes),
        size,
    });

    // Following 2 lines added by rbergen to conform to drag race output format
    print("\n", .{});
    try std.io.getStdOut().writer().print("devblok;{};{d:.5};1;algorithm=base,faithful=yes,bits=8\n", .{ passes, elapsed });
}
