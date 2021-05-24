const std = @import("std");
const heap = std.heap;
const print = std.debug.print;
const time = std.time;
const Sieve = @import("./prime.zig").Sieve;

pub fn main() anyerror!void {
    var allocator = heap.page_allocator;

    const timer = try time.Timer.start();
    const size: u64 = 1_000_000;

    var slice = try allocator.alloc(bool, size >> 1);
    defer allocator.free(slice);

    var passes: u64 = 0;
    while (timer.read() < 5 * time.ns_per_s) : (passes += 1) {
        var sieve = Sieve.init(slice, size);
        sieve.run();
    }

    const elapsed = @intToFloat(f32, timer.read()) / @intToFloat(f32, time.ns_per_s);

    try std.io.getStdOut().writer().print("zig-ityonemo;{};{d:.5};1\n", .{ passes, elapsed });
}
