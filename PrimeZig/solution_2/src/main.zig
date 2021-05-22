const std = @import("std");
const heap = std.heap;
const print = std.debug.print;
const time = std.time;
const prime = @import("./prime.zig");

pub fn main() anyerror!void {
    const timer = try time.Timer.start();

    const size = 1_000_000;
    const run_for = 5; // Seconds

    var passes: u64 = 0;
    while (timer.read() < run_for * time.ns_per_s) : (passes += 1) {
        const field = [_]u8{0} ** size;
        prime.Sieve(u8, 0, 1, size).init(field).run();
    }

    const elapsed = @intToFloat(f32, timer.read()) / @intToFloat(f32, time.ns_per_s);
    try std.io.getStdOut().writer().print("ManDeJan;{};{d:.5};1\n", .{ passes, elapsed });
}
