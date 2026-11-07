const std = @import("std");
const heap = std.heap;
const print = std.debug.print;
const time = std.time;
const Sieve = @import("./prime.zig").Sieve;

pub fn main(init: std.process.Init) anyerror!void {
    const io = init.io;
    const start = std.Io.Clock.Timestamp.now(io, .awake);
    const size: u64 = 1_000_000;

    var arena = heap.ArenaAllocator.init(heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var passes: u64 = 0;
    while (start.untilNow(io).raw.nanoseconds < 5 * time.ns_per_s) : (passes += 1) {
        var sieve = try Sieve(u64).init(allocator, size);
        sieve.run();
        sieve.deinit();
    }

    const elapsed_ns: u64 = @intCast(start.untilNow(io).raw.nanoseconds);
    const elapsed = @as(f32, @floatFromInt(elapsed_ns)) / @as(f32, @floatFromInt(time.ns_per_s));
    print("Passes: {}, Time: {d:.5}, Avg: {d:.5}, Limit: {}\n", .{
        passes,
        elapsed,
        elapsed / @as(f32, @floatFromInt(passes)),
        size,
    });

    // Following lines added by rbergen to conform to drag race output format
    print("\n", .{});
    var buffer: [256]u8 = undefined;
    var stdout = std.Io.File.stdout().writerStreaming(io, &buffer);
    try stdout.interface.print("devblok;{};{d:.5};1;algorithm=base,faithful=yes,bits=8\n", .{ passes, elapsed });
    try stdout.interface.flush();
}
