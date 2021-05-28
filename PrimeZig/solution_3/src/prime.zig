const std = @import("std");

/// Prime number sieve. Can be configured with any desired time
/// at compile time. So that switching between them would be easy.
pub fn Sieve(
    comptime Type: type,
    comptime true_val: Type,
    comptime false_val: Type,
    sieve_size: comptime_int,
) type {
    return struct {
        const Self = @This();
        const field_size = sieve_size >> 1;

        field: [field_size]Type align(std.mem.page_size),

        pub fn init(field: [field_size]Type) Self {
            return .{
                .field = field,
            };
        }

        pub fn run(self: *Self) void {
            @setAlignStack(256);
            comptime const q = @floatToInt(usize, @sqrt(@intToFloat(f64, sieve_size)));
            var factor: usize = 3;

            while (factor <= q) : (factor += 2) {
                var num: usize = factor;
                factorSet: while (num < field_size) : (num += 2) {
                    if (self.field[num >> 1] == true_val) {
                        factor = num;
                        break :factorSet;
                    }
                }

                num = (factor * factor) >> 1;
                while (num < field_size) : (num += factor) {
                    self.field[num] = false_val;
                }
            }
        }

        pub fn primeCount(self: *Self) usize {
            var count: usize = 0;
            var idx: usize = 0;

            while (idx < field_size) : (idx += 1) {
                count += @boolToInt(self.field[idx] == true_val);
            }

            return count;
        }
    };
}


const Mutex = std.Thread.Mutex.AtomicMutex;

pub fn ParallelAmdahlSieve(
    comptime Type: type,
    comptime true_val: Type,
    comptime false_val: Type,
    sieve_size: comptime_int,
) type {
    return struct {
        const Self = @This();
        const field_size = sieve_size >> 1;
        const stop = @floatToInt(usize, @sqrt(@intToFloat(f64, sieve_size)));

        field: [field_size]Type align(std.mem.page_size),
        mutex: Mutex = Mutex{},
        factor: usize = 1,

        pub fn init(field: [field_size]Type) Self {
            return .{
                .field = field,
            };
        }

        fn fetch_factor(self: *Self) u64 {
            const lock = self.mutex.acquire();
            defer lock.release();

            // return the current factor.
            var num: u64 = self.factor + 2;
            factorSearch: while (num < stop) : (num += 2) {
                if (self.field[num >> 1]) {
                    break :factorSearch;
                }
            }
            self.factor = num;
            return num;
        }

        pub fn parallelRun(self: *Self) !void {
            // ok but let's pretend we can have up to 256 cores.  That should be enough, right?
            var thread_buff : [256] *std.Thread = undefined;
            var totalThreads: usize = 2; //std.math.min(try std.Thread.cpuCount(), 256);
            var threadIndex: usize = 1;  // skip thread zero, that's main thread.
            var threads = thread_buff[0..totalThreads - 1]; // slice into all the available thread slots

            while (threadIndex < totalThreads) : (threadIndex += 1) {
                // NB as of zig 0.8.0 (~June 4 2021), std.Thread.Spawn will take params
                // (func, context) instead of (context, func)
                threads[threadIndex - 1] = try std.Thread.spawn(self, run);
            }

            // run on "self thread"
            self.run();
            // join all the other bad boys.
            for (threads) |thread| { thread.wait(); }
            // and that's it!
        }

        pub fn run(self: *Self) void {
            @setAlignStack(256);
            var factor: usize = 1;

            while (factor <= stop) {
                factor = self.fetch_factor();
                var num: usize = factor;
                num = (factor * factor) >> 1;

                while (num < field_size) : (num += factor) {
                    self.field[num] = false_val;
                }
            }
        }

        pub fn primeCount(self: *Self) usize {
            var count: usize = 0;
            var idx: usize = 0;

            while (idx < field_size) : (idx += 1) {
                count += @boolToInt(self.field[idx] == true_val);
            }

            return count;
        }
    };
}

test "Test byte sieve" {
    const expected_results = .{
        .{             10,         4 },
        .{            100,        25 },
        .{          1_000,       168 },
        .{         10_000,      1229 },
        .{        100_000,      9592 },
        .{      1_000_000,     78498 },
        // Uncommenting the following tests make my compiler crash ¯\_(ツ)_/¯ Probably cause it's allocating huge memory sizes at compile time
        // .{     10_000_000,    664579 },
        // .{    100_000_000,   5761455 },
        // .{  1_000_000_000,  50847534 },
        // .{ 10_000_000_000, 455052511 },
    };

    inline for (expected_results) |result| {
        const size = result[0];
        const count = result[1];

        var field = [_]bool{true} ** (size >> 1);
        var sieve = Sieve(bool, true, false, size).init(field);

        sieve.run();
        std.testing.expectEqual(@as(usize, count), sieve.primeCount());
    }
}

test "Test parallel-amdahl sieve" {
    const expected_results = .{
        .{             10,         4 },
        .{            100,        25 },
        .{          1_000,       168 },
        .{         10_000,      1229 },
        .{        100_000,      9592 },
        .{      1_000_000,     78498 },
        // Uncommenting the following tests make my compiler crash ¯\_(ツ)_/¯ Probably cause it's allocating huge memory sizes at compile time
        // .{     10_000_000,    664579 },
        // .{    100_000_000,   5761455 },
        // .{  1_000_000_000,  50847534 },
        // .{ 10_000_000_000, 455052511 },
    };

    inline for (expected_results) |result| {
        const size = result[0];
        const count = result[1];

        var field = [_]bool{true} ** (size >> 1);
        var sieve = ParallelAmdahlSieve(bool, true, false, size).init(field);

        try sieve.parallelRun();
        std.testing.expectEqual(@as(usize, count), sieve.primeCount());
    }
}

