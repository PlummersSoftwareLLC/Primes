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
var threadSlots: [255] *std.Thread = undefined;
var workerPool: [] *std.Thread = undefined;
var workerStarted: [255]bool = undefined;
var workerFinished: [255]bool = undefined;
var initMutex: Mutex = Mutex{};
var initHold: ?Mutex.Held = null;

/// job sharing parallelism.
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

        const ThreadInfo = struct {sieve: *Self, index: usize};

        const Job = struct {factor: usize};
        jobMutex: Mutex = Mutex{},
        currentJob: ?Job = null,

        field: [field_size]Type align(std.mem.page_size) = undefined,

        /// from the main thread, launches all of child threads for parallel computation
        pub fn parallelInit(self: *Self) !void {
            var totalThreads: usize = std.math.min(try std.Thread.cpuCount(), 256);
            workerPool = threadSlots[0..totalThreads - 1];

            initHold = initMutex.acquire();

            // initialize the threadPool
            for (workerPool) |*thread_ptr, index| {
                // NB as of zig 0.8.0 (~June 4 2021), std.Thread.Spawn will take params
                // (func, context) instead of (context, func)
                thread_ptr.* = try std.Thread.spawn(ThreadInfo{.sieve = self, .index = index}, workerLoop);
            }
        }

        pub fn init(self: *Self, field: [field_size]Type) *Self {
            if (initHold) |_hold| {} else {initHold = initMutex.acquire();}
            defer initHold.?.release();

            self.field = field;
            for (self.field) |*item| {item.* = true_val;}
            for (workerPool) |_, index| {
                workerStarted[index] = false;
                workerFinished[index] = false;
            }

            self.currentJob = Job{.factor = 3};
            return self;
        }

        fn setFinished(index: usize) void {
            const hold = initMutex.acquire();
            defer hold.release();

            if (workerStarted[index]) {
                workerFinished[index] = true;
            }
        }

        fn workerLoop(info: ThreadInfo) void {
            while (true) {
                if (fetchJob(info.sieve)) | job | {
                    // ensure that the state is "started"
                    workerStarted[info.index] = true;
                    sieveJob(info.sieve, job);
                } else {
                    // if we started this generation, set it to finished.
                    setFinished(info.index);
                    // wait 1ms for the next job to come around.
                    std.time.sleep(1000);
                }
            }
        }

        fn unfinished() bool {
            const hold = initMutex.acquire();
            defer hold.release();

            for (workerPool) | _, index | {
                // xor operation:  Must be (unstarted AND unfinished) OR (started AND finished)
                if (workerStarted[index] != workerFinished[index]) return true;
            }
            return false;
        }

        pub fn mainLoop(self: *Self) void {
            var maybe_job = self.fetchJob();
            while (maybe_job) | job | {
                self.sieveJob(job);
                maybe_job = self.fetchJob();
            }
            // spinlock till everyone else has finished.
            while (unfinished()) {
                std.time.sleep(100);
            }
        }

        fn fetchJob(self: *Self) ?Job {
            const hold = self.jobMutex.acquire();
            defer hold.release();

            if (self.currentJob) | currentJob | {
                // set up the next job
                var num: u64 = currentJob.factor + 2;
                while (num < stop) : (num += 2) {
                    if (self.field[num >> 1]) { break; }
                }

                self.currentJob = if (num <= stop) Job{.factor = num} else null;
                return currentJob;
            } else { return null; }
        }

        /// runs the sieve on one job.
        fn sieveJob(self: *Self, job: Job) void {
            var num: usize = (job.factor * job.factor) >> 1;
            while (num < field_size) : (num += job.factor) {
                self.field[num] = false_val;
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
        var sieve = ParallelAmdahlSieve(bool, true, false, size){};

        try sieve.parallelInit();

        sieve.init(field).mainLoop();
        std.testing.expectEqual(@as(usize, count), sieve.primeCount());

        //try sieve.mainLoop();
        //std.testing.expectEqual(@as(usize, count), sieve.primeCount());
    }
}

