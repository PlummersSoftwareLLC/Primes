const Mutex = std.Thread.Mutex.AtomicMutex;
var workerSlots: [255]*std.Thread = undefined;
var workerPool: []*std.Thread = undefined;
var workerStarted: [255]bool = undefined;
var workerFinished: [255]bool = undefined;
var initMutex: Mutex = Mutex{};
var initHold: ?Mutex.Held = null;

/// job sharing parallelism.
pub fn ParallelAmdahlSieve(
    comptime Type: type,
    sieve_size: comptime_int,
) type {
    return struct {
        const TRUE = if (Type == bool) true else 1;
        const FALSE = if (Type == bool) false else 0;

        const Self = @This();
        const field_size = sieve_size >> 1;
        const stop = @floatToInt(usize, @sqrt(@intToFloat(f64, sieve_size)));

        const ThreadInfo = struct { sieve: *Self, index: usize };

        const Job = struct { factor: usize };
        jobMutex: Mutex = Mutex{},
        currentJob: ?Job = null,
        finished: bool = false,

        field: [field_size]Type align(std.mem.page_size) = undefined,

        pub fn DataType() type { return Type; }
        pub fn size() comptime_int { return sieve_size; }

        /// from the main thread, launches all of child threads for parallel computation
        pub fn init(field: *[field_size]Type) !Self {
            var totalWorkers: usize = std.math.min(try std.Thread.cpuCount(), 256) - 1;
            workerPool = workerSlots[0..totalWorkers];

            initHold = initMutex.acquire();

            // initialize the threadPool
            for (workerPool) |*thread_ptr, index| {
                // NB as of zig 0.8.0 (~June 4 2021), std.Thread.Spawn will take params
                // (func, context) instead of (context, func)
                thread_ptr.* = try std.Thread.spawn(ThreadInfo{ .sieve = self, .index = index }, workerLoop);
            }

            return .{};
        }

        //pub fn init(self: *Self, field: [field_size]Type) *Self {
        //    if (initHold) |_hold| {} else {
        //        initHold = initMutex.acquire();
        //    }
        //    defer initHold.?.release();
//
        //    self.field = field;
        //    for (self.field) |*item| {
        //        item.* = true_val;
        //    }
        //    for (workerPool) |_, index| {
        //        workerStarted[index] = false;
        //        workerFinished[index] = false;
        //    }
//
        //    self.currentJob = Job{ .factor = 3 };
        //    return self;
        //}

        fn setFinished(index: usize) void {
            const hold = initMutex.acquire();
            defer hold.release();

            if (workerStarted[index]) {
                workerFinished[index] = true;
            }
        }

        fn workerLoop(info: ThreadInfo) void {
            while (!@atomicLoad(bool, &info.sieve.finished, .Monotonic)) {
                if (fetchJob(info.sieve)) |job| {
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

            for (workerPool) |_, index| {
                // xor operation:  Must be (unstarted AND unfinished) OR (started AND finished)
                if (workerStarted[index] != workerFinished[index]) return true;
            }
            return false;
        }

        pub fn mainLoop(self: *Self) void {
            var maybe_job = self.fetchJob();
            while (maybe_job) |job| {
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

            if (self.currentJob) |currentJob| {
                // set up the next job
                var num: u64 = currentJob.factor + 2;
                while (num < stop) : (num += 2) {
                    if (self.field[num >> 1]) {
                        break;
                    }
                }

                self.currentJob = if (num <= stop) Job{ .factor = num } else null;
                return currentJob;
            } else {
                return null;
            }
        }

        /// runs the sieve on one job.
        fn sieveJob(self: *Self, job: Job) void {
            var num: usize = (job.factor * job.factor) >> 1;
            while (num < field_size) : (num += job.factor) {
                self.field[num] = false_val;
            }
        }

        pub fn parallelCleanup(self: *Self) void {
            @atomicStore(bool, &self.finished, true, .Monotonic);
            for (workerPool) |worker| {
                worker.wait();
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

/// embarassing parallism
pub fn ParallelGustafsonSieve(
    comptime Type: type,
    comptime true_val: Type,
    comptime false_val: Type,
    sieve_size: comptime_int,
    run_for: comptime_int,
) type {
    return struct {
        const Self = @This();
        const field_size = sieve_size >> 1;

        const ThreadInfo = struct {
            field: [field_size]Type, passes: *u64, finished: *bool
        };

        pub fn parallelInit(passes: *u64, finished: *bool) !void {
            var totalWorkers: usize = std.math.min(try std.Thread.cpuCount(), 256) - 1;
            workerPool = workerSlots[0..totalWorkers];

            for (workerPool) |*worker| {
                var worker_field_slice: []Type = try std.heap.page_allocator.alloc(Type, field_size);
                var info = ThreadInfo{ .field = worker_field_slice[0..field_size].*, .passes = passes, .finished = finished };
                worker.* = try std.Thread.spawn(info, workerRun);
            }
        }

        pub fn mainRun(passes: *u64, finished: *bool) !u64 {
            const timer = try std.time.Timer.start();
            while (timer.read() < run_for * std.time.ns_per_s) {
                const field = [_]Type{true_val} ** (sieve_size >> 1);
                Sieve(Type, true_val, false_val, sieve_size).init(field).run();
                _ = @atomicRmw(u64, passes, .Add, 1, .Monotonic);
            }
            @atomicStore(bool, finished, true, .Monotonic);
            // wait till all of the workers have finished.
            for (workerPool) |worker| {
                worker.wait();
            }

            return timer.read();
        }

        fn workerRun(info: ThreadInfo) !void {
            var sieve = Sieve(Type, true_val, false_val, sieve_size).init(info.field).run();

            while (!@atomicLoad(bool, info.finished, .Monotonic)) {
                Sieve(Type, true_val, false_val, sieve_size).init(info.field).run();
                _ = @atomicRmw(u64, info.passes, .Add, 1, .Monotonic);
            }
        }
    };
}

//test "Test parallel-amdahl sieve" {
//    inline for (expected_results) |result| {
//        const size = result[0];
//        const expected_count = result[1];
//
//        try run_sieve(ParallelAmdahlSieve(bool, size), expected_count);
//    }
//}