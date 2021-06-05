const std = @import("std");
const Mutex = std.Thread.Mutex.AtomicMutex;
const Allocator = std.mem.Allocator;

// global sorts of things that can be shared across parallel implementations
var worker_slots: [255]*std.Thread = undefined;
var worker_pool: []*std.Thread = undefined;
var worker_started: [255]bool = undefined;
var worker_finished: [255]bool = undefined;
var init_mutex: Mutex = Mutex{};
var init_hold: ?Mutex.Held = null;

/// job sharing parallelism.
pub fn AmdahlRunner(comptime SieveType: type) type {
    const Type = SieveType.Type;
    const sieve_size = SieveType.size;
    const field_size = sieve_size >> 1;

    return struct {
        // PRIVATE TYPES
        const Self = @This();
        // information passed into the running thread.
        const ThreadInfo = struct {self: *Self, index: usize};
        // the job stores relevant information about what to be run.
        const Job = struct {factor: usize};

        // struct state
        sieve: SieveType = undefined,
        current_job: ?Job = null,
        finished: bool = false,

        pub fn init(self: *Self, allocator: *Allocator) !void {
            // set up the global worker pool, by abstracting a slice out of the available slots.
            var totalWorkers: usize = (std.math.min(try std.Thread.cpuCount(), 256) >> 1) - 1;
            worker_pool = worker_slots[0..totalWorkers];

            // lock all of the workers before launching.
            init_hold = init_mutex.acquire();

            // create the sieve and initialize the threadPool
            var sieve = try SieveType.create(allocator);
            errdefer sieve.destroy();
            self.sieve = sieve;

            for (worker_pool) |*thread_ptr, index| {
                // NB as of zig 0.8.0 (~June 4 2021), std.Thread.Spawn will take params
                // (func, context) instead of (context, func)
                thread_ptr.* = try std.Thread.spawn(
                    ThreadInfo{ .self = self, .index = index},
                    workerLoop);
            }
        }

        pub fn deinit(self: *Self) void {
            // first, set the "finished flag to true"
            @atomicStore(bool, &self.finished, true, .Monotonic);
            // join all of the threads.
            for (worker_pool) | thread_ptr, index | {
                thread_ptr.wait();
            }
            // deinit the sieve.
            self.sieve.destroy();
        }

        pub fn run(self: *Self, passes: *u64) void {
            // the main loop.
            init_hold.?.release();
            init_hold = null;

            var maybe_job = self.fetchJob();

            while (maybe_job) |job| {
                self.runJob(job);
                maybe_job = self.fetchJob();
            }

            // spinlock till everyone else has finished.
            while (threadsAreRunning()) {
                std.time.sleep(100);
            }

            passes.* += 1;
        }

        fn workerLoop(info: ThreadInfo) void {
            while (!@atomicLoad(bool, &info.self.finished, .Monotonic)) {
                if (info.self.fetchJob()) |job| {
                    // ensure that the state is "started"
                    worker_started[info.index] = true;
                    info.self.runJob(job);
                } else {
                    // if we started this generation, set it to finished.
                    setFinished(info.index);
                    // wait 1ms for the next job to come around.
                    std.time.sleep(1000);
                }
            }
        }

        pub fn reset(self: *Self) void {
            init_hold = if (init_hold) | hold| hold else init_mutex.acquire();

            // reset all of the workers.
            for (worker_pool) |_, index| {
                worker_started[index] = false;
                worker_finished[index] = false;
            }

            self.sieve.reset();

            // set up the current job to be the first.
            self.current_job = Job{ .factor = 3 };
        }

        /////// UTILITY FUNCTIONS

        fn fetchJob(self: *Self) ?Job {
            comptime const stop = @floatToInt(usize, @sqrt(@intToFloat(f64, sieve_size)));

            const hold = init_mutex.acquire();
            defer hold.release();

            var field = self.sieve.field;

            if (self.current_job) |current_job| {
                // set up the next job
                var num: u64 = current_job.factor + 2;
                while (num < stop) : (num += 2) {
                    if (SieveType.Type == bool) {
                        if (field.*[num >> 1]) { break; }
                    } else {
                        if (field.*[num >> 1] == SieveType.TRUE) { break; }
                    }
                }

                var next_job = if (num <= stop) Job{ .factor = num } else null;
                self.current_job = next_job;

                return current_job;
            } else {
                return null;
            }
        }

        fn runJob(self: *Self, job: Job) void {
            var num: usize = (job.factor * job.factor) >> 1;
            var field = self.sieve.field;
            while (num < field_size) : (num += job.factor) {
                field.*[num] = @TypeOf(self.sieve).FALSE;
            }
        }

        fn setFinished(index: usize) void {
            const hold = init_mutex.acquire();
            defer hold.release();

            if (worker_started[index]) {
                worker_finished[index] = true;
            }
        }

        fn threadsAreRunning() bool {
            const hold = init_mutex.acquire();
            defer hold.release();

            for (worker_pool) |_, index| {
                // xor operation:  Must be (unstarted AND unfinished) OR (started AND finished)
                if (worker_started[index] != worker_finished[index]) return true;
            }
            return false;
        }

        fn blockOnInit() void {
            var hold = init_mutex.acquire();
            hold.release();
        }
    };
}

//pub fn ParallelAmdahlSieve(
//    comptime Type: type,
//    sieve_size: comptime_int,
//) type {
//    return struct {
//        const TRUE = if (Type == bool) true else 1;
//        const FALSE = if (Type == bool) false else 0;
//
//        const Self = @This();
//        const field_size = sieve_size >> 1;
//        const stop = @floatToInt(usize, @sqrt(@intToFloat(f64, sieve_size)));
//
//        const ThreadInfo = struct { sieve: *Self, index: usize };
//
//        const Job = struct { factor: usize };
//        job_mutex: Mutex = Mutex{},
//        current_job: ?Job = null,
//        finished: bool = false,
//
//        field: [field_size]Type align(std.mem.page_size) = undefined,
//
//        pub fn DataType() type { return Type; }
//        pub fn size() comptime_int { return sieve_size; }
//
//        /// from the main thread, launches all of child threads for parallel computation
//        pub fn init(field: *[field_size]Type) !Self {
//
//            return .{};
//        }
//
//        //pub fn init(self: *Self, field: [field_size]Type) *Self {
//        //    if (init_hold) |_hold| {} else {
//        //        init_hold = init_mutex.acquire();
//        //    }
//        //    defer init_hold.?.release();
////
//        //    self.field = field;
//        //    for (self.field) |*item| {
//        //        item.* = true_val;
//        //    }
//        //    for (worker_pool) |_, index| {
//        //        worker_started[index] = false;
//        //        worker_finished[index] = false;
//        //    }
////
//        //    self.current_job = Job{ .factor = 3 };
//        //    return self;
//        //}
//
//        fn setFinished(index: usize) void {
//            const hold = init_mutex.acquire();
//            defer hold.release();
//
//            if (worker_started[index]) {
//                worker_finished[index] = true;
//            }
//        }
//
//        fn unfinished() bool {
//            const hold = init_mutex.acquire();
//            defer hold.release();
//
//            for (worker_pool) |_, index| {
//                // xor operation:  Must be (unstarted AND unfinished) OR (started AND finished)
//                if (worker_started[index] != worker_finished[index]) return true;
//            }
//            return false;
//        }
//
//
//        fn fetchJob(self: *Self) ?Job {
//            const hold = self.job_mutex.acquire();
//            defer hold.release();
//
//            if (self.current_job) |current_job| {
//                // set up the next job
//                var num: u64 = current_job.factor + 2;
//                while (num < stop) : (num += 2) {
//                    if (self.field[num >> 1]) {
//                        break;
//                    }
//                }
//
//                self.current_job = if (num <= stop) Job{ .factor = num } else null;
//                return current_job;
//            } else {
//                return null;
//            }
//        }
//
//        /// runs the sieve on one job.
//        fn sieveJob(self: *Self, job: Job) void {

//        }
//    };
//}
//
///// embarassing parallism
//pub fn ParallelGustafsonSieve(
//    comptime Type: type,
//    comptime true_val: Type,
//    comptime false_val: Type,
//    sieve_size: comptime_int,
//    run_for: comptime_int,
//) type {
//    return struct {
//        const Self = @This();
//        const field_size = sieve_size >> 1;
//
//        const ThreadInfo = struct {
//            field: [field_size]Type, passes: *u64, finished: *bool
//        };
//
//        pub fn parallelInit(passes: *u64, finished: *bool) !void {
//            var totalWorkers: usize = std.math.min(try std.Thread.cpuCount(), 256) - 1;
//            worker_pool = worker_slots[0..totalWorkers];
//
//            for (worker_pool) |*worker| {
//                var worker_field_slice: []Type = try std.heap.page_allocator.alloc(Type, field_size);
//                var info = ThreadInfo{ .field = worker_field_slice[0..field_size].*, .passes = passes, .finished = finished };
//                worker.* = try std.Thread.spawn(info, workerRun);
//            }
//        }
//
//        pub fn mainRun(passes: *u64, finished: *bool) !u64 {
//            const timer = try std.time.Timer.start();
//            while (timer.read() < run_for * std.time.ns_per_s) {
//                const field = [_]Type{true_val} ** (sieve_size >> 1);
//                Sieve(Type, true_val, false_val, sieve_size).init(field).run();
//                _ = @atomicRmw(u64, passes, .Add, 1, .Monotonic);
//            }
//            @atomicStore(bool, finished, true, .Monotonic);
//            // wait till all of the workers have finished.
//            for (worker_pool) |worker| {
//                worker.wait();
//            }
//
//            return timer.read();
//        }
//
//        fn workerRun(info: ThreadInfo) !void {
//            var sieve = Sieve(Type, true_val, false_val, sieve_size).init(info.field).run();
//
//            while (!@atomicLoad(bool, info.finished, .Monotonic)) {
//                Sieve(Type, true_val, false_val, sieve_size).init(info.field).run();
//                _ = @atomicRmw(u64, info.passes, .Add, 1, .Monotonic);
//            }
//        }
//    };
//}