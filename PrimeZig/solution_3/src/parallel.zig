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

// utility functions
fn blockOnInit() void {
    var hold = init_mutex.acquire();
    hold.release();
}

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
    };
}

/// embarassing parallism
pub fn GustafsonRunner(
    comptime SieveType: type
) type {
    const Type = SieveType.Type;
    const sieve_size = SieveType.size;
    const field_size = sieve_size >> 1;

    return struct {
        const Self = @This();
        const field_size = sieve_size >> 1;

        allocator: *Allocator = undefined,
        passes: *u64 = undefined,
        started: bool = false,
        finished: bool = false,
        sieve: SieveType = undefined,

        pub fn init(self: *Self, allocator: *Allocator) !void {
            // set up the global worker pool, by abstracting a slice out of the available slots.
            var totalWorkers: usize = (std.math.min(try std.Thread.cpuCount(), 256) >> 1) - 1;
            worker_pool = worker_slots[0..totalWorkers];

            // lock all of the workers before launching.
            init_hold = init_mutex.acquire();

            // create the main thread's sieve and initialize the threadpool
            var sieve = try SieveType.create(allocator);
            errdefer sieve.destroy();
            self.sieve = sieve;
            self.allocator = allocator;

            for (worker_pool) |*thread_ptr, index| {
                // NB as of zig 0.8.0 (~June 4 2021), std.Thread.Spawn will take params
                // (func, context) instead of (context, func)
                thread_ptr.* = try std.Thread.spawn(self, workerLoop);
            }
        }

        pub fn deinit(self: *Self) void {
            // first, set the "finished flag to true"
            @atomicStore(bool, &self.finished, true, .Monotonic);
            // join all of the threads.
            for (worker_pool) | thread_ptr, index | {
                thread_ptr.wait();
            }
            // destroy the main thread's sieve
            self.sieve.destroy();
        }

        pub fn run(self: *Self, passes: *u64) void {
            if (!self.started) {
                init_hold.?.release();
                self.passes = passes;
                self.started = true;
                init_hold = null;
            }

            runSieve(self.sieve);

            // increment the number of passes.
            _ = @atomicRmw(u64, passes, .Add, 1, .Monotonic);
        }

        pub fn workerLoop(runner: *Self) void {
            blockOnInit();
            var sieve = SieveType.create(runner.allocator) catch unreachable;
            defer sieve.destroy();

            while (!@atomicLoad(bool, &runner.finished, .Monotonic)) {
                sieve.reset();
                runSieve(sieve);

                // increment the number of passes.
                _ = @atomicRmw(u64, runner.passes, .Add, 1, .Monotonic);
            }
        }

        // NB only resets the main thread.
        pub fn reset(self: *Self) void { self.sieve.reset(); }

        // utility functions

        fn runSieve(sieve: SieveType) void {
            @setAlignStack(256);
            comptime const stop = @floatToInt(usize, @sqrt(@intToFloat(f64, sieve_size)));

            var factor: usize = 3;
            var field = sieve.field;

            while (factor <= stop) : (factor += 2) {
                var num: usize = factor;
                factorSet: while (num < field_size) : (num += 2) {
                    if (Type == bool) {
                        if (field.*[num >> 1]) {
                            factor = num;
                            break :factorSet;
                        }
                    } else {
                        if (field.*[num >> 1] == SieveType.TRUE) {
                            factor = num;
                            break :factorSet;
                        }
                    }
                }

                num = (factor * factor) >> 1;
                while (num < field_size) : (num += factor) {
                    field.*[num] = SieveType.FALSE;
                }
            }
        }
    };
}