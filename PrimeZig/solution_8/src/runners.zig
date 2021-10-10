const std = @import("std");
const Mutex = std.Thread.Mutex.AtomicMutex;
const Allocator = std.mem.Allocator;

const expected_1M_result = 78_498;

pub fn SingleThreadedRunner(comptime Sieve: type, comptime _opt: anytype) type {
    return struct {
        // passthrough information from the sieve.
        pub const algo = Sieve.algo;
        pub const bits = Sieve.bits;

        const Self = @This();
        sieve: Sieve = undefined,
        factor: usize = undefined,
        sieve_size: usize = undefined,
        passes: *u64 = undefined,

        // no special thread-related things to do
        pub fn init(sieve_size: usize, passes: *u64) !Self {
            return Self{
                .sieve_size = sieve_size,
                .passes = passes,
            };
        }

        pub fn deinit(self: *Self) void {}

        pub fn sieveInit(self: *Self) !void {
            self.sieve = try Sieve.init(self.sieve_size);
            self.factor = Sieve.STARTING_FACTOR;
        }

        pub fn sieveDeinit(self: *Self) void {
            self.sieve.deinit();
        }

        pub fn run(self: *Self) void {
            const stop = @floatToInt(usize, @sqrt(@intToFloat(f64, self.sieve_size)));
            var factor = self.factor;

            while (factor <= stop) : (factor = self.sieve.findNextFactor(factor)) {
                self.sieve.runFactor(factor);
            }

            // in debug mode, verify that we got the right answer.
            if (std.builtin.mode == .Debug) {
                if (self.sieve_size == 1_000_000) {
                    var prime_count = self.sieve.primeCount();
                    if (prime_count != expected_1M_result) {
                        std.debug.print("hey we got the wrong answer {}\n", .{self.sieve.primeCount()});
                        // panic-death.
                        unreachable;
                    }
                }
            }

            // increment the number of passes.
            self.passes.* += 1;
        }

        pub const name = "single-" ++ Sieve.name;
        pub fn threads() !usize {
            return 1;
        }
    };
}

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

// should we apply a thread count reduction assuming that the system is hyperthreading
// and we should only spawn half as many cores.  This setting makes sense if computation is
// more expensive than coordination, but it could be architecture-dependent.
const ParallelismOpts = struct { no_ht: bool = false };

/// job sharing parallelism.
pub fn AmdahlRunner(comptime Sieve: type, comptime opt: ParallelismOpts) type {
    const ht_reduction = if (opt.no_ht) 1 else 0;

    return struct {
        // PRIVATE TYPES
        const Self = @This();
        // information passed into the running thread.
        const ThreadInfo = struct { self: *Self, index: usize };
        // the job stores relevant information about what to be run.
        const Job = struct { factor: usize };
        pub const algo = Sieve.algo;
        pub const bits = Sieve.bits;

        // struct state
        sieve_size: usize,
        stop: usize,
        sieve: Sieve = undefined,
        current_job: ?Job = null,
        started: bool = false,
        finished: bool = false,
        passes: *u64,

        pub fn init(sieve_size: usize, passes: *u64) !Self {
            // set up the global worker pool, by abstracting a slice out of the available slots.
            var totalWorkers: usize = (std.math.min(try std.Thread.cpuCount(), 256) >> ht_reduction) - 1;
            worker_pool = worker_slots[0..totalWorkers];

            // lock all of the workers before launching.
            init_hold = init_mutex.acquire();

            return Self{
                .stop = @floatToInt(usize, @sqrt(@intToFloat(f64, sieve_size))),
                .sieve_size = sieve_size,
                .passes = passes,
            };
        }

        pub fn deinit(self: *Self) void {
            // first, set the "finished flag to true"
            @atomicStore(bool, &self.finished, true, .Monotonic);
            // join all of the threads.
            for (worker_pool) |thread_ptr, index| {
                thread_ptr.wait();
            }
        }

        pub fn sieveInit(self: *Self) !void {
            // create the sieve and initialize the threadPool
            var sieve = try Sieve.init(self.sieve_size);
            errdefer sieve.deinit();

            if (! self.started) {
                for (worker_pool) |*thread_ptr, index| {
                    thread_ptr.* = try std.Thread.spawn(workerLoop, ThreadInfo{ .self = self, .index = index });
                }
                self.started = true;
            }

            self.sieve = sieve;
            var factor = Sieve.STARTING_FACTOR;
            self.current_job = Job{.factor = factor};
        }

        pub fn sieveDeinit(self: *Self) void {
            self.sieve.deinit();
        }

        pub fn run(self: *Self) void {
            // unlock the workers.
            if (init_hold) | hold | {
                init_hold.?.release();
                init_hold = null;
            }

            while (self.fetchJob()) |job| {
                self.runJob(job);
            }

            // spinlock till everyone else has finished.
            while (threadsAreRunning()) {
                std.time.sleep(100);
            }

            // in debug mode, verify that we got the right answer.
            if (std.builtin.mode == .Debug) {
                if (self.sieve_size == 1_000_000) {
                    var primecount = self.sieve.primeCount();
                    std.debug.assert(self.sieve.primeCount() == expected_1M_result);
                }
            }

            self.passes.* += 1;
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

        /////// UTILITY FUNCTIONS

        fn fetchJob(self: *Self) ?Job {
            const hold = init_mutex.acquire();
            defer hold.release();

            var field = self.sieve.field;

            if (self.current_job) |current_job| {
                // set up the next job
                var next_factor = self.sieve.findNextFactor(current_job.factor);

                var next_job = if (next_factor <= self.stop) Job{ .factor = next_factor } else null;
                self.current_job = next_job;

                return current_job;
            } else {
                return null;
            }
        }

        fn runJob(self: *Self, job: Job) void {
            self.sieve.runFactor(job.factor);
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

        pub const name = "parallel-amdahl-" ++ Sieve.name;

        pub fn threads() !usize {
            return std.math.min(try std.Thread.cpuCount(), 256) >> ht_reduction;
        }
    };
}

/// embarassing parallism
pub fn GustafsonRunner(comptime Sieve: type, comptime opt: ParallelismOpts) type {
    const ht_reduction = if (opt.no_ht) 1 else 0;

    return struct {
        const Self = @This();
        const field_size = sieve_size >> 1;

        // passthrough information from sieve.
        pub const algo = Sieve.algo;
        pub const bits = Sieve.bits;

        passes: *u64,
        sieve_size: usize,
        started: bool = false,
        finished: bool = false,
        // main thread sieve info
        sieve: Sieve = undefined,
        factor: usize = undefined,

        pub fn init(sieve_size: usize, passes: *u64) !Self {
            // set up the global worker pool, by abstracting a slice out of the available slots.
            var totalWorkers: usize = (std.math.min(try std.Thread.cpuCount(), 256) >> ht_reduction) - 1;
            worker_pool = worker_slots[0..totalWorkers];

            // lock all of the workers before launching.
            init_hold = init_mutex.acquire();

            return Self{
                .passes = passes,
                .sieve_size = sieve_size,
            };
        }

        pub fn deinit(self: *Self) void {
            // first, set the "finished flag to true"
            @atomicStore(bool, &self.finished, true, .Monotonic);
            // join all of the threads.
            for (worker_pool) |thread_ptr, index| {
                thread_ptr.wait();
            }
        }

        pub fn sieveInit(self: *Self) !void {
            if (!self.started) {
                // spawn worker sieves on the first init call.
                for (worker_pool) |*thread_ptr, index| {
                    thread_ptr.* = try std.Thread.spawn(workerLoop, self);
                }

                // unlock the worker pool.
                self.started = true;
                init_hold.?.release();
            }

            // reset the sieve
            self.sieve = try Sieve.init(self.sieve_size);
            errdefer sieve.deinit();

            self.factor = Sieve.STARTING_FACTOR;
        }

        pub fn sieveDeinit(self: *Self) void {
            self.sieve.deinit();
        }

        pub fn run(self: *Self) void {
            runSieve(&self.sieve, self.sieve_size, self.factor);

            // increment the number of passes.
            _ = @atomicRmw(u64, self.passes, .Add, 1, .Monotonic);
        }

        pub fn workerLoop(runner: *Self) void {
            blockOnInit();

            while (!@atomicLoad(bool, &runner.finished, .Monotonic)) {
                var sieve = Sieve.init(runner.sieve_size) catch unreachable;
                defer sieve.deinit();

                var factor: usize = Sieve.STARTING_FACTOR;
                runSieve(&sieve, runner.sieve_size, factor);

                // increment the number of passes.
                _ = @atomicRmw(u64, runner.passes, .Add, 1, .Monotonic);
            }
        }

        // utility functions

        fn runSieve(sieve: *Sieve, sieve_size: usize, starting_factor: usize) void {
            const stop = @floatToInt(usize, @sqrt(@intToFloat(f64, sieve_size)));
            var factor = starting_factor;
            var field = sieve.field;

            while (factor <= stop) : (factor = sieve.findNextFactor(factor)) {
                sieve.runFactor(factor);
            }

            // in debug mode, verify that we got the right answer.
            if (std.builtin.mode == .Debug) {
                if (sieve_size == 1_000_000) {
                    std.debug.assert(sieve.primeCount() == expected_1M_result);
                }
            }
        }

        pub const name = "parallel-gustafson-" ++ Sieve.name;

        pub fn threads() !usize {
            return std.math.min(try std.Thread.cpuCount(), 256) >> ht_reduction;
        }
    };
}
