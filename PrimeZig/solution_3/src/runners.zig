const std = @import("std");
const Mutex = std.Io.Mutex;
const Allocator = std.mem.Allocator;
const coerce = @import("opts.zig").coerce;

const expected_1M_result = 78_498;

pub fn SingleThreadedRunner(comptime Sieve: type, comptime _opt: anytype) type {
    _ = _opt;
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
        pub fn init(sieve_size: usize, passes: *u64, io: std.Io) !Self {
            _ = io;
            return Self{
                .sieve_size = sieve_size,
                .passes = passes,
            };
        }

        pub fn deinit(self: *Self) void {
            _ = self;
        }

        pub fn sieveInit(self: *Self) !void {
            self.sieve = try Sieve.init(self.sieve_size);
            self.factor = Sieve.STARTING_FACTOR;
        }

        pub fn sieveDeinit(self: *Self) void {
            self.sieve.deinit();
        }

        pub fn run(self: *Self) void {
            const stop = @as(usize, @intFromFloat(@sqrt(@as(f64, @floatFromInt(self.sieve_size)))));
            var factor = self.factor;

            while (factor <= stop) : (factor = self.sieve.findNextFactor(factor)) {
                self.sieve.runFactor(factor);
            }

            // in debug mode, verify that we got the right answer.
            if (@import("builtin").mode == .Debug) {
                if (self.sieve_size == 1_000_000) {
                    const prime_count = self.sieve.primeCount();
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
        pub const authors = if (@hasDecl(Sieve, "authors")) Sieve.authors else "ManDeJan&ityonemo&SpexGuy";
        pub fn threads() !usize {
            return 1;
        }
    };
}

// global sorts of things that can be shared across parallel implementations
var worker_slots: [255]std.Thread = undefined;
var worker_pool: []std.Thread = undefined;
var worker_started: [255]bool = undefined;
var worker_finished: [255]bool = undefined;
var init_mutex: Mutex = .init;
var init_held: bool = false;

// utility functions
fn acquire(io: std.Io) void {
    init_mutex.lockUncancelable(io);
}

fn release(io: std.Io) void {
    init_mutex.unlock(io);
}

fn blockOnInit(io: std.Io) void {
    acquire(io);
    release(io);
}

// should we apply a thread count reduction assuming that the system is hyperthreading
// and we should only spawn half as many cores.  This setting makes sense if computation is
// more expensive than coordination, but it could be architecture-dependent.
const ParallelismOpts = struct { no_ht: bool = false };

/// job sharing parallelism.
pub fn AmdahlRunner(comptime Sieve: type, comptime opt_: anytype) type {
    const opt = coerce(ParallelismOpts, opt_);
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
        io: std.Io,

        pub fn init(sieve_size: usize, passes: *u64, io: std.Io) !Self {
            // set up the global worker pool, by abstracting a slice out of the available slots.
            const totalWorkers: usize = (@min(try std.Thread.getCpuCount(), 256) >> ht_reduction) - 1;
            worker_pool = worker_slots[0..totalWorkers];

            // lock all of the workers before launching.
            acquire(io);
            init_held = true;

            return Self{
                .stop = @as(usize, @intFromFloat(@sqrt(@as(f64, @floatFromInt(sieve_size))))),
                .sieve_size = sieve_size,
                .passes = passes,
                .io = io,
            };
        }

        pub fn deinit(self: *Self) void {
            // first, set the "finished flag to true"
            @atomicStore(bool, &self.finished, true, .monotonic);
            // join all of the threads.
            for (worker_pool) |thread_ptr| {
                thread_ptr.join();
            }
        }

        pub fn sieveInit(self: *Self) !void {
            // create the sieve and initialize the threadPool
            var sieve = try Sieve.init(self.sieve_size);
            errdefer sieve.deinit();

            if (! self.started) {
                for (worker_pool, 0..) |*thread_ptr, index| {
                    thread_ptr.* = try std.Thread.spawn(.{}, workerLoop, .{ThreadInfo{ .self = self, .index = index }});
                }
                self.started = true;
            }

            // reset the per-worker state for this pass under the lock.  Without this, the
            // flags from the previous pass make threadsAreRunning() report idle workers while
            // they are still running a job, and run() can return before the sieve is complete.
            // On the first pass, the lock is still held from init().
            const need_lock = !init_held;
            if (need_lock) acquire(self.io);
            for (worker_pool, 0..) |_, index| {
                worker_started[index] = false;
                worker_finished[index] = false;
            }
            self.sieve = sieve;
            const factor = Sieve.STARTING_FACTOR;
            self.current_job = Job{.factor = factor};
            if (need_lock) release(self.io);
        }

        pub fn sieveDeinit(self: *Self) void {
            self.sieve.deinit();
        }

        pub fn run(self: *Self) void {
            // unlock the workers.
            if (init_held) {
                release(self.io);
                init_held = false;
            }

            while (self.fetchJob()) |job| {
                self.runJob(job);
            }

            // spinlock till everyone else has finished.
            while (threadsAreRunning(self.io)) {
                self.io.sleep(.fromNanoseconds(100), .awake) catch {};
            }

            // in debug mode, verify that we got the right answer.
            if (@import("builtin").mode == .Debug) {
                if (self.sieve_size == 1_000_000) {
                    std.debug.assert(self.sieve.primeCount() == expected_1M_result);
                }
            }

            self.passes.* += 1;
        }

        fn workerLoop(info: ThreadInfo) void {
            while (!@atomicLoad(bool, &info.self.finished, .monotonic)) {
                if (info.self.fetchJob()) |job| {
                    // ensure that the state is "started"
                    worker_started[info.index] = true;
                    info.self.runJob(job);
                } else {
                    // if we started this generation, set it to finished.
                    setFinished(info.self.io, info.index);
                    // wait 1ms for the next job to come around.
                    info.self.io.sleep(.fromNanoseconds(1000), .awake) catch {};
                }
            }
        }

        /////// UTILITY FUNCTIONS

        fn fetchJob(self: *Self) ?Job {
            acquire(self.io);
            defer release(self.io);

            if (self.current_job) |current_job| {
                // set up the next job
                const next_factor = self.sieve.findNextFactor(current_job.factor);

                const next_job = if (next_factor <= self.stop) Job{ .factor = next_factor } else null;
                self.current_job = next_job;

                return current_job;
            } else {
                return null;
            }
        }

        fn runJob(self: *Self, job: Job) void {
            self.sieve.runFactor(job.factor);
        }

        fn setFinished(io: std.Io, index: usize) void {
            acquire(io);
            defer release(io);

            if (worker_started[index]) {
                worker_finished[index] = true;
            }
        }

        fn threadsAreRunning(io: std.Io) bool {
            acquire(io);
            defer release(io);

            for (worker_pool, 0..) |_, index| {
                // xor operation:  Must be (unstarted AND unfinished) OR (started AND finished)
                if (worker_started[index] != worker_finished[index]) return true;
            }
            return false;
        }

        pub const name = "parallel-amdahl-" ++ Sieve.name;
        pub const authors = if (@hasDecl(Sieve, "authors")) Sieve.authors else "ManDeJan&ityonemo&SpexGuy";

        pub fn threads() !usize {
            return @min(try std.Thread.getCpuCount(), 256) >> ht_reduction;
        }
    };
}

/// embarassing parallism
pub fn GustafsonRunner(comptime Sieve: type, comptime opt_: anytype) type {
    const opt = coerce(ParallelismOpts, opt_);
    const ht_reduction = if (opt.no_ht) 1 else 0;

    return struct {
        const Self = @This();

        // passthrough information from sieve.
        pub const algo = Sieve.algo;
        pub const bits = Sieve.bits;

        passes: *u64,
        sieve_size: usize,
        io: std.Io,
        started: bool = false,
        finished: bool = false,
        // main thread sieve info
        sieve: Sieve = undefined,
        factor: usize = undefined,

        pub fn init(sieve_size: usize, passes: *u64, io: std.Io) !Self {
            // set up the global worker pool, by abstracting a slice out of the available slots.
            const totalWorkers: usize = (@min(try std.Thread.getCpuCount(), 256) >> ht_reduction) - 1;
            worker_pool = worker_slots[0..totalWorkers];

            // lock all of the workers before launching.
            acquire(io);
            init_held = true;

            return Self{
                .passes = passes,
                .sieve_size = sieve_size,
                .io = io,
            };
        }

        pub fn deinit(self: *Self) void {
            // first, set the "finished flag to true"
            @atomicStore(bool, &self.finished, true, .monotonic);
            // join all of the threads.
            for (worker_pool) |thread_ptr| {
                thread_ptr.join();
            }
        }

        pub fn sieveInit(self: *Self) !void {
            if (!self.started) {
                // spawn worker sieves on the first init call.
                for (worker_pool) |*thread_ptr| {
                    thread_ptr.* = try std.Thread.spawn(.{}, workerLoop, .{self});
                }

                // unlock the worker pool.
                self.started = true;
                release(self.io);
                init_held = false;
            }

            // reset the sieve
            self.sieve = try Sieve.init(self.sieve_size);
            errdefer self.sieve.deinit();

            self.factor = Sieve.STARTING_FACTOR;
        }

        pub fn sieveDeinit(self: *Self) void {
            self.sieve.deinit();
        }

        pub fn run(self: *Self) void {
            runSieve(&self.sieve, self.sieve_size, self.factor);

            // increment the number of passes.
            _ = @atomicRmw(u64, self.passes, .Add, 1, .monotonic);
        }

        pub fn workerLoop(runner: *Self) void {
            blockOnInit(runner.io);

            while (!@atomicLoad(bool, &runner.finished, .monotonic)) {
                var sieve = Sieve.init(runner.sieve_size) catch unreachable;
                defer sieve.deinit();

                const factor: usize = Sieve.STARTING_FACTOR;
                runSieve(&sieve, runner.sieve_size, factor);

                // increment the number of passes.
                _ = @atomicRmw(u64, runner.passes, .Add, 1, .monotonic);
            }
        }

        // utility functions

        fn runSieve(sieve: *Sieve, sieve_size: usize, starting_factor: usize) void {
            const stop = @as(usize, @intFromFloat(@sqrt(@as(f64, @floatFromInt(sieve_size)))));
            var factor = starting_factor;

            while (factor <= stop) : (factor = sieve.findNextFactor(factor)) {
                sieve.runFactor(factor);
            }

            // in debug mode, verify that we got the right answer.
            if (@import("builtin").mode == .Debug) {
                if (sieve_size == 1_000_000) {
                    std.debug.assert(sieve.primeCount() == expected_1M_result);
                }
            }
        }

        pub const name = "parallel-gustafson-" ++ Sieve.name;
        pub const authors = if (@hasDecl(Sieve, "authors")) Sieve.authors else "ManDeJan&ityonemo&SpexGuy";

        pub fn threads() !usize {
            return @min(try std.Thread.getCpuCount(), 256) >> ht_reduction;
        }
    };
}
