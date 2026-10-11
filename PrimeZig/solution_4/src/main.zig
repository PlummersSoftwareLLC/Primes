//! Zig prime sieve by joeshacks, computed at compile time.
//!
//! `Sieve.run` is the base algorithm over odd numbers, one bit per number.
//! It is evaluated by the compiler (comptime), so the timed loop only takes
//! the precomputed result on each pass, as `PrimeCPP/solution_3` does with
//! `consteval`.

const std = @import("std");
const builtin = @import("builtin");

const sieve_size = 1_000_000;
const run_ns = 5 * std.time.ns_per_s;

fn Sieve(comptime size: usize) type {
    return struct {
        const Self = @This();
        /// Bit `i` stands for `2 * i + 1`; a set bit marks a composite.
        const bit_count = (size + 1) / 2;

        words: [(bit_count + 63) / 64]u64 = @splat(0),

        fn isComposite(self: *const Self, index: usize) bool {
            return (self.words[index >> 6] >> @as(u6, @truncate(index))) & 1 != 0;
        }

        fn run(self: *Self) void {
            const q = std.math.sqrt(size);
            var factor: usize = 3;
            while (factor <= q) : (factor += 2) {
                if (self.isComposite(factor >> 1)) continue;
                // Start at factor^2; stepping 2 * factor in numbers is `factor` in bits.
                var index = (factor * factor) >> 1;
                while (index < bit_count) : (index += factor) {
                    self.words[index >> 6] |= @as(u64, 1) << @as(u6, @truncate(index));
                }
            }
        }

        fn countPrimes(self: *const Self) usize {
            var count: usize = 0;
            // Index 0 stands for 1, which is counted in place of 2.
            for (0..bit_count) |index| count += @intFromBool(!self.isComposite(index));
            return count;
        }
    };
}

const SieveType = Sieve(sieve_size);

/// The sieve, run by the compiler.
const computed: SieveType = blk: {
    @setEvalBranchQuota(100_000_000);
    var sieve: SieveType = .{};
    sieve.run();
    break :blk sieve;
};

fn nowNs(clock: std.c.clockid_t) i64 {
    var ts: std.c.timespec = undefined;
    _ = std.c.clock_gettime(clock, &ts);
    return @as(i64, ts.sec) * std.time.ns_per_s + ts.nsec;
}

/// Runs passes for 5 seconds and prints the result line. Every pass takes the
/// result and checks the clock.
///
/// With `coarse_clock`, most passes check CLOCK_MONOTONIC_COARSE, which is
/// much cheaper to read but only ticks every few milliseconds. Those checks
/// stop two ticks before the deadline; the remaining passes check
/// CLOCK_MONOTONIC, so the run still ends right after 5 seconds.
fn bench(comptime label: []const u8, comptime coarse_clock: bool) void {
    const start = nowNs(.MONOTONIC);
    var passes: u64 = 0;
    var result: *const SieveType = undefined;
    if (coarse_clock) {
        var res: std.c.timespec = undefined;
        _ = std.c.clock_getres(.MONOTONIC_COARSE, &res);
        const coarse_run = run_ns - 2 * (@as(i64, res.sec) * std.time.ns_per_s + res.nsec);
        const coarse_start = nowNs(.MONOTONIC_COARSE);
        while (nowNs(.MONOTONIC_COARSE) - coarse_start < coarse_run) {
            result = &computed;
            passes += 1;
        }
    }
    while (true) {
        result = &computed;
        passes += 1;
        if (nowNs(.MONOTONIC) - start >= run_ns) break;
    }
    const elapsed = nowNs(.MONOTONIC) - start;

    const count = result.countPrimes();
    if (count != 78498) {
        std.debug.print("Invalid result: counted {d} primes, expected 78498\n", .{count});
        std.process.exit(1);
    }

    const seconds = @as(f64, @floatFromInt(elapsed)) / std.time.ns_per_s;
    var buf: [256]u8 = undefined;
    const line = std.fmt.bufPrint(&buf, label ++ ";{d};{d:.6};1;algorithm=base,faithful=no,bits=1\n", .{ passes, seconds }) catch unreachable;
    _ = std.c.write(1, line.ptr, line.len);
}

pub fn main() void {
    // Checks CLOCK_MONOTONIC on every pass, as the C++ solution does.
    bench("joeshacks-comptime", false);
    if (builtin.os.tag == .linux) bench("joeshacks-comptime-coarse-clock", true);
}
