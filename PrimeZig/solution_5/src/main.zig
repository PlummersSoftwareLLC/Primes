// Odd-only and 210-wheel Eratosthenes sieves for the drag race.
// BSD-3-Clause. Copyright 2026 Christian Rishøj.

const builtin = @import("builtin");
const std = @import("std");

const limit: usize = 1_000_000;
const expected_count: usize = 78_498;

const wheel: usize = 210;
const residue_count: usize = 48;

// Numbers in 1..209 coprime to 2*3*5*7. One bit per residue, packed into a
// u64 per block of 210 integers (16 high bits unused).
const residues = blk: {
    var out: [residue_count]u16 = undefined;
    var n: usize = 0;
    var i: u16 = 1;
    while (i < wheel) : (i += 1) {
        if (i % 2 != 0 and i % 3 != 0 and i % 5 != 0 and i % 7 != 0) {
            out[n] = i;
            n += 1;
        }
    }
    if (n != residue_count) @compileError("residue table size");
    break :blk out;
};

const residue_index = blk: {
    var out: [wheel]u8 = undefined;
    @memset(&out, 0xff);
    for (residues, 0..) |r, i| out[r] = @intCast(i);
    break :blk out;
};

fn modInv(a_in: usize, modulus: usize) usize {
    var old_r: i64 = @intCast(a_in % modulus);
    var r: i64 = @intCast(modulus);
    var old_s: i64 = 1;
    var s: i64 = 0;
    while (r != 0) {
        const q = @divTrunc(old_r, r);
        const next_r = old_r - q * r;
        old_r = r;
        r = next_r;
        const next_s = old_s - q * s;
        old_s = s;
        s = next_s;
    }
    if (old_r != 1) unreachable;
    var inv = old_s;
    if (inv < 0) inv += @intCast(modulus);
    return @intCast(inv);
}

fn firstBlock(p: usize, residue: usize) usize {
    const rm = residue % p;
    const neg = (p - rm) % p;
    const inv = modInv(wheel % p, p);
    return (neg * inv) % p;
}

fn pattern(comptime p: usize) [p]u64 {
    var pat: [p]u64 = undefined;
    @memset(&pat, 0);
    for (residues, 0..) |r, ri| {
        const b0 = firstBlock(p, r);
        pat[b0] |= @as(u64, 1) << @intCast(ri);
    }
    return pat;
}

// Sequential pattern fills beat strided bit strikes while the prime still
// touches a large fraction of the buffer. Above this, stride and skip.
const pattern_cutoff: usize = 112;

const pattern_primes = blk: {
    @setEvalBranchQuota(100_000);
    const max_p = pattern_cutoff;
    var comp: [max_p + 1]bool = undefined;
    @memset(&comp, false);
    var list: [80]u16 = undefined;
    var n: usize = 0;
    var i: usize = 2;
    while (i <= max_p) : (i += 1) {
        if (comp[i]) continue;
        if (i >= 11) {
            list[n] = @intCast(i);
            n += 1;
        }
        var j = i * i;
        if (j > max_p) continue;
        while (j <= max_p) : (j += i) comp[j] = true;
    }
    break :blk list[0..n].*;
};

const Strike = struct {
    step: u16,
    starts: [residue_count]u16,
};

const large_primes = blk: {
    @setEvalBranchQuota(1_000_000);
    const max_p = 1000;
    var comp: [max_p + 1]bool = undefined;
    @memset(&comp, false);
    var list: [168]Strike = undefined;
    var n: usize = 0;
    var i: usize = 2;
    while (i <= max_p) : (i += 1) {
        if (comp[i]) continue;
        if (i > pattern_cutoff) {
            var starts: [residue_count]u16 = undefined;
            for (residues, 0..) |r, ri| {
                starts[ri] = @intCast(firstBlock(i, r));
            }
            list[n] = .{ .step = @intCast(i), .starts = starts };
            n += 1;
        }
        var j = i * i;
        if (j > max_p) continue;
        while (j <= max_p) : (j += i) comp[j] = true;
    }
    break :blk list[0..n].*;
};

const clear_primes = blk: {
    @setEvalBranchQuota(1_000_000);
    const max_p = 1000;
    var comp: [max_p + 1]bool = undefined;
    @memset(&comp, false);
    var list: [168]u16 = undefined;
    var n: usize = 0;
    var i: usize = 2;
    while (i <= max_p) : (i += 1) {
        if (comp[i]) continue;
        if (i >= 11) {
            list[n] = @intCast(i);
            n += 1;
        }
        var j = i * i;
        if (j > max_p) continue;
        while (j <= max_p) : (j += i) comp[j] = true;
    }
    break :blk list[0..n].*;
};

fn blockCount(sieve_limit: usize) usize {
    return sieve_limit / wheel + 1;
}

fn applyPattern(comptime p: usize, words: []u64) void {
    const pat = comptime pattern(p);
    var dst = words.ptr;
    const end = words.ptr + words.len;
    if (words.len < p) {
        var k: usize = 0;
        while (dst != end) : ({
            dst += 1;
            k += 1;
        }) dst[0] |= pat[k];
        return;
    }
    const last = end - p;
    while (@intFromPtr(dst) <= @intFromPtr(last)) : (dst += p) {
        inline for (pat, 0..) |mask, k| dst[k] |= mask;
    }
    var k: usize = 0;
    while (dst != end) : ({
        dst += 1;
        k += 1;
    }) dst[0] |= pat[k];
}

fn applyPatterns(words: []u64) void {
    inline for (pattern_primes) |p| applyPattern(p, words);
}

fn strike(words: []u64, start: usize, step: usize, mask: u64) void {
    const n = words.len;
    if (start >= n or step == 0) return;
    var b = start;
    if (n > step * 3 and start < n - step * 3) {
        const end = n - step * 3;
        while (b < end) {
            words[b] |= mask;
            words[b + step] |= mask;
            words[b + 2 * step] |= mask;
            words[b + 3 * step] |= mask;
            b += step * 4;
        }
    }
    while (b < n) : (b += step) words[b] |= mask;
}

fn applyLarge(words: []u64, root: usize) void {
    for (large_primes) |prime| {
        const step: usize = prime.step;
        if (step > root) break;
        inline for (0..residue_count) |ri| {
            const mask: u64 = @as(u64, 1) << ri;
            strike(words, prime.starts[ri], step, mask);
        }
    }
}

fn applyPrime(words: []u64, p: usize) void {
    inline for (residues, 0..) |r, ri| {
        const mask: u64 = @as(u64, 1) << @intCast(ri);
        strike(words, firstBlock(p, r), p, mask);
    }
}

fn clearFactors(words: []u64, root: usize) void {
    for (clear_primes) |p| {
        if (p > root) break;
        const block = p / wheel;
        const ri = residue_index[p % wheel];
        words[block] &= ~(@as(u64, 1) << @intCast(ri));
    }
}

const WheelSieve = struct {
    words: []align(64) u64,
    sieve_limit: usize,
    allocator: std.mem.Allocator,

    fn init(allocator: std.mem.Allocator, sieve_limit: usize) !WheelSieve {
        const n = blockCount(sieve_limit);
        const words = try allocator.alignedAlloc(u64, .@"64", n);
        @memset(words, 0);
        return .{ .words = words, .sieve_limit = sieve_limit, .allocator = allocator };
    }

    fn deinit(self: *WheelSieve) void {
        self.allocator.free(self.words);
    }

    fn run(self: *WheelSieve) void {
        const root = isqrt(self.sieve_limit);
        if (root >= pattern_cutoff) {
            applyPatterns(self.words);
            applyLarge(self.words, root);
        } else {
            for (clear_primes) |p| {
                if (p > root) break;
                applyPrime(self.words, p);
            }
        }
        clearFactors(self.words, root);
    }

    fn isPrime(self: *const WheelSieve, n: usize) bool {
        if (n < 2 or n > self.sieve_limit) return false;
        if (n == 2 or n == 3 or n == 5 or n == 7) return true;
        const r = n % wheel;
        const ri = residue_index[r];
        if (ri == 0xff) return false;
        const block = n / wheel;
        return (self.words[block] >> @intCast(ri)) & 1 == 0;
    }

    fn primeCount(self: *const WheelSieve) usize {
        var count: usize = 0;
        var n: usize = 2;
        // Count via the bit buffer so a wrong bit cannot hide behind a shortcut.
        while (n <= self.sieve_limit) : (n += 1) {
            if (self.isPrime(n)) count += 1;
        }
        return count;
    }
};

fn isqrt(n: usize) usize {
    var x = n;
    var y = (x + 1) / 2;
    while (y < x) {
        x = y;
        y = (x + n / x) / 2;
    }
    return x;
}

fn mono() u64 {
    // Linux keeps the direct vDSO clock. Other targets use the libc clock,
    // which reads the same CLOCK_MONOTONIC. The branch is comptime, so the
    // Linux object code does not go through libc.
    if (builtin.os.tag == .linux) {
        var ts: std.os.linux.timespec = undefined;
        _ = std.os.linux.clock_gettime(.MONOTONIC, &ts);
        return @as(u64, @intCast(ts.sec)) * std.time.ns_per_s + @as(u64, @intCast(ts.nsec));
    } else {
        var ts: std.c.timespec = undefined;
        if (std.c.clock_gettime(.MONOTONIC, &ts) != 0) unreachable;
        return @as(u64, @intCast(ts.sec)) * std.time.ns_per_s + @as(u64, @intCast(ts.nsec));
    }
}

fn validate() void {
    const allocator = std.heap.c_allocator;
    const ref = allocator.alloc(bool, limit + 1) catch @panic("alloc");
    defer allocator.free(ref);
    @memset(ref, true);
    ref[0] = false;
    ref[1] = false;
    var p: usize = 2;
    while (p * p <= limit) : (p += 1) {
        if (!ref[p]) continue;
        var m = p * p;
        while (m <= limit) : (m += p) ref[m] = false;
    }

    const samples = [_]usize{ 10, 100, 1_000, 10_000, limit };
    for (samples) |sample| {
        var sieve = WheelSieve.init(allocator, sample) catch @panic("alloc");
        defer sieve.deinit();
        sieve.run();
        var n: usize = 0;
        var count: usize = 0;
        while (n <= sample) : (n += 1) {
            const is_prime = sieve.isPrime(n);
            if (is_prime != ref[n]) std.debug.panic("mismatch at {d} (limit {d})", .{ n, sample });
            if (is_prime) count += 1;
        }
        if (sample == limit and count != expected_count) {
            std.debug.panic("count {d}", .{count});
        }
    }
}

const Counter = struct {
    value: std.atomic.Value(u64) align(64) = std.atomic.Value(u64).init(0),
};

fn worker(stop: *std.atomic.Value(bool), counter: *Counter) void {
    const allocator = std.heap.c_allocator;
    var passes: u64 = 0;
    while (!stop.load(.acquire)) {
        var sieve = WheelSieve.init(allocator, limit) catch unreachable;
        sieve.run();
        sieve.deinit();
        passes += 1;
    }
    counter.value.store(passes, .release);
}

fn emit(label: []const u8, passes: u64, elapsed_ns: u64, threads: usize, algo: []const u8, bits: usize) void {
    const secs = @as(f64, @floatFromInt(elapsed_ns)) / @as(f64, @floatFromInt(std.time.ns_per_s));
    var buf: [192]u8 = undefined;
    const line = std.fmt.bufPrint(
        &buf,
        "{s};{d};{d:.5};{d};algorithm={s},faithful=yes,bits={d}\n",
        .{ label, passes, secs, threads, algo, bits },
    ) catch unreachable;
    if (builtin.os.tag == .linux) {
        _ = std.os.linux.write(1, line.ptr, line.len);
    } else {
        _ = std.c.write(1, line.ptr, line.len);
    }
}

fn durationNs() u64 {
    const raw = std.c.getenv("SIEVE_SECONDS") orelse return 5 * std.time.ns_per_s;
    const secs = std.fmt.parseInt(u64, std.mem.span(raw), 10) catch 5;
    return secs * std.time.ns_per_s;
}

fn benchSingle(seconds_ns: u64) void {
    const allocator = std.heap.c_allocator;
    var passes: u64 = 0;
    const t0 = mono();
    while (mono() - t0 < seconds_ns) {
        var sieve = WheelSieve.init(allocator, limit) catch unreachable;
        sieve.run();
        sieve.deinit();
        passes += 1;
    }
    emit("crishoj_wheel", passes, mono() - t0, 1, "wheel", 1);
}

fn sleepNs(ns: u64) void {
    if (builtin.os.tag == .linux) {
        var req = std.os.linux.timespec{
            .sec = @intCast(ns / std.time.ns_per_s),
            .nsec = @intCast(ns % std.time.ns_per_s),
        };
        while (true) {
            var rem: std.os.linux.timespec = undefined;
            const rc = std.os.linux.nanosleep(&req, &rem);
            if (rc == 0) return;
            req = rem;
        }
    } else {
        var req = std.c.timespec{
            .sec = @intCast(ns / std.time.ns_per_s),
            .nsec = @intCast(ns % std.time.ns_per_s),
        };
        while (true) {
            var rem: std.c.timespec = undefined;
            const rc = std.c.nanosleep(&req, &rem);
            if (rc == 0) return;
            // libc nanosleep reports interruption as -1 and EINTR.
            if (std.c.errno(rc) != .INTR) return;
            req = rem;
        }
    }
}

fn benchParallel(seconds_ns: u64) void {
    const threads = std.Thread.getCpuCount() catch 1;
    var stop = std.atomic.Value(bool).init(false);
    var counters: [32]Counter = undefined;
    @memset(&counters, .{});
    var handles: [32]std.Thread = undefined;
    const n = @min(threads, counters.len);
    const t0 = mono();
    for (0..n) |i| {
        handles[i] = std.Thread.spawn(.{}, worker, .{ &stop, &counters[i] }) catch unreachable;
    }
    // Sleep instead of spinning so the workers keep every core.
    sleepNs(seconds_ns);
    stop.store(true, .release);
    for (handles[0..n]) |handle| handle.join();
    const elapsed = mono() - t0;
    var passes: u64 = 0;
    for (counters[0..n]) |*counter| passes += counter.value.load(.acquire);
    emit("crishoj_wheel_mt", passes, elapsed, n, "wheel", 1);
}

pub fn main() void {
    validate();
    const seconds_ns = durationNs();
    benchSingle(seconds_ns);
    benchParallel(seconds_ns);
}
