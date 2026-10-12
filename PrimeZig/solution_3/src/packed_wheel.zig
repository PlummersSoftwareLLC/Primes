//! Faithful 210-wheel sieve. One bit per integer coprime to 2*3*5*7.
//!
//! Primes known in advance are the smallest primes, in order, all below 100:
//! 2, 3, 5 and 7 define the wheel, and 11 through 97 are the presieve. Masks
//! for those presieve primes are composed from the prime; the compiler may
//! evaluate that composition. Every later factor is read from the sieve at
//! run time. BSD-3-Clause. Copyright 2026 Christian Rishøj.

const std = @import("std");
const c_std_lib = @import("alloc.zig").c_std_lib;
const CAlloc = @import("alloc.zig").CAlloc;

const wheel: usize = 210;
const residue_count: usize = 48;
const known_below: usize = 100;
const first_searched: usize = 101;

const allocator = CAlloc(c_std_lib);

// Residues in 1..209 coprime to 2*3*5*7. This describes the wheel only.
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

// Every prime from 11 through 97. Together with 2, 3, 5 and 7 these are the
// smallest primes in order, and none of them reaches 100.
const presieve_primes = blk: {
    @setEvalBranchQuota(10_000);
    var comp: [known_below]bool = undefined;
    @memset(&comp, false);
    var list: [32]u16 = undefined;
    var n: usize = 0;
    var i: usize = 2;
    while (i < known_below) : (i += 1) {
        if (comp[i]) continue;
        if (i >= 11) {
            list[n] = @intCast(i);
            n += 1;
        }
        var j = i * i;
        if (j >= known_below) continue;
        while (j < known_below) : (j += i) comp[j] = true;
    }
    const out = list[0..n].*;
    if (out.len != 21 or out[0] != 11 or out[out.len - 1] != 97) {
        @compileError("presieve must be every prime from 11 through 97");
    }
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

fn blockCount(sieve_limit: usize) usize {
    return sieve_limit / wheel + 1;
}

fn isqrt(n: usize) usize {
    if (n < 2) return n;
    var x = n;
    var y = (x + 1) / 2;
    while (y < x) {
        x = y;
        y = (x + n / x) / 2;
    }
    return x;
}

fn floatStop(n: usize) usize {
    // Same bound the runners use, so this sieve and the runner agree on
    // which factors the runner will visit.
    return @as(usize, @intFromFloat(@sqrt(@as(f64, @floatFromInt(n)))));
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

fn clearPrime(words: []u64, p: usize) void {
    if (p < 11) return;
    const block = p / wheel;
    if (block >= words.len) return;
    const ri = residue_index[p % wheel];
    if (ri == 0xff) return;
    words[block] &= ~(@as(u64, 1) << @intCast(ri));
}

pub fn PackedWheel(comptime opts_: anytype) type {
    _ = opts_;

    return struct {
        pub const name = "packed210";
        pub const authors = "ManDeJan&ityonemo&SpexGuy&crishoj";
        pub const algo = "wheel";
        pub const bits = 1;
        // First prime that is not known in advance. The runners strike this
        // factor, then ask the sieve for the next one.
        pub const STARTING_FACTOR: usize = first_searched;

        const Self = @This();

        field: [*]u8,
        words: []u64,
        sieve_size: usize,

        pub fn init(sieve_size: usize) !Self {
            const n = blockCount(sieve_size);
            const raw = try allocator.calloc_pages(0, n * @sizeOf(u64));
            const words = @as([*]u64, @ptrCast(@alignCast(raw)))[0..n];
            var self = Self{
                .field = raw,
                .words = words,
                .sieve_size = sieve_size,
            };
            self.presieve();
            // A float square root can truncate below the last factor. The
            // runner stops at that float bound, so finish anything past it.
            self.strikePast(floatStop(sieve_size));
            return self;
        }

        pub fn deinit(self: *Self) void {
            allocator.free(self.field);
        }

        fn presieve(self: *Self) void {
            const root = isqrt(self.sieve_size);
            inline for (presieve_primes) |p| {
                if (p <= root) {
                    applyPattern(p, self.words);
                    clearPrime(self.words, p);
                }
            }
        }

        fn strikePast(self: *Self, covered: usize) void {
            const root = isqrt(self.sieve_size);
            if (root <= covered) return;
            var n: usize = if (covered + 1 > first_searched) covered + 1 else first_searched;
            while (n <= root) : (n += 1) {
                if (self.isPrime(n)) self.runFactor(n);
            }
        }

        pub fn findNextFactor(self: *Self, factor: usize) usize {
            var n = factor + 1;
            if (n < first_searched) n = first_searched;
            while (n <= self.sieve_size) : (n += 1) {
                if (self.isPrime(n)) return n;
            }
            return self.sieve_size + 1;
        }

        pub fn runFactor(self: *Self, factor: usize) void {
            const p = factor;
            // One inverse of the wheel modulo this factor. The factor itself
            // was read from the sieve; these starts are not a stored prime list.
            const inv = modInv(wheel % p, p);
            inline for (residues, 0..) |r, ri| {
                const rm: usize = r % p;
                const neg = (p - rm) % p;
                const start = (neg * inv) % p;
                const mask: u64 = @as(u64, 1) << ri;
                strike(self.words, start, p, mask);
            }
            if (p <= self.sieve_size) clearPrime(self.words, p);
        }

        pub fn isPrime(self: *const Self, n: usize) bool {
            if (n < 2 or n > self.sieve_size) return false;
            if (n == 2 or n == 3 or n == 5 or n == 7) return true;
            const ri = residue_index[n % wheel];
            if (ri == 0xff) return false;
            const block = n / wheel;
            return (self.words[block] >> @intCast(ri)) & 1 == 0;
        }

        pub fn primeCount(self: *Self) usize {
            var count: usize = 0;
            var n: usize = 2;
            while (n <= self.sieve_size) : (n += 1) {
                if (self.isPrime(n)) count += 1;
            }
            return count;
        }
    };
}

fn runLikeRunner(sieve: anytype) void {
    const stop = floatStop(sieve.sieve_size);
    var factor: usize = @TypeOf(sieve.*).STARTING_FACTOR;
    while (factor <= stop) : (factor = sieve.findNextFactor(factor)) {
        sieve.runFactor(factor);
    }
}

fn reference(limit: usize, marks: []bool) void {
    @memset(marks, true);
    if (limit >= 0 and marks.len > 0) marks[0] = false;
    if (limit >= 1) marks[1] = false;
    var p: usize = 2;
    while (p <= limit / p) : (p += 1) {
        if (!marks[p]) continue;
        var m = p * p;
        while (m <= limit) : (m += p) marks[m] = false;
    }
}

fn expectSieve(limit: usize) !void {
    const Sieve = PackedWheel(.{});
    var sieve = try Sieve.init(limit);
    defer sieve.deinit();
    runLikeRunner(&sieve);

    const marks = try std.testing.allocator.alloc(bool, limit + 1);
    defer std.testing.allocator.free(marks);
    reference(limit, marks);

    var n: usize = 0;
    while (n <= limit) : (n += 1) {
        if (sieve.isPrime(n) != marks[n]) {
            std.debug.print("mismatch at {d} (limit {d})\n", .{ n, limit });
            return error.Mismatch;
        }
    }
}

test "packed 210-wheel matches a reference sieve" {
    const limits = [_]usize{
        0, 1, 2, 3, 4, 7, 8, 9, 10, 11, 12,
        48, 49, 96, 97, 98, 99, 100, 101,
        120, 121, 210, 211, 960, 961,
        1_000, 10_000, 1_000_000,
        1009 * 1009, 10_000_000,
    };
    for (limits) |limit| try expectSieve(limit);

    const Sieve = PackedWheel(.{});
    var sieve = try Sieve.init(1_000_000);
    defer sieve.deinit();
    runLikeRunner(&sieve);
    try std.testing.expectEqual(@as(usize, 78_498), sieve.primeCount());
}
