//! Zig prime sieve by joeshacks.
//!
//! Base algorithm, faithful, 1 bit per odd candidate. Multiples are marked
//! from factor^2 with a step of 2 * factor. Factors below 64 OR whole words
//! with their mark pattern, as `PrimeCPP/solution_5` does (`clearDense`);
//! larger factors mark one bit at a time in comptime-unrolled loops
//! (`clearMultiples`).

const std = @import("std");
const builtin = @import("builtin");

comptime {
    // Bits are addressed through a byte view of the u64 words; the tail mask in
    // `countPrimes` assumes the low bytes of a word come first.
    std.debug.assert(builtin.cpu.arch.endian() == .little);
}

const Word = u64;
const word_bits = @bitSizeOf(Word);

/// Sieve over odd numbers only: bit `i` represents `2 * i + 1`. A set bit marks
/// a composite (inverted logic), so a freshly zeroed buffer means "all prime".
const Sieve = struct {
    size: usize,
    bit_count: usize,
    words: []Word,

    fn init(size: usize) !Sieve {
        const bit_count = (size + 1) / 2;
        const len = (bit_count + word_bits - 1) / word_bits;
        // calloc rather than alloc + @memset: on Linux, @memset lowers to the
        // compiler_rt memset linked into the binary, which stores one byte at
        // a time and costs about a quarter of a pass. libc zeroes far faster.
        const ptr = std.c.calloc(len, @sizeOf(Word)) orelse return error.OutOfMemory;
        const words: [*]Word = @ptrCast(@alignCast(ptr));
        return .{ .size = size, .bit_count = bit_count, .words = words[0..len] };
    }

    fn deinit(self: *Sieve) void {
        std.c.free(self.words.ptr);
    }

    fn bytes(self: *Sieve) [*]u8 {
        return @ptrCast(self.words.ptr);
    }

    fn isComposite(self: *Sieve, index: usize) bool {
        return (self.bytes()[index >> 3] >> @intCast(index & 7)) & 1 != 0;
    }

    fn run(self: *Sieve) void {
        const q = std.math.sqrt(self.size);
        var factor: usize = 3;
        var first = true;
        while (factor <= q) : (factor += 2) {
            if (self.isComposite(factor >> 1)) continue;
            // Start at factor^2; stepping 2 * factor in numbers is `factor` in bits.
            const start = (factor * factor) >> 1;
            if (factor < dense_step_limit) {
                // The first factor's marks land on an all-zero buffer, so it can
                // store its pattern instead of read-modify-writing it.
                if (first) clearDense(true, self.words, start, factor) else clearDense(false, self.words, start, factor);
                first = false;
            } else {
                clearMultiples(self.bytes(), start, factor, self.bit_count);
            }
        }
    }

    fn countPrimes(self: *Sieve) usize {
        var composites: usize = 0;
        const full = self.bit_count / word_bits;
        for (self.words[0..full]) |w| composites += @popCount(w);
        const rem = self.bit_count % word_bits;
        if (rem != 0) {
            const mask = (@as(Word, 1) << @intCast(rem)) - 1;
            composites += @popCount(self.words[full] & mask);
        }
        // Bit 0 stands for 1, which is counted in place of 2.
        return self.bit_count - composites;
    }
};

/// Steps below this use word patterns; larger ones touch at most one bit per word.
const dense_step_limit = word_bits;
/// Words per vector op in `clearDense`.
const lanes = 16;
const Vec = @Vector(lanes, Word);
const ShiftVec = @Vector(lanes, std.math.Log2Int(Word));

/// Mask of bits `offset`, `offset + step`, ... within one word.
fn stepMask(offset: usize, step: usize) Word {
    var mask: Word = 0;
    var bit = offset;
    while (bit < word_bits) : (bit += step) mask |= @as(Word, 1) << @intCast(bit);
    return mask;
}

/// The marks of a step form a bit pattern with period `step`. Moving `n` bits
/// further along it is the same as moving `n % step` bits, so the mask of the
/// word `n / 64` words later is a rotation within the period:
/// `(m >> d) | (m << (step - d))` with `d = n % step`.
fn rollAmounts(n: usize, step: usize) struct { std.math.Log2Int(Word), std.math.Log2Int(Word) } {
    const d = n % step;
    return .{ @intCast(d), @intCast(step - d) };
}

/// Same marks as `clearMultiples`, for small steps where every word holds
/// several marks: OR whole words with the step's pattern. The masks are
/// rolled forward in registers rather than read from a table. Bits past the
/// sieve in the last word may be set; they are masked off when counting.
fn clearDense(comptime overwrite: bool, words: []Word, start: usize, step: usize) void {
    var w = start / word_bits;
    const bit = start % word_bits;
    // Pattern for word w, as if the marks also continued below `start`.
    var mask = stepMask(bit % step, step);
    const first = mask & (~@as(Word, 0) << @intCast(bit));
    words[w] = if (overwrite) first else words[w] | first;
    w += 1;

    const r1, const l1 = rollAmounts(word_bits, step);
    var masks: Vec = undefined;
    inline for (0..lanes) |k| {
        mask = (mask >> r1) | (mask << l1);
        masks[k] = mask;
    }

    const rv, const lv = rollAmounts(lanes * word_bits, step);
    const shr: ShiftVec = @splat(rv);
    const shl: ShiftVec = @splat(lv);
    while (w + lanes <= words.len) : (w += lanes) {
        const dst = words[w..][0..lanes];
        dst.* = if (overwrite) masks else @as(Vec, dst.*) | masks;
        masks = (masks >> shr) | (masks << shl);
    }

    mask = masks[0];
    for (words[w..]) |*dst| {
        dst.* = if (overwrite) mask else dst.* | mask;
        mask = (mask >> r1) | (mask << l1);
    }
}

/// Sets bits start, start + step, ... below `limit`, one at a time.
///
/// `step` is odd, so eight steps advance exactly `step` bytes and the
/// in-byte bit pattern repeats. That makes the eight masks of a round depend
/// only on `start % 8` and `step % 8`, so we dispatch to one of 32 comptime
/// specializations whose masks are immediates and whose byte offsets are
/// `k * (step / 8) + constant`.
fn clearMultiples(bytes: [*]u8, start: usize, step: usize, limit: usize) void {
    const key = ((start & 7) << 2) | ((step & 7) >> 1);
    switch (key) {
        inline 0...31 => |k| clearUnrolled(k >> 2, ((k & 3) << 1) | 1, bytes, start, step, limit),
        else => unreachable,
    }
}

inline fn clearUnrolled(
    comptime r: usize,
    comptime b: usize,
    bytes: [*]u8,
    start: usize,
    step: usize,
    limit: usize,
) void {
    const a = step >> 3;
    var idx = start;
    if (limit > 7 * step) {
        const stop = limit - 7 * step;
        var p = bytes + (start >> 3);
        while (idx < stop) : (idx += 8 * step) {
            inline for (0..8) |k| {
                const bit = r + k * b;
                p[k * a + (bit >> 3)] |= @as(u8, 1) << @intCast(bit & 7);
            }
            p += step;
        }
    }
    while (idx < limit) : (idx += step) {
        bytes[idx >> 3] |= @as(u8, 1) << @intCast(idx & 7);
    }
}

fn expectedCount(size: usize) ?usize {
    return switch (size) {
        10 => 4,
        100 => 25,
        1_000 => 168,
        10_000 => 1229,
        100_000 => 9592,
        1_000_000 => 78498,
        10_000_000 => 664579,
        100_000_000 => 5761455,
        else => null,
    };
}

pub fn main(init: std.process.Init) !void {
    const io = init.io;

    // Read at runtime so the compiler cannot specialize on the sieve size.
    var size: usize = 1_000_000;
    var args = init.minimal.args.iterate();
    _ = args.skip();
    if (args.next()) |arg| size = try std.fmt.parseInt(usize, arg, 10);

    const run_ns: i96 = 5 * std.time.ns_per_s;
    const start = std.Io.Clock.Timestamp.now(io, .awake);
    var passes: usize = 0;
    var elapsed: i96 = 0;
    // Like the original, only the last sieve is counted and validated.
    var sieve: Sieve = undefined;
    while (true) {
        sieve = try Sieve.init(size);
        sieve.run();
        passes += 1;
        elapsed = start.untilNow(io).raw.nanoseconds;
        if (elapsed >= run_ns) break;
        sieve.deinit();
    }
    const count = sieve.countPrimes();
    sieve.deinit();

    if (expectedCount(size)) |expected| {
        if (count != expected) {
            std.debug.print("Invalid result: counted {d} primes, expected {d}\n", .{ count, expected });
            std.process.exit(1);
        }
    }

    var buf: [256]u8 = undefined;
    var stdout = std.Io.File.stdout().writerStreaming(io, &buf);
    const seconds = @as(f64, @floatFromInt(elapsed)) / std.time.ns_per_s;
    try stdout.interface.print("joeshacks;{d};{d:.6};1;algorithm=base,faithful=yes,bits=1\n", .{ passes, seconds });
    try stdout.interface.flush();
}
