//! implementation of the ability to pregenerate primes at compile-time
//! and use them. should generate into the compiled artifact exactly the
//! memory layout to be copied in as a "wheel" for the first (n) primes

const std = @import("std");
const OEIS_PRIMES = [_]comptime_int{ 3, 5, 7, 11, 13, 17, 19 };
const IntSieve = @import("sieves.zig").IntSieve;
const Unit = enum { byte, bit };

pub fn Wheel(comptime count: usize, comptime gsize: Unit) type {
    var prods = std.mem.zeroes([OEIS_PRIMES.len]comptime usize);
    var source_primes = std.mem.zeroes([count]comptime_int);

    var prod: comptime_int = 1;

    // fail if we are trying too hard.
    if (count > 5) unreachable;

    for (OEIS_PRIMES) |prime, index| {
        prod *= prime;
        prods[index] = prod;
        if (index < count) {
            source_primes[index] = prime;
        }
    }
    const max_prime = prods[count - 1];
    var field: [max_prime]u8 = undefined;

    // fill out key values to make the generator usable.  Note we are not using
    // an allocator here (because you don't get one at comptime), so instead od using
    // the init function, we must set the field directly.
    var generator = IntSieve(comptime u8, .{}){
        .field = field[0..],
        .allocator = std.heap.page_allocator, //just as a placeholder.  won't be used.
    };

    // this takes a bunch of computational power:  Let it happen.
    @setEvalBranchQuota(1000000);

    _ = generator.reset();

    // elucidate the pattern for each of the primes.
    for (OEIS_PRIMES) |prime, index| {
        if (index < count) {
            generator.runFactor(prime);
            generator.field[prime >> 1] = 0;
        }
    }

    var bigenoughbuf: [20]u8 = undefined;
    var buf = try std.fmt.bufPrint(bigenoughbuf[0..], "{}of{}", .{ generator.primeCount(), max_prime * 2 });
    const buf_len = buf.len;

    if (gsize == .bit) {
        compress(generator.field[0..]);
    }

    return struct {
        pub const primes = source_primes;
        pub const template: *[max_prime]u8 = generator.field[0..max_prime];
        pub const first_prime = OEIS_PRIMES[count];
        pub const name = bigenoughbuf[0..buf_len];
    };
}

// COMPRESSION UTILITIES:  turns a jagged (non-multiple of 8) list of bytes, repeats it 8 times,
// and results in bytes of bitmaps.

fn compress(slice: []u8) void {
    const length = slice.len;
    // encode the first slice.
    for (slice) |value, index| {
        encode(slice, value, index);
    }
    // encode further slices
    var this_index = length;
    while (this_index < length * 8) : (this_index += 1) {
        encode_copy(slice, this_index, this_index % length);
    }
}

fn encode(slice: []u8, value: u8, index: usize) void {
    const byte_index = index >> 3;
    const bit_index = @truncate(u3, index & 0b111);
    if (bit_index == 0) {
        slice[byte_index] = value;
    } else {
        slice[byte_index] = slice[byte_index] | (value << bit_index);
    }
}

fn encode_copy(slice: []u8, dest: usize, source: usize) void {
    const source_byte = source >> 3;
    const source_bit = @truncate(u3, source & 0b111);

    const dest_byte = dest >> 3;
    const dest_bit = @truncate(u3, dest & 0b111);

    const bit: u8 = if ((slice[source_byte] & (@as(u8, 1) << source_bit)) == 0) 0 else 1;

    if (dest_bit == 0) {
        slice[dest_byte] = bit;
    } else {
        slice[dest_byte] = slice[dest_byte] | (bit << dest_bit);
    }
}

// TESTS

const seeds = [_]comptime_int{ 2, 3, 4, 5 };

fn relatively_prime(a: usize, b: usize) bool {
    return ((b % a) != 0);
}

test "the generation of a byte table is correct" {
    inline for (seeds) |seed| {
        const T = Wheel(seed, .byte);
        for (T.template) |v, index| {
            var this = 2 * index + 1;
            var maybe_prime = true;

            // note that our table should NOT show the primes themselves to be flagged
            // because in "higher generations" of the recurring sequence we want them
            // to not be set.  The prime numbers in the wheel themselves should be set
            // manually during initialization.
            inline for (T.primes) |prime| {
                maybe_prime = maybe_prime and relatively_prime(prime, this);
            }

            if (v == 1) {
                std.debug.assert(maybe_prime);
            } else {
                std.debug.assert(!maybe_prime);
            }
        }
    }
}

test "the generation of a bit table is correct" {
    inline for (seeds) |seed| {
        const T = Wheel(seed, .bit);
        for (T.template) |v, index| {
            var subindex: usize = 0;
            while (subindex < 8) : (subindex += 1) {
                var this = 2 * ((index << 3) + subindex) + 1;
                var maybe_prime = true;
                inline for (T.primes) |prime| {
                    maybe_prime = maybe_prime and relatively_prime(prime, this);
                }
                if ((v & (@as(u8, 1) << @truncate(u3, subindex))) != 0) {
                    std.debug.assert(maybe_prime);
                } else {
                    std.debug.assert(!maybe_prime);
                }
            }
        }
    }
}

test "compression function works" {
    var uncompressed: [3]u8 = .{ 1, 0, 1 };
    var compressed: [3]u8 = .{ 0b01_101_101, 0b1_101_101_1, 0b101_101_10 };
    compress(uncompressed[0..]);
    try std.testing.expectEqual(compressed, uncompressed);
}
