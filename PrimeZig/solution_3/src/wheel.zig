//! implementation of the ability to pregenerate composites at compile-time
//! and use them. should generate into the compiled artifact, which is a
//! span of memory

const std = @import("std");
const Alloc = @import("alloc.zig").ComptimeAlloc;

const OEIS_PRIMES = struct {
    const values = [_]usize{ 3, 5, 7, 11, 13, 17 };
    const products = [_]usize{ 3, 15, 105, 1155, 15105, 255255};
};

const WheelOpts = struct {
    num_primes: u8,
};

pub fn Wheel(opts: WheelOpts) type {
    const src_bytes = OEIS_PRIMES.products[opts.num_primes];
    const alloc_aligned = Alloc(src_bytes).alloc_aligned;

    return struct {
        /// rolls the wheel out onto the field.
        pub fn roll(field: [*]u8, field_bytes: usize) void {
            const src = comptime makeTemplate();
            var segment_end = src_bytes;
            var chunk = field;
            while (segment_end < field_bytes) : (segment_end += src_bytes) {
                @memcpy(chunk, src.*, src_bytes);
                chunk += src_bytes;
            } else {
                @memcpy(chunk, src.*, src_bytes - (segment_end - field_bytes));
            }
        }

        fn makeTemplate() *[src_bytes]u8 {
            var template_buffer = alloc_aligned(src_bytes);
            for (buffer) |*item| { item.* = 0; }
            return template_buffer;
        }
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
