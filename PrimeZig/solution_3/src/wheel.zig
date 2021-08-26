//! implementation of the ability to pregenerate composites at compile-time
//! and use them. should generate into the compiled artifact, which is a
//! span of memory

const std = @import("std");
const comptimeAlloc = @import("alloc.zig").comptimeAlloc;
const IntSieve = @import("sieves.zig").IntSieve;
const BitSieve = @import("sieves.zig").BitSieve;

const Oeis = struct {
    const primes = [_]usize{ 3, 5, 7, 11, 13, 17, 19 };
    const products = [_]usize{ 3, 15, 105, 1155, 15105, 255255};
};

const WheelOpts = struct {
    num_primes: u8,
    PRIME: u8 = 0,
    bits: bool = false
};

pub fn Wheel(comptime opts: WheelOpts) type {
    // exists so that we don't run out of compiler credits.  Zig compiler is stingier than AWS.
    @setEvalBranchQuota(100000);

    const T = @TypeOf(opts.PRIME);
    const src_bytes = Oeis.products[opts.num_primes - 1];
    const COMPOSITE: T = if (T == bool) !opts.PRIME else 1 - opts.PRIME;

    return struct {
        pub const STARTING_FACTOR: usize = Oeis.primes[opts.num_primes];
        pub const template: [src_bytes]T align(std.mem.page_size) = makeTemplate();
        pub const bytes = src_bytes;

        /// rolls the wheel out onto the field.
        pub fn roll(field: [*]T, field_bytes: usize) void {
            var segment_end = src_bytes;
            var chunk = field;
            while (segment_end < field_bytes) : (segment_end += src_bytes) {
                @memcpy(chunk, @ptrCast([*]const T, &template), src_bytes);
                chunk += src_bytes;
            } else {
                @memcpy(chunk, @ptrCast([*]const T, &template), src_bytes - (segment_end - field_bytes));
            }
            // when you're done, put the primes back.
            inline for (Oeis.primes[0..opts.num_primes]) |prime| {
                put(field, prime, opts.PRIME);
            }
        }

        fn makeTemplate() [src_bytes]T {
            @setEvalBranchQuota(1_000_000);

            var template_buffer: [src_bytes] T align(std.mem.page_size) = undefined;

            // initialize everything to be prime
            for (template_buffer) |*item| { item.* = opts.PRIME; }

            // set up a bog-standard sieve.
            var sieve: IntSieve(.{.PRIME = opts.PRIME}) =
              .{.field = &template_buffer, .field_count = src_bytes};

            inline for (Oeis.primes[0..opts.num_primes]) | prime | {
                sieve.runFactor(prime);
                put(@ptrCast([*]T, &template_buffer), prime, COMPOSITE);
            }

            if (opts.bits) {
                compressCopy(template_buffer[0..]);
            }

            return template_buffer;
        }

        inline fn put(template_buffer: [*]T, comptime index: usize, comptime value: anytype) void {
            if (opts.bits) {
                const position = index / 2;
                const byte_index = position / 8;
                const mask = @as(u8, 1) << @intCast(u3, position % 8);
                if (value == 0) {
                    template_buffer[byte_index] &= ~mask;
                } else {
                    template_buffer[byte_index] |= mask;
                }
            } else {
                template_buffer[index / 2] = value;
            }
        }
    };
}

// TESTS
const wheel_sizes = [_]usize{ 2, 3, 4, 5 }; // can't test 6 due to overflow.

test "the generation of a byte table is correct" {
    inline for (wheel_sizes) |num_primes| {
        var template = Wheel(.{.num_primes = num_primes}).makeTemplate();

        for (template) | byte, index | {
            const this = 2 * index + 1;

            // note that our table should NOT show the primes themselves to be flagged
            // because in "higher generations" of the recurring sequence we want them
            // to not be set.  The prime numbers in the wheel themselves should be set
            // as prime manually during initialization.
            var composite = false;
            for (Oeis.primes[0..num_primes]) |prime| {
                composite = composite or (this % prime == 0);
            }
            try std.testing.expectEqual(composite, byte == 1);
        }
    }
}

test "the generation of an inverted byte table is correct" {
    inline for (wheel_sizes) |num_primes| {
        var template = Wheel(.{.num_primes = num_primes, .PRIME = 1}).makeTemplate();

        for (template) | byte, index | {
            const this = 2 * index + 1;

            // note that our table should NOT show the primes themselves to be flagged
            // because in "higher generations" of the recurring sequence we want them
            // to not be set.  The prime numbers in the wheel themselves should be set
            // as prime manually during initialization.
            var composite = false;
            for (Oeis.primes[0..num_primes]) |prime| {
                composite = composite or (this % prime == 0);
            }
            try std.testing.expectEqual(composite, byte == 0);
        }
    }
}

// one wierd hack
fn range(len: usize) []const u0 {
    return @as([*]u0, undefined)[0..len];
}

// COMPRESSION UTILITIES:  turns a jagged (non-multiple of 8) list of bytes, repeats it 8 times,
// and results in bytes of bitmaps.

fn compressCopy(slice: []u8) void {
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

test "compression function works" {
    var uncompressed: [3]u8 = .{ 1, 0, 1 };
    var compressed: [3]u8 = .{ 0b01_101_101, 0b1_101_101_1, 0b101_101_10 };
    compressCopy(uncompressed[0..]);
    try std.testing.expectEqual(compressed, uncompressed);
}