//! implementation of the ability to pregenerate composites at compile-time
//! and use them. should generate into the compiled artifact, which is a
//! span of memory

const std = @import("std");
const comptimeAlloc = @import("alloc.zig").comptimeAlloc;
const IntSieve = @import("sieves.zig").IntSieve;
const BitSieve = @import("sieves.zig").BitSieve;

const Oeis = struct {
    const primes = [_]usize{ 3, 5, 7, 11, 13, 17, 19 };
    const prime_products = [_]usize{ 3, 15, 105, 1155, 15015, 255255};
};

const WheelOpts = struct {
    num_primes: u8,
    PRIME: u8 = 0,
    bits: bool = false,
    copy_vector: usize = 0,
};

const skip_counts = .{"", "", "8of30", "48of201", "480of2310", "5760of30030", "92160of510510"};

pub fn Wheel(comptime opts: WheelOpts) type {
    // exists so that we don't run out of compiler credits.  Zig compiler is stingier than AWS.
    @setEvalBranchQuota(100000);

    const T = @TypeOf(opts.PRIME);
    const wheel_bytes = Oeis.prime_products[opts.num_primes - 1];
    const COMPOSITE: T = if (T == bool) !opts.PRIME else 1 - opts.PRIME;

    std.debug.assert(opts.num_primes >= 2);

    return struct {
        pub const STARTING_FACTOR: usize = Oeis.primes[opts.num_primes];
        pub const template: [wheel_bytes]T align(256) = makeTemplate();
        pub const bytes = wheel_bytes;
        pub const name = skip_counts[opts.num_primes] ++ if (opts.copy_vector > 0) "v" else "";

        /// rolls the wheel out onto the field.
        pub fn roll(field: [*]T, field_bytes: usize) void {
            const field_end = @ptrToInt(field) + field_bytes - wheel_bytes;
            var chunk = field;
            while (@ptrToInt(chunk) < field_end) : (chunk += wheel_bytes) {
                if (opts.copy_vector > 0) {
                    simd_copy_fixed(chunk);
                } else {
                    @memcpy(chunk, @ptrCast([*]const T, &template), wheel_bytes);
                }
            } else {
                const leftovers = field_bytes - (@ptrToInt(chunk) - @ptrToInt(field));
                @memcpy(chunk, @ptrCast([*]const T, &template), leftovers);
            }
            // when you're done, put the primes back.
            inline for (Oeis.primes[0..opts.num_primes]) |prime| {
                put(field, prime, opts.PRIME, opts.bits);
            }
        }

        inline fn simd_copy_fixed(chunk: [*]T) void {
            const vector_size = opts.copy_vector;
            comptime var bytes_so_far = 0;
            const template_array = @ptrCast([*]const T, &template);

            inline while (bytes_so_far < wheel_bytes) : (bytes_so_far += opts.copy_vector) {
                const vector = std.math.min(vector_size, wheel_bytes - bytes_so_far);
                const V = std.meta.Vector(vector_size, u8);

                const src_ptr_t = *const V;
                const dst_ptr_t = *align(1) V;

                const src_ptr = @ptrCast(src_ptr_t, @alignCast(vector_size, template_array + bytes_so_far));
                const dst_ptr = @ptrCast(dst_ptr_t, chunk + bytes_so_far);
                dst_ptr.* = src_ptr.*;
            }
        }

        fn makeTemplate() [wheel_bytes]T {
            @setEvalBranchQuota(1_000_000);

            var template_buffer: [wheel_bytes] T align(std.mem.page_size) = undefined;

            // initialize everything to be prime
            for (template_buffer) |*item| { item.* = opts.PRIME; }

            // set up a bog-standard sieve.
            var sieve: IntSieve(.{.PRIME = opts.PRIME}) =
              .{.field = &template_buffer, .field_count = wheel_bytes};

            inline for (Oeis.primes[0..opts.num_primes]) | prime | {
                sieve.runFactor(prime);
                put(@ptrCast([*]T, &template_buffer), prime, COMPOSITE, false);
            }

            if (opts.bits) {
                compressCopy(template_buffer[0..]);
            }

            return template_buffer;
        }

        inline fn put(template_buffer: [*]T, comptime index: usize, comptime value: anytype, comptime use_bits: bool) void {
            if (use_bits) {
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
const wheel_sizes = [_]usize{ 2, 3, 4, 5, 6 };

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

test "the generation of a bit table is correct" {
    inline for (wheel_sizes) |num_primes| {
        var template = Wheel(.{.num_primes = num_primes, .bits = true}).makeTemplate();

        for (template) | byte, byte_index | {
            var bit_index: usize = 0;
            while (bit_index < 8) : (bit_index += 1) {
                const index = byte_index * 8 + bit_index;
                const this = 2 * index + 1;
                const bit = (byte >> @intCast(u3, bit_index)) & 0x01;

                var composite = false;
                for (Oeis.primes[0..num_primes]) |prime| {
                    composite = composite or (this % prime == 0);
                }

                if (composite != (bit == 1)) {
                    std.debug.print("\n{} should be: {} found: {}, ({})\n", .{this, composite, bit, num_primes});
                }

                try std.testing.expectEqual(composite, bit == 1);
            }
        }
    }
}

// COMPRESSION UTILITIES:  turns a jagged (non-multiple of 8) list of bytes, repeats it 8 times,
// and results in bytes of bitmaps.

fn compressCopy(slice: []u8) void {
    @setEvalBranchQuota(10_000_000);

    compress(slice);

    var index: usize = 1;
    while (index < 8) : (index += 1) {
        copyBits(slice, index);
    }
}

fn compress(slice: []u8) void {
    for (slice) |src, index| {
        const dst_byte = index / 8;
        const dst_bit = @intCast(u3, index % 8);
        const mask = ~(@as(u8, 1) << dst_bit);
        slice[dst_byte] = (slice[dst_byte] & mask) | (src << dst_bit);
    }
}

fn copyBits(slice: []u8, shift: usize) void {
    const bits = slice.len;
    var src_index: usize = 0;
    while (src_index < bits) : (src_index += 1) {
        const src_byte = src_index / 8;
        const src_bit = @intCast(u3, src_index % 8);
        const dst_index = src_index + shift * bits;
        const dst_byte = dst_index / 8;
        const dst_bit = @intCast(u3, dst_index % 8);
        const mask = ~(@as(u8, 1) << dst_bit);
        const src = (slice[src_byte] >> src_bit) & 0x01;
        slice[dst_byte] = (slice[dst_byte] & mask) | (src << dst_bit);
    }
}

test "compression function works in the small" {
    var uncompressed: [3]u8 = .{ 1, 0, 1 };
    var compressed: [3]u8 = .{ 0b01_101_101, 0b1_101_101_1, 0b101_101_10 };
    compressCopy(uncompressed[0..]);
    try std.testing.expectEqual(compressed, uncompressed);
}
