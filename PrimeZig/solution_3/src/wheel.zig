//! implementation of the ability to pregenerate composites at compile-time
//! and use them. should generate into the compiled artifact, which is a
//! span of memory

const std = @import("std");
const Alloc = @import("alloc.zig").ComptimeAlloc;
const IntSieve = @import("sieves.zig").IntSieve;
const BitSieve = @import("sieves.zig").BitSieve;

const Oeis = struct {
    const primes = [_]usize{ 3, 5, 7, 11, 13, 17 };
    const products = [_]usize{ 3, 15, 105, 1155, 15105, 255255};
};

const WheelOpts = struct {
    num_primes: u8,
    primeval: anytype = 0,
    SieveFn: anytype = IntSieve,
};

pub fn Wheel(comptime opts: WheelOpts) type {
    const T = if (@TypeOf(opts.primeval) == bool) bool else u8;
    const src_bytes = Oeis.products[opts.num_primes - 1];
    const alloc_aligned = Alloc(opts, src_bytes).alloc_aligned;
    const field_count = if (opts.SieveFn == IntSieve) src_bytes else src_bytes * 8;
    const COMPOSITE: T = if (T == bool) !opts.primeval else 1 - opts.primeval;

    return struct {
        /// rolls the wheel out onto the field.
        pub fn roll(field: [*]T, field_bytes: usize) void {
            const src = comptime makeTemplate();
            var segment_end = src_bytes;
            var chunk = field;
            while (segment_end < field_bytes) : (segment_end += src_bytes) {
                @memcpy(chunk, src.*, src_bytes);
                chunk += src_bytes;
            } else {
                @memcpy(chunk, src.*, src_bytes - (segment_end - field_bytes));
            }
            // when you're done, put
            inline for (Oeis.primes[0..opts.num_primes]) |prime| {
                put(field, prime, opts.primeval);
            }
        }

        fn makeTemplate() *[src_bytes]T {
            var template_buffer = alloc_aligned(T, src_bytes);

            // initialize everything to be prime
            for (template_buffer.*[0..src_bytes]) |*item| { item.* = opts.primeval; }

            // set up a bog-standard sieve.
            var sieve: opts.SieveFn(.{.primeval = opts.primeval}) =
              .{.field = @ptrCast([*]T, template_buffer), .field_count = field_count};

            inline for (Oeis.primes[0..opts.num_primes]) | prime | {
                sieve.runFactor(prime);
                put(template_buffer, prime, COMPOSITE);
            }

            return @ptrCast(*[src_bytes]T, template_buffer);
        }

        inline fn put(template_buffer: *[src_bytes]T, comptime index: usize, comptime value: anytype) void {
            if (opts.SieveFn == IntSieve) {
                template_buffer.*[index / 2] = value;
            } else {
                const position = index / 2;
                const byte_index = position / 8;
                const mask = @as(u8, 1) << @intCast(u3, position % 8);
                if (value == 0) {
                    template_buffer.*[byte_index] &= ~mask;
                } else {
                    template_buffer.*[byte_index] |= mask;
                }
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

test "the generation of a bool table is correct" {
    inline for (wheel_sizes) |num_primes| {
        var template = Wheel(.{.num_primes = num_primes, .primeval = false}).makeTemplate();

        for (template) | byte, index | {
            const this = 2 * index + 1;
            var composite = false;
            for (Oeis.primes[0..num_primes]) |prime| {
                composite = composite or (this % prime == 0);
            }
            try std.testing.expectEqual(composite, byte);
        }
    }
}

test "the generation of an inverted byte table is correct" {
    inline for (wheel_sizes) |num_primes| {
        var template = Wheel(.{.num_primes = num_primes, .primeval = 1}).makeTemplate();

        for (template) | byte, index | {
            const this = 2 * index + 1;
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
        var template = Wheel(.{.num_primes = num_primes, .SieveFn = BitSieve}).makeTemplate();

        for (template) | byte, byte_index | {
            for (range(8)) | _, bit_index | {
                const expanded_index = byte_index * 8 + bit_index;
                const this = 2 * expanded_index + 1;

                const bit = (byte >> @intCast(u3, bit_index)) & 0x01;

                var composite = false;
                for (Oeis.primes[0..num_primes]) |prime| {
                    composite = composite or (this % prime == 0);
                }
                try std.testing.expectEqual(composite, bit == 1);
            }
        }
    }
}

// one wierd hack
fn range(len: usize) []const u0 {
    return @as([*]u0, undefined)[0..len];
}