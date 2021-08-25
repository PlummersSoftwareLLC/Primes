//! implementation of the ability to pregenerate composites at compile-time
//! and use them. should generate into the compiled artifact, which is a
//! span of memory

const std = @import("std");
const Alloc = @import("alloc.zig").ComptimeAlloc;
const IntSieve = @import("sieves.zig").IntSieve;

const Oeis = struct {
    const primes = [_]usize{ 3, 5, 7, 11, 13, 17 };
    const products = [_]usize{ 3, 15, 105, 1155, 15105, 255255};
};

const WheelOpts = struct {
    num_primes: u8,
    primeval: anytype = 0,
};

pub fn Wheel(comptime opts: WheelOpts) type {
    const T = if (@TypeOf(opts.primeval) == bool) bool else u8;
    const src_bytes = Oeis.products[opts.num_primes - 1];
    const alloc_aligned = Alloc(opts, src_bytes).alloc_aligned;

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
        }

        fn makeTemplate() *[src_bytes]T {
            var template_buffer = alloc_aligned(T, src_bytes);

            // initialize everything to be prime
            for (template_buffer.*[0..src_bytes]) |*item| { item.* = opts.primeval; }

            // set up a bog-standard sieve.
            var sieve: IntSieve(.{.primeval = opts.primeval}) =
              .{.field = @ptrCast([*]T, template_buffer), .field_count = src_bytes};

            for (Oeis.primes[0..opts.num_primes]) | prime | {
                sieve.runFactor(prime);
                put(template_buffer, prime);
            }

            return @ptrCast(*[src_bytes]T, template_buffer);
        }

        fn put(template_buffer: *[src_bytes]T, index: usize) void {
            template_buffer.*[index / 2] = if (T == bool) !opts.primeval else (1 - opts.primeval);
        }
    };
}

// TESTS

const wheel_sizes = [_]usize{ 2, 3, 4, 5, 6 };

test "the generation of a byte table is correct" {
    inline for (wheel_sizes) |num_primes| {
        var template = Wheel(.{.num_primes = num_primes}).makeTemplate();

        for (template) | state, index | {
            const this = 2 * index + 1;

            // note that our table should NOT show the primes themselves to be flagged
            // because in "higher generations" of the recurring sequence we want them
            // to not be set.  The prime numbers in the wheel themselves should be set
            // manually during initialization.
            var composite = false;
            for (Oeis.primes[0..num_primes]) |prime| {
                composite = composite or (this % prime == 0);
            }
            try std.testing.expectEqual(composite, state == 1);
        }
    }
}

test "the generation of a bool table is correct" {
    inline for (wheel_sizes) |num_primes| {
        var template = Wheel(.{.num_primes = num_primes, .primeval = false}).makeTemplate();

        for (template) | state, index | {
            const this = 2 * index + 1;

            // note that our table should NOT show the primes themselves to be flagged
            // because in "higher generations" of the recurring sequence we want them
            // to not be set.  The prime numbers in the wheel themselves should be set
            // manually during initialization.
            var composite = false;
            for (Oeis.primes[0..num_primes]) |prime| {
                composite = composite or (this % prime == 0);
            }
            try std.testing.expectEqual(composite, state);
        }
    }
}

test "the generation of an inverted byte table is correct" {
    inline for (wheel_sizes) |num_primes| {
        var template = Wheel(.{.num_primes = num_primes, .primeval = 1}).makeTemplate();

        for (template) | state, index | {
            const this = 2 * index + 1;

            // note that our table should NOT show the primes themselves to be flagged
            // because in "higher generations" of the recurring sequence we want them
            // to not be set.  The prime numbers in the wheel themselves should be set
            // manually during initialization.
            var composite = false;
            for (Oeis.primes[0..num_primes]) |prime| {
                composite = composite or (this % prime == 0);
            }
            try std.testing.expectEqual(composite, state == 0);
        }
    }
}

//test "the generation of a bit table is correct" {
//    inline for (seeds) |seed| {
//        const T = Wheel(seed, .bit);
//        for (T.template) |v, index| {
//            var subindex: usize = 0;
//            while (subindex < 8) : (subindex += 1) {
//                var this = 2 * ((index << 3) + subindex) + 1;
//                var maybe_prime = true;
//                inline for (T.primes) |prime| {
//                    maybe_prime = maybe_prime and relatively_prime(prime, this);
//                }
//                if ((v & (@as(u8, 1) << @truncate(u3, subindex))) != 0) {
//                    std.debug.assert(maybe_prime);
//                } else {
//                    std.debug.assert(!maybe_prime);
//                }
//            }
//        }
//    }
//}
