//! implementation of the ability to pregenerate primes and use them.
//! array should be a compile-time list of usize numbers which are
//! "skip" values.

const std = @import("std");
const OEIS_PRIMES = .{3, 5, 7, 11, 13, 17};
const IntSieve = @import("sieves.zig").IntSieve;

pub fn PreGenerated(comptime count: usize) type {
    var prods = std.mem.zeroes([OEIS_PRIMES.len]comptime usize);
    var source_primes_ = std.mem.zeroes([count]comptime_int);

    var prod: comptime_int = 1;
    for (OEIS_PRIMES) |prime, index| {
        prod *= prime;
        prods[index] = prod;
        if (index < count) { source_primes_[index] = prime; }
    }
    const max_prime = prods[count - 1];
    var field: [max_prime]comptime bool = undefined;

    // bind in a sieve.
    var generator = IntSieve(comptime bool, max_prime * 2){
        .field = &field,
        .allocator = std.heap.page_allocator, //just as a placeholder.  won't be used.
    };

    generator.reset();

    @setEvalBranchQuota(100000);

    // elucidate the pattern for all of the primes.
    for (OEIS_PRIMES) | prime, index | {
        if (index < count) {
            generator.runFactor(prime);
        }
    }

    // next, count all of the trues.
    const hops_size = generator.primeCount();
    var hops_: [hops_size]usize = undefined;

    var hops_index = 0;
    var last_index = 0;
    // convert from a list to hops.
    for (generator.field) |item, index| {
        if ((index != 0) and item) {
            hops_[hops_index] = index - last_index;
            hops_index += 1;
            last_index = index;
        }
    }
    //hops[hops_size - 1] = hops_size - last_index;

    return struct {
        const source_primes = source_primes_;
        const size = hops_size;
        const hops = hops_;
    };
}

const seeds = [_]comptime_int{2, 3, 4, 5};

fn relatively_prime(a: usize, b: usize) bool {
    return (a == b) or ((b % a) != 0);
}

test "the generation of our hops is generally correct" {
    inline for (seeds) |seed| {
        const T = PreGenerated(seed);
        var this: usize = 1;
        for (T.hops) |v| {
            this += 2 * v;
            var sound = true;
            inline for (T.source_primes) |prime| {
                sound = sound and relatively_prime(prime, this);
            }
            std.debug.assert(sound);
        }
    }
}