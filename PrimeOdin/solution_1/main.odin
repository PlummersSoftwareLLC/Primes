// ported to Odin based on Dave's Garage Prime Sieve
package main

import "core:fmt"
import "core:time"
import "core:math"
import bt "core:math/bits"

expected :: proc(upper: u64) -> u64 {
    switch upper {
    case 10:            return 4;
    case 100:           return 25;
    case 1_000:         return 168;
    case 10_000:        return 1229;
    case 100_000:       return 9592;
    case 1_000_000:     return 78498;
    case 10_000_000:    return 664579;
    case 100_000_000:   return 5761455;
    }
    panic("Unreachable");
}

SIEVE_BACKING :: u32;
SIEVE_BACKING_BITS :: size_of(SIEVE_BACKING) * 8;
BitSieve :: struct {
    upper: u64,
    bits: []SIEVE_BACKING,
}

make_bits :: proc(using sieve: ^BitSieve, size: u64) {
    bits = make([]SIEVE_BACKING, math.ceil(f32(size / SIEVE_BACKING_BITS)), context.temp_allocator);
}

get_bit :: proc(using sieve: ^BitSieve, index: u64) -> bool #no_bounds_check {
    backing := bits[index / SIEVE_BACKING_BITS];
    masked := backing & (1 << (index & (SIEVE_BACKING_BITS - 1)));
    return masked != 0;
}

set_bit :: proc(using sieve: ^BitSieve, index: u64, value: SIEVE_BACKING) #no_bounds_check {
    // stdlib version (bits.bitfield_insert) hardcoded to our case
    backing := &bits[index / SIEVE_BACKING_BITS];
    // index % SIEVE_BACKING_BITS
    offset := (index & (SIEVE_BACKING_BITS - 1));
    // clear the bit we want to set to value in backing
    // then OR with the value shifted in the correct place
    backing^ = (backing^ &~ (1<<offset)) | (value<<offset);
}

// adapted from https://github.com/PlummersSoftwareLLC/Primes/tree/drag-race/PrimeRust/solution_1
// by mike-barber
set_bits :: #force_inline proc(using sieve: ^BitSieve, start: u64, skip: u64) #no_bounds_check {
    bit_idx := start & (SIEVE_BACKING_BITS - 1);
    rolling_mask: SIEVE_BACKING = 1 << bit_idx;
    for i := start; i < sieve.upper; i += skip {
        bits[i / SIEVE_BACKING_BITS] |= rolling_mask;
        rolling_mask = bt.rotate_left32(rolling_mask, int(skip));
    }
}

run_sieve_bit :: proc(upper: u64) -> (sieve: BitSieve) #no_bounds_check {
    sieve.upper = upper;
    make_bits(&sieve, upper);

    factor := u64(3);
    q := u64(math.sqrt(f64(upper)));

    // step by 2 to skip evens
    for ; factor <= q; factor += 2 {
        for num := factor; num < upper; num += 2 {
            if !get_bit(&sieve, num) {
                factor = num;
                break;
            }
        }

        // every second multiple can be skipped since it will be even
        for num := factor * factor; num < upper; num += factor * 2 {
            // mark non-prime
            set_bit(&sieve, num, 1);
        }
        // above is actually faster
        /* set_bits(&sieve, factor * factor, factor * 2); */
    }

    return sieve;
}

count_primes_bit :: proc(using sieve: ^BitSieve) -> (count: u64) #no_bounds_check {
    count = 1 if upper >= 2 else 0;
    for i := u64(3); i < upper; i += 2 {
        if !get_bit(sieve, i) {
            count += 1;
        }
    }
    return count;
}

ByteSieve :: struct {
    upper: u64,
    bits: []bool,
}

run_sieve_byte :: proc(upper: u64) -> (sieve: ByteSieve) #no_bounds_check {
    sieve.upper = upper;
    sieve.bits = make([]bool, upper, context.temp_allocator);
    factor := u64(3);
    q := u64(math.sqrt(f64(upper)));

    // step by 2 to skip evens
    for ; factor <= q; factor += 2 {
        for num := factor; num < upper; num += 2 {
            if !sieve.bits[num] {
                factor = num;
                break;
            }
        }

        // every second multiple can be skipped since it will be even
        for num := factor * factor; num < upper; num += factor * 2 {
            // mark non-prime
            sieve.bits[num] = true;
        }
    }

    return sieve;
}

count_primes_byte :: proc(using sieve: ^ByteSieve) -> (count: u64) #no_bounds_check {
    count = 1 if upper >= 2 else 0;
    for i := 3; i < len(bits); i += 2 {
        if !bits[i] {
            count += 1;
        }
    }
    return count;
}

// explicit procedure overloading
count_primes :: proc{ count_primes_bit, count_primes_byte };

measure_run :: proc(size: u64, run_func: $T, bits: u8) {
    start_time := time.tick_now();

    passes: u32 = 0;
    for time.tick_since(start_time) < 5*time.Second {
        sieve := run_func(size);
        primes := count_primes(&sieve);
        /* expected := expected(size); */
        /* fmt.println("Expected:", expected, "Primes:", primes); */
        /* assert(primes == expected); */
        passes += 1;
    }

    total := time.duration_seconds(time.tick_since(start_time));
    impl_name := "bit" if bits == 1 else "byte";
    fmt.printf("\nodin_%s_moe;%v;%v;1;algorithm=base,faithful=yes,bits=%v", impl_name, passes, total, bits);
}

main :: proc() {
    size :: 1_000_000;
    measure_run(size, run_sieve_bit, 1);
    measure_run(size, run_sieve_byte, 8);
}
