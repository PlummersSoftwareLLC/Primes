// ported to Odin based on Dave's Garage Prime Sieve
package main

import "core:fmt"
import "core:time"
import "core:math"

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
    // we don't store evens
    bits = make([]SIEVE_BACKING, math.ceil(f32(size / SIEVE_BACKING_BITS / 2)), context.temp_allocator);
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

run_sieve_bit :: proc(upper: u64) -> (sieve: BitSieve) #no_bounds_check {
    sieve.upper = upper;
    make_bits(&sieve, upper);

    factor := u64(3);
    q := u64(math.sqrt(f64(upper)));

    // step by 2 to skip evens
    for ; factor <= q; factor += 2 {
        // step by 1 and start/end at half, since we don't store evens
        for num := factor / 2; num < upper / 2; num += 1 {
            if !get_bit(&sieve, num) {
                factor = num * 2 + 1;
                break;
            }
        }

        // every second multiple can be skipped since it will be even
        // step by 1*factor and start/end at half the original values, since we don't
        // store evens anymore
        for num := factor * factor / 2; num < upper / 2; num += factor {
            // mark non-prime
            set_bit(&sieve, num, 1);
        }
    }

    return sieve;
}

count_primes_bit :: proc(using sieve: ^BitSieve) -> (count: u64) #no_bounds_check {
    count = 1 if upper >= 2 else 0;
    // step by 1 and start/end at half, since we don't store evens
    for i := u64(3) / 2; i < upper / 2; i += 1 {
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
    sieve.bits = make([]bool, upper / 2, context.temp_allocator);
    factor := u64(3);
    q := u64(math.sqrt(f64(upper)));

    // step by 2 to skip evens
    for ; factor <= q; factor += 2 {
        // step by 1 and start/end at half, since we don't store evens
        for num := factor / 2; num < upper / 2; num += 1 {
            if !sieve.bits[num] {
                factor = num * 2 + 1;
                break;
            }
        }

        // every second multiple can be skipped since it will be even
        // step by 1*factor and start/end at half the original values, since we don't
        // store evens anymore
        for num := factor * factor / 2; num < upper / 2; num += factor {
            // mark non-prime
            sieve.bits[num] = true;
        }
    }

    return sieve;
}

count_primes_byte :: proc(using sieve: ^ByteSieve) -> (count: u64) #no_bounds_check {
    count = 1 if upper >= 2 else 0;
    // step by 1 and start/end at half, since we don't store evens
    for i := u64(3) / 2; i < upper / 2; i += 1 {
        if !bits[i] {
            count += 1;
        }
    }
    return count;
}

// explicit procedure overloading
count_primes :: proc{ count_primes_bit, count_primes_byte };

measure_run :: proc(size: u64, run_func: $T) -> (passes: u32, total_time: f64) {
    start_time := time.tick_now();

    primes: u64;
    for time.tick_since(start_time) < 5*time.Second {
        sieve := run_func(size);
        primes = count_primes(&sieve);
        passes += 1;
    }
    total_time = time.duration_seconds(time.tick_since(start_time));

    /* expected := expected(size); */
    /* fmt.println("Expected:", expected, "Primes:", primes); */
    /* assert(primes == expected); */

    return passes, total_time;
}

main :: proc() {
    size :: 1_000_000;
    passes, total := measure_run(size, run_sieve_bit);
    fmt.printf("\nodin_bit_moe;%v;%v;1;algorithm=base,faithful=yes,bits=1", passes, total);
    passes, total = measure_run(size, run_sieve_byte);
    fmt.printf("\nodin_byte_moe;%v;%v;1;algorithm=base,faithful=yes,bits=8", passes, total);
}
