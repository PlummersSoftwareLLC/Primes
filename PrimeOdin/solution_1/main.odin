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

Sieve :: struct {
    upper: u64,
    bits: [dynamic]bool,
}

run_sieve :: proc(upper: u64) -> (sieve: Sieve) {
    sieve.upper = upper;
    sieve.bits = make([dynamic]bool, upper, upper);
    factor : u64 = 3;
    q := u64(math.sqrt(f64(upper)));

    // step by 2 to skip evens
    #no_bounds_check for ; factor <= q; factor += 2 {
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

count_primes :: proc(using sieve: ^Sieve) -> (count: u64) {
    count = u64(upper >= 2);
    #no_bounds_check for i := 3; i < len(bits); i += 2 {
        if !bits[i] {
            count += 1;
        }
    }
    return count;
}

main :: proc() {
    size :: 1_000_000;

    start_time := time.now();

    passes: u32 = 0;
    for time.duration_seconds(time.diff(start_time, time.now())) < 5 {
        sieve := run_sieve(size);
        primes := count_primes(&sieve);
        //assert(primes == expected(size));
        passes += 1;
    }

    total := time.duration_seconds(time.diff(start_time, time.now()));
    fmt.printf("odin_moe;%v;%v1;algorithm=base;faithful=yes;bits=1", passes, total);
}

