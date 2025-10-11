package prime_drag_race

import "base:intrinsics"
import "core:container/bit_array"
import "core:math"
import "core:fmt"
import "core:time"
import "core:mem"

BitSieve :: struct {
    size : int,
    bits : ^bit_array.Bit_Array
}

RunBitSieve :: proc( sieveSize : int) -> (result :BitSieve)
{
    factor := 3
    
    bitSieve := bit_array.create( sieveSize)
    // q := int( math.sqrt_f64( f64(sieveSize)))

    for factor*factor <= sieveSize { // faster than "factor <= q"
        // find the first "confirmed" prime in the Sieve
        #no_bounds_check {
            for num := factor; num < sieveSize; num += 2 {
                if !bit_array.unsafe_get( bitSieve, num) {
                    factor = num
                    break;
                }
            }

            // then "nullify" the subsequent multiples of that prime.
            step := factor * 2
            for num := factor * factor; num < sieveSize; num += step {
                bit_array.unsafe_set( bitSieve, num)
            }
            
            factor += 2
        }
    }
    result.size = sieveSize
    result.bits = bitSieve

    return
}

GoBitSieve :: proc ( sieveSize : int)
{
    fiveSecs :: time.Duration(5_000_000_000)   // nano seconds

    passCount := 0

    timer : time.Stopwatch
    time.stopwatch_start( &timer)
    defer time.stopwatch_stop( &timer)

    for  {
        bitSieve := RunBitSieve( sieveSize)
        passCount += 1

        duration := time.stopwatch_duration( timer)
        if duration > fiveSecs {
            fmt.printfln( "arenol;%d;%.5f;1;algorithm=base,faithful=yes,bits=1", passCount, f64(duration) * 1.0e-9)
            bit_array.destroy( bitSieve.bits)
            break
        }
        bit_array.destroy( bitSieve.bits)
    }

}
