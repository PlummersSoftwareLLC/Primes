package prime_drag_race

import "core:container/bit_array"
import "core:math"
import "core:fmt"
import "core:time"
import "core:mem"



RunBitSieve :: proc( bitSieve :^bit_array.Bit_Array)
{
    sieveSize := bitSieve.length

    factor := 3
    
    for factor <= q {
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
}

GoBitSieve :: proc ()
{
    fiveSecs :: time.Duration(5_000_000_000)   // nano seconds

    theSieve := bit_array.create( sieveSize)
    defer bit_array.destroy( theSieve)

    passCount := 0

    timer : time.Stopwatch
    time.stopwatch_start( &timer)
    defer time.stopwatch_stop( &timer)

    for  {
        bit_array.clear( theSieve)
        RunBitSieve( theSieve)
        passCount += 1

        duration := time.stopwatch_duration( timer)
        if duration > fiveSecs {
            fmt.printfln( "arenol;%d;%.4f;1;algorithm=base,faithful=yes,bits=1", passCount, f64(duration) * 1.0e-9)
            break
        }
    }

}
