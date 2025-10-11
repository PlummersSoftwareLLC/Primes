package prime_drag_race

import "core:fmt"
import "core:time"
import "core:mem"


ByteSieve :: struct {
    size : int,
    bits : []bool,
}

RunByteSieve :: proc( size :int) -> (result :ByteSieve)
{
    upper := size / 2;
    theSieve : []bool = make( []bool, upper)

    for factor := 3; factor*factor < size; factor +=2 {
        #no_bounds_check {      // speeds up the algorithm with a factor of 2.5
            // find the first "confirmed" prime in the Sieve
            for num := factor/2; num < upper; num += 1 {
                if !theSieve[num] {
                    factor = num * 2 + 1
                    break;
                }
            }

            // then "nullify" the subsequent multiples of that prime.
            for num := factor * factor / 2; num < upper; num += factor {
                theSieve[num] = true
            }
            
        }
    }
    result.size = size 
    result.bits = theSieve
    return
}

PrintPrimes :: proc( calculatedSieve : []bool )
{
    fmt.print( "2")

    for b, i in calculatedSieve[1:] {
        if !b {
            fmt.print( ",", i*2+3)
        }
    }

    fmt.println( "")
}

CountPrimes :: proc( calculatedSieve :[]bool) -> int
{
    n  := len( calculatedSieve)
    count := sieveSize >= 2 ? 1 : 0
    for b in calculatedSieve[1:] {
        if !b {
            count += 1
        }
    }
    return count
}

ExpectedPrimeCount :: proc( siveSize :int) -> int{
    switch sieveSize {
        case 10:            return 4
        case 100:           return 25
        case 1_000:         return 168
        case 10_000:        return 1_229
        case 100_000:       return 9_592
        case 1_000_000:     return 78_498
        case 10_000_000:    return 664_579
        case 100_000_000:   return 5_761_455
    }
    panic("Invalid sieve size")
}


GoByteSieve :: proc(  sieveSize : int)
{

    fiveSecs :: time.Duration(5_000_000_000)   // nano seconds
   
    passCount := 0

    /* 
        note that the default value of bool in odin is false.
        rather than initialize the sieve with trues and make them
        false, as we go, we reverse the logic.
    */


    timer : time.Stopwatch
    time.stopwatch_start( &timer)
    defer time.stopwatch_stop( &timer)

    for  {
        theSieve := RunByteSieve( sieveSize)
        passCount += 1

        duration := time.stopwatch_duration( timer)
        if duration > fiveSecs {
            assert( CountPrimes( theSieve.bits[:]) == ExpectedPrimeCount( theSieve.size))
            fmt.printfln( "arenol;%d,%.5f;1;algorithm=base,faithful=yes,bits=8", passCount, f64(duration) * 1.0e-9)
            // PrintPrimes( theSieve.bits[:])
            free( (^rawptr)(&theSieve.bits)^)
            break
        }
        free( (^rawptr)(&theSieve.bits)^)
    }


}