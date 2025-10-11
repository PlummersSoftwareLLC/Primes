package prime_drag_race

import "core:fmt"
import "core:time"
import "core:mem"


Sieve :: [sieveSize/2]bool

RunByteSieve :: proc( theSive :^Sieve)
{
    upper := len(theSive)

    for factor := 3; factor <= q; factor +=2 {
        #no_bounds_check {      // speeds up the algorithm with a factor of 2.5
            
            // find the first "confirmed" prime in the Sieve
            for num := factor/2; num < upper; num += 1 {
                if !theSive[num] {
                    factor = num * 2 + 1
                    break;
                }
            }

            // then "nullify" the subsequent multiples of that prime.
            for num := factor * factor / 2; num < upper; num += factor {
                theSive[num] = true
            }
            
        }
    }
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


GoByteSieve :: proc()
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
        theSieve := new( Sieve)
        RunByteSieve( theSieve)
        passCount += 1

        duration := time.stopwatch_duration( timer)
        if duration > fiveSecs {
            assert( CountPrimes( theSieve[:]) == ExpectedPrimeCount( len(theSieve)))
            fmt.printfln( "arenol;%d,%.4f;1;algorithm=base,faithful=yes,bits=8", passCount, f64(duration) * 1.0e-9)
            // PrintPrimes( theSieve[:])
            free( theSieve)
            break
        }
        free( theSieve)
    
    }


}