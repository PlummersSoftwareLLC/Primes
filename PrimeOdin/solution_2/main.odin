
package prime_drag_race

import "core:math"

sieveSize :: 1_000_000

// calculate q once and for all
q := int( math.sqrt_f64( f64(sieveSize)))


main :: proc()
{
    GoByteSieve()
    GoBitSieve()
}