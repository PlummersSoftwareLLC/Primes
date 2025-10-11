
package prime_drag_race

import "core:math"

sieveSize :: 1_000_000

// calculate q once and for all

main :: proc()
{
    GoByteSieve( sieveSize)
    GoBitSieve( sieveSize)
}