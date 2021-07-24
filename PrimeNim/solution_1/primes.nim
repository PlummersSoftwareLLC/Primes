import times
import math
import strformat
import tables

const DICT = {
    10'u64: 4,
    100'u64: 25,
    1000'u64: 168,
    10000'u64: 1229,
    100000'u64: 9592,
    1000000'u64: 78498,
    10000000'u64: 664579,
    100000000'u64: 5761455,
    1000000000'u64: 50847534,
    10000000000'u64: 455052511,
}.toTable()

type PrimeSieve = ref object of RootObj
    sieveSize: uint64
    bits: seq[bool]

method runSieve(this: PrimeSieve) {.base.} =
    var factor = 3'u64
    let q = uint64(math.sqrt(float64(this.sieveSize)))

    while factor <= q:
        for num in countup(factor, this.sieveSize, 2'u64):
            if not this.bits[num]:
                factor = num
                break

        var num2 = factor * factor
        while num2 < this.sieveSize:
            this.bits[num2] = true
            num2 += (factor * 2'u64)

        factor += 2'u64

method countPrimes(this: PrimeSieve): uint64 {.base.} =
    var count = 1'u64

    for num in countup(3'u64, this.sieveSize, 2'u64):
        if not this.bits[num]:
            count += 1'u64

    return count

method printResults(this: PrimeSieve, showResults: bool, duration: float64, passes: int) {.base.} =
    if showResults:
        stdout.write("2, ")

    var count = 1'u64
    for num in countup(3'u64, this.sieveSize, 2'u64):
        if not this.bits[num]:
            if showResults:
                stdout.write(&"{num}")
            count += 1'u64

    if showResults:
        echo ""

    let
        countPrimes = this.countPrimes()
        isValid = (countPrimes == uint64(DICT[this.sieveSize]))

    stderr.writeLine(&"Passes: {passes}, Time: {duration}, Avg: {(duration / float64(passes))}, Limit: {this.sieveSize}, Count1: {count}, Count2: {countPrimes}, Valid: {isValid}")
    echo &"marghidanu;{passes};{duration};1;algorithm=base,faithful=yes,bits=8"

proc newPrimeSieve(sieveSize: uint64): PrimeSieve =
    PrimeSieve(sieveSize: sieveSize, bits: newSeq[bool](sieveSize))

# --- Main block
var passes = 0
let startTime = times.epochTime()

while true:
    var sieve = newPrimeSieve(1000000'u64)
    sieve.runSieve()

    passes += 1
    var duration = times.epochTime() - startTime
    if duration >= 5:
        sieve.printResults(false, duration, passes)
        break

