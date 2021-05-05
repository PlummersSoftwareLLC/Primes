import times
import math
import strformat

type PrimeSieve = ref object of RootObj
    sieveSize: uint64
    bits: seq[bool]

method runSieve(this: PrimeSieve) {.base.} =
    var factor: uint64 = 3
    let q = uint64(math.sqrt(float64(this.sieveSize)))

    while factor <= q:
        for num in countup(factor, this.sieveSize, 2):
            if not this.bits[num]:
                factor = num
                break

        var num2 = factor * factor
        while num2 < this.sieveSize:
            this.bits[num2] = true
            num2 += (factor * 2)

        factor += 2

method countPrimes(this: PrimeSieve): uint64 {.base.} =
    var count: uint64 = 1

    for num in countup(uint64(3), this.sieveSize, 2):
        if not this.bits[num]:
            count += 1

    return count

method printResults(this: PrimeSieve, showResults: bool, duration: float64, passes: int) {.base.} =
    if showResults:
        stdout.write("2, ")

    var count: uint64 = 1
    for num in countup(uint64(3), this.sieveSize, 2):
        if not this.bits[num]:
            if showResults:
                stdout.write(&"{num}")
            count += 1

    if showResults:
        echo ""

    let
        countPrimes = this.countPrimes()
        isValid = (countPrimes == 78498)
    stderr.writeLine(&"Passes: {passes}, Time: {duration}, Avg: {(duration / float64(passes))}, Limit: {this.sieveSize}, Count1: {count}, Count2: {countPrimes}, Valid: {isValid}")
    echo &"marghidanu;{passes};{duration};1"

proc newSieve(sieveSize: uint64): PrimeSieve =
    var bits = newSeq[bool](sieveSize)
    PrimeSieve(sieveSize: sieveSize, bits: bits)

# --- Main block
var passes = 0
let startTime = times.epochTime()

while true:
    var sieve = newSieve(1_000_000)
    sieve.runSieve()

    passes += 1
    var duration = times.epochTime() - startTime
    if duration >= 5:
        sieve.printResults(false, duration, passes)
        break

