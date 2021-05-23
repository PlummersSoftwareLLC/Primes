import std/[times, math, strformat, tables]

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

type PrimeSieve[size: static uint64] =  array[size, bool]

proc runSieve(this: var PrimeSieve) =
    var factor = 3'u64
    let q = uint64(sqrt(float64(this.size)))

    while factor <= q:
        for num in countup(factor, this.size, 2'u64):
            if not this[num]:
                factor = num
                break

        var num2 = factor * factor
        while num2 < this.size:
            this[num2] = true
            num2 += (factor * 2'u64)

        factor += 2'u64

proc countPrimes(this: var PrimeSieve): uint64 =
    result = 1

    for num in countup(3'u64, this.size, 2'u64):
        if not this[num]:
          inc result

proc printResults(this: var PrimeSieve, showResults: bool, duration: float64, passes: int) =
    if showResults:
        stdout.write("2, ")

    var count = 1'u64
    for num in countup(3'u64, this.size, 2'u64):
        if not this[num]:
            if showResults:
                stdout.write(&"{num}")
            inc count

    if showResults:
        echo ""

    let
        countPrimes = this.countPrimes()
        isValid = (countPrimes == uint64(DICT[this.size]))

    stderr.writeLine(&"Passes: {passes}, Time: {duration}, Avg: {(duration / float64(passes))}, Limit: {this.size}, Count1: {count}, Count2: {countPrimes}, Valid: {isValid}")
    echo &"marghidanu;{passes};{duration};1"

proc initPrimeSieve[S: static uint64](): PrimeSieve[S] = discard # Nim 0-inits

# --- Main block
var passes = 0
let startTime = epochTime()

while true:
    var sieve = initPrimeSieve[1000000]()
    sieve.runSieve()

    passes += 1
    var duration = epochTime() - startTime
    if duration >= 5:
        sieve.printResults(false, duration, passes)
        break
