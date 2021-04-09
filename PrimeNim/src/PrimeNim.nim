import tables, sequtils, math, times, strformat

type
  PrimeSieve* = object
    sieveSize: int64
    bits: seq[bool]
    myDict: Table[int64, int]

proc generateTable(): Table[int64, int] =
  result[10] = 4
  result[100] = 25
  result[1000] = 168
  result[10000] = 1229
  result[100000] = 9592
  result[1000000] = 78498
  result[10000000] = 664579
  result[100000000] = 5761455
  result[1000000000] = 50847534
  result[10000000000] = 455052511

const PrimeTable = generateTable()

proc countPrimes(self: PrimeSieve): int =
  result = 1
  var i = 3
  while i < self.sieveSize:
    if self.bits[i]:
      result += 1
    i += 2

proc validateResults(self: PrimeSieve): bool =
  if not self.myDict.contains(self.sieveSize): return false
  result = self.myDict[self.sieveSize] == self.countPrimes()

proc newPrimeSieve(n: int64): PrimeSieve =
  result.sieveSize = n
  result.myDict = PrimeTable
  result.bits = true.repeat(n)

proc runSieve(self: var PrimeSieve) =
  var factor = 3
  let q = int(sqrt(float32(self.sieveSize)))
  while factor <= q:
    var num = factor
    while num < self.sieveSize:
      if self.bits[num]:
        factor = num
        break
      num += 2
    var n = factor * factor
    while n < self.sieveSize:
      self.bits[n] = false
      n += factor * 2
    factor += 2

proc printResults(self: PrimeSieve, showResults: bool, duration: Duration, passes: int) =
  if showResults: stdout.write("2, ")
  var count = 1
  var num = 3
  while num <= self.sieveSize:
    if self.bits[num]:
      count += 1
      if showResults:
        stdout.write($num, ", ")
    num += 2

  if showResults: echo()

  echo fmt"Passes: {passes}, Time: {duration}, Avg: {float32(duration.inNanoseconds())/float32(passes)}, Limit: {self.sieveSize}, Count1: {count}, Count2: {self.countPrimes()}, Valid: {self.validateResults()}"


proc main() =
  var passes = 0
  let tStart = getTime()

  while true:
    var sieve = newPrimeSieve(10000000)
    sieve.runSieve()
    passes += 1
    let elapsed = getTime() - tStart
    if elapsed >= initDuration(seconds = 5):
      sieve.printResults(false, elapsed, passes)
      open("../primes.csv", fmAppend).writeLine("Nim,", passes)
      break

main()
