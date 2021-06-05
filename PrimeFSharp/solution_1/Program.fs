open System
open System.Collections
open System.Globalization

[<AllowNullLiteral(true)>]
type PrimeSieve (size) =
  let sieveSize = size
  let primesArray = BitArray((size + 1) / 2, true)

  member _this.CountPrimes = 
    let mutable count = 0
        
    for value in primesArray do
      if value then count <- count + 1

    count

  member _this.RunSieve = 
    let maxFactor = Convert.ToInt32(sqrt(float sieveSize))

    let FindNextPrime start = 
      let mutable number = start
            
      while number <= maxFactor && not primesArray.[number / 2] do
        number <- number + 2

      (number, number <= maxFactor)

    let MarkNonPrimes factor = 
      let mutable number = factor * factor

      while number <= sieveSize do
        primesArray.[number / 2] <- false
        number <- number + (factor * 2)

    let mutable factor = 3
    let mutable proceed = true

    while proceed do
      MarkNonPrimes(factor)

      FindNextPrime(factor + 2) |> fun(f, p) ->
        factor <- f
        proceed <- p

    primesArray

[<EntryPoint>]
let main _argv =
  CultureInfo.CurrentCulture <- CultureInfo("en-US", false)

  let referenceResults = 
    dict [
      10, 4; 
      100, 25; 
      1000, 168; 
      10000, 1229; 
      100000, 9592; 
      1000000, 78498; 
      10000000, 664579; 
      100000000, 5761455
    ]

  let TimedRun(sieveSize) =
    let start = DateTime.UtcNow
    let mutable passCount = 0
    let mutable sieve = null

    while (DateTime.UtcNow - start).TotalSeconds <= 5.0 do
      sieve <- PrimeSieve(sieveSize)
      sieve.RunSieve |> ignore
      passCount <- passCount + 1

    (sieve, passCount, (DateTime.UtcNow - start).TotalSeconds)

  let sieveSize = 1000000
  let (sieve, passCount, duration) = TimedRun sieveSize

  if sieve.CountPrimes <> referenceResults.[sieveSize] then
    printfn "WARNING: result is incorrect!"

  printfn $"rbergen;{ passCount };{ duration };1;algorithm=base,faithful=yes,bits=1"

  0

