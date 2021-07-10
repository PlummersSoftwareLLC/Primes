open System
open System.Globalization

let primeCounts = Map.ofList([ 
    10, 4               // Historical data for validating our results - the number of prim)
    100, 25            // to be found under some limit, such as 168 primes under 10)
    1000, 168
    10000, 1229
    100000, 9592
    1000000, 78498
    10000000, 664579
    100000000, 5761455
]) 

let inline initPrimeSieve sieveSize = 
    let raw = GC.AllocateUninitializedArray(sieveSize, true).AsSpan()
    raw.Fill(true)
    raw

let filterPrimes bitArray =
    [|3..2..Array.length bitArray|]
    |> Array.filter (Array.get bitArray)

let countPrimes (primes: int[]) = Array.length primes + 1

let validateResults primeCounts sieveSize primes =
    match Map.tryFind sieveSize primeCounts with
    | Some expected -> expected = countPrimes primes
    | None -> false

let runSieve sieveSize (bitArray: Span<bool>) =
    let mutable factor = 3
    let mutable num = 0
    let q = sieveSize |> float |> sqrt |> int

    while factor <= q do
        num <- factor
        while not (bitArray.Item num) && num < sieveSize do
            num <- num + 2
        factor <- num

        num <- factor * factor
        while num < sieveSize do
            bitArray.Item num <- false
            num <- num + factor * 2

        factor <- factor + 2
 
let printResults showResults duration passes sieveSize bitArray =
    let primes = bitArray |> filterPrimes 
    let isValid = validateResults primeCounts sieveSize primes

    if showResults then printfn "2, %s" (String.Join(", ", primes))

    printfn $"Passes: %d{passes}, Time: %f{duration}, Avg: %f{duration / (float passes)}, Limit: %d{sieveSize}, Count: %d{countPrimes primes}, Valid: %b{isValid}\n"

    if isValid then
        printfn $"dmannock_fsharp_port;%d{passes};%f{duration};1;algorithm=base,faithful=yes"
    else
        printfn "ERROR: invalid results"

[<EntryPoint>]
let main _ =
    CultureInfo.CurrentCulture <- CultureInfo("en-GB", false)
    let mutable passes = 0
    let sieveSize = 1_000_000
    let mutable sieve = initPrimeSieve sieveSize
    let tStart = DateTime.UtcNow

    while (DateTime.UtcNow - tStart).TotalSeconds < 5. do
        sieve <- initPrimeSieve sieveSize
        runSieve sieveSize sieve
        passes <- passes + 1

    let duration = (DateTime.UtcNow - tStart).TotalSeconds
    printResults false duration passes sieveSize (sieve.ToArray())
    0 // return an integer exit code