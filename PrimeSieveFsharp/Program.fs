open System

let primeCounts = Map.ofList([ 
    10, 1               // Historical data for validating our results - the number of prim)
    100, 25            // to be found under some limit, such as 168 primes under 10)
    1000, 168
    10000, 1229
    100000, 9592
    1000000, 78498
    10000000, 664579
    100000000, 5761455
]) 

let initPrimeSieve sieveSize = Array.init sieveSize (fun _ -> true)

let filterPrimes bitArray =
    [|3..2..Array.length bitArray|]
    |> Array.filter (Array.get bitArray)

let countPrimes (primes: int[]) = Array.length primes + 1

let validateResults primeCounts sieveSize primes =
    match Map.tryFind sieveSize primeCounts with
    | Some expected -> expected = countPrimes primes
    | None -> false

let runSieve sieveSize (bitArray: bool[]) =
    let mutable factor = 3
    let mutable num = 0
    let q = sieveSize |> float |> sqrt |> int

    while factor < q do
        num <- factor
        while not (Array.get bitArray num) && num < sieveSize do
            num <- num + 2
        factor <- num

        num <- factor * factor
        while num < sieveSize do
            Array.set bitArray num false
            num <- num + factor * 2

        factor <- factor + 2
    bitArray

let printResults showResults duration passes sieveSize bitArray =
    let primes = bitArray |> filterPrimes 
    if showResults then printfn "2, %s" (String.Join(", ", primes))

    printfn "Passes: %d, Time: %f, Avg: %f, Limit: %d, Count: %d, Valid: %b"
        passes
        duration
        (duration / (float passes))
        sieveSize
        (countPrimes primes)
        (validateResults primeCounts sieveSize primes)

[<EntryPoint>]
let main argv =
    let mutable passes = 0
    let sieveSize = 1_000_000
    let runSieve() = initPrimeSieve sieveSize |> runSieve sieveSize
    let tStart = DateTime.UtcNow

    while (DateTime.UtcNow - tStart).TotalSeconds < 5. do
        runSieve() |> ignore
        passes <- passes + 1

    let tD = DateTime.UtcNow - tStart
    runSieve() |> printResults false tD.TotalSeconds passes sieveSize
    0 // return an integer exit code