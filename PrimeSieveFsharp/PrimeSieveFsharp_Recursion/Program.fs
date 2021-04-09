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
    let q = sieveSize |> float |> sqrt |> int

    let rec findNext num =
        if Array.get bitArray num
        then num
        else findNext (num + 2)

    let eliminate factor =
        let rec loop num =
            if num < sieveSize then
                Array.set bitArray num false
                loop (num + factor * 2)
            else ()
        loop (factor * factor)
      
    let rec run factor =
        if factor <= q then
            let factor = findNext factor
            eliminate factor
            run (factor +  2)
        else ()
    run 3

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
let main _ =
    let mutable passes = 0
    let sieveSize = 1_000_000
    let tStart = DateTime.UtcNow

    while (DateTime.UtcNow - tStart).TotalSeconds < 5. do
        initPrimeSieve sieveSize |> runSieve sieveSize
        passes <- passes + 1

    let tD = DateTime.UtcNow - tStart
    let checkBitArray = initPrimeSieve sieveSize
    runSieve sieveSize checkBitArray
    printResults false tD.TotalSeconds passes sieveSize checkBitArray
    0 // return an integer exit code