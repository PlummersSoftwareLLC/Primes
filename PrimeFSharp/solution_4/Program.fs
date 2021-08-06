open System

let cLIMIT = 1000000

let primeCounts = Map.ofList([ 
  10, 4               // Historical data for validating our results - the number of primes
  100, 25             // to be found under some limit, (such as 168 primes under 1000)
  1000, 168
  10000, 1229
  100000, 9592
  1000000, 78498
  10000000, 664579
  100000000, 5761455 ])

let cEXPECTED = match Map.tryFind cLIMIT primeCounts with
                  | Some result -> result
                  | None -> failwith "Not a handled limit value!"

let cBITMASK = [| 1uy; 2uy; 4uy; 8uy; 16uy; 32uy; 64uy; 128uy |]

let newPrimeSieve sieveSize = 
  let sqrtlmtndx = ((sieveSize |> float |> sqrt |> int) - 3) >>> 1
  let bitlmt = (sieveSize - 3) >>> 1
  let cmpsts = Array.zeroCreate ((bitlmt + 8) >>> 3)
  let inline isprimendx i = cmpsts.[i >>> 3] &&& cBITMASK.[i &&& 7] = 0uy

  let rec loopndx ndx =
    if ndx <= sqrtlmtndx then
      if isprimendx ndx then
        let bp = ndx + ndx + 3
        let swi = (bp * bp - 3) >>> 1
        let llmt = min bitlmt (swi + (bp <<< 3) - 1)
        let rec loopl l =
          if l <= llmt then
            let mask = cBITMASK.[l &&& 7]
            let rec loopcull cull =
              if cull < cmpsts.Length then
                cmpsts.[cull] <- cmpsts.[cull] ||| mask
                loopcull (cull + bp)
            loopcull (l >>> 3); loopl (l + bp)   
        loopl swi
      loopndx (ndx + 1)
  loopndx 0
         
  seq { yield 2; yield! seq { for i in 0 .. bitlmt do
                                if isprimendx i then yield i + i + 3 } }

let printResults showResults duration passes sieveSize (primes: seq<int>) =
  let numprimes = Seq.length primes
  let isValid = numprimes = cEXPECTED

  if showResults then printfn "2, %s" (String.Join(", ", primes))

  printfn "Passes: %d, Time: %f, Avg: %f, Limit: %d, Count: %d, Valid: %b"
          passes duration (duration / (float passes)) sieveSize numprimes isValid

  if isValid then
    printfn "GordonBGood_unpeeling;%d;%f;1;algorithm=base;faithful=yes;bits=1" passes duration
  else
    printfn "ERROR: invalid results"

[<EntryPoint>]
let main _ =
  let tStart = DateTime.Now.Ticks

  let rec loop duration passes =
    let sieve = newPrimeSieve cLIMIT
    if duration >= 50000000L then
      printResults false (float duration / 1e7) passes cLIMIT sieve
    else loop (DateTime.Now.Ticks - tStart) (passes + 1)
  loop 0L 0
           
  0 // return an integer exit code

