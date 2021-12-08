// Software Drag Race submission - base faithful 1-bit...
// compile with "dotnet build -c=Release" for speed...

open System
open Microsoft.FSharp.NativeInterop

# nowarn "9"
# nowarn "51"

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
  let cmpstsplmti: nativeint = NativePtr.toNativeInt &&cmpsts.[cmpsts.Length - 1]
  let inline isprimendx i = cmpsts.[i >>> 3] &&& cBITMASK.[i &&& 7] = 0uy

  let rec loopndx ndx =
    if ndx <= sqrtlmtndx then
      if isprimendx ndx then
        let bp = ndx + ndx + 3
        let bpni : nativeint = nativeint bp
        let swi = (bp * bp - 3) >>> 1
        let llmt = min bitlmt (swi + (bp <<< 3) - 1)
        let rec loopl l =
          if l <= llmt then
            let mask = cBITMASK.[l &&& 7]
            let rec loopcull cullpi =
              if cullpi <= cmpstsplmti then
                let cullp = NativePtr.ofNativeInt<uint8> cullpi
                NativePtr.write cullp (NativePtr.read cullp ||| mask)
                loopcull (cullpi + bpni)
            loopcull <| NativePtr.toNativeInt &&cmpsts.[l >>> 3]
            loopl (l + bp)   
        loopl swi
      loopndx (ndx + 1)
  loopndx 0
  cmpsts.[cmpsts.Length - 1] <- // mask primes > bitlmt
    cmpsts.[cmpsts.Length - 1]  ||| (0xFEuy <<< (bitlmt &&& 7))
  cmpsts // returns the fully sieved butter!!!
         
let primes (oddComposites: byte[]) =
  let bitlmt = oddComposites.Length * 8 - 1
  let inline isprimendx i =
    oddComposites.[i >>> 3] &&& cBITMASK.[i &&& 7] = 0uy
  seq { yield 2; yield! seq { for i in 0 .. bitlmt do
                                if isprimendx i then yield i + i + 3 } }

let printResults showResults duration passes sieveSize numPrimes =
  let isValid = numPrimes = cEXPECTED

  if showResults then printfn "2, %s" (String.Join(", ", primes))

  printfn "Passes: %d, Time: %f, Avg: %f, Limit: %d, Count: %d, Valid: %b"
          passes duration (duration / (float passes)) sieveSize numPrimes isValid

  if isValid then
    printfn "GordonBGood_unpeeled;%d;%f;1;algorithm=base,faithful=yes,bits=1" passes duration
  else
    printfn "ERROR: invalid results"

[<EntryPoint>]
let main _ =
  let tStart = DateTime.Now.Ticks

  let rec loop duration passes =
    let sieveBuffer = newPrimeSieve cLIMIT // computation, once per pass
    if duration < 50000000L then // duration in usints of 100 nsecs
      loop (DateTime.Now.Ticks - tStart) (passes + 1)
    else printResults false (float duration / 1e7)
                      passes cLIMIT (sieveBuffer |> primes |> Seq.length)
  loop 0L 0
           
  0 // return an integer exit code

