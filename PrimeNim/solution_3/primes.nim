import std/[ monotimes, tables, strformat ] # timing, verifying, displaying
import std/macros
from std/math import sqrt
from bitops import popCount
 
type Prime = uint64
 
const LIMIT = 1_000_000.Prime
const DICT = {
    10.Prime : 4,
    100.Prime : 25,
    1000.Prime : 168,
    10000.Prime : 1229,
    100000.Prime : 9592,
    1000000.Prime : 78498,
    10000000.Prime : 664579,
    100000000.Prime : 5761455,
    1000000000.Prime : 50847534,
    10000000000.Prime : 455052511,
}.toTable()
const RESULT = DICT[LIMIT]

const CPUL1CACHE {.intdefine.} = 16384 # in bytes

const BITMASK = [ 1'u8, 2, 4, 8, 16, 32, 64, 128 ] # faster than shifting!

# ca is the pointer to the array as int; sz is size of sieving buffer in byts;
# bp is base prime int; strtndx0 is the cull pass start bit index...
macro unrollLoops(ca, sz, bp, strtndx0: untyped) = # strtndx0 must be var
  let cmpstsalmtid = "cmpstsalmt".newIdentNode
  let strt0id = "strt0".newIdentNode
  let strt7id = "strt7".newIdentNode
  let endalmtid = "endalmt".newIdentNode
  let dltaid = "dlta".newIdentNode
  let cullaid = "culla".newIdentNode
  result = quote do:
    let `cmpstsalmtid` = `ca` + `sz`
    let `dltaid` = `bp`.int shl 1
    let `strt0id` = `strtndx0` shr 3
  for i in 1 .. 7:
    let strtndxido =
      if i < 2: `strtndx0` else: newIdentNode("strtndx" & $(i - 1))
    let strtndxidn = newIdentNode("strtndx" & $i)
    let strtid = newIdentNode("strt" & $i)
    result.add quote do:
      let `strtndxidn` = `strtndxido` + `dltaid`
      let `strtid` = (`strtndxidn` shr 3) - `strt0id`
  let csstmnt = quote do:
    case (((`bp`.int and 0x6) shl 2) + (`strtndx0` and 7)).uint8
    of 0'u8: break
  csstmnt.del 1 # delete last dummy "of"
  for n in 0'u8 .. 0x3F'u8: # actually used cases...
    let pn = (n shr 2) or 1'u8
    let cn = n and 7'u8
    let mod0id = newLit(cn)
    let loopstmnts = nnkStmtList.newTree()
    for i in 0'u8 .. 7'u8: # the unrolled culls are by uint16's
      let mskid = newLit(1'u8 shl ((cn + 2 * pn * i) and 7'u8).int)
      let cptrid = ("cptr" & $i).newIdentNode
      let strtid = ("strt" & $i).newIdentNode
      if i == 0'u8:
        loopstmnts.add quote do:
          let `cptrid` = cast[ptr uint8](`cullaid`)
      else:
        loopstmnts.add quote do:
          let `cptrid` = cast[ptr uint8](`cullaid` + `strtid`)
      loopstmnts.add quote do:
        `cptrid`[] = `cptrid`[] or `mskid`
    loopstmnts.add quote do:
      `cullaid` += `dltaid`
    let ofbrstmnts = quote do:
      while `cullaid` < `endalmtid`:
        `loopstmnts`
      `strtndx0` = ((`cullaid` - `ca`) shl 3) or `mod0id`.int
    csstmnt.add nnkOfBranch.newTree(
      newLit(n),
      ofbrstmnts
    )
  for n in 0x40'u8 .. 255'u8: # fill in defaults for remaining possibilities
    csstmnt.add nnkOfBranch.newTree(
      newLit(n),
      nnkStmtList.newTree(
        nnkBreakStmt.newTree(
          newEmptyNode()
        )
      )
    )
  result.add quote do:
    let `endalmtid` = `cmpstsalmtid` - `strt7id`
    var `cullaid` = `ca` + `strt0id`
    `csstmnt`
#  echo csstmnt[10].astGenRepr # see AST for a given case (plus one - 1 .. 32)
#  echo csstmnt[10].toStrLit # see code for a given case (plus one - 1 .. 32)
#  echo result.toStrLit # see entire produced code at compile time

type
  PrimeSieve = ref object
    limit: Prime
    sieveBuffer: seq[byte]

func newPrimeSieve(lmt: Prime): PrimeSieve = # seq size rounded up to uint64
  doAssert (lmt <= 1.Prime shl 34), "Specified limit is too large!!!"
  if lmt < 3: return PrimeSieve(limit: if lmt < 2: 0 else: 2,
                                sieveBuffer: newSeq[byte](0))
  result = PrimeSieve(limit: lmt,
                      sievebuffer: newSeq[byte]((lmt.int + 64) div 64 * 8))
  var cmpsts = newSeq[byte](CPUL1CACHE)
  let cmpstsp = cast[ptr UncheckedArray[byte]](cmpsts[0].addr)
  let cmpstsa = cast[int](cmpstsp)
  let sqrtndx = lmt.float64.sqrt.int
  let bitlmt = lmt.int64; let bufsz = result.sievebuffer.len

  var numbps = 0
  for bp in countup(3, sqrtndx, 2): # first just cull the base primes...
    if (cmpstsp[bp shr 3] and BITMASK[bp and 7]) == 0'u8: # for base prime
      numbps.inc
      for c in countup(bp * bp, sqrtndx, bp shl 1):
        let cp = cast[ptr uint16](cmpstsa + (c shr 3))
        cp[] = cp[] or BITMASK[c and 7] # pointer is 10% faster

  # then fill arrays of base primes/start indexes...
  var swis = newSeq[int](numbps); var bps = newSeq[int](numbps); var j = 0
  for bp in countup(3, sqrtndx, 2):
    if (cmpstsp[bp shr 3] and BITMASK[bp and 7]) == 0'u8: # for base prime
      bps[j] = bp; swis[j] = bp * bp; j.inc

  # finally, cull by CPU L1 cache sizes...
  for pgbs in countup(0, bufsz - 1, CPUL1CACHE):
    let remsz = min(CPUL1CACHE, bufsz - pgbs); zeroMem(cmpstsp, remsz)
    let clmt = min(bitlmt - pgbs * 8, CPUL1CACHE * 8 - 1)
    for i, bp in bps.pairs: # then cull by cache pages...
      var c = swis[i]; let ulmt = clmt - bp * 16
      if c <= ulmt: # discard
        unrollLoops(cmpstsa, remsz, bp, c)
      let dlta = bp + bp
      while c <= clmt:
        let cp = cast[ptr byte](cmpstsa + (c shr 3))
        cp[] = cp[] or BITMASK[c and 7]; c += dlta # pointer is 10% faster
      swis[i] = c - CPUL1CACHE * 8 # store last index for next cache page
    # moving each sieved cache size into the final result...
    copyMem(result.sieveBuffer[pgbs].addr, cmpsts[0].addr, remsz)

iterator primes(this: PrimeSieve): Prime {.closure.} =
  if this.limit >= 2:
    yield 2
    let cmpstsp = cast[ptr UncheckedArray[byte]](this.sieveBuffer[0].addr)
    for i in countup(3, this.limit.int, 2):
      if (cmpstsp[i shr 3] and BITMASK[i and 7]) == 0'u16:
        yield i.Prime

# alternate very fast counting by 64 bit popCount...
method countPrimes(this: PrimeSieve): int64 {.base.} =
  if this.limit < 3:
    if this.limit < 2: return 0'i64 else: return 1'i64
  let cmpstsp = cast[ptr UncheckedArray[uint64]](this.sieveBuffer[0].addr)
  let lstwrd = this.limit.int64 shr 6
  let mask = (0'u64 - 2'u64) shl (this.limit and 63)
  result = lstwrd * 32 + (if lstwrd == 0: 58 else: 63)
  for i in 0 ..< lstwrd: result -= cmpstsp[i].popCount
  result -= (cmpstsp[lstwrd] or mask).popCount

# showing results...

var rslts = ""
for p in 100.newPrimeSieve.primes: rslts &= $p & " "
var isValid = rslts == "2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79 83 89 97 "

var count = 0
for p in LIMIT.newPrimeSieve.primes: count.inc
isValid = isValid and count == RESULT

let start = getMonoTime().ticks; var duration = 0'i64
var passes = 0; var rslt: PrimeSieve
while duration < 5_000_000_000:
  rslt = LIMIT.newPrimeSieve
  GC_FullCollect() # so garbage collection is equivalent to reference counting!
  duration = getMonoTime().ticks - start; passes += 1
let elapsed = duration.float64 / 1_000_000_000'f64
let primeCount = rslt.countPrimes
isValid = isValid and primeCount == RESULT

stderr.writeLine(&"Passes: {passes}, Time: {duration.float64 / 1e9}, Avg: {(elapsed / passes.float64)}, Limit: {LIMIT}, Count1: {count}, Count2: {primeCount}, Valid: {isValid}")
echo &"GordonBGood_1of2;{passes};{elapsed};1;algorithm=base,faithful=yes,bits=1"

