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

# Nim does not have a bit array slice stepping by a value so
# this implements it using a macro; works like Python arr{start:step:limit}
# or like Fortran, or like Julia, or etc.
# bitbufa is the pointer to the array as int; ndx0 is the starting bit index;
# bytelen is size of sieving buffer in byts; step is base prime int...
macro unrollLoops(bitbufa, bytelen, ndx0, step: untyped) = # ndx0 must be var
  let bufalmtid = "bufalmt".newIdentNode
  let strt0id = "strt0".newIdentNode
  let strt7id = "strt7".newIdentNode
  let endalmtid = "endalmt".newIdentNode
  let setaid = "seta".newIdentNode
  result = quote do:
    let `bufalmtid` = `bitbufa` + `bytelen`
    let `strt0id` = `ndx0` shr 3
  for i in 1 .. 7:
    let strtndxido =
      if i < 2: `ndx0` else: newIdentNode("strtndx" & $(i - 1))
    let strtndxidn = newIdentNode("strtndx" & $i)
    let strtid = newIdentNode("strt" & $i)
    result.add quote do:
      let `strtndxidn` = `strtndxido` + `step`
      let `strtid` = (`strtndxidn` shr 3) - `strt0id`
  let csstmnt = quote do:
    case (((`step` and 7) shl 3) + (`ndx0` and 7)).uint8
    of 0'u8: discard
  csstmnt.del 1 # delete last dummy "of"
  for n in 0'u8 .. 0x7F'u8: # actually used 64 cases...
    let stpn = n shr 3
    let ndxn = n and 7'u8
    let mod0id = newLit(ndxn)
    let loopstmnts = nnkStmtList.newTree()
    for i in 0'u8 .. 7'u8: # the unrolled culls are by bytes's
      let mskid = newLit(1'u8 shl ((ndxn + stpn * i) and 7'u8).int)
      let cptrid = ("cptr" & $i).newIdentNode
      let strtid = ("strt" & $i).newIdentNode
      if i == 0'u8:
        loopstmnts.add quote do:
          let `cptrid` = cast[ptr uint8](`setaid`)
      else:
        loopstmnts.add quote do:
          let `cptrid` = cast[ptr uint8](`setaid` + `strtid`)
      loopstmnts.add quote do:
        `cptrid`[] = `cptrid`[] or `mskid`
    loopstmnts.add quote do:
      `setaid` += `step`
    let ofbrstmnts = quote do:
      while `setaid` < `endalmtid`:
        `loopstmnts`
      `ndx0` = ((`setaid` - `bitbufa`) shl 3) or `mod0id`.int
    csstmnt.add nnkOfBranch.newTree(
      newLit(n),
      ofbrstmnts
    )
  for n in 0x80'u8 .. 255'u8: # fill in defaults for remaining possibilities
    csstmnt.add nnkOfBranch.newTree(
      newLit(n),
      nnkStmtList.newTree(
        nnkDiscardStmt.newTree(
          newEmptyNode()
        )
      )
    )
  result.add quote do:
    let `endalmtid` = `bufalmtid` - `strt7id`
    var `setaid` = `bitbufa` + `strt0id`
    `csstmnt`
#  echo csstmnt[10].astGenRepr # see AST for a given case (plus one - 1 .. 32)
#  echo csstmnt[50].toStrLit # see code for a given case (plus one - 1 .. 32)
#  echo result.toStrLit # see entire produced code at compile time

type # encloses all bit sequence operations used...
  BitSeq = ref object
    size: Natural
    buffer: seq[byte]

func newBitSeq(size: int): BitSeq =
  BitSeq(size: size, buffer: newSeq[byte](((size - 1) shr 3) + 1))

func `[]`(bitseq: BitSeq; startstop: HSlice[int, int];
          step: int = 1): iterator: bool {.closure.} {.inline.} =
  assert step <= 0 or startstop.b < startstop.a,
         "Error:  illegal slice limits or step size!!"
  return iterator: bool {.closure.} =
    for i in countup(startstop.a, startstop.b, step):
      yield (bitseq.buffer[i shr 3] and BITMASK[i and 7]) != 0

# sets a range of the BitSeq by step size to true and returns the next index...
func setRange(bitseq: BitSeq; start, stop: int; step: int = 1): int =
  assert step <= 0 or stop < start or stop > bitseq.size,
         "Error:  illegal slice limits or step size!!!"
  let bitbufa = cast[int](bitseq.buffer[0].addr)
  result = start
  let sz = min(bitseq.buffer.len, (stop + 8) shr 3) # round up
  if start <= sz * 8 - 16 * step: # enough loops to be worth the setup
    unrollLoops(bitbufa, sz, result, step)
  while result <= stop:
    let cp = cast[ptr byte](bitbufa + (result shr 3))
    cp[] = cp[] or BITMASK[result and 7]
    result += step

func countTrues(bitseq: BitSeq): int =
  let bsp = cast[ptr UncheckedArray[uint64]](bitseq.buffer[0].addr)
  let lstwrd = bitseq.size shr 6
  let mask = not ((0'u64 - 2'u64) shl (bitseq.size and 63))
  result = 0
  for i in 0 ..< lstwrd: result += bsp[i].popCount
  result -= (bsp[lstwrd] and mask).popCount

type
  PrimeSieve = ref object
    limit: Prime
    sieveBuffer: BitSeq

func newPrimeSieve(lmt: Prime): PrimeSieve = # seq size rounded up to uint64
  doAssert (lmt <= 1.Prime shl 34), "Specified limit is too large!!!"
  if lmt < 3: return PrimeSieve(limit: if lmt < 2: 0 else: 2,
                                sieveBuffer: newBitSeq(0))
  result = PrimeSieve(limit: lmt,
                      sievebuffer: newBitSeq((lmt.int + 64) and -64))

  # include the sieving of the sieveBuffer in the initialization of PrimeSieve
  let cmpsts = newBitSeq(CPUL1CACHE * 8)
  let cmpstsp = cast[ptr UncheckedArray[byte]](cmpsts.buffer[0].addr)
  let cmpstsa = cast[int](cmpstsp)
  let sqrtndx = lmt.float64.sqrt.int
  let bitlmt = lmt.int; let bufsz = (lmt.int + 8) shr 3

  var numbps = 0 # first just cull the base primes and count them...
  for bp in countup(3, sqrtndx, 2):
    if (cmpstsp[bp shr 3] and (1'u8 shl (bp and 7))) == 0'u8: # for base prime
      numbps.inc
      for c in countup(bp * bp, sqrtndx, bp shl 1):
        let cp = cast[ptr uint16](cmpstsa + (c shr 3))
        cp[] = cp[] or (1'u8 shl (c and 7))

  # then fill arrays of base primes/start indexes...
  var basePrimes = newSeq[int](numbps)
  var startIndices = newSeq[int](numbps); var j = 0
  for bp in countup(3, sqrtndx, 2):
    if (cmpstsp[bp shr 3] and (1'u8 shl (bp and 7))) == 0'u8: # for base prime
      basePrimes[j] = bp
      startIndices[j] = (bp * bp) and ((CPUL1CACHE shl 3) - 1); j.inc

  # finally, cull by CPU L1 cache sizes...
  for pgbs in countup(0, bufsz - 1, CPUL1CACHE):
    let cullSize = min(CPUL1CACHE, bufsz - pgbs); zeroMem(cmpstsp, cullSize)
    let cullIndexLimit = min(bitlmt - pgbs * 8, CPUL1CACHE * 8 - 1).int
    let pageLimit = (pgbs + CPUL1CACHE) shl 3
    for i, basePrime in basePrimes.pairs: # then cull by cache pages...
      if basePrime * basePrime >= pageLimit: break
      let delta = 2 * basePrime.int; var cullIndex = startIndices[i]
      let nextCullIndex = cmpsts.setRange(cullIndex, cullIndexLimit, delta)
      startIndices[i] = # store next index for next cache page
        nextCullIndex - CPUL1CACHE * 8
    # moving each sieved cache size into the final result...
    copyMem(result.sieveBuffer.buffer[pgbs].addr,
            cmpsts.buffer[0].addr, cullSize)

method primes(this: PrimeSieve): iterator: Prime {.closure.} {.base.} =
  return iterator: Prime {.closure.} =
    if this.limit >= 2:
      yield 2
      var i = 3; let prs = this.sieveBuffer[3 .. this.limit.int, step = 2]
      for b in prs():
        if not b: yield i.Prime
        i += 2

# alternate very fast counting by 64 bit popCount...
method countPrimes(this: PrimeSieve): int64 {.base.} =
  if this.limit < 3:
    if this.limit < 2: return 0'i64 else: return 1'i64
  let lstwrd = this.limit.int64 div 64
  lstwrd * 32 + (if lstwrd == 0: (this.limit - (this.limit shr 1)).int else: 0) -
    this.sieveBuffer.countTrues.int64

# showing results...

var rslts = ""; let ps0 = 100.newPrimeSieve.primes
for p in ps0(): rslts &= $p & " "
var isValid = rslts == "2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79 83 89 97 "

var count = 0; let ps1 = LIMIT.newPrimeSieve.primes
for p in ps1(): count.inc
isValid = isValid and count == RESULT # two checks

let start = getMonoTime().ticks; var duration = 0'i64
var passes = 0; var rslt: PrimeSieve
while duration < 5_000_000_000:
  rslt = LIMIT.newPrimeSieve
  duration = getMonoTime().ticks - start; passes += 1
let elapsed = duration.float64 / 1e9
let primeCount = rslt.countPrimes
isValid = isValid and primeCount == RESULT # a thrid check

stderr.writeLine(&"Passes: {passes}, Time: {elapsed}, Avg: {(elapsed / passes.float64)}, Limit: {LIMIT}, Count1: {count}, Count2: {primeCount}, Valid: {isValid}")
echo &"GordonBGood_1of2;{passes};{elapsed};1;algorithm=base,faithful=yes,bits=1"
