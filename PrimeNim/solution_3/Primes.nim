# Extreme-Loop-Unrolled Odds-Only Bit-Packed Sieve of Eratosthenes Benchmark...
# This was created for the "Software Drag Racing" GitHub repo as per:
# https://github.com/PlummersSoftwareLLC/Primes...
 
import std/[ monotimes, tables, strformat ] # timing, verifying, displaying
import std/macros
from bitops import popCount
from os import sleep
 
type
  Prime = uint64
  Techniques = enum
    bittwiddle, stride8, stride8block, extreme, extremehybrid  
 
const RANGE = 1_000_000.Prime
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
const EXPECTED = DICT[RANGE]

const CPUL1CACHE = 16384 # 16 Kilobytes

const FORTO = 5_000_000_000 # nanoseconds to test = five secondsS

const DENSETHRESHOLD = 129

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
  csstmnt.add nnkElse.newTree(
    nnkDiscardStmt.newTree(
      newEmptyNode()
    )
  )
  result.add quote do:
    let `endalmtid` = `bufalmtid` - `strt7id`
    var `setaid` = `bitbufa` + `strt0id`
    `csstmnt`
#  echo csstmnt[10].astGenRepr # see AST for a given case (plus one - 1 .. 32)
#  echo csstmnt[2].toStrLit # see code for a given case (plus one - 1 .. 32)
#  echo result.toStrLit # see entire produced code at compile time

# This macro adds special treatment for base prime values which
# are less than some threshold like 31 and still have multiple culls within
# a given 64-bit word...
macro denseSetBits(bitbufa, bytelen, ndx0, step: untyped) = # ndx0 must be var
  let endalmtid = "endalmt".newIdentNode
  let advid = "adv".newIdentNode
  let setaid = "seta".newIdentNode
  let cptrid = "cptr".newIdentNode
  result = quote do:
    while (`ndx0` and 63) > 0: # <= ndxlmt:
      let `cptrid` = cast[ptr byte](`bitbufa` +  (`ndx0` shr 3))
      `cptrid`[] = `cptrid`[] or BITMASK[`ndx0` and 7]
      `ndx0` += `step`
    let `advid` = `step` shl 3 # eight byte per step
    let `endalmtid` = `bitbufa` + `bytelen` - (`advid` - 8) - 1
    var `setaid` = `bitbufa` + ((`ndx0` shr 3) and (-8))
    `ndx0` = `ndx0` and 63
  let csstmnt = quote do:
    case `step`.uint8
    of 0'u8: discard
  csstmnt.del 1 # delete last dummy "of"
  for stpv in countup(3, DENSETHRESHOLD, 2):
    var bi = 0; var wi = -1 # ensure first bit!
    let vid = "v".newIdentNode
    let loopstmnts = nnkStmtList.newTree()
    if stpv < 64:
      loopstmnts.add quote do:
        var `vid`: uint64
    loopstmnts.add quote do:
      let `cptrid` = cast[ptr UncheckedArray[uint64]](`setaid`)
    for _ in 0 .. 63: # the moduloa pattern is word bits in length...
      let mskid = newLit(1'u64 shl (bi and 63))
      if bi shr 6 > wi: # first bit of word
        wi = bi shr 6; bi += stpv
        let wrdid = newLit(wi)
        if bi shr 6 > wi: # only only one marking in word
          loopstmnts.add quote do:
            `cptrid`[`wrdid`] = `cptrid`[`wrdid`] or `mskid`
        else: # first of many markings in word
          loopstmnts.add quote do:
            `vid` = `cptrid`[`wrdid`] or `mskid`
        continue
      else: # multple or last marking in word
        bi += stpv
        if bi shr 6 > wi: # last marking bit in word
          let wrdid = newLit(wi)
          loopstmnts.add quote do:
            `cptrid`[`wrdid`] = `vid` or `mskid`
        else: # next of many markings in word
          loopstmnts.add quote do:
            `vid` = `vid` or `mskid`
    loopstmnts.add quote do:
      `setaid` += `advid`
    let ofbrstmnts = quote do:
      while `setaid` <= `endalmtid`:
        `loopstmnts`
      `ndx0` += ((`setaid` - `bitbufa`) shl 3)
    csstmnt.add nnkOfBranch.newTree(
      newLit(stpv.uint8),
      ofbrstmnts
    )
  csstmnt.add nnkElse.newTree(
    nnkDiscardStmt.newTree(
      newEmptyNode()
    )
  )
  result.add quote do:
    `csstmnt`
#  echo csstmnt[1].astGenRepr # see AST for a given case (this is for step 3)
#  echo csstmnt[1].toStrLit # see code for a given case (this is for step 3)
#  echo result.toStrLit # see entire produced code at compile time

# encloses all bit sequence operations used...
type BitSeq = ref object
  size: Natural
  buffer: seq[byte]
  bufferp: ptr UncheckedArray[byte]

func newBitSeq(size: int): BitSeq =
  # round up to even 256-bit size + 256 bits in bytes...
  let sq = newSeq[byte](((size - 1 + 512) shr 3) and (-32))
  let sqpi = (cast[int](sq[0].unsafeAddr) + 31) and (-32)
  BitSeq(size: size, buffer: sq, bufferp: cast[ptr UncheckedArray[byte]](sqpi))

func `[]`(bitseq: BitSeq; i: int): bool {.inline.} =
  (bitseq.bufferp[i shr 3] and BITMASK[i and 7]) != 0'u8

func `[]`(bitseq: BitSeq; startstop: HSlice[int, int];
          step: int = 1): iterator: bool {.closure.} {.inline.} =
  assert step <= 0 or startstop.b < startstop.a,
         "Error:  illegal slice limits or step size!!"
  return iterator: bool {.closure.} =
    for i in countup(startstop.a, startstop.b, step):
      yield (bitseq.bufferp[i shr 3] and BITMASK[i and 7]) != 0

# sets a range of the BitSeq by step size to true and returns the next index...
func setRange(bitseq: BitSeq; start, stop: int; step: int = 1,
              hybrid: bool = false) =
  assert step <= 0 or stop < start or stop > bitseq.size,
         "Error:  illegal slice limits or step size!!!"
  let bitbufa = cast[int](bitseq.bufferp)
  var ndx = start
  let sz = min(bitseq.buffer.len, (stop + 8) shr 3) # round up
  if start <= sz * 8 - 16 * step: # enough loops to be worth the setup
    if hybrid and step <= DENSETHRESHOLD: denseSetBits(bitbufa, sz, ndx, step)
    else: unrollLoops(bitbufa, sz, ndx, step)
  while ndx <= stop:
    let cp = cast[ptr byte](bitbufa + (ndx shr 3))
    cp[] = cp[] or BITMASK[ndx and 7] # BITMASK[result and 7]
    ndx += step

func countTruesTo(bitseq: BitSeq; index: int): int =
  let bsp = cast[ptr UncheckedArray[uint64]](bitseq.bufferp)
  let lstwrd = index shr 6
  let mask = not ((0'u64 - 2'u64) shl (index and 63))
  result = 0
  for i in 0 ..< lstwrd: result += bsp[i].popCount
  result += (bsp[lstwrd] and mask).popCount

type
  PrimeSieve = ref object
    limit: Prime
    sieveBuffer: BitSeq

func newPrimeSieve(lmt: Prime; tec: Techniques): PrimeSieve =
  doAssert (lmt <= 1.Prime shl 34), "Specified limit is too large!!!"
  if lmt < 3: return PrimeSieve(limit: if lmt < 2: 0 else: 2,
                                sieveBuffer: newBitSeq(0))
  let bitlmt = ((lmt - 3.Prime) shr 1).int 
  result = PrimeSieve(limit: lmt, # BitSeq size rounded up to nearest uint64...
                      sievebuffer: newBitSeq((bitlmt + 64) and (-63)))

  let cmpstsBytesp = result.sieveBuffer.bufferp
  let starts = newSeq[int](8)
  let startsp = cast[ptr UncheckedArray[int]](starts[0].unsafeAddr)
  
  for i in 0 .. bitlmt:
    if result.sieveBuffer[i]: continue
    var cullIndex = (i + i) * (i + 3) + 3 # from ((i + i + 3)^2 - 3) / 2
    if cullIndex > bitlmt: break
    let basePrime = i + i + 3
 
    case tec:
      of bittwiddle:
        while cullIndex <= bitlmt:
          let byteIndex = cullIndex shr 3
          cmpstsBytesp[byteIndex] =
            cmpstsBytesp[byteIndex] or BITMASK[cullIndex and 7]
          cullIndex += basePrime
      
      of stride8:
        let bufa = cast[int](cmpstsBytesp)
        let bytealmt = bufa + (bitlmt shr 3)
        let slmt = min(bitlmt, cullIndex + (basePrime shl 3) - 1)       
        while cullIndex <= slmt:
          let mask = BITMASK[cullIndex and 7]
          var bytepa = bufa + (cullIndex shr 3)
          while bytepa <= bytealmt:
            let cp = cast[ptr byte](bytepa)
            cp[] = cp[] or mask; bytepa += basePrime
          cullIndex += basePrime
      
      of stride8block:
        let bufa = cast[int](cmpstsBytesp); let bufalmt = bufa + (bitlmt shr 3)
        let starta = bufa + ((cullIndex shr 3) and (-CPUL1CACHE))
        let bp2 = basePrime + basePrime; let bp3 = basePrime + bp2
        let bp4 = basePrime + bp3
        for _ in 0 .. 7:
          startsp[cullIndex and 7] = bufa + (cullIndex shr 3)
          cullIndex += basePrime
        for pagea in countup(starta, bufalmt, CPUL1CACHE):
          let pageLimit = min(bufalmt, pagea + CPUL1CACHE - 1)
          for si in 0 .. 7:
            let mask = BITMASK[si]; var bytea = startsp[si]
            while bytea <= pageLimit - bp3:
              let cp = cast[ptr UncheckedArray[byte]](bytea)
              cp[0] = cp[0] or mask; cp[basePrime] = cp[basePrime] or mask
              cp[bp2] = cp[bp2] or mask; cp[bp3] = cp[bp3] or mask
              bytea += bp4
            while bytea <= pageLimit:
              let cp = cast[ptr byte](bytea)
              cp[] = cp[] or mask; bytea += basePrime
            startsp[si] = bytea
      
      of extreme:
        result.sieveBuffer.setRange(cullIndex, bitlmt, step = basePrime)
      
      of extremehybrid:
        result.sieveBuffer.setRange(cullIndex, bitlmt, step = basePrime,
                                    hybrid = true)

method primes(this: PrimeSieve): iterator: Prime {.closure.} {.base.} =
  return iterator: Prime {.closure.} =
    if this.limit >= 2:
      yield 2
      let lmt = ((this.limit - 3.Prime) shr 1).int
      var i = 3; let prs = this.sieveBuffer[0 .. lmt]
      for b in prs():
        if not b: yield i.Prime
        i += 2

# alternate very fast counting by 64 bit popCount...
method countPrimes(this: PrimeSieve): int64 {.base.} =
  if this.limit < 3:
    if this.limit < 2: return 0'i64 else: return 1'i64
  let lstndx = ((this.limit - 3.Prime) shr 1).int64
  let lstwrd = (lstndx shr 6); let lstmod = lstndx and 63
  2 + lstmod + lstwrd * 64 - this.sieveBuffer.countTruesTo(lstndx.int).int64

# verifying, producing, showing results...
proc bench(tec: Techniques) =
  var rslts = ""; let ps0 = 100.newPrimeSieve(tec).primes
  for p in ps0(): rslts &= $p & " "
  var isValid = rslts == "2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79 83 89 97 "

  var count = 0; let ps1 = RANGE.newPrimeSieve(tec).primes
  for p in ps1(): count.inc
  isValid = isValid and count == EXPECTED # two checks

  sleep 5000 # sleep 5000 milliseconds to let CPU cool down!
  let start = getMonoTime().ticks; var duration = 0'i64
  var passes = 0; var rslt: PrimeSieve
  while duration < FORTO:
    rslt = RANGE.newPrimeSieve(tec)
    duration = getMonoTime().ticks - start; passes += 1

  let elapsed = duration.float64 / 1e9 # in float64 seconds
  let primeCount = rslt.countPrimes
  isValid = isValid and primeCount == EXPECTED # a third check

  let label = "GordonBGood_" & (
                case tec:
                    of bittwiddle: "bittwiddle"
                    of stride8: "stride8"
                    of stride8block: "stride8block-16K"
                    of extreme: "extreme"
                    of extremehybrid: "extreme-hybrid" )
  
  stderr.write(&"Passes: {passes}, Time: {elapsed}, ")
  stderr.write(&"Avg: {(elapsed / passes.float64)}, Limit: {RANGE}, ")
  stderr.writeLine(&"Count1: {count}, Count2: {primeCount}, Valid: {isValid}")

  stdout.write(&"{label};{passes};{elapsed}")
  echo ";1;algorithm=base,faithful=yes,bits=1"

for t in Techniques.bittwiddle .. Techniques.extremehybrid: bench t

