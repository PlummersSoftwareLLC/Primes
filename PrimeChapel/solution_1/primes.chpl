// Benchmark repeating basic Odds-Only Sieve of Eratosthenes...

// compile with "--fast" for speed!

// can use c_ptr to access linear arrays, but select statement is not
// computed `goto`, must use an array of
// classes emulating first class functions with captures...

use Time; use BitOps; use CPtr;

type Prime = uint(64);

const RANGE: Prime = 1000000;

enum Techniques { OddsOnly, Unpeeled, UnpeeledBlock, Unrolled }

const CPUL1CACHE: int = 16384; // in bytes

const RESULTS =
  [ 10: Prime => 4,
    100: Prime => 25,
    1000: Prime => 168,
    10000: Prime => 1229,
    100000: Prime => 9592,
    1000000: Prime => 78498,
    10000000: Prime => 664579,
    100000000: Prime => 5761455,
    1000000000: Prime => 50847534,
    10000000000: Prime => 455052511 ];
const EXPECTED: int = RESULTS[RANGE];

// faster than bit twiddling...
const BITMASK = for i in 0 .. 7 do 1: uint(8) << i;

// super class for a constant interface...
class UR {
  proc this(arrp: c_ptr(uint(8)), si: int, lmti: int, stp: int): int {
    return 42; }
}

// each different generic parame instantiation makes a different sub class...
class URr: UR {
  param ndx: uint(8);
  param pp = (ndx >> 3) & 7;
  param ps = ndx & 7;
  param m0 = 1: uint(8) << ps;
  param m1 = 1: uint(8) << ((ps + pp) & 7);
  param m2 = 1: uint(8) << ((ps + 2 * pp) & 7);
  param m3 = 1: uint(8) << ((ps + 3 * pp) & 7);
  param m4 = 1: uint(8) << ((ps + 4 * pp) & 7);
  param m5 = 1: uint(8) << ((ps + 5 * pp) & 7);
  param m6 = 1: uint(8) << ((ps + 6 * pp) & 7);
  param m7 = 1: uint(8) << ((ps + 7 * pp) & 7);
  // "magic" loop unroller function; uses precomputed offsets and constants...
  override proc this(arrp: c_ptr(uint(8)), si: int, lmti: int, stp: int): int {
    var r0 = si >> 3;
    var r1 = si + stp;
    var r2 = r1 + stp;
    var r3 = r2 + stp;
    var r4 = r3 + stp;
    var r5 = r4 + stp;
    var r6 = r5 + stp;
    var r7 = ((r6 + stp) >> 3) - r0;
    r6 = (r6 >> 3) - r0;
    r5 = (r5 >> 3) - r0;
    r4 = (r4 >> 3) - r0;
    r3 = (r3 >> 3) - r0;
    r2 = (r2 >> 3) - r0;
    r1 = (r1 >> 3) - r0;
    const rlmtp = c_ptrTo(arrp((lmti >> 3) - r7));
    var rp = c_ptrTo(arrp(r0));
    // inner unrolled loop; m0..m7 are constants!
    while rp: int(64) <= rlmtp: int(64) {
      rp(0) |= m0;
      rp(r1) |= m1;
      rp(r2) |= m2;
      rp(r3) |= m3;
      rp(r4) |= m4;
      rp(r5) |= m5;
      rp(r6) |= m6;
      rp(r7) |= m7;
      rp += stp;
    }
    var c = ((rp - arrp) << 3) | ps;
    while c <= lmti { arrp(c >> 3) |= BITMASK(c & 7); c += stp; }
    return c;
  }
}

// a computed goto jump table using closure classes...
type OUR = owned UR; // shorten it up!
const unrollr = [
  new URr(0): OUR, new URr(1): OUR, new URr(2): OUR, new URr(3): OUR,
  new URr(4): OUR, new URr(5): OUR, new URr(6): OUR, new URr(7): OUR,
  new URr(8): OUR, new URr(9): OUR, new URr(10): OUR, new URr(11): OUR,
  new URr(12): OUR, new URr(13): OUR, new URr(14): OUR, new URr(15): OUR,
  new URr(16): OUR, new URr(17): OUR, new URr(18): OUR, new URr(19): OUR,
  new URr(20): OUR, new URr(21): OUR, new URr(22): OUR, new URr(23): OUR,
  new URr(24): OUR, new URr(25): OUR, new URr(26): OUR, new URr(27): OUR,
  new URr(28): OUR, new URr(29): OUR, new URr(30): OUR, new URr(31): OUR,
  new URr(32): OUR, new URr(33): OUR, new URr(34): OUR, new URr(35): OUR,
  new URr(36): OUR, new URr(37): OUR, new URr(38): OUR, new URr(39): OUR,
  new URr(40): OUR, new URr(41): OUR, new URr(42): OUR, new URr(43): OUR,
  new URr(44): OUR, new URr(45): OUR, new URr(46): OUR, new URr(47): OUR,
  new URr(48): OUR, new URr(49): OUR, new URr(50): OUR, new URr(51): OUR,
  new URr(52): OUR, new URr(53): OUR, new URr(54): OUR, new URr(55): OUR,
  new URr(56): OUR, new URr(57): OUR, new URr(58): OUR, new URr(59): OUR,
  new URr(60): OUR, new URr(61): OUR, new URr(62): OUR, new URr(63): OUR ];

class PrimeSieve {
  const limit: Prime;
  const size: int = 0;
  const buffer: [ 0 ..< size ] uint(8);
  proc init(lmt: Prime, tec: Techniques) {
    this.limit = lmt;
    const bitlmt: int = (lmt - 3): int >> 1;
    if bitlmt < 0 then return;
    this.size = ((bitlmt + 64) >> 3) & (-8); // round up to 64 bits
    this.buffer = 0;
    const bufferp = c_ptrTo(this.buffer[0]);
    select tec {
      when Techniques.OddsOnly do
        for i in 0 .. bitlmt {
          if bufferp[i >> 3] & BITMASK[i & 7] == 0 {
            var startndx = (i + i) * (i + 3) + 3;
            if startndx > bitlmt then break;
            const bp = i + i + 3;
            while startndx <= bitlmt {
              bufferp[startndx >> 3] |= BITMASK[startndx & 7];
              startndx += bp;
            }
          }
        }
      when Techniques.Unpeeled do
        for i in 0 .. bitlmt {
          if bufferp[i >> 3] & BITMASK[i & 7] == 0 {
            var startndx = (i + i) * (i + 3) + 3;
            if startndx > bitlmt then break;
            const bp = i + i + 3;
            const slmt = min(bitlmt, startndx + (bp << 3) - 1);
            while startndx <= slmt {
              const mask = BITMASK[startndx & 7];
              var cullndx = startndx >> 3;
              while cullndx < this.size {
                bufferp[cullndx] |= mask;
                cullndx += bp;
              }
              startndx += bp;
            }
          }
        }
      when Techniques.UnpeeledBlock do {
        var strts: [ {0 .. 7} ] int = 0;
        for i in 0 .. bitlmt {
          if bufferp[i >> 3] & BITMASK[i & 7] == 0 {
            const startndx = (i + i) * (i + 3) + 3;
            if startndx > bitlmt then break;
            const bp = i + i + 3;
            var si = startndx;
            for j in 0 .. 7 { strts[si & 7] = si >> 3; si += bp; }
            for pgndx in ((startndx >> 3) & (-CPUL1CACHE))
                            ..< this.size by CPUL1CACHE {
              const pglmt = min(this.size, pgndx + CPUL1CACHE) - 1;
              for j in 0 .. 7 {
                var cullndx = strts[j];
                const mask = BITMASK[j];
                while cullndx <= pglmt {
                  bufferp[cullndx] |= mask; cullndx += bp;
                }
                strts[j] = cullndx;
              }
            }
          }
        }
      }
      when Techniques.Unrolled do
        for i in 0 .. bitlmt {
          if bufferp[i >> 3] & BITMASK[i & 7] == 0 {
            var startndx = (i + i) * (i + 3) + 3;
            if startndx > bitlmt then break;
            const bp = i + i + 3;
            unrollr[((bp & 7) << 3) | (startndx & 7)](bufferp, startndx, bitlmt, bp);
          }
        }
    }
  }
  iter these(): Prime {
    if this.limit > 1 then yield 2: Prime;
    if this.limit < 3 then return;
    const bitlmt: int = ((this.limit - 3) >> 1): int;
    for i in 0 .. bitlmt {
      if this.buffer[i >> 3] & BITMASK[i & 7] == 0 then
        yield (i + i + 3): Prime;
    }
  }
}

// producing, verifying, showing results...

proc benchmark(tech: Techniques) {
  var passes: int = 0;
  var duration: real(64) = 0;
  var sieve: shared PrimeSieve? = nil;
  var timer: Timer; timer.start();
  while duration < 5 {
    sieve = new shared PrimeSieve(RANGE, tech);
    passes += 1; duration = timer.elapsed();
  }
  timer.stop();

  var count: int = 0;
  for p in sieve: shared PrimeSieve do count += 1;

  if count == EXPECTED {
    const lbl: string =
      "GordonBGood_" +
        if tech == Techniques.OddsOnly then "1of2;"
        else if tech == Techniques.Unpeeled then "unpeeled;"
        else if tech == Techniques.UnpeeledBlock then "unpeeled_block;"
        else "unrolled;";
    writeln(lbl, passes, ";", duration, ";1;algorithm=base,faithful=yes,bits=1");
  }
  else { writeln("Invalid result!"); }  
}

for t in Techniques do benchmark(t);
