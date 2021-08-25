// Benchmark repeating basic Odds-Only Sieve of Eratosthenes...

// compile with "--fast" for speed!

// can use c_ptr to access linear arrays, but select statement is not
// computed `goto`, must use an array of
// classes emulating first class functions with captures...

use Time; use BitOps; use CPtr;

type Prime = uint(64);

const RANGE: Prime = 1000000;

enum Techniques { BitTwiddle, Unpeeled, UnpeeledBlock, Unrolled, UnrolledHybrid }

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

// each different generic param instantiation makes a different sub class...
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

// a computed goto jump table using modulo pattern closure classes...
type OUR = owned UR; // shorten it up!
const unrollr = [ // note that only cases 15, 27, 43, and 63 are used!
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

// each different generic param instantiation makes a different sub class...
// don't have macros, so must generate code by hand or with generator...

class URh3: UR {
  // hybrid bit setting by densely packed uint64's...
  override proc this(arrp: c_ptr(uint(8)), si: int, lmti: int, stp: int): int {
    const ilmt = si | 63; // make stop limit to next 64-bit boundary
    var biti = si;
    while biti <= ilmt { arrp(biti >> 3) |= BITMASK(biti & 7); biti += stp; }
    const rlmtp =
      c_ptrTo(arrp(((lmti >> 3) & (-8)) - 8 * (stp - 1))): c_ptr(uint(64));
    var rp = c_ptrTo(arrp((biti >> 3) & (-8))): c_ptr(uint(64));
    while rp: int(64) <= rlmtp: int(64) {
      var v = rp(0) | 0x0000000000000004: uint(64);
      v = v | 0x0000000000000020: uint(64);
      v = v | 0x0000000000000100: uint(64);
      v = v | 0x0000000000000800: uint(64);
      v = v | 0x0000000000004000: uint(64);
      v = v | 0x0000000000020000: uint(64);
      v = v | 0x0000000000100000: uint(64);
      v = v | 0x0000000000800000: uint(64);
      v = v | 0x0000000004000000: uint(64);
      v = v | 0x0000000020000000: uint(64);
      v = v | 0x0000000100000000: uint(64);
      v = v | 0x0000000800000000: uint(64);
      v = v | 0x0000004000000000: uint(64);
      v = v | 0x0000020000000000: uint(64);
      v = v | 0x0000100000000000: uint(64);
      v = v | 0x0000800000000000: uint(64);
      v = v | 0x0004000000000000: uint(64);
      v = v | 0x0020000000000000: uint(64);
      v = v | 0x0100000000000000: uint(64);
      v = v | 0x0800000000000000: uint(64);
      rp(0) = v | 0x4000000000000000: uint(64);
      v = rp(1) | 0x0000000000000002: uint(64);
      v = v | 0x0000000000000010: uint(64);
      v = v | 0x0000000000000080: uint(64);
      v = v | 0x0000000000000400: uint(64);
      v = v | 0x0000000000002000: uint(64);
      v = v | 0x0000000000010000: uint(64);
      v = v | 0x0000000000080000: uint(64);
      v = v | 0x0000000000400000: uint(64);
      v = v | 0x0000000002000000: uint(64);
      v = v | 0x0000000010000000: uint(64);
      v = v | 0x0000000080000000: uint(64);
      v = v | 0x0000000400000000: uint(64);
      v = v | 0x0000002000000000: uint(64);
      v = v | 0x0000010000000000: uint(64);
      v = v | 0x0000080000000000: uint(64);
      v = v | 0x0000400000000000: uint(64);
      v = v | 0x0002000000000000: uint(64);
      v = v | 0x0010000000000000: uint(64);
      v = v | 0x0080000000000000: uint(64);
      v = v | 0x0400000000000000: uint(64);
      rp(1) = v | 0x2000000000000000: uint(64);
      v = rp(2) | 0x0000000000000001: uint(64);
      v = v | 0x0000000000000008: uint(64);
      v = v | 0x0000000000000040: uint(64);
      v = v | 0x0000000000000200: uint(64);
      v = v | 0x0000000000001000: uint(64);
      v = v | 0x0000000000008000: uint(64);
      v = v | 0x0000000000040000: uint(64);
      v = v | 0x0000000000200000: uint(64);
      v = v | 0x0000000001000000: uint(64);
      v = v | 0x0000000008000000: uint(64);
      v = v | 0x0000000040000000: uint(64);
      v = v | 0x0000000200000000: uint(64);
      v = v | 0x0000001000000000: uint(64);
      v = v | 0x0000008000000000: uint(64);
      v = v | 0x0000040000000000: uint(64);
      v = v | 0x0000200000000000: uint(64);
      v = v | 0x0001000000000000: uint(64);
      v = v | 0x0008000000000000: uint(64);
      v = v | 0x0040000000000000: uint(64);
      v = v | 0x0200000000000000: uint(64);
      v = v | 0x1000000000000000: uint(64);
      rp(2) = v | 0x8000000000000000: uint(64);
      rp += stp;
    }
    biti = ((rp: int(64) - arrp: int(64)) << 3) + (biti & 63);
    while biti <= lmti { arrp(biti >> 3) |= BITMASK(biti & 7); biti += stp; }
    return biti;
  }
}

class URh5: UR {
  // hybrid bit setting by densely packed uint64's...
  override proc this(arrp: c_ptr(uint(8)), si: int, lmti: int, stp: int): int {
    const ilmt = si | 63; // make stop limit to next 64-bit boundary
    var biti = si;
    while biti <= ilmt { arrp(biti >> 3) |= BITMASK(biti & 7); biti += stp; }
    const rlmtp =
      c_ptrTo(arrp(((lmti >> 3) & (-8)) - 8 * (stp - 1))): c_ptr(uint(64));
    var rp = c_ptrTo(arrp((biti >> 3) & (-8))): c_ptr(uint(64));
    while rp: int(64) <= rlmtp: int(64) {
      var v = rp(0) | 0x0000000000000004: uint(64);
      v = v | 0x0000000000000080: uint(64);
      v = v | 0x0000000000001000: uint(64);
      v = v | 0x0000000000020000: uint(64);
      v = v | 0x0000000000400000: uint(64);
      v = v | 0x0000000008000000: uint(64);
      v = v | 0x0000000100000000: uint(64);
      v = v | 0x0000002000000000: uint(64);
      v = v | 0x0000040000000000: uint(64);
      v = v | 0x0000800000000000: uint(64);
      v = v | 0x0010000000000000: uint(64);
      v = v | 0x0200000000000000: uint(64);
      rp(0) = v | 0x4000000000000000: uint(64);
      v = rp(1) | 0x0000000000000008: uint(64);
      v = v | 0x0000000000000100: uint(64);
      v = v | 0x0000000000002000: uint(64);
      v = v | 0x0000000000040000: uint(64);
      v = v | 0x0000000000800000: uint(64);
      v = v | 0x0000000010000000: uint(64);
      v = v | 0x0000000200000000: uint(64);
      v = v | 0x0000004000000000: uint(64);
      v = v | 0x0000080000000000: uint(64);
      v = v | 0x0001000000000000: uint(64);
      v = v | 0x0020000000000000: uint(64);
      v = v | 0x0400000000000000: uint(64);
      rp(1) = v | 0x8000000000000000: uint(64);
      v = rp(2) | 0x0000000000000010: uint(64);
      v = v | 0x0000000000000200: uint(64);
      v = v | 0x0000000000004000: uint(64);
      v = v | 0x0000000000080000: uint(64);
      v = v | 0x0000000001000000: uint(64);
      v = v | 0x0000000020000000: uint(64);
      v = v | 0x0000000400000000: uint(64);
      v = v | 0x0000008000000000: uint(64);
      v = v | 0x0000100000000000: uint(64);
      v = v | 0x0002000000000000: uint(64);
      v = v | 0x0040000000000000: uint(64);
      rp(2) = v | 0x0800000000000000: uint(64);
      v = rp(3) | 0x0000000000000001: uint(64);
      v = v | 0x0000000000000020: uint(64);
      v = v | 0x0000000000000400: uint(64);
      v = v | 0x0000000000008000: uint(64);
      v = v | 0x0000000000100000: uint(64);
      v = v | 0x0000000002000000: uint(64);
      v = v | 0x0000000040000000: uint(64);
      v = v | 0x0000000800000000: uint(64);
      v = v | 0x0000010000000000: uint(64);
      v = v | 0x0000200000000000: uint(64);
      v = v | 0x0004000000000000: uint(64);
      v = v | 0x0080000000000000: uint(64);
      rp(3) = v | 0x1000000000000000: uint(64);
      v = rp(4) | 0x0000000000000002: uint(64);
      v = v | 0x0000000000000040: uint(64);
      v = v | 0x0000000000000800: uint(64);
      v = v | 0x0000000000010000: uint(64);
      v = v | 0x0000000000200000: uint(64);
      v = v | 0x0000000004000000: uint(64);
      v = v | 0x0000000080000000: uint(64);
      v = v | 0x0000001000000000: uint(64);
      v = v | 0x0000020000000000: uint(64);
      v = v | 0x0000400000000000: uint(64);
      v = v | 0x0008000000000000: uint(64);
      v = v | 0x0100000000000000: uint(64);
      rp(4) = v | 0x2000000000000000: uint(64);
      rp += stp;
    }
    biti = ((rp: int(64) - arrp: int(64)) << 3) + (biti & 63);
    while biti <= lmti { arrp(biti >> 3) |= BITMASK(biti & 7); biti += stp; }
    return biti;
  }
}

class URh7: UR {
  // hybrid bit setting by densely packed uint64's...
  override proc this(arrp: c_ptr(uint(8)), si: int, lmti: int, stp: int): int {
    const ilmt = si | 63; // make stop limit to next 64-bit boundary
    var biti = si;
    while biti <= ilmt { arrp(biti >> 3) |= BITMASK(biti & 7); biti += stp; }
    const rlmtp =
      c_ptrTo(arrp(((lmti >> 3) & (-8)) - 8 * (stp - 1))): c_ptr(uint(64));
    var rp = c_ptrTo(arrp((biti >> 3) & (-8))): c_ptr(uint(64));
    while rp: int(64) <= rlmtp: int(64) {
      var v = rp(0) | 0x0000000000000002: uint(64);
      v = v | 0x0000000000000100: uint(64);
      v = v | 0x0000000000008000: uint(64);
      v = v | 0x0000000000400000: uint(64);
      v = v | 0x0000000020000000: uint(64);
      v = v | 0x0000001000000000: uint(64);
      v = v | 0x0000080000000000: uint(64);
      v = v | 0x0004000000000000: uint(64);
      rp(0) = v | 0x0200000000000000: uint(64);
      v = rp(1) | 0x0000000000000001: uint(64);
      v = v | 0x0000000000000080: uint(64);
      v = v | 0x0000000000004000: uint(64);
      v = v | 0x0000000000200000: uint(64);
      v = v | 0x0000000010000000: uint(64);
      v = v | 0x0000000800000000: uint(64);
      v = v | 0x0000040000000000: uint(64);
      v = v | 0x0002000000000000: uint(64);
      v = v | 0x0100000000000000: uint(64);
      rp(1) = v | 0x8000000000000000: uint(64);
      v = rp(2) | 0x0000000000000040: uint(64);
      v = v | 0x0000000000002000: uint(64);
      v = v | 0x0000000000100000: uint(64);
      v = v | 0x0000000008000000: uint(64);
      v = v | 0x0000000400000000: uint(64);
      v = v | 0x0000020000000000: uint(64);
      v = v | 0x0001000000000000: uint(64);
      v = v | 0x0080000000000000: uint(64);
      rp(2) = v | 0x4000000000000000: uint(64);
      v = rp(3) | 0x0000000000000020: uint(64);
      v = v | 0x0000000000001000: uint(64);
      v = v | 0x0000000000080000: uint(64);
      v = v | 0x0000000004000000: uint(64);
      v = v | 0x0000000200000000: uint(64);
      v = v | 0x0000010000000000: uint(64);
      v = v | 0x0000800000000000: uint(64);
      v = v | 0x0040000000000000: uint(64);
      rp(3) = v | 0x2000000000000000: uint(64);
      v = rp(4) | 0x0000000000000010: uint(64);
      v = v | 0x0000000000000800: uint(64);
      v = v | 0x0000000000040000: uint(64);
      v = v | 0x0000000002000000: uint(64);
      v = v | 0x0000000100000000: uint(64);
      v = v | 0x0000008000000000: uint(64);
      v = v | 0x0000400000000000: uint(64);
      v = v | 0x0020000000000000: uint(64);
      rp(4) = v | 0x1000000000000000: uint(64);
      v = rp(5) | 0x0000000000000008: uint(64);
      v = v | 0x0000000000000400: uint(64);
      v = v | 0x0000000000020000: uint(64);
      v = v | 0x0000000001000000: uint(64);
      v = v | 0x0000000080000000: uint(64);
      v = v | 0x0000004000000000: uint(64);
      v = v | 0x0000200000000000: uint(64);
      v = v | 0x0010000000000000: uint(64);
      rp(5) = v | 0x0800000000000000: uint(64);
      v = rp(6) | 0x0000000000000004: uint(64);
      v = v | 0x0000000000000200: uint(64);
      v = v | 0x0000000000010000: uint(64);
      v = v | 0x0000000000800000: uint(64);
      v = v | 0x0000000040000000: uint(64);
      v = v | 0x0000002000000000: uint(64);
      v = v | 0x0000100000000000: uint(64);
      v = v | 0x0008000000000000: uint(64);
      rp(6) = v | 0x0400000000000000: uint(64);
      rp += stp;
    }
    biti = ((rp: int(64) - arrp: int(64)) << 3) + (biti & 63);
    while biti <= lmti { arrp(biti >> 3) |= BITMASK(biti & 7); biti += stp; }
    return biti;
  }
}

class URh9: UR {
  // hybrid bit setting by densely packed uint64's...
  override proc this(arrp: c_ptr(uint(8)), si: int, lmti: int, stp: int): int {
    const ilmt = si | 63; // make stop limit to next 64-bit boundary
    var biti = si;
    while biti <= ilmt { arrp(biti >> 3) |= BITMASK(biti & 7); biti += stp; }
    const rlmtp =
      c_ptrTo(arrp(((lmti >> 3) & (-8)) - 8 * (stp - 1))): c_ptr(uint(64));
    var rp = c_ptrTo(arrp((biti >> 3) & (-8))): c_ptr(uint(64));
    while rp: int(64) <= rlmtp: int(64) {
      var v = rp(0) | 0x0000000000000004: uint(64);
      v = v | 0x0000000000000800: uint(64);
      v = v | 0x0000000000100000: uint(64);
      v = v | 0x0000000020000000: uint(64);
      v = v | 0x0000004000000000: uint(64);
      v = v | 0x0000800000000000: uint(64);
      rp(0) = v | 0x0100000000000000: uint(64);
      v = rp(1) | 0x0000000000000002: uint(64);
      v = v | 0x0000000000000400: uint(64);
      v = v | 0x0000000000080000: uint(64);
      v = v | 0x0000000010000000: uint(64);
      v = v | 0x0000002000000000: uint(64);
      v = v | 0x0000400000000000: uint(64);
      rp(1) = v | 0x0080000000000000: uint(64);
      v = rp(2) | 0x0000000000000001: uint(64);
      v = v | 0x0000000000000200: uint(64);
      v = v | 0x0000000000040000: uint(64);
      v = v | 0x0000000008000000: uint(64);
      v = v | 0x0000001000000000: uint(64);
      v = v | 0x0000200000000000: uint(64);
      v = v | 0x0040000000000000: uint(64);
      rp(2) = v | 0x8000000000000000: uint(64);
      v = rp(3) | 0x0000000000000100: uint(64);
      v = v | 0x0000000000020000: uint(64);
      v = v | 0x0000000004000000: uint(64);
      v = v | 0x0000000800000000: uint(64);
      v = v | 0x0000100000000000: uint(64);
      v = v | 0x0020000000000000: uint(64);
      rp(3) = v | 0x4000000000000000: uint(64);
      v = rp(4) | 0x0000000000000080: uint(64);
      v = v | 0x0000000000010000: uint(64);
      v = v | 0x0000000002000000: uint(64);
      v = v | 0x0000000400000000: uint(64);
      v = v | 0x0000080000000000: uint(64);
      v = v | 0x0010000000000000: uint(64);
      rp(4) = v | 0x2000000000000000: uint(64);
      v = rp(5) | 0x0000000000000040: uint(64);
      v = v | 0x0000000000008000: uint(64);
      v = v | 0x0000000001000000: uint(64);
      v = v | 0x0000000200000000: uint(64);
      v = v | 0x0000040000000000: uint(64);
      v = v | 0x0008000000000000: uint(64);
      rp(5) = v | 0x1000000000000000: uint(64);
      v = rp(6) | 0x0000000000000020: uint(64);
      v = v | 0x0000000000004000: uint(64);
      v = v | 0x0000000000800000: uint(64);
      v = v | 0x0000000100000000: uint(64);
      v = v | 0x0000020000000000: uint(64);
      v = v | 0x0004000000000000: uint(64);
      rp(6) = v | 0x0800000000000000: uint(64);
      v = rp(7) | 0x0000000000000010: uint(64);
      v = v | 0x0000000000002000: uint(64);
      v = v | 0x0000000000400000: uint(64);
      v = v | 0x0000000080000000: uint(64);
      v = v | 0x0000010000000000: uint(64);
      v = v | 0x0002000000000000: uint(64);
      rp(7) = v | 0x0400000000000000: uint(64);
      v = rp(8) | 0x0000000000000008: uint(64);
      v = v | 0x0000000000001000: uint(64);
      v = v | 0x0000000000200000: uint(64);
      v = v | 0x0000000040000000: uint(64);
      v = v | 0x0000008000000000: uint(64);
      v = v | 0x0001000000000000: uint(64);
      rp(8) = v | 0x0200000000000000: uint(64);
      rp += stp;
    }
    biti = ((rp: int(64) - arrp: int(64)) << 3) + (biti & 63);
    while biti <= lmti { arrp(biti >> 3) |= BITMASK(biti & 7); biti += stp; }
    return biti;
  }
}

class URh11: UR {
  // hybrid bit setting by densely packed uint64's...
  override proc this(arrp: c_ptr(uint(8)), si: int, lmti: int, stp: int): int {
    const ilmt = si | 63; // make stop limit to next 64-bit boundary
    var biti = si;
    while biti <= ilmt { arrp(biti >> 3) |= BITMASK(biti & 7); biti += stp; }
    const rlmtp =
      c_ptrTo(arrp(((lmti >> 3) & (-8)) - 8 * (stp - 1))): c_ptr(uint(64));
    var rp = c_ptrTo(arrp((biti >> 3) & (-8))): c_ptr(uint(64));
    while rp: int(64) <= rlmtp: int(64) {
      var v = rp(0) | 0x0000000000000040: uint(64);
      v = v | 0x0000000000020000: uint(64);
      v = v | 0x0000000010000000: uint(64);
      v = v | 0x0000008000000000: uint(64);
      v = v | 0x0004000000000000: uint(64);
      rp(0) = v | 0x2000000000000000: uint(64);
      v = rp(1) | 0x0000000000000100: uint(64);
      v = v | 0x0000000000080000: uint(64);
      v = v | 0x0000000040000000: uint(64);
      v = v | 0x0000020000000000: uint(64);
      v = v | 0x0010000000000000: uint(64);
      rp(1) = v | 0x8000000000000000: uint(64);
      v = rp(2) | 0x0000000000000400: uint(64);
      v = v | 0x0000000000200000: uint(64);
      v = v | 0x0000000100000000: uint(64);
      v = v | 0x0000080000000000: uint(64);
      rp(2) = v | 0x0040000000000000: uint(64);
      v = rp(3) | 0x0000000000000002: uint(64);
      v = v | 0x0000000000001000: uint(64);
      v = v | 0x0000000000800000: uint(64);
      v = v | 0x0000000400000000: uint(64);
      v = v | 0x0000200000000000: uint(64);
      rp(3) = v | 0x0100000000000000: uint(64);
      v = rp(4) | 0x0000000000000008: uint(64);
      v = v | 0x0000000000004000: uint(64);
      v = v | 0x0000000002000000: uint(64);
      v = v | 0x0000001000000000: uint(64);
      v = v | 0x0000800000000000: uint(64);
      rp(4) = v | 0x0400000000000000: uint(64);
      v = rp(5) | 0x0000000000000020: uint(64);
      v = v | 0x0000000000010000: uint(64);
      v = v | 0x0000000008000000: uint(64);
      v = v | 0x0000004000000000: uint(64);
      v = v | 0x0002000000000000: uint(64);
      rp(5) = v | 0x1000000000000000: uint(64);
      v = rp(6) | 0x0000000000000080: uint(64);
      v = v | 0x0000000000040000: uint(64);
      v = v | 0x0000000020000000: uint(64);
      v = v | 0x0000010000000000: uint(64);
      v = v | 0x0008000000000000: uint(64);
      rp(6) = v | 0x4000000000000000: uint(64);
      v = rp(7) | 0x0000000000000200: uint(64);
      v = v | 0x0000000000100000: uint(64);
      v = v | 0x0000000080000000: uint(64);
      v = v | 0x0000040000000000: uint(64);
      rp(7) = v | 0x0020000000000000: uint(64);
      v = rp(8) | 0x0000000000000001: uint(64);
      v = v | 0x0000000000000800: uint(64);
      v = v | 0x0000000000400000: uint(64);
      v = v | 0x0000000200000000: uint(64);
      v = v | 0x0000100000000000: uint(64);
      rp(8) = v | 0x0080000000000000: uint(64);
      v = rp(9) | 0x0000000000000004: uint(64);
      v = v | 0x0000000000002000: uint(64);
      v = v | 0x0000000001000000: uint(64);
      v = v | 0x0000000800000000: uint(64);
      v = v | 0x0000400000000000: uint(64);
      rp(9) = v | 0x0200000000000000: uint(64);
      v = rp(10) | 0x0000000000000010: uint(64);
      v = v | 0x0000000000008000: uint(64);
      v = v | 0x0000000004000000: uint(64);
      v = v | 0x0000002000000000: uint(64);
      v = v | 0x0001000000000000: uint(64);
      rp(10) = v | 0x0800000000000000: uint(64);
      rp += stp;
    }
    biti = ((rp: int(64) - arrp: int(64)) << 3) + (biti & 63);
    while biti <= lmti { arrp(biti >> 3) |= BITMASK(biti & 7); biti += stp; }
    return biti;
  }
}

class URh13: UR {
  // hybrid bit setting by densely packed uint64's...
  override proc this(arrp: c_ptr(uint(8)), si: int, lmti: int, stp: int): int {
    const ilmt = si | 63; // make stop limit to next 64-bit boundary
    var biti = si;
    while biti <= ilmt { arrp(biti >> 3) |= BITMASK(biti & 7); biti += stp; }
    const rlmtp =
      c_ptrTo(arrp(((lmti >> 3) & (-8)) - 8 * (stp - 1))): c_ptr(uint(64));
    var rp = c_ptrTo(arrp((biti >> 3) & (-8))): c_ptr(uint(64));
    while rp: int(64) <= rlmtp: int(64) {
      var v = rp(0) | 0x0000000000000080: uint(64);
      v = v | 0x0000000000100000: uint(64);
      v = v | 0x0000000200000000: uint(64);
      v = v | 0x0000400000000000: uint(64);
      rp(0) = v | 0x0800000000000000: uint(64);
      v = rp(1) | 0x0000000000000100: uint(64);
      v = v | 0x0000000000200000: uint(64);
      v = v | 0x0000000400000000: uint(64);
      v = v | 0x0000800000000000: uint(64);
      rp(1) = v | 0x1000000000000000: uint(64);
      v = rp(2) | 0x0000000000000200: uint(64);
      v = v | 0x0000000000400000: uint(64);
      v = v | 0x0000000800000000: uint(64);
      v = v | 0x0001000000000000: uint(64);
      rp(2) = v | 0x2000000000000000: uint(64);
      v = rp(3) | 0x0000000000000400: uint(64);
      v = v | 0x0000000000800000: uint(64);
      v = v | 0x0000001000000000: uint(64);
      v = v | 0x0002000000000000: uint(64);
      rp(3) = v | 0x4000000000000000: uint(64);
      v = rp(4) | 0x0000000000000800: uint(64);
      v = v | 0x0000000001000000: uint(64);
      v = v | 0x0000002000000000: uint(64);
      v = v | 0x0004000000000000: uint(64);
      rp(4) = v | 0x8000000000000000: uint(64);
      v = rp(5) | 0x0000000000001000: uint(64);
      v = v | 0x0000000002000000: uint(64);
      v = v | 0x0000004000000000: uint(64);
      rp(5) = v | 0x0008000000000000: uint(64);
      v = rp(6) | 0x0000000000000001: uint(64);
      v = v | 0x0000000000002000: uint(64);
      v = v | 0x0000000004000000: uint(64);
      v = v | 0x0000008000000000: uint(64);
      rp(6) = v | 0x0010000000000000: uint(64);
      v = rp(7) | 0x0000000000000002: uint(64);
      v = v | 0x0000000000004000: uint(64);
      v = v | 0x0000000008000000: uint(64);
      v = v | 0x0000010000000000: uint(64);
      rp(7) = v | 0x0020000000000000: uint(64);
      v = rp(8) | 0x0000000000000004: uint(64);
      v = v | 0x0000000000008000: uint(64);
      v = v | 0x0000000010000000: uint(64);
      v = v | 0x0000020000000000: uint(64);
      rp(8) = v | 0x0040000000000000: uint(64);
      v = rp(9) | 0x0000000000000008: uint(64);
      v = v | 0x0000000000010000: uint(64);
      v = v | 0x0000000020000000: uint(64);
      v = v | 0x0000040000000000: uint(64);
      rp(9) = v | 0x0080000000000000: uint(64);
      v = rp(10) | 0x0000000000000010: uint(64);
      v = v | 0x0000000000020000: uint(64);
      v = v | 0x0000000040000000: uint(64);
      v = v | 0x0000080000000000: uint(64);
      rp(10) = v | 0x0100000000000000: uint(64);
      v = rp(11) | 0x0000000000000020: uint(64);
      v = v | 0x0000000000040000: uint(64);
      v = v | 0x0000000080000000: uint(64);
      v = v | 0x0000100000000000: uint(64);
      rp(11) = v | 0x0200000000000000: uint(64);
      v = rp(12) | 0x0000000000000040: uint(64);
      v = v | 0x0000000000080000: uint(64);
      v = v | 0x0000000100000000: uint(64);
      v = v | 0x0000200000000000: uint(64);
      rp(12) = v | 0x0400000000000000: uint(64);
      rp += stp;
    }
    biti = ((rp: int(64) - arrp: int(64)) << 3) + (biti & 63);
    while biti <= lmti { arrp(biti >> 3) |= BITMASK(biti & 7); biti += stp; }
    return biti;
  }
}

class URh15: UR {
  // hybrid bit setting by densely packed uint64's...
  override proc this(arrp: c_ptr(uint(8)), si: int, lmti: int, stp: int): int {
    const ilmt = si | 63; // make stop limit to next 64-bit boundary
    var biti = si;
    while biti <= ilmt { arrp(biti >> 3) |= BITMASK(biti & 7); biti += stp; }
    const rlmtp =
      c_ptrTo(arrp(((lmti >> 3) & (-8)) - 8 * (stp - 1))): c_ptr(uint(64));
    var rp = c_ptrTo(arrp((biti >> 3) & (-8))): c_ptr(uint(64));
    while rp: int(64) <= rlmtp: int(64) {
      var v = rp(0) | 0x0000000000002000: uint(64);
      v = v | 0x0000000010000000: uint(64);
      v = v | 0x0000080000000000: uint(64);
      rp(0) = v | 0x0400000000000000: uint(64);
      v = rp(1) | 0x0000000000000200: uint(64);
      v = v | 0x0000000001000000: uint(64);
      v = v | 0x0000008000000000: uint(64);
      rp(1) = v | 0x0040000000000000: uint(64);
      v = rp(2) | 0x0000000000000020: uint(64);
      v = v | 0x0000000000100000: uint(64);
      v = v | 0x0000000800000000: uint(64);
      rp(2) = v | 0x0004000000000000: uint(64);
      v = rp(3) | 0x0000000000000002: uint(64);
      v = v | 0x0000000000010000: uint(64);
      v = v | 0x0000000080000000: uint(64);
      v = v | 0x0000400000000000: uint(64);
      rp(3) = v | 0x2000000000000000: uint(64);
      v = rp(4) | 0x0000000000001000: uint(64);
      v = v | 0x0000000008000000: uint(64);
      v = v | 0x0000040000000000: uint(64);
      rp(4) = v | 0x0200000000000000: uint(64);
      v = rp(5) | 0x0000000000000100: uint(64);
      v = v | 0x0000000000800000: uint(64);
      v = v | 0x0000004000000000: uint(64);
      rp(5) = v | 0x0020000000000000: uint(64);
      v = rp(6) | 0x0000000000000010: uint(64);
      v = v | 0x0000000000080000: uint(64);
      v = v | 0x0000000400000000: uint(64);
      rp(6) = v | 0x0002000000000000: uint(64);
      v = rp(7) | 0x0000000000000001: uint(64);
      v = v | 0x0000000000008000: uint(64);
      v = v | 0x0000000040000000: uint(64);
      v = v | 0x0000200000000000: uint(64);
      rp(7) = v | 0x1000000000000000: uint(64);
      v = rp(8) | 0x0000000000000800: uint(64);
      v = v | 0x0000000004000000: uint(64);
      v = v | 0x0000020000000000: uint(64);
      rp(8) = v | 0x0100000000000000: uint(64);
      v = rp(9) | 0x0000000000000080: uint(64);
      v = v | 0x0000000000400000: uint(64);
      v = v | 0x0000002000000000: uint(64);
      rp(9) = v | 0x0010000000000000: uint(64);
      v = rp(10) | 0x0000000000000008: uint(64);
      v = v | 0x0000000000040000: uint(64);
      v = v | 0x0000000200000000: uint(64);
      v = v | 0x0001000000000000: uint(64);
      rp(10) = v | 0x8000000000000000: uint(64);
      v = rp(11) | 0x0000000000004000: uint(64);
      v = v | 0x0000000020000000: uint(64);
      v = v | 0x0000100000000000: uint(64);
      rp(11) = v | 0x0800000000000000: uint(64);
      v = rp(12) | 0x0000000000000400: uint(64);
      v = v | 0x0000000002000000: uint(64);
      v = v | 0x0000010000000000: uint(64);
      rp(12) = v | 0x0080000000000000: uint(64);
      v = rp(13) | 0x0000000000000040: uint(64);
      v = v | 0x0000000000200000: uint(64);
      v = v | 0x0000001000000000: uint(64);
      rp(13) = v | 0x0008000000000000: uint(64);
      v = rp(14) | 0x0000000000000004: uint(64);
      v = v | 0x0000000000020000: uint(64);
      v = v | 0x0000000100000000: uint(64);
      v = v | 0x0000800000000000: uint(64);
      rp(14) = v | 0x4000000000000000: uint(64);
      rp += stp;
    }
    biti = ((rp: int(64) - arrp: int(64)) << 3) + (biti & 63);
    while biti <= lmti { arrp(biti >> 3) |= BITMASK(biti & 7); biti += stp; }
    return biti;
  }
}

class URh17: UR {
  // hybrid bit setting by densely packed uint64's...
  override proc this(arrp: c_ptr(uint(8)), si: int, lmti: int, stp: int): int {
    const ilmt = si | 63; // make stop limit to next 64-bit boundary
    var biti = si;
    while biti <= ilmt { arrp(biti >> 3) |= BITMASK(biti & 7); biti += stp; }
    const rlmtp =
      c_ptrTo(arrp(((lmti >> 3) & (-8)) - 8 * (stp - 1))): c_ptr(uint(64));
    var rp = c_ptrTo(arrp((biti >> 3) & (-8))): c_ptr(uint(64));
    while rp: int(64) <= rlmtp: int(64) {
     var v = rp(0) | 0x0000000000000004: uint(64);
      v = v | 0x0000000000080000: uint(64);
      v = v | 0x0000001000000000: uint(64);
      rp(0) = v | 0x0020000000000000: uint(64);
      v = rp(1) | 0x0000000000000040: uint(64);
      v = v | 0x0000000000800000: uint(64);
      v = v | 0x0000010000000000: uint(64);
      rp(1) = v | 0x0200000000000000: uint(64);
      v = rp(2) | 0x0000000000000400: uint(64);
      v = v | 0x0000000008000000: uint(64);
      v = v | 0x0000100000000000: uint(64);
      rp(2) = v | 0x2000000000000000: uint(64);
      v = rp(3) | 0x0000000000004000: uint(64);
      v = v | 0x0000000080000000: uint(64);
      rp(3) = v | 0x0001000000000000: uint(64);
      v = rp(4) | 0x0000000000000002: uint(64);
      v = v | 0x0000000000040000: uint(64);
      v = v | 0x0000000800000000: uint(64);
      rp(4) = v | 0x0010000000000000: uint(64);
      v = rp(5) | 0x0000000000000020: uint(64);
      v = v | 0x0000000000400000: uint(64);
      v = v | 0x0000008000000000: uint(64);
      rp(5) = v | 0x0100000000000000: uint(64);
      v = rp(6) | 0x0000000000000200: uint(64);
      v = v | 0x0000000004000000: uint(64);
      v = v | 0x0000080000000000: uint(64);
      rp(6) = v | 0x1000000000000000: uint(64);
      v = rp(7) | 0x0000000000002000: uint(64);
      v = v | 0x0000000040000000: uint(64);
      rp(7) = v | 0x0000800000000000: uint(64);
      v = rp(8) | 0x0000000000000001: uint(64);
      v = v | 0x0000000000020000: uint(64);
      v = v | 0x0000000400000000: uint(64);
      rp(8) = v | 0x0008000000000000: uint(64);
      v = rp(9) | 0x0000000000000010: uint(64);
      v = v | 0x0000000000200000: uint(64);
      v = v | 0x0000004000000000: uint(64);
      rp(9) = v | 0x0080000000000000: uint(64);
      v = rp(10) | 0x0000000000000100: uint(64);
      v = v | 0x0000000002000000: uint(64);
      v = v | 0x0000040000000000: uint(64);
      rp(10) = v | 0x0800000000000000: uint(64);
      v = rp(11) | 0x0000000000001000: uint(64);
      v = v | 0x0000000020000000: uint(64);
      v = v | 0x0000400000000000: uint(64);
      rp(11) = v | 0x8000000000000000: uint(64);
      v = rp(12) | 0x0000000000010000: uint(64);
      v = v | 0x0000000200000000: uint(64);
      rp(12) = v | 0x0004000000000000: uint(64);
      v = rp(13) | 0x0000000000000008: uint(64);
      v = v | 0x0000000000100000: uint(64);
      v = v | 0x0000002000000000: uint(64);
      rp(13) = v | 0x0040000000000000: uint(64);
      v = rp(14) | 0x0000000000000080: uint(64);
      v = v | 0x0000000001000000: uint(64);
      v = v | 0x0000020000000000: uint(64);
      rp(14) = v | 0x0400000000000000: uint(64);
      v = rp(15) | 0x0000000000000800: uint(64);
      v = v | 0x0000000010000000: uint(64);
      v = v | 0x0000200000000000: uint(64);
      rp(15) = v | 0x4000000000000000: uint(64);
      v = rp(16) | 0x0000000000008000: uint(64);
      v = v | 0x0000000100000000: uint(64);
      rp(16) = v | 0x0002000000000000: uint(64);
      rp += stp;
    }
    biti = ((rp: int(64) - arrp: int(64)) << 3) + (biti & 63);
    while biti <= lmti { arrp(biti >> 3) |= BITMASK(biti & 7); biti += stp; }
    return biti;
  }
}

class URh19: UR {
  // hybrid bit setting by densely packed uint64's...
  override proc this(arrp: c_ptr(uint(8)), si: int, lmti: int, stp: int): int {
    const ilmt = si | 63; // make stop limit to next 64-bit boundary
    var biti = si;
    while biti <= ilmt { arrp(biti >> 3) |= BITMASK(biti & 7); biti += stp; }
    const rlmtp =
      c_ptrTo(arrp(((lmti >> 3) & (-8)) - 8 * (stp - 1))): c_ptr(uint(64));
    var rp = c_ptrTo(arrp((biti >> 3) & (-8))): c_ptr(uint(64));
    while rp: int(64) <= rlmtp: int(64) {
      var v = rp(0) | 0x0000000000000040: uint(64);
      v = v | 0x0000000002000000: uint(64);
      v = v | 0x0000100000000000: uint(64);
      rp(0) = v | 0x8000000000000000: uint(64);
      v = rp(1) | 0x0000000000040000: uint(64);
      v = v | 0x0000002000000000: uint(64);
      rp(1) = v | 0x0100000000000000: uint(64);
      v = rp(2) | 0x0000000000000800: uint(64);
      v = v | 0x0000000040000000: uint(64);
      rp(2) = v | 0x0002000000000000: uint(64);
      v = rp(3) | 0x0000000000000010: uint(64);
      v = v | 0x0000000000800000: uint(64);
      v = v | 0x0000040000000000: uint(64);
      rp(3) = v | 0x2000000000000000: uint(64);
      v = rp(4) | 0x0000000000010000: uint(64);
      v = v | 0x0000000800000000: uint(64);
      rp(4) = v | 0x0040000000000000: uint(64);
      v = rp(5) | 0x0000000000000200: uint(64);
      v = v | 0x0000000010000000: uint(64);
      rp(5) = v | 0x0000800000000000: uint(64);
      v = rp(6) | 0x0000000000000004: uint(64);
      v = v | 0x0000000000200000: uint(64);
      v = v | 0x0000010000000000: uint(64);
      rp(6) = v | 0x0800000000000000: uint(64);
      v = rp(7) | 0x0000000000004000: uint(64);
      v = v | 0x0000000200000000: uint(64);
      rp(7) = v | 0x0010000000000000: uint(64);
      v = rp(8) | 0x0000000000000080: uint(64);
      v = v | 0x0000000004000000: uint(64);
      rp(8) = v | 0x0000200000000000: uint(64);
      v = rp(9) | 0x0000000000000001: uint(64);
      v = v | 0x0000000000080000: uint(64);
      v = v | 0x0000004000000000: uint(64);
      rp(9) = v | 0x0200000000000000: uint(64);
      v = rp(10) | 0x0000000000001000: uint(64);
      v = v | 0x0000000080000000: uint(64);
      rp(10) = v | 0x0004000000000000: uint(64);
      v = rp(11) | 0x0000000000000020: uint(64);
      v = v | 0x0000000001000000: uint(64);
      v = v | 0x0000080000000000: uint(64);
      rp(11) = v | 0x4000000000000000: uint(64);
      v = rp(12) | 0x0000000000020000: uint(64);
      v = v | 0x0000001000000000: uint(64);
      rp(12) = v | 0x0080000000000000: uint(64);
      v = rp(13) | 0x0000000000000400: uint(64);
      v = v | 0x0000000020000000: uint(64);
      rp(13) = v | 0x0001000000000000: uint(64);
      v = rp(14) | 0x0000000000000008: uint(64);
      v = v | 0x0000000000400000: uint(64);
      v = v | 0x0000020000000000: uint(64);
      rp(14) = v | 0x1000000000000000: uint(64);
      v = rp(15) | 0x0000000000008000: uint(64);
      v = v | 0x0000000400000000: uint(64);
      rp(15) = v | 0x0020000000000000: uint(64);
      v = rp(16) | 0x0000000000000100: uint(64);
      v = v | 0x0000000008000000: uint(64);
      rp(16) = v | 0x0000400000000000: uint(64);
      v = rp(17) | 0x0000000000000002: uint(64);
      v = v | 0x0000000000100000: uint(64);
      v = v | 0x0000008000000000: uint(64);
      rp(17) = v | 0x0400000000000000: uint(64);
      v = rp(18) | 0x0000000000002000: uint(64);
      v = v | 0x0000000100000000: uint(64);
      rp(18) = v | 0x0008000000000000: uint(64);
      rp += stp;
    }
    biti = ((rp: int(64) - arrp: int(64)) << 3) + (biti & 63);
    while biti <= lmti { arrp(biti >> 3) |= BITMASK(biti & 7); biti += stp; }
    return biti;
  }
}

// a computed goto jump table using modulo pattern closure classes...
const hybrid = [ // note only cases 3, 5, 7 .. 19 are used; 9 and 15 never hit!
  new URr(0): OUR, new URr(1): OUR, new URr(2): OUR, new URh3(): OUR,
  new URr(4): OUR, new URh5(): OUR, new URr(6): OUR, new URh7(): OUR,
  new URr(8): OUR, new URh9(): OUR, new URr(10): OUR, new URh11(): OUR,
  new URr(12): OUR, new URh13(): OUR, new URr(14): OUR, new URh15(): OUR,
  new URr(16): OUR, new URh17(): OUR, new URr(18): OUR, new URh19(): OUR,
  new URr(20): OUR, new URr(21): OUR, new URr(22): OUR, new URr(23): OUR ];

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
      when Techniques.BitTwiddle do
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
            unrollr[((bp & 7) << 3) | (startndx & 7)]
              (bufferp, startndx, bitlmt, bp);
          }
        }
      when Techniques.UnrolledHybrid do
        for i in 0 .. bitlmt {
          if bufferp[i >> 3] & BITMASK[i & 7] == 0 {
            var startndx = (i + i) * (i + 3) + 3;
            if startndx > bitlmt then break;
            const bp = i + i + 3;
            if bp <= 19 then hybrid[bp](bufferp, startndx, bitlmt, bp);
            else unrollr[((bp & 7) << 3) | (startndx & 7)]
                   (bufferp, startndx, bitlmt, bp);
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
        if tech == Techniques.BitTwiddle then "bittwiddle;"
        else if tech == Techniques.Unpeeled then "unpeeled;"
        else if tech == Techniques.UnpeeledBlock then "unpeeled_block;"
        else if tech == Techniques.Unrolled then "unrolled;"
        else "unrolled_hybrid;";
    writeln(lbl, passes, ";", duration, ";1;algorithm=base,faithful=yes,bits=1");
  }
  else { writeln("Invalid result!"); }  
}

for t in Techniques do benchmark(t);

