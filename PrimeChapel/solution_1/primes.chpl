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

const HYBRID_THRESHOLD: int = 63; // dense base prime threshold

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

class URx: UR {
  param ndx: uint(8);
  param bp = (ndx: int(64) + ndx: int(64)) + 3;
  param strt = (bp * bp - 3) >> 1;
  param strtlmt = (strt + 64) & -64;
  param r = (strtlmt - strt) % bp;
  param s0 = if r == 0 then 0 else (bp - r) & 63;
  param m0 = 1: uint(64) << s0;
  param s1 = s0 + bp; param i1 = s1 >> 6; param m1 = 1: uint(64) << (s1 & 63);
  param s2 = s1 + bp; param i2 = s2 >> 6; param m2 = 1: uint(64) << (s2 & 63);
  param s3 = s2 + bp; param i3 = s3 >> 6; param m3 = 1: uint(64) << (s3 & 63);
  param s4 = s3 + bp; param i4 = s4 >> 6; param m4 = 1: uint(64) << (s4 & 63);
  param s5 = s4 + bp; param i5 = s5 >> 6; param m5 = 1: uint(64) << (s5 & 63);
  param s6 = s5 + bp; param i6 = s6 >> 6; param m6 = 1: uint(64) << (s6 & 63);
  param s7 = s6 + bp; param i7 = s7 >> 6; param m7 = 1: uint(64) << (s7 & 63);
  param s8 = s7 + bp; param i8 = s8 >> 6; param m8 = 1: uint(64) << (s8 & 63);
  param s9 = s8 + bp; param i9 = s9 >> 6; param m9 = 1: uint(64) << (s9 & 63);
  param s10 = s9 + bp; param i10 = s10 >> 6; param m10 = 1: uint(64) << (s10 & 63);
  param s11 = s10 + bp; param i11 = s11 >> 6; param m11 = 1: uint(64) << (s11 & 63);
  param s12 = s11 + bp; param i12 = s12 >> 6; param m12 = 1: uint(64) << (s12 & 63);
  param s13 = s12 + bp; param i13 = s13 >> 6; param m13 = 1: uint(64) << (s13 & 63);
  param s14 = s13 + bp; param i14 = s14 >> 6; param m14 = 1: uint(64) << (s14 & 63);
  param s15 = s14 + bp; param i15 = s15 >> 6; param m15 = 1: uint(64) << (s15 & 63);
  param s16 = s15 + bp; param i16 = s16 >> 6; param m16 = 1: uint(64) << (s16 & 63);
  param s17 = s16 + bp; param i17 = s17 >> 6; param m17 = 1: uint(64) << (s17 & 63);
  param s18 = s17 + bp; param i18 = s18 >> 6; param m18 = 1: uint(64) << (s18 & 63);
  param s19 = s18 + bp; param i19 = s19 >> 6; param m19 = 1: uint(64) << (s19 & 63);
  param s20 = s19 + bp; param i20 = s20 >> 6; param m20 = 1: uint(64) << (s20 & 63);
  param s21 = s20 + bp; param i21 = s21 >> 6; param m21 = 1: uint(64) << (s21 & 63);
  param s22 = s21 + bp; param i22 = s22 >> 6; param m22 = 1: uint(64) << (s22 & 63);
  param s23 = s22 + bp; param i23 = s23 >> 6; param m23 = 1: uint(64) << (s23 & 63);
  param s24 = s23 + bp; param i24 = s24 >> 6; param m24 = 1: uint(64) << (s24 & 63);
  param s25 = s24 + bp; param i25 = s25 >> 6; param m25 = 1: uint(64) << (s25 & 63);
  param s26 = s25 + bp; param i26 = s26 >> 6; param m26 = 1: uint(64) << (s26 & 63);
  param s27 = s26 + bp; param i27 = s27 >> 6; param m27 = 1: uint(64) << (s27 & 63);
  param s28 = s27 + bp; param i28 = s28 >> 6; param m28 = 1: uint(64) << (s28 & 63);
  param s29 = s28 + bp; param i29 = s29 >> 6; param m29 = 1: uint(64) << (s29 & 63);
  param s30 = s29 + bp; param i30 = s30 >> 6; param m30 = 1: uint(64) << (s30 & 63);
  param s31 = s30 + bp; param i31 = s31 >> 6; param m31 = 1: uint(64) << (s31 & 63);
  param s32 = s31 + bp; param i32 = s32 >> 6; param m32 = 1: uint(64) << (s32 & 63);
  param s33 = s32 + bp; param i33 = s33 >> 6; param m33 = 1: uint(64) << (s33 & 63);
  param s34 = s33 + bp; param i34 = s34 >> 6; param m34 = 1: uint(64) << (s34 & 63);
  param s35 = s34 + bp; param i35 = s35 >> 6; param m35 = 1: uint(64) << (s35 & 63);
  param s36 = s35 + bp; param i36 = s36 >> 6; param m36 = 1: uint(64) << (s36 & 63);
  param s37 = s36 + bp; param i37 = s37 >> 6; param m37 = 1: uint(64) << (s37 & 63);
  param s38 = s37 + bp; param i38 = s38 >> 6; param m38 = 1: uint(64) << (s38 & 63);
  param s39 = s38 + bp; param i39 = s39 >> 6; param m39 = 1: uint(64) << (s39 & 63);
  param s40 = s39 + bp; param i40 = s40 >> 6; param m40 = 1: uint(64) << (s40 & 63);
  param s41 = s40 + bp; param i41 = s41 >> 6; param m41 = 1: uint(64) << (s41 & 63);
  param s42 = s41 + bp; param i42 = s42 >> 6; param m42 = 1: uint(64) << (s42 & 63);
  param s43 = s42 + bp; param i43 = s43 >> 6; param m43 = 1: uint(64) << (s43 & 63);
  param s44 = s43 + bp; param i44 = s44 >> 6; param m44 = 1: uint(64) << (s44 & 63);
  param s45 = s44 + bp; param i45 = s45 >> 6; param m45 = 1: uint(64) << (s45 & 63);
  param s46 = s45 + bp; param i46 = s46 >> 6; param m46 = 1: uint(64) << (s46 & 63);
  param s47 = s46 + bp; param i47 = s47 >> 6; param m47 = 1: uint(64) << (s47 & 63);
  param s48 = s47 + bp; param i48 = s48 >> 6; param m48 = 1: uint(64) << (s48 & 63);
  param s49 = s48 + bp; param i49 = s49 >> 6; param m49 = 1: uint(64) << (s49 & 63);
  param s50 = s49 + bp; param i50 = s50 >> 6; param m50 = 1: uint(64) << (s50 & 63);
  param s51 = s50 + bp; param i51 = s51 >> 6; param m51 = 1: uint(64) << (s51 & 63);
  param s52 = s51 + bp; param i52 = s52 >> 6; param m52 = 1: uint(64) << (s52 & 63);
  param s53 = s52 + bp; param i53 = s53 >> 6; param m53 = 1: uint(64) << (s53 & 63);
  param s54 = s53 + bp; param i54 = s54 >> 6; param m54 = 1: uint(64) << (s54 & 63);
  param s55 = s54 + bp; param i55 = s55 >> 6; param m55 = 1: uint(64) << (s55 & 63);
  param s56 = s55 + bp; param i56 = s56 >> 6; param m56 = 1: uint(64) << (s56 & 63);
  param s57 = s56 + bp; param i57 = s57 >> 6; param m57 = 1: uint(64) << (s57 & 63);
  param s58 = s57 + bp; param i58 = s58 >> 6; param m58 = 1: uint(64) << (s58 & 63);
  param s59 = s58 + bp; param i59 = s59 >> 6; param m59 = 1: uint(64) << (s59 & 63);
  param s60 = s59 + bp; param i60 = s60 >> 6; param m60 = 1: uint(64) << (s60 & 63);
  param s61 = s60 + bp; param i61 = s61 >> 6; param m61 = 1: uint(64) << (s61 & 63);
  param s62 = s61 + bp; param i62 = s62 >> 6; param m62 = 1: uint(64) << (s62 & 63);
  param s63 = s62 + bp; param i63 = s63 >> 6; param m63 = 1: uint(64) << (s63 & 63);
  override proc this(arrp: c_ptr(uint(8)), si: int, lmti: int, stp: int): int {
    const ilmt = si | 63; // make stop limit to next 64-bit boundary
    var biti = si;
    while biti <= ilmt { arrp(biti >> 3) |= BITMASK(biti & 7); biti += stp; }
    const rlmtp =
      c_ptrTo(arrp(((lmti >> 3) & (-8)) - 8 * (stp - 1))): c_ptr(uint(64));
    var rp = c_ptrTo(arrp((biti >> 3) & (-8))): c_ptr(uint(64));
    while rp: int(64) <= rlmtp: int(64) {
      var v = rp(0) | m0: uint(64); rp(0) = v; v = rp(i1);
      v |= m1: uint(64); rp(i1) = v; v = rp(i2);
      v |= m2: uint(64); rp(i2) = v; v = rp(i3);
      v |= m3: uint(64); rp(i3) = v; v = rp(i4);
      v |= m4: uint(64); rp(i4) = v; v = rp(i5);
      v |= m5: uint(64); rp(i5) = v; v = rp(i6);
      v |= m6: uint(64); rp(i6) = v; v = rp(i7);
      v |= m7: uint(64); rp(i7) = v; v = rp(i8);
      v |= m8: uint(64); rp(i8) = v; v = rp(i9);
      v |= m9: uint(64); rp(i9) = v; v = rp(i10);
      v |= m10: uint(64); rp(i10) = v; v = rp(i11);
      v |= m11: uint(64); rp(i11) = v; v = rp(i12);
      v |= m12: uint(64); rp(i12) = v; v = rp(i13);
      v |= m13: uint(64); rp(i13) = v; v = rp(i14);
      v |= m14: uint(64); rp(i14) = v; v = rp(i15);
      v |= m15: uint(64); rp(i15) = v; v = rp(i16);
      v |= m16: uint(64); rp(i16) = v; v = rp(i17);
      v |= m17: uint(64); rp(i17) = v; v = rp(i18);
      v |= m18: uint(64); rp(i18) = v; v = rp(i19);
      v |= m19: uint(64); rp(i19) = v; v = rp(i20);
      v |= m20: uint(64); rp(i20) = v; v = rp(i21);
      v |= m21: uint(64); rp(i21) = v; v = rp(i22);
      v |= m22: uint(64); rp(i22) = v; v = rp(i23);
      v |= m23: uint(64); rp(i23) = v; v = rp(i24);
      v |= m24: uint(64); rp(i24) = v; v = rp(i25);
      v |= m25: uint(64); rp(i25) = v; v = rp(i26);
      v |= m26: uint(64); rp(i26) = v; v = rp(i27);
      v |= m27: uint(64); rp(i27) = v; v = rp(i28);
      v |= m28: uint(64); rp(i28) = v; v = rp(i29);
      v |= m29: uint(64); rp(i29) = v; v = rp(i30);
      v |= m30: uint(64); rp(i30) = v; v = rp(i31);
      v |= m31: uint(64); rp(i31) = v; v = rp(i32);
      v |= m32: uint(64); rp(i32) = v; v = rp(i33);
      v |= m33: uint(64); rp(i33) = v; v = rp(i34);
      v |= m34: uint(64); rp(i34) = v; v = rp(i35);
      v |= m35: uint(64); rp(i35) = v; v = rp(i36);
      v |= m36: uint(64); rp(i36) = v; v = rp(i37);
      v |= m37: uint(64); rp(i37) = v; v = rp(i38);
      v |= m38: uint(64); rp(i38) = v; v = rp(i39);
      v |= m39: uint(64); rp(i39) = v; v = rp(i40);
      v |= m40: uint(64); rp(i40) = v; v = rp(i41);
      v |= m41: uint(64); rp(i41) = v; v = rp(i42);
      v |= m42: uint(64); rp(i42) = v; v = rp(i43);
      v |= m43: uint(64); rp(i43) = v; v = rp(i44);
      v |= m44: uint(64); rp(i44) = v; v = rp(i45);
      v |= m45: uint(64); rp(i45) = v; v = rp(i46);
      v |= m46: uint(64); rp(i46) = v; v = rp(i47);
      v |= m47: uint(64); rp(i47) = v; v = rp(i48);
      v |= m48: uint(64); rp(i48) = v; v = rp(i49);
      v |= m49: uint(64); rp(i49) = v; v = rp(i50);
      v |= m50: uint(64); rp(i50) = v; v = rp(i51);
      v |= m51: uint(64); rp(i51) = v; v = rp(i52);
      v |= m52: uint(64); rp(i52) = v; v = rp(i53);
      v |= m53: uint(64); rp(i53) = v; v = rp(i54);
      v |= m54: uint(64); rp(i54) = v; v = rp(i55);
      v |= m55: uint(64); rp(i55) = v; v = rp(i56);
      v |= m56: uint(64); rp(i56) = v; v = rp(i57);
      v |= m57: uint(64); rp(i57) = v; v = rp(i58);
      v |= m58: uint(64); rp(i58) = v; v = rp(i59);
      v |= m59: uint(64); rp(i59) = v; v = rp(i60);
      v |= m60: uint(64); rp(i60) = v; v = rp(i61);
      v |= m61: uint(64); rp(i61) = v; v = rp(i62);
      v |= m62: uint(64); rp(i62) = v; v = rp(i63);
      rp(i63) = v | m63: uint(64);
      rp += stp;
    }
    biti = ((rp: int(64) - arrp: int(64)) << 3) + (biti & 63);
    while biti <= lmti { arrp(biti >> 3) |= BITMASK(biti & 7); biti += stp; }
    return biti;
  }
}

// a computed goto jump table using modulo pattern closure classes...
const hybrid = [
  new URx(0): OUR, new URx(1): OUR, new URx(2): OUR, new URx(3): OUR,
  new URx(4): OUR, new URx(5): OUR, new URx(6): OUR, new URx(7): OUR,
  new URx(8): OUR, new URx(9): OUR, new URx(10): OUR, new URx(11): OUR,
  new URx(12): OUR, new URx(13): OUR, new URx(14): OUR, new URx(15): OUR,
  new URx(16): OUR, new URx(17): OUR, new URx(18): OUR, new URx(19): OUR,
  new URx(20): OUR, new URx(21): OUR, new URx(22): OUR, new URx(23): OUR,
  new URx(24): OUR, new URx(25): OUR, new URx(26): OUR, new URx(27): OUR,
  new URx(28): OUR, new URx(29): OUR, new URx(30): OUR, new URx(31): OUR ];

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
            if bp <= HYBRID_THRESHOLD then
              hybrid[i](bufferp, startndx, bitlmt, bp);
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
        else if tech == Techniques.Unpeeled then "stride8;"
        else if tech == Techniques.UnpeeledBlock then "stride8_block16K;"
        else if tech == Techniques.Unrolled then "extreme;"
        else "extreme_hybrid;";
    writeln(lbl, passes, ";", duration, ";1;algorithm=base,faithful=yes,bits=1");
  }
  else { writeln("Invalid result:  ", count, " should be ", EXPECTED, "!"); }  
}

for t in Techniques do benchmark(t);
