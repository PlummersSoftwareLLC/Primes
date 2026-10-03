/-!
  # Prime sieve — base algorithm, striped 1-bit marking, Lean 4

  Storage is odd-only and bit-packed: bit `k` denotes the number `2k+1`, so the
  buffer is exactly `⌈(1 + sieveSize / 2) / 8⌉` bytes. Marking logic is inverted
  (`0` means "still a prime candidate"), which the base rules permit and which
  means a freshly zeroed buffer is already the correct initial state.

  ## Striped marking

  The obvious way to set bit `i` is `bits[i / 8] |= 1 <<< (i % 8)`, which pays a
  divide and a shift on every write. The multiples of a base prime `p` can
  instead be split into eight interleaved subsequences by `i % 8`. Within one
  subsequence the bit position never changes — advancing eight steps of `p` in
  bit space advances exactly `p` in byte space — so each subsequence is a loop
  whose mask and byte stride are both **loop-invariant**, computed once from the
  discovered prime:

      for b := start/8, start/8 + p, start/8 + 2p, …   bits[b] |= mask

  No bit arithmetic survives in the hot loop. This is the same transformation
  used by the Rust, Nim, Java and F# entries, and like them it remains
  `algorithm=base`: each store clears one composite, only the order changes.

  Measured against a byte-per-flag sieve of the same structure on an Apple M4
  Max, striped 1-bit runs about 1.38x faster. Three effects combine: the buffer
  drops from 500001 to 62501 bytes and so fits in L1 (128 KB here, which the
  byte buffer never did), the zero-fill shrinks eightfold, and the per-write
  index arithmetic disappears. A *non*-striped 1-bit sieve is about 20% slower
  than the byte version, so the striping is what makes bit packing pay.

  ## Dense marking for the smallest primes

  Striping issues one store per composite. For `p = 3` and `p = 5` a single byte
  holds two or three multiples of `p`, so those stores repeatedly read, modify
  and write the same byte. Marking such a prime densely instead — walking every
  byte once and OR-ing a mask that carries all of that prime's bits in it —
  costs one store per byte regardless of `p`.

  Which bits a byte needs depends only on its index modulo `p`, because a byte
  advances the bit index by 8. So one period of `p` masks is composed per prime,
  after that prime has been discovered from the sieve, by stepping it eight times
  and setting one bit per step. The byte loop is then unrolled `p` times so each
  mask reaches its store as a loop-invariant runtime value rather than something
  re-derived per byte.

  Dense marking only pays while a prime has more than one multiple per byte.
  Measured cost for clearing the multiples of 3 across the whole buffer is
  47.7 µs striped against 21.8 µs dense, and dense stores are marginally more
  expensive each (0.35 ns against 0.29 ns), which puts break-even just under
  `p = 7`. So 3 and 5 are marked densely and everything from 7 up is striped.

  This is still `algorithm=base`, because every bit of every mask is an
  identifiable marking operation in the source. `periodMasks` composes a mask by
  stepping the discovered prime through one period and setting one bit per step
  with `1 <<< (i % 8)`; nothing arrives as finished bitmap data, and no mask is
  built before the prime it belongs to has been found in the sieve.

  The two halves are worth separating, because it is the first that earns the
  label. Composing the mask steps the prime through one period, one multiple at
  a time. Applying it then walks the buffer once per byte, so a single store can
  clear several of that prime's multiples at once. Each mask carries bits for one
  prime only.

  The precedents are PrimeV/solution_2, whose `dense_bitset` composes per-bit
  with single-bit literals, and PrimeAssembly/solution_4's
  `cwager_x64ff_mt_extreme_maskgen_onefactor`, which builds one scratch mask per
  runtime-discovered factor with visible per-multiple bit operations and then
  applies it — the closer match, since it composes and then applies rather than
  fusing the two. The Nim entry's `denseSetBits` and PrimeSwift/solution_1's
  dense small-factor path are the same work in fused form, OR-ing each multiple
  into a register holding the sieve word before committing it; their thresholds
  follow from word granularity rather than from anything about classification.

  Lean has no packed word array to mark through (`ByteArray` is byte-addressed
  and `Array UInt64` boxes every element), so byte granularity is the limit here.

  ## Why this shape of Lean code

  Three techniques are Lean-specific and look unusual next to the C and Rust
  solutions.

  **Bounds checks are discharged by proof, not disabled.** The loops carry an
  erased proof that the limit is within the buffer and derive each access's
  obligation from the loop comparison already made, so `uget`/`uset` compile to
  plain indexed accesses. The safe `get!`/`set!` would repeat that comparison at
  run time. This moves the check to compile time rather than skipping it, and
  Lean's equivalent of `-no-bounds-check` is not used.

  **The zero-fill is hand-rolled** because Lean has no `ByteArray` zero-fill
  primitive, and the obvious `⟨Array.replicate n 0⟩` first builds a boxed
  `Array UInt8` at one machine word per element. `growExact` doubles an owned
  `ByteArray` instead, staying packed, and does one final partial append so the
  result is exactly the requested size rather than rounded up to a power of two
  — the rules require the buffer to correspond to the sieve size.

  **The loops are tail-recursive rather than `while` loops over mutable state.**
  A `USize` held as a mutable binding is heap-allocated every iteration: the
  generated C shows a `lean_box_usize` call in the loop body, because a `USize`
  cannot be a tagged immediate the way a small `Nat` can. That form costs a
  malloc per marked composite. As function parameters these are unboxed
  `size_t`.
-/

/-! ## Bounds lemmas -/

private theorem bsizeUset {a : ByteArray} {i : USize} {v : UInt8} (h : i.toNat < a.size) :
    (a.uset i v h).size = a.size :=
  Array.size_uset (xs := a.data) h

private theorem bnd {bits : ByteArray} {i lim : USize}
    (hi : i < lim) (hlim : lim.toNat ≤ bits.size) : i.toNat < bits.size :=
  Nat.lt_of_lt_of_le (USize.lt_iff_toNat_lt.mp hi) hlim

/-! ## Allocation -/

private partial def growExact (acc : ByteArray) (n : Nat) : ByteArray :=
  let s := acc.size
  if s ≥ n then acc
  else if 2 * s ≤ n then growExact (acc ++ acc) n
  else acc ++ acc.extract 0 (n - s)

private def zeroExact (n : Nat) : ByteArray :=
  if n == 0 then ByteArray.empty else growExact (ByteArray.empty.push 0) n

/-! ## Marking -/

/-- One stripe: `mask` and the byte stride are loop-invariant, both derived from
    the base prime. This is the hot loop, and it contains no bit arithmetic. -/
private partial def markStripe (bits : ByteArray) (b step lim : USize) (mask : UInt8)
    (hlim : lim.toNat ≤ bits.size) : ByteArray :=
  if hb : b < lim then
    let h := bnd hb hlim
    markStripe (bits.uset b (bits.uget b h ||| mask) h) (b + step) step lim mask
      (by rw [bsizeUset]; exact hlim)
  else bits

/-- Runs the eight stripes for base prime `p`, whose first multiple to clear is
    at bit `start`. Stripe `s` begins at bit `start + s * p`; since those are
    increasing in `s`, the first one past the end ends the sweep. -/
private partial def markStriped (bits : ByteArray) (start p lenBits stripe : Nat) : ByteArray :=
  if stripe ≥ 8 then bits
  else
    let bitIdx := start + stripe * p
    if bitIdx ≥ lenBits then bits
    else
      let bitPos := bitIdx % 8
      -- Largest byte index for which `bitPos` is still a bit below `lenBits`.
      let byteLim := (lenBits - bitPos + 7) / 8
      let mask : UInt8 := 1 <<< bitPos.toUInt8
      let bits :=
        if h : byteLim.toUSize.toNat ≤ bits.size then
          markStripe bits (bitIdx / 8).toUSize p.toUSize byteLim.toUSize mask h
        else bits
      markStriped bits start p lenBits (stripe + 1)

/-! ### Dense marking

Everything below serves primes 3 and 5, where a byte holds more than one
multiple. -/

/-- One guarded OR into a byte. Not recursive, so unlike the loops its effect on
    size is provable, which lets the unrolled loop below thread a single
    invariant rather than re-deriving one per store. -/
private def orAt (bits : ByteArray) (i lim : USize) (mask : UInt8)
    (hlim : lim.toNat ≤ bits.size) : ByteArray :=
  if h : i < lim then bits.uset i (bits.uget i (bnd h hlim) ||| mask) (bnd h hlim) else bits

private theorem size_orAt {bits : ByteArray} {i lim : USize} {mask : UInt8}
    {hlim : lim.toNat ≤ bits.size} : (orAt bits i lim mask hlim).size = bits.size := by
  unfold orAt
  split
  · exact bsizeUset _
  · rfl

/-- Marks bits `i, i + p, i + 2p, …` below `limBits` one at a time. Used only
    for the handful of multiples below the first whole byte of a dense sweep. -/
private partial def markBits (bits : ByteArray) (i p limBits : Nat) : ByteArray :=
  if i ≥ limBits then bits
  else
    let b := (i / 8).toUSize
    let bits :=
      if h : b.toNat < bits.size then
        bits.uset b (bits.uget b h ||| (1 <<< (i % 8).toUInt8)) h
      else bits
    markBits bits (i + p) p limBits

/-- One period of masks for prime `p`, covering bytes `b0 … b0 + p - 1`.

    Composed one multiple at a time: each step advances the bit index by `p` and
    sets that one bit, so every bit in the finished masks corresponds to an
    identifiable marking operation here. The masks are built only after `p` has
    been discovered from the sieve, and are working storage rather than a
    precomputed table.

    A window of `p` bytes spans `8p` bits and so contains exactly eight
    multiples of `p`; `i` starts at the lowest one at or above byte `b0`, so all
    eight land inside the window. -/
private partial def periodMasks (acc : ByteArray) (i p b0 left : Nat) : ByteArray :=
  if left == 0 then acc
  else
    let j := i / 8 - b0
    periodMasks (acc.set! j (acc.get! j ||| (1 <<< (i % 8).toUInt8))) (i + p) p b0 (left - 1)

/-- Dense byte loop for `p = 3`: the three masks composed for this prime, applied
    in rotation, byte index stepping by 3. -/
private partial def markDense3 (bits : ByteArray) (b lim : USize) (m0 m1 m2 : UInt8)
    (hlim : lim.toNat ≤ bits.size) : ByteArray :=
  if b ≥ lim then bits
  else
    let b1 := orAt bits b lim m0 hlim
    have h1 : lim.toNat ≤ b1.size := by rw [size_orAt]; exact hlim
    let b2 := orAt b1 (b + 1) lim m1 h1
    have h2 : lim.toNat ≤ b2.size := by rw [size_orAt]; exact h1
    let b3 := orAt b2 (b + 2) lim m2 h2
    have h3 : lim.toNat ≤ b3.size := by rw [size_orAt]; exact h2
    markDense3 b3 (b + 3) lim m0 m1 m2 h3

/-- Dense byte loop for `p = 5`: the five masks composed for this prime, applied
    in rotation, byte index stepping by 5. -/
private partial def markDense5 (bits : ByteArray) (b lim : USize) (m0 m1 m2 m3 m4 : UInt8)
    (hlim : lim.toNat ≤ bits.size) : ByteArray :=
  if b ≥ lim then bits
  else
    let b1 := orAt bits b lim m0 hlim
    have h1 : lim.toNat ≤ b1.size := by rw [size_orAt]; exact hlim
    let b2 := orAt b1 (b + 1) lim m1 h1
    have h2 : lim.toNat ≤ b2.size := by rw [size_orAt]; exact h1
    let b3 := orAt b2 (b + 2) lim m2 h2
    have h3 : lim.toNat ≤ b3.size := by rw [size_orAt]; exact h2
    let b4 := orAt b3 (b + 3) lim m3 h3
    have h4 : lim.toNat ≤ b4.size := by rw [size_orAt]; exact h3
    let b5 := orAt b4 (b + 4) lim m4 h4
    have h5 : lim.toNat ≤ b5.size := by rw [size_orAt]; exact h4
    markDense5 b5 (b + 5) lim m0 m1 m2 m3 m4 h5

/-- Marks every multiple of a small `p` from bit `start`: the multiples below the
    first whole byte one at a time, then one store per byte.

    The dense loop runs to the end of the buffer, so it can set padding bits
    above `lenBits` in the final byte. Those are never read — counting stops at
    `(sieveSize + 1) / 2 ≤ lenBits`, and the scan for the next prime stops at
    `lenBits` and never gets past `√sieveSize / 2` anyway. -/
private def markDenseSmall (bits : ByteArray) (start p lenBits nbytes : Nat) : ByteArray :=
  let b0 := start / 8 + 1
  if b0 ≥ nbytes then markBits bits start p lenBits
  else
    -- Lowest multiple of `p` at or above byte `b0`, and the leading ones below it.
    let first := start + ((8 * b0 - start) + p - 1) / p * p
    let bits := markBits bits start p (8 * b0)
    let m := periodMasks (zeroExact p) first p b0 8
    if hn : nbytes.toUSize.toNat ≤ bits.size then
      if p == 3 then
        markDense3 bits b0.toUSize nbytes.toUSize (m.get! 0) (m.get! 1) (m.get! 2) hn
      else
        markDense5 bits b0.toUSize nbytes.toUSize
          (m.get! 0) (m.get! 1) (m.get! 2) (m.get! 3) (m.get! 4) hn
    else bits

/-- First unmarked bit at or after `i`. Only runs about once per prime, so the
    per-bit arithmetic here is not on the hot path. -/
private partial def findNextUnset (bits : ByteArray) (i lenBits nbytes : USize)
    (hn : nbytes.toNat ≤ bits.size) : USize :=
  if i < lenBits then
    if hb : (i / 8) < nbytes then
      if (bits.uget (i / 8) (bnd hb hn) >>> (i % 8).toUInt8) &&& 1 == 0 then i
      else findNextUnset bits (i + 1) lenBits nbytes hn
    else lenBits
  else lenBits

/-- Outer loop: find the next prime, clear its multiples, repeat while
    `factor² ≤ sieveSize`. The length invariant is re-established once per
    iteration and threaded into the inner loops as an erased proof. -/
private partial def sieveLoop (bits : ByteArray) (f size : USize) (lenBits nbytes : Nat) :
    ByteArray :=
  if f * f > size then bits
  else if hn : nbytes.toUSize.toNat ≤ bits.size then
    let f' := (findNextUnset bits (f / 2) lenBits.toUSize nbytes.toUSize hn) * 2 + 1
    let p := f'.toNat
    -- `p` is odd, so `p * p / 2` truncates to the bit index of `p * p`.
    let start := p * p / 2
    let bits :=
      if p ≤ 5 then markDenseSmall bits start p lenBits nbytes
      else markStriped bits start p lenBits 0
    sieveLoop bits (f' + 2) size lenBits nbytes
  else bits

/-! ## Sieve -/

/-- Full state of one sieve run. `bits` holds odd-only flags, bit-packed: bit
    `k` represents the number `2k+1`, and `0` means "still a prime candidate". -/
structure Sieve where
  sieveSize : Nat
  bits : ByteArray

namespace Sieve

/-- Allocates the buffer at exactly one bit per odd candidate. -/
def create (n : Nat) : Sieve :=
  { sieveSize := n, bits := zeroExact ((1 + n / 2 + 7) / 8) }

/-- Runs the sieve over this instance's buffer and returns the flags.

    The buffer has to stay uniquely referenced for `uset` to update it in place
    rather than copying on first write. -/
def run : Sieve → Sieve
  | { sieveSize := n, bits := b } =>
    let lenBits := 1 + n / 2
    { sieveSize := n, bits := sieveLoop b 3 n.toUSize lenBits ((lenBits + 7) / 8) }

private partial def countFrom (bits : ByteArray) (i lim acc : Nat) : Nat :=
  if i ≥ lim then acc
  else
    let bit := (bits.get! (i / 8) >>> (i % 8).toUInt8) &&& 1
    countFrom bits (i + 1) lim (if bit == 0 then acc + 1 else acc)

/-- Counts primes: 2, plus every unmarked odd candidate above 1. -/
def countPrimes (s : Sieve) : Nat :=
  if s.sieveSize < 2 then 0 else countFrom s.bits 1 ((s.sieveSize + 1) / 2) 1

end Sieve

/-! ## Harness -/

private def solutionLabel : String := "ronald-d-rogers_lean4_striped1"

private def knownCounts : List (Nat × Nat) :=
  [(10, 4), (100, 25), (1000, 168), (10000, 1229), (100000, 9592),
   (1000000, 78498), (10000000, 664579)]

/-- `Sieve.create sz` is pure and loop-invariant, so reading the size back
    through a ref each pass keeps the compiler from hoisting the whole sieve out
    of the timing loop and running it once. This is the same guard the C
    solutions get from `volatile` and the Rust ones from `black_box`: one
    pointer load per pass, and it does not touch the sieve. -/
private def timedRun (sizeRef : IO.Ref Nat) (seconds : Nat) : IO (Nat × Float) := do
  let start ← IO.monoNanosNow
  let deadline := start + seconds * 1000000000
  let mut passes : Nat := 0
  let mut now := start
  while now < deadline do
    let sz ← sizeRef.get
    let s := (Sieve.create sz).run
    if s.bits.size == 0 then IO.println "unreachable"
    passes := passes + 1
    now ← IO.monoNanosNow
  return (passes, (now - start).toFloat / 1000000000.0)

def main (args : List String) : IO Unit := do
  let sieveSize : Nat := 1000000

  if args.contains "--validate" then
    for (n, expected) in knownCounts do
      let s := (Sieve.create n).run
      let c := s.countPrimes
      IO.println s!"n={n} buffer={s.bits.size}B count={c} expected={expected} \
        {if c == expected then "PASS" else "FAIL"}"
    return

  let v := (Sieve.create sieveSize).run
  let count := v.countPrimes
  IO.eprintln s!"validate: sieveSize={sieveSize} bufferBytes={v.bits.size} \
    count={count} expected=78498 {if count == 78498 then "PASS" else "FAIL"}"

  let sizeRef ← IO.mkRef sieveSize
  let (passes, duration) ← timedRun sizeRef 5
  IO.println s!"{solutionLabel};{passes};{duration};1;algorithm=base,faithful=yes,bits=1"
