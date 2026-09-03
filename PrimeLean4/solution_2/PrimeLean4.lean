/-!
  Prime sieve — faithful base implementation, 8-bit flags.

  PlummersSoftwareLLC/Primes drag race, `PrimeLean4/solution_2`.

  This is a performance-oriented rewrite of `PrimeLean4/solution_1`
  (badly-drawn-wizards), which was written against `leanprover/lean4:4.0.0-m2`
  and states that "not much has been done to optimize this implementation".
  The algorithm is unchanged; only its encoding in Lean is different.

  Compliance notes (CONTRIBUTING.md § Faithfulness):
  * `Sieve` holds the full sieve state and a fresh instance is created per pass.
  * The flag buffer is allocated at runtime with size exactly `1 + sieveSize / 2`
    (odd-only storage, one byte per candidate → bits=8).
  * Base algorithm: outer loop finds the next odd factor from 3, then clears its
    multiples stepping by `2 * factor`. Storage index `k` denotes the odd number
    `2k+1`, so stepping the index by `factor` *is* stepping the number by
    `2 * factor`.
  * Runs for at least 5 seconds and stops after the pass that crosses it.
  * No external dependencies; no FFI — sieve, timing loop and counting are Lean.
-/

/-- Solution label reported on the output line. Convention is to identify the
    author, as `solution_1` does with `badly-drawn-wizards`. -/
def solutionLabel : String := "ronald-d-rogers_lean4_bool8"

private theorem bsizeUset {a : ByteArray} {i : USize} {v : UInt8} (h : i.toNat < a.size) :
    (a.uset i v h).size = a.size :=
  Array.size_uset (xs := a.data) h

/-- The `uset`/`uget` bounds obligation, derived from the loop comparison.
    Threading this as an erased proof keeps the hot loops free of runtime
    bounds checks, so `uget`/`uset` compile to plain indexed loads and stores.
    This is a compile-time argument, not a runtime check that has been
    switched off: the safe `get!`/`set!` operations would perform the same
    comparison again at run time. -/
private theorem bnd {bits : ByteArray} {i len : USize}
    (hi : i < len) (hlen : len.toNat ≤ bits.size) : i.toNat < bits.size :=
  Nat.lt_of_lt_of_le (USize.lt_iff_toNat_lt.mp hi) hlen

/-- Zero buffer of *exactly* `n` bytes: double while it still fits, then one
    partial append.

    Lean has no native `ByteArray` zero-fill. The obvious
    `⟨Array.replicate n 0⟩` would first build a boxed `Array UInt8` — eight
    bytes per element, so a 4 MB intermediate for a 500 KB buffer — and then
    pack it down. Doubling an owned `ByteArray` stays in packed representation
    throughout. The result is the exact requested size; the final partial
    append trims to `n` rather than rounding up to a power of two. -/
private partial def growExact (acc : ByteArray) (n : Nat) : ByteArray :=
  let s := acc.size
  if s ≥ n then acc
  else if 2 * s ≤ n then growExact (acc ++ acc) n
  else acc ++ acc.extract 0 (n - s)

private def zeroExact (n : Nat) : ByteArray :=
  if n == 0 then ByteArray.empty else growExact (ByteArray.empty.push 0) n

/-- Scan upward for the next unmarked index. -/
private partial def findNextUnset (bits : ByteArray) (i len : USize)
    (hlen : len.toNat ≤ bits.size) : USize :=
  if hi : i < len then
    if bits.uget i (bnd hi hlen) == 0 then i
    else findNextUnset bits (i + 1) len hlen
  else len

/-- Clear multiples: store at `i`, then advance the index by `step`. -/
private partial def markFrom (bits : ByteArray) (i step len : USize)
    (hlen : len.toNat ≤ bits.size) : ByteArray :=
  if hi : i < len then
    markFrom (bits.uset i 1 (bnd hi hlen)) (i + step) step len
      (by rw [bsizeUset]; exact hlen)
  else bits

/-- Tail-recursive rather than a `for`/`while` loop with mutable bindings:
    `USize` locals carried as mutable state get heap-boxed, whereas `USize`
    function parameters are passed unboxed as `size_t`. -/
private partial def sieveLoop (bits : ByteArray) (f size len : USize) : ByteArray :=
  if f * f > size then bits
  else if hlen : len.toNat ≤ bits.size then
    let f' := (findNextUnset bits (f / 2) len hlen) * 2 + 1
    sieveLoop (markFrom bits (f' * f' / 2) f' len hlen) (f' + 2) size len
  else bits

/-- Full state of one sieve run. `bits` holds odd-only flags: index `k`
    represents the number `2k+1`, and `0` means "still a prime candidate". -/
structure Sieve where
  sieveSize : Nat
  bits : ByteArray

namespace Sieve

def create (n : Nat) : Sieve :=
  { sieveSize := n, bits := zeroExact (1 + n / 2) }

/-- Destructuring (rather than `{ s with .. }`) consumes the instance, so the
    buffer stays uniquely referenced and every `uset` updates it in place
    instead of copying on first write. -/
def run : Sieve → Sieve
  | { sieveSize := n, bits := b } =>
    { sieveSize := n, bits := sieveLoop b 3 n.toUSize (1 + n / 2).toUSize }

private partial def countFrom (bits : ByteArray) (i lim acc : Nat) : Nat :=
  if i ≥ lim then acc
  else countFrom bits (i + 1) lim (if bits.get! i == 0 then acc + 1 else acc)

/-- Count of primes ≤ `sieveSize`, counted after the run (2, plus unmarked odds). -/
def countPrimes (s : Sieve) : Nat :=
  if s.sieveSize < 2 then 0
  else countFrom s.bits 1 ((s.sieveSize + 1) / 2) 1

end Sieve

def main : IO Unit := do
  let sieveSize : Nat := 1000000
  let expected : Nat := 78498

  let validated := (Sieve.create sieveSize).run
  let validation := validated.countPrimes
  IO.eprintln s!"validate: sieveSize={sieveSize} bufferBytes={validated.bits.size} \
    count={validation} expected={expected} \
    {if validation == expected then "PASS" else "FAIL"}"

  -- The sieve size is read back through a ref on every pass. `Sieve.create sz`
  -- is otherwise loop-invariant and pure, so the compiler is free to hoist it
  -- out of the timing loop and run the sieve once. This is the same guard the
  -- C solutions get from `volatile` and the Rust ones from `black_box`; it adds
  -- one pointer load per pass and does not touch the sieve itself.
  let sizeRef ← IO.mkRef sieveSize
  let start ← IO.monoNanosNow
  let deadline := start + 5000000000
  let mut passes : Nat := 0
  let mut now := start
  while now < deadline do
    let sz ← sizeRef.get
    let s := (Sieve.create sz).run
    if s.bits.size == 0 then IO.println "unreachable"
    passes := passes + 1
    now ← IO.monoNanosNow
  let duration := (now - start).toFloat / 1000000000.0

  IO.println s!"{solutionLabel};{passes};{duration};1;algorithm=base,faithful=yes,bits=8"
