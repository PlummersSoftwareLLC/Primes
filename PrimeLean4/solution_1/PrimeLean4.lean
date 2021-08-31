import Init

/- From lean4 tests -/
structure Loop where
@[inline]
partial def Loop.forIn {β : Type u} {m : Type u → Type v} [Monad m] (loop : Loop) (init : β) (f : Unit → β → m (ForInStep β)) : m β :=
  let rec @[specialize] loop (b : β) : m β := do
    match ← f () b with
      | ForInStep.done b  => pure b
      | ForInStep.yield b => loop b
  loop init
instance : ForIn m Loop Unit where
  forIn := Loop.forIn


def le_sqrt (x yy : Nat) := x*x ≤ yy

class BitArray (α : Sort u) where
  init : Nat → α
  size : α → Nat
  get : α → Nat → Bool 
  set : α → Nat → Bool → α
open BitArray

instance : BitArray (Array Bool) where
  init n := mkArray n false
  size α := α.size
  get := Array.get!
  set := Array.set!

structure PrimeBitArray (α : Sort u) := 
  size : Nat
  array : α

instance (α : Sort u) [BitArray α] : BitArray (PrimeBitArray α) where
  init n := ⟨n, init (1 + n / 2)⟩
  size α := α.size
  get α n := get α.array (n / 2)
  set α i v := if i % 2 == 0 then α else ⟨α.size, set α.array (i / 2) v⟩

def count_primes {α} [BitArray α] (array : α) : Nat := do
  let mut cnt : Nat := 1
  for num in [3 : size array : 2] do
    if !(get array num) then cnt := cnt+1
  pure cnt

def run_sieve {α} [BitArray α] (size : Nat) : α := do
  let mut primes : α := init size
  let mut factor : Nat := 3
  let q : Float := Float.sqrt size.toFloat
  for _ in Loop.mk do
    if !(factor.toFloat <= q) then break
    for num in [factor:size : 2] do
      if !(get primes num) then do
        factor := num
        break
    for num in [factor * factor : size : factor*2] do
      primes := set primes num true
    factor := factor + 2

  pure primes

structure Tag :=
  name : String
  value : String

instance : ToString Tag where
  toString tag := s!"{tag.name}={tag.value}"


structure ImplMetadata :=
  label : String
  num_threads : Nat
  tags : List Tag

structure Impl extends ImplMetadata :=
  array : Type
  run : Nat → array

structure Output :=
  impl : ImplMetadata
  iterations : Nat
  total_ms : Nat

instance : ToString Output where
  toString o := 
    let tags := ",".intercalate (o.impl.tags.map toString)
    let total_s := o.total_ms.toFloat / 1000
    s!"{o.impl.label};{o.iterations};{total_s};{o.impl.num_threads};{tags}"

def benchmark_impl (impl : Impl) (size : Nat := 1000000) (dur_ms : Nat := 5000) (don't_optimize : ∀ {α}, α → IO Unit := λ _ => pure ())  : IO Output := do
  let start_ms := ← IO.monoMsNow
  let stop_ms := start_ms + dur_ms
  let mut iterations : Nat := 0
  let mut last_ms := start_ms
  for _ in Loop.mk do
    last_ms := (← IO.monoMsNow)
    if stop_ms ≤ last_ms then break
    don't_optimize $ impl.run size
    iterations := iterations + 1
  pure ⟨impl.toImplMetadata, iterations, last_ms - start_ms⟩
  

def sieve_impls : List Impl := [{
  label := "badly-drawn-wizards"
  num_threads := 1
  tags := [⟨"algorithm", "base"⟩, ⟨"faithful", "yes"⟩, ⟨"bits", "8"⟩]
  array := PrimeBitArray $ Array Bool
  run := run_sieve
}]

def main : IO Unit := do
  for impl in sieve_impls do
    let output := ← benchmark_impl impl
    IO.println output
