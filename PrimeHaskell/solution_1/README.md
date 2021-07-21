# Haskell solution by fatho

The Haskell implementation implements the solution in an imperative style so that the core
algorithm remains recognizable.

While the benchmarked implementation uses the `IO` monad (and thus doesn't isolate potential side
effects very well), all the data structures used are compatible with `ST` and can be used in a pure
setting as well.

## Run instructions

If you have [`stack`](https://docs.haskellstack.org/en/stable/README/), it's simply `stack run`.

Otherwise, use the provided `Dockerfile`.

## Output

- AMD Ryzen 7 3800X, docker, no LLVM

  ```
  fatho/bitset;3812;5.000701;1;algorithm=base,bits=1,faithful=no
  fatho/bitset_unchecked;3827;5.000592;1;algorithm=base,bits=1,faithful=no
  fatho/vector;9431;5.000294;1;algorithm=base,bits=8,faithful=no
  fatho/vector_unchecked;11560;5.000402;1;algorithm=base,bits=8,faithful=no
  ```

- AMD Ryzen 7 3800X, docker, with LLVM

  ```
  fatho/bitset;10240;5.000118;1;algorithm=base,bits=1,faithful=no
  fatho/bitset_unchecked;11378;5.000351;1;algorithm=base,bits=1,faithful=no
  fatho/vector;12688;5.000288;1;algorithm=base,bits=8,faithful=no
  fatho/vector_unchecked;14413;5.000267;1;algorithm=base,bits=8,faithful=no
  ```