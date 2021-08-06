# Haskell "Loop Unpeeling" solution by GordonBGood

This Haskell solution is implemented in an imperative style using `forM_` so that the core algorithm remains recognizable.  Unlike the earlier solution, this solution does not use imported libraries to accomplish the task, so thus is `faithful to base`.  The number representation is one bit per odd number.

The implementation uses the same algorithm as the Rust "striped" algorithm, so thus is `faithful base`, although the actual loops are very simple and thus no separate storage implementation is used.  The outer loop starting at code line 50 searches for the base prime values as required; The next inner loop level has a limit set at code line 55 so that it never runs more than eight times, then loops starting at code line 56 by just setting up the constant mask value and starting byte index to be used in the innermost actual marking loops as per code lines 59 to 61.  The boolean deliverable array is returned at code line 62.

Note that there are two views into the same array as set up in code lines 45 to 47, one as a boolean one-bit-per-value array and one as a Word8 byte array, with search for base prime values done in the boolean representation and culling done in the Word8 byte representation for the "striping"/"loop unpeeling" algorithm.  The returned view is the boolean one because then we can use the Haskell built-in `assocs` function to produce the returned lazy list of prime values as a list comprehension in code lines 41 and 42.

## Run instructions

If you have [GHC Haskell installed on your machine](https://www.haskell.org/ghc/download_ghc_8_10_5.html); on Windows, LLVM does not work on Windows for versions newer than 8.6.5 up to 9.0.1 so [version 8.6.5 must be used](https://www.haskell.org/ghc/download_ghc_8_6_5.html).

Once LLVM and GHC Haskell are installed (as well as GIMP if using that version), just run "ghc primes" to compile and "./primes" to run the application.

Otherwise, use the provided `Dockerfile`.

## Output

- Intel SkyLake i5-6500, no LLVM

  ```
  GordonBGood_unpeeled;3831;5.000076052;1;algorithm=base;faithful=yes;bits=1
  ```

- Intel SkyLake i5-6500, with LLVM

  ```
  GordonBGood_unpeeled;11042;5.000068131s;1;algorithm=base;faithful=yes;bits=1

  ```

- Intel SkyLake i5-6500, docker, with LLVM

  ```
  GordonBGood_unpeeled;11075;5.00014;1;algorithm=base,bits=1,faithful=no
  ```
