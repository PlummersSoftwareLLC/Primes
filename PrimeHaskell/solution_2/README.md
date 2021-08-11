# Haskell "Loop Unpeeling" solution by GordonBGood

This Haskell solution is implemented in an imperative style using `forM_` so that the core algorithm remains recognizable.  Unlike the earlier solution, this solution does not use imported libraries to accomplish the task, so thus is `faithful to base`.  The number representation is one bit per odd number.

The implementation uses a similar algorithm as the Rust "striped" algorithm but instead of changing the order of bits within the sieve buffer, leaves the order as normal and culls/marks them by "stripes" in place, so thus is `faithful base`.  The actual loops are very simple and thus no separate storage implementation is used.  The outer loop starting at code line 53 searches for the base prime values as required; The next inner loop level has a limit set at code line 58 so that it never runs more than eight times, then loops starting at code line 59 by just setting up the constant mask value and starting byte index to be used in the innermost actual marking loops as per code lines starting at 62.  The boolean deliverable array is returned at code line 67 after masking off all values above the given range in the above two lines as those values may not have been processed and aren't desired in the output listing.

Note that there are two views into the same array as set up in code lines 48 to 50, one as a boolean one-bit-per-value array and one as a Word8 byte array, with search for base prime values done in the boolean representation and culling done in the Word8 byte representation for the "loop unpeeling" algorithm.  The returned view is the boolean one because then we can use the Haskell built-in `assocs` function to produce the returned lazy list of prime values as a list comprehension in the `listPrimes` function code in lines 123 to 125.

The second implementation differs from the above in two respects:  It implements some basic loop unrolling (by four loops), and it runs all up to eight bit "unpeeling" loops over one CPU L1 cache-sized "page" at a time and repeats for all the required "pages" in order to get better cache associativity (less cache "thrashing").  To do this easily, it uses a small eight-integer array created at line 75 just below where the two views into the sieving buffer array were created.  Then, for each base prime value to be used for marking of composite representation bits, it fills this small array with the initial start byte indices at lines 86 and 87, followed by the rest of the loops, first by buffer "page" starting at line 88, then by the up to eight "bit-loops" starting at line 96, and then the marking loops.  For this implementation, marking is broken up into two phases, with the first unrolled loop starting at line 99 and the second single mark per loop starting at line 113 doing the marking up to the end of the page when there are less than four markings to be done.  This implementation uses `foldM` as necessary to thread the current marking value through the loops so that it can be saved back to the current bit position location in the small eight value array for use in the next page pass, which saving is done at line 118.  As before, the boolean view is returned to be easy to scan for unmarked values and convert to prime values outside the timing loop using the `listPrimes` function in line 123.

## Run instructions

If you have [GHC Haskell installed on your machine](https://www.haskell.org/ghc/download_ghc_8_10_5.html); on Windows, LLVM does not work on Windows for versions newer than 8.6.5 up to 9.0.1 so [version 8.6.5 must be used](https://www.haskell.org/ghc/download_ghc_8_6_5.html).

Once LLVM and GHC Haskell are installed (as well as GIMP if using that version), just run "ghc primes" to compile and "./primes" to run the application.

Otherwise, use the provided `Dockerfile`.

## Output

- Intel SkyLake i5-6500, no LLVM

  ```
  GordonBGood_unpeeled;9518;5.000187273;1;algorithm=base,faithful=yes,bits=1
  GordonBGood_unpeeled_block;9265;5.000361937;1;algorithm=base,faithful=yes,bits=1
  ```

- Intel SkyLake i5-6500, with LLVM (version 12)

  ```
  GordonBGood_unpeeled;10716;5.000059751;1;algorithm=base,faithful=yes,bits=1
  GordonBGood_unpeeled_block;13147;5.000007136;1;algorithm=base,faithful=yes,bits=1bits=1
  ```

- Intel SkyLake i5-6500, docker, with LLVM

  ```
  GordonBGood_unpeeled;11109;5.00011;1;algorithm=base,bits=1,faithful=no
  GordonBGood_unpeeled_block;12613;5.00031;1;algorithm=base,bits=1,faithful=no
  ```
