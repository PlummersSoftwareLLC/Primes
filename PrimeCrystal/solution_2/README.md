# Crystal solution_2 implementation by GordonBGood

![Algorithm](https://img.shields.io/badge/Algorithm-base-green)
![Faithfulness](https://img.shields.io/badge/Faithful-yes-green)
![Parallelism](https://img.shields.io/badge/Parallel-no-green)
![Bit count](https://img.shields.io/badge/Bits-1-green)

## Run instructions

### Running locally

Crystal is available for several operating systems.  Check out the Crystal install guide here: https://crystal-lang.org/install/. Note that full native Windows support is still not there so the recommended way of running it on Windows is under Windows Subsystem for Linux (WSL), although it may be passible to run it with MSYS2 (mingw64) or even more likely under Cygwin (as works for the Chapel language).  You can build and execute the application using the following commands:

```
crystal build primes.cr --release --no-debug
./primes
```

The `--release` flag will turn on all optimizations; this ensures we run as fast as possible.

### Docker

You can also choose the easy option of running the application inside a Docker container.

```
docker build -t primes .
docker run --rm primes
```

## Benchmarks

### Running locally

On Intel SkyLake i5-6500 (3.6 GHz when single threaded as here)

```
GordonBGood_bittwiddle;6112;5.000005;1;algorithm=base,faithful=yes,bits=1
Passes: 6112 Time: 5.000005 Avg: 0.000818 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
GordonBGood_stride8;11760;5.000305;1;algorithm=base,faithful=yes,bits=1
Passes: 11760 Time: 5.000305 Avg: 0.000425 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
GordonBGood_stride8-rblock16K;15007;5.000271;1;algorithm=base,faithful=yes,bits=1
Passes: 15007 Time: 5.000271 Avg: 0.000333 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
GordonBGood_extreme;17425;5.000041;1;algorithm=base,faithful=yes,bits=1
Passes: 17425 Time: 5.000041 Avg: 0.000287 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
GordonBGood_extreme-hybrid;32248;5.000081;1;algorithm=base,faithful=yes,bits=1
Passes: 32248 Time: 5.000081 Avg: 0.000155 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
```

## Docker run - same machine

```
                                                                 Single-threaded                                                                 
┌───────┬────────────────┬──────────┬───────────────────────────────┬────────┬──────────┬─────────┬───────────┬──────────┬──────┬───────────────┐
│ Index │ Implementation │ Solution │ Label                         │ Passes │ Duration │ Threads │ Algorithm │ Faithful │ Bits │ Passes/Second │
├───────┼────────────────┼──────────┼───────────────────────────────┼────────┼──────────┼─────────┼───────────┼──────────┼──────┼───────────────┤
│   1   │ crystal        │ 2        │ GordonBGood_extreme-hybrid    │ 32475  │ 5.00012  │    1    │   base    │   yes    │ 1    │  6494.84153   │
│   2   │ crystal        │ 2        │ GordonBGood_extreme           │ 17562  │ 5.00022  │    1    │   base    │   yes    │ 1    │  3512.24406   │
│   3   │ crystal        │ 2        │ GordonBGood_stride8-rblock16K │ 15159  │ 5.00028  │    1    │   base    │   yes    │ 1    │  3031.62902   │
│   4   │ crystal        │ 2        │ GordonBGood_stride8           │ 11848  │ 5.00010  │    1    │   base    │   yes    │ 1    │  2369.55261   │
│   5   │ crystal        │ 2        │ GordonBGood_bittwiddle        │  6073  │ 5.00051  │    1    │   base    │   yes    │ 1    │  1214.47637   │
└───────┴────────────────┴──────────┴───────────────────────────────┴────────┴──────────┴─────────┴───────────┴──────────┴──────┴───────────────┘
```

## Notes

Crystal is a very solid statically-typed imperative Object Oriented Programming (OOP) language that has just recently reached stable version 1.0 status.  It still has a couple of weakness on the roadmap, one being support for installation on native Windows as mentioned previously, and another that the multi-threading support is still in preview status.  Also, it depends on LLVM as a back end so when LLVM miss applies optimizations there is no easy means of passing alternate optimization parameters through to the LLVM compilation passes.  As well, the macro system has quite a few limitations as it doesn't offer full control of the Abstract Syntax Tree (AST), but rather operates more as a formulator for code quotations with some extensions for code generation using macro iterations and conditionals.  It does have a couple of "killer" features being the implmmentation of spawned co-routines (as go routings in Go language) and inferred Union Types that make the language feel like it is dynamically typed when it's not.  Neither of these features were required here, so I don't know how their performance would be.  The latter allows one to do the following:
```crystal
s = 42
s = "Hello"
```
just as one would do in dynamically-typed Ruby, but instead of `s` having the `Any` catch-all type, it would have the `Int32 | String` type, meaning that it can be either.  I would think that using this would impact performance since it would likely require runtime checking.  This is what is used to implement nullable types such as `ClassType | Nil` but `Reference` derived types may not be so slow as this is likely elided into just the usually dealing with the pointer possibly being `null/nil`.

This benchmark is written to be true to the "faithful to base algorithm" specification as follows:

1. It implements the odds-only bit-packed Sieve of Eratosthenes algorithm.
2. The output array contains at least one bit for each odd number in the sieved range as per the required algorithm.
3. The program marks as composite all odd composite representative bits that represent multiples of the odd base primes.
4. This implementation starts looking for base primes at three and scans only odd value indices up to the square root of the limit for prime base numbers as is allowed.
5. There is an outer loop that scans for instances of odd base prime values from three up to the index representing the square root of the limit, and with the body of this outer loop then marking all multiples of the found base prime value starting at the index representing the square of that base prime for all the multiples in the delivered sieving array as is also allowed.
6. The sieving buffer starts in zeroed state with composite number bit representatives marked as one when culled as is allowed.
7. There are approximately 810,000 culling/marking of composite representations just as for the original odds-only algorithm.

The previous solution_1 just used an bit array representing `bool`'s ..  The techniques use here are as follows, which all using pointer operations for maximum reduction of execution overheads:

1. The simplest is just the bit twidding one, with one small improvement in using a Look Up Table (LUT) for the bit masking patterns by bit index rather than using shifting as this is slightly faster.
2. The second technique uses striding in eight inner loops using a fixed mask for all bytes in each inner loop, for eight loops, each with a different mask value.  When there is "cache thrashing" as here, this often improves the performance due to the different cache line order, and also improves the performance because the inner loops are simpler.
3. Improving on the above technique, the next strided technique improves on the above by operating over a given block size at a time and thus slightly reducing the amount of "cache thrashing".
4. The fourth technique is that of "Extreme Loop Unrolling" where the eight "strided" loops are assembled into a single loop by recognizing the modulo marking patterns and using immediate values for the marking of each individual composite number bit; this can't use the block approach because it advances through the sieve buffer from start to finish.  This is implemented using the `unroll_setbits` macro.
5. The final technique uses the above for base prime values above 64 but for base primes below this threshold, it gives the dense marking treatment where it reads in a 64-bit word from the sieve buffer, marks each composite bit one by one by a macro generated modulo marking pattern, and when all bits in a given word have been marked, the word is commited back to the original location in the deliverable sieve buffer.  This dense marking macro is the `dense_setbits` macro.

The two macros used generate thousands of lines of code to cover all of the required case combinations.  The "extreme" techniqe is quite fast as it can reduce the average number of CPU clock cycles per marking operation down to just over one, excluding the time lost to "cache thrashing".  The "dense" technique can produce an even faster average speed down to as little as about a third of a CPU clock cycle per marking operation for the dense base prime values to which it applies due to the use of immediate-to-register `or`ing operations within each 64-bit word. This is so fast for two reasons:  first, it reduces the average operation time to as little a one third of a CPU clock cycle per operation (even less for even more modern CPU's), and second, it improves cache associativity by cache lines as it advances linearly across the entire sieving buffer, just as the above extreme loop unrolling does, but now the extreme loop unrolling also advances by at least eight bytes at a time.  This dense marking technique is likely even more important than the extreme loop unrolling technique as many more of the operations are dense than not.  The dense threshold has been set at base primes below 64, which seems to be about optimum.

In order to make it possible for reviewers to view the macro generated loop code, one can run the following command in the same directory as the source code is placed:
```
crystal tool expand -Dexpand_macro -c primes.cr:279:3 ./primes.cr > macro.txt
```
The "macro.txt" file will then contain the expansion of both the `unroll_setbit` macro and the `dense_setbit` macro.  For example, the following is the case for the dense culling for a base prime value of three, which is repeated for every group of three 64-bit words to the limit where there aren't a full three words left to cull:

```crystal
   when 3.to_u8
     while wordp <= wordlmtp
           # for all modulo pattern 64-bit words
       # for all modulo pattern 64-bit words
       # first bit of many in word
   v = wordp[0] | 4_u64
       v = v | 32_u64
       v = v | 256_u64
       v = v | 2048_u64
       v = v | 16384_u64
       v = v | 131072_u64
       v = v | 1048576_u64
       v = v | 8388608_u64
       v = v | 67108864_u64
       v = v | 536870912_u64
       v = v | 4294967296_u64
       v = v | 34359738368_u64
       v = v | 274877906944_u64
       v = v | 2199023255552_u64
       v = v | 17592186044416_u64
       v = v | 140737488355328_u64
       v = v | 1125899906842624_u64
       v = v | 9007199254740992_u64
       v = v | 72057594037927936_u64
       v = v | 576460752303423488_u64
       wordp[0] = v | 4611686018427387904_u64
           # for all modulo pattern 64-bit words
       # first bit of many in word
   v = wordp[1] | 2_u64
       v = v | 16_u64
       v = v | 128_u64
       v = v | 1024_u64
       v = v | 8192_u64
       v = v | 65536_u64
       v = v | 524288_u64
       v = v | 4194304_u64
       v = v | 33554432_u64
       v = v | 268435456_u64
       v = v | 2147483648_u64
       v = v | 17179869184_u64
       v = v | 137438953472_u64
       v = v | 1099511627776_u64
       v = v | 8796093022208_u64
       v = v | 70368744177664_u64
       v = v | 562949953421312_u64
       v = v | 4503599627370496_u64
       v = v | 36028797018963968_u64
       v = v | 288230376151711744_u64
       wordp[1] = v | 2305843009213693952_u64
           # for all modulo pattern 64-bit words
       # first bit of many in word
   v = wordp[2] | 1_u64
       v = v | 8_u64
       v = v | 64_u64
       v = v | 512_u64
       v = v | 4096_u64
       v = v | 32768_u64
       v = v | 262144_u64
       v = v | 2097152_u64
       v = v | 16777216_u64
       v = v | 134217728_u64
       v = v | 1073741824_u64
       v = v | 8589934592_u64
       v = v | 68719476736_u64
       v = v | 549755813888_u64
       v = v | 4398046511104_u64
       v = v | 35184372088832_u64
       v = v | 281474976710656_u64
       v = v | 2251799813685248_u64
       v = v | 18014398509481984_u64
       v = v | 144115188075855872_u64
       v = v | 1152921504606846976_u64
       wordp[2] = v | 9223372036854775808_u64
       wordp = wordp + 3
     end
```

and for the base prime value of 61 (the highest for which the dense technique is used) is as follows in that file, repeated for every 61 64-bit words in the sieve buffer to the limit where there isn't a full 61 words left:

```crystal
   when 61.to_u8
     while wordp <= wordlmtp
           # for all modulo pattern 64-bit words
       # for all modulo pattern 64-bit words
       # first bit of many in word
   v = wordp[0] | 1_u64
       wordp[0] = v | 2305843009213693952_u64
       wordp[1] = wordp[1] | 288230376151711744_u64
       wordp[2] = wordp[2] | 36028797018963968_u64
       wordp[3] = wordp[3] | 4503599627370496_u64
       wordp[4] = wordp[4] | 562949953421312_u64
       wordp[5] = wordp[5] | 70368744177664_u64
       wordp[6] = wordp[6] | 8796093022208_u64
       wordp[7] = wordp[7] | 1099511627776_u64
       wordp[8] = wordp[8] | 137438953472_u64
       wordp[9] = wordp[9] | 17179869184_u64
       wordp[10] = wordp[10] | 2147483648_u64
       wordp[11] = wordp[11] | 268435456_u64
       wordp[12] = wordp[12] | 33554432_u64
       wordp[13] = wordp[13] | 4194304_u64
       wordp[14] = wordp[14] | 524288_u64
       wordp[15] = wordp[15] | 65536_u64
       wordp[16] = wordp[16] | 8192_u64
       wordp[17] = wordp[17] | 1024_u64
       wordp[18] = wordp[18] | 128_u64
       wordp[19] = wordp[19] | 16_u64
           # for all modulo pattern 64-bit words
       # first bit of many in word
   v = wordp[20] | 2_u64
       wordp[20] = v | 4611686018427387904_u64
       wordp[21] = wordp[21] | 576460752303423488_u64
       wordp[22] = wordp[22] | 72057594037927936_u64
       wordp[23] = wordp[23] | 9007199254740992_u64
       wordp[24] = wordp[24] | 1125899906842624_u64
       wordp[25] = wordp[25] | 140737488355328_u64
       wordp[26] = wordp[26] | 17592186044416_u64
       wordp[27] = wordp[27] | 2199023255552_u64
       wordp[28] = wordp[28] | 274877906944_u64
       wordp[29] = wordp[29] | 34359738368_u64
       wordp[30] = wordp[30] | 4294967296_u64
       wordp[31] = wordp[31] | 536870912_u64
       wordp[32] = wordp[32] | 67108864_u64
       wordp[33] = wordp[33] | 8388608_u64
       wordp[34] = wordp[34] | 1048576_u64
       wordp[35] = wordp[35] | 131072_u64
       wordp[36] = wordp[36] | 16384_u64
       wordp[37] = wordp[37] | 2048_u64
       wordp[38] = wordp[38] | 256_u64
       wordp[39] = wordp[39] | 32_u64
           # for all modulo pattern 64-bit words
       # first bit of many in word
   v = wordp[40] | 4_u64
       wordp[40] = v | 9223372036854775808_u64
       wordp[41] = wordp[41] | 1152921504606846976_u64
       wordp[42] = wordp[42] | 144115188075855872_u64
       wordp[43] = wordp[43] | 18014398509481984_u64
       wordp[44] = wordp[44] | 2251799813685248_u64
       wordp[45] = wordp[45] | 281474976710656_u64
       wordp[46] = wordp[46] | 35184372088832_u64
       wordp[47] = wordp[47] | 4398046511104_u64
       wordp[48] = wordp[48] | 549755813888_u64
       wordp[49] = wordp[49] | 68719476736_u64
       wordp[50] = wordp[50] | 8589934592_u64
       wordp[51] = wordp[51] | 1073741824_u64
       wordp[52] = wordp[52] | 134217728_u64
       wordp[53] = wordp[53] | 16777216_u64
       wordp[54] = wordp[54] | 2097152_u64
       wordp[55] = wordp[55] | 262144_u64
       wordp[56] = wordp[56] | 32768_u64
       wordp[57] = wordp[57] | 4096_u64
       wordp[58] = wordp[58] | 512_u64
       wordp[59] = wordp[59] | 64_u64
       wordp[60] = wordp[60] | 8_u64
       wordp = wordp + 61
     end
```

In order to make it obvious that this implementation is "faithful to base", the bit array maniplulations have been implemented as if they are `setbit` functions that take as arguments a pointer to the bit array, a start bit index within that array, a stop bit index within the array, and a step value for the stride distance between each bit to be set, which is a built-in ability in many languages.

This technique allows culling/marking operations to take an average of about 0.75 CPU clock cycles per marking operation with a modern high-efficiency CPU; it is slower than in some other languaged due to the limitations of the LLVM optimization passes, for which some optimizations produce a loss in exection speed in some situations, while nailing the optimizations in other situations.  In general, the optimizations used by other languages that use gcc as a back end are more consistently good and the average gain over extreme techniques such as these is generally greater.

As common to all efficient SoE implementations, almost all of the expended time is spent in the composite number culling/marking.

## Author

W. Gordon Goodsman (GordonBGood)
