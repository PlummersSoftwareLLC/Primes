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
GordonBGood_bittwiddle;6078;5.000029;1;algorithm=base,faithful=yes,bits=1
Passes: 6078 Time: 5.000029 Avg: 0.000823 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
GordonBGood_stride8;11765;5.000244;1;algorithm=base,faithful=yes,bits=1
Passes: 11765 Time: 5.000244 Avg: 0.000425 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
GordonBGood_stride8-block16K;13465;5.000127;1;algorithm=base,faithful=yes,bits=1
Passes: 13465 Time: 5.000127 Avg: 0.000371 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
GordonBGood_extreme;17234;5.000401;1;algorithm=base,faithful=yes,bits=1
Passes: 17234 Time: 5.000401 Avg: 0.000290 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
GordonBGood_extreme-hybrid;31443;5.000053;1;algorithm=base,faithful=yes,bits=1
Passes: 31443 Time: 5.000053 Avg: 0.000159 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
```

## Docker run - same machine

```
                                                               Single-threaded                                                                 
┌───────┬────────────────┬──────────┬──────────────────────────────┬────────┬──────────┬─────────┬───────────┬──────────┬──────┬───────────────┐
│ Index │ Implementation │ Solution │ Label                        │ Passes │ Duration │ Threads │ Algorithm │ Faithful │ Bits │ Passes/Second │
├───────┼────────────────┼──────────┼──────────────────────────────┼────────┼──────────┼─────────┼───────────┼──────────┼──────┼───────────────┤
│   1   │ crystal        │ 2        │ GordonBGood_extreme-hybrid   │ 31558  │ 5.00011  │    1    │   base    │   yes    │ 1    │  6311.46241   │
│   2   │ crystal        │ 2        │ GordonBGood_extreme          │ 17228  │ 5.00025  │    1    │   base    │   yes    │ 1    │  3445.42497   │
│   3   │ crystal        │ 2        │ GordonBGood_stride8-block16K │ 13513  │ 5.00009  │    1    │   base    │   yes    │ 1    │  2702.55298   │
│   4   │ crystal        │ 2        │ GordonBGood_stride8          │ 11785  │ 5.00035  │    1    │   base    │   yes    │ 1    │  2356.83408   │
│   5   │ crystal        │ 2        │ GordonBGood_bittwiddle       │  6033  │ 5.00014  │    1    │   base    │   yes    │ 1    │  1206.56549   │
└───────┴────────────────┴──────────┴──────────────────────────────┴────────┴──────────┴─────────┴───────────┴──────────┴──────┴───────────────┘
```

## Notes

Crystal is a very solid imperative Object Oriented Programming (OOP) language that has just recently reached stable version 1.0 status.  It still has a couple of weakness on the roadmap, one bing support for installation on native Windows as mentioned previously, and another that the multi-threading support is still in preview status.  Also, it depends on LLVM as a back end so when LLVM miss applies optimizations there is no easy means of passing alternate optimization parameters through to the LLVM compilation passes.  As well, the macro system has quite a few limitations as it doesn't offer full control of the Abstract Syntax Tree (AST), but rather operates more as a formulator for code quotations with some extensions for code generation using macro iterations and conditionals.

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

In order to make it possible for reviewers to view the macro generated loop code, the main test running code at line 275 can be commented out and the seven coummented out lines immediately above that can be un-commented, with the code at lines 103, 105, 108, 111, and 114 also un-commented.  When compiled and run, the code for the particual dense base prime value given as an argument in line 273 will be shown.  For instance, for the base prime value of three the following code will be used, repeated for every three 64-bit words in the sieve buffer to the limit where there isn't a full three words:

```crystal
3, 0, 0, 0:  v = wordp[0] | 4_u64
3, 0, 0, 1:  v |= 32_u64
3, 0, 0, 2:  v |= 256_u64
3, 0, 0, 3:  v |= 2048_u64
3, 0, 0, 4:  v |= 16384_u64
3, 0, 0, 5:  v |= 131072_u64
3, 0, 0, 6:  v |= 1048576_u64
3, 0, 0, 7:  v |= 8388608_u64
3, 0, 0, 8:  v |= 67108864_u64
3, 0, 0, 9:  v |= 536870912_u64
3, 0, 0, 10:  v |= 4294967296_u64
3, 0, 0, 11:  v |= 34359738368_u64
3, 0, 0, 12:  v |= 274877906944_u64
3, 0, 0, 13:  v |= 2199023255552_u64
3, 0, 0, 14:  v |= 17592186044416_u64
3, 0, 0, 15:  v |= 140737488355328_u64
3, 0, 0, 16:  v |= 1125899906842624_u64
3, 0, 0, 17:  v |= 9007199254740992_u64
3, 0, 0, 18:  v |= 72057594037927936_u64
3, 0, 0, 19:  v |= 576460752303423488_u64
3, 0, 0, 20:  wordp[0] = v | 4611686018427387904_u64
3, 0, 1, 21:  v = wordp[1] | 2_u64
3, 0, 1, 22:  v |= 16_u64
3, 0, 1, 23:  v |= 128_u64
3, 0, 1, 24:  v |= 1024_u64
3, 0, 1, 25:  v |= 8192_u64
3, 0, 1, 26:  v |= 65536_u64
3, 0, 1, 27:  v |= 524288_u64
3, 0, 1, 28:  v |= 4194304_u64
3, 0, 1, 29:  v |= 33554432_u64
3, 0, 1, 30:  v |= 268435456_u64
3, 0, 1, 31:  v |= 2147483648_u64
3, 0, 1, 32:  v |= 17179869184_u64
3, 0, 1, 33:  v |= 137438953472_u64
3, 0, 1, 34:  v |= 1099511627776_u64
3, 0, 1, 35:  v |= 8796093022208_u64
3, 0, 1, 36:  v |= 70368744177664_u64
3, 0, 1, 37:  v |= 562949953421312_u64
3, 0, 1, 38:  v |= 4503599627370496_u64
3, 0, 1, 39:  v |= 36028797018963968_u64
3, 0, 1, 40:  v |= 288230376151711744_u64
3, 0, 1, 41:  wordp[1] = v | 2305843009213693952_u64
3, 0, 2, 42:  v = wordp[2] | 1_u64
3, 0, 2, 43:  v |= 8_u64
3, 0, 2, 44:  v |= 64_u64
3, 0, 2, 45:  v |= 512_u64
3, 0, 2, 46:  v |= 4096_u64
3, 0, 2, 47:  v |= 32768_u64
3, 0, 2, 48:  v |= 262144_u64
3, 0, 2, 49:  v |= 2097152_u64
3, 0, 2, 50:  v |= 16777216_u64
3, 0, 2, 51:  v |= 134217728_u64
3, 0, 2, 52:  v |= 1073741824_u64
3, 0, 2, 53:  v |= 8589934592_u64
3, 0, 2, 54:  v |= 68719476736_u64
3, 0, 2, 55:  v |= 549755813888_u64
3, 0, 2, 56:  v |= 4398046511104_u64
3, 0, 2, 57:  v |= 35184372088832_u64
3, 0, 2, 58:  v |= 281474976710656_u64
3, 0, 2, 59:  v |= 2251799813685248_u64
3, 0, 2, 60:  v |= 18014398509481984_u64
3, 0, 2, 61:  v |= 144115188075855872_u64
3, 0, 2, 62:  v |= 1152921504606846976_u64
3, 0, 2, 63:  wordp[2] = v | 9223372036854775808_u64
```

and for the base prime value of 61 (the highest for which the dense technique is used) will be shown as follows, repeated for every 61 64-bit words in the sieve buffer to the limit where there isn't a full 61 words left:

```crystal
61, 29, 0, 0:  v = wordp[0] | 1_u64
61, 29, 0, 1:  wordp[0] = v | 2305843009213693952_u64
61, 29, 1, 1:  v = wordp[1] | 2305843009213693952_u64
61, 29, 1, 2:  wordp[1] |= 288230376151711744_u64
61, 29, 2, 2:  v = wordp[2] | 288230376151711744_u64
61, 29, 2, 3:  wordp[2] |= 36028797018963968_u64
61, 29, 3, 3:  v = wordp[3] | 36028797018963968_u64
61, 29, 3, 4:  wordp[3] |= 4503599627370496_u64
61, 29, 4, 4:  v = wordp[4] | 4503599627370496_u64
61, 29, 4, 5:  wordp[4] |= 562949953421312_u64
61, 29, 5, 5:  v = wordp[5] | 562949953421312_u64
61, 29, 5, 6:  wordp[5] |= 70368744177664_u64
61, 29, 6, 6:  v = wordp[6] | 70368744177664_u64
61, 29, 6, 7:  wordp[6] |= 8796093022208_u64
61, 29, 7, 7:  v = wordp[7] | 8796093022208_u64
61, 29, 7, 8:  wordp[7] |= 1099511627776_u64
61, 29, 8, 8:  v = wordp[8] | 1099511627776_u64
61, 29, 8, 9:  wordp[8] |= 137438953472_u64
61, 29, 9, 9:  v = wordp[9] | 137438953472_u64
61, 29, 9, 10:  wordp[9] |= 17179869184_u64
61, 29, 10, 10:  v = wordp[10] | 17179869184_u64
61, 29, 10, 11:  wordp[10] |= 2147483648_u64
61, 29, 11, 11:  v = wordp[11] | 2147483648_u64
61, 29, 11, 12:  wordp[11] |= 268435456_u64
61, 29, 12, 12:  v = wordp[12] | 268435456_u64
61, 29, 12, 13:  wordp[12] |= 33554432_u64
61, 29, 13, 13:  v = wordp[13] | 33554432_u64
61, 29, 13, 14:  wordp[13] |= 4194304_u64
61, 29, 14, 14:  v = wordp[14] | 4194304_u64
61, 29, 14, 15:  wordp[14] |= 524288_u64
61, 29, 15, 15:  v = wordp[15] | 524288_u64
61, 29, 15, 16:  wordp[15] |= 65536_u64
61, 29, 16, 16:  v = wordp[16] | 65536_u64
61, 29, 16, 17:  wordp[16] |= 8192_u64
61, 29, 17, 17:  v = wordp[17] | 8192_u64
61, 29, 17, 18:  wordp[17] |= 1024_u64
61, 29, 18, 18:  v = wordp[18] | 1024_u64
61, 29, 18, 19:  wordp[18] |= 128_u64
61, 29, 19, 19:  v = wordp[19] | 128_u64
61, 29, 19, 20:  wordp[19] |= 16_u64
61, 29, 20, 20:  v = wordp[20] | 16_u64
61, 29, 20, 21:  v = wordp[20] | 2_u64
61, 29, 20, 22:  wordp[20] = v | 4611686018427387904_u64
61, 29, 21, 22:  v = wordp[21] | 4611686018427387904_u64
61, 29, 21, 23:  wordp[21] |= 576460752303423488_u64
61, 29, 22, 23:  v = wordp[22] | 576460752303423488_u64
61, 29, 22, 24:  wordp[22] |= 72057594037927936_u64
61, 29, 23, 24:  v = wordp[23] | 72057594037927936_u64
61, 29, 23, 25:  wordp[23] |= 9007199254740992_u64
61, 29, 24, 25:  v = wordp[24] | 9007199254740992_u64
61, 29, 24, 26:  wordp[24] |= 1125899906842624_u64
61, 29, 25, 26:  v = wordp[25] | 1125899906842624_u64
61, 29, 25, 27:  wordp[25] |= 140737488355328_u64
61, 29, 26, 27:  v = wordp[26] | 140737488355328_u64
61, 29, 26, 28:  wordp[26] |= 17592186044416_u64
61, 29, 27, 28:  v = wordp[27] | 17592186044416_u64
61, 29, 27, 29:  wordp[27] |= 2199023255552_u64
61, 29, 28, 29:  v = wordp[28] | 2199023255552_u64
61, 29, 28, 30:  wordp[28] |= 274877906944_u64
61, 29, 29, 30:  v = wordp[29] | 274877906944_u64
61, 29, 29, 31:  wordp[29] |= 34359738368_u64
61, 29, 30, 31:  v = wordp[30] | 34359738368_u64
61, 29, 30, 32:  wordp[30] |= 4294967296_u64
61, 29, 31, 32:  v = wordp[31] | 4294967296_u64
61, 29, 31, 33:  wordp[31] |= 536870912_u64
61, 29, 32, 33:  v = wordp[32] | 536870912_u64
61, 29, 32, 34:  wordp[32] |= 67108864_u64
61, 29, 33, 34:  v = wordp[33] | 67108864_u64
61, 29, 33, 35:  wordp[33] |= 8388608_u64
61, 29, 34, 35:  v = wordp[34] | 8388608_u64
61, 29, 34, 36:  wordp[34] |= 1048576_u64
61, 29, 35, 36:  v = wordp[35] | 1048576_u64
61, 29, 35, 37:  wordp[35] |= 131072_u64
61, 29, 36, 37:  v = wordp[36] | 131072_u64
61, 29, 36, 38:  wordp[36] |= 16384_u64
61, 29, 37, 38:  v = wordp[37] | 16384_u64
61, 29, 37, 39:  wordp[37] |= 2048_u64
61, 29, 38, 39:  v = wordp[38] | 2048_u64
61, 29, 38, 40:  wordp[38] |= 256_u64
61, 29, 39, 40:  v = wordp[39] | 256_u64
61, 29, 39, 41:  wordp[39] |= 32_u64
61, 29, 40, 41:  v = wordp[40] | 32_u64
61, 29, 40, 42:  v = wordp[40] | 4_u64
61, 29, 40, 43:  wordp[40] = v | 9223372036854775808_u64
61, 29, 41, 43:  v = wordp[41] | 9223372036854775808_u64
61, 29, 41, 44:  wordp[41] |= 1152921504606846976_u64
61, 29, 42, 44:  v = wordp[42] | 1152921504606846976_u64
61, 29, 42, 45:  wordp[42] |= 144115188075855872_u64
61, 29, 43, 45:  v = wordp[43] | 144115188075855872_u64
61, 29, 43, 46:  wordp[43] |= 18014398509481984_u64
61, 29, 44, 46:  v = wordp[44] | 18014398509481984_u64
61, 29, 44, 47:  wordp[44] |= 2251799813685248_u64
61, 29, 45, 47:  v = wordp[45] | 2251799813685248_u64
61, 29, 45, 48:  wordp[45] |= 281474976710656_u64
61, 29, 46, 48:  v = wordp[46] | 281474976710656_u64
61, 29, 46, 49:  wordp[46] |= 35184372088832_u64
61, 29, 47, 49:  v = wordp[47] | 35184372088832_u64
61, 29, 47, 50:  wordp[47] |= 4398046511104_u64
61, 29, 48, 50:  v = wordp[48] | 4398046511104_u64
61, 29, 48, 51:  wordp[48] |= 549755813888_u64
61, 29, 49, 51:  v = wordp[49] | 549755813888_u64
61, 29, 49, 52:  wordp[49] |= 68719476736_u64
61, 29, 50, 52:  v = wordp[50] | 68719476736_u64
61, 29, 50, 53:  wordp[50] |= 8589934592_u64
61, 29, 51, 53:  v = wordp[51] | 8589934592_u64
61, 29, 51, 54:  wordp[51] |= 1073741824_u64
61, 29, 52, 54:  v = wordp[52] | 1073741824_u64
61, 29, 52, 55:  wordp[52] |= 134217728_u64
61, 29, 53, 55:  v = wordp[53] | 134217728_u64
61, 29, 53, 56:  wordp[53] |= 16777216_u64
61, 29, 54, 56:  v = wordp[54] | 16777216_u64
61, 29, 54, 57:  wordp[54] |= 2097152_u64
61, 29, 55, 57:  v = wordp[55] | 2097152_u64
61, 29, 55, 58:  wordp[55] |= 262144_u64
61, 29, 56, 58:  v = wordp[56] | 262144_u64
61, 29, 56, 59:  wordp[56] |= 32768_u64
61, 29, 57, 59:  v = wordp[57] | 32768_u64
61, 29, 57, 60:  wordp[57] |= 4096_u64
61, 29, 58, 60:  v = wordp[58] | 4096_u64
61, 29, 58, 61:  wordp[58] |= 512_u64
61, 29, 59, 61:  v = wordp[59] | 512_u64
61, 29, 59, 62:  wordp[59] |= 64_u64
61, 29, 60, 62:  v = wordp[60] | 64_u64
61, 29, 60, 63:  wordp[60] |= 8_u64
```

Note that these are just the generated code line strings as printed, preceeded by the base prime value used, the index of the pattern ((step - 3) / 2), the word number, and the bit number within the multi-word modulo pattern, which was used for debugging purposes.

In order to make it obvious that this implementation is "faithful to base", the bit array maniplulations have been implemented as if they are `setbit` functions that take as arguments a pointer to the bit array, a start bit index within that array, a stop bit index within the array, and a step value for the stride distance between each bit to be set, which is a built-in ability in many languages.

This technique allows culling/marking operations to take an average of about 0.75 CPU clock cycles per marking operation with a modern high-efficiency CPU; it is slower than in some other languaged due to the limitations of the LLVM optimization passes, for which some optimizations produce a loss in exection speed in some situations, while nailing the optimizations in other situations.  In general, the optimizations used by other languages that use gcc as a back end are more consistently good and the average gain over extreme techniques such as these is generally greater.

As common to all efficient SoE implementations, almost all of the expended time is spent in the composite number culling/marking.

## Author

W. Gordon Goodsman (GordonBGood)
