# C++ constexpr solution by flo80

![Algorithm](https://img.shields.io/badge/Algorithm-base-green)
![Faithfulness](https://img.shields.io/badge/Faithful-no-yellowgreen)
![Parallelism](https://img.shields.io/badge/Parallel-yes-green)
![Bit count](https://img.shields.io/badge/Bits-1-green)
![Deviation](https://img.shields.io/badge/Deviation-compile%20time-blue)

This solution declares as much as possible as `constexpr`, allowing the compiler to calculate it during compile time.
Since the standard library does not provide required functions, sqrt and bitfield are custom versions.

*Deviation*: It does use the base algorithm but due to compile time optimization, a lot of the actual calculation does not happen at runtime

*Faithfulness*: Since the buffer size is fixed, it is [not considered faithful](https://github.com/PlummersSoftwareLLC/Primes/pull/274).

*Note*: this solution is limited to numbers up to around 50,000,000 (stack size limit on Mac OS it seems).

## Run instructions

`./run.sh`, requires CLANG in a fairly recent version (supporting C++ 20)

## Output

All on Apple M1 (Macbook Air)

### Native performance

     Computing primes to 1000000 on 24 threads for 5 seconds.
     Passes: 591836324, Time: 5.000693, Avg: 0.000000, Limit: 1000000, Count1: 78498, Count2: 78498, Valid: 1

     flo80_pol_constexpr;591836324;5.000693;24;algorithm=base,faithful=no,bits=1

     Computing primes to 1000000 on 1 thread for 5 seconds.
     Passes: 228773990, Time: 5.000000, Avg: 0.000000, Limit: 1000000, Count1: 78498, Count2: 78498, Valid: 1

     flo80_pol_constexpr;228773990;5.000000;1;algorithm=base,faithful=no,bits=1

Compared to other C++ implementations:

*Solution 1*

     Passes: 4471, Time: 5.000244, Avg: 0.001118, Limit: 1000000, Count1: 78498, Count2: 78498, Valid: 1
     davepl;4471;5.000244;1;algorithm=base,faithful=yes,bits=1

*Solution 2*

     Computing primes to 10000000 on 8 threads for 5 seconds.
     Passes: 2264, Threads: 8, Time: 5.00982, Average: 0.00221282, Limit: 10000000, Counts: 664579/664579, Valid : Pass
     
     davepl_par;2264;5.00982;8;algorithm=base,faithful=yes,bits=1

### Docker performance

                                                          Single-threaded                                                      
     ┌───────┬────────────────┬──────────┬────────┬────────┬──────────┬─────────┬───────────┬──────────┬──────┬───────────────┐
     │ Index │ Implementation │ Solution │ Label  │ Passes │ Duration │ Threads │ Algorithm │ Faithful │ Bits │ Passes/Second │
     ├───────┼────────────────┼──────────┼────────┼────────┼──────────┼─────────┼───────────┼──────────┼──────┼───────────────┤
     │   1   │ cpp            │ 1        │ davepl │  3982  │ 5.00001  │    1    │   base    │   yes    │ 1    │   796.39857   │
     └───────┴────────────────┴──────────┴────────┴────────┴──────────┴─────────┴───────────┴──────────┴──────┴───────────────┘
     
                                                           Multi-threaded                                                        
     ┌───────┬────────────────┬──────────┬────────────┬────────┬──────────┬─────────┬───────────┬──────────┬──────┬───────────────┐
     │ Index │ Implementation │ Solution │ Label      │ Passes │ Duration │ Threads │ Algorithm │ Faithful │ Bits │ Passes/Second │
     ├───────┼────────────────┼──────────┼────────────┼────────┼──────────┼─────────┼───────────┼──────────┼──────┼───────────────┤
     │   1   │ cpp            │ 2        │ davepl_par │ 13192  │ 5.00080  │    4    │   base    │   yes    │ 1    │   659.49448   │
     └───────┴────────────────┴──────────┴────────────┴────────┴──────────┴─────────┴───────────┴──────────┴──────┴───────────────┘
     
     
                                                               Single-threaded                                                          
     ┌───────┬────────────────┬──────────┬─────────────────────┬───────────┬──────────┬─────────┬───────────┬──────────┬──────┬────────────────┐
     │ Index │ Implementation │ Solution │ Label               │  Passes   │ Duration │ Threads │ Algorithm │ Faithful │ Bits │ Passes/Second  │
     ├───────┼────────────────┼──────────┼─────────────────────┼───────────┼──────────┼─────────┼───────────┼──────────┼──────┼────────────────┤
     │   1   │ PrimeCPP       │ 3        │ flo80_pol_constexpr │ 234051587 │ 5.00000  │    1    │   base    │    no    │ 1    │ 46810317.40000 │
     └───────┴────────────────┴──────────┴─────────────────────┴───────────┴──────────┴─────────┴───────────┴──────────┴──────┴────────────────┘
     
                                                               Multi-threaded                                                           
     ┌───────┬────────────────┬──────────┬─────────────────────┬───────────┬──────────┬─────────┬───────────┬──────────┬──────┬───────────────┐
     │ Index │ Implementation │ Solution │ Label               │  Passes   │ Duration │ Threads │ Algorithm │ Faithful │ Bits │ Passes/Second │
     ├───────┼────────────────┼──────────┼─────────────────────┼───────────┼──────────┼─────────┼───────────┼──────────┼──────┼───────────────┤
     │   1   │ PrimeCPP       │ 3        │ flo80_pol_constexpr │ 587300645 │ 5.00052  │   24    │   base    │    no    │ 1    │ 4893659.18618 │
     └───────┴────────────────┴──────────┴─────────────────────┴───────────┴──────────┴─────────┴───────────┴──────────┴──────┴───────────────┘