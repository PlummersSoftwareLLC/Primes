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

     Computing primes to 1000000 on 8 threads for 5 seconds.
     Passes: 662952, Time: 5.000016, Avg: 0.000008, Limit: 1000000, Count1: 78498, Count2: 78498, Valid: 1
     
     flo80_constexpr;662952;5.000016;8;algorithm=base,faithful=no,bits=1
     
     
     Computing primes to 1000000 on 1 thread for 5 seconds.
     Passes: 484448, Time: 5.000007, Avg: 0.000010, Limit: 1000000, Count1: 78498, Count2: 78498, Valid: 1
     
     flo80_constexpr;484448;5.000007;1;algorithm=base,faithful=no,bits=1

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
     ┌───────┬────────────────┬──────────┬─────────────────┬────────┬──────────┬─────────┬───────────┬──────────┬──────┬───────────────┐
     │ Index │ Implementation │ Solution │ Label           │ Passes │ Duration │ Threads │ Algorithm │ Faithful │ Bits │ Passes/Second │
     ├───────┼────────────────┼──────────┼─────────────────┼────────┼──────────┼─────────┼───────────┼──────────┼──────┼───────────────┤
     │   1   │ cpp            │ 3        │ flo80_constexpr │ 69508  │ 5.00003  │    1    │   base    │    no    │ 1    │  13901.52215  │
     └───────┴────────────────┴──────────┴─────────────────┴────────┴──────────┴─────────┴───────────┴──────────┴──────┴───────────────┘
     
                                                               Multi-threaded                                                           
     ┌───────┬────────────────┬──────────┬─────────────────┬────────┬──────────┬─────────┬───────────┬──────────┬──────┬───────────────┐
     │ Index │ Implementation │ Solution │ Label           │ Passes │ Duration │ Threads │ Algorithm │ Faithful │ Bits │ Passes/Second │
     ├───────┼────────────────┼──────────┼─────────────────┼────────┼──────────┼─────────┼───────────┼──────────┼──────┼───────────────┤
     │   1   │ cpp            │ 3        │ flo80_constexpr │ 139692 │ 5.00012  │    4    │   base    │    no    │ 1    │  6984.42539   │
     └───────┴────────────────┴──────────┴─────────────────┴────────┴──────────┴─────────┴───────────┴──────────┴──────┴───────────────┘