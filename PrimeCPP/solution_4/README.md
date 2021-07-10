# C++ solution by fastware (https://www.youtube.com/channel/UCxgBT1XAXUP9sy5keLAmPPA)

![Algorithm](https://img.shields.io/badge/10M-table-yellowgreen)
![Faithfulness](https://img.shields.io/badge/Faithful-no-yellowgreen)
![Parallelism](https://img.shields.io/badge/Parallel-yes-green)
![Bit count](https://img.shields.io/badge/Bits-1-green)
![Deviation](https://img.shields.io/badge/Deviation-compile%20time-blue)
![Generated](https://img.shields.io/badge/Steps-2-blue)

C++ generated table solution (compute once - use multiple times)

Single-threaded - 82X better performance than davepl and 2.7X performance than flo80_constexpr
Multi-threaded - 104X better performance than davepl and 32X performance than flo80_constexpr

### Docker performance

                                                          Single-threaded                                                          
┌───────┬────────────────┬──────────┬─────────────────┬────────┬──────────┬─────────┬───────────┬──────────┬──────┬───────────────┐
│ Index │ Implementation │ Solution │ Label           │ Passes │ Duration │ Threads │ Algorithm │ Faithful │ Bits │ Passes/Second │
├───────┼────────────────┼──────────┼─────────────────┼────────┼──────────┼─────────┼───────────┼──────────┼──────┼───────────────┤
│   1   │ cpp            │ 4        │ fastware        │ 803327 │ 5.00000  │    1    │   table   │    no    │ 1    │ 160665.40000  │
│   2   │ cpp            │ 3        │ flo80_constexpr │ 297975 │ 5.00001  │    1    │   base    │    no    │ 1    │  59594.89273  │
│   3   │ cpp            │ 1        │ davepl          │  9769  │ 5.00044  │    1    │   base    │   yes    │ 1    │  1953.62691   │
└───────┴────────────────┴──────────┴─────────────────┴────────┴──────────┴─────────┴───────────┴──────────┴──────┴───────────────┘
                                                           Multi-threaded                                                            
┌───────┬────────────────┬──────────┬─────────────────┬──────────┬──────────┬─────────┬───────────┬──────────┬──────┬───────────────┐
│ Index │ Implementation │ Solution │ Label           │  Passes  │ Duration │ Threads │ Algorithm │ Faithful │ Bits │ Passes/Second │
├───────┼────────────────┼──────────┼─────────────────┼──────────┼──────────┼─────────┼───────────┼──────────┼──────┼───────────────┤
│   1   │ cpp            │ 4        │ fastware        │ 11349981 │ 5.00001  │   32    │   table   │    no    │ 1    │  70937.19681  │
│   2   │ cpp            │ 3        │ flo80_constexpr │  357504  │ 5.00016  │   32    │   base    │    no    │ 1    │  2234.32671   │
│   3   │ cpp            │ 2        │ davepl_par      │  108160  │ 5.00005  │   32    │   base    │   yes    │ 1    │   675.99324   │
└───────┴────────────────┴──────────┴─────────────────┴──────────┴──────────┴─────────┴───────────┴──────────┴──────┴───────────────┘