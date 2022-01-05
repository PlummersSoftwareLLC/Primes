
# Lua solution by Mooshua
![Algorithm](https://img.shields.io/badge/Algorithm-wheel-yellowgreen)
![Faithfulness](https://img.shields.io/badge/Faithful-no-yellowgreen)
![Parallelism](https://img.shields.io/badge/Parallel-no-green)
![Bit count](https://img.shields.io/badge/Bits-8-yellowgreen)
![Bit count](https://img.shields.io/badge/Bits-unknown-yellowgreen)

This implementation is very simple, and it bites the bullet of allocating a full 1MB of memory for the sieve.
The solution takes advantage of LuaJIT's runtime code emitting to heavily unroll the heaviest loop into a dependent instruction chain. 

The solution uses a wheel factorization algorithm to complete the sieve, and is not faithful as the runtime buffer cannot be resized and has no OOP-like structure (plus the wheel is hardcoded...)

The bitsize of the LJ_Hashtable and LJ_Hash_VM benchmarks is unknown and architecture-specific.

all_primes is collected from http://my.core.com/~katiemarie10/prime/prime1-1G.htm.

## Run instructions
### Windows & *nix
1. Install LuaJIT (https://luajit.org/download.html and https://luajit.org/install.html)
    > **Note**: **Clone the git repo. Do not download the .tar.gz or .zip**
2. Run `luajit prog.lua bench` (on windows, you may need luajit.exe)
3. For additional options, run with no arguments (`luajit prog.lua`)

## Output
Intel i9-9900 8-core 16-thread @ 3.10 GHz
```
mooshua_lj_b8_u32;6875;5.015625;1;algorithm=wheel,faithful=false,storage=8,parallelism=1
mooshua_lj_b8_u24;6968;5.015625;1;algorithm=wheel,faithful=false,storage=8,parallelism=1
mooshua_lj_b8_u16;6950;5.015625;1;algorithm=wheel,faithful=false,storage=8,parallelism=1
mooshua_lj_b8_u1;6777;5.015625;1;algorithm=wheel,faithful=false,storage=8,parallelism=1
mooshua_lj_hashtable_8;3027;5.015625;1;algorithm=wheel,faithful=false,storage=unknown,parallelism=1
mooshua_lj_hash_vm;606;5.015625;1;algorithm=wheel,faithful=false,storage=unknown,parallelism=1
```

## Test instructions
This test can be used to benchmark the completeness of the solution, and to debug any issues with the sieve.
1. Run `luajit prog.lua quick` or `luajit prog.lua dump`
2. Run `luajit test.lua`

## Profiling
Use `emit` (`luajit prog.lua emit 5 32`) to create a function to visualize profiler results. Once you have the emmitted information and have placed it into a file, use the LuaJIT profiler (`-jv`) and mentally replace instances of `[string]` with the line number in the emitted code file. For this, it is recommended to use the `l` option (`-jv=l`) for line numbers.

*This is not effective for the benchmarks* as they use different parameters, but you can still use `luajit prog.lua emit 5 32` to get a gist of what's going on. This profiling is most effective for `prog.lua once`.