
# Lua solution by Mooshua
![Algorithm](https://img.shields.io/badge/Algorithm-base-green)
![Faithfulness](https://img.shields.io/badge/Faithful-no-yellowgreen)
![Parallelism](https://img.shields.io/badge/Parallel-no-green)
![Bit count](https://img.shields.io/badge/Bits-8-yellowgreen)
![Bit count](https://img.shields.io/badge/Bits-64-yellowgreen)

This implementation is very simple, biting the bullet of allocating large swaths of memory in order to avoid loading before storing a prime clear bit. 

While the algorithm is almost faithful, it does not re-create the class with each iteration, and is thus unfaithful. It does, however, check all other boxes, including no external dependencies, using a class system, and allocating the memory at runtime.

The algorithm uses loadstring() to emit specialized lua code for each benchmark. Not much is specialized, however, and it is largely pointless besides manual unrolling done on the inner loop.

This solution changes a Luajit optimization using the primary benchmark with the purpose of speeding up the runs ("Ohotloop=1"). You may disable this when running `prog.lua bench` by using an additional option, `prog.lua bench notune`

## Run instructions
### Windows & *nix
1. Install LuaJIT (https://luajit.org/download.html and https://luajit.org/install.html)
    > **Note**: **Clone the git repo. Do not download the .tar.gz or .zip**
2. Run `luajit prog.lua bench` (on windows, you may need luajit.exe)
3. For additional options, run with no arguments (`luajit prog.lua`)

## Output
Intel i9-9900 8-core 16-thread @ 3.10 GHz
```
mooshua_luajit_24;7016;5.001;1;algorithm=base,faithful=no,bits=8
mooshua_luajit_16;7043;5.001;1;algorithm=base,faithful=no,bits=8
mooshua_luajit_8;7025;5.001;1;algorithm=base,faithful=no,bits=8
mooshua_luajit_1;6764;5;1;algorithm=base,faithful=no,bits=8
mooshua_luajit_hash;2994;5.001;1;algorithm=base,faithful=no,bits=8
mooshua_luajit_slow_ffi;2505;5.001;1;algorithm=base,faithful=no,bits=8
mooshua_luajit_slow_hash;920;5.003;1;algorithm=base,faithful=no,bits=64
mooshua_luajit_vm_ffi;163;5.005;1;algorithm=base,faithful=no,bits=8
mooshua_luajit_vm_hash;767;5.005;1;algorithm=base,faithful=no,bits=64
```

## Test instructions
This test can be used to benchmark the completeness of the solution, and to debug any issues with the sieve.
1. Run `luajit prog.lua quick` or `luajit prog.lua dump`
2. Run `luajit test.lua`

## Profiling
Use `emit` (`luajit prog.lua emit 5 24`) to create a function to visualize profiler results. Once you have the emmitted information and have placed it into a file, use the LuaJIT profiler (`-jv`) and mentally replace instances of `[string]` with the line number in the emitted code file. For this, it is recommended to use the `l` option (`-jv=l`) for line numbers.

*This is not effective for the benchmarks* as they use different parameters, but you can still use `luajit prog.lua emit 5 24` to get a gist of what's going on. This profiling is most effective for `prog.lua once`.