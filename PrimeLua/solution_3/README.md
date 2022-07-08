
# Lua solution by Mooshua
![Algorithm](https://img.shields.io/badge/Algorithm-base-green)
![Faithfulness](https://img.shields.io/badge/Faithful-no-yellowgreen)
![Parallelism](https://img.shields.io/badge/Parallel-no-green)
![Bit count](https://img.shields.io/badge/Bits-8-yellowgreen)
![Bit count](https://img.shields.io/badge/Bits-64-yellowgreen)

This implementation is very simple, biting the bullet of allocating large swaths of memory in order to avoid loading before storing a prime clear bit. 

While the algorithm is almost faithful, it does not re-create the class with each iteration, and is thus unfaithful. It does, however, check all other boxes, including no external dependencies, using a class system, and allocating the memory at runtime.

The algorithm uses `loadstring()` to emit specialized lua code for each benchmark (visible in `compiled/mooshua_*.lua`). Not much is specialized, however, and it is largely pointless besides manual unrolling done on the inner loop.

## Run instructions
### Windows & *nix
1. Install LuaJIT (https://luajit.org/download.html and https://luajit.org/install.html)
    > **Note**: **Clone the git repo. Do not download the .tar.gz or .zip**
2. Run `luajit prog.lua bench` (on windows, you may need luajit.exe)
3. For additional options, run with no arguments (`luajit prog.lua`)

## Output
Intel i9-9900 8-core 16-thread @ 3.10 GHz
(lines prefixed with `--` were added for clarity and are not present in output)
```
--  tuned run
mooshua_luajit;8721;5.001;1;algorithm=base,faithful=no,bits=8
--  run with different unroll factors
mooshua_luajit_16;8690;5.001;1;algorithm=base,faithful=no,bits=8
mooshua_luajit_8;8616;5.001;1;algorithm=base,faithful=no,bits=8
mooshua_luajit_1;8695;5;1;algorithm=base,faithful=no,bits=8
--  run with experimental dependency unrolling
mooshua_luajit_d_16;7687;5.001;1;algorithm=base,faithful=no,bits=8
mooshua_luajit_d_8;7722;5.001;1;algorithm=base,faithful=no,bits=8
mooshua_luajit_d_4;7984;5.001;1;algorithm=base,faithful=no,bits=8
--  run with luajit hashtable instead of ffi array
mooshua_luajit_hash;4537;5.001;1;algorithm=base,faithful=no,bits=8
--  run with baseline JIT (no major JIT optimizations)
mooshua_luajit_slow_ffi;2452;5.002;1;algorithm=base,faithful=no,bits=8
mooshua_luajit_slow_hash;944;5.003;1;algorithm=base,faithful=no,bits=64
--  run on interpreter
mooshua_luajit_vm_ffi;162;5.019;1;algorithm=base,faithful=no,bits=8
mooshua_luajit_vm_hash;692;5.005;1;algorithm=base,faithful=no,bits=64
```

Each level (_24, _16, etc.) is a different unroll level. The loop is manually unrolled for very small performance gains over LuaJIT's unroller.

## Test instructions
This test can be used to benchmark the completeness of the solution, and to debug any issues with the sieve.
1. Run `luajit prog.lua quick` or `luajit prog.lua dump`
2. Run `luajit test.lua`

## Profiling
Use `emit` (`luajit prog.lua emit 5 24`) to create a function to visualize profiler results. Once you have the emmitted information and have placed it into a file, use the LuaJIT profiler (`-jp`) and mentally replace instances of `[string]` with the line number in the emitted code file. For this, it is recommended to use the `l` option (`-jp=l`) for line numbers.

For annotation of the source code, use `-jp=m0i0flA` (`luajit -jp=m0i0flA prog.lua o`).

## JIT Effectiveness
Using `j`, you can see how effective each LuaJIT optimization is. All other optimizations are enabled but the one specified (note--LuaJIT optimizations are generally closely tied, so disabling one will likely disable others)
```
mooshua_lj_no_fold;3445;5;1;algorithm=base,faithful=no,bits=8
mooshua_lj_no_cse;3389;5.001;1;algorithm=base,faithful=no,bits=8
mooshua_lj_no_dce;7695;5.001;1;algorithm=base,faithful=no,bits=8
mooshua_lj_no_narrow;5017;5.001;1;algorithm=base,faithful=no,bits=8
mooshua_lj_no_fuse;8402;5.001;1;algorithm=base,faithful=no,bits=8
mooshua_lj_no_store;8621;5.001;1;algorithm=base,faithful=no,bits=8
mooshua_lj_no_alias;3354;5.002;1;algorithm=base,faithful=no,bits=8
mooshua_lj_no_sink;8672;5.001;1;algorithm=base,faithful=no,bits=8
mooshua_lj_no_array;8688;5.001;1;algorithm=base,faithful=no,bits=8
mooshua_lj_no_loop;1243;5.003;1;algorithm=base,faithful=no,bits=8
```