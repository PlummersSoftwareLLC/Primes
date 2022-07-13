
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
mooshua_luajit;8644;5.001;1;algorithm=base,faithful=no,bits=8
mooshua_luajit_16;8639;5.001;1;algorithm=base,faithful=no,bits=8
mooshua_luajit_8;8736;5.001;1;algorithm=base,faithful=no,bits=8
mooshua_luajit_1;8735;5.001;1;algorithm=base,faithful=no,bits=8
mooshua_luajit_hash;4410;5.001;1;algorithm=base,faithful=no,bits=8
mooshua_luajit_array;4469;5.001;1;algorithm=base,faithful=no,bits=8
mooshua_luajit_slow_ffi;2459;5.002;1;algorithm=base,faithful=no,bits=8
mooshua_luajit_slow_hash;903;5.004;1;algorithm=base,faithful=no,bits=64
mooshua_luajit_vm_ffi;160;5.027;1;algorithm=base,faithful=no,bits=8
mooshua_luajit_vm_hash;367;5.001;1;algorithm=base,faithful=no,bits=64
```

Each level (_24, _16, etc.) is a different unroll level. The loop is manually unrolled for very small performance gains over LuaJIT's unroller.

## Test instructions
This test can be used to benchmark the completeness of the solution, and to debug any issues with the sieve.
1. Run `luajit prog.lua quick` or `luajit prog.lua dump`
2. Run `luajit test.lua`

## Profiling
Use `emit` (`luajit prog.lua emit 5 24`) to create a function to visualize profiler results. Once you have the emmitted information and have placed it into a file, use the LuaJIT profiler (`-jp`) and mentally replace instances of `[string]` with the line number in the emitted code file. For this, it is recommended to use the `l` option (`-jp=l`) for line numbers.

For annotation of the source code, use `-jp=m0i0flA` (`luajit -jp=m0i0flA prog.lua o`).

## Assembly

This is the assembly of `mooshua_luajit_8`'s core non-prime-marking loop of x64 2021 luajit 2.1.0-beta3 with `ON SSE3 SSE4.1 BMI2 fold cse dce fwd dse narrow loop abc sink fuse`.

```asm
->LOOP:
7fa6e361ffb0  mov ebx, ebp
7fa6e361ffb2  shr ebx, 1
7fa6e361ffb4  movsxd rbx, ebx
7fa6e361ffb7  mov byte [rbx+rdx+0x10], 0x1
7fa6e361ffbc  add rbx, rsi
7fa6e361ffbf  mov byte [rdx+rbx+0x10], 0x1
7fa6e361ffc4  add rbx, rsi
7fa6e361ffc7  mov byte [rdx+rbx+0x10], 0x1
7fa6e361ffcc  add rbx, rsi
7fa6e361ffcf  mov byte [rdx+rbx+0x10], 0x1
7fa6e361ffd4  add rbx, rsi
7fa6e361ffd7  mov byte [rdx+rbx+0x10], 0x1
7fa6e361ffdc  add rbx, rsi
7fa6e361ffdf  mov byte [rdx+rbx+0x10], 0x1
7fa6e361ffe4  add rbx, rsi
7fa6e361ffe7  mov byte [rdx+rbx+0x10], 0x1
7fa6e361ffec  add rbx, rsi
7fa6e361ffef  mov byte [rdx+rbx+0x10], 0x1
7fa6e361fff4  add ebp, ecx
7fa6e361fff6  cmp ebp, eax
7fa6e361fff8  jle 0x7fa6e361ffb0        ->LOOP
7fa6e361fffa  jmp 0x7fa6e361005c        ->4 (exit)
```


## JIT Effectiveness
Using `j`, you can see how effective each LuaJIT optimization is. All other optimizations are enabled but the one specified (note--LuaJIT optimizations are generally closely tied, so disabling one will likely disable others)
```
mooshua_lj_no_fold;7405;5.001;1;algorithm=base,faithful=no,bits=8
mooshua_lj_no_cse;7981;5.001;1;algorithm=base,faithful=no,bits=8
mooshua_lj_no_dce;7638;5.001;1;algorithm=base,faithful=no,bits=8
mooshua_lj_no_narrow;5027;5.001;1;algorithm=base,faithful=no,bits=8
mooshua_lj_no_fuse;8675;5.001;1;algorithm=base,faithful=no,bits=8
mooshua_lj_no_store;8697;5.001;1;algorithm=base,faithful=no,bits=8
mooshua_lj_no_alias;8001;5.001;1;algorithm=base,faithful=no,bits=8
mooshua_lj_no_sink;8667;5.001;1;algorithm=base,faithful=no,bits=8
mooshua_lj_no_array;8587;5.001;1;algorithm=base,faithful=no,bits=8
mooshua_lj_no_loop;1360;5.002;1;algorithm=base,faithful=no,bits=8
```