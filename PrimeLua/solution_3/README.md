
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
--  Non-linear sieve versions optimized for minimal L1 cache evictions
--  Faster on most CPUs with good caches
--  First number = unroll factor, second = cache block size
mooshua_luajit_16_c8k;11656;5.001;1;algorithm=base,faithful=no,bits=8
mooshua_luajit_16_c16k;16600;5.001;1;algorithm=base,faithful=no,bits=8
mooshua_luajit_16_c24k;18584;5.001;1;algorithm=base,faithful=no,bits=8
mooshua_luajit_16_c32k;19654;5.001;1;algorithm=base,faithful=no,bits=8
mooshua_luajit_16_c48k;20206;5.001;1;algorithm=base,faithful=no,bits=8
mooshua_luajit_16_c64k;16307;5.001;1;algorithm=base,faithful=no,bits=8
mooshua_luajit_8_c24k;18853;5.001;1;algorithm=base,faithful=no,bits=8
mooshua_luajit_8_c32k;19955;5.001;1;algorithm=base,faithful=no,bits=8
--  Standard linear sieve, number = unroll factor
mooshua_luajit;8772;5.001;1;algorithm=base,faithful=no,bits=8
mooshua_luajit_16;8721;5.001;1;algorithm=base,faithful=no,bits=8
mooshua_luajit_8;8783;5.001;1;algorithm=base,faithful=no,bits=8
mooshua_luajit_1;8829;5.001;1;algorithm=base,faithful=no,bits=8
--  Versions which use slower LuaJIT constructs, more of a "business logic" benchmark
mooshua_luajit_hash;4572;5.001;1;algorithm=base,faithful=no,bits=8
mooshua_luajit_array;4552;5;1;algorithm=base,faithful=no,bits=8
--  Versions with critical JIT compiler optimizations disabled
--  Uses 2X memory!
mooshua_luajit_slow_ffi;2415;5.001;1;algorithm=base,faithful=no,bits=8
mooshua_luajit_slow_hash;931;5.002;1;algorithm=base,faithful=no,bits=64
--  Versions that only use the interpreter
mooshua_luajit_vm_ffi;160;5.002;1;algorithm=base,faithful=no,bits=8
mooshua_luajit_vm_hash;348;5.011;1;algorithm=base,faithful=no,bits=64
```

Each level (_24, _16, etc.) is a different unroll level. The loop is manually unrolled for very small performance gains over LuaJIT's unroller.

## Test instructions
This test can be used to benchmark the completeness of the solution, and to debug any issues with the sieve.
1. Run `luajit prog.lua quick` or `luajit prog.lua dump` or `luajit prog.lua l1` (for cache-optimized)
2. Run `luajit test.lua`

## Profiling
Use `emit` (`luajit prog.lua emit 5 24`) to create a function to visualize profiler results. Once you have the emmitted information and have placed it into a file, use the LuaJIT profiler (`-jp`) and mentally replace instances of `[string]` with the line number in the emitted code file. For this, it is recommended to use the `l` option (`-jp=l`) for line numbers.

For annotation of the source code, use `-jp=m0i0flA` (`luajit -jp=m0i0flA prog.lua o`).

## Cache-optimized

Cache optimized versions (ending in `_c--k`) use a non-linear sieve algorithm, which is broken into two steps:
1. A naive, linear sieve goes over the first `sqrt(size)` numbers, putting the ones that are prime into a list,
2. A non-linear sieve goes over every "block" of memory, and within each block sieves out the primes that stage 1 collected.

This reduces cache evictions as each block of memory is processed once, instead of `sqrt(size)` times like the standard linear sieves. Check out the sources in `/compiled/l1`!

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
Using `j` or `jit`, you can see how effective each LuaJIT optimization is. All other optimizations are enabled but the one specified (note--LuaJIT optimizations are generally closely tied, so disabling one will likely disable others. This is in no way representative of ALL, or even a major subset of, luajit programs.)
```
mooshua_lj_no_fold;7432;5.001;1;algorithm=base,faithful=no,bits=8
mooshua_lj_no_cse;7914;5.001;1;algorithm=base,faithful=no,bits=8
mooshua_lj_no_dce;7557;5.001;1;algorithm=base,faithful=no,bits=8
mooshua_lj_no_narrow;5011;5.001;1;algorithm=base,faithful=no,bits=8
mooshua_lj_no_fuse;8581;5.001;1;algorithm=base,faithful=no,bits=8
mooshua_lj_no_store;8612;5.001;1;algorithm=base,faithful=no,bits=8
mooshua_lj_no_alias;7508;5;1;algorithm=base,faithful=no,bits=8
mooshua_lj_no_sink;8475;5.001;1;algorithm=base,faithful=no,bits=8
mooshua_lj_no_array;8654;5.001;1;algorithm=base,faithful=no,bits=8
mooshua_lj_no_loop;1341;5.004;1;algorithm=base,faithful=no,bits=8
--  Cache-optimized below
mooshua_lj_no_fold_c;5986;5.001;1;algorithm=base,faithful=no,bits=8
mooshua_lj_no_cse_c;8362;5.001;1;algorithm=base,faithful=no,bits=8
mooshua_lj_no_dce_c;10804;5.001;1;algorithm=base,faithful=no,bits=8
mooshua_lj_no_narrow_c;5065;5.001;1;algorithm=base,faithful=no,bits=8
mooshua_lj_no_fuse_c;14500;5.001;1;algorithm=base,faithful=no,bits=8
mooshua_lj_no_store_c;18222;5.001;1;algorithm=base,faithful=no,bits=8
mooshua_lj_no_alias_c;7813;5.001;1;algorithm=base,faithful=no,bits=8
mooshua_lj_no_sink_c;7847;5.001;1;algorithm=base,faithful=no,bits=8
mooshua_lj_no_array_c;7882;5.001;1;algorithm=base,faithful=no,bits=8
mooshua_lj_no_loop_c;1269;5.004;1;algorithm=base,faithful=no,bits=8
```