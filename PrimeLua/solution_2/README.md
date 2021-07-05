# Lua solution by BEN1JEN
![Algorithm](https://img.shields.io/badge/Algorithm-base-green)
![Faithfulness](https://img.shields.io/badge/Faithful-no-yellowgreen)
![Parallelism](https://img.shields.io/badge/Parallel-no-green)
![Storage](https://img.shields.io/badge/Bits-1-green)
![Language](https://img.shields.io/badge/Language-Lua-green)

Designed to be as fast as possible running single-threaded on LuaJIT. This
implementation isn't technically "Faithful", because it does not use an
object oriented design, instead I simply store it in an array recreated each
iteration.

I'm almost certain that this can be optimised further, but on my machine
this currently gets 700 passes, but I had an implementation capable of
getting 2000 passes, but that version was only able to find every other
prime number (because of how I was doing the bitmap).

## Run instructions
### Docker
1. Build: `docker build -t luaprimes .`
1. Run: `docker run -it luaprimes`
### Linux (and other Unixes, like MacOS, too)
1. Install LuaJIT (or just Lua), make sure to get the development packages
   too on Debian based systems (`luajit` and `libluajit-dev` on Ubuntu).
4. Run it: `luajit PrimesLua.lua`
### Windows
1. Install (LuaJIT)[https://luajit.org/download.html]
4. Run it: `luajit.exe PrimesLua.lua`

## Output
Intel i7-10750H x 12 @ 5GHz
```
ben1jen_luajit1;778;5.000000;1;algorithm=base,faithful=no,bits=1
```

AMD Ryzen 7 2700 x 16 @ 3.2GHz
```
ben1jen_luajit1;658;5.000000;1;algorithm=base,faithful=no,bits=1
```
