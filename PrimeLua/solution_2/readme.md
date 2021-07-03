# Primes Lua
Lua implementation of the prime seive by Ben.

## Running
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
