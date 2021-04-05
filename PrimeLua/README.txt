Converted from the C# implementation, and designed to look as close as possible (including a conditional that always responds to the same input). 

Only three changes come to mind - 
 - It's not a Module (which is similar to a C# Class) because that would have consumed valuable minutes
 - I changed the format of the bit array. Lua isn't strongly typed, so don't feel too bad. I changed it to make the search more simple. The input is now the key (sort of. Lua doesn't like non consecutive numerical keys in tables)
 - Lua starts at 1 in tables (arrays) which caused a bug that I fixed poorly (I just translated the code, so wasn't close enough to debug it well). That caused the output to show the wrong count. The easiest fix was to kill that code and write is again. It's only three lines or therabouts.

TO RUN LUA FILE
 - Install Lua interpreter (tested against 5.1.5-51)
 - run `lua Primes.lua` to run with default parameters (target of 1000, durations of 2 seconds) OR...
 - eun `lua Primes.lua [target] [dration in seconds]` to set your own parameters, ie, `lua Primes.lua 1000000 5` (NB - I suggest you don't bother with a target this high. Lua is very, very slow!)

TO RUN RBXL
 - install Roblox Studio 
 - launch the RBXL file.

TO RUN RBXL W/O ROBLOX STUDIO
 - go here to https://www.roblox.com/games/6602512508/Primes