Converted from the C# implementation, and designed to look as close as possible (including a conditional that always responds to the same input). 

Only three changes come to mind - 
 - It's not a Module (which is similar to a C# Class) because that would have consumed valuable minutes
 - I changed the format of the bit array. Lua isn't strongly typed, so don't feel too bad. I changed it to make the search more simple. The input is now the key (sort of. Lua doesn't like non consecutive numerical keys in tables)
 - Lua starts at 1 in tables (arrays) which caused a bug that I fixed poorly (I just translated the code, so wasn't close enough to debug it well). That caused the output to show the wrong count. The easiest fix was to kill that code and write is again. It's only three lines or therabouts.

To run, install Roblox Studio and launch the RBXL file.

To run without Roblox Studio, go here - https://www.roblox.com/games/6602512508/Primes