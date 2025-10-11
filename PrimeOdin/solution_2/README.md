# Odin solution by Agnar Renolen
Just implemented the original C++ implementation in Odin.

Could not compile the multithreaded and bit versions of Ginger Bill and Kelimion's solution with my current odin installation (version dev-2025-08) on my laptop. But I got a significant improvement on the single-threaded byte version.

The bit version was just an experiment to figure out how the *core:collection/byte_array* package would perform. Surprisingly well, I'd say.

## Tested variations
Rather than allocate and deallocate the sieve for each iteration, I tried to create one sieve, and reset it by "memcopy-ing" from a blank sieve between each iteration. I was surprised to see that this was slightly slower.

## Run instructions
This solution was developed under odin *dev-2025-8*. It should work on later versions, and may also work on some earlier versions, though the *bit_array* package implementation used in *dev-2025-8* is quite new.

You should run the program using **odin run . -o:speed**.
I also tried the *aggressive* optimization without any improvement.

## Output
On my laptop:

    arenol;8377,5.0000;1;algorithm=base,faithful=yes,bits=8
    arenol;7731;5.0006;1;algorithm=base,faithful=yes,bits=1

Compared to Ginger Bill and Kelimion's solution with the same laptop and compiler:

    odin_byte_moe;7461;5.0005955;1;algorithm=base,faithful=yes,bits=8

