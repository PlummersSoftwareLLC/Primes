# Terra solution by Enter1he


8-bit solution for counting the number of primes up to 1 million written in Terra programming language.
Now we are using Terra capabilities to define arrays at compile time. Inspired by CPP solution_3, however I still
don't know how to realize certain aspects in Terra.

## Run instructions

There's no way to install Terra through the package so the best way is to get
release from a terralang/terra GitHub.
After that to run the program you need a Terra compiler in your PATH

Put the folowing command in terminal:
'''
terra Primes.t
'''
Resulting program would be right in the program folder

## Output
From AMD Ryzen 5 3500U 3.7 Ghz 4 cores

Computing primes to 1000000 on 1 thread for 5 seconds.

Passes: 195192, Time: 5.000000, Avg: 0.000026, Limit: 1000000, Count: 78498, Valid: 1
Enter1he;195192;5.000000;1;algorithm=other,faithful=no,bits=8
