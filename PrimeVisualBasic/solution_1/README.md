# Visual Basic .NET Prime Sieve by DoctorDalek1963

![Algorithm](https://img.shields.io/badge/Algorithm-base-green)
![Faithfulness](https://img.shields.io/badge/Faithful-no-green)
![Parallelism](https://img.shields.io/badge/Parallel-no-green)
![Bit count](https://img.shields.io/badge/Bits-unknown-yellowgreen)

## Compiling and running

Install a VB.NET compiler from your package manager. I use `mono-vbnc` from apt, which can be installed with the command `sudo apt-get install mono-vbnc` on systems with apt. Then, run `vbnc Primes.vb` and it should generate `Primes.exe`.

Most Linux distros come with a mono runtime pre-installed, but you may need to install it with something like `sudo apt-get install mono-runtime`.

## Results on my machine

Here's an example of output on my machine:
```
Passes: 299, Time: 5.00527620315552, Avg: 0.0167400541911555852842809365, Limit: 1000000, Count: 78498, Valid: True

DoctorDalek1963;299;5.00527620315552;1;algorithm=base,faithful=no
```

Over 5 runs, I got an average of 296.4 passes in 5 seconds.

My specs are:
 - Intel i5-4460S 3.4GHz on Ubuntu 20.04.2
 - vbnc 0.0.0.5943
 - mono 6.8.0.105

## Note
I very rarely use VB.NET, and there are likely improvements to be made in my implementation. An object oriented approach with a class is also possible.