# Euphoria solution by rzuckerm

![Algorithm](https://img.shields.io/badge/Algorithm-other-yellowgreen)
![Faithfulness](https://img.shields.io/badge/Faithful-no-yellowgreen)
![Parallelism](https://img.shields.io/badge/Parallel-no-green)
![Bit count](https://img.shields.io/badge/Bits-unknown-yellowgreen)

* `primes.ex` uses the built-in 
  [prime_list function](https://openeuphoria.org/docs/std_primes.html#_4387_prime_list).
  After looking at the
  [source code](https://github.com/OpenEuphoria/euphoria/blob/4.1.0/include/std/primes.e)
  for this function, it looks like it uses trial division

## Run instructions

Build the docker image with this:

```bash
./build.sh
```

You should only need to do this once. Run the docker image:

```bash
./run.sh
```

## Output

On an Intel(R) Core(TM) i7-8700 CPU @ 3.20GHz with 32 GB of memory on a Windows 10 desktop running
a Ubuntu 22.04 VM in VirtualBox 6.1:

Passes: 70834, Time: 5.00, Avg: 0.00007059, Limit: 1000000, Count: 78498, Valid: true                                                                                                                        

rzuckerm-builtin;70834;5.00;1;algorithm=other,faithful=no
