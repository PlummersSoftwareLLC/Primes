# Algol 68g solution by rzuckerm

![Algorithm](https://img.shields.io/badge/Algorithm-base-green)
![Faithfulness](https://img.shields.io/badge/Faithful-no-yellowgreen)
![Parallelism](https://img.shields.io/badge/Parallel-no-green)
![Bit count](https://img.shields.io/badge/Bits-1-green)

Faithfulness is *no* due to not being able to run a sieve size greater than 100 million.

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

On a Intel i7-7500U CPU @ 2.7 GHz with 16 GB of memory on a Windows 10 laptop running
a Ubuntu 22.04 VM in VirtualBox 7:

```
Passes: 138, Time: 5.02173100, Avg: .03638936, Limit: 1000000, Count1: 78498, Count2: 78498, Valid: true

rzuckerm;138;5.02173100;1;algorithm=base,faithful=no,bits=1
```

