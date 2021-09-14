# COMAL solution by kottm, adapted for OpenCOMAL by rbergen

![Algorithm](https://img.shields.io/badge/Algorithm-base-green)
![Faithfulness](https://img.shields.io/badge/Faithful-no-yellowgreen)
![Parallelism](https://img.shields.io/badge/Parallel-no-green)
![Bit count](https://img.shields.io/badge/Bits-1-green)

This is an adaption of kottm's COMAL solution that targets UniComal. This version has been changed to allow it to be run under [OpenCOMAL](https://github.com/poldy/OpenCOMAL), specifically [version 0.3.0](https://github.com/poldy/OpenCOMAL/releases/tag/v0.3.0). Amongst others, this allows the solution to be run within Docker.
Changes made to the original solution are the following:
- The sieve size is set to the "standard" 1,000,000.
- The calculation of `checkindex#` is done using `LOG10()` instead of `LOG()`. The division involved in the latter triggered an integer rounding error that yielded the wrong index.
- Instead of `TIMER`, the solution uses `SYS(now)` for timekeeping, at the cost of losing timing precision (i.e., duration is now counted in whole seconds). The reason is that OpenCOMAL doesn't include support for `TIMER`.
- Output is being redirected to a file, so it is accessible outside of the OpenCOMAL runtime environment.
- All output (`PRINT` commands) have been removed, except for the final output lines.

While putting the Dockerfile and support scripts together, I found that OpenCOMAL doesn't respond well to the absence of a TTY. The `build.sh` and `run.sh` scripts include some `socat` trickery and input/output redirection to work around this.

## Run instructions

### Docker
A Dockerfile has been provided. It can be used to build and run the solution using the following commands:
```
docker build -t comal-2 .
docker run -ti --rm comal-2
```

## Output
```
Passes: 1, Time: 13, Avg: 13, Count1: 78498, Count2: 78498, Valid: 1

kottm;1;13;1;algorithm=base,faithful=no,bits=1
```