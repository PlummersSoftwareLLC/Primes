# D Solution by Eagerestwolf

![Algorithm](https://img.shields.io/badge/Algorithm-base-green)
![Faithfulness](https://img.shields.io/badge/Faithful-yes-green)
![Parallelism](https://img.shields.io/badge/Parallel-no-green)
![Bit count](https://img.shields.io/badge/Bits-8-yellowgreen)

D, or dlang, is a programming language that used to pit itself as a direct
competitor to C and C++, and it's easy to see why: it's a statically typed,
low level programming language with a very "C-like" syntax. Their tagline is
"Fast Code. Fast.", but how fast is it really? The answer, very.

Also, just like with my Dart solution, if you want an explanation of the code,
then open the source and read the comments. I firmly believe in good
documentation.

## Run Instructions

**Good news arm users, this solution supports you out of the box!!**

Much like my Dart solution, you have two options: download `dub` and build the
solution, or just use the Dockerfile.

### Docker

*NOTE*: The docker builds don't use `dub` or `dmd`. The reason is speed.
Instead, this solution uses [GDC](https://gdcproject.org) to build a highly
optimized executable (with `GDB` support to boot).

Again, much like my original Dart solution, this solution uses a 2 stage
pipeline. First, it builds the app in one container; then promptly trashes
that container, and runs the app in a fresh container for max "sanic-ness".
Running the solution, is a simple command

```bash
# For amd64
docker build -t prime-d . && \
docker run --rm -it prime-d

# For arm64 (Apple M1, Raspberry Pi Model 3-4 (some 2's), etc)
docker buildx build --platform linux/arm64 -t prime-d:arm64 . && \
docker run --rm -it prime-d:arm64

# For armv7 (Raspberry Pi Model 1-2 (most 2's), etc)
docker buildx build --platform linux/arm/v7 -t prime-d:armv7 . && \
docker run --rm -it prime-d:armv7
```

If Docker complains that buildx is not available, you'll need to go into your
Docker settings and turn "Experimental Features" on (NOTE: this is not required
on the M1 builds of Docker, it's enabled by default).

### dub

[dub](https://github.com/dlang/dub) is the official package manager and build
tool for D, similar to `npm` for nodejs. Follow the instructions on the Github
page to install dub for your specific distribution/OS, then simply run the
following command in the solution directory:

```bash
dub run -b 'release'
```

## Output

### Machine Specifications

* **Model**: Apple MacBook Pro (Late-2020, M1) (MacBookPro17,1)
* **CPU**: Apple M1
* **RAM**: 8GB DDR4
* **GPU**: Apple M1
* **OS**: MacOS Big Sur 11.3.1

### Docker Results

```
Passes: 9306, Time: 5.000000, Avg: 0.000537, Limit: 1000000, Count1: 78498, Count2: 78498, Valid: 1

eagerestwolf;9306;5.000000;1;algorithm=base,faithful=yes,bits=8
```

### dub

```
Passes: 8514, Time: 5.000000, Avg: 0.000587, Limit: 1000000, Count1: 78498, Count2: 78498, Valid: 1

eagerestwolf;8514;5.000000;1;algorithm=base,faithful=yes,bits=8
```