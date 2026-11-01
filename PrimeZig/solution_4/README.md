# Zig solution by joeshacks

![Algorithm](https://img.shields.io/badge/Algorithm-base-green)
![Faithfulness](https://img.shields.io/badge/Faithful-no-yellowgreen)
![Parallelism](https://img.shields.io/badge/Parallel-no-green)
![Bit count](https://img.shields.io/badge/Bits-1-green)
![Deviation](https://img.shields.io/badge/Deviation-compile%20time-blue)

The sieve runs at compile time, using Zig's `comptime`. This is the Zig
counterpart of `PrimeCPP/solution_3` (`consteval`) and the `hashdot`
variant in `PrimeLisp/solution_2` (read-time evaluation).

`Sieve.run` is the base algorithm over odd numbers, with one bit per
number. Starting at `factor * factor`, it marks every multiple with a step
of `2 * factor`. The `computed` constant is a sieve that the compiler has
already run (in about 10 seconds), so the program contains the finished
result. Each timed pass takes that result and checks the clock. After the
loop, the result is counted and checked against 78,498 primes.

*Deviation*: the sieve's work happens during compilation, not in the timed
loop. *Faithfulness*: the sieve size and buffer are fixed at compile time,
so this is not faithful.

## Variants

Both variants run the same loop: take the result, count the pass, check the
clock. The reported time always comes from `CLOCK_MONOTONIC`.

- `joeshacks-comptime` checks `CLOCK_MONOTONIC` on every pass, like the C++
  solution.
- `joeshacks-comptime-coarse-clock` (Linux only) checks
  `CLOCK_MONOTONIC_COARSE` instead, which is much cheaper to read but only
  ticks every few milliseconds. It stops using the coarse clock two ticks
  before the deadline and checks `CLOCK_MONOTONIC` from there. That way the
  run still ends right after 5 seconds rather than up to a tick early or
  late. Nothing else differs, so the gap between the two variants is the
  cost of the clock read.

## Run instructions

Docker:

```
docker build -t primezig-joeshacks-comptime .
docker run --rm primezig-joeshacks-comptime
```

Locally, with Zig 0.16:

```
zig build-exe src/main.zig -O ReleaseFast -lc -femit-bin=PrimeZig
./PrimeZig
```

## Output

Apple M1 Max, Docker (linux/arm64):

```
joeshacks-comptime;270057615;5.000000;1;algorithm=base,faithful=no,bits=1
joeshacks-comptime-coarse-clock;1770538428;5.000000;1;algorithm=base,faithful=no,bits=1
```

On the same machine, alternating with these runs, `PrimeCPP/solution_3`'s
single-threaded run made 245,031,606 to 245,380,853 passes.
