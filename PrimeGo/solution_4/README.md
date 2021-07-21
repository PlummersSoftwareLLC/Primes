# Go solution by kpym

4-bit real-multi-threaded Go solution

## Run instructions

 - Install [Go](https://golang.org/)
 - Run:
```
> go run . [args]
```

### Run arguments

```
> go run . -h
Usage of primes:
  -limit int
        calculate primes up to limit (default 1000000)
  -routines int
        number of workers to use (default 24)
  -time duration
        sampling duration (default 5s)
```

## How it works

Check [HOW_IT_WORKS](HOW_IT_WORKS.md).

## Output

**Processor:** Intel(R) Core(TM) i7-7700HQ CPU @ 2.80GHz, 2808 Mhz, 4 Core(s), 8 Logical Processor(s)
**Total Physical Memory:** 7,89 GB
**OS:** Microsoft Windows 10 Education Version 10.0.19042 Build 19042
**Go version:** go1.16.6 windows/amd64


```
> go run .
Run for 5.0 seconds using 24 workers to builing a sieve up to 1000000...

Passes: 21150, Time: 5009 ms, Avg: 236841 ns/op, Limit: 1000000, Count1: 78498, Count2: 78498, Valid: true

kpym-go-multi;21150;5.009205;4;algorithm=base,faithful=yes
```
