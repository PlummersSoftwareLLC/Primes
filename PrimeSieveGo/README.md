# WHAT IS IT?
Primes based on the code from Dave Plummer Primes repository.

A Golang copy of the algorithm presented in PrimeCPP.


# PERFORMANCE
This is based on raw directly on host without any virtualization or container system.


### Intel Xeon E3-1535M v6 3.1 GHz

```
Passes: 5796, Time: 5000590800, Avg: 0.000000, Limit: 1000000, Count1: 78498, Count2: 78498, Valid: true
```

### Docker
```
$ docker run --rm --name primes -it primes:local
Passes: 2954, Time: 5000474351, Avg: 1.000000, Limit: 1000000, Count1: 78498, Count2: 78498, Valid: true
Passes: 2956, Time: 5000527939, Avg: 1.000000, Limit: 1000000, Count1: 78498, Count2: 78498, Valid: true
```

# HOW TO BUILD

Install Go, get Golang from https://golang.org/dl

```bash 
$ go run ./main.go
```
