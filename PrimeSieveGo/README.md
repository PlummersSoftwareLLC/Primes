# Golang solution by bundgaard

Primes based on the code from Dave Plummer Primes repository.

A Golang copy of the algorithm presented in PrimeCPP.

## Run instructions

### Native 

Install Go, get Golang from https://golang.org/dl

```bash 
$ go run ./main.go

``` 

### Docker

```

$ docker run --rm --name primes -it primes:local

``` 

## Output

### Native on Intel Xeon E3-1535M v6 3.1 GHz 

```

Passes: 5796, Time: 5000590800, Avg: 0.000000, Limit: 1000000, Count1: 78498, Count2: 78498, Valid: true

``` 

### Docker output

```

Passes: 2954, Time: 5000474351, Avg: 1.000000, Limit: 1000000, Count1: 78498, Count2: 78498, Valid: true
Passes: 2956, Time: 5000527939, Avg: 1.000000, Limit: 1000000, Count1: 78498, Count2: 78498, Valid: true
```
