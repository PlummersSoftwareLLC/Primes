# Go solution by ssovest

Collection of 1-bit single-threaded Go solutions.

*sieve8.go* stores bits in []uint8. Implements the base algorithm.

*sieve32.go* stores bits in []uint32. Implements the base algorithm.

*sieve_ptr.go* stores bits in []uint32 and uses unsafe pointers instead of slice indexing. Implements the base algorithm.

*sieve_other.go* stores bits in []uint32 and uses unsafe pointers instead of slice indexing. Implements other algorithm, but is close to the base one.

Every file compiles in 2 versions: with and without "-B" flag, which disables bounds check.

## Run instructions

 - Install [Go](https://golang.org/)

 - Run
```
go run sieve8.go [args]
```
or with disabled bounds check:
```
go run --gcflags="-B" sieve8.go [args]
```

### Command line args:

`-limit X`: Limit. Default is 1000000

`-time X`: Duration, in [Go duration format](https://golang.org/pkg/time/#ParseDuration). Default is "5s"

`-v`: Provide additional human-readable output

## Output

AMD A4-3305M 1.9 GHz, Windows 7 64 bit
```
ssovest-go;2434;5.0042862;1;algorithm=base,faithful=yes,bits=1
```
