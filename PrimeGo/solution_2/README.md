# Go solution by ssovest

Collection of 1-bit single-threaded Go solutions.

*sieve8.go* stores bits in []uint8. Implements the base algorithm. The slowest one, it's here just for comparison.

*sieve32.go* stores bits in []uint32. Implements the base algorithm, but uses a datatype optimized for setting ranges of bits.

*sieve32_block.go* is a modified version of the above. Optimized for CPUs with small cache. Slower than sieve32.go if cache is not a problem.

*sieve_ptr.go* stores bits in []uint32 and uses unsafe pointers instead of slice indexing. Implements the base algorithm.

*sieve_other.go* stores bits in []uint32. Implements the Sieve of Eratosthenes, but doesn't fit into the definition of the `base` algorithm.

*sieve_other_block.go* is a modified version of the above. Uses the same optimizations as sieve32_block.go

*sieve_other_segmented.go* is a segmented version of sieve_other.go. It's not as fast as the `block` version on small limits, but is quite performant on large numbers.

Every version is compiled with boundscheck disabled (`-B` compiler flag).

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

And for `block`/`segmented` versions:

`-block`: Block size, in bits. Default is 128_000

## Output

AMD A4-3305M 1.9 GHz, Windows 7 64 bit
```
ssovest-go-u32-B;8138;5.000469686;1;algorithm=base,faithful=yes,bits=1
ssovest-go-u32-blocks-B;6916;5.001404427;1;algorithm=base,faithful=yes,bits=1
ssovest-go-u8-B;2053;5.003531582;1;algorithm=base,faithful=yes,bits=1
ssovest-go-other-B;13586;5.000640339;1;algorithm=other,faithful=yes,bits=1
ssovest-go-other-blocks-B;10723;5.000676751;1;algorithm=other,faithful=yes,bits=1
ssovest-go-other-segmented-B;8315;5.000506643;1;algorithm=other,faithful=yes,bits=1
ssovest-go-ptr-B;2360;5.000475421;1;algorithm=base,faithful=yes,bits=1
```
