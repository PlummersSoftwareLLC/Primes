# Cgo solution by ssovest

1-bit single-threaded solution that uses [cgo](https://pkg.go.dev/cmd/cgo)

It doesn't implement the base algorithm, but is close to it.

## Run instructions

 - Install [Go](https://golang.org/)

 - Install gcc (On Windows you can use [mingw-w64](http://mingw-w64.org). Make sure it's in your PATH.)

 - Run
```
go run sieve_cgo.go [args]
```

### Command line args:

`-limit X`: Limit. Default is 1000000

`-time X`: Duration, in [Go duration format](https://golang.org/pkg/time/#ParseDuration). Default is "5s"

`-v`: Provide additional human-readable output

## Output

AMD A4-3305M 1.9 GHz, Windows 7 64 bit
```
ssovest-cgo;8704;5.0042862;1;algorithm=other,faithful=no,bits=1
```