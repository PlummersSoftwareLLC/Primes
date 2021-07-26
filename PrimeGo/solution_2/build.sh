#!/bin/sh

go build -o ./sieves/sieve8 sieve8.go &&
go build -o ./sieves/sieve32 sieve32.go &&
go build -o ./sieves/sieve_ptr sieve_ptr.go &&
go build -o ./sieves/sieve_other sieve_other.go &&
go build --gcflags="-B" --ldflags="-X 'main.label=ssovest-go-uint8-B'" -o ./sieves/sieve8_b sieve8.go &&
go build --gcflags="-B" --ldflags="-X 'main.label=ssovest-go-uint32-B'" -o ./sieves/sieve32_b sieve32.go &&
go build --gcflags="-B" --ldflags="-X 'main.label=ssovest-go-ptr-B'" -o ./sieves/sieve_ptr_b sieve_ptr.go &&
go build --gcflags="-B" --ldflags="-X 'main.label=ssovest-go-other-B'" -o ./sieves/sieve_other_b sieve_other.go
