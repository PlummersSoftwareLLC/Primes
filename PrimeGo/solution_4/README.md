# Go Prime Sieve solution by Richard Masci (rmasci)

The reason I am highlighting this is show how easy it is to write concurrent in Go as concurrency, and Go was designed to be concurrent -- it wasn't added in later. Today computers don't scale like they did when most of these languages were written, where you could wait a few months and you had a faster processor and more RAM. Today, it's more about the number of cores.

I took parts of solution 1 and solution 2 (Mostly from 2) to get this program. Without much work I am able to spawn 100 go routines to run at the same time, this is the first 'for' loop in the main function.  The second 'for' loop waits for 1 of the 100 go routines to finish, if it finishes with a 'true', it launches another go routine in it's place, and hopefully there will always be more than one running at the same time.

## Run instructions
1. Install Go
2. go build -o sol3 main.go
   You can also use `go run main.go`
3. ./sol3

## Output

```
[rmasci@mbpro ~/Primes/PrimeGo/solution_3] $ go run main.go
rmasci;39480;5.02;1;algorithm=base,faithful=true
```

You can set the number of go routines to launch using -g -- experiment with this as if you use too many, it will slow down, too few will also slow it down. 100 is the default 

Each go routine launches the Sieve 'loops' number of times, default is 7, You can also set the number of loops each go routine runs using -l. Again each system is going to be different, more is not better here. 

On my system, 10th gen i7 1.6ghz 100 go routines , with a loop count of 10 gave me 39480.

Even with one routine, and one loop it is twice as fast as the other solutions.
```
[rmasci@mbpro ~/go/Primes/PrimeGo/solution_3] $ go run main.go -r 1 -l 1
rmasci;14007;5.00;1;algorithm=base,faithful=true
```