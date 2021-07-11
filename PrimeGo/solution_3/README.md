# Go Prime Sieve -- Using Go Routines

The reason I am highlighting this is show how easy it is to write concurrent in Go as concurrency, and Go was designed to be concurrent -- it wasn't added in later. Today computers don't scale like they did when most of these languages were written, where you could wait a few months and you had a faster processor and more RAM. Today, it's more about the number of cores.

I took parts of solution 1 and solution 2 (Mostly from 2) to get this program. Without much work I am able to spawn 100 go routines to run at the same time, this is the first 'for' loop in the main function.  The second 'for' loop waits for 1 of the 100 go routines to finish, if it finishes with a 'true', it launches another go routine in it's place, and hopefully there will always be more than one running at the same time.
```
bundgaard;4449;5.001030;1;algorithm=base,faithful=yes
ssovest-go;5368;5.00079764;1;algorithm=base,faithful=yes,bits=1
rmasci;28479;5.00;1;algorithm=base,faithful=true
```

You can set the number of go routines to launch using -g -- experiment with this as if you use too many, it will slow down, too few will also slow it down. 1000 is the default and seems to work best, and on my macbook pro I got 26,562 using 1000 go routines. Even with one routine is twice as fast as the other solutions. 

```
[rmasci@mbpro ~/go/Primes/PrimeGo/solution_3] $ go run main.go  -g 1
rmasci;11188;5.00;1;algorithm=base,faithful=true
```