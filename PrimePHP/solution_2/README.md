# PHP solution by Sqonk


[![Minimum PHP Version](https://img.shields.io/badge/PHP-%3E%3D%208.2-yellow)](https://php.net/)
![Algorithm](https://img.shields.io/badge/Algorithm-base-green)
![Faithfulness](https://img.shields.io/badge/Faithful-yes-green)
![Parallelism](https://img.shields.io/badge/Parallel-no-green)
![Bit count](https://img.shields.io/badge/Bits-1-yellowgreen)

This solution attempts to boost the performance out of PHP significantly, by using anything that will allow the interpreter and the Opcache JIT to precompile where possible. 

The prime count storage utilises bitwise operations, as with many other high performance solutions for other languages. However, PHP has a limitation with regard to its type handling, and the maximum size it can allocate for a single integer. And so in my experiments simply replacing an array of bools with a singular int will only output the correct count for a sieve of 100 or 10. 

This solution instead runs a series of ints as storage for every 100 units, storing the whole lot in an array. 

_Finally_, of particular note (especially after the latest episode of Dave's Garage) is the core algorithm in this solution is heavily influenced and aided by ChatGPT, which assisted over many iterations of trial and error in producing a workable solution. In the spirit of that video's key frame - it really did help '10x' my solution!

### Running with PHP

This solution is designed to run with PHP 8.2 or later. You will need The opcache module enabled and its JIT switched on for best results.

```
cd path/to/sieve
```
```
php prime.php [--once] [--print] [--size=sieveSize]
```

### Command line arguments

 - `--size=X`: set upper limit for calculating primes. Default is 1_000_000.
 - `--print`: Output the result of the sieve to StdOut (console).
 - `--once`: Run the sieve only once, instead of the 5 second loop. Useful for testing output correctness.
 
 
### Running tests

Unit tests exist for and pass for sieve sizes 10 thru 100,000,000, with sizes 10, 100 and 1000 also verifying output of found primes.


First install Unit Test dependancies:

 ```
 cd path/to/sieve
 composer install
 ```
 Then run tests:
 ```
 vendor/bin/phpunit --testdox tests
 ```
 
### Benchmarks / Performance

Personal computer:

 - Apple M1 Max (ARM64) 64-bit
 - PHP: 8.2.2
 - Opcache: enabled
 - Opcache JIT: enabled

#### Sieve 100,000
sqonk;416899;5.00;1;algorithm=base,faithful=yes,bits=1

#### Sieve 1,000,000
 sqonk;47706;5.00;1;algorithm=base,faithful=yes,bits=1
 
#### Sieve 10,000,000
 sqonk;4200;5.00;1;algorithm=base,faithful=yes,bits=1
 
 
#### Solution_1 Sieve of 1,000,000
DennisdeBest;1169;5.011909;1;algorithm=base,faithful=yes,bits=8

