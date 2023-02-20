# PHP solution by Sqonk


[![Minimum PHP Version](https://img.shields.io/badge/PHP-%3E%3D%208.2-yellow)](https://php.net/)
![Algorithm](https://img.shields.io/badge/Algorithm-other-yellow)
![Faithfulness](https://img.shields.io/badge/Faithful-no-red)
![Parallelism](https://img.shields.io/badge/Parallel-no-green)
![Bit count](https://img.shields.io/badge/Bits-1-yellowgreen)

This solution is in-effective but was a worthwhile test.

The fixed algorithm is functional but impractical for speed.

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
 

