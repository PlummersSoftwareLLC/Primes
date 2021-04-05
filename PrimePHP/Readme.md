# WHAT IS IT?
Primes based on the code from Dave Plummer Primes repository.

A PHP copy of the algorithm presented in PrimeCPP.

# PERFORMANCE

8 × Intel® Core™ i7-4771 CPU @ 3.50GHz

```
// PHP 7.4 or PHP 8.0 (without JIT compiler)
Passes: 120, Time: 5.019950, Avg: 0.041833, Limit: 1000000, Count1: 78498, Count2: 78498, Valid: 1

// PHP 8.0 (with JIT compiler)
Passes: 195, Time: 5.013734, Avg: 0.025711, Limit: 1000000, Count1: 78498, Count2: 78498, Valid: 1
```

# HOW TO BUILD

Install PHP (CLI - Command Line Interface) 7.4 or above

> PHP 8.0 is provided with Ubuntu 20.10 and installable using `sudo apt install php-cli`


# EXECUTE

PHP 7.4 or above

```bash
$ php -dopcache.enable_cli=1 -dopcache.enable=1 -dxdebug.mode=off PrimeSieve.php
```

PHP 8.0 or above (with JIT compiler)

```bash
$ php -dopcache.enable_cli=1 -dopcache.enable=1 -dopcache.jit_buffer_size=100M -dopcache.jit=1255 -dxdebug.mode=off PrimeSieve.php
```
