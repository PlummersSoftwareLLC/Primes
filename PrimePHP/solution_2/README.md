# PHP8 JIT solution by HugoSantiagoBecerraAdan

![Algorithm](https://img.shields.io/badge/Algorithm-base-green)
![Faithfulness](https://img.shields.io/badge/Faithful-yes-green)
![Parallelism](https://img.shields.io/badge/Parallel-no-green)
![Bit count](https://img.shields.io/badge/Bits-unknown-yellowgreen)

Implementation for PHP8 JIT with a faithful aproach to the algorithm presented in PrimeCPP by Dave Plummer almost line by line.

This solution provides an expected **performance improvement between x3 and x4** over Solution #1.

The main code difference in comparison with the solution #1 or the CPP version is the use of `SplFixedArray` instead of raw arrays and the inverted values of the list.

A big performance penalty happens when initializing the elements of the list to `true`. Even calling the function `array_fill` it takes a significant amount of time. Using `SplFixedArray` instead of raw arrays provides a faster aproach in comparison when handling large lists with fixed number of elements.

In order to improve more the performance, the elements of the list object are not initialized with `true` value, so they keep the default value of `null` and all validation works in inverted form.

```php
// Creates the whole list of elements with 'null' as value
$this->rawBits = new SplFixedArray($this->sieveSize);
```

The validation and asignments are inverted to keep the functionality intact.

```php
// Inverted validation
if (null === $this->rawBits[$num]) {
```

```php
// Inverted value asignment
$this->rawBits[$num] = true;
```

## Run instructions

`./run.sh`, requires PHP 8.0 or above.

Install PHP (CLI - Command Line Interface)

> PHP8 is provided with Ubuntu and installable using `sudo apt install php-cli`

How to verify installed PHP version:

```bash
$ php -v
PHP 8.0.8 (cli) (built: Jul  1 2021 15:26:46) ( NTS )
```

## Output

* **OS**: GNU/Linux (Kubuntu 20.04) 5.4.0-77-generic 64 bits
* **CPU**: 8 × Intel® Core™ i7-8550U CPU @ 1.80GHz
* **RAM**: 16 GiB

### Native performance

```bash
$ ./run.sh
Passes: 128, Time: 5.011532, Avg: 0.039153, Limit: 1000000, Count: 78498, Valid: 1

HugoSantiagoBecerraAdan;128;5.011532;1;algorithm=base,faithful=yes
```

Compared to other PHP implementations:

*Solution 1 (with corrected time limit of 5 sec.)*

> Solution 1 uses a limit of **10 sec.** for execution time instead of 5 like the other implementations, so results must be inaccurate for comparisons.

     Passes: 37, Time: 5117ms, Avg: 138ms, Limit: 1000000, Count: 78498, Valid: True

     DennisdeBest;37;5.117794;1;algorithm=base,faithful=yes
