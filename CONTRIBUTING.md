# Contributing

## Guide

Once you've written the solution, make a determination on whether it is
faithful to the original implementation, and whether it is parallel or not.
You can do this according to [these category descriptions](#Categories).

Place your solution in the following folder:
`Prime<Language>/<Category>/<YourUserName>/`. Once you've done that, you
are kindly requested to add either a `Dockerfile` or `README.md`
describing how to run your solution.

Finally, submit a Pull request targetting the branch `drag-race`, and
place at least the name of the language in the title.

## Categories

### Faithful

Directory name: `faithful`.

* Your solution uses no external dependencies.
* Your solution uses a class to encapsulate the sieve, or an equivalent
   feature in your language.
* Your solution does not use multi-threading or multi-processing.
* Your solution conforms to the base [rules](#Rules).

### Parallel faithful

Directory name: `parallel`.

* Your solution uses no external dependencies (example, `-lpthread` is fine).
* Your solution uses a class to encapsulate the sieve, or an equivalent
   feature in your language.
* Your solution conforms to the base [rules](#Rules).

### Unfaithful

Directory name: `unfaithful`.

* Your solution conforms to the base [rules](#Rules).

## Rules

* Your solution uses the sieve of Erastosthenes.
* Your solution returns either a list of primes
   or the `is_prime` array, containing the result
   of the sieve.
* The test code outputs the following text to standard output:
   ```
   <iterations>
   <total_time>
   <num_primes>
   ```
* Your solution adheres to the requirements of the category.
* You own copyright to all code and are willing to license that code under
   BSD-3 or compatible, or the code is available under BSD-3 or compatible.

