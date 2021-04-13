# Contributing

## Guide

Once you've written the solution, make a determination on whether it is
faithful to the original implementation, and whether it is parallel or not.
You can do this according to [these category descriptions](#Categories).

Now make sure to add a `README.md` conforming to at least this template:

```
# <Language> solution by <YourUserName>

<CategoryBadge>

*Give a short description of your implementation*

## Run instructions

*Describe how to run your application here*

## Output

*Show the output you got on your machine(s) here, in code-blocks*
```

*TODO: Add the different category badges here*

Now look on the `drag-race` branch, and see what the highest numbered
implementation is for the language you chose, and place your solution
in the following folder:
`Prime<Language>/<solution>_<highest+1>/` where highest is the number of the
highest numbered solution you found.
Once you've done that, you are kindly requested to add a `Dockerfile` that
is configured to run your solution and place the result, in the requested
format, on standard-output.

Finally, submit a Pull request targetting the branch `drag-race`, and
place at least the name of the language in the title.

## Categories

### Faithful

* Your solution uses no external dependencies to calculate the actual sieve.
* Your solution uses a class to encapsulate the sieve, or an equivalent
   feature in your language. This class must contain the full state of the
   sieve. Each iteration should re-create a new instance of this class.
* Your solution does not use multi-threading or multi-processing.
* Your solution conforms to the base [rules](#Rules).

### Parallel faithful

* Your solution uses no external dependencies to calculate the actual sieve
   (example, `-lpthread` is fine).
* Your solution uses a class to encapsulate the sieve, or an equivalent
   feature in your language. This class must contain the full state of the
   sieve. Each iteration should re-create a new instance of this class.
* Your solution conforms to the base [rules](#Rules).

### Unfaithful

* Your solution conforms to the base [rules](#Rules).

## Rules

* Your solution uses the sieve of Erastosthenes.
* Your benchmarked code returns either a list of primes
   or the `is_prime` array, containing the result
   of the sieve.
* Your solution runs for at least 5 seconds, and stops as quickly as possible
   after that.
* Your solution calculates all the primes up to 1,000,000.
* The test code outputs the following text to standard output:
   ```
   <name>;<iterations>;<total_time>;<num_threads>
   ```
   where `name` is **at least** your username, and if you have multiple
   implementations, a short keyword to discriminate each implementation,
   `iterations` is the amount of times your code ran in the allotted time,
   and `total_time` is the total time it took to run, which would be sligtly
   more than 5 seconds, in an `en_US` formatted decimal value (so, use `.`
   instead of `,` for the decimal separator).
   `num_threads` is the amount of threads that were used (so 1 for a
   single-threaded solution).
* Your solution adheres to the requirements of the category.
* You own copyright to all code and are willing to license that code under
   BSD-3 or compatible, or the code is available under BSD-3 or compatible.

