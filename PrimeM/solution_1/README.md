# M solution by rheit

![Algorithm](https://img.shields.io/badge/Algorithm-base-green)
![Algorithm](https://img.shields.io/badge/Algorithm-wheel-yellowgreen)
![Faithfulness](https://img.shields.io/badge/Faithful-yes-green)
![Faithfulness](https://img.shields.io/badge/Faithful-no-yellowgreen)
![Parallelism](https://img.shields.io/badge/Parallel-no-green)
![Bit count](https://img.shields.io/badge/Bits-1-green)
![Bit count](https://img.shields.io/badge/Bits-unknown-yellowgreen)

This is a collection of implementations in M (also known as MUMPS), of which:

1. One is an implementation that tries to be as close to davepl's original C++ solution as possible by using one bit per number in a language without bitwise operators.
2. One drops the attempt to force bits on a language that doesn't like them but still allocates the whole array at once, so it still remains faithful to the original C++.
3. One is a more idiomatic solution that uses the fact that M arrays are actually B-trees and only allocates nodes as they are marked not prime.
4. One is an implementation that builds on #3 and borrows from Daniel Spångberg's C solution to add three different wheel sizes for progressively faster results with larger wheels.

## Run instructions

### Docker

A Dockerfile using YottaDB as the M implementation is provided and is the preferred way to run the solution.

### YottaDB

1. Compile the source files:
   ```
   yottadb ./*.m
   ```
2. Run the tests. The preferred way is to run each individually, either via the run script:
   ```
   . ./run.sh
   ```
   or directly from the command line:
   ```
   yottadb -run test0^primes
   yottadb -run test1^primes
   yottadb -run test2^primes
   yottadb -run test3^primes
   yottadb -run test4^primes
   yottadb -run test5^primes
   ```
   It is also possible to run all the tests at once with a single command line. This is not recommended because the later tests will be handicapped by running in the same process as earlier tests, but can be done:
   ```
   yottadb -run ^primes
   ```


## Output
On a Core i3-10100:
```
rheit_m_bits;16;5.117089;1;algorithm=base,faithful=yes;bits=1
rheit_m_array;17;5.039665;1;algorithm=base,faithful=yes
rheit_m_tree;19;5.051892;1;algorithm=base,faithful=no
rheit_m_8of30;25;5.037559;1;algorithm=wheel,faithful=no
rheit_m_48of210;28;5.000972;1;algorithm=wheel,faithful=no
rheit_m_480of2310;31;5.098601;1;algorithm=wheel,faithful=no
```

## Notes
M is used primarily as a database language in the healthcare and financial industries. Dating back to 1966, it predates C and has some interesting "quirks" of note here:

* There are only two value types: floating point decimal numbers and strings, which are converted between each other automatically depending on context.
* Arrays are implemented as sparse B-trees.
* Whitespace is significant and used to separate command parameters. If you have two commands on one line and are omitting the parameter to the first command, you still need the whitespace that corresponds with the missing parameter.
* Operators are evaluated strictly left to right. e.g. 1+2\*3 is evaluated as (1+2)\*3 and not as 1+(2\*3).
* Most commands can be executed conditionally by appending a colon and the condition to the command.
* If you want to execute multiple commands with a `for` or `if` command, you can either put them all on the same line or use a parameter-less `do` which will execute commands on subsequent lines starting with `.`.
* The language has an indirection operator `@` which takes the contents of a variable and executes it as if it was source code, which is how this solution fakes virtual class methods.
* Callers can pass any variable by reference by prepending it with a `.`.
* Commands can be (and usually are) abbreviated to just their first letters, making for a very terse language. The commands used in this solution are few:
  * `d` - `do`
  * `i` - `if`
  * `f` - `for`
  * `k` - `kill`
  * `n` - `new`
  * `s` - `set`
  * `w` - `write`
  * `q` - `quit`
  * `.`

## Author
Marisa Heit
