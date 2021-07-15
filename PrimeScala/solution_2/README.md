# Scala (JVM, Native and ScalaJS) solution by scilari (Ilari Vallivaara)

![Algorithm](https://img.shields.io/badge/Algorithm-base-green)
![Faithfulness](https://img.shields.io/badge/Faithful-yes-green)
![Parallelism](https://img.shields.io/badge/Parallel-no-green)
![Bit count](https://img.shields.io/badge/Bits-unknown-yellowgreen)

This tail recursive solution improves the current Scala solution_1 by both performance (~4X) and correctness. Used solution_1 as a base for the build and Docker files.

## Run Instructions
* JVM

`./mill sieveJVM.run`

* Native

`./mill sieve.run`

* ScalaJS

`./mill sieveJS.run`

NOTE: You can check the validity by uncommenting the "validate" line at the end of the main method

## Output
`scilari;3514;5.0;1;algorithm=base,faithful=yes`

## Implementation Notes
* Follows the original base algorithm faithfully
* Looping over stuff is implemented as tail recursive functions (these are compiled to while loops by the Scala compiler)
* Uses sentinel in the bit (Boolean) array to avoid some checks
* Uses false as the "isPrime" value for the bit array, as Scala (Java) arrays initialize to false as default

## Performance Notes
* For some reason, the JVM version gets slightly better results on my machine (Year 2018 Ultrabook, WSL2 on Win10) than the Scala Native version
* The performance on my machine varies wildly between runs (~3-4K passes on JVM)

| Solution   | Passes JVM | Passes Native | Passes ScalaJS |
| ---------- | :--------: | :-----------: | :------------: |
| This       |    3-4K    |     ~ 3K      |     ~0.7K      |
| solution_1 |   ~0.8K    |     ~1.5K     |      n/a       |

* Note: Results compared to solution_1 are not really valid, as solution_1 seems to have some issues with the correctness. Below are the primes up to 100 produced by solution_1 (25 clearly not a prime, for example):

```
2, 3, 5, 7, 11, 13, 17, 19, 23, 25, 29, 31, 35, 37, 41, 43, 47, 49, 53, 55, 59, 61, 65, 67, 71, 73, 77, 79, 83, 85, 89, 91, 95, 97, 
rom1dep;2059635;5.0;1;algorithm=base,faithful=yes
```



