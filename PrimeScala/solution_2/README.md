# Scala (JVM, Native and ScalaJS) solution by scilari (Ilari Vallivaara)

![Algorithm](https://img.shields.io/badge/Algorithm-base-green)
![Faithfulness](https://img.shields.io/badge/Faithful-yes-green)
![Parallelism](https://img.shields.io/badge/Parallel-no-green)
![Bit count](https://img.shields.io/badge/Bits-unknown-yellowgreen)

Tail recursive Scala solution to the Primes Drag Race. Used solution_1 as a base for the build and Docker files.

## Run Instructions
* JVM

`./mill sieveJVM.run`

* Native (Note: full optimized compilation will take a long time)

`./mill sieve.run`

* ScalaJS

`./mill sieveJS.run`

NOTE: You can disable and enable results validation and warmup by toggling the flags in the main method.

## Output
`scilari;3514;5.0;1;algorithm=base,faithful=yes`

## Implementation Notes
* Follows the original base algorithm faithfully
* Looping over stuff is implemented as tail recursive functions (these are compiled to while loops by the Scala compiler)
* Uses sentinel in the bit (Boolean) array to avoid some checks
* Uses false as the "isPrime" value for the bit array, as Scala (Java) arrays initialize to false as default

## Performance Notes
* For some reason, the JVM version gets slightly better results on my machine (Year 2018 Ultrabook, WSL2 on Win10) than the Scala Native version
* The performance on my machine varies wildly between runs (~3-4K passes on JVM) and improves if even 5 sec warmup is allowed

| Solution      | Passes JVM      | Passes Native | Passes ScalaJS      |
| ----------    | :--------:      | :-----------: | :------------:      |
| This          | 3.0K (std .09K) |     ~2.7K     |     0.6K (std .04K) |
|This (warm 5s) | 3.8K (std .25K) |     n/a       |     n/a             |
| solution_1    |   ~1.8K         |     n/a       |      n/a            |




