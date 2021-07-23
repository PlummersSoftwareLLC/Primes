# Prime Sieve Algorithms
![Algorithm](https://img.shields.io/badge/Algorithm-base-green)
![Faithfulness](https://img.shields.io/badge/Faithful-yes-green)
![Parallelism](https://img.shields.io/badge/Parallel-yes-green)
![Parallelism](https://img.shields.io/badge/Parallel-no-green)
![Bit count](https://img.shields.io/badge/Bits-unknown-yellowgreen)

## Kotlin Solution

Speed should be pretty close to what you get with a plain Java implementation.

### Solutions

| Solution | Description |
| ------ | ------- |
| Idiomatic | Focused on a readable, idiomatic way to write Kotlin code, avoiding compiler workarounds and thus probably more representative of what you would find in a real-world project.
| Idiomatic Fast | Identical to Idiomatic, but contains two compiler optimizations. |
| Traditional | A traditional solution that doesn't make use of any Java-specific features |

#### Compiler Optimizations

The Idiomatic Solution Mainly fails at two points:
1. Badly optimized `for` loops. If you use the `step` function, the range object doesn't get eliminated, so we resort
to a workaround function
```kotlin
inline fun fastFor(from: Int, to: Int, step: Int, block: (Int) -> Unit) {
    var i = from
    while (i <= to) {
        block(i)
        i += step
    }
}
```
2. Integer boxing. Usually, Integers become Java Primitive types, however if you make an integer nullable,
it **has** to become the boxed `Integer`. To work around this, we can use `-1` as the `null` equivalent and replace
   the optional chains with inline functions.

### Contributors

| Contributor | Contribution |
----- |---------
| wulkanat | Idiomatic & Idiomatic Fast Solution, Polishing, (unfinished) Multiplatform |
| bluefireoly | Traditional Solution, Coroutines |

## Usage

Use the command line to run individual tests.
You may supply multiple arguments, tests are going to be ran in the supplied order.

| Argument | Description |
----- |---------
| `-t` | Traditional Solution |
| `-t:m` | Traditional Solution (Multithreaded) |
| `-if` | Fast Idiomatic Solution |
| `-if:m` | Fast Idiomatic Solution (Multithreaded) |
| `-i` | Idiomatic Solution |
| `-i:m` | Idiomatic Solution (Multithreaded) |

## Results

Tested on a 6 Core i5 8400 and 16 GB 2400 MHz RAM

| Passes | Name | Output |
--- | --- | ---
| 30938| Idiomatic Fast (Multithreaded) | `kotlin_idiomatic_fast_multi;30938;5.0;1;algorithm=base,faithful=yes` |
| 30768 | Traditional (Multithreaded) | `kotlin_traditional_multi;30768;5.0;1;algorithm=base,faithful=yes` |
| 29173 | Idiomatic (Multithreaded) | `kotlin_idiomatic_multi;29173;5.0;1;algorithm=base,faithful=yes` |
| 5893 | Idiomatic Fast | `kotlin_idiomatic_fast_single;5893;5.0;1;algorithm=base,faithful=yes` |
| 5720 | Traditional | `kotlin_traditional_single;5720;5.0;1;algorithm=base,faithful=yes` |
| 5401 | Idiomatic | `kotlin_idiomatic_single;5401;5.0;1;algorithm=base,faithful=yes` |

