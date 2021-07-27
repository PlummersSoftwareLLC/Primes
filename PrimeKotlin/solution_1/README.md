# Prime Sieve Algorithms
![Algorithm](https://img.shields.io/badge/Algorithm-base-green)
![Faithfulness](https://img.shields.io/badge/Faithful-yes-green)
![Parallelism](https://img.shields.io/badge/Parallel-yes-green)
![Parallelism](https://img.shields.io/badge/Parallel-no-green)
![Bit count](https://img.shields.io/badge/Bits-unknown-yellowgreen)

## Kotlin Solution

Speed should be pretty close to what you get with a plain Java implementation.

The primary takeaway for the JVM solution should be that Kotlin can be slower if you
don't pay attention. However, if you know what you are doing you don't
have to sacrifice readability and still get excellent performance. If you write
any performance critical code:
* Avoid primitive boxing (don't use nullable `Int?`, `Boolean?` and use `BooleanArray` over
  `Array<Boolean>`)
* Make sure your output bytecode is clean by using IntelliJ `Tools` ➔ `Kotlin` ➔
  `Show Kotlin Bytecode` ➔ `Decompile`


All solutions are also available in Kotlin/JS and Kotlin/Native with Kotlin Multiplatform.
The takeaway for this is don't expect them to be fast.

### Solutions

| Solution | Description |
| ------ | ------- |
| Idiomatic | Focused on a readable, idiomatic way to write Kotlin code, avoiding compiler workarounds and thus probably more representative of what you would find in a real-world project.
| Idiomatic Fast | Identical to Idiomatic, but contains two compiler optimizations. |
| Traditional | A traditional solution that doesn't make use of any Java-specific features |

### Limitations

#### Docker/Build

* [ ] Native should support multi-threading, but isn't fully implemented yet.
* [ ] Building using Gradle is rather slow as
   - [ ] Gradle Daemon needs to start for each task
   - [ ] Gradle needs to download various dependencies

#### Common
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

#### JS
* Some Math operations are implemented as costly functions to work around JS numbers
* JS cannot support multi-threading

### Contributors

| Contributor | Contribution |
----- |---------
| wulkanat | Idiomatic & Idiomatic Fast Solution, Multiplatform, Build |
| bluefireoly | Traditional Solution, Coroutines |
| fvbakel | Docker & Build |

## Usage

Use the command line to run individual tests.
You may supply multiple arguments, tests are going to be ran in the supplied order.

*Multithreading is only supported in Kotlin/JVM for now*

| Argument | Description |
----- |---------
| `-c=6` | Run the test with a specific amount of cores (multithreaded only) |
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
| 30938| **JVM** Idiomatic Fast (Multithreaded) | `jvm_kotlin_idiomatic_fast_multi;30938;5.0;6;algorithm=base,faithful=yes` |
| 30768 | **JVM** Traditional (Multithreaded) | `jvm_kotlin_traditional_multi;30768;5.0;6;algorithm=base,faithful=yes` |
| 29173 | **JVM** Idiomatic (Multithreaded) | `jvm_kotlin_idiomatic_multi;29173;5.0;6;algorithm=base,faithful=yes` |
| 5893 | **JVM** Idiomatic Fast | `jvm_kotlin_idiomatic_fast_single;5893;5.0;1;algorithm=base,faithful=yes` |
| 5720 | **JVM** Traditional | `jvm_kotlin_traditional_single;5720;5.0;1;algorithm=base,faithful=yes` |
| 5401 | **JVM** Idiomatic | `jvm_kotlin_idiomatic_single;5401;5.0;1;algorithm=base,faithful=yes` |
| 2415 | **Native** Traditional | `native_kotlin_traditional_single;2415;5.002;1;algorithm=base,faithful=yes` |
| 2249 | **Native** Idiomatic Fast | `native_kotlin_idiomatic_fast_single;2249;5.003;1;algorithm=base,faithful=yes` |
| 2070 | **Native** Idiomatic | `native_kotlin_idiomatic_single;2070;5.001;1;algorithm=base,faithful=yes` |
| 1180 | **JS** Idiomatic Fast | `js_kotlin_idiomatic_fast_single;1180;5.008;1;algorithm=base,faithful=yes` |
| 1172 | **JS** Idiomatic | `js_kotlin_idiomatic_single;1172;5.01;1;algorithm=base,faithful=yes` |
| 1162 | **JS** Traditional | `js_kotlin_traditional_single;1162;5.006;1;algorithm=base,faithful=yes` |