# Prime Sieve Algorithms
![Algorithm](https://img.shields.io/badge/Algorithm-base-green)
![Faithfulness](https://img.shields.io/badge/Faithful-yes-green)
![Parallelism](https://img.shields.io/badge/Parallel-yes-green)
![Parallelism](https://img.shields.io/badge/Parallel-no-green)
![Bit count](https://img.shields.io/badge/Bits-unknown-yellowgreen)

## Kotlin Solution

Speed should be pretty close to what you get with a plain Java implementation.

The primary takeaway for the JVM solution should be that Kotlin can be slower if you don't pay attention. However, if you know what you are doing, you don't have to sacrifice readability to get excellent performance. Here's some hints for writing high-performance code:

* Avoid primitive boxing (don't use nullable `Int?`, `Boolean?` and use `BooleanArray` over `Array<Boolean>`)
* Make sure your output bytecode is clean by using IntelliJ `Tools` ➔ `Kotlin` ➔ `Show Kotlin Bytecode` ➔ `Decompile`

All solutions are also available in Kotlin/JS and Kotlin/Native with Kotlin Multiplatform.

### Solutions

| Solution       | Description                                                                |
| -------------- | -------------------------------------------------------------------------- |
| Idiomatic      | Focuses on a readable, idiomatic way to write Kotlin code.                 |
| Idiomatic Fast | Identical to Idiomatic, but with two lesser-known compiler optimizations.  |
| Traditional    | A traditional solution that doesn't make use of any Java-specific features |

### Limitations

#### Common

The Idiomatic Solution Mainly fails at two points:

1. Badly optimized `for` loops. If you use the `step` function, the range object doesn't get eliminated, so we resort to a workaround function:
   ```kotlin
   inline fun fastFor(from: Int, to: Int, step: Int, block: (Int) -> Unit) {
       var i = from
       while (i <= to) {
           block(i)
           i += step
       }
   }
   ```
   However, this optimization may be obsolete as of Kotlin 1.8.10, as the idiomatic_fast variant is now neck-and-neck with the idiomatic variant in some variants.
2. Integer boxing. Usually, Integers become Java Primitive types, however if you make an integer nullable, it **has** to become the boxed `Integer`. To work around this, we can use `-1` as the `null` equivalent and replace the optional chains with inline functions.

#### JS

* Some Math operations are implemented as costly dynamic functions to work around limitations of JS number functions.
* JS doesn't support multi-threading; some critical KotlinX Coroutines functions are missing.

### Contributors

| Contributor  | Contribution                                              |
| ------------ | --------------------------------------------------------- |
| @wulkanat    | Idiomatic & Idiomatic Fast Solution, Multiplatform, Build |
| @bluefireoly | Traditional Solution, Coroutines                          |
| @fvbakel     | Docker & Build                                            |
| @nhubbard    | Modernization to Kotlin 1.8, Kotlin/Native Coroutines     |

## Usage

Use the command line to run individual tests. You may supply multiple arguments, which will make tests run in the supplied order.

*Multithreading is only supported in Kotlin/JVM and Kotlin/Native for now. Additionally, building Kotlin/Native on ARM Linux is not possible at this time.*

| Argument | Description                                                       |
| -------- | ----------------------------------------------------------------- |
| `-c=n`   | Run the test with a specific amount of cores (multithreaded only) |
| `-t`     | Traditional Solution                                              |
| `-t:m`   | Traditional Solution (Multithreaded)                              |
| `-if`    | Fast Idiomatic Solution                                           |
| `-if:m`  | Fast Idiomatic Solution (Multithreaded)                           |
| `-i`     | Idiomatic Solution                                                |
| `-i:m`   | Idiomatic Solution (Multithreaded)                                |

## Results

### PC

Specs: 6-core Intel Core i5-8400, 16 GB 2400 MHz RAM

Note that these benchmarks are out of sync with the most recent code revision.

| Variant                                | Passes | Output                                                                         |
| -------------------------------------- | ------ | ------------------------------------------------------------------------------ |
| **JVM** Idiomatic Fast (Multithreaded) | 30938  | `jvm_kotlin_idiomatic_fast_multi;30938;5.0;6;algorithm=base,faithful=yes`      |
| **JVM** Traditional (Multithreaded)    | 30768  | `jvm_kotlin_traditional_multi;30768;5.0;6;algorithm=base,faithful=yes`         |
| **JVM** Idiomatic (Multithreaded)      | 29173  | `jvm_kotlin_idiomatic_multi;29173;5.0;6;algorithm=base,faithful=yes`           |
| **JVM** Idiomatic Fast                 | 5893   | `jvm_kotlin_idiomatic_fast_single;5893;5.0;1;algorithm=base,faithful=yes`      |
| **JVM** Traditional                    | 5720   | `jvm_kotlin_traditional_single;5720;5.0;1;algorithm=base,faithful=yes`         |
| **JVM** Idiomatic                      | 5401   | `jvm_kotlin_idiomatic_single;5401;5.0;1;algorithm=base,faithful=yes`           |
| **Native** Traditional                 | 2415   | `native_kotlin_traditional_single;2415;5.002;1;algorithm=base,faithful=yes`    |
| **Native** Idiomatic Fast              | 2249   | `native_kotlin_idiomatic_fast_single;2249;5.003;1;algorithm=base,faithful=yes` |
| **Native** Idiomatic                   | 2070   | `native_kotlin_idiomatic_single;2070;5.001;1;algorithm=base,faithful=yes`      |
| **JS** Idiomatic Fast                  | 1180   | `js_kotlin_idiomatic_fast_single;1180;5.008;1;algorithm=base,faithful=yes`     |
| **JS** Idiomatic                       | 1172   | `js_kotlin_idiomatic_single;1172;5.01;1;algorithm=base,faithful=yes`           |
| **JS** Traditional                     | 1162   | `js_kotlin_traditional_single;1162;5.006;1;algorithm=base,faithful=yes`        |

### macOS

Specs: 2021 MacBook Pro 16", M1 Pro 10/16/16, 32 GB 6400 MHz LPDDR5 RAM, macOS Ventura 13.2.1

Tests were run outside of Docker due to a lack of support for ARM64 Linux with Kotlin/Native.

| Variant                                   | Passes | Output                                                                         |
| ----------------------------------------- | ------ | ------------------------------------------------------------------------------ |
| **JVM** Traditional (Multithreaded)       | 66038  | `jvm_kotlin_traditional_multi;66038;5.0;10;algorithm=base,faithful=yes`        |
| **JVM** Idiomatic (Multithreaded)         | 66021  | `jvm_kotlin_idiomatic_multi;66021;5.0;10;algorithm=base,faithful=yes`          |
| **JVM** Idiomatic Fast (Multithreaded)    | 65862  | `jvm_kotlin_idiomatic_fast_multi;65862;5.0;10;algorithm=base,faithful=yes`     |
| **Native** Idiomatic Fast (Multithreaded) | 58160  | `native_kotlin_idiomatic_fast_multi;58160;5.0;1;algorithm=base,faithful=yes`   |
| **Native** Idiomatic (Multithreaded)      | 54584  | `native_kotlin_idiomatic_fast_multi;58160;5.0;1;algorithm=base,faithful=yes`   |
| **Native** Traditional (Multithreaded)    | 53911  | `native_kotlin_traditional_multi;53911;5.0;1;algorithm=base,faithful=yes`      |
| **JVM** Idiomatic Fast                    | 8146   | `jvm_kotlin_idiomatic_fast_single;8146;5.005;1;algorithm=base,faithful=yes`    |
| **JVM** Idiomatic                         | 8129   | `jvm_kotlin_idiomatic_single;8129;5.002;1;algorithm=base,faithful=yes`         |
| **JVM** Traditional                       | 8077   | `jvm_kotlin_traditional_single;8077;5.014;1;algorithm=base,faithful=yes`       |
| **Native** Idiomatic Fast                 | 6787   | `native_kotlin_idiomatic_fast_single;6787;5.001;1;algorithm=base,faithful=yes` |
| **Native** Idiomatic                      | 6574   | `native_kotlin_idiomatic_single;6574;5.001;1;algorithm=base,faithful=yes`      |
| **Native** Traditional                    | 6081   | `native_kotlin_traditional_single;6081;5.001;1;algorithm=base,faithful=yes`    |
| **JS** Traditional                        | 2781   | `js_kotlin_traditional_single;2781;5.003;1;algorithm=base,faithful=yes`        |
| **JS** Idiomatic                          | 2733   | `js_kotlin_idiomatic_single;2733;5.003;1;algorithm=base,faithful=yes`          |
| **JS** Idiomatic Fast                     | 2732   | `js_kotlin_idiomatic_fast_single;2732;5.003;1;algorithm=base,faithful=yes`     |
