# Contributing <!-- omit in toc -->

## Table of Contents <!-- omit in toc -->
- [Introduction](#introduction)
- [If (a) solution(s) already exists for your language...](#if-a-solutions-already-exists-for-your-language)
- [Guide](#guide)
  - [Determining the implementation characteristics](#determining-the-implementation-characteristics)
  - [README.md](#readmemd)
  - [Folder/directory](#folderdirectory)
  - [Dockerfile](#dockerfile)
  - [Pull request](#pull-request)
  - [Help/support](#helpsupport)
- [Rules](#rules)
- [Characteristics](#characteristics)
  - [Algorithm](#algorithm)
  - [Faithfulness](#faithfulness)
  - [Parallelism](#parallelism)
  - [Flag storage](#flag-storage)
- [Output](#output)
  - [Tags](#tags)
  - [Examples](#examples)

## Introduction
Please follow the guidelines in this document if you want to submit a solution/implementation for inclusion in the drag race.

These guidelines have been drafted to facilitate a "fair" comparison between solutions, and to allow results to be processed and reported on, automatically. **Submissions that do not conform to these guidelines will, in principle, not be accepted.**

## If (a) solution(s) already exists for your language...

...then please:

1. First see if your ideas/approach can be used to improve one of the existing solutions. If the [key characteristics](#characteristics) of your approach match those of an existing solution then that is a strong indication that improving is the way to go. If that is the case, structure your [pull request](#pull-request) to improve the solution in question. Include only those changes that objectively improve the performance of the benchmarked/timed part of the code. We will not merge changes in style, idiomatic improvements, whitespace optimizations, etc. unless you arrange approval from the original author of the solution you're aiming to improve.

2. Before opening your pull request, check if another one is already open that aims to modify the same solution that yours does. If one exists, please discuss in that PR how your improvements can be added to it.

3. Only if you're convinced that adding a new solution is the only appropriate way forward, open a pull requests that aims to achieve that. In that case, a) explain clearly in your pull request description why you think a new solution is warranted, and b) please keep reading.

## Guide

### Determining the implementation characteristics
Once you've written the solution, make sure it complies with the basic [rules](#rules) and determine what are the relevant characteristics of your implementation(s). You can do this by referring to [the descriptions of relevant characteristics](#characteristics) included below.

### README.md
Now make sure to add a `README.md` that contains at least the following:

```
# <Language> solution by <YourUserName>

(Optional) <Badges>

*Give a short description of your implementation*

## Run instructions

*Describe how to run your application here. If build steps are required to make the solution runnable, include those too.*

## Output

*Show the output you got on your machine(s) here, in code blocks*
```

With the introduction of [tags](#tags) in the [output format](#output), badges are now optional in the `README.md`, _provided that tags are indeed used in the solution output_. If you do choose to add badges to your `README.md`, then:
* They need to comply with what is described in [the characteristics section](#characteristics). 
* If your solution includes multiple implementations that have different characteristics, then add all appropriate badges, once.
* If your solution deviates from the basic [rules](#rules), add an additional badge, using the following template:
  ```
  ![Deviation](https://img.shields.io/badge/Deviation-<name>-blue)
  ```
  Choose an appropriate name, and include an explanation in your `README.md`.

### Folder/directory
In the  `drag-race` branch, see what the highest numbered solution is for the language you chose, and place your solution in the following folder:
`Prime<Language>/solution_<highest+1>/` where "highest" is the number of the highest numbered solution you found. 

If no solution yet exists for the language you're submitting, put yours in `Prime<Language>/solution_1/`

### Dockerfile
Please add a `Dockerfile` that is configured to build and run your solution. It should output the run result to standard output (stdout), using the format described under [output](#output). As indicated there, any "auxiliary" output should be directed to standard error (stderr), if the language and toolkit you chose allows.

When composing the Dockerfile, please use the following as a reference for selecting the base image:
* If an official image exists on [Docker Hub](https://hub.docker.com/) for the language you chose, use that. If multiple images are available with different underlying Linux distributions, select the one to use in accordance with the next steps in this list.
* Otherwise, if it is possible to get the solution to build and run on Alpine 3.13 using Alpine 3.13 packages, use that.
* Otherwise, if it is possible to get the solution to build and run on a supported LTS release of Ubuntu (currently 18.04 or 20.04), use that. Employ standard packages to the extent possible.
* Otherwise, choose another base image that you can get the solution to build and run in.

Also:
* If the solution requires a significantly larger number of packages/files to build than it does to run, please define an `AS build` image for the build stage.
* Do not include binary dependencies (executables, archives, etc) in your solution submission. If these are needed, create a base image on Docker Hub, preferably backed by a public GitHub repo under your account.

#### Support for hardware architectures
We encourage solutions and therefore Docker images to support both amd64/x86_64 and arm64/aarch64 hardware architectures.

If your solution fundamentally supports only one architecture, you can use a "flag file" to indicate what architecture that is. Currently, examples of architecture-specific builds are the assembly builds for amd64 and arm64.

A flag file is an empty file in the solution directory that tells the CI and benchmark implementations to build and run the solution only for/on the architecture indicated. The flag file for arm64 builds is `arch-arm64`, for amd64 builds it is `arch-amd64`. 

#### Disabling build and benchmark
If it is not possible to include your solution in the CI workflow and/or automated benchmark runs, a `build-no` flag file has to be added to the solution directory. Note that:
- Your solution has to be included in the automated benchmark runs, if it is possible to do so. For one, lack of familiarity with Docker is _not_ a valid reason to exclude it.
- Automated builds should _only_ be disabled after the repository maintainers have specifically indicated that you need to do so. 

#### Hadolint
During the review of any PR, a CI workflow is triggered that includes a linting of Dockerfiles using [hadolint](https://github.com/hadolint/hadolint). Any issues that are found in a solution's Dockerfile will need to be fixed before the respective PR is merged.

If you want to run hadolint locally before submitting your Dockerfile, you can do so using the configuration in [config/hadolint.yml](config/hadolint.yml).
Instructions for installing hadolint can be found in the tool's documentation. Using a Docker container is a "non-intrusive" way of running hadolint once Docker is installed. In a Unix-like shell, this can be done by running the following command from the root directory of the Primes repository:
```
tools/hadolint.sh <language> <number>
```  

where:

* `<language>` corresponds to the `Prime` directory for the specified language. This is case-insensitive.
* `<number>` corresponds to the solution number.

Examples:

```
tools/hadolint.sh CPP 1
```

or

```
tools/hadolint.sh cpp 1
```

Both of these correspond to the `PrimeCPP/solution_1` directory.

### Pull request
Finally, submit a pull request **targeting the branch `drag-race`**, and place at least the name of the language in the title. Make sure to verify and check the contributing requirements that are summarized in the pull request template.

**Please note:** pull requests that are opened on another branch than `drag-race` will not be merged, by definition.

### Help/support
If you need assistance with conforming to any of the guidelines mentioned above, then please clearly indicate this in your pull request and be specific in what you need help with. 
Note that we cannot guarantee that we can help make your solution mergeable if it doesn't conform to the guidelines, but we will do our best to help where we can. 

## Rules

* Your solution uses the [sieve of Eratosthenes](https://en.wikipedia.org/wiki/Sieve_of_Eratosthenes).
* Your benchmarked code returns either a list of primes or the `is_prime` array, containing the result of the sieve.
* Your solution runs for at least 5 seconds, and stops as quickly as possible after that.
* Your solution calculates all the primes up to 1,000,000.
* You own copyright to all code and are willing to license that code under BSD-new/BSD-3 or a more permissive license, or the code is available under BSD-new/BSD-3 or a more permissive license.

## Characteristics

The collection of solutions in this repository have come to use different approaches to implementing the prime number sieve. A number of characteristics have been defined as being relevant for an equal comparison between implementations. These are:
| Name | Description |
|-|-|
| [algorithm](#algorithm) | The algorithm used to find the primes in the prime number sieve. |
| [faithfulness](#faithfulness) | If the implementation is true to the original one implemented by @davepl, at a technical level. |
| [parallelism](#parallelism) | If the implementation uses any type of multi-threaded processing/calculation. |
| [storage](#flag-storage) | The number of bits used to indicate if a number in the sieve is a prime number, or not. |

These characteristics are discussed in more detail in the following sections.

### Algorithm

This defines the algorithm(s) used by your implementation(s). 

#### Known algorithms

We currently consider the following algorithms to be "known" algorithms:
| Name | Description |
|-|-|
| base | This is the algorithm that was used by @davepl in the YouTube video that spawned this repository. It is described in more detail, [below](#base-algorithm). |
| wheel | Algorithms rooted in the principle of [wheel factorization](https://en.wikipedia.org/wiki/Wheel_factorization). These tend to take a (pre)calculated set of prime numbers within a certain base number range, that are then sequentially projected onto the sieve. |
| other | All algorithms that do not fall in the values already mentioned. This is used as the default if no algorithm is specified. |

**Note:** All implementations **must** use a form of the [sieve of Erastosthenes](https://en.wikipedia.org/wiki/Sieve_of_Eratosthenes) as the fundamental algorithm, as indicated in the base [rules](#rules).

#### Base algorithm

The base algorithm is defined as follows:
* The algorithm uses an outer loop, in which two operations are performed:
  1. searching for the next prime in the sieve, and 
  2. clearing this prime's multiples in the sieve. 
  
  In order to allow for idiomatic code, it is permissible to perform these steps in either order, provided the results are correct.

* When seeking factors (the first operation), the algorithm sequentially checks all odd numbers, starting at 3.

* When clearing non-primes in the sieve (the second operation), the algorithm clears all non-primes individually, increasing the number with 2 * factor on each cycle.

* Not seeking factors beyond the square root of the sieve size is permissible.

* Inverted prime marking logic (marking primes as zero/false and non-primes as one/true) is permissible.

* Starting the clearing loop at factor * factor (instead of 3 * factor, as done in the original implementation) is permissible.

If needed, the [original implementation in C++](https://github.com/PlummersSoftwareLLC/Primes/blob/38c826678a52a37b8a864465410562d330002091/PrimeCPP/solution_1/PrimeCPP.cpp) can be used as a reference.

#### Tag

The [output tag](#output) for the algorithm is `algorithm`. Recognized values are those listed under [known algorithms](#known-algorithms). If an implementation doesn't write an `algorithm` tag to output, it is considered to be an `other` algorithm.

#### Badge

If you choose to include badges in your `README.md`, then: 
* if the base algorithm is used, the badge to use is:

  ![Algorithm](https://img.shields.io/badge/Algorithm-base-green)

  The corresponding markdown is:

  ```
  ![Algorithm](https://img.shields.io/badge/Algorithm-base-green)
  ```

* if a different algorithm is used (either wheel or other), then the badge template to use is:

  ![Algorithm](https://img.shields.io/badge/Algorithm-<algorithm>-yellowgreen)

  The corresponding markdown is:

  ```
  ![Algorithm](https://img.shields.io/badge/Algorithm-<algorithm>-yellowgreen)
  ```

### Faithfulness

At a technical level, an implementation is considered faithful if it complies with the following:

* It uses no external dependencies to calculate the actual sieve.
* It uses a class to encapsulate the sieve, or a (closest) equivalent feature in your language if a class construct is not available. This class must contain the full state of the sieve. Each iteration should re-create a new instance of this class from scratch.
* The sieve size and corresponding prime candidate memory buffer (or language equivalent) are set/allocated dynamically at runtime. The size of the memory buffer must correspond to the size of the sieve.
* It conforms to the base [rules](#Rules).

All other implementations are considered unfaithful. Note that they still need to conform to the base [rules](#Rules). 

If the faithfulness of an implementation is not specified, it is assumed to be unfaithful.

#### Tag

The [output tag](#output) to express faithfulness is `faithful`. Recognized values are `yes` for faithful implementations, and `no` for unfaithful implementations. If an implementation doesn't write a `faithful` tag to output, it is considered to be an unfaithful algorithm.

#### Badge

If you choose to include badges in your `README.md`, then: 
* if the implementation is faithful, the badge to use is:

  ![Faithfulness](https://img.shields.io/badge/Faithful-yes-green)

  The corresponding markdown is:

  ```
  ![Faithfulness](https://img.shields.io/badge/Faithful-yes-green)
  ```

* if the implementation is unfaithful, the badge to use is: 

  ![Faithfulness](https://img.shields.io/badge/Faithful-no-yellowgreen)

  The corresponding markdown is:

  ```
  ![Faithfulness](https://img.shields.io/badge/Faithful-no-yellowgreen)
  ```

### Parallelism

This indicates if the implementation uses any form of parallelism. In this context, an implementation is considered to do so if more than 1 thread is involved with running the sieve, i.e. identifying the primes in it.

#### Tag

As the [output](#output) of implementations specifies how many threads are used, it is not needed to use a tag to indicate if an implementation is multi-threaded. That is, if the thread count that is written to output is 1, then the implementation is considered to be single-threaded. If the output includes a higher thread count, then it is marked as a multi-threaded implementation.

#### Badge

If you choose to include badges in your `README.md`, then: 
* if the implementation is single-threaded, the badge to use is:

  ![Parallelism](https://img.shields.io/badge/Parallel-no-green)

  The corresponding markdown is:

  ```
  ![Parallelism](https://img.shields.io/badge/Parallel-no-green)
  ```

* if the implementation is multi-threaded, the badge to use is:

  ![Parallelism](https://img.shields.io/badge/Parallel-yes-green)

  The corresponding markdown is:

  ```
  ![Parallelism](https://img.shields.io/badge/Parallel-yes-green)
  ```

### Flag storage

This characteristic specifies how many bits the implementation uses to store the indication (flag) if a number in the sieve is a prime number, or not.  

Common bit counts are:
| Number | Used when |
|-|-|
| 1 | Each prime number flag occupies one bit. Common implementations of this type use a bit array or bit masking on wider data types to get and set individual flags. |
| 8 | Common implementations that occupy 8 bits per flag store the flags in a "byte" variable. | 
| 32 | Common implementations that occupy 32 bits per flag store the flags in a "regular" integer variable. | 
| 64 | Common implementations that occupy 64 bits per flag store the flags in a "long" integer variable. | 

It's possible that the number of bits per flag is unknown. For example, this can be the case if an implementation uses a "boolean" basic type provided by the language, and the language does not define how booleans are logically stored in memory.

#### Tag

The [output tag](#output) for the flag size is `bits`. The value should reflect the exact number of bits that are occupied by each prime number flag. If an implementation doesn't write a `bits` tag to output, the bit count per tag is considered to be unknown.

#### Badge

If you choose to include badges in your `README.md`, then: 
* if the implementation's flag size is known to be 1 bit, the badge to use is:

  ![Bit count](https://img.shields.io/badge/Bits-1-green)

  The corresponding markdown is:

  ```
  ![Bit count](https://img.shields.io/badge/Bits-1-green)
  ```

* if the implementation's flag size is known but larger than 1 bit, the badge template to use is:

  ![Bit count](https://img.shields.io/badge/Bits-<count>-yellowgreen)

  The corresponding markdown is:

  ```
  ![Bit count](https://img.shields.io/badge/Bits-<count>-yellowgreen)
  ```

* if the implementation's flag size is unknown, the badge to use is: 

  ![Bit count](https://img.shields.io/badge/Bits-unknown-yellowgreen)

  The corresponding markdown is:

  ```
  ![Bit count](https://img.shields.io/badge/Bits-unknown-yellowgreen)
  ```

## Output

Your solution should write the following text to standard output for each implementation that it runs:
```
<label>;<iterations>;<total_time>;<num_threads>;<tags>
```
where:
* `label` is **at least** your username, and if you have multiple implementations for a specific language, a short keyword to discriminate each implementation.
* `iterations` is the amount of times your code ran in the allotted time.
* `total_time` is the total time it took to run, which would be slightly more than 5 seconds, in an `en_US` formatted decimal value (so, use `.` (period) and not `,` (comma) as the decimal separator).
* `num_threads` is the total amount of threads that were used to execute the indicated number of `iterations` (so 1 for a single-threaded solution).
* `tags` is an optional collection of name/value pairs that provide information on the [characteristics](#characteristics) of your implementation(s). They are discussed in more detail, [below](#tags).

Any other output should be directed to standard error, if possible. 

### Tags

If you choose to include tags, then:
* They must conform to what's indicated in the [characteristics](#characteristics) section. 
* Specificy each tag as `<name>=<value>`
* Make sure that `<name>` and `<value>` each don't exceed 32 characters in length
* If multiple tags are included in your output, separate them with commas (`,`)
* Don't use spaces anywhere in or between names, values, or name/value pairs

If you don't output tags, then the semicolon (`;`) between `num_threads` and `tags` should also not be written. 

Do note that default values are used for all tags that are not included, as mentioned under [characteristics](#characteristics).

### Examples

An example of a line of output with tags is:
```
rbergen;1234;5.005678;1;algorithm=base,faithful=no,bits=1
```

A line of output without tags could look like this:
```
rbergen;1234;5.005678;1
```
