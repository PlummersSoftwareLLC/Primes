# Contributing

## Introduction
Please follow the guidelines in this document if you want to submit a solution/implementation for inclusion in the drag race.

These guidelines have been drafted to facilitate a "fair" comparison between solutions, and to allow results to be processed and reported on, automatically. **Submissions that do not conform to these guidelines will, in principle, not be accepted.**

## Guide

### Determining the category/categories
Once you've written the solution, make a determination on whether it is faithful to the original implementation, and whether it is parallel or not.
You can do this by referring to [the category descriptions](#Categories) mentioned below.

### README.md
Now make sure to add a `README.md` that contains at least the following:

```
# <Language> solution by <YourUserName>

<CategoryBadge(s)>

*Give a short description of your implementation*

## Run instructions

*Describe how to run your application here. If build steps are required to make the solution runnable, include those too.*

## Output

*Show the output you got on your machine(s) here, in code blocks*
```

Concerning the category badge(s):
* The category badges for the various categories are specified in [their respective sections](#Categories).
* If your solution includes multiple implementations that fall under different categories, include one category badge for each category.
* If your solution deviates from the basic [rules](#Rules), add an additional category badge, using the following template:
  ```
  ![Category](https://img.shields.io/badge/Category-<name>-blue)
  ```
  Choose an appropriate name, and include an explanation in your README.md.

### Folder/directory
In the  `drag-race` branch, see what the highest numbered solution is for the language you chose, and place your solution in the following folder:
`Prime<Language>/solution_<highest+1>/` where "highest" is the number of the highest numbered solution you found. 

If no solution yet exists for the language you're submitting, put yours in `Prime<Language>/solution_1/`

### Dockerfile
Please add a `Dockerfile` that is configured to build and run your solution. It should output the run result to standard output (stdout), using the format specified in the basic [rules](#Rules). Any "auxiliary" output should be directed to standard error (stderr), if the language and toolkit you chose allows.

When composing the Dockerfile, please use the following as a reference for selecting the base image:
* If an official image exists on [Docker Hub](https://hub.docker.com/) for the language you chose, use that. If multiple images are available with different underlying Linux distributions, select the one to use in accordance with the next steps in this list.
* Otherwise, if it is possible to get the solution to build and run on Alpine 3.13 using Alpine 3.13 packages, use that.
* Otherwise, if it is possible to get the solution to build and run on Ubuntu 18.04, use that. Employ standard packages to the extent possible.
* Otherwise, choose another base image that you can get the solution to build and run in.

Also:
* If the solution requires a significantly larger number of packages/files to build than it does to run, please define an `AS build` image for the build stage.
* Do not include binary dependencies (executables, archives, etc) in your solution submission. If these are needed, create a base image on Docker Hub, preferably backed by a public GitHub repo under your account.

### Pull request
Finally, submit a pull request **targeting the branch `drag-race`**, and place at least the name of the language in the title. Make sure to verify and check the contributing requirements that are summarized in the pull request template.

**Please note:** pull requests that are opened on another branch than `drag-race` will not be merged, by definition.

### Help/support
If you need assistance with conforming to any of the guidelines mentioned above, then please clearly indicate this in your pull request and be specific in what you need help with. 
Note that we cannot guarantee that we can help make your solution mergeable if it doesn't conform to the guidelines, but we will do our best to help where we can. 

## Categories

### Faithful

#### Requirements:

* Your solution uses no external dependencies to calculate the actual sieve.
* Your solution uses a class to encapsulate the sieve, or an equivalent feature in your language. This class must contain the full state of the sieve. Each iteration should re-create a new instance of this class.
* Your solution does not use multi-threading or multi-processing.
* Your solution conforms to the base [rules](#Rules).

#### Category badge:

![Category](https://img.shields.io/badge/Category-faithful-green)

#### Category badge markdown:

```
![Category](https://img.shields.io/badge/Category-faithful-green)
```

### Parallel faithful

#### Requirements:

* Your solution uses no external dependencies to calculate the actual sieve (for example, `-lpthread` is fine).
* Your solution uses a class to encapsulate the sieve, or an equivalent feature in your language. This class must contain the full state of the sieve. Each iteration should re-create a new instance of this class.
* Your solution conforms to the base [rules](#Rules).

#### Category badge:

![Category](https://img.shields.io/badge/Category-faithful%2Cparallel-green)

#### Category badge markdown:

```
![Category](https://img.shields.io/badge/Category-faithful%2Cparallel-green)
```

### Unfaithful

#### Requirements:
* Your solution is not faithful, but conforms to the base [rules](#Rules).

#### Category badge:

![Category](https://img.shields.io/badge/Category-unfaithful-yellowgreen)

#### Category badge markdown:

```
![Category](https://img.shields.io/badge/Category-unfaithful-yellowgreen)
```

## Rules

* Your solution uses the sieve of Erastosthenes.
* Your benchmarked code returns either a list of primes or the `is_prime` array, containing the result of the sieve.
* Your solution runs for at least 5 seconds, and stops as quickly as possible after that.
* Your solution calculates all the primes up to 1,000,000.
* The test code outputs the following text to standard output:
   ```
   <name>;<iterations>;<total_time>;<num_threads>
   ```
   where:
   * `name` is **at least** your username, and if you have multiple implementations, a short keyword to discriminate each implementation
   * `iterations` is the amount of times your code ran in the allotted time
   * `total_time` is the total time it took to run, which would be slightly more than 5 seconds, in an `en_US` formatted decimal value (so, use `.` (period) and not `,` (comma) as the decimal separator)
   * `num_threads` is the total amount of threads that were used to execute the indicated number of `iterations`(so 1 for a single-threaded solution)
* Your solution adheres to the requirements of the category/categories you put it under.
* You own copyright to all code and are willing to license that code under BSD-3 or compatible, or the code is available under BSD-3 or compatible.
