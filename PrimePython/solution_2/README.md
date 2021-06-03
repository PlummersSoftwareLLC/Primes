# Python Prime Sieve by ssovest

![Algorithm](https://img.shields.io/badge/Algorithm-base-green)
![Faithfulness](https://img.shields.io/badge/Faithful-yes-green)
![Parallelism](https://img.shields.io/badge/Parallel-no-green)
![Bit count](https://img.shields.io/badge/Bits-8-yellowgreen)

## Running with Python

Install Python: https://www.python.org/downloads/


```
cd path/to/sieve
python PrimePY.py
```

## Running with Pypy

Download and extract Pypy3: https://www.pypy.org/download.html


```
cd path/to/pypy
pypy3 path/to/sieve/PrimePY.py
```

## Command line arguments

 - `--limit=X`, `-l X`: set upper limit for calculating primes. Default is 1_000_000.
 - `--time=X`, `-t X`: set running time, in seconds. Default is 10.
 - `--show`, `-s`: output the found primes.

## Running tests

```
cd path/to/sieve
python -m unittest
```

# Results on my machine

 - AMD A4-3305M 1.9 GHz, Windows 7 64 bit
 - Python: 3.8.1 64 bit
 - PyPy: 7.3.3
 - g++: 9.2.0

Passes, average over 10 runs, 10 sec. duration each
-
|Limit      |Python     |PyPy        |Python (original)|PyPy (original)|C#         |C++
|-----------|-----------|------------|-----------------|---------------|-----------|-----------
|10         |1'460'684.6|14'054'698.3|      1'341'886.6|   20'563'347.0|9'012'584.3|3'441'332.0
|100        |  654'359.3| 7'479'784.3|        238'464.1|    4'057'462.0|6'913'831.5|1'766'864.4
|1000       |  309'068.4| 1'997'727.7|         22'573.2|      549'117.2|1'728'762.2|1'353'130.4
|10000      |  118'406.0|   285'541.6|          1'937.1|       49'720.8|  171'218.5|  504'840.4
|100000     |   28'074.0|    26'177.5|            166.2|        3'161.2|   15'153.7|   63'118.2
|**1000000**|**2'222.2**| **1'710.4**|         **14.8**|      **203.8**|**1'377.7**|**4'870.8**
|10000000   |       98.1|        86.7|              2.0|           15.0|      107.2|      193.5
|100000000  |        7.3|         7.0|             1.0*|            2.0|        8.0|       14.0

Passes/sec., average over 10 runs
-
|Limit      |Python       |PyPy           |Python (original)|PyPy (original)|C#           |C++
|-----------|-------------|---------------|-----------------|---------------|-------------|-------------
|10         |146'068.41190|1'405'459.97299|    134'188.61218|2'056'315.06029|901'206.88097|344'133.20000
|100        | 65'435.74311|  747'974.69452|     23'846.33893|  405'743.77278|691'343.60515|176'686.44000
|1000       | 30'906.63142|  199'771.71623|      2'257.27410|   54'911.30435|172'866.33205|135'313.04000
|10000      | 11'840.54234|   28'553.94879|        193.64954|    4'972.00489| 17'120.87069| 50'484.04000
|100000     |  2'807.29283|    2'617.65142|         16.58595|      316.07127|  1'515.28333|  6'311.82000
|**1000000**|**222.16822**|  **170.97555**|      **1.42078**|   **20.32647**|**137.71392**|**487.08000**
|10000000   |      9.76870|        8.60671|          0.12896|        1.46159|     10.67402|     19.35000
|100000000  |      0.69502|        0.65422|         0.01194*|        0.11842|      0.72642|      1.40000

_*Avg. over 3 runs. I wasn't patient enough._

This implementation doesn't show significant performance on low limits, which is expectable. But then, as more and more work is delegated to Python's built-in methods, it becomes faster, with performance peaks at limits 100'000 and 1'000'000. Then performance somewhat drops again, but not much. I'm not 100% sure why, but my guess is the bytearray begins to take too much memory to fit in the cpu cache.

Also, PyPy seems to do an amazing job optimizing things which CPython is slow at. But there are some things, like slice assignment, that are faster in CPython.