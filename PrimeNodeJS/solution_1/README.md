# NodeJS solution by fvbakel

![Algorithm](https://img.shields.io/badge/Algorithm-base-green)
![Faithfulness](https://img.shields.io/badge/Faithful-yes-green)
![Parallelism](https://img.shields.io/badge/Parallel-no-green)
![Bit count](https://img.shields.io/badge/Bits-1-green)

First implementation in nodeJS, with a 32 bit integer array as buffer
for the bit array. This implementation is based on the logic from:

- Python/solution_2 by ssovest
- PrimeCPP          by Dave Plummer

## Run instructions

### Run native

Install nodeJS: <https://nodejs.org/en/download/>

```bash
cd path/to/sieve
node PrimeNode.js
```

### Run with docker

To run with Docker take the following steps:

1. Install Docker: <https://docs.docker.com/get-docker/>
2. Build the image:

    ```bash
    docker build --pull --rm -f "Dockerfile" -t nodesolution1:latest "."
    ```

3. Run with Docker:

    ```bash
    docker run --rm -it  nodesolution1:latest 
    ```

## Output

Below is an example of the output on my machine, running with Docker.

```bash
docker run --rm -it  nodesolution1:latest 
Passes: 1573, Time: 10.001, Avg: 0.00635791, Limit: 1000000, Count: 78498, Valid: true

fvbakelnodejs; 1573;10.001;1;algorithm=base,faithful=yes,bits=1
```

## Command line arguments

```bash
node PrimeNode.js [-h] [-l <limit>] [-t <time limit>] [-s] [-m <max nr of passes>] [-v]
   -h              Show this help message
   -l <limit>      Upper search limit for calculating prime numbers, default=1000000
   -t <time limit> Minimal runtime in seconds, default=10
   -s              Write the found prime numbers, default disabled
   -m              Maximum number of passes, this overides the -t parameter, default disabled
   -v              Write extra verbose messages, default disabled
```

## Results on my machine

- Intel(R) Core(TM) i7-3520M CPU @ 2.90GHz, Lubuntu 21.04 64 bit
- Node: 16.2.0 running in docker node:latest
- Docker version 20.10.2, build 20.10.2-0ubuntu2

The test result below are run on this machine, using the Docker method

Relative speed per solution, compared to the C++ solution based on
passes/sec., average over 10 runs. Calculation is: passes/sec devided by passes/sec C++ 

|Limit      |NodeJS      |Python (solution 2)|C++ (solution 1)
|-----------|------------|-------------------|----------------
|10         |      0.59  |             0.13  |            1
|100        |      0.47  |             0.06  |            1
|1000       |      0.22  |             0.05  |            1
|10000      |      0.19  |             0.14  |            1
|100000     |      0.16  |             0.17  |            1
|**1000000**|    **0.15**|           **0.26**|          **1**
|10000000   |     0.20   |             0.33  |            1
|100000000  |     0.30   |             0.50  |            1
  
Passes, average over 10 runs, 10 sec. duration each.

|Limit      |NodeJS      |Python (solution 2)|C++ (solution 1)
|-----------|------------|-------------------|----------------
|10         | 9408837.3  |        2142144.5  |  16019646.3
|100        | 6327163.3  |         873227.4  |  13581319.7
|1000       | 1708633.7  |         412989.9  |   7723773.9
|10000      |  202556.3  |         148427.6  |   1082042.6
|100000     |   15897.7  |          16626.7  |     98592.3
|**1000000**|  **1251.3**|         **2148.9**|    **8331.4**
|10000000   |     119.5  |            200.8  |       598.5
|100000000  |       8.7  |             14.2  |        27.2

Passes/sec., average over 10 runs

|Limit      |NodeJS     |Python (solution 2)|C++  (solution 1)
|-----------|-----------|-------------------|-----------------
|10         | 940883.7  |         214214.3  |  1601964.6
|100        | 632716.3  |          87316.8  |  1358132.0
|1000       | 170808.0  |          41284.9  |   772377.4
|10000      |  20255.6  |          14842.0  |   108204.3
|100000     |   1589.5  |           1662.5  |     9859.2
|**1000000**|  **125.0**|          **214.8**|    **833.1**
|10000000   |     11.9  |             20.0  |       59.9
|100000000  |      0.8  |              1.4  |        2.7
