# GNU Octave solution by Brandon-Johns
![Algorithm](https://img.shields.io/badge/Algorithm-base-green)
![Faithfulness](https://img.shields.io/badge/Faithful-yes-green)
![Parallelism](https://img.shields.io/badge/Parallel-no-green)
![Bit count](https://img.shields.io/badge/Bits-1-green)
![Bit count](https://img.shields.io/badge/Bits-8-yellowgreen)

Multiple implementations:
* **1bit** is a fully faithful implementation, using bitwise operations with the `uint8` type to consume only 1 bit in memory per prime candidate.
* **8bit** is faithful in all but using the native `logical` data type which consumes 8 bits in memory per prime candidate. However, this allows much faster vectorised calculations.

GNU Octave is a close relative to the proprietary language MATLAB. Both of these implementations were written in MATLAB, then ported with very few changes required. These languages are intended for mathematics computations. They are optimised for 'vectorised calculations', which is where the syntax is used to operate on entire matrices instead of individually addressing the matrix elements inside of loops. e.g. `5*A` multiplies every element of the matrix A by 5.

The implementations are based on the C++ solution 1.

## Run instructions
Tested with Octave 6.3.0 on Ubuntu 18.04 LTE (through VirtualBox - hence expect values to be lower than normal)

Run from bash with
```bash
# Nominal tests:
#	'1bit',1000000,'basic'
#	'8bit',1000000,'basic'
octave -q -W --norc run.m

# Individually
$echo "PrimesRun('<Run Mode>', <Sieve Size>,'<Output Option>')" | octave -q -W --norc
```
`<Run Mode>`  (default '8bit') chooses the implementation to run. Must be one of
* `1bit`
* `8bit`

`<Sieve Size>` (default 1,000,000) is the upper limit of primes to find

`<Output Option>`  (default 'basic') must be one of
* `basic`: Show only the minimum required output
* `stats`: Also show some extra stats, same as in the C++ solution 1
* `all`: Also show all of the calculated primes

E.G.
```bash
$echo "PrimesRun('1bit',1000000,'basic')" | octave -q -W --norc
$echo "PrimesRun('8bit',1000000,'basic')" | octave -q -W --norc
$echo "PrimesRun('1bit',101,'all')" | octave -q -W --norc
$echo "PrimesRun('8bit',101,'all')" | octave -q -W --norc
```

## Output
### 1 bit
```bash
$echo "PrimesRun('1bit',1000000,'stats')" | octave -q -W --norc
# Output not recorded
# Takes about 3min to run
```

```bash
$echo "PrimesRun('1bit',101,'all')" | octave -q -W --norc
2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71,73,79,83,89,97,101,

Passes: 183, Time: 5.010980, Avg: 0.027382, Limit: 101, Count: 26, Valid: 1

Brandon-Johns_1bit;183;5.010980;1;algorithm=base,faithful=yes,bits=1
```

### 8 bit
```bash
$echo "PrimesRun('8bit',1000000,'stats')" | octave -q -W --norc
Passes: 85, Time: 5.019477, Avg: 0.059053, Limit: 1000000, Count: 78498, Valid: 1

Brandon-Johns_8bit;85;5.019477;1;algorithm=base,faithful=yes,bits=8
```


```bash
$echo "PrimesRun('8bit',101,'all')" | octave -q -W --norc
2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71,73,79,83,89,97,101,

Passes: 3701, Time: 5.000980, Avg: 0.001351, Limit: 101, Count: 26, Valid: 1

Brandon-Johns_8bit;3701;5.000980;1;algorithm=base,faithful=yes,bits=8
```

