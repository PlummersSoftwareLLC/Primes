# Cython solution by ssovest

1-bit Cython solution.

It splits the primes array into blocks which are then processed independently, this helps to negate the speed drop on limits higher than a million. Block size is configurable through command line args.

Supports parallelization, but defaults to running in 1 thread. Every block is processed by 1 thread, so there's no need to set the number of threads greater than the number of blocks in the array.

## Run instructions

Install [Python](https://python.org/)

Install Cython: `pip install cython`

If pip isn't installed, run `python -m ensurepip` and repeat the previous step

After Cython is installed, run:
```
cython -3 --embed Prime32.pyx -o Prime32.c -X cdivision=True -X boundscheck=False
```

_Linux_: Compile the c file:
```
gcc Prime32.c -Ofast -o sieve `python3-config --embed --includes --ldflags` -fopenmp
```

Then run `./sieve`

_Windows_: on Windows you'll have to provide Python's include and libraries folders manually. Run `where python` to get Python's installation path. You'll need `include` and `libs` folders from there. If the path contains non-ascii characters, copy these folders somewhere else, otherwise the build might fail.

Compilation with [Clang](https://github.com/llvm/llvm-project/releases/tag/llvmorg-12.0.0) on Windows is straighforward, just install it and run:
```
clang Prime32.c -Ofast -o sieve.exe -Ipath/to/python/include -Lpath/to/python/libs -lpython38 -fopenmp
```

(Replace "-lpython38" with the actual version of the lib)

Alternatively, compilation can be done with [mingw-w64](http://mingw-w64.org/doku.php):
```
gcc Prime32.c -Ofast -o sieve.exe -Ipath/to/python/include -Lpath/to/python/libs -lpython38 -DMS_WIN64 -municode -fopenmp
```


## Command line args:

 - `--limit=X`, `-l X`: set upper limit for calculating primes. Default is 1_000_000.
 - `--time=X`, `-t X`: set running time, in seconds. Default is 5.
 - `--show`, `-s`: output the found primes.
 - `--bsize`, `-b`: block size, in bytes. Must be a multiple of 4.
 - `--threads`, `-p`: number of threads, default is 1.

## Output

AMD A4-3305M 1.9 GHz, Windows 7 64 bit
```
Passes: 8057, Time: 5.0, Avg: 0.000620578379049274, Limit: 1000000, Count: 78498, Valid: True

ssovest-cy;8057;5.0;1;algorithm=base,faithful=yes,bits=1   
```