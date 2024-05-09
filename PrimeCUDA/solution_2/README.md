# NVIDIA CUDA solution by rbergen

![Algorithm](https://img.shields.io/badge/Algorithm-base-green)
![Faithfulness](https://img.shields.io/badge/Faithful-yes-green)
![Parallelism](https://img.shields.io/badge/Parallel-no-green)
![Bit count](https://img.shields.io/badge/Bits-1-green)
![Deviation](https://img.shields.io/badge/Deviation-GPU_processing-blue)

This is an implementation using NVIDIA CUDA. This means it runs part of the sieve algorithm on a CUDA-capable NVIDIA GPU.

Primes up to and including the square root of the sieve limit are sieved on the CPU. The unmarking of multiples beyond the square root is done on the GPU in a heavily parallelized fashion. In fact, the solution implements two ways of doing this:

- One where each thread unmarks all the multiples of one or more individual primes
- One where the sieve space is cut up into blocks, and each thread unmarks the multiples of all primes in one block

The second method seems to be consistently faster than the first. This is most likely related at least in part to heavy use of an "atomic binary AND" in the first method.

Credits for coming up with the idea for a combined CPU/GPU approach go to Dave Plummer.

This solution depends on the [NVIDIA CUDA Toolkit](https://developer.nvidia.com/cuda-toolkit) but does not use other CUDA-related dependencies. In this, it differs from [the first CUDA solution](../solution_1/) in this repository.

## Notes

- Getting the solution to run on a system with a suitable NVIDIA GPU does take some work, as detailed in the [Run instructions](#run-instructions) below.
The reason is that the way to setup CUDA Toolkit varies across platforms and GPUs. It includes some case-specific manual changes to the Makefiles of this solution.
- To build this solution and its prerequisites, a basic build toolchain needs be in place. On Ubuntu systems, it can be installed using the following command:
  
  ```text
  sudo apt install build-essential
  ```

- Due to the rather system-specific steps required to make this solution work, as well as it requiring the presence of a CUDA-capable NVIDIA GPU, no `Dockerfile` has been provided.

## Configuration

At the top of the [primes.h](primes.h) header file, there are some defines that can be used to modify certain aspects of the sieve's implementation. They are described in the following table:

| Define | Description |
|-|-|
| `DEFAULT_SIEVE_SIZE` | The sieve limit that is used if none is passed as an argument to the program (see [Building and running the Primes CUDA solution](#building-and-running-the-primes-cuda-solution) below). |
| `MAX_THREADS` | The maximum number of parallel threads used on the GPU. The actual number of threads used can be lower, if there aren't enough "chunks of work" to keep this number of threads busy. |
| `BITS_PER_WORD` | The number of bits per sieve buffer word. Can be 32 or 64. |
| `ROLLING_LIMIT` | The highest (prime) number for which we'll use a rolling bit mask. This value cannot be higher than the number of bits per word. Set to 0 to disable the use of a rolling bit mask. The relevant code won't be compiled then, thus reducing performance side effects to zero. |
| `DEBUG` | If defined, the code will show debug output and run both unmarking methods once. |

The values currently set in `primes.h` are the ones that work best for me on my machine that's equipped with a CUDA-capable GPU, but YMMV.

## Run instructions

### Disclaimer

These instructions are based on the systems I have been testing this solution on, both of which are of the x86-64 architecture, running Ubuntu 22.04. Also, the GPUs used are of the Pascal architecture. Running this solution on setups with different characteristics may well require modifications to the instructions below.

### Installing the NVIDIA CUDA Toolkit

The NVIDIA CUDA Toolkit can be downloaded from [the NVIDIA Developer website](https://developer.nvidia.com/cuda-downloads). Installation guides for [Linux and Windows](https://docs.nvidia.com/cuda/index.html#installation-guides) are available there as well. Make sure to download the CUDA Toolkit package and follow the installation guides for your system/platform exactly.

### Installing the NVIDIA CUDA Samples (optional)

NVIDIA has published a [GitHub repository](https://github.com/NVIDIA/cuda-samples) with a number of sample programs that use the NVIDIA CUDA Toolkit. Instructions for cloning and building the samples are provided in the [repository's README](https://github.com/NVIDIA/cuda-samples#getting-started).

Installing the CUDA Samples is optional, but can help with identifying the exact characteristics of the CUDA card in your system. Specifically, it is the `deviceQuery` tool that can be used for this. To run it, from the root of the CUDA Samples (`cuda-samples`) repository clone, execute the following command:

```text
bin/x86_64/linux/release/deviceQuery
```

The command will output the characteristics of the NVIDIA GPU in your system. Specifically note the CUDA Capability version reported by the tool, in a line similar to the following:

```text
CUDA Capability Major/Minor version number:    6.1
```

### Building and running the Primes CUDA solution

This solution's Makefile includes some variables that may well need to be changed to match your environment. They are at the top of the file `Makefile`.

|Variable|Description|
|-|-|
|`CUDA_DIR`|The directory the NVIDIA CUDA Toolkit is installed in. On Ubuntu, this is `/usr/local/cuda`.|
|`GPU_ARCH`|The compute architecture of the GPU in your system. The value of this variable can be based on the output of the `deviceQuery` tool mentioned in the [CUDA Samples section](#installing-the-nvidia-cuda-samples-optional).|
|`GPU_CODE`|The GPU architectures for which code should be compiled. As with `GPU_ARCH`, the values of this variable can be based on the output of `deviceQuery`, and should at least include the respective value for `GPU_ARCH`. Fewer entries will yield smaller executables.|

After setting these variables to the correct values, build and run the solution by executing the following command from its root directory:

```text
make
./primes
```

If you'd like to run the solution with a different sieve size than the standard 1,000,000 the application can be started with the desired sieve size as an argument:

```text
./primes 100000000
```

## Output

```text
Passes: 7606, Time: 5.000132, Avg: 0.000657, Word size: 32, Max GPU threads: 256, Type: blocks, Limit: 1000000, Count: 78498, Validated: 1
rbergen_faithful_cuda_blocks;7606;5.000132;1;algorithm=base,faithful=yes,bits=1

Passes: 886, Time: 5.001567, Avg: 0.005645, Word size: 32, Max GPU threads: 256, Type: threads, Limit: 1000000, Count: 78498, Validated: 1
rbergen_faithful_cuda_threads;886;5.001567;1;algorithm=base,faithful=yes,bits=1

```
