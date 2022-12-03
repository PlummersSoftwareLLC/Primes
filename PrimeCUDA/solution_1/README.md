# NVIDIA CUDA solution by rbergen

![Algorithm](https://img.shields.io/badge/Algorithm-other-yellowgreen)
![Faithfulness](https://img.shields.io/badge/Faithful-yes-green)
![Parallelism](https://img.shields.io/badge/Parallel-no-green)
![Bit count](https://img.shields.io/badge/Bits-1-green)

This is an implementation using NVIDIA CUDA, meaning it runs the actual sieve on a CUDA-capable NVIDIA GPU.

The solution heavily depends on CUDASieve, a [GitHub-hosted project](https://github.com/curtisseizert/CUDASieve) created by Curtis Seizert. This is illustrated by the very small size of the actual code in this solution. As such, Curtis deserves the bulk of the credit for this solution being here, in a working state. In turn, CUDASieve depends on the [NVIDIA CUDA Toolkit](https://developer.nvidia.com/cuda-toolkit).

## Notes

- Getting the solution to run on a system with a suitable NVIDIA GPU does take some work, as detailed in the [Run Instructions](#run-instructions) below.
The reason is that the way to setup the primary dependencies (CUDASieve and CUDA Toolkit) varies across platforms and GPUs. It includes some case-specific manual changes to the Makefiles of CUDASieve and this solution. 
- To build this solution and its prerequisites, a basic build toolchain needs be in place. On Ubuntu systems, it can be installed using the following command:
  
  ```text
  sudo apt install build-essential
  ```

- Due to the rather system-specific steps required to make this solution work, as well as it requiring the presence of a CUDA-capable NVIDIA GPU, no `Dockerfile` has been provided.

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

### Cloning and building CUDASieve

_**Note:** The contents of this solution's Makefile are based on the first commands in this section being executed from the directory `~/source`. The [section that discusses building this solution](#building-and-running-the-primes-cuda-solution) indicates what to do if you use a different directory._

CUDASieve is published in a [GitHub repository](https://github.com/curtisseizert/CUDASieve). Clone the repo with the following commands:

```text
git clone https://github.com/curtisseizert/CUDASieve.git
mkdir CUDASieve/obj
``` 

The CUDASieve makefile includes some variables that may well need to be changed to match your environment. They are at the top of the `makefile` file in the `CUDASieve` directory. 

|Variable|Description|
|-|-|
|`CUDA_DIR`|The directory the NVIDIA CUDA Toolkit is installed in. On Ubuntu, this is `/usr/local/cuda`.|
|`GPU_ARCH`|The compute architecture of the GPU in your system. The value of this variable can be based on the output of the `deviceQuery` tool mentioned in the [CUDA Samples section](#installing-the-nvidia-cuda-samples-optional).|
|`GPU_CODE`|The GPU architectures for which code should be compiled. As with `GPU_ARCH`, the values of this variable can be based on the output of `deviceQuery`, and should at least include the respective value for `GPU_ARCH`. Fewer entries will yield smaller executables.|

After modifying the `makefile`, build CUDASieve by running the following command from the `CUDASieve` directory:

```text
make 
```

### Building and running the Primes CUDA solution

Before the solution in this directory can be built, the solution's `Makefile` needs to be modified to match your environment. Specifically, the following variables must be set to their correct values:

|Variable|Description|
|-|-|
|`CUDA_DIR`|Set this to the same value the `CUDA_DIR` variable is set to in the `makefile` for CUDASieve.|
|`CUDASIEVE_DIR`|The directory that CUDASieve is located in. This is the directory CUDASieve was cloned into while following the steps in the [CUDASieve section](#cloning-and-building-cudasieve) of this README.|
|`GPU_ARCH`|Set this to the same value the `GPU_ARCH` variable is set to in the `makefile` for CUDASieve.|
|`GPU_CODE`|Set this to the same value the `GPU_CODE` variable is set to in the `makefile` for CUDASieve.|

After modifying the `Makefile`, build and run the solution by executing the following command from its root directory:

```text
make
./benchmark
```

If you'd like to run the solution with a different sieve size than the standard 1,000,000 the application can be started with the desired sieve size as an argument:

```text
./benchmark 100000000
```

## Output

```
Passes: 2007, Time: 5.001347, Avg: 0.002492, Limit: 1000000, Count: 78498, Validated: 1

rbergen_cuda;2007;5.001347;1;algorithm=other,faithful=yes,bits=1
```