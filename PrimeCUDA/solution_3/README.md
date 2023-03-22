# NVIDIA CUDA solution by rbergen, modified by davepl

![Algorithm](https://img.shields.io/badge/Algorithm-base-green)
![Faithfulness](https://img.shields.io/badge/Faithful-yes-green)
![Parallelism](https://img.shields.io/badge/Parallel-no-green)
![Bit count](https://img.shields.io/badge/Bits-1-green)
![Deviation](https://img.shields.io/badge/Deviation-GPU_processing-blue)

This is an implementation using NVIDIA CUDA. This means it runs part of the sieve algorithm on a CUDA-capable NVIDIA GPU.

This particular solution is a modified version of [PrimeCUDA/solution_2](../solution_2/). It uses managed memory and incorporates a number of other changes aimed at maximizing the performance of the GPU.

As such, further documentation about the solution's approach, configuration, build instructions, etc. can be found in [the README for solution_2](../solution_2/README.md).
