# arm64 assembly solution by rbergen

![Algorithm](https://img.shields.io/badge/Algorithm-base-green)
![Faithfulness](https://img.shields.io/badge/Faithful-yes-green)
![Parallelism](https://img.shields.io/badge/Parallel-no-green)
![Bit count](https://img.shields.io/badge/Bits-1-green)
![Bit count](https://img.shields.io/badge/Bits-8-yellowgreen)

This solution contains three implementations in arm64 assembly. Two use bits to store prime number flags (one with precalculated bit masks), the other uses bytes. The basic algorithm used is that of the original C#/C++ implementations.

## Run instructions

### GCC
Execute the following command from the implementation directory, after GCC (with supporting build tools, including as) has been installed:
```
. build.sh
. run.sh
```

### Docker
A Dockerfile has been provided.

## Output
```
rbergen_arm64_byte;908;5.002;1;algorithm=base,faithful=yes,bits=8
rbergen_arm64_bitmap;793;5.005;1;algorithm=base,faithful=yes,bits=1
rbergen_arm64_bitshift;890;5.003;1;algorithm=base,faithful=yes,bits=1
```