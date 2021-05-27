# arm64 assembly solution by rbergen

![Category](https://img.shields.io/badge/Category-faithful-green)


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
rbergen_arm64_byte;908;5.002;1
rbergen_arm64_bitmap;793;5.005;1
rbergen_arm64_bitshift;890;5.003;1
```