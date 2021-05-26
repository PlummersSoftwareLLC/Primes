# arm64 assembly solution by rbergen

![Category](https://img.shields.io/badge/Category-faithful-green)


This solution contains two implementations in arm64 assembly. One uses bits to store prime number flags, the other uses bytes. The basic algorithm used is that of the original C#/C++ implementations.

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
rbergen_arm64_byte;780;5.005;1
rbergen_arm64_bit;793;5.005;1
```