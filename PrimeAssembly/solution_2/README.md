# arm64 assembly solution by rbergen

![Category](https://img.shields.io/badge/Category-faithful-green)


This is an implementations in arm64 assembly. The basic algorithm used is that of the original C#/C++ implementations.

## Run instructions

### gcc
Execute the following command from the implementation directory, after gcc (with supporting build tools, including as) has been installed:
```
gcc primes_arm64.s -o primes_arm64
./primes_arm64
```

### Docker
A Dockerfile has been provided.

## Output
```
rbergen_arm64;2355;5.000;1
```