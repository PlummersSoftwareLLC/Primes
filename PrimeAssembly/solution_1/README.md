# x86-64 assembly solution by rbergen

![Category](https://img.shields.io/badge/Category-faithful-green)
![Category](https://img.shields.io/badge/Category-unfaithful-yellowgreen)


This solution contains two implementations in x86-64 assembly, of which one is faithull (x64ff) and one is not (x64uff). The basic algorithm used is that of the original C#/C++ implementations.

## Run instructions

### NASM/gcc
Execute the following command from the implementation directory, after NASM and gcc have been installed:
```
. build.sh
. run.sh
```

### Docker
A Dockerfile has been provided.

## Output
```
rbergen_x64ff;8097;5.000;1
rbergen_x64uff;7352;5.000;1
```