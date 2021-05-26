# x86-64 assembly solution by rbergen

![Category](https://img.shields.io/badge/Category-faithful-green)
![Category](https://img.shields.io/badge/Category-unfaithful-yellowgreen)


This solution contains four implementations in x86-64 assembly, of which:
* one (x64ff_bit) is faithul and uses bits to store prime flags 
* one (x64ff_byte) is faithul and uses bytes to store prime flags 
* one (x64uff_bit) is unfaithul and uses bits to store prime flags 
* one (x64uff_byte) is unfaithul and uses bytes to store prime flags 

The basic algorithm used in all of these is that of the original C#/C++ implementations.

## Run instructions

### NASM/GCC
Execute the following command from the implementation directory, after NASM and GCC have been installed:
```
. build.sh
. run.sh
```

### Docker
A Dockerfile has been provided.

## Output
```
rbergen_x64ff_byte;8255;5.000;1
rbergen_x64uff_byte;7248;5.000;1
rbergen_x64ff_bit;4151;5.000;1
rbergen_x64uff_bit;4179;5.000;1
```