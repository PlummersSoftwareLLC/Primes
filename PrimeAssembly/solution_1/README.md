# x86-64 assembly solution by rbergen

![Algorithm](https://img.shields.io/badge/Algorithm-base-green)
![Faithfulness](https://img.shields.io/badge/Faithful-yes-green)
![Faithfulness](https://img.shields.io/badge/Faithful-no-yellowgreen)
![Parallelism](https://img.shields.io/badge/Parallel-no-green)
![Bit count](https://img.shields.io/badge/Bits-1-green)
![Bit count](https://img.shields.io/badge/Bits-8-yellowgreen)

This solution contains six implementations in x86-64 assembly, of which:
* x64ff_bitbtr is faithful, uses bits to store prime number flags and clears them using the btr instruction
* x64ff_bitshift is faithful, uses bits to store prime number flags and clears them using manual bit shifting
* x64ff_byte is faithful and uses bytes to store prime number flags 
* x64uff_bitbtr is unfaithful, uses bits to store prime number flags and clears them using the btr instruction
* x64uff_bitshift is unfaithful, uses bits to store prime number flags and clears them using manual bit shifting
* x64uff_byte is unfaithful and uses bytes to store prime number flags 

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
rbergen_x64uff_byte;7334;5.000;1;algorithm=base,faithful=no,bits=8
rbergen_x64ff_byte;8302;5.000;1;algorithm=base,faithful=yes,bits=8
rbergen_x64uff_bitbtr;4179;5.000;1;algorithm=base,faithful=no,bits=1
rbergen_x64ff_bitbtr;4177;5.000;1;algorithm=base,faithful=yes,bits=1
rbergen_x64uff_bitshift;6269;5.000;1;algorithm=base,faithful=no,bits=1
rbergen_x64ff_bitshift;4600;5.000;1;algorithm=base,faithful=yes,bits=1
```