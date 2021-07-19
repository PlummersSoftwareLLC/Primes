# Octave solution based on sources from fspigel

![Algorithm](https://img.shields.io/badge/Algorithm-base-green)
![Faithfulness](https://img.shields.io/badge/Faithful-no-yellowgreen)
![Parallelism](https://img.shields.io/badge/Parallel-no-green)
![Bit count](https://img.shields.io/badge/Bits-8-yellowgreen)

Bit count tested to be 8 bits by calling
```octave
a=true(1,9);
whos a
```
This shows 9 bytes used for a array of 9 logicals. Hence, each logical uses 1 byte

Modified by rbergen as indicated in the source code. Marked as arch-amd64 because an arm64 gnuoctave/octave Docker image does not (yet) seem to be available, and creating one is no mean feat.