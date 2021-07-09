# PostScript solution by @epithet

![Algorithm](https://img.shields.io/badge/Algorithm-base-green)
![Faithfulness](https://img.shields.io/badge/Faithful-no-yellowgreen)
![Parallelism](https://img.shields.io/badge/Parallel-no-green)
![Bit count](https://img.shields.io/badge/Bits-8-yellowgreen)

This is how you run the code from the command line:
```
$ bash run.sh
epithet-ps;250;5.013;1;algorithm=base,faithful=no,bits=8
```

Execute `bash test.sh` to run unit tests.

As a bonus, you can run `make` to create a 1000x1000 pixel bitmap,
where each pixel represents a number from 1 to a million
(starting from the bottom left corner, increasing to the right,
and wrapping around after every 1000th pixel),
prime numbers are black, and the rest is white.
The dimensions can be configured in the `makefile`.
PostScript is primarily intended to be used for graphics, after all.
