Prime Sieve: Ada
================

  An Ada translation of the CPP prime sieve written by davepl

Prerequisites
-------------

  All you need is an Ada Compiler.

### Windows 

  Download and install GNAT Community from [AdaCore](https://www.adacore.com/download)
  
### Linux

  Install FSF GNAT toolchain on debian\ubuntu you can use `apt install gnat gprbuild`
  You can also get them them from [AdaCore](https://www.adacore.com/download)

Compilation
-----------

  `gprbuild primesieveada.gpr`

Running
-------

  Should output an executable in the `bin` folder

Preformance
-----------

  I obtained the following prefomance on an AMD Ryzen 1800x @ 3.8GHz
  
    E:\Development\Primes\PrimeSieveAda\bin\main.exe
    Limit   : 1000000
    Time    : 5.000000000
    Passes  : 12448
    Average : 0.000401670
    Expected: 78498
    Actual  : 78498

  