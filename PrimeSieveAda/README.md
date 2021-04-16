Prime Sieve: Ada
================

  An Ada translation of the CPP prime sieve written by davepl

Prerequisites
-------------

  All you need is an Ada Compiler.

### Windows 
  
  Install FSF GNAT toolchain from [MinGW](https://sourceforge.net/projects/mingw/)\
  Alternatively you can download and install GNAT Community from [AdaCore](https://www.adacore.com/download)
  
### Linux

  Install FSF GNAT toolchain using your package manager\
  on debian\ubuntu you can use `apt install gnat gprbuild` to install this\
  Alternatively you can download and install GNAT Community from [AdaCore](https://www.adacore.com/download)

Compilation
-----------

  `gprbuild primesieveada.gpr`

Running
-------

  run `.\bin\main.exe` on windows and `./bin/main` on linux
