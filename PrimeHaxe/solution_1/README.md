# Haxe solution by [Taylor Robinson](https://github.com/tayiorrobinson)

![Algorithm](https://img.shields.io/badge/Algorithm-base-green)
![Faithfulness](https://img.shields.io/badge/Faithful-yes-green)
![Parallelism](https://img.shields.io/badge/Parallel-no-green)
![Bit count](https://img.shields.io/badge/Bits-unknown-yellowgreen)

An implementation of a Prime Sieve in Haxe.

## What's Haxe?

See: https://haxe.org/

Haxe is a programming language that while it does have it's own interpreter, can also be cross-compiled to other languages such as C++, Python, C# & more.

## Run Instructions


1. Make sure you have a working [Haxe](https://haxe.org/) installation.
   - Make sure you have setup HaxeLib by typing `haxelib setup`
   
For simplicity, you can run the following commands from a bash shell to compile and run all implementations:
```
./compile.sh
./run.sh
``` 

### Haxe Interpreter

2. `haxe interp.hxml`

### Python

2. `haxe python.hxml`
3. `python3 bin/py.py`

### C++

2. `haxe cpp.hxml`
   1. If you get a `Error: Library hxcpp is not installed`, run `haxelib install hxcpp`
3. `./bin/cpp/Main`

## Output

My machine is a MacBook Pro 2020 (M1, 8GB RAM) running macOS Big Sur.

```
TayIorRobinson_Haxe_C++;2052;10.0021259784698;1;algorithm=base,faithful=yes
TayIorRobinson_Haxe_HaxeEval;49;10.1027731895446777;1;algorithm=base,faithful=yes
TayIorRobinson_Haxe_Python;26;10.114451885223389;1;algorithm=base,faithful=yes
```