# CPython solution by 1mikegrn

![Algorithm](https://img.shields.io/badge/Algorithm-base-green)
![Faithfulness](https://img.shields.io/badge/Faithful-no-yellowgreen)
![Parallelism](https://img.shields.io/badge/Parallel-no-green)
![Bit count](https://img.shields.io/badge/Bits-32-yellowgreen)

Python's inherent strengths as a programming language come from it's simplicity and flexibility. Neither of these attributes are particularly useful however when we just need to brute-force some number crunching. As Dave showed in his demo, native Python code hardly has the grit necessary to keep up with the likes of C# and C++ when it comes to calculating prime numbers. 

Python as an implementation however has some tricks available for devs when the task at hand just requires raw power. The standard interpreter, the CPython interpreter, is written in C; and as such, Python developers have the option to write extension modules in C which Python code can interface with directly. This is often the best approach to writing performant Python - push any heavy computation to the C layer and then return results to the Python layer for further interpretation.

This implementation makes use of that dynamic, by interfacing with the parameters of the problem in the Python layer and crunching the numbers in the C layer. The result is a ~6600% boost over the original interpretation. No other optimizations have been applied.

## Run instructions
this module runs in docker, simply build and run.

```
docker build -t primemixed-cpython-solution2 .
docker run --rm primemixed-cpython-solution2
```

## Output

```
1mikegrn;1853;5.001447677612305;1;algorithm=base,faithful=yes,bits=32
```
