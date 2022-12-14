# Ruby solution by darnellbrawner

![Algorithm](https://img.shields.io/badge/Algorithm-base-green)
![Faithfulness](https://img.shields.io/badge/Faithful-no-yellowgreen)
![Parallelism](https://img.shields.io/badge/Parallel-yes-green)
![Parallelism](https://img.shields.io/badge/Parallel-no-green)
![Bit count](https://img.shields.io/badge/Bits-unknown-yellowgreen)

This implements single threaded Numo solution (ruby's version of numpy), multithreaded solution using Ractor and 
a hybrid multithreaded Numo solution. There seems to be a flaw in Numo that causes a slow down when using more 
than 2 Ractors. 

## Run instructions

### Ruby
Execute the following command from the implementation directory:
```
ruby prime.rb
```

### Docker
A Dockerfile has been provided.

# Results on my machine

 - Macbook Pro M1 2021

## Output
```
darnellbrawner-Numo;1562;5.001;1;algorithm=base,faithful=no
darnellbrawner-MultiThreaded;987;5.013;1;algorithm=base,faithful=no
darnellbrawner-MultiThreaded-Numo_2core;2517;5.002;1;algorithm=base,faithful=no

```
