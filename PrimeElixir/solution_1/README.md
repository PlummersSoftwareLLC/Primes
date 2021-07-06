# Ruby solution by rbergen

![Algorithm](https://img.shields.io/badge/Algorithm-base-green)
![Faithfulness](https://img.shields.io/badge/Faithful-no-green)
![Parallelism](https://img.shields.io/badge/Parallel-no-green)
![Bit count](https://img.shields.io/badge/Bits-unknown-yellowgreen)

This is an implementation in Elixir. This implementation uses recursion in place of a while loop since Elixir does not have a true while loop. Elixir is also immutable requiring the bit array in Dave's implementation to be transformed and returned.

## Run instructions

### Elixir
Execute the following command from the implementation directory:
```
elixir prime.ex
```

### Docker
A Dockerfile has been provided.

Build Dockerfile
```
docker build -t elixir-prime .
```

Run 
```
docker run --rm elixir-prime . 
```

## Output
```
cdesch;1;32.212;1;algorithm=base,faithful=no
```
