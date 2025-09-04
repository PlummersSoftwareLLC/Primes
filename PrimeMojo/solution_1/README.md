# Mojo solution by Evan Lucas-Currie

![Algorithm](https://img.shields.io/badge/Algorithm-base-green)
![Faithfulness](https://img.shields.io/badge/Faithful-yes-green)
![Faithfulness](https://img.shields.io/badge/Faithful-no-yellowgreen)
![Parallelism](https://img.shields.io/badge/Parallel-no-green)
![Bit count](https://img.shields.io/badge/Bits-1-green)
![Bit count](https://img.shields.io/badge/Bits-8-yellowgreen)

The solution follows the base algorithm and stays faithful with differing flag_storage approaches:
* 1bit - This is my attempt to make it 1:1 with the video that Dave has on youtube.
* 8bit - Here we use a boolean array which is 8bits per boolean and we simplify the logic by cutting back some calculations.
* Additionally we have two metaprogramming examples for the same solution which use statically sized arrays.

## Run instructions
```
docker build -f DockerFile -t mojosieve .
docker run --rm -it mojosieve
```

(M2 pro Mac)

## Output

```
ELucasCurrie_1bit;2248;5.0;1;algorithm=base,faithful=no,bit=1
ELucasCurrie_8bit;12876;5.0;1;algorithm=base,faithful=no,bit=8
ELucasCurrie_1bit;6880;5.0;1;algorithm=base,faithful=yes,bit=1
ELucasCurrie_8bit;9765;5.0;1;algorithm=base,faithful=yes,bit=8
```
