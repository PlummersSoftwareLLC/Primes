# Elixir solution by Thomas9911, (Parallelised by Deemooneill)

![Algorithm](https://img.shields.io/badge/Algorithm-base-green)
![Faithfulness](https://img.shields.io/badge/Faithful-yes-green)
![Parallelism](https://img.shields.io/badge/Parallel-yes-green)
![Bit count](https://img.shields.io/badge/Bits-1-green)

In Elixir it is possible to address a single bit in a binary (or what other programming languages call a string (technically not exactly the same) ). This is just a show case that it is possible, because it is not really fast and also not readable.

## Run instructions

Local:

```sh
MIX_ENV=prod mix run -e 'PrimeSieve.main'
```

Docker:

```sh
docker build -t primeelixir .
docker run primeelixir
```

## Output

Intel Core i5 9400F, Windows 10 pre Task useage:

```txt
thomas9911;37;5.682;1;algorithm=base,faithful=yes,bits=1
```

Intel Core i5 9400F, Windows 10 post Task useage:

```txt
thomas9911_parr;137;6.155;1;algorithm=base,faithful=yes,bits=1
```

Intel Core i5 9400F, docker post Task useage:

```txt
thomas9911_parr;168;5.094;1;algorithm=base,faithful=yes,bits=1
```
