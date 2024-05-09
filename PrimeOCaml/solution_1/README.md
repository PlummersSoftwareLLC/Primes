# OCaml solution by @gkpotter

![Algorithm](https://img.shields.io/badge/Algorithm-base-green)
![Faithfulness](https://img.shields.io/badge/Faithful-yes-green)
![Parallelism](https://img.shields.io/badge/Parallel-no-green)
![Bit count](https://img.shields.io/badge/Bits-unknown-yellowgreen)

A faithful implementation of the prime sieving algorithm in [OCaml](https://ocaml.org).

## Run Instructions

### Run Locally
#### Install OCaml and OPAM:

[OPAM](https://opam.ocaml.org) is the OCaml package manager. For more info on how to install both OCaml and OPAM, check out the OCaml [installation guide](https://ocaml.org/docs/install.html).

#### Install Libraries and Tools
This implementation uses [Core](https://opensource.janestreet.com/core/), which is an alternative standard library for OCaml, and [Dune](https://dune.build/) which is a build system for OCaml projects. These can be installed using OPAM via:
```
opam install core dune
```
#### Build and Run using Dune
Build and run using Dune via:
```
dune build
dune exec prime-ocaml
```

### Docker
As an alternative to running locally, build and run using [Docker](https://www.docker.com/) via:
```
docker build -t prime-ocaml .
docker run --rm prime-ocaml
```

## Output
**Machine:** MacBook Air (M2, 2022)

**Processor:** Apple M2

Example output running *locally* with OCaml 5.1:
```
gkpotter-faithful;1263;5.003109;1;algorithm=base,faithful=yes
gkpotter-faithful;1266;5.001691;1;algorithm=base,faithful=yes
gkpotter-faithful;1288;5.003413;1;algorithm=base,faithful=yes
gkpotter-faithful;1276;5.002650;1;algorithm=base,faithful=yes
gkpotter-faithful;1285;5.000810;1;algorithm=base,faithful=yes
```
Example output running *via Docker* with OCaml 5.1:
```
gkpotter-faithful;1207;5.000417;1;algorithm=base,faithful=yes
gkpotter-faithful;1221;5.001276;1;algorithm=base,faithful=yes
gkpotter-faithful;1241;5.000009;1;algorithm=base,faithful=yes
gkpotter-faithful;1240;5.002623;1;algorithm=base,faithful=yes
gkpotter-faithful;1214;5.002387;1;algorithm=base,faithful=yes
```
