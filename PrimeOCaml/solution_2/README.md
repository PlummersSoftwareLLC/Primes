# OCaml solution by @gkpotter

![Algorithm](https://img.shields.io/badge/Algorithm-base-green)
![Faithfulness](https://img.shields.io/badge/Faithful-no-yellowgreen)
![Parallelism](https://img.shields.io/badge/Parallel-no-green)
![Bit count](https://img.shields.io/badge/Bits-unknown-yellowgreen)

A functional implementation of the prime sieving algorithm in [OCaml](https://ocaml.org).

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
dune exec prime-ocaml-functional
```

### Docker
As an alternative to running locally, build and run using [Docker](https://www.docker.com/) via:
```
docker build -t prime-ocaml-functional .
docker run --rm prime-ocaml-functional
```

## Output
**Machine:** MacBook Air (M2, 2022)

**Processor:** Apple M2

Example output running *locally* with OCaml 5.1:
```
gkpotter-unfaithful;1350;5.001539;1;algorithm=base,faithful=no
gkpotter-unfaithful;1359;5.001631;1;algorithm=base,faithful=no
gkpotter-unfaithful;1372;5.001619;1;algorithm=base,faithful=no
gkpotter-unfaithful;1366;5.001790;1;algorithm=base,faithful=no
gkpotter-unfaithful;1329;5.001237;1;algorithm=base,faithful=no
```
Example output running *via Docker* with OCaml 5.1:
```
gkpotter-unfaithful;1272;5.002131;1;algorithm=base,faithful=no
gkpotter-unfaithful;1256;5.000893;1;algorithm=base,faithful=no
gkpotter-unfaithful;1273;5.000340;1;algorithm=base,faithful=no
gkpotter-unfaithful;1263;5.003227;1;algorithm=base,faithful=no
gkpotter-unfaithful;1269;5.001596;1;algorithm=base,faithful=no
```
