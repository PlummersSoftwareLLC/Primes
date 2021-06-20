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

#### Install Core
[OCamlbuild](https://github.com/ocaml/ocamlbuild) is a convenient builder for simple projects, and [Core](https://opensource.janestreet.com/core/) is an alternative standard library for OCaml, which can be installed using OPAM via:
```
opam install ocamlbuild
opam install core
```
#### Build and Run
Build and run using corebuild via:
```
corebuild PrimeOcaml.native
./PrimeOCaml.native
```

### Docker
As an alternative to running locally, build and run using docker via:
```
docker build -t prime-ocaml .
docker run --rm prime-ocaml
```

## Output
**Machine:** MacBook Pro (16-inch, 2019)

**Processor:** Intel Core i7 (I7-9750H) at 2.6 GHz

Example output running *locally* with OCaml 4.10:
```
gkpotter-faithful;953;5.002805;1
gkpotter-faithful;943;5.000744;1
gkpotter-faithful;938;5.000446;1
gkpotter-faithful;939;5.002426;1
gkpotter-faithful;955;5.002166;1
```
Example output running *via Docker* with OCaml 4.12:
```
gkpotter-faithful;724;5.002301;1
gkpotter-faithful;718;5.001542;1
gkpotter-faithful;731;5.009693;1
gkpotter-faithful;728;5.000515;1
gkpotter-faithful;731;5.005918;1
```
