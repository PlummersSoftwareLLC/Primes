# OCaml solution by @gkpotter

![Algorithm](https://img.shields.io/badge/Algorithm-base-green)
![Faithfulness](https://img.shields.io/badge/Faithful-no-yellowgreen)
![Parallelism](https://img.shields.io/badge/Parallel-no-green)
![Bit count](https://img.shields.io/badge/Bits-unknown-yellowgreen)

An unfaithful implementation of the prime sieving algorithm using a functional approach in [OCaml](https://ocaml.org).

## Run Instructions
### Run Locally
#### Install OCaml and OPAM:

[OPAM](https://opam.ocaml.org) is the OCaml package manager. For more info on how to install both OCaml and OPAM, check out the OCaml [installation guide](https://ocaml.org/docs/install.html).

#### Install OCamlbuild and Core
[OCamlbuild](https://github.com/ocaml/ocamlbuild) is a convenient builder for simple projects, and [Core](https://opensource.janestreet.com/core/) is an alternative standard library for OCaml, both of which can be installed using OPAM via:
```
opam install ocamlbuild
opam install core
```
#### Build and Run
Build and run using corebuild via:
```
corebuild PrimeOcamlFunctional.native
./PrimeOCamlFunctional.native
```

### Docker
As an alternative to running locally, build and run using docker via:
```
docker build -t prime-ocaml-functional .
docker run --rm prime-ocaml-functional
```

## Output
**Machine:** MacBook Pro (16-inch, 2019)

**Processor:** Intel Core i7 (I7-9750H) at 2.6 GHz

Example output running *locally* with OCaml 4.10:
```
gkpotter-unfaithful;954;5.004650;1
gkpotter-unfaithful;948;5.003024;1
gkpotter-unfaithful;941;5.000812;1
gkpotter-unfaithful;913;5.003839;1
gkpotter-unfaithful;935;5.000789;1
```
Example output running *via Docker* with OCaml 4.12:
```
gkpotter-unfaithful;702;5.006194;1
gkpotter-unfaithful;708;5.001435;1
gkpotter-unfaithful;708;5.003118;1
gkpotter-unfaithful;707;5.006474;1
gkpotter-unfaithful;704;5.003153;1
```
