# OCaml solution by @gkpotter

An unfaithful implementation of the prime sieving algorithm using a functional approach in [OCaml](https://ocaml.org).

## Run Instructions
### Install OCaml and OPAM:

[OPAM](https://opam.ocaml.org) is the OCaml package manager. For more info on how to install both OCaml and OPAM, check out the OCaml [installation guide](https://ocaml.org/docs/install.html).

### Install Core
[Core](https://opensource.janestreet.com/core/) is an alternative standard library for OCaml, which can be installed using OPAM via:
```
opam install core
```
### Build
Build using corebuild via:
```
corebuild PrimeOcamlFunctional.native
```
### Run
Run the native executables via:
```
./PrimeOCamlFunctional.native
```
## Output
Example output on a MacBook Pro (16-inch, 2019) with an Intel Core i7 (I7-9750H) at 2.6 GHz running macOS Catalina 10.15.7, OCaml 4.10.0 and OCamlbuild 0.14.0:
```
gkpotter-unfaithful;954;5.004650;1
gkpotter-unfaithful;948;5.003024;1
gkpotter-unfaithful;941;5.000812;1
gkpotter-unfaithful;913;5.003839;1
gkpotter-unfaithful;935;5.000789;1
```