# OCaml solution by @gkpotter

A faithful implementation of the prime sieving algorithm in [OCaml](https://ocaml.org).

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
corebuild PrimeOcaml.native
```
### Run
Run the native executables via:
```
./PrimeOCaml.native
```
## Output
Example output on a MacBook Pro (16-inch, 2019) with an Intel Core i7 (I7-9750H) at 2.6 GHz running macOS Catalina 10.15.7, OCaml 4.10.0 and OCamlbuild 0.14.0:
```
gkpotter-faithful;953;5.002805;1
gkpotter-faithful;943;5.000744;1
gkpotter-faithful;938;5.000446;1
gkpotter-faithful;939;5.002426;1
gkpotter-faithful;955;5.002166;1
```