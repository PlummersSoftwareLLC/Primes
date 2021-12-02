FROM primeimages/ocaml:4.12-r1 AS build

RUN mkdir /home/opam/primeocaml

WORKDIR /home/opam/primeocaml

COPY *.ml ./

RUN corebuild PrimeOCamlFunctional.native

FROM alpine:3.13

WORKDIR /app
COPY --from=build /home/opam/primeocaml/PrimeOCamlFunctional.native PrimeOCamlFunctional.native

ENTRYPOINT [ "./PrimeOCamlFunctional.native" ]
