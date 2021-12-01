FROM primeimages/ocaml:4.12-r1 AS build

WORKDIR /home/opam/primeocaml

COPY *.ml ./

RUN corebuild PrimeOCaml.native

FROM alpine:3.13

WORKDIR /app
COPY --from=build /home/opam/primeocaml/PrimeOCaml.native PrimeOCaml.native

ENTRYPOINT [ "./PrimeOCaml.native" ]
