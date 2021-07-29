FROM primeimages/ocaml:4.12-r1 AS build

WORKDIR /home/opam/primeocaml

COPY *.ml ./

# hadolint ignore=DL3004
RUN sudo chown -R opam:nogroup .
RUN corebuild PrimeOCaml.native

FROM alpine:3.13

WORKDIR /app
COPY --from=build /home/opam/primeocaml/PrimeOCaml.native PrimeOCaml.native

ENTRYPOINT [ "./PrimeOCaml.native" ]
