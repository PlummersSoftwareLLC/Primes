FROM ocaml/opam:alpine AS Build

WORKDIR primeocaml

RUN sudo apk add linux-headers

RUN opam install ocamlbuild
RUN opam install core

ENV PATH="/home/opam/.opam/4.12/bin:${PATH}"

ADD . .

RUN sudo chown -R opam:nogroup .
RUN corebuild PrimeOCaml.native

FROM alpine

WORKDIR /app
COPY --from=build /home/opam/primeocaml/PrimeOCaml.native PrimeOCaml.native

CMD ./PrimeOCaml.native