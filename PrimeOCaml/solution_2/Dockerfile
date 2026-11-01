FROM ocaml/opam:alpine-3.24-ocaml-5.5 AS build

RUN opam install -y core.v0.17.2 dune.3.24.2

WORKDIR /home/opam/prime-ocaml-functional

COPY --chown=opam:opam . .

RUN opam exec -- dune build

FROM alpine:3.24

WORKDIR /app
COPY --from=build /home/opam/prime-ocaml-functional/_build/install/default/bin/prime-ocaml-functional prime-ocaml-functional.exe

ENTRYPOINT [ "./prime-ocaml-functional.exe" ]
