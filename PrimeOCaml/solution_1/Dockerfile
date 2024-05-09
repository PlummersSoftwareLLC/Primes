FROM primeimages/ocaml:5.1 AS build

WORKDIR /prime-ocaml

COPY . .

RUN dune build

FROM alpine:3.19

WORKDIR /app
COPY --from=build /prime-ocaml/_build/install/default/bin/prime-ocaml prime-ocaml.exe

ENTRYPOINT [ "./prime-ocaml.exe" ]
