FROM primeimages/ocaml AS build

WORKDIR /prime-ocaml-functional

COPY . .

RUN dune build

FROM alpine

WORKDIR /app
COPY --from=build /prime-ocaml-functional/_build/install/default/bin/prime-ocaml-functional prime-ocaml-functional.exe

ENTRYPOINT [ "./prime-ocaml-functional.exe" ]
