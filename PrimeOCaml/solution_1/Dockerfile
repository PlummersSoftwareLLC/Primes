FROM primeimages/ocaml AS build

WORKDIR /prime-ocaml

COPY . .

RUN dune build

FROM alpine

WORKDIR /app
COPY --from=build /prime-ocaml/_build/install/default/bin/prime-ocaml prime-ocaml.exe

ENTRYPOINT [ "./prime-ocaml.exe" ]
