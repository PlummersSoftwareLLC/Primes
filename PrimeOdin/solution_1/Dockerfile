FROM ubuntu:20.04 AS build

# Odin needs llvm and clang for building itself as well as building .odin files on *Nix systems
RUN apt-get update -qq \
    && apt-get install -y llvm-11 clang git build-essential

WORKDIR /opt/Odin
# get tag dev-2021-07 and build it
RUN git clone --depth 1 --branch dev-2021-07 https://github.com/odin-lang/Odin.git /opt/Odin && make

COPY *.odin /opt/app/
WORKDIR /opt/app
RUN /opt/Odin/odin build main.odin -opt:3 -no-bounds-check -out:primes-odin-1

FROM ubuntu:20.04 AS runtime
COPY --from=build /opt/app/primes-odin-1 primes-odin-1

ENTRYPOINT [ "./primes-odin-1" ]
