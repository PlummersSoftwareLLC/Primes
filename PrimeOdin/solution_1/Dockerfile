FROM primeimages/odin:dev-2025-12-ubuntu24.04 AS build

WORKDIR /opt/app

COPY *.odin ./
RUN odin build . -o:speed -no-bounds-check -out:primes-odin-1


FROM ubuntu:24.04 AS runtime

WORKDIR /opt/app

COPY --from=build /opt/app/primes-odin-1 primes-odin-1

ENTRYPOINT ["./primes-odin-1"]
