FROM primeimages/odin:commit-481fc8a5b60cf15d AS build

WORKDIR /opt/app

COPY *.odin ./
RUN odin build main.odin -opt:3 -no-bounds-check -out:primes-odin-1

FROM primeimages/odin:commit-481fc8a5b60cf15d AS runtime

WORKDIR /opt/app

COPY --from=build /opt/app/primes-odin-1 primes-odin-1

ENTRYPOINT [ "./primes-odin-1" ]
